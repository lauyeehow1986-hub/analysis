# scale/stream_fit.R - the streaming fit of the linear model: TSQR over chunks, the aliasing rule and
# the lm finish (P1e.1, spec section 6.4, rulings R3, R10 and R12).
#
# Every .h_st_* function here is emitted into the analysis code with .sr_inline(), so it is
# self-contained: base and stats only (plus the other .h_ helpers), ASCII text, no StatRouter call.
#
# The fold: M <- .h_st_tsqr(M, cbind(X, y)) per chunk, starting from M = NULL. M is then the R factor
# of the whole [X | y] (columns in the design order), so crossprod(M) = crossprod(cbind(X, y)). The
# rank decision is taken once, on the final M, by .h_st_alias with the tolerance lm uses (1e-7): the
# LINPACK limited-pivoting QR compares each column's residual norm with its original norm, and both
# are invariant under the orthogonal transformation from [X | y] to M, so the kept and aliased
# columns are those of stats::lm.fit on the full data.

#' One TSQR merge (ruling R12): the R factor of rbind(M, B) by LINPACK qr(tol = 0), with its columns
#' put back in the original order (qr.R(qa)[, order(qa$pivot)]), so a column that is all zero in a chunk
#' (a factor level absent from it) cannot permute the result. With M = NULL it starts from B. A chunk
#' with fewer rows than columns gives a short (rows x columns) factor, which later chunks extend. An
#' empty B leaves M unchanged.
.h_st_tsqr <- function(M, B) {
  B <- as.matrix(B)
  storage.mode(B) <- "double"
  A <- if (is.null(M)) B else rbind(M, B)
  if (!nrow(A)) return(M)
  qa <- qr(A, tol = 0)
  R <- qr.R(qa)
  R[, order(qa$pivot), drop = FALSE]
}

#' The aliasing rule (rulings R10 and R12) on the final TSQR factor M: qa <- qr(M[, 1:p], tol = tol).
#' Returns list(R = the rank x rank upper triangle of the kept columns in pivot order, keep = the kept
#' column indices in the original order, rank, pivot, qr = qa). LINPACK's limited pivoting moves only
#' the deficient columns to the end and keeps the order of the others, so pivot order and original
#' order agree on the kept columns.
.h_st_alias <- function(M, p, tol) {
  p <- as.integer(p)
  qa <- qr(M[, seq_len(p), drop = FALSE], tol = tol)
  r <- qa$rank
  piv <- qa$pivot
  list(R = qr.R(qa)[seq_len(r), seq_len(r), drop = FALSE], keep = sort(piv[seq_len(r)]), rank = r,
       pivot = piv, qr = qa)
}

#' Chan's merge of the count, mean and centred sum of squares (M2) of y into st (NULL to start), for
#' the streamed total sum of squares of ruling R3 (st$m2 at the end).
.h_st_moments_add <- function(st, y) {
  if (is.null(st)) st <- list(n = 0, mean = 0, m2 = 0)
  y <- as.numeric(y)
  nb <- length(y)
  if (!nb) return(st)
  mb <- mean(y)
  m2b <- sum((y - mb)^2)
  n <- st$n + nb
  d <- mb - st$mean
  list(n = n, mean = st$mean + d * nb / n, m2 = st$m2 + m2b + d^2 * st$n * nb / n)
}

#' Finish the streamed lm from the TSQR factor M of [X | y] (p design columns named nms, n rows) and
#' the total sum of squares sst (centred, for a model with an intercept; sum(y^2) without one, as
#' summary.lm). Returns list(beta (named, NA for an aliased column), R, keep, rank, rss, df_resid,
#' sigma, r2, adj_r2, aliased, B, n): R the triangle of the kept columns, B = chol2inv(R) in keep order
#' named by the kept columns (the bread of every SE), sigma = sqrt(rss / (n - rank)) (ruling R3).
.h_st_lm_finish <- function(M, p, nms, n, sst) {
  if (is.null(M)) stop("no complete rows: every row has a missing value in the model")
  p <- as.integer(p)
  al <- .h_st_alias(M, p, 1e-7)
  r <- al$rank
  df_resid <- n - r
  if (df_resid < 1) {
    stop("no residual degrees of freedom: ", n, " complete rows for ", p, " coefficients")
  }
  z <- qr.qty(al$qr, M[, p + 1L])
  pk <- al$pivot[seq_len(r)]
  beta <- stats::setNames(rep(NA_real_, p), nms)
  beta[pk] <- backsolve(al$R, z[seq_len(r)])
  rss <- if (length(z) > r) sum(z[-seq_len(r)]^2) else 0
  o <- order(pk)
  B <- chol2inv(al$R)[o, o, drop = FALSE]
  dimnames(B) <- list(nms[al$keep], nms[al$keep])
  df_int <- if ("(Intercept)" %in% nms) 1 else 0
  r2 <- 1 - rss / sst
  list(beta = beta, R = al$R, keep = al$keep, rank = r, rss = rss, df_resid = df_resid,
       sigma = sqrt(rss / df_resid), r2 = r2, adj_r2 = 1 - (1 - r2) * ((n - df_int) / df_resid),
       aliased = nms[is.na(beta)], B = B, n = n)
}

# ---- glm: TSQR-IRLS following glm.fit (spec section 6.4, rulings R2, R10 and R11) ----------------
#
# Each IRLS step is one data pass: every chunk adds [sqrt(w) X | sqrt(w) z] to a TSQR factor and its
# deviance at the current mu. Pass 1 starts from mustart; pass k + 1 uses eta = X beta_k, so it reports
# D(beta_k) and convergence is decided there, by glm.fit's rule. The fit keeps beta_k and the R of pass k
# (weights at beta_{k-1}) and discards the WLS of pass k + 1, as glm.fit does. Aliased coefficients are 0
# inside the iteration (as Cdqrls returns them) and NA in the result. Only the canonical families of the
# routes are supported, with unit prior weights: binomial (logit) and poisson (log). The emit checker
# refuses a call through a computed function (family$linkinv(...)), so .h_st_glm_eval restates the
# family functions of stats (logit_linkinv, logit_mu_eta and binomial_dev_resids in C, poisson in R),
# with the same clipping, operation for operation.

#' The family name of `family`: a family object or its name ("binomial" or "poisson").
.h_st_fam <- function(family) {
  fam <- if (is.character(family)) family else family$family
  if (!(identical(fam, "binomial") || identical(fam, "poisson"))) {
    stop("the streaming fit supports the binomial and poisson families only")
  }
  fam
}

#' The starting mu of family$initialize for unit prior weights (a 0/1 outcome for binomial).
.h_st_mustart <- function(y, family) {
  y <- as.numeric(y)
  if (identical(.h_st_fam(family), "binomial")) {
    if (any(y < 0 | y > 1)) stop("y values must be 0 <= y <= 1")
    return((1 * y + 0.5) / (1 + 1))
  }
  if (any(y < 0)) stop("negative values not allowed for the Poisson family")
  y + 0.1
}

#' The link of mu: log(mu / (1 - mu)) for binomial, log(mu) for poisson.
.h_st_linkfun <- function(mu, family) {
  if (identical(.h_st_fam(family), "binomial")) log(mu / (1 - mu)) else log(mu)
}

#' The family functions at eta (and y): list(mu = linkinv(eta), mu_eta = mu.eta(eta),
#' var = variance(mu), dev = the unit deviances dev.resids(y, mu, 1)).
.h_st_glm_eval <- function(eta, y, family) {
  eps <- .Machine$double.eps
  y <- as.numeric(y)
  if (identical(.h_st_fam(family), "binomial")) {
    ex <- exp(eta)
    tmp <- ex
    tmp[eta < -30] <- eps
    tmp[eta > 30] <- 1 / eps
    mu <- tmp / (1 + tmp)
    opexp <- 1 + ex
    me <- ex / (opexp * opexp)
    me[eta > 30 | eta < -30] <- eps
    a <- numeric(length(y))
    b <- numeric(length(y))
    i <- y != 0
    a[i] <- y[i] * log(y[i] / mu[i])
    j <- (1 - y) != 0
    b[j] <- (1 - y[j]) * log((1 - y[j]) / (1 - mu[j]))
    list(mu = mu, mu_eta = me, var = mu * (1 - mu), dev = 2 * 1 * (a + b))
  } else {
    mu <- pmax(exp(eta), eps)
    r <- mu * 1
    i <- y > 0
    r[i] <- (1 * (y * log(y / mu) - (y - mu)))[i]
    list(mu = mu, mu_eta = pmax(exp(eta), eps), var = mu, dev = 2 * r)
  }
}

#' Streamed IRLS. passfn(beta_or_NULL, wls_fun) runs one data pass and calls wls_fun(X, y) per chunk
#' (X the p design columns named nms, y the outcome); beta is NULL on the first pass (mu = mustart),
#' else the working coefficients (0 for an aliased column). family is a family object or its name.
#' Returns list(beta (named, NA for an aliased column), beta_prev (the working coefficients the kept
#' pass's weights came from; NULL when they came from mustart), R (rank x rank, pivot order), keep, rank,
#' B = chol2inv(R) in keep order named by the kept columns, iter, converged, deviance (at beta),
#' near_separation, risk_above_one (both on the final mu, as routes/glm.R: binomial mu < 1e-08 or
#' > 1 - 1e-08; poisson mu > 1), aliased, passes, n, sel).
#' select (NULL, or a function of no arguments) is called once after the first pass and returns
#' list(nms, sel): the names of the p = length(nms) streamed design columns and the indices of the columns
#' the model has (the stream compile drops the columns of factor levels absent from the fitted rows, as
#' lm and glm do with drop.unused.levels). p and nms may then be NA and NULL at the call. Only the selected
#' columns enter the WLS solve; beta, keep, B and aliased are over them (keep indexes sel), beta_prev is
#' over all p columns (0 outside sel). Without select, sel is seq_len(p) and the fit is unchanged.
.h_st_irls <- function(passfn, family, p, nms, maxit = 25L, epsilon = 1e-8, select = NULL) {
  fam <- .h_st_fam(family)
  p <- as.integer(p)
  sel <- NULL
  maxit <- as.integer(maxit)
  if (is.na(maxit) || maxit < 1L) stop("maxit must be a whole number >= 1")
  one <- function(beta) {
    acc <- new.env(parent = emptyenv())
    acc$M <- NULL
    acc$dev <- 0
    acc$n <- 0
    acc$sep <- FALSE
    acc$rab <- FALSE
    wls <- function(X, y) {
      X <- as.matrix(X)
      storage.mode(X) <- "double"
      y <- as.numeric(y)
      m <- length(y)
      if (!m) return(invisible(NULL))
      eta <- if (is.null(beta)) .h_st_linkfun(.h_st_mustart(y, fam), fam) else drop(X %*% beta)
      fe <- .h_st_glm_eval(eta, y, fam)
      dev <- sum(fe$dev)
      if (!is.finite(dev)) stop("the streaming fit met a non-finite deviance; refit in memory")
      good <- fe$mu_eta != 0
      me <- fe$mu_eta[good]
      z <- eta[good] + (y[good] - fe$mu[good]) / me
      w <- sqrt((1 * me^2) / fe$var[good])
      A <- cbind(X[good, , drop = FALSE] * w, z * w)
      if (!all(is.finite(A))) stop("the streaming fit met a non-finite working value; refit in memory")
      acc$M <- .h_st_tsqr(acc$M, A)
      acc$dev <- acc$dev + dev
      acc$n <- acc$n + m
      acc$sep <- acc$sep || any(fe$mu < 1e-08 | fe$mu > 1 - 1e-08)
      acc$rab <- acc$rab || any(fe$mu > 1)
      invisible(NULL)
    }
    passfn(beta, wls)
    acc
  }
  step <- function(acc) {
    if (is.null(acc$M)) stop("no complete rows: every row has a missing value in the model")
    s <- if (is.null(sel)) seq_len(p) else sel
    ps <- length(s)
    Ms <- acc$M[, c(s, p + 1L), drop = FALSE]
    al <- .h_st_alias(Ms, ps, 1e-11)
    r <- al$rank
    zq <- qr.qty(al$qr, Ms[, ps + 1L])
    b <- stats::setNames(rep(0, p), nms)
    b[s[al$pivot[seq_len(r)]]] <- backsolve(al$R, zq[seq_len(r)])
    list(b = b, al = al, s = s)
  }
  a <- one(NULL)
  if (!is.null(select)) {
    sv <- select()
    nms <- as.character(sv[["nms"]])
    p <- length(nms)
    sel <- as.integer(sv[["sel"]])
  }
  cur <- step(a)
  devold <- a$dev
  prev <- NULL
  passes <- 1L
  conv <- FALSE
  iter <- 0L
  last <- a
  for (k in seq_len(maxit)) {
    last <- one(cur$b)
    passes <- passes + 1L
    iter <- k
    dev <- last$dev
    if (abs(dev - devold) / (abs(dev) + 0.1) < epsilon) {
      conv <- TRUE
      break
    }
    if (k == maxit) break
    devold <- dev
    prev <- cur$b
    cur <- step(last)
  }
  al <- cur$al
  s <- cur$s
  ns <- nms[s]
  r <- al$rank
  beta <- cur$b[s]
  beta[-al$keep] <- NA_real_
  o <- order(al$pivot[seq_len(r)])
  B <- chol2inv(al$R)[o, o, drop = FALSE]
  dimnames(B) <- list(ns[al$keep], ns[al$keep])
  binom <- identical(fam, "binomial")
  list(beta = beta, beta_prev = prev, R = al$R, keep = al$keep, rank = r, B = B, iter = iter,
       converged = conv, deviance = last$dev, near_separation = binom && last$sep,
       risk_above_one = !binom && last$rab, aliased = ns[is.na(beta)], passes = passes, n = last$n,
       sel = s)
}

#' The glm scores of ruling R11 (sandwich::estfun.glm, dispersion 1) for the kept design columns X:
#' s_i = x_i (y_i - mu_i) / mu.eta(eta_i) w_i, with mu and eta at the final beta and the working weight
#' w_i = mu.eta(eta_prev_i)^2 / variance(mu_prev_i) at the coefficients the kept pass's weights came
#' from (eta_prev = NULL: from mustart; 0 where mu.eta is 0, as glm.fit). Returns the rows x columns
#' score matrix; fold it with .h_st_meat_add(S, s, 1) or .h_st_cluster_step(st, s, 1, g).
.h_st_glm_scores <- function(X, y, eta, eta_prev, family) {
  fam <- .h_st_fam(family)
  X <- as.matrix(X)
  y <- as.numeric(y)
  if (is.null(eta_prev)) eta_prev <- .h_st_linkfun(.h_st_mustart(y, fam), fam)
  fe <- .h_st_glm_eval(eta, y, fam)
  fp <- .h_st_glm_eval(eta_prev, y, fam)
  w <- fp$mu_eta^2 / fp$var
  w[fp$mu_eta == 0] <- 0
  X * ((y - fe$mu) / fe$mu_eta * w)
}
