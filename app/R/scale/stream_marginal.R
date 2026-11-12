# scale/stream_marginal.R - streamed g-computation marginal effects and the streamed observed-risk counts
# of the streaming engine (P1e.1, spec section 6.6, rulings R8 and R9).
#
# Every .h_st_* function here is emitted into the analysis code with .sr_inline(), so it is
# self-contained: base and stats only (plus the other .h_ helpers), ASCII text, no StatRouter call.
#
# Marginal effects, one pass over the standardisation rows (the fitted rows for "analysis"; for "all"
# also the rows lost only to a missing outcome), decided per chunk in R with the in-memory snippets (R8):
#   Xa <- .h_st_marg_rows(over_chunk, rhs, exposure_col, values)   the chunk's design, exposure set to a
#   st <- .h_st_marg_add(st, Xa, beta, keep, family)               sum mu(eta_a), sum mu.eta(eta_a) x_a, n
#   .h_st_marg_finish(st, V, c(exposed, reference))                 means and gradients (sums / n), then
#                                                                   .h_gcomp_finish (below)
# Dividing the sums by n gives the mean and colMeans(X * mu.eta(eta)) of .sr_h_gcomp, and .h_gcomp_finish
# restates the end of .sr_h_gcomp operation for operation, so RD, log RR and their delta-method SEs are
# computed as in memory (a test pins the two at 1e-12). .sr_h_gcomp itself is left as it was, so the
# memory engine's emitted code stays byte-identical to before P1e (Review Focus 1, R6). Only the estimated coefficients enter
# (keep; aliased columns are dropped as .sr_h_gcomp drops the NA coefficients), so V is the covariance
# of the kept coefficients. A level absent from a chunk does not matter: every row is set to each level.
#
# Observed risk (R9), on the fitted rows during the first main pass: .h_st_obs_add counts n and events
# per exposure level, .h_st_obs_finish builds the table of .sr_glm_obs_code with the shared .h_wilson
# (inlined from .sr_h_wilson).

#' The finish of g-computation for the stream path: the arithmetic of the end of .sr_h_gcomp
#' (routes/marginal.R), restated here so that .sr_h_gcomp and the memory code it emits stay unchanged.
#' p holds the standardised means and grad their gradients over the estimated coefficients, both named
#' by as.character(level); levels = c(exposed, reference); V is the covariance of those coefficients.
#' Returns list(p1, p0, rd, rd_se, log_rr, log_rr_se) as .sr_h_gcomp.
.h_gcomp_finish <- function(p, grad, V, levels) {
  k <- as.character(levels)
  if (length(k) != 2L || !all(k %in% names(p)) || !all(k %in% names(grad))) {
    stop("the means and gradients must hold the exposed and the reference level")
  }
  m1 <- p[[k[1L]]]
  m0 <- p[[k[2L]]]
  g1 <- grad[[k[1L]]]
  g0 <- grad[[k[2L]]]
  se <- function(g) {
    v <- drop(crossprod(g, V %*% g))
    if (is.finite(v)) sqrt(max(v, 0)) else NA_real_
  }
  pos <- m1 > 0 && m0 > 0
  list(p1 = m1, p0 = m0, rd = m1 - m0, rd_se = se(g1 - g0),
       log_rr = if (pos) log(m1 / m0) else NA_real_,
       log_rr_se = if (pos) se(g1 / m1 - g0 / m0) else NA_real_)
}

#' The design rows of the chunk `over` with the exposure column set to each of `values` in turn, as
#' .sr_h_gcomp builds them (a factor exposure keeps its levels). rhs is the model's right-hand side
#' (sr_terms_code()$rhs); the factor columns carry the frozen levels, so every chunk has the same columns.
#' Returns a list of matrices named by as.character(values), or NULL for a chunk without rows.
.h_st_marg_rows <- function(over, rhs, exposure_col, values) {
  if (!nrow(over)) return(NULL)
  tt <- stats::delete.response(stats::terms(stats::as.formula(paste("~", rhs))))
  x <- over[[exposure_col]]
  out <- list()
  for (v in values) {
    d <- over
    d[[exposure_col]] <- if (is.factor(x)) {
      factor(rep(as.character(v), nrow(d)), levels = base::levels(x))
    } else {
      rep(v, nrow(d))
    }
    mf <- stats::model.frame(tt, d, na.action = stats::na.pass)
    if (anyNA(mf)) stop("the rows to standardise over have missing values in the model variables")
    out[[as.character(v)]] <- stats::model.matrix(tt, mf)
  }
  out
}

#' Add one chunk of the marginal pass. Xa_list holds, per exposure level a (named), the chunk's design rows
#' with the exposure set to a (all p columns); beta the p coefficients (NA for an aliased column) and keep
#' the indices of the estimated ones. family is "binomial", "poisson" or "gaussian" (identity, for lm), or
#' a family object. st is NULL at the start, then list(n, mu = sum mu(eta_a) per level, grad = sum
#' mu.eta(eta_a) x_a per level over the kept columns). A NULL or empty Xa_list leaves st unchanged.
.h_st_marg_add <- function(st, Xa_list, beta, keep, family) {
  if (is.null(Xa_list) || !length(Xa_list)) return(st)
  fam <- if (is.character(family)) family else family$family
  if (!identical(fam, "gaussian")) fam <- .h_st_fam(fam)
  m <- nrow(Xa_list[[1L]])
  if (!m) return(st)
  b <- beta[keep]
  if (anyNA(b)) stop("keep must index the estimated (non-NA) coefficients")
  lv <- names(Xa_list)
  if (is.null(st)) {
    st <- list(n = 0, mu = stats::setNames(numeric(length(lv)), lv),
               grad = stats::setNames(vector("list", length(lv)), lv))
  }
  for (a in lv) {
    X <- as.matrix(Xa_list[[a]])
    if (nrow(X) != m || ncol(X) != length(beta)) {
      stop("every level needs the same rows and one column per coefficient")
    }
    Xk <- X[, keep, drop = FALSE]
    eta <- drop(Xk %*% b)
    if (identical(fam, "gaussian")) {
      mu <- eta
      me <- rep.int(1, length(eta))
    } else {
      fe <- .h_st_glm_eval(eta, numeric(m), fam)
      mu <- fe$mu
      me <- fe$mu_eta
    }
    g <- colSums(Xk * me)
    st$mu[[a]] <- st$mu[[a]] + sum(mu)
    st$grad[[a]] <- if (is.null(st$grad[[a]])) g else st$grad[[a]] + g
  }
  st$n <- st$n + m
  st
}

#' Finish the marginal pass for one contrast, levels = c(exposed, reference): the means sum / n and the
#' gradients sum / n go to .h_gcomp_finish with V, the covariance of the kept coefficients (reordered by
#' name when it is named). Returns list(p1, p0, rd, rd_se, log_rr, log_rr_se) as .sr_h_gcomp.
.h_st_marg_finish <- function(st, V, levels) {
  if (is.null(st) || st$n < 1) stop("over must be a data frame with at least one row")
  if (!is.matrix(V) || nrow(V) != ncol(V)) stop("V must be a square covariance matrix")
  nm <- names(st$grad[[1L]])
  if (nrow(V) != length(nm)) {
    stop("V must have one row and column for each estimated coefficient (", length(nm), ")")
  }
  if (!is.null(rownames(V)) && !is.null(colnames(V)) && !is.null(nm)) {
    if (!setequal(rownames(V), nm) || !setequal(colnames(V), nm)) {
      stop("V must be named like the estimated coefficients")
    }
    V <- V[nm, nm, drop = FALSE]
  }
  n <- st$n
  .h_gcomp_finish(st$mu / n, lapply(st$grad, function(g) g / n), V, levels)
}

#' Add one chunk of fitted rows to the observed-risk counts (R9). exposure is the exposure design column
#' (t1f, a factor, or t1, numeric) and y the 0/1 outcome of the same rows. st is NULL at the start, then
#' list(lev, n, events, bin): the levels seen, their row and event counts, and whether every numeric
#' exposure value so far is 0 or 1 (counting stops once it is not: that exposure has no table).
.h_st_obs_add <- function(st, exposure, y) {
  if (is.null(st)) st <- list(lev = character(), n = numeric(), events = numeric(), bin = TRUE)
  m <- length(exposure)
  if (!m) return(st)
  if (length(y) != m) stop("exposure and y must have the same length")
  if (!is.factor(exposure)) {
    st$bin <- st$bin && all(exposure %in% c(0, 1))
    if (!st$bin) return(st)
  }
  k <- as.character(exposure)
  u <- unique(k)
  new <- u[!(u %in% st$lev)]
  if (length(new)) {
    st$lev <- c(st$lev, new)
    st$n <- c(st$n, numeric(length(new)))
    st$events <- c(st$events, numeric(length(new)))
  }
  i <- match(k, u)
  j <- match(u, st$lev)
  st$n[j] <- st$n[j] + tabulate(i, length(u))
  st$events[j] <- st$events[j] + as.numeric(rowsum(as.numeric(y), i, reorder = TRUE))
  st
}

#' The observed-risk table of .sr_glm_obs_code from the streamed counts: one row per level of `levels`
#' (the frozen factor levels), or for a numeric exposure (levels = NULL) the rows 0 and 1 when every value
#' was 0 or 1 and NULL otherwise. Counts are integers (as in memory) while they fit; the Wilson interval is
#' the inlined .h_wilson.
.h_st_obs_finish <- function(st, levels) {
  if (is.null(levels)) {
    if (!is.null(st) && !st$bin) return(NULL)
    levels <- c(0, 1)
  }
  k <- length(levels)
  n <- numeric(k)
  e <- numeric(k)
  if (!is.null(st)) {
    j <- match(as.character(levels), st$lev)
    hit <- !is.na(j)
    n[hit] <- st$n[j[hit]]
    e[hit] <- st$events[j[hit]]
  }
  # one type for the whole count columns, decided once: integer (as in memory) when every count fits
  if (all(n <= .Machine$integer.max)) {
    n <- as.integer(n)
    e <- as.integer(e)
  }
  obs <- NULL
  for (i in seq_len(k)) {
    w <- .h_wilson(e[i], n[i])
    obs <- rbind(obs, data.frame(level = as.character(levels[i]), n = n[i], events = e[i],
                                 est = w[["est"]], lo = w[["lo"]], hi = w[["hi"]],
                                 stringsAsFactors = FALSE))
  }
  obs
}
