# Streaming glm core: TSQR-IRLS following glm.fit, and the streamed glm sandwich (P1e.1 Task 7).
# Synthetic data only.
#
# The reference values come from stats::glm and sandwich on the same rows. Tolerances are those of spec
# section 10: 1e-8 for estimates and model SEs, 1e-7 for robust and cluster SEs. The data pass is driven
# over an in-memory design split into row blocks (chunks of 3 and 7 are below the column count).

.glc_data <- function(n = 5000L, dup = FALSE) {
  df <- .sc_frame(n)
  df$grp <- factor(df$grp, levels = sort(unique(df$grp)))
  if (dup) df$dup <- 2 * df$x1
  df
}
.glc_fml <- function(dup = FALSE) {
  stats::as.formula(paste("y ~ x1 + grp + x2 + I(x2^2)", if (dup) "+ dup"))
}

# Row blocks of 1..n.
.glc_blocks <- function(n, chunk) {
  s <- seq(1L, n, by = chunk)
  lapply(s, function(a) a:min(n, a + chunk - 1L))
}

# A pass function over the rows of X / y in chunks; it logs each pass's beta (NULL on the first pass) in
# the environment `log`.
.glc_passfn <- function(X, y, chunk, log = new.env()) {
  log$betas <- list()
  bl <- .glc_blocks(nrow(X), chunk)
  function(beta, wls) {
    log$betas[[length(log$betas) + 1L]] <- if (is.null(beta)) "start" else beta
    for (i in bl) wls(X[i, , drop = FALSE], y[i])
    invisible(NULL)
  }
}

.glc_fit <- function(X, y, family, chunk, maxit = 25L, log = new.env()) {
  .h_st_irls(.glc_passfn(X, y, chunk, log), family, ncol(X), colnames(X), maxit = maxit)
}

.glc_families <- list(binomial = stats::binomial(), poisson = stats::poisson(link = "log"))

# The model V and coefficients of a streamed fit against glm.
.glc_expect_glm <- function(st, fit, chunk) {
  info <- paste("chunk", chunk)
  cf <- stats::coef(fit)
  expect_identical(names(st$beta), names(cf), info = info)
  expect_identical(is.na(st$beta), is.na(cf), info = info)
  expect_equal(st$beta, cf, tolerance = 1e-8, info = info)
  expect_identical(st$iter, fit$iter, info = info)
  expect_identical(st$converged, fit$converged, info = info)
  expect_identical(st$passes, fit$iter + 1L, info = info)
  expect_identical(st$rank, fit$rank, info = info)
  expect_equal(st$deviance, fit$deviance, tolerance = 1e-8, info = info)
  nm <- names(cf)[!is.na(cf)]
  expect_identical(st$keep, unname(which(!is.na(cf))), info = info)
  expect_identical(st$aliased, names(cf)[is.na(cf)], info = info)
  V <- .h_st_vcov_finish("model", st$B, 1, st$n, st$rank, NA)
  expect_identical(dimnames(V), list(nm, nm))
  expect_equal(V, stats::vcov(fit)[nm, nm], tolerance = 1e-8, info = info)
  # the kept triangle is that of the fit's final QR (same weights): chol2inv(R) gives V directly
  o <- order(st$keep)
  expect_equal(unname(chol2inv(st$R)), unname(stats::vcov(fit)[nm, nm][o, o]), tolerance = 1e-8, info = info)
  expect_equal(st$n, stats::nobs(fit), info = info)
}

test_that("streamed IRLS equals glm binomial", {
  df <- .glc_data()
  fml <- .glc_fml()
  X <- stats::model.matrix(fml, df)
  fit <- stats::glm(fml, data = df, family = stats::binomial())
  expect_true(fit$converged)
  for (chunk in c(3L, 7L, 1000L, nrow(X) + 5L)) {
    log <- new.env()
    st <- .glc_fit(X, df$y, stats::binomial(), chunk, log = log)
    .glc_expect_glm(st, fit, chunk)
    expect_true(st$converged)
    expect_false(st$near_separation)
    expect_false(st$risk_above_one)
    # one pass per iteration plus the pass that decides convergence; the first starts from mustart
    expect_length(log$betas, st$passes)
    expect_identical(log$betas[[1L]], "start")
    expect_equal(log$betas[[st$passes]], ifelse(is.na(st$beta), 0, st$beta), tolerance = 0)
    # beta_prev: the coefficients the kept pass's weights came from
    expect_equal(st$beta_prev, log$betas[[st$passes - 1L]], tolerance = 0)
  }
})

test_that("streamed IRLS equals glm poisson log on a binary outcome", {
  df <- .glc_data()
  fml <- .glc_fml()
  X <- stats::model.matrix(fml, df)
  fit <- stats::glm(fml, data = df, family = stats::poisson(link = "log"))
  expect_true(fit$converged)
  for (chunk in c(7L, 1000L, nrow(X) + 5L)) {
    st <- .glc_fit(X, df$y, stats::poisson(link = "log"), chunk)
    .glc_expect_glm(st, fit, chunk)
    expect_true(st$converged)
    expect_false(st$near_separation)
    expect_identical(st$risk_above_one, any(fit$fitted.values > 1), info = paste("chunk", chunk))
  }
})

# The R11 scores of a streamed fit, folded over row blocks into the HC0 meat and the sorted cluster pass.
.glc_sandwich <- function(st, X, y, family, g, chunk) {
  Xk <- X[, st$keep, drop = FALSE]
  eta <- drop(Xk %*% st$beta[st$keep])
  eta_prev <- if (is.null(st$beta_prev)) NULL else drop(X %*% st$beta_prev)
  S <- NULL
  cl <- NULL
  sc <- NULL
  for (i in .glc_blocks(nrow(X), chunk)) {
    s <- .h_st_glm_scores(Xk[i, , drop = FALSE], y[i], eta[i], if (is.null(eta_prev)) NULL else eta_prev[i], family)
    S <- .h_st_meat_add(S, s, 1)
    cl <- .h_st_cluster_step(cl, s, 1, g[i])
    sc <- rbind(sc, s)
  }
  cl <- .h_st_cluster_flush(cl)
  list(scores = sc, hc0 = .h_st_vcov_finish("robust", st$B, S, st$n, st$rank, NA),
       hc1 = .h_st_vcov_finish("cluster", st$B, cl$S, st$n, st$rank, cl$G), G = cl$G)
}

test_that("glm HC0 and cluster HC1 equal sandwich", {
  df <- .glc_data(5000L, dup = TRUE)
  # a cluster of 9 rows straddling the 7-row boundary at 14/15 and a cluster ending on the last row
  df$pid[10:18] <- df$pid[10]
  df$pid[(nrow(df) - 3L):nrow(df)] <- max(df$pid) + 1
  expect_true(all(diff(df$pid) >= 0))
  fml <- .glc_fml(dup = TRUE)
  X <- stats::model.matrix(fml, df)
  for (fam in names(.glc_families)) {
    family <- .glc_families[[fam]]
    fit <- stats::glm(fml, data = df, family = family)
    nm <- names(stats::coef(fit))[!is.na(stats::coef(fit))]
    ref0 <- sandwich::vcovHC(fit, type = "HC0")
    ref1 <- sandwich::vcovCL(fit, cluster = df$pid, type = "HC1")
    ef <- sandwich::estfun(fit)
    for (chunk in c(7L, 1000L, nrow(X) + 1L)) {
      info <- paste(fam, "chunk", chunk)
      st <- .glc_fit(X, df$y, family, chunk)
      sw <- .glc_sandwich(st, X, df$y, family, df$pid, chunk)
      expect_equal(unname(sw$scores), unname(ef), tolerance = 1e-8, info = info)
      expect_identical(sw$G, length(unique(df$pid)), info = info)
      expect_equal(sw$hc0, ref0[nm, nm], tolerance = 1e-7, info = info)
      expect_equal(sw$hc1, ref1[nm, nm], tolerance = 1e-7, info = info)
    }
  }
})

test_that("non-convergence and separation flags match glm", {
  df <- .glc_data(2000L)
  fml <- .glc_fml()
  X <- stats::model.matrix(fml, df)
  # maxit = 2: not converged; the fit keeps beta_2 and the R of pass 2, as glm.fit does
  for (fam in names(.glc_families)) {
    family <- .glc_families[[fam]]
    ref <- suppressWarnings(stats::glm(fml, data = df, family = family, control = stats::glm.control(maxit = 2)))
    expect_false(ref$converged)
    for (chunk in c(7L, 1000L)) {
      st <- .glc_fit(X, df$y, family, chunk, maxit = 2L)
      expect_identical(st$converged, ref$converged)
      expect_identical(st$iter, 2L)
      expect_identical(st$passes, 3L)
      expect_equal(st$beta, stats::coef(ref), tolerance = 1e-8, info = paste(fam, chunk))
      expect_equal(.h_st_vcov_finish("model", st$B, 1, st$n, st$rank, NA), stats::vcov(ref), tolerance = 1e-8)
    }
  }
  # a perfectly separating predictor
  d <- df[1:400, ]
  d$xs <- ifelse(d$y == 1, 1, -1) * (1 + d$x2)
  f2 <- y ~ x1 + xs
  X2 <- stats::model.matrix(f2, d)
  ref <- suppressWarnings(stats::glm(f2, data = d, family = stats::binomial()))
  ref_sep <- any(ref$fitted.values < 1e-08 | ref$fitted.values > 1 - 1e-08)
  expect_true(ref_sep)
  for (chunk in c(7L, 1000L)) {
    st <- .glc_fit(X2, d$y, stats::binomial(), chunk)
    expect_true(st$near_separation)
    expect_identical(st$near_separation, ref_sep)
    expect_identical(st$converged, ref$converged)
    expect_identical(st$iter, ref$iter)
    expect_false(st$risk_above_one)
  }
})

test_that("aliased column is reported like glm", {
  df <- .glc_data(3000L, dup = TRUE)
  df$k3 <- 3
  fml <- stats::update(.glc_fml(dup = TRUE), . ~ . + k3)
  X <- stats::model.matrix(fml, df)
  for (fam in names(.glc_families)) {
    fit <- stats::glm(fml, data = df, family = .glc_families[[fam]])
    al <- names(stats::coef(fit))[is.na(stats::coef(fit))]
    expect_identical(al, c("dup", "k3"))
    for (chunk in c(7L, 1000L)) {
      st <- .glc_fit(X, df$y, .glc_families[[fam]], chunk)
      expect_identical(st$aliased, al, info = paste(fam, chunk))
      .glc_expect_glm(st, fit, chunk)
    }
  }
})

test_that("an all-zero column in a chunk, an all-same outcome and pass counts follow glm", {
  df <- .glc_data(1000L)
  # rows 1-50 carry no level C: the grpC column is all zero in the first 50-row chunk
  df$grp[1:50][df$grp[1:50] == "C"] <- "A"
  fml <- .glc_fml()
  X <- stats::model.matrix(fml, df)
  expect_true(all(X[1:50, "grpC"] == 0))
  fit <- stats::glm(fml, data = df, family = stats::binomial())
  for (chunk in c(50L, 7L)) .glc_expect_glm(.glc_fit(X, df$y, stats::binomial(), chunk), fit, chunk)
  # an outcome with no events: glm runs to maxit with fitted probabilities near 0
  y0 <- rep(0, nrow(df))
  d0 <- transform(df, y = y0)
  for (fam in names(.glc_families)) {
    ref <- suppressWarnings(stats::glm(y ~ x1, data = d0, family = .glc_families[[fam]]))
    st <- .glc_fit(stats::model.matrix(~ x1, d0), y0, .glc_families[[fam]], 7L)
    expect_identical(st$converged, ref$converged)
    expect_identical(st$iter, ref$iter)
    expect_equal(st$beta[["(Intercept)"]], stats::coef(ref)[["(Intercept)"]], tolerance = 1e-8)
    expect_equal(st$deviance, ref$deviance, tolerance = 1e-6)
    if (fam == "binomial") expect_true(st$near_separation)
  }
})

test_that("a non-finite deviance stops the streaming fit", {
  df <- .glc_data(300L)
  X <- stats::model.matrix(~ x1 + x2, df)
  y <- df$y
  # from the second pass on the design is blown up, so exp(eta) overflows
  passfn <- function(beta, wls) {
    if (is.null(beta)) wls(X, y) else wls(X * 1e306, y)
  }
  expect_error(.h_st_irls(passfn, stats::poisson(link = "log"), ncol(X), colnames(X)),
               "the streaming fit met a non-finite deviance; refit in memory", fixed = TRUE)
  # an Inf in a chunk passes complete.cases; it stops the fit instead of reaching LINPACK
  Xi <- X
  Xi[20, "x2"] <- Inf
  expect_error(.h_st_irls(function(beta, wls) wls(Xi, y), stats::binomial(), ncol(X), colnames(X)),
               "the streaming fit met a non-finite working value; refit in memory", fixed = TRUE)
  expect_error(.h_st_irls(function(beta, wls) invisible(NULL), stats::binomial(), 2L, c("a", "b")),
               "no complete rows: every row has a missing value in the model", fixed = TRUE)
})

test_that("the restated family functions are identical to stats::binomial and stats::poisson", {
  eta <- c(-800, -40, -30.5, -30, -29.9, -12, -1, -1e-9, 0, 1e-9, 0.3, 2, 17, 29.9, 30, 30.5, 40, 700)
  y <- rep(c(0, 1), length.out = length(eta))
  for (fam in names(.glc_families)) {
    f <- .glc_families[[fam]]
    e <- if (fam == "poisson") eta[eta < 700] else eta
    yy <- y[seq_along(e)]
    fe <- .h_st_glm_eval(e, yy, fam)
    mu <- f$linkinv(e)
    expect_identical(fe$mu, mu, info = fam)
    expect_identical(fe$mu_eta, f$mu.eta(e), info = fam)
    expect_identical(fe$var, f$variance(mu), info = fam)
    expect_identical(fe$dev, f$dev.resids(yy, mu, rep(1, length(yy))), info = fam)
    ms <- .h_st_mustart(yy, f)
    expect_identical(.h_st_linkfun(ms, fam), f$linkfun(ms), info = fam)
    # family$initialize for unit weights
    ev <- new.env()
    ev$y <- yy
    ev$weights <- rep(1, length(yy))
    ev$nobs <- length(yy)
    eval(f$initialize, ev)
    expect_identical(ms, ev$mustart, info = fam)
  }
  expect_error(.h_st_fam(stats::gaussian()), "binomial and poisson families only")
  expect_identical(.h_st_fam("poisson"), "poisson")
})

test_that("the glm core helpers inline as ASCII code the emit checker accepts and run standalone", {
  fns <- c(".h_st_tsqr", ".h_st_alias", ".h_st_fam", ".h_st_mustart", ".h_st_linkfun", ".h_st_glm_eval",
           ".h_st_irls", ".h_st_glm_scores", ".h_st_meat_add", ".h_st_vcov_finish")
  code <- paste(vapply(fns, function(f) .sr_inline(match.fun(f), f), character(1)), collapse = "\n")
  expect_false(grepl("[^\\x01-\\x7F]", code, perl = TRUE))
  expect_silent(.sr_emit_check(code, "analysis"))
  e <- new.env(parent = baseenv())
  eval(parse(text = code), e)
  df <- .glc_data(500L)
  fml <- .glc_fml()
  X <- stats::model.matrix(fml, df)
  fit <- stats::glm(fml, data = df, family = stats::binomial())
  bl <- .glc_blocks(nrow(X), 7L)
  st <- e$.h_st_irls(function(beta, wls) for (i in bl) wls(X[i, , drop = FALSE], df$y[i]),
                     stats::binomial(), ncol(X), colnames(X))
  expect_equal(st$beta, stats::coef(fit), tolerance = 1e-8)
  expect_identical(st$iter, fit$iter)
  s <- e$.h_st_glm_scores(X, df$y, drop(X %*% st$beta), drop(X %*% st$beta_prev), stats::binomial())
  expect_equal(e$.h_st_vcov_finish("robust", st$B, e$.h_st_meat_add(NULL, s, 1), st$n, st$rank, NA),
               sandwich::vcovHC(fit, type = "HC0"), tolerance = 1e-7)
})
