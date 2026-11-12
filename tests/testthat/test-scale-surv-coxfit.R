# P1e.2 Task 4: the streamed Cox Newton driver (.h_st_coxfit, coxfit6's loop with the scaled cholesky2)
# and coxph.fit's warnings (.h_st_cox_warn). The passes run in memory through cox_eval (helper-surv.R);
# the reference is survival::coxph. Synthetic data only.

# coxph's {-1,0,1} rule (S1): such a column is neither centred nor scaled, and coxph's lp keeps it
# uncentred. Every other column is scaled by n / sum|x - m|, read from the first pass (beta = 0).
.cf_prep <- function(df, xcols) {
  X <- as.matrix(df[, xcols, drop = FALSE])
  storage.mode(X) <- "double"
  z1 <- apply(X, 2L, function(z) all(z %in% c(-1, 0, 1)))
  list(X = X, z1 = z1, Xlp = sweep(X, 2L, ifelse(z1, 0, colMeans(X))))
}

.cf_passfn <- function(df, xcols, ties, strata, chunk) {
  pr <- .cf_prep(df, xcols)
  function(beta) {
    e <- cox_eval(beta, df, xcols, ties, chunk = chunk, strata = strata)
    e$max_eta <- max(pr$Xlp %*% beta)
    e
  }
}

.cf_scale <- function(df, xcols) {
  z1 <- .cf_prep(df, xcols)$z1
  n <- nrow(df)
  function(r) ifelse(z1 | r$sumabs == 0, 1, n / r$sumabs)
}

.cf_fit <- function(df, xcols, ties = "efron", strata = NULL, maxit = 20L, chunk = 7L) {
  .h_st_coxfit(.cf_passfn(df, xcols, ties, strata, chunk), length(xcols), xcols,
               .cf_scale(df, xcols), maxit = maxit)
}

.cf_ref <- function(df, xcols, ties = "efron", strata = NULL, maxit = 20L) {
  rhs <- paste(xcols, collapse = " + ")
  if (!is.null(strata)) rhs <- paste0(rhs, " + strata(", strata, ")")
  f <- stats::as.formula(paste("Surv(time, status) ~", rhs), env = new.env(parent = asNamespace("survival")))
  w <- character()
  fit <- withCallingHandlers(
    survival::coxph(f, data = df, ties = ties, control = survival::coxph.control(iter.max = maxit)),
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleWarning")
    })
  list(fit = fit, warnings = w)
}

.cf_same <- function(got, ref, info, tol = 1e-8) {
  expect_equal(unname(got$coef), unname(stats::coef(ref$fit)), tolerance = tol, info = info)
  expect_equal(unname(got$V), unname(ref$fit$var), tolerance = tol, info = info)
  expect_identical(got$iter, as.integer(ref$fit$iter), info = info)
  expect_equal(got$loglik, ref$fit$loglik, tolerance = 1e-10, info = info)
  expect_identical(.h_st_cox_warn(got), ref$warnings, info = info)
}

test_that("coxfit equals coxph coef and vcov", {
  d <- .cs_data()
  xc <- c("x1", "x2", "yr")
  for (ties in c("efron", "breslow")) {
    got <- .cf_fit(d, xc, ties)
    .cf_same(got, .cf_ref(d, xc, ties), ties)
    expect_identical(got$passes, got$iter + 1L)
    expect_identical(names(got$coef), xc)
    expect_identical(dimnames(got$V), list(xc, xc))
    expect_identical(got$flag, 3L)
  }
  # two strata, the first ending on a chunk boundary; an all-censored stratum
  d$s <- c(rep("a", 42L), rep("b", nrow(d) - 42L))
  d3 <- .cs_data(150L, 8L)
  d3$s <- rep(c("a", "b", "c"), each = 50L)
  d3$status[d3$s == "b"] <- 0L
  for (ties in c("efron", "breslow")) {
    .cf_same(.cf_fit(d, xc, ties, "s"), .cf_ref(d, xc, ties, "s"), paste("strata", ties))
    .cf_same(.cf_fit(d3, xc, ties, "s"), .cf_ref(d3, xc, ties, "s"), paste("censored stratum", ties))
  }
  # a fixture with a large effect (several iterations), chunk 1 and 1e5
  set.seed(31)
  n <- 300L
  y <- data.frame(x1 = rnorm(n, sd = 2), x2 = rbinom(n, 1L, 0.5))
  y$time <- round(rexp(n) * exp(-1.2 * y$x1 + 0.8 * y$x2), 2)
  y$status <- rbinom(n, 1L, 0.85)
  for (ch in c(1L, 1e5L)) .cf_same(.cf_fit(y, c("x1", "x2"), chunk = ch), .cf_ref(y, c("x1", "x2")), ch)
  # scale given as the S1 vector instead of a function of the first pass
  pr <- .cf_prep(y, c("x1", "x2"))
  sv <- ifelse(pr$z1, 1, n / colSums(abs(sweep(pr$X, 2L, colMeans(pr$X)))))
  got <- .h_st_coxfit(.cf_passfn(y, c("x1", "x2"), "efron", NULL, 7L), 2L, c("x1", "x2"), sv)
  .cf_same(got, .cf_ref(y, c("x1", "x2")), "scale vector")
})

test_that("the iteration trace equals coxph at every iter.max", {
  # a heavy-tailed covariate makes the first Newton steps overshoot, so step-halving runs; the best beta
  # at iter.max = k (the exhaustion pass included) then equals coxph's for every k
  set.seed(10)
  n <- 40L
  y <- data.frame(x1 = rt(n, 1), x2 = rt(n, 1), x3 = rnorm(n))
  y$time <- rexp(n) * exp(-2 * y$x3 - 0.5 * sign(y$x1))
  y$status <- rbinom(n, 1L, 0.9)
  xc <- c("x1", "x2", "x3")
  lls <- numeric()
  pf <- .cf_passfn(y, xc, "efron", NULL, 7L)
  full <- .h_st_coxfit(function(b) {
    e <- pf(b)
    lls <<- c(lls, e$loglik)
    e
  }, 3L, xc, .cf_scale(y, xc))
  expect_true(any(diff(lls) < 0))                 # a step was rejected and halved
  for (k in seq_len(full$iter + 1L)) {
    .cf_same(.cf_fit(y, xc, maxit = k), .cf_ref(y, xc, maxit = k), paste("iter.max", k))
  }
})

test_that("aliased column gives NA as coxph", {
  d <- .cs_data()
  d$x3 <- d$x1 + d$x2
  xc <- c("x1", "x2", "x3", "yr")
  for (ties in c("efron", "breslow")) {
    got <- .cf_fit(d, xc, ties)
    ref <- .cf_ref(d, xc, ties)
    na <- is.na(stats::coef(ref$fit))
    expect_identical(unname(is.na(got$coef)), unname(na), info = ties)
    expect_identical(unname(got$sing), unname(na), info = ties)
    expect_identical(got$flag, 3L)
    expect_equal(unname(got$coef[!na]), unname(stats::coef(ref$fit)[!na]), tolerance = 1e-8, info = ties)
    expect_equal(unname(got$V), unname(ref$fit$var), tolerance = 1e-8, info = ties)
    expect_identical(got$iter, as.integer(ref$fit$iter), info = ties)
    expect_identical(.h_st_cox_warn(got), ref$warnings, info = ties)
  }
})

test_that("monotone likelihood warns with coxph's text", {
  set.seed(7)
  n <- 80L
  d <- data.frame(time = rexp(n), status = rbinom(n, 1L, 0.8), x1 = rnorm(n))
  d$z <- as.numeric(d$time < stats::median(d$time) & d$status == 1)   # z = 1 only on early events
  d$w <- d$z * 3 + 2                                                    # the same, not in {-1, 0, 1}
  for (xc in list(c("x1", "z"), c("w", "x1"))) {
    for (ties in c("efron", "breslow")) {
      got <- .cf_fit(d, xc, ties)
      ref <- .cf_ref(d, xc, ties)
      info <- paste(c(xc, ties), collapse = " ")
      expect_true(length(ref$warnings) > 0L, info = info)
      expect_identical(.h_st_cox_warn(got), ref$warnings, info = info)
      expect_identical(got$iter, as.integer(ref$fit$iter), info = info)
    }
  }
  # coxph's exact text, trailing space included: paste() with its default sep
  got <- .cf_fit(d, c("x1", "z"))
  expect_identical(.h_st_cox_warn(got), "Loglik converged before variable  2 ; coefficient may be infinite. ")
})

test_that("no convergence warns", {
  set.seed(31)
  n <- 300L
  y <- data.frame(x1 = rnorm(n, sd = 2), x2 = rbinom(n, 1L, 0.5))
  y$time <- round(rexp(n) * exp(-1.2 * y$x1 + 0.8 * y$x2), 2)
  y$status <- rbinom(n, 1L, 0.85)
  xc <- c("x1", "x2")
  got <- .cf_fit(y, xc, maxit = 2L)
  ref <- .cf_ref(y, xc, maxit = 2L)
  expect_identical(.h_st_cox_warn(got), "Ran out of iterations and did not converge")
  expect_identical(ref$warnings, "Ran out of iterations and did not converge")
  expect_equal(unname(got$coef), unname(stats::coef(ref$fit)), tolerance = 1e-8)
  expect_equal(unname(got$V), unname(ref$fit$var), tolerance = 1e-8)
  expect_identical(got$flag, 1000L)
  expect_identical(got$iter, 3L)                  # coxfit6 has no break at the last iteration: maxit + 1
  expect_identical(got$passes, 4L)                # start, 2 iterations, the exhaustion pass
  expect_identical(got$V[1L, 2L], 0)              # chinv2 on the unfactored information: diagonal V
  # the infinite-coefficient follow-up: non-converged monotone fit
  set.seed(7)
  d <- data.frame(time = rexp(80L), status = rbinom(80L, 1L, 0.8), x1 = rnorm(80L))
  d$z <- as.numeric(d$time < stats::median(d$time) & d$status == 1)
  for (k in c(3L, 6L)) {
    expect_identical(.h_st_cox_warn(.cf_fit(d, c("x1", "z"), maxit = k)),
                     .cf_ref(d, c("x1", "z"), maxit = k)$warnings, info = k)
  }
  # max_eta above 500 adds the second warning
  fit <- list(flag = 1000L, maxit = 20L, coef = c(a = 1), U = c(a = 0), V = matrix(1), max_eta = 501)
  expect_identical(.h_st_cox_warn(fit), c("Ran out of iterations and did not converge",
                                          "one or more coefficients may be infinite"))
})

test_that("non-finite final loglik stops", {
  passfn <- local({
    k <- 0L
    function(beta) {
      k <<- k + 1L
      if (k == 1L) return(list(loglik = -10, U = c(1, 0.5), I = diag(2), max_eta = 0))
      list(loglik = NaN, U = c(NaN, NaN), I = matrix(NaN, 2L, 2L), max_eta = NaN)
    }
  })
  expect_error(.h_st_coxfit(passfn, 2L, c("a", "b"), c(1, 1)),
               "the streamed Cox fit gave a non-finite log-likelihood at every step-halving", fixed = TRUE)
})

test_that("the Newton driver helpers are self-contained and inline cleanly", {
  fns <- c(".h_st_chol2", ".h_st_chsolve2", ".h_st_chinv2", ".h_st_coxfit", ".h_st_cox_warn")
  for (f in fns[4:5]) {
    fn <- get(f)
    code <- .sr_inline(fn, f)
    expect_false(grepl("[^\x01-\x7F]", code, perl = TRUE))
    g <- codetools::findGlobals(fn, merge = FALSE)$functions
    own <- g[!vapply(g, function(x) exists(x, envir = baseenv()) || exists(x, envir = asNamespace("stats")) ||
                       x %in% fns, NA)]
    expect_identical(own, character(), info = f)
  }
})
