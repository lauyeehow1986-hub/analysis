# Tests for impute/pool.R: Rubin's rules with the Barnard-Rubin degrees of freedom (.sr_h_rubin, inlined
# into emitted code as .h_rubin) and the two-stage choice of m (.sr_h_auto_m). Public package data only
# (mice::nhanes).

# Per-imputation coefficients and covariance matrices of lm(chl ~ age + bmi) on mice::nhanes, m = 5.
.pl_fits <- function() {
  imp <- mice::mice(mice::nhanes, m = 5, seed = 1, printFlag = FALSE)
  fit <- with(imp, lm(chl ~ age + bmi))
  list(fit = fit, est = lapply(fit$analyses, stats::coef), vcov = lapply(fit$analyses, stats::vcov),
       dfcom = fit$analyses[[1L]]$df.residual)
}

test_that(".h_rubin equals mice::pool: estimate, SE, Barnard-Rubin df and FMI", {
  f <- .pl_fits()
  got <- .sr_h_rubin(f$est, f$vcov, f$dfcom)
  expect_identical(names(got), c("term", "est", "se", "lo", "hi", "p", "df", "riv", "lambda", "fmi", "mcse"))
  pl <- mice::pool(f$fit)
  sm <- summary(pl)
  expect_identical(got$term, as.character(sm$term))
  expect_equal(got$est, sm$estimate, tolerance = 1e-10)
  expect_equal(got$se, sm$std.error, tolerance = 1e-10)
  expect_equal(got$df, sm$df, tolerance = 1e-8)
  expect_equal(got$fmi, pl$pooled$fmi, tolerance = 1e-10)
  expect_equal(got$riv, pl$pooled$riv, tolerance = 1e-10)
  expect_equal(got$lambda, pl$pooled$lambda, tolerance = 1e-10)
  expect_equal(got$p, sm[[grep("^p", names(sm))]], tolerance = 1e-8)
  expect_equal(got$lo, got$est - stats::qt(0.975, got$df) * got$se, tolerance = 1e-12)
  expect_equal(got$hi, got$est + stats::qt(0.975, got$df) * got$se, tolerance = 1e-12)
  Q <- do.call(rbind, f$est)
  expect_equal(got$mcse, unname(sqrt(apply(Q, 2, stats::var) / 5)), tolerance = 1e-12)
  expect_identical(rownames(got), as.character(1:3))
})

test_that(".h_rubin with dfcom = Inf gives df_old and a normal interval", {
  f <- .pl_fits()
  got <- .sr_h_rubin(f$est, f$vcov, Inf)
  expect_equal(got$df, 4 / got$lambda^2, tolerance = 1e-12)
  expect_equal(got$lo, got$est - stats::qnorm(0.975) * got$se, tolerance = 1e-12)
  expect_equal(got$p, 2 * stats::pnorm(-abs(got$est / got$se)), tolerance = 1e-12)
  expect_equal(got$fmi, (got$riv + 2 / (got$df + 3)) / (got$riv + 1), tolerance = 1e-12)
})

test_that(".h_rubin stops when the design differs across imputations", {
  f <- .pl_fits()
  bad <- f$est
  names(bad[[3L]])[2L] <- "other"
  expect_error(.sr_h_rubin(bad, f$vcov, f$dfcom), "design differs across imputations", fixed = TRUE)
  short <- f$est
  short[[2L]] <- short[[2L]][-1L]
  expect_error(.sr_h_rubin(short, f$vcov, f$dfcom), "design differs across imputations", fixed = TRUE)
  v <- f$vcov
  v[[4L]] <- v[[4L]][-1L, -1L]
  expect_error(.sr_h_rubin(f$est, v, f$dfcom), "design differs across imputations", fixed = TRUE)
  v <- f$vcov
  dimnames(v[[1L]]) <- list(c("a", "b", "c"), c("a", "b", "c"))
  expect_error(.sr_h_rubin(f$est, v, f$dfcom), "design differs across imputations", fixed = TRUE)
  ro <- f$est
  ro[[5L]] <- ro[[5L]][c(2L, 1L, 3L)]
  expect_error(.sr_h_rubin(ro, f$vcov, f$dfcom), "design differs across imputations", fixed = TRUE)
})

test_that(".h_rubin refuses fewer than two imputations and malformed arguments", {
  f <- .pl_fits()
  expect_error(.sr_h_rubin(f$est[1L], f$vcov[1L], f$dfcom), "two imputations")
  expect_error(.sr_h_rubin(f$est, f$vcov[1:4], f$dfcom), "same length")
  expect_error(.sr_h_rubin(f$est, f$vcov, NA_real_), "dfcom")
  expect_error(.sr_h_rubin(f$est, f$vcov, 0), "dfcom")
  expect_error(.sr_h_rubin(lapply(f$est, unname), f$vcov, f$dfcom), "named")
})

test_that(".h_rubin with no between-imputation variance (B = 0) has no NaN: df = df_obs, or Inf", {
  est <- c(a = 1.5, b = -0.25)
  V <- matrix(c(0.04, 0.01, 0.01, 0.09), 2L, dimnames = list(names(est), names(est)))
  got <- .sr_h_rubin(rep(list(est), 5L), rep(list(V), 5L), 20)
  expect_equal(got$est, unname(est), tolerance = 1e-14)
  expect_equal(got$se, c(0.2, 0.3), tolerance = 1e-14)
  expect_equal(got$df, rep(21 / 23 * 20, 2L), tolerance = 1e-12)
  expect_identical(got$riv, c(0, 0))
  expect_identical(got$lambda, c(0, 0))
  expect_identical(got$mcse, c(0, 0))
  expect_equal(got$fmi, 2 / (got$df + 3), tolerance = 1e-12)
  expect_false(anyNA(got[c("est", "se", "lo", "hi", "p", "df", "riv", "lambda", "fmi", "mcse")]))
  inf <- .sr_h_rubin(rep(list(est), 5L), rep(list(V), 5L), Inf)
  expect_identical(inf$df, c(Inf, Inf))
  expect_identical(inf$fmi, c(0, 0))
  expect_equal(inf$lo, unname(est) - stats::qnorm(0.975) * c(0.2, 0.3), tolerance = 1e-12)
  expect_false(anyNA(inf[c("est", "se", "lo", "hi", "p", "df", "riv", "lambda", "fmi", "mcse")]))
})

test_that(".h_rubin inlines into emitted code under its .h_ name", {
  code <- .sr_inline(.sr_h_rubin, ".h_rubin")
  expect_silent(.sr_emit_check(code, "analysis"))
  env <- new.env(parent = globalenv())
  eval(parse(text = code), envir = env)
  f <- .pl_fits()
  expect_identical(env$.h_rubin(f$est, f$vcov, f$dfcom), .sr_h_rubin(f$est, f$vcov, f$dfcom))
})

test_that(".h_auto_m is von Hippel's two-stage rule, floored at the pilot and capped at 200", {
  one <- function(fmi) {
    up <- stats::plogis(stats::qlogis(fmi) + 1.96 * sqrt(2 / 20))
    ceiling(1 + 0.5 * (up / 0.05)^2)
  }
  for (fmi in c(0.05, 0.2, 0.35, 0.5)) {
    a <- .sr_h_auto_m(fmi)
    expect_identical(a$m_pilot, 20L)
    expect_identical(a$fmi_max, fmi)
    expect_identical(a$m_final, as.integer(min(200, max(20, one(fmi)))), info = fmi)
    expect_identical(a$m_capped, one(fmi) > 200, info = fmi)
  }
  expect_identical(.sr_h_auto_m(0)$m_final, 20L)
  expect_false(.sr_h_auto_m(0)$m_capped)
  expect_identical(.sr_h_auto_m(0.9)$m_final, as.integer(one(0.9)))        # 180: the cap binds only near 1
  expect_false(.sr_h_auto_m(0.9)$m_capped)
  expect_identical(.sr_h_auto_m(0.999)$m_final, 200L)
  expect_true(.sr_h_auto_m(0.999)$m_capped)
  expect_identical(.sr_h_auto_m(1)$m_final, 200L)
  expect_error(.sr_h_auto_m(NA_real_), "fmi")
  expect_error(.sr_h_auto_m(1.2), "fmi")
  code <- .sr_inline(.sr_h_auto_m, ".h_auto_m")
  expect_silent(.sr_emit_check(code, "analysis"))
})
