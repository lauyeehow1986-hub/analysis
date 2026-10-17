# Tests for routes/marginal.R: g-computation (.sr_h_gcomp), NNT (.sr_h_nnt), the Wilson interval
# (.sr_h_wilson) and the marginal mean difference of the lm route. Synthetic data only (seeded);
# projects live under withr::local_tempdir().

.mg_df <- function(n = 600L, seed = 11L) {
  withr::with_seed(seed, {
    x1 <- stats::rnorm(n, mean = 1)
    x2 <- stats::rbinom(n, 1L, 0.4)
    a <- stats::rbinom(n, 1L, stats::plogis(0.3 * x1 - 0.4 * x2))
    y <- stats::rbinom(n, 1L, stats::plogis(-0.8 + 0.7 * a + 0.5 * x1 + 0.3 * x2 + 0.4 * a * x1))
    g <- factor(sample(c("lo", "mid", "hi"), n, replace = TRUE), levels = c("lo", "mid", "hi"))
    z <- 1 + 0.5 * a + 0.3 * x1 - 0.2 * x2 + 0.4 * a * x1 + stats::rnorm(n)
    data.frame(y = y, a = a, x1 = x1, x2 = x2, g = g, z = z)
  })
}

# Counterfactual mean prediction by hand: the exposure column set to `value` on `over`.
.mg_hand_mean <- function(fit, over, col, value) {
  d <- over
  d[[col]] <- if (is.factor(over[[col]])) factor(rep(value, nrow(d)), levels = levels(over[[col]])) else value
  mean(stats::predict(fit, newdata = d, type = "response"))
}

# ---- .h_gcomp -----------------------------------------------------------------------------------

test_that(".h_gcomp equals a hand computation on counterfactual copies to 1e-12 (logistic)", {
  d <- .mg_df()
  fit <- stats::glm(y ~ a + x1 + x2, data = d, family = stats::binomial())
  g <- .sr_h_gcomp(fit, d, "a", c(1, 0), stats::vcov(fit))
  expect_identical(names(g), c("p1", "p0", "rd", "rd_se", "log_rr", "log_rr_se"))
  p1 <- .mg_hand_mean(fit, d, "a", 1)
  p0 <- .mg_hand_mean(fit, d, "a", 0)
  expect_equal(g$p1, p1, tolerance = 1e-12)
  expect_equal(g$p0, p0, tolerance = 1e-12)
  expect_equal(g$rd, p1 - p0, tolerance = 1e-12)
  expect_equal(g$log_rr, log(p1 / p0), tolerance = 1e-12)
})

test_that(".h_gcomp delta-method SEs equal the quadratic form g^T V g with a numerical gradient", {
  d <- .mg_df()
  fit <- stats::glm(y ~ a + x1 + x2, data = d, family = stats::binomial())
  V <- stats::vcov(fit)
  g <- .sr_h_gcomp(fit, d, "a", c(1, 0), V)
  b <- stats::coef(fit)
  d1 <- transform(d, a = 1)
  d0 <- transform(d, a = 0)
  X1 <- stats::model.matrix(~ a + x1 + x2, d1)
  X0 <- stats::model.matrix(~ a + x1 + x2, d0)
  mu <- function(X, beta) mean(stats::plogis(X %*% beta))
  grad <- function(f) vapply(seq_along(b), function(j) {
    h <- 1e-6
    up <- b
    dn <- b
    up[j] <- up[j] + h
    dn[j] <- dn[j] - h
    (f(up) - f(dn)) / (2 * h)
  }, 0)
  g_rd <- grad(function(beta) mu(X1, beta) - mu(X0, beta))
  g_lrr <- grad(function(beta) log(mu(X1, beta) / mu(X0, beta)))
  expect_equal(g$rd_se, sqrt(drop(t(g_rd) %*% V %*% g_rd)), tolerance = 1e-6)
  expect_equal(g$log_rr_se, sqrt(drop(t(g_lrr) %*% V %*% g_lrr)), tolerance = 1e-6)
})

test_that("the delta-method RD SE is within 5% of a seeded 2000-replicate bootstrap SE", {
  d <- .mg_df(2000L, seed = 5L)
  fit <- stats::glm(y ~ a + x1 + x2, data = d, family = stats::binomial())
  V <- stats::vcov(fit)
  g <- .sr_h_gcomp(fit, d, "a", c(1, 0), V)
  rd <- withr::with_seed(2024L, vapply(seq_len(2000L), function(i) {
    b <- d[sample.int(nrow(d), replace = TRUE), ]
    f <- suppressWarnings(stats::glm(y ~ a + x1 + x2, data = b, family = stats::binomial()))
    .sr_h_gcomp(f, b, "a", c(1, 0), V)$rd
  }, 0))
  expect_lt(abs(stats::sd(rd) / g$rd_se - 1), 0.05)
})

test_that(".h_gcomp standardises over `over` when given, and with interactions in the model", {
  d <- .mg_df()
  fit <- stats::glm(y ~ a * x1 + x2, data = d, family = stats::binomial())
  over <- d[d$x2 == 1, ]
  g <- .sr_h_gcomp(fit, d, "a", c(1, 0), stats::vcov(fit), over = over)
  expect_equal(g$p1, .mg_hand_mean(fit, over, "a", 1), tolerance = 1e-12)
  expect_equal(g$p0, .mg_hand_mean(fit, over, "a", 0), tolerance = 1e-12)
  g_all <- .sr_h_gcomp(fit, d, "a", c(1, 0), stats::vcov(fit))
  expect_equal(g_all$p1, .mg_hand_mean(fit, d, "a", 1), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(g$rd, g_all$rd)))
  # the levels order is (exposed, reference): swapping them negates the RD
  sw <- .sr_h_gcomp(fit, d, "a", c(0, 1), stats::vcov(fit))
  expect_equal(sw$rd, -g_all$rd, tolerance = 1e-12)
  expect_equal(sw$rd_se, g_all$rd_se, tolerance = 1e-12)
})

test_that(".h_gcomp with over = NULL standardises over data, like the default", {
  d <- .mg_df()
  fit <- stats::glm(y ~ a * x1 + x2, data = d, family = stats::binomial())
  V <- stats::vcov(fit)
  expect_identical(.sr_h_gcomp(fit, d, "a", c(1, 0), V, over = NULL), .sr_h_gcomp(fit, d, "a", c(1, 0), V, over = d))
})

test_that(".h_gcomp sets a factor exposure to each level", {
  d <- .mg_df()
  fit <- stats::glm(y ~ g + x1 + x2, data = d, family = stats::binomial())
  g <- .sr_h_gcomp(fit, d, "g", c("hi", "lo"), stats::vcov(fit))
  expect_equal(g$p1, .mg_hand_mean(fit, d, "g", "hi"), tolerance = 1e-12)
  expect_equal(g$p0, .mg_hand_mean(fit, d, "g", "lo"), tolerance = 1e-12)
  fit2 <- stats::glm(y ~ g * x1, data = d, family = stats::binomial())
  g2 <- .sr_h_gcomp(fit2, d, "g", c("mid", "lo"), stats::vcov(fit2))
  expect_equal(g2$p1, .mg_hand_mean(fit2, d, "g", "mid"), tolerance = 1e-12)
})

test_that(".h_gcomp works for a log-link glm and gives the mean difference for an lm", {
  d <- .mg_df()
  fit <- stats::glm(y ~ a + x1 + x2, data = d, family = stats::poisson(link = "log"))
  g <- .sr_h_gcomp(fit, d, "a", c(1, 0), sandwich::vcovHC(fit, type = "HC0"))
  expect_equal(g$p1, .mg_hand_mean(fit, d, "a", 1), tolerance = 1e-12)
  expect_equal(g$p0, .mg_hand_mean(fit, d, "a", 0), tolerance = 1e-12)
  lmf <- stats::lm(z ~ a + x1 + x2, data = d)
  m <- .sr_h_gcomp(lmf, d, "a", c(1, 0), stats::vcov(lmf))
  # identity link and no exposure interaction: the marginal MD is the coefficient, SE included
  expect_equal(m$rd, unname(stats::coef(lmf)["a"]), tolerance = 1e-12)
  expect_equal(m$rd_se, unname(sqrt(stats::vcov(lmf)["a", "a"])), tolerance = 1e-12)
  expect_equal(m$p1 - m$p0, m$rd, tolerance = 1e-12)
  # with an exposure x covariate interaction it is the average of the counterfactual differences
  lmi <- stats::lm(z ~ a * x1 + x2, data = d)
  mi <- .sr_h_gcomp(lmi, d, "a", c(1, 0), stats::vcov(lmi))
  by_hand <- mean(stats::predict(lmi, transform(d, a = 1))) - mean(stats::predict(lmi, transform(d, a = 0)))
  expect_equal(mi$rd, by_hand, tolerance = 1e-12)
  expect_gt(abs(mi$rd - unname(stats::coef(lmi)["a"])), 0.01)
})

test_that(".h_gcomp drops aliased coefficients and aligns V by name", {
  d <- .mg_df()
  d$x1b <- 2 * d$x1
  fit <- stats::glm(y ~ a + x1 + x1b + x2, data = d, family = stats::binomial())
  expect_true(anyNA(stats::coef(fit)))
  Vfull <- stats::vcov(fit)                                    # NA-filled for the aliased coefficient
  expect_identical(nrow(Vfull), length(stats::coef(fit)))
  ok <- !is.na(stats::coef(fit))
  V <- stats::vcov(fit, complete = FALSE)
  expect_identical(dim(V), c(sum(ok), sum(ok)))
  g <- .sr_h_gcomp(fit, d, "a", c(1, 0), V)
  expect_identical(.sr_h_gcomp(fit, d, "a", c(1, 0), Vfull), g)   # an NA-filled full V is accepted too
  ref <- stats::glm(y ~ a + x1 + x2, data = d, family = stats::binomial())
  gr <- .sr_h_gcomp(ref, d, "a", c(1, 0), stats::vcov(ref))
  expect_equal(g$rd, gr$rd, tolerance = 1e-10)
  expect_equal(g$rd_se, gr$rd_se, tolerance = 1e-8)
})

test_that(".h_gcomp is a pure function of its arguments and refuses bad input", {
  d <- .mg_df()
  fit <- stats::glm(y ~ a + x1 + x2, data = d, family = stats::binomial())
  V <- stats::vcov(fit)
  expect_identical(.sr_h_gcomp(fit, d, "a", c(1, 0), V), .sr_h_gcomp(fit, d, "a", c(1, 0), V))
  expect_error(.sr_h_gcomp(fit, d, "nope", c(1, 0), V), "exposure")
  expect_error(.sr_h_gcomp(fit, d, "a", 1, V), "levels")
  expect_error(.sr_h_gcomp(fit, d, "a", c(1, 0), V[1:2, 1:2]), "V")
  d2 <- d
  d2$x1[3] <- NA
  expect_error(.sr_h_gcomp(fit, d2, "a", c(1, 0), V), "missing")
  expect_error(.sr_h_gcomp(fit, d, "a", c(1, 0), V, over = d[0, ]), "over")
})

test_that("the three helpers inline into emitted code and give the same numbers", {
  code <- paste(.sr_inline(.sr_h_gcomp, ".h_gcomp"), .sr_inline(.sr_h_nnt, ".h_nnt"),
                .sr_inline(.sr_h_wilson, ".h_wilson"), sep = "\n")
  expect_silent(.sr_emit_check(code, "analysis"))
  env <- new.env(parent = globalenv())
  eval(parse(text = code), envir = env)
  d <- .mg_df()
  fit <- stats::glm(y ~ a + x1 + x2, data = d, family = stats::binomial())
  expect_identical(env$.h_gcomp(fit, d, "a", c(1, 0), stats::vcov(fit)),
                   .sr_h_gcomp(fit, d, "a", c(1, 0), stats::vcov(fit)))
  expect_identical(env$.h_nnt(0.1, 0.05, 0.15), .sr_h_nnt(0.1, 0.05, 0.15))
  expect_identical(env$.h_wilson(5, 10), .sr_h_wilson(5, 10))
})

# ---- .h_nnt, .h_wilson --------------------------------------------------------------------------

test_that(".h_nnt: a CI that excludes 0 gives NNTB or NNTH with bounds 1/hi .. 1/lo", {
  n <- .sr_h_nnt(0.1, 0.05, 0.15)
  expect_equal(n$nnt, 10, tolerance = 1e-12)
  expect_equal(n$lo, 1 / 0.15, tolerance = 1e-12)
  expect_equal(n$hi, 20, tolerance = 1e-12)
  expect_match(n$label, "^NNTB 10 ")
  expect_match(n$label, "6.7 to 20", fixed = TRUE)
  h <- .sr_h_nnt(-0.1, -0.15, -0.05)
  expect_equal(h$nnt, -10, tolerance = 1e-12)
  expect_equal(c(h$lo, h$hi), c(-20, -1 / 0.15), tolerance = 1e-12)
  expect_match(h$label, "^NNTH 10 ")
})

test_that(".h_nnt: a CI that crosses 0 gives the Altman label and no numeric bounds", {
  n <- .sr_h_nnt(0.05, -0.02, 0.12)
  expect_equal(n$nnt, 20, tolerance = 1e-12)
  expect_true(is.na(n$lo) && is.na(n$hi))
  expect_match(n$label, "NNTB 8.3 to infinity to NNTH 50", fixed = TRUE)
  expect_match(n$label, "^NNTB 20 ")
  z <- .sr_h_nnt(0, -0.02, 0.02)
  expect_true(is.na(z$nnt))
  expect_match(z$label, "infinity")
  expect_error(.sr_h_nnt(0.1, 0.2, 0.15), "lo")
  expect_error(.sr_h_nnt(c(0.1, 0.2), 0.05, 0.15), "one")
})

test_that(".h_wilson matches the pinned values and is clamped to [0, 1]", {
  w0 <- .sr_h_wilson(0, 10)
  expect_identical(names(w0), c("est", "lo", "hi"))
  expect_equal(unname(w0), c(0, 0, 0.2775), tolerance = 1e-4)
  expect_equal(unname(.sr_h_wilson(5, 10)), c(0.5, 0.2366, 0.7634), tolerance = 1e-4)
  expect_equal(unname(.sr_h_wilson(10, 10))[3], 1, tolerance = 1e-12)
  w9 <- .sr_h_wilson(7, 20, level = 0.9)
  z <- stats::qnorm(0.95)
  ctr <- (0.35 + z^2 / 40) / (1 + z^2 / 20)
  half <- z * sqrt(0.35 * 0.65 / 20 + z^2 / 1600) / (1 + z^2 / 20)
  expect_equal(unname(w9), c(0.35, ctr - half, ctr + half), tolerance = 1e-12)
  expect_true(all(is.na(.sr_h_wilson(0, 0))))
  expect_error(.sr_h_wilson(11, 10), "x")
  expect_error(.sr_h_wilson(1, 10, level = 1), "level")
})

# ---- the lm route: marginal mean difference (ruling R3) -----------------------------------------

.mgl_layers <- function() {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = FALSE)
}

.mgl_spec <- function(...) {
  s <- list(purpose = "explain-causal", outcome = "z", exposure = "a",
            terms = list(list(var = "x1"), list(var = "x2")),
            marginal = TRUE, layers = .mgl_layers(), seed = 1L)
  mod <- list(...)
  for (nm in names(mod)) s[nm] <- list(mod[[nm]])
  s
}

.mgl_route <- function() {
  if (!(.sr_route_key("lm", 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(.sr_lm_route())
  sr_route_get("lm", 1L)
}

.mgl_value <- function(df, spec) {
  route <- .mgl_route()
  code <- sr_compile(route, spec)
  expect_silent(.sr_emit_check(code, "analysis"))
  env <- new.env(parent = globalenv())
  env$sr_data <- df
  eval(parse(text = code), envir = env)
}

.mgl_present <- function(value, spec) {
  r <- .mgl_route()
  sr_result_check(r$present(value, spec), r)
}

test_that("lm marginal = TRUE is accepted; without exposure interactions the marginal MD is the coefficient", {
  r <- .mgl_route()
  expect_true(sr_spec_validate(.mgl_spec(), r)$ok)
  d <- .mg_df()
  s <- .mgl_spec(se = "robust")
  pr <- .mgl_present(.mgl_value(d, s), s)
  e <- pr$effects
  fit <- stats::lm(z ~ a + x1 + x2, data = d)
  mg <- e[e$measure == "MD" & !e$conditional, ]
  cf <- e[e$term == "a", ]
  expect_identical(nrow(mg), 2L)                                  # the coefficient row and the marginal row
  m <- mg[grepl("marginal", mg$term), ]
  expect_identical(nrow(m), 1L)
  expect_identical(m$role, "primary")
  expect_equal(m$est, unname(stats::coef(fit)["a"]), tolerance = 1e-10)
  expect_equal(m$est, cf$est, tolerance = 1e-10)
  se <- sqrt(sandwich::vcovHC(fit, type = "HC3")["a", "a"])
  tq <- stats::qt(0.975, fit$df.residual)
  expect_equal(m$lo, unname(stats::coef(fit)["a"]) - tq * se, tolerance = 1e-8)
  expect_equal(m$hi, unname(stats::coef(fit)["a"]) + tq * se, tolerance = 1e-8)
  expect_identical(m$scale, "difference")
  expect_match(m$ci_method, "delta")
  expect_true("marginal" %in% names(pr$tables))
})

test_that("lm marginal MD with an exposure x covariate interaction equals the hand counterfactual means", {
  d <- .mg_df()
  s <- .mgl_spec(interactions = list(c("a", "x1")))
  val <- .mgl_value(d, s)
  fit <- stats::lm(z ~ a * x1 + x2, data = d)
  hand <- mean(stats::predict(fit, transform(d, a = 1))) - mean(stats::predict(fit, transform(d, a = 0)))
  pr <- .mgl_present(val, s)
  m <- pr$effects[grepl("marginal", pr$effects$term), ]
  expect_equal(m$est, hand, tolerance = 1e-12)
  cf <- pr$effects[pr$effects$term == "a", ]
  expect_true(cf$conditional)                                    # the coefficient is the effect at x1 = 0
  expect_false(m$conditional)
  expect_gt(abs(m$est - cf$est), 0.01)
  # the delta SE is the SE of that average, from the quadratic form g^T V g
  X1 <- stats::model.matrix(~ a * x1 + x2, transform(d, a = 1))
  X0 <- stats::model.matrix(~ a * x1 + x2, transform(d, a = 0))
  gr <- colMeans(X1 - X0)
  se <- sqrt(drop(t(gr) %*% stats::vcov(fit) %*% gr))
  tq <- stats::qt(0.975, fit$df.residual)
  expect_equal(m$hi - m$lo, 2 * tq * se, tolerance = 1e-8)
})

test_that("lm standardize_over = all adds the rows lost only to a missing outcome", {
  d <- .mg_df()
  d$z[c(4, 9, 15, 30, 31)] <- NA                                 # planted missing outcomes
  d$x1[c(2, 3)] <- NA                                            # missing covariates: dropped from both
  s_all <- .mgl_spec(interactions = list(c("a", "x1")), standardize_over = "all")
  s_an <- .mgl_spec(interactions = list(c("a", "x1")), standardize_over = "analysis")
  cc <- stats::complete.cases(d[c("z", "a", "x1", "x2")])
  cov_ok <- stats::complete.cases(d[c("a", "x1", "x2")])
  expect_gt(sum(cov_ok), sum(cc))
  fit <- stats::lm(z ~ a * x1 + x2, data = d[cc, ])
  by_hand <- function(over) mean(stats::predict(fit, transform(over, a = 1))) -
    mean(stats::predict(fit, transform(over, a = 0)))
  v_all <- .mgl_value(d, s_all)
  v_an <- .mgl_value(d, s_an)
  m_all <- .mgl_present(v_all, s_all)$effects
  m_an <- .mgl_present(v_an, s_an)$effects
  expect_equal(m_all$est[grepl("marginal", m_all$term)], by_hand(d[cov_ok, ]), tolerance = 1e-12)
  expect_equal(m_an$est[grepl("marginal", m_an$term)], by_hand(d[cc, ]), tolerance = 1e-12)
  expect_equal(v_all$marginal$n_over, sum(cov_ok))
  expect_equal(v_an$marginal$n_over, sum(cc))
  expect_equal(v_all$n, sum(cc))                                 # the fit is on the complete cases either way
  expect_equal(v_all$coef$est, v_an$coef$est, tolerance = 1e-12)
})

test_that("lm marginal MD handles a factor exposure and refuses a continuous one", {
  d <- .mg_df()
  s <- .mgl_spec(exposure = "g", exposure_ref = "lo", terms = list(list(var = "x1")))
  val <- .mgl_value(d, s)
  pr <- .mgl_present(val, s)
  fit <- stats::lm(z ~ g + x1, data = d)
  mg <- pr$effects[grepl("marginal", pr$effects$term), ]
  expect_identical(nrow(mg), 2L)
  expect_identical(mg$level, c("mid", "hi"))
  expect_equal(mg$est, unname(stats::coef(fit)[c("gmid", "ghi")]), tolerance = 1e-10)
  s2 <- .mgl_spec(exposure = "x1", terms = list(list(var = "x2")))
  expect_error(.mgl_value(d, s2), "marginal")
  expect_error(.mgl_value(d, s2), "binary|categorical")
})

test_that("lm marginal = FALSE (the default) adds no marginal rows", {
  d <- .mg_df()
  s <- .mgl_spec(marginal = FALSE)
  pr <- .mgl_present(.mgl_value(d, s), s)
  expect_false(any(grepl("marginal", pr$effects$term)))
  expect_false("marginal" %in% names(pr$tables))
  s2 <- .mgl_spec()
  s2$marginal <- NULL
  pr2 <- .mgl_present(.mgl_value(d, s2), s2)
  expect_false(any(grepl("marginal", pr2$effects$term)))
})
