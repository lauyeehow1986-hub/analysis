# Tests for routes/glm.R: logistic@1 (with its describe twin) and modpois@1. Synthetic data only
# (seeded); projects live under withr::local_tempdir().

.gl_layers <- function(clustered = FALSE) {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = clustered)
}

# y (0/1), a (0/1 exposure), x1, x2, g (3-level factor), status (text outcome), cl (cluster id)
.gl_df <- function(n = 500L, seed = 21L) {
  withr::with_seed(seed, {
    x1 <- stats::rnorm(n)
    x2 <- stats::rbinom(n, 1L, 0.4)
    a <- stats::rbinom(n, 1L, stats::plogis(0.3 * x1 - 0.4 * x2))
    y <- stats::rbinom(n, 1L, stats::plogis(-1 + 0.7 * a + 0.5 * x1 + 0.3 * x2))
    g <- factor(sample(c("lo", "mid", "hi"), n, replace = TRUE), levels = c("lo", "mid", "hi"))
    data.frame(y = y, a = a, x1 = x1, x2 = x2, g = g,
               status = ifelse(y == 1L, "dead", "alive"), cl = sample(1:25, n, replace = TRUE),
               stringsAsFactors = FALSE)
  })
}

.gl_spec <- function(se = NULL, purpose = "explain-causal", ...) {
  s <- list(purpose = purpose, outcome = "y", exposure = "a",
            terms = list(list(var = "x1"), list(var = "x2")),
            layers = .gl_layers(), seed = 42L)
  if (!is.null(se)) {
    s$se <- se
    if (identical(se, "cluster")) {
      s$cluster <- "cl"
      s$layers <- .gl_layers(clustered = TRUE)
    }
  }
  mod <- list(...)
  for (nm in names(mod)) s[nm] <- list(mod[[nm]])
  s
}

# The routes, registered again when an earlier test file emptied the registry.
.gl_route <- function(id = "logistic") {
  reg <- function(key, make) {
    if (!(.sr_route_key(key, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make())
  }
  reg("logistic", .sr_logistic_route)
  reg("logistic_describe", .sr_logistic_describe_route)
  reg("modpois", .sr_modpois_route)
  sr_route_get(id, 1L)
}

.gl_value <- function(df, spec = .gl_spec(), id = "logistic") {
  route <- .gl_route(id)
  code <- sr_compile(route, spec)
  expect_silent(.sr_emit_check(code, "analysis"))
  env <- new.env(parent = globalenv())
  env$sr_data <- df
  eval(parse(text = code), envir = env)
}

.gl_present <- function(value, spec = .gl_spec(), id = "logistic") {
  r <- .gl_route(id)
  sr_result_check(r$present(value, spec), r)
}

# the effects rows of one measure whose term contains `pat`
.gl_rows <- function(e, measure, pat = "") e[e$measure == measure & grepl(pat, e$term, fixed = TRUE), , drop = FALSE]

# the effects rows without the descriptive observed-risk rows
.gl_cf <- function(e) e[e$measure != "risk", , drop = FALSE]


# ---- registration and validation ----------------------------------------------------------------

test_that("logistic, logistic_describe and modpois are registered with the documented schema", {
  r <- .gl_route("logistic")
  d <- .gl_route("logistic_describe")
  m <- .gl_route("modpois")
  expect_identical(r$family, "regression")
  expect_identical(r$purpose, "explain-causal")
  expect_identical(d$purpose, "describe")
  expect_identical(m$purpose, "explain-causal")
  base <- c("outcome", "event_value", "exposure", "terms", "interactions", "se", "cluster", "marginal",
            "standardize_over", "layers", "seed")
  expect_true(all(base %in% names(r$spec_schema)))
  expect_true("penalty" %in% names(r$spec_schema))
  expect_false("penalty" %in% names(m$spec_schema))
  expect_identical(d$spec_schema, r$spec_schema)
  expect_identical(r$spec_schema$penalty$values, c("none", "firth"))
  expect_identical(r$spec_schema$standardize_over$values, c("analysis", "all"))
  expect_identical(r$spec_schema$marginal$type, "lgl")
  expect_identical(r$spec_schema$event_value$type, "chr")
  expect_false(isTRUE(r$spec_schema$event_value$required))
  expect_length(formals(r$compile), 1L)
  expect_identical(r$compile, d$compile)
  expect_identical(r$present, d$present)
  est <- r$estimand(.gl_spec())
  expect_s3_class(est, "sr_estimand")
  expect_identical(est$level, "marginal")
  expect_identical(r$estimand(.gl_spec(marginal = FALSE))$level, "conditional")
  expect_true(sr_spec_validate(.gl_spec(), r)$ok)
  expect_true(sr_spec_validate(.gl_spec(purpose = "describe"), d)$ok)
  expect_true(sr_spec_validate(.gl_spec(se = "robust"), m)$ok)
  expect_true(sr_spec_validate(.gl_spec(), m)$ok)
})

test_that("modified Poisson refuses a model-based SE; logistic accepts any SE", {
  m <- .gl_route("modpois")
  v <- sr_spec_validate(.gl_spec(se = "model"), m)
  expect_false(v$ok)
  expect_match(paste(v$errors, collapse = " "), "robust or cluster", fixed = TRUE)
  expect_error(sr_compile(m, .gl_spec(se = "model")), "robust or cluster")
  expect_true(sr_spec_validate(.gl_spec(se = "cluster"), m)$ok)
  bad <- .gl_spec(se = "cluster")
  bad$cluster <- NULL
  expect_false(sr_spec_validate(bad, m)$ok)
  expect_false(sr_spec_validate(.gl_spec(penalty = "firth"), m)$ok)
  r <- .gl_route("logistic")
  for (se in c("model", "robust", "cluster")) expect_true(sr_spec_validate(.gl_spec(se = se), r)$ok, info = se)
  expect_false(sr_spec_validate(.gl_spec(se = "bootstrap"), r)$ok)
})

test_that("Firth: marginal effects and robust SEs are refused when asked for", {
  r <- .gl_route("logistic")
  expect_true(sr_spec_validate(.gl_spec(penalty = "firth"), r)$ok)
  expect_true(sr_spec_validate(.gl_spec(penalty = "firth", marginal = FALSE), r)$ok)
  expect_identical(r$estimand(.gl_spec(penalty = "firth"))$level, "conditional")
  v <- sr_spec_validate(.gl_spec(penalty = "firth", marginal = TRUE), r)
  expect_false(v$ok)
  expect_match(paste(v$errors, collapse = " "), "Firth", fixed = TRUE)
  expect_false(sr_spec_validate(.gl_spec(penalty = "firth", se = "robust"), r)$ok)
  expect_false(sr_spec_validate(.gl_spec(penalty = "firth", se = "cluster"), r)$ok)
  expect_true(sr_spec_validate(.gl_spec(penalty = "firth", se = "model"), r)$ok)
  expect_false(sr_spec_validate(.gl_spec(penalty = "ridge"), r)$ok)
})

test_that("event_value must be one value; a missing one on a non-0/1 outcome is refused at run time", {
  r <- .gl_route("logistic")
  expect_true(sr_spec_validate(.gl_spec(outcome = "status", event_value = "dead"), r)$ok)
  v <- sr_spec_validate(.gl_spec(outcome = "status", event_value = c("dead", "alive")), r)
  expect_false(v$ok)
  expect_match(paste(v$errors, collapse = " "), "event_value")
  expect_false(sr_spec_validate(.gl_spec(event_value = ""), r)$ok)
  s <- .gl_spec(outcome = "status")
  expect_true(sr_spec_validate(s, r)$ok)
  expect_error(.gl_value(.gl_df(), s), "event_value")
  expect_silent(.gl_value(.gl_df()))
  d <- .gl_df()
  d$y <- d$y == 1L
  expect_silent(.gl_value(d))
  d2 <- .gl_df()
  d2$y <- d2$y * 2
  expect_error(.gl_value(d2), "event_value")
})

test_that("unimplemented layers are refused at compile", {
  r <- .gl_route("logistic")
  for (lay in list(list(missing = "mi"), list(weights = "ipw"), list(matching = TRUE),
                   list(resampling = "bootstrap"), list(framework = "bayesian"))) {
    s <- .gl_spec(layers = modifyList(.gl_layers(), lay))
    expect_error(sr_compile(r, s), "layer", info = names(lay))
    expect_error(sr_compile(.gl_route("modpois"), s), "layer", info = names(lay))
  }
})

test_that("compile is deterministic and passes the emit check; JSON state compiles identically", {
  r <- .gl_route("logistic")
  a <- sr_compile(r, .gl_spec())
  expect_identical(a, sr_compile(r, .gl_spec()))
  expect_silent(.sr_emit_check(a, "analysis"))
  expect_false(identical(a, sr_compile(r, .gl_spec(se = "robust"))))
  s <- .gl_spec(se = "cluster", outcome = "status", event_value = "dead", interactions = list(c("a", "x1")))
  back <- .sr_spec_from_state(jsonlite::fromJSON(jsonlite::toJSON(s, auto_unbox = TRUE), simplifyVector = FALSE))
  expect_true(sr_spec_validate(back, r)$ok)
  expect_identical(sr_compile(r, back), sr_compile(r, s))
  expect_silent(.sr_emit_check(sr_compile(r, .gl_spec(penalty = "firth")), "analysis"))
  expect_silent(.sr_emit_check(sr_compile(.gl_route("modpois"), .gl_spec()), "analysis"))
})

# ---- numbers: logistic --------------------------------------------------------------------------

test_that("logistic equals glm: coefficients, model-based z intervals, Wald p; OR rows are exp()", {
  d <- .gl_df()
  s <- .gl_spec(marginal = FALSE)
  val <- .gl_value(d, s)
  fit <- stats::glm(y ~ a + x1 + x2, data = d, family = stats::binomial())
  expect_equal(unname(val$coef$est), unname(stats::coef(fit)), tolerance = 1e-10)
  expect_equal(val$coef$se, unname(sqrt(diag(stats::vcov(fit)))), tolerance = 1e-10)
  ci <- stats::confint.default(fit)
  expect_equal(val$coef$lo, unname(ci[, 1]), tolerance = 1e-10)
  expect_equal(val$coef$hi, unname(ci[, 2]), tolerance = 1e-10)
  expect_identical(val$n, 500L)
  expect_equal(val$events, sum(d$y))
  pr <- .gl_present(val, s)
  e <- .gl_cf(pr$effects)
  expect_identical(e$term, c("a", "x1", "x2"))
  expect_true(all(e$measure == "OR"))
  expect_true(all(e$scale == "ratio"))
  expect_equal(e$est, exp(unname(stats::coef(fit))[-1]), tolerance = 1e-10)
  expect_equal(e$lo, exp(unname(ci[-1, 1])), tolerance = 1e-10)
  expect_equal(e$hi, exp(unname(ci[-1, 2])), tolerance = 1e-10)
  expect_equal(e$p, unname(summary(fit)$coefficients[-1, 4]), tolerance = 1e-10)
  expect_identical(e$role, c("primary", "covariate", "covariate"))
  expect_identical(unique(e$ci_method), "z_model")
  expect_identical(e$conditional, c(TRUE, FALSE, FALSE))
  expect_true(all(e$n == 500))
  expect_true(any(pr$flags$flag == "non_collapsible" & pr$flags$term == "a"))
  expect_true(all(c("coefficients", "model_fit") %in% names(pr$tables)))
})

test_that("logistic robust and cluster SEs equal the sandwich HC0 / HC1 forms with z intervals", {
  d <- .gl_df()
  fit <- stats::glm(y ~ a + x1 + x2, data = d, family = stats::binomial())
  s <- .gl_spec(se = "robust", marginal = FALSE)
  val <- .gl_value(d, s)
  expect_equal(val$coef$se, unname(sqrt(diag(sandwich::vcovHC(fit, type = "HC0")))), tolerance = 1e-10)
  pr <- .gl_present(val, s)
  expect_identical(unique(.gl_cf(pr$effects)$ci_method), "z_robust_hc0")
  b <- unname(stats::coef(fit)["a"])
  se <- sqrt(sandwich::vcovHC(fit, type = "HC0")["a", "a"])
  expect_equal(.gl_cf(pr$effects)$lo[1], exp(b - stats::qnorm(0.975) * se), tolerance = 1e-10)
  sc <- .gl_spec(se = "cluster", marginal = FALSE)
  vc <- .gl_value(d, sc)
  expect_equal(vc$coef$se, unname(sqrt(diag(sandwich::vcovCL(fit, cluster = d$cl, type = "HC1")))),
               tolerance = 1e-10)
  expect_equal(vc$n_clusters, 25)
  expect_identical(unique(.gl_cf(.gl_present(vc, sc)$effects)$ci_method), "z_cluster_hc1")
})

test_that("a text outcome with an event_value gives the same fit as the 0/1 outcome", {
  d <- .gl_df()
  a <- .gl_value(d, .gl_spec(marginal = FALSE))
  b <- .gl_value(d, .gl_spec(outcome = "status", event_value = "dead", marginal = FALSE))
  expect_equal(a$coef$est, b$coef$est, tolerance = 1e-12)
  d$y <- d$y == 1L
  expect_equal(a$coef$est, .gl_value(d, .gl_spec(marginal = FALSE))$coef$est, tolerance = 1e-12)
})

test_that("rows with a missing outcome, term or cluster are dropped and counted", {
  d <- .gl_df()
  d$y[c(3, 8)] <- NA
  d$x1[12] <- NA
  s <- .gl_spec(marginal = FALSE)
  val <- .gl_value(d, s)
  cc <- stats::complete.cases(d[c("y", "a", "x1", "x2")])
  fit <- stats::glm(y ~ a + x1 + x2, data = d[cc, ], family = stats::binomial())
  expect_equal(val$coef$est, unname(stats::coef(fit)), tolerance = 1e-10)
  expect_identical(val$n, sum(cc))
  expect_identical(val$n_total, 500L)
  expect_match(.gl_present(val, s)$fragments$glm_note, "3 excluded", fixed = TRUE)
})

test_that("a factor exposure gives one OR per non-reference level and an exposure collinear with a term is refused", {
  d <- .gl_df()
  s <- .gl_spec(exposure = "g", exposure_ref = "lo", marginal = FALSE)
  val <- .gl_value(d, s)
  fit <- stats::glm(y ~ factor(g, levels = c("lo", "mid", "hi")) + x1 + x2, data = d, family = stats::binomial())
  e <- .gl_cf(.gl_present(val, s)$effects)
  expect_identical(e$term[1:2], c("g: mid vs lo", "g: hi vs lo"))
  expect_identical(e$level[1:2], c("mid", "hi"))
  expect_identical(e$role[1:2], c("primary", "primary"))
  expect_equal(e$est[1:2], exp(unname(stats::coef(fit))[2:3]), tolerance = 1e-10)
  d$a <- 1                                                       # collinear with the intercept
  s2 <- .gl_spec(marginal = FALSE)
  expect_error(.gl_present(.gl_value(d, s2), s2), "exposure")
})

test_that("an exposure interaction marks the exposure OR conditional and adds a fragment", {
  d <- .gl_df()
  s <- .gl_spec(interactions = list(c("a", "x1")), marginal = FALSE)
  val <- .gl_value(d, s)
  fit <- stats::glm(y ~ a * x1 + x2, data = d, family = stats::binomial())
  pr <- .gl_present(val, s)
  e <- .gl_cf(pr$effects)
  expect_equal(e$est[e$term == "a"], exp(unname(stats::coef(fit)["a"])), tolerance = 1e-10)
  expect_true(e$conditional[e$term == "a"])
  expect_true("glm_interaction" %in% names(pr$fragments))
  expect_true("a:x1" %in% e$term)
})

# ---- numbers: modified Poisson, Firth, the describe twin ---------------------------------------

test_that("modpois equals a log-link Poisson glm with robust SEs; RR rows are exp()", {
  d <- .gl_df()
  s <- .gl_spec(marginal = FALSE)
  val <- .gl_value(d, s, "modpois")
  fit <- stats::glm(y ~ a + x1 + x2, data = d, family = stats::poisson(link = "log"))
  V <- sandwich::vcovHC(fit, type = "HC0")
  expect_equal(unname(val$coef$est), unname(stats::coef(fit)), tolerance = 1e-10)
  expect_equal(val$coef$se, unname(sqrt(diag(V))), tolerance = 1e-10)
  expect_identical(val$se_type, "robust")
  pr <- .gl_present(val, s, "modpois")
  e <- .gl_cf(pr$effects)
  expect_true(all(e$measure == "RR"))
  expect_equal(e$est, exp(unname(stats::coef(fit))[-1]), tolerance = 1e-10)
  z <- stats::qnorm(0.975)
  expect_equal(e$lo[1], exp(unname(stats::coef(fit)["a"]) - z * sqrt(V["a", "a"])), tolerance = 1e-10)
  expect_identical(unique(e$ci_method), "z_robust_hc0")
  expect_false(any(pr$flags$flag == "non_collapsible"))
  # an explicit cluster SE is honoured
  sc <- .gl_spec(se = "cluster", marginal = FALSE)
  vc <- .gl_value(d, sc, "modpois")
  expect_equal(vc$coef$se, unname(sqrt(diag(sandwich::vcovCL(fit, cluster = d$cl, type = "HC1")))),
               tolerance = 1e-10)
  expect_match(.gl_present(vc, sc, "modpois")$fragments$glm_note, "Poisson", fixed = TRUE)
})

test_that("Firth logistic equals logistf with profile-likelihood intervals and gives no marginal rows", {
  d <- .gl_df(120L, seed = 3L)
  s <- .gl_spec(penalty = "firth")
  val <- .gl_value(d, s)
  fit <- logistf::logistf(y ~ a + x1 + x2, data = d)
  expect_equal(unname(val$coef$est), unname(fit$coefficients), tolerance = 1e-8)
  expect_equal(val$coef$lo, unname(fit$ci.lower), tolerance = 1e-6)
  expect_equal(val$coef$hi, unname(fit$ci.upper), tolerance = 1e-6)
  expect_equal(val$coef$p, unname(fit$prob), tolerance = 1e-8)
  expect_identical(val$penalty, "firth")
  pr <- .gl_present(val, s)
  e <- .gl_cf(pr$effects)
  expect_identical(unique(e$ci_method), "profile_firth")
  expect_equal(e$est[1], exp(unname(fit$coefficients["a"])), tolerance = 1e-8)
  expect_equal(e$lo[1], exp(unname(fit$ci.lower["a"])), tolerance = 1e-6)
  expect_false(any(e$measure %in% c("RD", "NNT")))
  expect_false("marginal" %in% names(pr$tables))
  expect_match(pr$fragments$glm_note, "Firth", fixed = TRUE)
})

test_that("Firth handles a perfectly separated predictor that plain logistic regression cannot", {
  d <- .gl_df(80L, seed = 9L)
  d$x2 <- as.numeric(d$y == 1L & d$x1 > 0.2)
  s <- .gl_spec(penalty = "firth")
  val <- .gl_value(d, s)
  expect_true(all(is.finite(val$coef$est)))
  expect_true(all(is.finite(val$coef$lo)) && all(is.finite(val$coef$hi)))
})

test_that("the describe twin gives the same numbers as the explain route", {
  d <- .gl_df()
  s <- .gl_spec(marginal = FALSE)
  sd <- .gl_spec(purpose = "describe", marginal = FALSE)
  a <- .gl_present(.gl_value(d, s, "logistic"), s, "logistic")
  b <- .gl_present(.gl_value(d, sd, "logistic_describe"), sd, "logistic_describe")
  expect_identical(a$effects, b$effects)
  expect_true(any(a$flags$flag == "table2"))
  expect_false(any(b$flags$flag == "table2"))
})

# ---- marginal effects: RD, RR, NNT ---------------------------------------------------------------

test_that("logistic reports the g-computation RD, RR and NNT by default for a binary exposure", {
  d <- .gl_df()
  s <- .gl_spec()                                              # marginal not set: on by default
  val <- .gl_value(d, s)
  fit <- stats::glm(y ~ a + x1 + x2, data = d, family = stats::binomial())
  g <- .sr_h_gcomp(fit, d, "a", c(1, 0), stats::vcov(fit))
  pr <- .gl_present(val, s)
  e <- .gl_cf(pr$effects)
  z <- stats::qnorm(0.975)
  rd <- e[e$measure == "RD", ]
  rr <- e[e$measure == "RR", ]
  nn <- e[e$measure == "NNT", ]
  expect_identical(nrow(rd), 1L)
  expect_identical(nrow(rr), 1L)
  expect_identical(nrow(nn), 1L)
  expect_equal(rd$est, g$rd, tolerance = 1e-10)
  expect_equal(c(rd$lo, rd$hi), g$rd + c(-1, 1) * z * g$rd_se, tolerance = 1e-10)
  expect_equal(rd$p, 2 * stats::pnorm(-abs(g$rd / g$rd_se)), tolerance = 1e-10)
  expect_identical(rd$scale, "difference")
  expect_identical(rd$role, "primary")
  expect_false(rd$conditional)
  expect_match(rd$ci_method, "delta")
  expect_equal(rr$est, exp(g$log_rr), tolerance = 1e-10)
  expect_equal(c(rr$lo, rr$hi), exp(g$log_rr + c(-1, 1) * z * g$log_rr_se), tolerance = 1e-10)
  expect_identical(rr$scale, "ratio")
  expect_identical(rr$role, "primary")
  # the conditional OR stays, marked conditional, next to the marginal measures
  expect_identical(e$measure[e$term == "a"], "OR")
  expect_true(e$conditional[e$term == "a"])
  # the NNT row follows the RD CI (Altman)
  nt <- .sr_h_nnt(g$rd, rd$lo, rd$hi)
  expect_equal(nn$est, nt$nnt, tolerance = 1e-10)
  expect_identical(nn$role, "descriptive")
  expect_true("marginal" %in% names(pr$tables))
  expect_true("NNT (95% CI)" %in% names(pr$tables$marginal))
  expect_equal(val$marginal$n_over, 500)
})

test_that("modpois reports marginal measures from the log-link fit with the robust covariance", {
  d <- .gl_df()
  s <- .gl_spec()
  val <- .gl_value(d, s, "modpois")
  fit <- stats::glm(y ~ a + x1 + x2, data = d, family = stats::poisson(link = "log"))
  g <- .sr_h_gcomp(fit, d, "a", c(1, 0), sandwich::vcovHC(fit, type = "HC0"))
  e <- .gl_present(val, s, "modpois")$effects
  expect_equal(e$est[e$measure == "RD"], g$rd, tolerance = 1e-10)
  expect_equal(e$est[e$measure == "RR" & grepl("marginal", e$term)], exp(g$log_rr), tolerance = 1e-10)
  expect_equal(e$est[e$measure == "RR" & e$term == "a"], exp(unname(stats::coef(fit)["a"])), tolerance = 1e-10)
})

test_that("marginal measures with an exposure interaction use the whole model; standardize_over = all adds outcome-missing rows", {
  d <- .gl_df()
  d$y[c(4, 9, 15)] <- NA
  d$x1[c(2, 3)] <- NA
  si <- .gl_spec(interactions = list(c("a", "x1")), standardize_over = "all")
  val <- .gl_value(d, si)
  cc <- stats::complete.cases(d[c("y", "a", "x1", "x2")])
  cov_ok <- stats::complete.cases(d[c("a", "x1", "x2")])
  fit <- stats::glm(y ~ a * x1 + x2, data = d[cc, ], family = stats::binomial())
  g <- .sr_h_gcomp(fit, d[cc, ], "a", c(1, 0), stats::vcov(fit), over = d[cov_ok, ])
  e <- .gl_present(val, si)$effects
  expect_equal(e$est[e$measure == "RD"], g$rd, tolerance = 1e-10)
  expect_equal(val$marginal$n_over, sum(cov_ok))
  sa <- .gl_spec(interactions = list(c("a", "x1")))
  va <- .gl_value(d, sa)
  g2 <- .sr_h_gcomp(fit, d[cc, ], "a", c(1, 0), stats::vcov(fit))
  expect_equal(.gl_present(va, sa)$effects$est[.gl_present(va, sa)$effects$measure == "RD"], g2$rd, tolerance = 1e-10)
  expect_equal(va$marginal$n_over, sum(cc))
})

test_that("a factor exposure gets marginal RD, RR and NNT rows for each non-reference level", {
  d <- .gl_df()
  s <- .gl_spec(exposure = "g", exposure_ref = "lo")
  val <- .gl_value(d, s)
  fit <- stats::glm(y ~ factor(g, levels = c("lo", "mid", "hi")) + x1 + x2, data = d, family = stats::binomial())
  e <- .gl_present(val, s)$effects
  rd <- e[e$measure == "RD", ]
  expect_identical(rd$level, c("mid", "hi"))
  expect_identical(rd$term, c("g: mid vs lo (marginal RD)", "g: hi vs lo (marginal RD)"))
  d2 <- transform(d, g = factor(g, levels = c("lo", "mid", "hi")))
  fit2 <- stats::glm(y ~ g + x1 + x2, data = d2, family = stats::binomial())
  gm <- .sr_h_gcomp(fit2, d2, "g", c("mid", "lo"), stats::vcov(fit2))
  expect_equal(rd$est[1], gm$rd, tolerance = 1e-10)
  expect_identical(sum(e$measure == "NNT"), 2L)
  expect_identical(sum(e$measure == "RR" & grepl("marginal", e$term)), 2L)
})

test_that("a continuous exposure: marginal is skipped when defaulted and refused when asked for", {
  d <- .gl_df()
  s <- .gl_spec(exposure = "x1", terms = list(list(var = "x2")))
  val <- .gl_value(d, s)
  pr <- .gl_present(val, s)
  expect_false(any(pr$effects$measure %in% c("RD", "NNT")))
  expect_true("glm_marginal_skipped" %in% names(pr$fragments))
  expect_match(pr$fragments$glm_marginal_skipped, "binary", fixed = TRUE)
  expect_equal(pr$effects$measure[pr$effects$term == "x1"], "OR")
  s2 <- .gl_spec(exposure = "x1", terms = list(list(var = "x2")), marginal = TRUE)
  expect_error(.gl_value(d, s2), "marginal")
  expect_error(.gl_value(d, s2), "binary|categorical")
})

test_that("observed risks by exposure level carry Wilson intervals", {
  d <- .gl_df()
  s <- .gl_spec(marginal = FALSE)
  val <- .gl_value(d, s)
  ob <- val$obs
  expect_identical(nrow(ob), 2L)
  expect_identical(ob$level, c("0", "1"))
  n1 <- sum(d$a == 1)
  x1 <- sum(d$y[d$a == 1])
  w <- .sr_h_wilson(x1, n1)
  expect_equal(c(ob$est[2], ob$lo[2], ob$hi[2]), unname(w), tolerance = 1e-10)
  expect_equal(ob$n[2], n1)
  expect_equal(ob$events[2], x1)
  pr <- .gl_present(val, s)
  expect_true("observed_risk" %in% names(pr$tables))
  expect_identical(nrow(pr$tables$observed_risk), 2L)
  rk <- pr$effects[pr$effects$measure == "risk", , drop = FALSE]
  expect_identical(nrow(rk), 2L)
  expect_identical(rk$level, c("0", "1"))
  expect_true(all(rk$role == "descriptive"))
  expect_true(all(grepl("wilson", rk$ci_method, ignore.case = TRUE)))
  expect_equal(rk$est, ob$est, tolerance = 1e-12)
  expect_equal(rk$lo, ob$lo, tolerance = 1e-12)
  expect_equal(rk$hi, ob$hi, tolerance = 1e-12)
  expect_equal(rk$n, ob$n)
  expect_equal(rk$events, ob$events)
  expect_equal(rk$est[2], x1 / n1, tolerance = 1e-12)
  expect_equal(c(rk$est[2], rk$lo[2], rk$hi[2]), unname(w), tolerance = 1e-10)
})

test_that("observed risk rows are emitted for the describe twin, modpois and a factor exposure", {
  d <- .gl_df()
  sd <- .gl_spec(purpose = "describe", marginal = FALSE)
  rd <- .gl_present(.gl_value(d, sd, "logistic_describe"), sd, "logistic_describe")$effects
  rk <- rd[rd$measure == "risk", , drop = FALSE]
  expect_identical(nrow(rk), 2L)
  expect_true(all(rk$role == "descriptive"))
  sm <- .gl_spec(marginal = FALSE)
  em <- .gl_present(.gl_value(d, sm, "modpois"), sm, "modpois")$effects
  rm <- em[em$measure == "risk", , drop = FALSE]
  expect_identical(nrow(rm), 2L)
  expect_equal(rm$est, rk$est, tolerance = 1e-12)
  sf <- .gl_spec(exposure = "g", exposure_ref = "lo", marginal = FALSE)
  ef <- .gl_present(.gl_value(d, sf), sf)$effects
  rf <- ef[ef$measure == "risk", , drop = FALSE]
  expect_identical(rf$level, c("lo", "mid", "hi"))
  for (i in 1:3) {
    ii <- d$g == rf$level[i]
    expect_equal(c(rf$est[i], rf$lo[i], rf$hi[i]),
                 unname(.sr_h_wilson(sum(d$y[ii]), sum(ii))), tolerance = 1e-10)
  }
  # a continuous exposure has no levels, so no risk rows
  sc <- .gl_spec(exposure = "x1", terms = list(list(var = "x2")), marginal = FALSE)
  ec <- .gl_present(.gl_value(d, sc), sc)$effects
  expect_false("risk" %in% ec$measure)
})

test_that("plain logistic with a separating covariate records the separation fragment and no R warning escapes", {
  d <- .gl_df()
  # x3 is 1 only on 100 rows that all have the event: the covariate separates those rows
  d$x3 <- 0L
  d$x3[which(d$y == 1L)[1:100]] <- 1L
  s <- .gl_spec(terms = list(list(var = "x3")), marginal = FALSE, penalty = "none")
  expect_no_warning({
    val <- .gl_value(d, s)
    pr <- .gl_present(val, s)
  })
  expect_true(isTRUE(val$near_separation))
  expect_true(all(is.finite(pr$effects$est[pr$effects$term == "a"])))
  expect_true("glm_separation" %in% names(pr$fragments))
  expect_match(pr$fragments$glm_separation, "Firth", fixed = TRUE)
  # a well-behaved fit has no separation fragment
  s0 <- .gl_spec(marginal = FALSE)
  p0 <- .gl_present(.gl_value(.gl_df(), s0), s0)
  expect_false("glm_separation" %in% names(p0$fragments))
})

test_that("modpois with fitted risk above 1 gets the risk-above-one fragment, not the separation one", {
  d <- .gl_df()
  withr::with_seed(5L, {
    d$x1 <- stats::rnorm(nrow(d))
    d$y <- stats::rbinom(nrow(d), 1L, stats::plogis(-0.2 + 2.5 * d$x1))
  })
  fit <- stats::glm(y ~ a + x1 + x2, data = d, family = stats::poisson(link = "log"))
  expect_true(any(fit$fitted.values > 1))
  s <- .gl_spec(marginal = FALSE)
  expect_no_warning({
    val <- .gl_value(d, s, "modpois")
    pr <- .gl_present(val, s, "modpois")
  })
  expect_true("glm_risk_above_one" %in% names(pr$fragments))
  expect_match(pr$fragments$glm_risk_above_one, "above 1", fixed = TRUE)
  expect_false(grepl("Firth", pr$fragments$glm_risk_above_one, fixed = TRUE))
  expect_false("glm_separation" %in% names(pr$fragments))
  # modpois with no fitted value above 1 has neither fragment
  dn <- .gl_df()
  withr::with_seed(3L, dn$y <- stats::rbinom(nrow(dn), 1L, stats::plogis(-2.5 + 0.5 * dn$a + 0.2 * dn$x1)))
  expect_false(any(stats::fitted(stats::glm(y ~ a + x1 + x2, data = dn, family = stats::poisson())) > 1))
  s0 <- .gl_spec(marginal = FALSE)
  p0 <- .gl_present(.gl_value(dn, s0, "modpois"), s0, "modpois")
  expect_false(any(c("glm_risk_above_one", "glm_separation") %in% names(p0$fragments)))
})

test_that("modpois with a standardised risk above 1 under marginal effects gets the fragment", {
  d <- .gl_df()
  withr::with_seed(6L, {
    d$x1 <- stats::rnorm(nrow(d))
    d$y <- stats::rbinom(nrow(d), 1L, stats::plogis(-1 + 0.3 * d$a + 0.3 * d$x1))
  })
  s <- .gl_spec()
  val <- .gl_value(d, s, "modpois")
  expect_true(!is.null(val$marginal$rows))
  expect_false(isTRUE(val$risk_above_one))
  expect_false(any(c(val$marginal$rows$p1, val$marginal$rows$p0) > 1))
  expect_false("glm_risk_above_one" %in% names(.gl_present(val, s, "modpois")$fragments))
  # a standardised risk above 1 raises the fragment even when no single fitted value is above 1
  v2 <- val
  v2$marginal$rows$p1[1] <- 1.2
  pr <- .gl_present(v2, s, "modpois")
  expect_true("glm_risk_above_one" %in% names(pr$fragments))
  expect_false("glm_separation" %in% names(pr$fragments))
})

test_that("the convergence fragment appears when the fit reports non-convergence", {
  d <- .gl_df()
  s <- .gl_spec(marginal = FALSE)
  val <- .gl_value(d, s)
  expect_false("glm_convergence" %in% names(.gl_present(val, s)$fragments))
  val$converged <- FALSE
  expect_true("glm_convergence" %in% names(.gl_present(val, s)$fragments))
})
# ---- end to end -------------------------------------------------------------------------------

test_that("logistic and modpois run through sr_sap_add, lock and sr_run; hostile names are data", {
  bt <- intToUtf8(96)
  hostile <- paste0("v", bt, "); system(", intToUtf8(39), "calc", intToUtf8(39), "); (")
  df <- data.frame(id = seq_len(nrow(mtcars)), mtcars)
  names(df)[names(df) == "vs"] <- hostile
  names(df)[names(df) == "am"] <- "a b"
  roles <- c(id = "id")
  roles[hostile] <- "outcome"
  p <- .fx_ready_project(df, "t", roles)
  .gl_route("logistic")
  .gl_route("modpois")
  s <- .gl_spec(outcome = hostile, exposure = "a b", terms = list(list(var = "wt")))
  bad <- .gl_spec(outcome = hostile, exposure = "a b", terms = list(list(var = "wt")), penalty = "firth",
                  marginal = TRUE)
  expect_error(sr_sap_add(p, "bad", "logistic", bad, "t", label = "primary"), "Firth")
  bad2 <- .gl_spec(outcome = hostile, exposure = "a b", se = "model")
  expect_error(sr_sap_add(p, "bad2", "modpois", bad2, "t", label = "primary"), "robust or cluster")
  p <- sr_sap_add(p, "m1", "logistic", s, "t", label = "primary")
  p <- sr_sap_add(p, "m2", "modpois", s, "t", label = "secondary")
  p <- sr_sap_lock(p)
  p <- sr_run(p, "m1")
  p <- sr_run(p, "m2")
  expect_true(p$state$runs$m1$ok, info = p$state$runs$m1$error)
  expect_true(p$state$runs$m2$ok, info = p$state$runs$m2$error)
  d2 <- data.frame(y = df[[hostile]], a = df[["a b"]], wt = df$wt)
  fit <- stats::glm(y ~ a + wt, data = d2, family = stats::binomial())
  res <- sr_result(p, "m1")
  or <- res$effects[res$effects$measure == "OR" & res$effects$term == "a b", ]
  expect_equal(or$est, exp(unname(stats::coef(fit)["a"])), tolerance = 1e-10)
  g <- .sr_h_gcomp(fit, d2, "a", c(1, 0), stats::vcov(fit))
  expect_equal(res$effects$est[res$effects$measure == "RD"], g$rd, tolerance = 1e-10)
  expect_true(any(res$flags$flag == "non_collapsible"))
  fit2 <- stats::glm(y ~ a + wt, data = d2, family = stats::poisson(link = "log"))
  res2 <- sr_result(p, "m2")
  expect_equal(res2$effects$est[res2$effects$measure == "RR" & res2$effects$term == "a b"],
               exp(unname(stats::coef(fit2)["a"])), tolerance = 1e-10)
  code <- sr_compile(.gl_route("logistic"), s)
  expect_false("system" %in% all.names(parse(text = code)))
  p2 <- sr_run(p, "m1")
  expect_true(p2$state$runs$m1$cached)
})
