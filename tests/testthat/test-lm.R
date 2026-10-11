# Tests for routes/se.R and routes/lm.R: the SE helpers and the linear regression route (lm@1 and its
# describe twin lm_describe@1). Synthetic or built-in data only (mtcars); projects live under
# withr::local_tempdir().

.lm_layers <- function(clustered = FALSE) {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = clustered)
}

# mpg ~ am + wt + hp, exposure am
.lm_spec <- function(se = "model", cluster = NULL, purpose = "explain-causal", ...) {
  s <- list(purpose = purpose, outcome = "mpg", exposure = "am",
            terms = list(list(var = "wt"), list(var = "hp")),
            se = se, layers = .lm_layers(clustered = identical(se, "cluster")), seed = 42L)
  if (!is.null(cluster)) s$cluster <- cluster
  mod <- list(...)
  for (nm in names(mod)) s[nm] <- list(mod[[nm]])
  s
}

# The routes, registered again when an earlier test file emptied the registry.
.lm_route <- function(id = "lm") {
  if (!(.sr_route_key("lm", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
    sr_route_register(.sr_lm_route())
  }
  if (!(.sr_route_key("lm_describe", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
    sr_route_register(.sr_lm_describe_route())
  }
  sr_route_get(id, 1L)
}

# Compile the spec, vet the code, run it on `df` and return the value.
.lm_value <- function(df, spec = .lm_spec(), id = "lm") {
  route <- .lm_route(id)
  code <- sr_compile(route, spec)
  expect_silent(.sr_emit_check(code, "analysis"))
  env <- new.env(parent = globalenv())
  env$sr_data <- df
  eval(parse(text = code), envir = env)
}

.lm_present <- function(value, spec = .lm_spec(), id = "lm") {
  r <- .lm_route(id)
  sr_result_check(r$present(value, spec), r)
}

# ---- the SE helpers ---------------------------------------------------------------------------

test_that(".h_vcov: model is stats::vcov, robust is HC3 for lm and HC0 for glm, cluster is HC1", {
  fit <- stats::lm(mpg ~ am + wt + hp, data = mtcars)
  expect_equal(.sr_h_vcov(fit, "model"), stats::vcov(fit), tolerance = 1e-12)
  expect_equal(.sr_h_vcov(fit, "robust"), sandwich::vcovHC(fit, type = "HC3"), tolerance = 1e-12)
  expect_equal(.sr_h_vcov(fit, "cluster", mtcars$cyl),
               sandwich::vcovCL(fit, cluster = mtcars$cyl, type = "HC1"), tolerance = 1e-12)
  g <- stats::glm(am ~ wt + hp, data = mtcars, family = stats::binomial())
  expect_equal(.sr_h_vcov(g, "robust"), sandwich::vcovHC(g, type = "HC0"), tolerance = 1e-12)
  expect_equal(.sr_h_vcov(g, "cluster", mtcars$cyl),
               sandwich::vcovCL(g, cluster = mtcars$cyl, type = "HC1"), tolerance = 1e-12)
  expect_equal(.sr_h_vcov(fit), stats::vcov(fit), tolerance = 1e-12)           # default is the model SE
  expect_error(.sr_h_vcov(fit, "cluster"), "cluster")
  expect_error(.sr_h_vcov(fit, "cluster", mtcars$cyl[1:5]), "cluster")
  expect_error(.sr_h_vcov(fit, "sandwich"))
})

test_that(".h_coef_table gives t intervals for a finite df and z intervals for df = Inf", {
  est <- c(a = 1.5, b = -0.4)
  V <- diag(c(0.25, 0.01))
  tb <- .sr_h_coef_table(est, V, df = 20)
  expect_identical(names(tb), c("term", "est", "se", "lo", "hi", "p"))
  expect_identical(tb$term, c("a", "b"))
  expect_equal(tb$se, c(0.5, 0.1), tolerance = 1e-14)
  expect_equal(tb$lo, unname(est - stats::qt(0.975, 20) * c(0.5, 0.1)), tolerance = 1e-12)
  expect_equal(tb$hi, unname(est + stats::qt(0.975, 20) * c(0.5, 0.1)), tolerance = 1e-12)
  expect_equal(tb$p, unname(2 * stats::pt(-abs(est / c(0.5, 0.1)), 20)), tolerance = 1e-12)
  z <- .sr_h_coef_table(est, V)
  expect_equal(z$lo, unname(est - stats::qnorm(0.975) * c(0.5, 0.1)), tolerance = 1e-12)
  expect_equal(z$p, unname(2 * stats::pnorm(-abs(est / c(0.5, 0.1)))), tolerance = 1e-12)
  w <- .sr_h_coef_table(est, V, df = 20, level = 0.9)
  expect_equal(w$hi, unname(est + stats::qt(0.95, 20) * c(0.5, 0.1)), tolerance = 1e-12)
  expect_identical(rownames(tb), c("1", "2"))
  expect_error(.sr_h_coef_table(est, V, level = 1.2), "level")
  expect_error(.sr_h_coef_table(est, diag(3)), "match|length|dimension")
})

test_that("the helpers inline into emitted code under their .h_ names", {
  code <- paste(.sr_inline(.sr_h_vcov, ".h_vcov"), .sr_inline(.sr_h_coef_table, ".h_coef_table"), sep = "\n")
  expect_silent(.sr_emit_check(code, "analysis"))
  env <- new.env(parent = globalenv())
  eval(parse(text = code), envir = env)
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  expect_equal(env$.h_vcov(fit, "robust"), sandwich::vcovHC(fit, type = "HC3"), tolerance = 1e-12)
})

# ---- the routes -------------------------------------------------------------------------------

test_that("lm@1 and lm_describe@1 are registered with the documented schema and estimand", {
  r <- .lm_route("lm")
  d <- .lm_route("lm_describe")
  expect_identical(r$family, "regression")
  expect_identical(r$purpose, "explain-causal")
  expect_identical(d$family, "regression")
  expect_identical(d$purpose, "describe")
  expect_identical(r$version, 1L)
  sc <- r$spec_schema
  expect_setequal(names(sc), c("outcome", "exposure", "terms", "interactions", "se", "cluster", "marginal",
                               "standardize_over", "layers", "seed"))
  expect_true(sc$outcome$required)
  expect_true(sc$exposure$required)
  expect_true(sc$layers$required)
  expect_true(sc$seed$required)
  expect_false(sc$se$required)
  expect_identical(sc$se$values, c("model", "robust", "cluster"))
  expect_identical(sc$marginal$type, "lgl")
  expect_identical(d$spec_schema, sc)
  expect_length(formals(r$compile), 1L)                      # compile takes the spec and no data
  expect_identical(r$compile, d$compile)
  expect_identical(r$present, d$present)
  est <- r$estimand(.lm_spec())
  expect_s3_class(est, "sr_estimand")
  expect_identical(est$population, "complete cases")
  expect_setequal(est$variables, c("am", "mpg"))
  expect_identical(est$intercurrent, "not applicable")
  expect_identical(est$summary, "adjusted mean difference")
  mi <- .lm_spec(layers = modifyList(.lm_layers(), list(missing = "mi")))
  expect_identical(r$estimand(mi)$population, "all analysed (MI)")
  expect_true(sr_spec_validate(.lm_spec(), r)$ok)
  expect_true(sr_spec_validate(.lm_spec(purpose = "describe"), d)$ok)
  expect_false(sr_spec_validate(.lm_spec(purpose = "describe"), r)$ok)   # purpose must match the route
})

test_that("sr_spec_cols reads outcome, exposure, terms and cluster", {
  expect_identical(sr_spec_cols(.lm_spec()), c("mpg", "am", "wt", "hp"))
  expect_identical(sr_spec_cols(.lm_spec(se = "cluster", cluster = "cyl")), c("mpg", "am", "wt", "hp", "cyl"))
})

test_that("validation refuses what lm cannot do: cluster SE without a cluster, unknown standardisation", {
  r <- .lm_route()
  v <- sr_spec_validate(.lm_spec(se = "cluster"), r)
  expect_false(v$ok)
  expect_match(paste(v$errors, collapse = " "), "cluster")
  # a cluster column but layers$clustered = FALSE is refused as well
  s <- .lm_spec(se = "cluster", cluster = "cyl")
  s$layers$clustered <- FALSE
  expect_false(sr_spec_validate(s, r)$ok)
  expect_true(sr_spec_validate(.lm_spec(se = "cluster", cluster = "cyl"), r)$ok)
  # R3: marginal effects are implemented since g-computation landed (Task 7); the refusal is gone
  expect_true(sr_spec_validate(.lm_spec(marginal = TRUE), r)$ok)
  expect_true(sr_spec_validate(.lm_spec(marginal = TRUE, standardize_over = "all"), r)$ok)
  expect_true(sr_spec_validate(.lm_spec(marginal = FALSE, standardize_over = "all"), r)$ok)
  expect_silent(sr_compile(r, .lm_spec(marginal = TRUE)))
  # only the standardisations that exist are accepted
  expect_false(sr_spec_validate(.lm_spec(marginal = TRUE, standardize_over = "exposed"), r)$ok)
  expect_false(sr_spec_validate(.lm_spec(se = "bootstrap"), r)$ok)
  expect_false(sr_spec_validate(modifyList(.lm_spec(), list(outcome = NULL)), r)$ok)
})

test_that("compile refuses layers that lm@1 does not implement instead of ignoring them", {
  r <- .lm_route()
  for (lay in list(list(missing = "mi"), list(weights = "ipw"), list(matching = TRUE),
                   list(resampling = "bootstrap"), list(framework = "bayesian"))) {
    s <- .lm_spec(layers = modifyList(.lm_layers(), lay))
    expect_error(sr_compile(r, s), "layer", info = names(lay))
  }
})

test_that("compile is deterministic, data-free and passes the emit check", {
  r <- .lm_route()
  a <- sr_compile(r, .lm_spec())
  expect_identical(a, sr_compile(r, .lm_spec()))
  expect_silent(.sr_emit_check(a, "analysis"))
  expect_false(identical(a, sr_compile(r, .lm_spec(se = "robust"))))
})

test_that("a spec read back from JSON state (lists of terms, integer seed) compiles to the same code", {
  r <- .lm_route()
  s <- .lm_spec(se = "cluster", cluster = "cyl", interactions = list(c("am", "wt")),
                terms = list(list(var = "wt"), list(var = "hp", type = "rcs", knots = 3)))
  back <- .sr_spec_from_state(jsonlite::fromJSON(jsonlite::toJSON(s, auto_unbox = TRUE), simplifyVector = FALSE))
  expect_true(sr_spec_validate(back, r)$ok)
  expect_identical(sr_compile(r, back), sr_compile(r, s))
})

# ---- the numbers ------------------------------------------------------------------------------

test_that("model SE: est, lo and hi equal confint(lm()) to 1e-10", {
  val <- .lm_value(mtcars)
  fit <- stats::lm(mpg ~ am + wt + hp, data = mtcars)
  ci <- stats::confint(fit)
  tb <- val$coef
  expect_identical(tb$term, c("(Intercept)", "t1", "t2", "t3"))
  expect_equal(tb$est, unname(stats::coef(fit)), tolerance = 1e-10)
  expect_equal(tb$lo, unname(ci[, 1]), tolerance = 1e-10)
  expect_equal(tb$hi, unname(ci[, 2]), tolerance = 1e-10)
  expect_equal(tb$se, unname(summary(fit)$coefficients[, 2]), tolerance = 1e-10)
  expect_equal(tb$p, unname(summary(fit)$coefficients[, 4]), tolerance = 1e-10)
  expect_identical(val$se_type, "model")
  expect_equal(val$n, 32)
  expect_equal(val$df_resid, 28)
  expect_equal(val$r2, summary(fit)$r.squared, tolerance = 1e-12)
  expect_true(.sr_value_plain(val))
})

test_that("robust SE equals sandwich::vcovHC(type = HC3)", {
  val <- .lm_value(mtcars, .lm_spec(se = "robust"))
  fit <- stats::lm(mpg ~ am + wt + hp, data = mtcars)
  se <- sqrt(diag(sandwich::vcovHC(fit, type = "HC3")))
  tb <- val$coef
  expect_equal(tb$est, unname(stats::coef(fit)), tolerance = 1e-10)
  expect_equal(tb$se, unname(se), tolerance = 1e-10)
  tq <- stats::qt(0.975, fit$df.residual)
  expect_equal(tb$lo, unname(stats::coef(fit) - tq * se), tolerance = 1e-10)
  expect_equal(tb$hi, unname(stats::coef(fit) + tq * se), tolerance = 1e-10)
  expect_identical(val$se_type, "robust")
})

test_that("cluster SE on cyl equals sandwich::vcovCL(cluster = ~cyl, type = HC1)", {
  val <- .lm_value(mtcars, .lm_spec(se = "cluster", cluster = "cyl"))
  fit <- stats::lm(mpg ~ am + wt + hp, data = mtcars)
  se <- sqrt(diag(sandwich::vcovCL(fit, cluster = ~cyl, type = "HC1")))
  tb <- val$coef
  expect_equal(tb$est, unname(stats::coef(fit)), tolerance = 1e-10)
  expect_equal(tb$se, unname(se), tolerance = 1e-10)
  # few clusters: t quantile on G - 1 = 2 degrees of freedom (never more than the residual df)
  expect_equal(val$n_clusters, 3)
  expect_equal(val$df, 2)
  tq <- stats::qt(0.975, 2)
  expect_equal(tb$lo, unname(stats::coef(fit) - tq * se), tolerance = 1e-10)
  expect_equal(tb$hi, unname(stats::coef(fit) + tq * se), tolerance = 1e-10)
  expect_identical(val$se_type, "cluster")
})

test_that("a cluster SE with one cluster is refused at run time", {
  df <- mtcars
  df$cyl <- 4
  expect_error(.lm_value(df, .lm_spec(se = "cluster", cluster = "cyl")), "cluster")
})

test_that("rows with a missing outcome, covariate or cluster value are dropped and counted", {
  df <- mtcars
  df$mpg[c(2, 7)] <- NA
  df$wt[11] <- NA
  df$cyl[c(5, 6)] <- NA
  val <- .lm_value(df, .lm_spec(se = "cluster", cluster = "cyl"))
  keep <- stats::complete.cases(df[c("mpg", "am", "wt", "hp", "cyl")])
  d2 <- df[keep, ]
  fit <- stats::lm(mpg ~ am + wt + hp, data = d2)
  expect_equal(val$n, sum(keep))
  expect_equal(val$n_total, 32)
  expect_equal(val$coef$est, unname(stats::coef(fit)), tolerance = 1e-10)
  expect_equal(val$coef$se, unname(sqrt(diag(sandwich::vcovCL(fit, cluster = d2$cyl, type = "HC1")))),
               tolerance = 1e-10)
  expect_error(.lm_value(mtcars[1:3, ]), "rows|degrees")
})

test_that("a perfectly collinear covariate is dropped and reported, the rest unchanged", {
  df <- mtcars
  df$wt2 <- 2 * df$wt
  s <- .lm_spec(terms = list(list(var = "wt"), list(var = "wt2"), list(var = "hp")), se = "robust")
  val <- .lm_value(df, s)
  fit <- stats::lm(mpg ~ am + wt + hp, data = df)
  expect_identical(val$aliased, "t3")
  expect_identical(val$coef$term, c("(Intercept)", "t1", "t2", "t4"))
  expect_equal(val$coef$est, unname(stats::coef(fit)), tolerance = 1e-10)
  expect_equal(val$coef$se, unname(sqrt(diag(sandwich::vcovHC(fit, type = "HC3")))), tolerance = 1e-10)
  pr <- .lm_present(val, s)
  expect_false("wt2" %in% pr$effects$term)
  expect_match(paste(unlist(pr$fragments), collapse = " "), "wt2", fixed = TRUE)
  # an exposure that is collinear has no effect to report: refused
  df$am <- 1
  s2 <- .lm_spec(terms = list(list(var = "wt")))
  expect_error(.lm_present(.lm_value(df, s2), s2), "exposure")
})

test_that("figure data holds fitted, residuals, standardised residuals, leverage and Cook distance", {
  val <- .lm_value(mtcars)
  fit <- stats::lm(mpg ~ am + wt + hp, data = mtcars)
  fg <- val$fig
  expect_identical(names(fg), c("fitted", "resid", "std_resid", "leverage", "cooks"))
  expect_equal(fg$fitted, unname(stats::fitted(fit)), tolerance = 1e-10)
  expect_equal(fg$resid, unname(stats::residuals(fit)), tolerance = 1e-10)
  expect_equal(fg$std_resid, unname(stats::rstandard(fit)), tolerance = 1e-10)
  expect_equal(fg$leverage, unname(stats::hatvalues(fit)), tolerance = 1e-10)
  expect_equal(fg$cooks, unname(stats::cooks.distance(fit)), tolerance = 1e-10)
  pr <- .lm_present(val)
  expect_identical(pr$figures$resid_panel, fg)
})

# ---- present ----------------------------------------------------------------------------------

test_that("present: the exposure is the primary MD row, covariates carry table2", {
  val <- .lm_value(mtcars)
  pr <- .lm_present(val)
  e <- pr$effects
  fit <- stats::lm(mpg ~ am + wt + hp, data = mtcars)
  ci <- stats::confint(fit)
  expect_identical(e$term, c("am", "wt", "hp"))
  expect_identical(e$role, c("primary", "covariate", "covariate"))
  expect_identical(e$measure, c("MD", "coef", "coef"))
  expect_true(all(e$scale == "difference"))
  expect_equal(e$est, unname(stats::coef(fit))[-1], tolerance = 1e-10)
  expect_equal(e$lo, unname(ci[-1, 1]), tolerance = 1e-10)
  expect_equal(e$hi, unname(ci[-1, 2]), tolerance = 1e-10)
  expect_equal(e$p, unname(summary(fit)$coefficients[-1, 4]), tolerance = 1e-10)
  expect_true(all(e$n == 32))
  expect_false(any(e$conditional))
  expect_identical(pr$flags$term, c("wt", "hp"))
  expect_true(all(pr$flags$flag == "table2"))
  expect_false(any(names(pr$tables$coefficients) %in% .sr_lint_pnames))
  expect_true(any(grepl("(Intercept)", pr$tables$coefficients[[1L]], fixed = TRUE)))
  expect_true(length(pr$fragments) >= 1L)
})

test_that("the describe twin shares the compile and present but carries no Table 2 flags", {
  s <- .lm_spec(purpose = "describe")
  val <- .lm_value(mtcars, s, id = "lm_describe")
  pr <- .lm_present(val, s, id = "lm_describe")
  expect_identical(pr$effects$role, c("primary", "covariate", "covariate"))
  expect_identical(nrow(pr$flags), 0L)
  expect_equal(pr$effects$est, unname(stats::coef(stats::lm(mpg ~ am + wt + hp, data = mtcars)))[-1],
               tolerance = 1e-10)
})

test_that("a factor exposure gives one primary row per non-reference level", {
  s <- .lm_spec(exposure = "cyl", exposure_ref = 4)
  s$terms <- list(list(var = "wt"))
  val <- .lm_value(mtcars, s)
  fit <- stats::lm(mpg ~ factor(cyl) + wt, data = mtcars)
  expect_identical(val$coef$term, c("(Intercept)", "t1f6", "t1f8", "t2"))
  expect_equal(val$coef$est, unname(stats::coef(fit)), tolerance = 1e-10)
  pr <- .lm_present(val, s)
  e <- pr$effects
  expect_identical(e$role, c("primary", "primary", "covariate"))
  expect_identical(e$level[1:2], c("6", "8"))
  expect_true(all(grepl("^cyl", e$term[1:2])))
  expect_match(e$term[1], "6", fixed = TRUE)
  expect_match(e$term[1], "4", fixed = TRUE)                  # the reference is named
  expect_identical(e$term[3], "wt")
  expect_equal(e$est[1:2], unname(stats::coef(fit))[2:3], tolerance = 1e-10)
  expect_identical(sum(pr$flags$flag == "table2"), 1L)
})

test_that("interaction rows are covariate/coef rows; the exposure main effect becomes conditional", {
  s <- .lm_spec(interactions = list(c("am", "wt")))
  val <- .lm_value(mtcars, s)
  fit <- stats::lm(mpg ~ am * wt + hp, data = mtcars)
  expect_equal(val$coef$est[match(c("t1", "t2", "t3", "t1:t2"), val$coef$term)],
               unname(stats::coef(fit)[c("am", "wt", "hp", "am:wt")]), tolerance = 1e-10)
  e <- .lm_present(val, s)$effects
  expect_identical(e$term, c("am", "wt", "hp", "am:wt"))
  expect_identical(e$role, c("primary", "covariate", "covariate", "covariate"))
  expect_identical(e$measure, c("MD", "coef", "coef", "coef"))
  expect_identical(e$conditional, c(TRUE, FALSE, FALSE, FALSE))
})

test_that("rcs and quadratic terms are labelled by the user variable", {
  s <- .lm_spec(terms = list(list(var = "wt", type = "rcs", knots = 3), list(var = "hp", type = "quad")))
  val <- .lm_value(mtcars, s)
  pr <- .lm_present(val, s)
  expect_identical(pr$effects$term, c("am", "wt", "wt (spline 1)", "hp", "hp^2"))
  expect_identical(pr$effects$role, c("primary", rep("covariate", 4)))
  kn <- Hmisc::rcspline.eval(mtcars$wt, nk = 3, knots.only = TRUE)
  b <- Hmisc::rcspline.eval(mtcars$wt, knots = kn, inclx = TRUE)
  fit <- stats::lm(mtcars$mpg ~ mtcars$am + b + mtcars$hp + I(mtcars$hp^2))
  expect_equal(pr$effects$est, unname(stats::coef(fit))[-1], tolerance = 1e-10)
})

test_that("the display table, fragment and the result contract hold for every se type", {
  for (se in c("model", "robust", "cluster")) {
    s <- .lm_spec(se = se, cluster = if (se == "cluster") "cyl" else NULL)
    pr <- .lm_present(.lm_value(mtcars, s), s)
    expect_true("coefficients" %in% names(pr$tables), info = se)
    expect_match(paste(unlist(pr$fragments), collapse = " "), se, fixed = TRUE, info = se)
    expect_identical(pr$effects$ci_method[1], c(model = "t_model", robust = "t_robust_hc3",
                                                cluster = "t_cluster_hc1")[[se]])
  }
})

# ---- end to end -------------------------------------------------------------------------------

test_that("lm runs through sr_sap_add, lock and sr_run; a hostile column name is data, not code", {
  bt <- intToUtf8(96)
  hostile <- paste0("x", bt, "); system(", intToUtf8(39), "calc", intToUtf8(39), "); (")
  df <- data.frame(id = seq_len(nrow(mtcars)), mtcars)
  names(df)[names(df) == "mpg"] <- hostile
  names(df)[names(df) == "am"] <- "a b"
  roles <- c(id = "id")
  roles[hostile] <- "outcome"
  p <- .fx_ready_project(df, "t", roles)
  .lm_route()
  s <- .lm_spec(outcome = hostile, exposure = "a b")
  # route validation runs at sr_sap_add
  bad1 <- .lm_spec(outcome = hostile, exposure = "a b", se = "cluster")
  expect_error(sr_sap_add(p, "bad1", "lm", bad1, "t", label = "primary"), "cluster")
  bad2 <- .lm_spec(outcome = hostile, exposure = "a b", standardize_over = "exposed")
  expect_error(sr_sap_add(p, "bad2", "lm", bad2, "t", label = "primary"), "standardize_over")
  p <- sr_sap_add(p, "m1", "lm", s, "t", label = "primary")
  p <- sr_sap_lock(p)
  p <- sr_run(p, "m1")
  expect_true(p$state$runs$m1$ok, info = p$state$runs$m1$error)
  res <- sr_result(p, "m1")
  d2 <- data.frame(y = df[[hostile]], a = df[["a b"]], wt = df$wt, hp = df$hp)
  fit <- stats::lm(y ~ a + wt + hp, data = d2)
  expect_equal(res$effects$est, unname(stats::coef(fit))[-1], tolerance = 1e-10)
  expect_identical(res$effects$term[1], "a b")
  expect_identical(res$effects$role[1], "primary")
  expect_true(all(res$flags$flag == "table2"))
  code <- sr_compile(.lm_route(), s)
  nm <- all.names(parse(text = code))
  expect_false("system" %in% nm)
  expect_true(grepl("system", code, fixed = TRUE))             # present, but only inside a string
  p2 <- sr_run(p, "m1")
  expect_true(p2$state$runs$m1$cached)
})
