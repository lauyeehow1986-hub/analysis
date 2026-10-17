# Tests for impute/impute.R: the multiple-imputation wrapper (sr_mi_code) behind the model routes, the
# choice of m, the survival imputation model and sr_missing_summary. Public package data only
# (mice::nhanes, survival::lung, mtcars) and seeded synthetic data; projects live under
# withr::local_tempdir().

.im_layers <- function(...) {
  utils::modifyList(list(missing = "mi", weights = "none", matching = FALSE, resampling = "none",
                         framework = "frequentist", clustered = FALSE), list(...))
}

# The routes, registered again when an earlier test file emptied the registry.
.im_route <- function(id) {
  make <- list(lm = .sr_lm_route, lm_describe = .sr_lm_describe_route, logistic = .sr_logistic_route,
               logistic_describe = .sr_logistic_describe_route, modpois = .sr_modpois_route,
               cox = .sr_cox_route, cox_describe = .sr_cox_describe_route, rmst = .sr_rmst_route)
  for (k in names(make)) {
    if (!(.sr_route_key(k, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make[[k]]())
  }
  sr_route_get(id, 1L)
}

# Compile, check and run the emitted code on a data frame under the seed of the spec (default RNG kind).
.im_value <- function(df, spec, id) {
  route <- .im_route(id)
  code <- sr_compile(route, spec)
  expect_silent(.sr_emit_check(code, "analysis"))
  env <- new.env(parent = globalenv())
  env$sr_data <- df
  withr::with_seed(spec$seed, eval(parse(text = code), envir = env))
}

.im_present <- function(value, spec, id) {
  r <- .im_route(id)
  sr_result_check(r$present(value, spec), r)
}

# mice::nhanes with an id: age (complete), bmi, hyp, chl (each with missing values), 25 rows.
.im_nhanes <- function() {
  d <- mice::nhanes
  data.frame(id = seq_len(nrow(d)), age = as.numeric(d$age), bmi = d$bmi, hyp = d$hyp, chl = d$chl)
}

.im_lm_spec <- function(...) {
  s <- list(purpose = "explain-causal", outcome = "chl", exposure = "bmi", terms = list(list(var = "age")),
            layers = .im_layers(), mi = list(m = 5, maxit = 10), seed = 1L)
  mod <- list(...)
  for (nm in names(mod)) s[nm] <- list(mod[[nm]])
  s
}

# survival::lung: time, status (1 = censored, 2 = dead), sex (1/2), age (complete), wt.loss (14 missing)
.im_cox_spec <- function(...) {
  s <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 2, exposure = "sex",
            terms = list(list(var = "age"), list(var = "wt.loss")), layers = .im_layers(),
            mi = list(m = 3), seed = 11L)
  mod <- list(...)
  for (nm in names(mod)) s[nm] <- list(mod[[nm]])
  s
}

# The Nelson-Aalen cumulative hazard the wrapper adds to the imputation model.
.im_na <- function(time, event) {
  mice::nelsonaalen(data.frame(.time = time, .event = event), .time, .event)
}

# ---- through sr_run -----------------------------------------------------------------------------

test_that("lm under MI through sr_run equals a direct mice run under the same RNG preamble", {
  df <- .im_nhanes()
  p <- .fx_ready_project(df, "t", c(id = "id", chl = "outcome"))
  .im_route("lm")
  s <- .im_lm_spec()
  p <- sr_sap_add(p, "m1", "lm", s, "t", label = "primary")
  p <- sr_sap_lock(p)
  p <- sr_run(p, "m1")
  run <- p$state$runs$m1
  expect_true(run$ok, info = run$error)
  res <- sr_result(p, "m1")
  val <- readRDS(file.path(p$paths$fits, paste0(run$ok_key, ".rds")))$value

  # the same rows in the same order as the run saw them, and the imputation frame the wrapper builds:
  # the exposure, the other terms, then the outcome, under generated names
  xf <- file.path(p$paths$dir, "data", "extracts", paste0(run$extract_sha, ".rds"))
  ext <- unserialize(readBin(xf, "raw", n = file.size(xf)))
  frame <- data.frame(t1 = as.numeric(ext$bmi), t2 = as.numeric(ext$age), .y = as.numeric(ext$chl))
  withr::local_preserve_seed()
  old <- RNGkind()
  withr::defer(RNGkind(old[1L], old[2L], old[3L]))
  eval(parse(text = sr_rng_preamble(1)))
  imp <- mice::mice(frame, m = 5, maxit = 10, printFlag = FALSE)
  fit <- with(imp, lm(.y ~ t1 + t2))
  pl <- mice::pool(fit)
  sm <- summary(pl)

  expect_identical(val$coef$term, c("(Intercept)", "t1", "t2"))
  expect_equal(val$coef$est, sm$estimate, tolerance = 1e-10)
  expect_equal(val$coef$se, sm$std.error, tolerance = 1e-10)
  expect_equal(val$coef$df, sm$df, tolerance = 1e-8)
  expect_equal(val$coef$fmi, pl$pooled$fmi, tolerance = 1e-10)
  expect_identical(res$effects$term, c("bmi", "age"))
  expect_equal(res$effects$est, sm$estimate[-1L], tolerance = 1e-10)
  expect_equal(res$effects$lo, (sm$estimate - stats::qt(0.975, sm$df) * sm$std.error)[-1L], tolerance = 1e-8)
  expect_equal(res$effects$hi, (sm$estimate + stats::qt(0.975, sm$df) * sm$std.error)[-1L], tolerance = 1e-8)
  expect_equal(res$effects$mcse, val$coef$mcse[-1L], tolerance = 1e-14)
  expect_true(all(res$effects$mcse > 0))
  expect_true(all(res$effects$ci_method == "rubin"))
  expect_identical(res$effects$role, c("primary", "covariate"))
  expect_identical(res$effects$measure, c("MD", "coef"))
  expect_true(all(res$effects$n == 25))                         # every row is analysed, imputed outcome included
  expect_identical(val$mi$m, 5L)
  expect_identical(val$mi$maxit, 10L)
  expect_length(val$mi$coef_names, 5L)
  # the fragment states m, maxit, the methods, the outcome in the model and the impute-then-transform assumption
  note <- res$fragments$mi_note
  expect_match(note, "m = 5", fixed = TRUE)
  expect_match(note, "10 iteration", fixed = TRUE)
  expect_match(note, "pmm", fixed = TRUE)
  expect_match(note, "impute, then transform", fixed = TRUE)
  expect_match(note, "imputed outcome", fixed = TRUE)
  expect_match(note, "Rubin", fixed = TRUE)
  expect_true("imputation" %in% names(res$tables))
  expect_null(res$fragments$mi_auto)
  # a second run is a cache hit: the imputation is reproducible from the key
  p2 <- sr_run(p, "m1")
  expect_true(p2$state$runs$m1$cached)
})

# ---- the fixed design (Review Focus 4) ------------------------------------------------------------

test_that("knots and levels are fixed before mice: observed-data knots, identical names in every imputation", {
  d <- .im_nhanes()
  h <- ifelse(is.na(d$hyp), NA_character_, ifelse(d$hyp == 2, "yes", "no"))
  h[which(h == "no")[1:2]] <- "maybe"                          # a level with two cases
  d$hyp <- h
  expect_identical(sum(d$hyp == "maybe", na.rm = TRUE), 2L)
  s <- list(purpose = "explain-causal", outcome = "chl", exposure = "age",
            terms = list(list(var = "bmi", type = "rcs", knots = 3), list(var = "hyp", type = "factor", ref = "no")),
            layers = .im_layers(), mi = list(m = 5), seed = 4L)
  v <- .im_value(d, s, "lm")
  obs_knots <- Hmisc::rcspline.eval(d$bmi[!is.na(d$bmi)], nk = 3, knots.only = TRUE)
  expect_equal(v$mi$knots[["t2"]], obs_knots, tolerance = 1e-14)
  expect_identical(v$levels[["t3"]], c("no", "maybe", "yes"))
  expect_length(v$mi$coef_names, 5L)
  expect_length(unique(v$mi$coef_names), 1L)
  expect_identical(v$mi$coef_names[[1L]], c("(Intercept)", "t1", "t2", "t2_s1", "t3fmaybe", "t3fyes"))
  expect_identical(v$coef$term, v$mi$coef_names[[1L]])
  cols <- v$mi$columns
  expect_identical(cols$column, c("t1", "t2", "t3", ".y"))
  expect_identical(cols$source, c("age", "bmi", "hyp", "chl"))
  expect_identical(cols$role, c("exposure", "term", "term", "outcome"))
  expect_identical(cols$method, c("", "pmm", "polyreg", "pmm"))
  expect_equal(cols$n_missing, unname(colSums(is.na(d[c("age", "bmi", "hyp", "chl")]))))
  expect_identical(v$mi$maxit, 10L)                             # the default
  res <- .im_present(v, s, "lm")
  expect_identical(res$effects$term, c("age", "bmi", "bmi (spline 1)", "hyp: maybe vs no", "hyp: yes vs no"))
  expect_match(res$fragments$mi_note, "polyreg", fixed = TRUE)
  # a two-level factor is imputed by logistic regression
  d2 <- .im_nhanes()
  d2$hyp <- ifelse(is.na(d2$hyp), NA_character_, ifelse(d2$hyp == 2, "yes", "no"))
  s2 <- .im_lm_spec(terms = list(list(var = "hyp", type = "factor")), mi = list(m = 3))
  v2 <- .im_value(d2, s2, "lm")
  expect_identical(v2$mi$columns$method, c("pmm", "logreg", "pmm"))
  expect_length(unique(v2$mi$coef_names), 1L)
})

test_that("the emitted wrapper is deterministic, survives the JSON round trip and leaves cc code alone", {
  r <- .im_route("lm")
  s <- .im_lm_spec()
  a <- sr_compile(r, s)
  expect_identical(a, sr_compile(r, s))
  expect_silent(.sr_emit_check(a, "analysis"))
  back <- .sr_spec_from_state(jsonlite::fromJSON(jsonlite::toJSON(s, auto_unbox = TRUE), simplifyVector = FALSE))
  expect_identical(sr_compile(r, back), a)
  expect_match(a, "mice::mice", fixed = TRUE)
  cc <- .im_lm_spec(layers = .im_layers(missing = "cc"), mi = NULL)
  expect_false(grepl("mice", sr_compile(r, cc), fixed = TRUE))
  # setup (levels, knots) comes before the imputation
  expect_lt(regexpr(".sr_fixed <- list", a, fixed = TRUE), regexpr("mice::mice", a, fixed = TRUE))
  expect_true(sr_spec_validate(s, r)$ok)
  expect_true(sr_compose_check(s)$ok)
  expect_identical(r$estimand(s)$population, "all analysed (MI)")
})

# ---- the choice of m ------------------------------------------------------------------------------

test_that("m = auto runs a pilot of 20 and reimputes with the two-stage m from the pilot FMI", {
  d <- .im_nhanes()
  s <- .im_lm_spec(mi = list(m = "auto"))
  v <- .im_value(d, s, "lm")
  expect_identical(v$mi$m_pilot, 20L)
  f <- v$mi$fmi_max
  expect_true(is.numeric(f) && length(f) == 1L && f > 0 && f < 1)
  up <- stats::plogis(stats::qlogis(f) + 1.96 * sqrt(2 / 20))
  need <- ceiling(1 + 0.5 * (up / 0.05)^2)
  expect_identical(v$mi$m_final, as.integer(min(200, max(20, need))))
  expect_identical(v$mi$m_capped, need > 200)
  expect_identical(v$mi$m, v$mi$m_final)
  expect_length(v$mi$coef_names, v$mi$m_final)                  # the final run reimputes with that m
  res <- .im_present(v, s, "lm")
  au <- res$fragments$mi_auto
  expect_match(au, "pilot", fixed = TRUE)
  expect_match(au, "20", fixed = TRUE)
  expect_match(au, as.character(v$mi$m_final), fixed = TRUE)
  expect_match(res$fragments$mi_note, sprintf("m = %d", v$mi$m_final), fixed = TRUE)
  # a fixed m reports no pilot
  v5 <- .im_value(d, .im_lm_spec(), "lm")
  expect_true(is.na(v5$mi$m_pilot))
  expect_identical(v5$mi$m_final, 5L)
  expect_false(v5$mi$m_capped)
  expect_equal(v5$mi$fmi_max, v5$coef$fmi[v5$coef$term == "t1"], tolerance = 1e-14)   # the primary term only
})

test_that("spec$mi is validated at compile: m, maxit, aux", {
  r <- .im_route("lm")
  expect_error(sr_compile(r, .im_lm_spec(mi = NULL)), "does not implement the layer")   # no m: not pre-specified
  expect_error(sr_compile(r, .im_lm_spec(mi = list(maxit = 5))), "spec$mi$m", fixed = TRUE)
  for (bad in list(1, 2.5, -3, "many", NA, c(5, 10))) {
    expect_error(sr_compile(r, .im_lm_spec(mi = list(m = bad))), "spec$mi$m", fixed = TRUE)
  }
  for (bad in list(0, 1.5, "ten")) {
    expect_error(sr_compile(r, .im_lm_spec(mi = list(m = 5, maxit = bad))), "maxit", fixed = TRUE)
  }
  expect_error(sr_compile(r, .im_lm_spec(mi = list(m = 5, aux = "bmi"))), "aux", fixed = TRUE)
  expect_error(sr_compile(r, .im_lm_spec(mi = list(m = 5, aux = 3))), "aux", fixed = TRUE)
  expect_silent(sr_compile(r, .im_lm_spec(mi = list(m = 2, maxit = 1, aux = "hyp"))))
  # the other unimplemented layers stay refused under MI
  expect_error(sr_compile(r, .im_lm_spec(layers = .im_layers(weights = "ipw"))), "does not implement")
  expect_error(sr_compile(r, .im_lm_spec(layers = .im_layers(resampling = "bootstrap"))), "mi_x_boot")
})

test_that("MI with clustered data or with the Firth penalty is refused at compile, naming the combination", {
  lmr <- .im_route("lm")
  s <- .im_lm_spec(se = "cluster", cluster = "age", layers = .im_layers(clustered = TRUE))
  expect_error(sr_compile(lmr, s), "multiple imputation.*clustered")
  expect_error(sr_compile(lmr, .im_lm_spec(layers = .im_layers(clustered = TRUE))), "multiple imputation.*clustered")
  g <- list(purpose = "explain-causal", outcome = "hyp", event_value = "2", exposure = "bmi",
            layers = .im_layers(), mi = list(m = 5), seed = 1L)
  lg <- .im_route("logistic")
  expect_silent(sr_compile(lg, g))
  gf <- g
  gf$penalty <- "firth"
  gf$marginal <- FALSE
  expect_error(sr_compile(lg, gf), "multiple imputation.*firth")
  gc <- g
  gc$se <- "cluster"
  gc$cluster <- "age"
  gc$layers <- .im_layers(clustered = TRUE)
  expect_error(sr_compile(lg, gc), "multiple imputation.*clustered")
  expect_error(sr_compile(.im_route("modpois"), gc), "multiple imputation.*clustered")
  cx <- .im_cox_spec(cluster = "inst", layers = .im_layers(clustered = TRUE))
  expect_error(sr_compile(.im_route("cox"), cx), "multiple imputation.*clustered")
})

# ---- auxiliary variables --------------------------------------------------------------------------

test_that("auxiliary columns are predictors of the imputation model only", {
  d <- .im_nhanes()
  s <- .im_lm_spec(terms = NULL, mi = list(m = 3, aux = c("hyp", "age")))
  expect_true(all(c("hyp", "age") %in% sr_spec_cols(s)))
  v <- .im_value(d, s, "lm")
  cols <- v$mi$columns
  expect_identical(cols$column, c("t1", ".y", ".a1", ".a2"))
  expect_identical(cols$role, c("exposure", "outcome", "aux", "aux"))
  expect_identical(cols$source, c("bmi", "chl", "hyp", "age"))
  expect_identical(v$coef$term, c("(Intercept)", "t1"))
  expect_true(all(v$mi$predictors["t1", c(".a1", ".a2")] == 1))
  res <- .im_present(v, s, "lm")
  expect_match(res$fragments$mi_note, "auxiliary", fixed = TRUE)
})

test_that("a character auxiliary column with more than SR_MI_AUX_MAX_LEVELS levels is refused at run time", {
  expect_identical(SR_MI_AUX_MAX_LEVELS, 50L)
  d <- .im_nhanes()
  s <- .im_lm_spec(terms = NULL, mi = list(m = 2, aux = "code"))
  code <- sr_compile(.im_route("lm"), s)
  expect_match(code, ".h_mi_col(.mi_base[[\"code\"]], FALSE, \"code\", 50L)", fixed = TRUE)
  wide <- d[rep(seq_len(nrow(d)), 3L), ]
  wide$id <- seq_len(nrow(wide))
  wide$code <- sprintf("c%02d", seq_len(nrow(wide)))                  # 75 distinct values
  e <- tryCatch(.im_value(wide, s, "lm"), error = function(e) conditionMessage(e))
  expect_match(e, "auxiliary column \"code\" has 75 distinct values (more than 50)", fixed = TRUE)
  expect_match(e, "drop it from the imputation model", fixed = TRUE)
  # a few levels run; exactly 50 levels pass the helper; a stratum is never capped
  ok <- wide
  ok$code <- rep(c("a", "b", "c"), length.out = nrow(ok))
  expect_true(is.list(.im_value(ok, s, "lm")))
  expect_identical(nlevels(.sr_h_mi_col(sprintf("s%03d", 1:50), FALSE, "f", 50L)), 50L)
  expect_identical(nlevels(.sr_h_mi_col(sprintf("s%03d", 1:120), TRUE)), 120L)
  expect_error(.sr_h_mi_col(factor(sprintf("s%03d", 1:51)), FALSE, "f", 50L), "\"f\" has 51 distinct values")
})

# ---- complete data (ruling R25d)------------------------------------------------------------------

test_that("data without missing values still run under MI and pool to the complete-case estimates", {
  s <- list(purpose = "explain-causal", outcome = "mpg", exposure = "am", terms = list(list(var = "wt")),
            layers = .im_layers(), mi = list(m = 3), seed = 2L)
  v <- .im_value(mtcars, s, "lm")
  fit <- stats::lm(mpg ~ am + wt, data = mtcars)
  expect_equal(v$coef$est, unname(stats::coef(fit)), tolerance = 1e-12)
  expect_equal(v$coef$se, unname(sqrt(diag(stats::vcov(fit)))), tolerance = 1e-12)
  expect_equal(v$coef$df, rep((29 + 1) / (29 + 3) * 29, 3L), tolerance = 1e-12)   # df_obs: no missing information
  expect_identical(v$coef$riv, c(0, 0, 0))
  expect_identical(v$coef$mcse, c(0, 0, 0))
  expect_false(anyNA(v$coef[c("est", "se", "lo", "hi", "p", "df", "riv", "lambda", "fmi", "mcse")]))
  expect_true(all(v$mi$columns$method == ""))
  res <- .im_present(v, s, "lm")
  expect_true(all(res$effects$mcse == 0))
  expect_true(all(is.finite(c(res$effects$lo, res$effects$hi))))
  # a glm has no finite complete-data df: the pooled df is infinite and the FMI is 0
  d <- mtcars
  g <- list(purpose = "explain-causal", outcome = "vs", exposure = "am", terms = list(list(var = "wt")),
            layers = .im_layers(), mi = list(m = 3), seed = 2L)
  vg <- .im_value(d, g, "logistic")
  gfit <- stats::glm(vs ~ am + wt, data = d, family = stats::binomial())
  expect_equal(vg$coef$est, unname(stats::coef(gfit)), tolerance = 1e-10)
  expect_equal(vg$coef$se, unname(sqrt(diag(stats::vcov(gfit)))), tolerance = 1e-10)
  expect_identical(vg$coef$df, c(Inf, Inf, Inf))
  expect_identical(vg$coef$fmi, c(0, 0, 0))
  expect_false(anyNA(vg$coef[c("est", "se", "lo", "hi", "p", "df", "riv", "lambda", "fmi", "mcse")]))
  rg <- .im_present(vg, g, "logistic")
  rd <- rg$effects[rg$effects$measure == "RD", ]
  cc <- .sr_h_gcomp(gfit, d, "am", c(1, 0), stats::vcov(gfit))
  expect_equal(rd$est, cc$rd, tolerance = 1e-10)
  expect_equal(rd$lo, cc$rd - stats::qnorm(0.975) * cc$rd_se, tolerance = 1e-10)
})

# ---- binary outcomes: ratios pooled on the log scale, marginal effects per imputation -----------

.im_bin_df <- function(n = 300L, seed = 5L) {
  withr::with_seed(seed, {
    x1 <- stats::rnorm(n)
    a <- stats::rbinom(n, 1L, stats::plogis(0.3 * x1))
    y <- stats::rbinom(n, 1L, stats::plogis(-0.8 + 0.7 * a + 0.5 * x1))
    x1[sample(n, 40L)] <- NA
    y[sample(n, 25L)] <- NA
    a[sample(n, 20L)] <- NA
    data.frame(y = y, a = a, x1 = x1)
  })
}

# The same imputations as the wrapper (same frame, same seed), then the fit and g-computation in each.
.im_bin_direct <- function(d, seed, family, se = "model") {
  frame <- data.frame(t1 = as.numeric(d$a), t2 = d$x1, .y = factor(d$y, levels = c(0L, 1L)))
  imp <- withr::with_seed(seed, mice::mice(frame, m = 5, maxit = 10, printFlag = FALSE))
  est <- vc <- sc <- sv <- list()
  ev <- numeric()
  for (i in 1:5) {
    ci <- mice::complete(imp, i)
    dd <- data.frame(.y = as.integer(as.character(ci$.y)), t1 = ci$t1, t2 = ci$t2)
    fit <- suppressWarnings(stats::glm(.y ~ t1 + t2, data = dd, family = family))
    V <- .sr_h_vcov(fit, se)
    g <- .sr_h_gcomp(fit, dd, "t1", c(1, 0), V)
    est[[i]] <- stats::coef(fit)
    vc[[i]] <- V
    sc[[i]] <- c(rd = g$rd, lrr = g$log_rr)
    sv[[i]] <- diag(c(rd = g$rd_se, lrr = g$log_rr_se)^2)
    dimnames(sv[[i]]) <- list(c("rd", "lrr"), c("rd", "lrr"))
    ev[i] <- sum(dd$.y)
  }
  list(coef = .sr_h_rubin(est, vc, Inf), marg = .sr_h_rubin(sc, sv, Inf), events = mean(ev))
}

test_that("logistic under MI pools the log OR and the per-imputation g-computation RD (identity) and RR (log)", {
  d <- .im_bin_df()
  s <- list(purpose = "explain-causal", outcome = "y", exposure = "a", terms = list(list(var = "x1")),
            layers = .im_layers(), mi = list(m = 5), seed = 3L)
  v <- .im_value(d, s, "logistic")
  ref <- .im_bin_direct(d, 3L, stats::binomial())
  expect_equal(v$coef$est, ref$coef$est, tolerance = 1e-10)
  expect_equal(v$coef$se, ref$coef$se, tolerance = 1e-10)
  expect_identical(v$mi$columns$method, c("pmm", "pmm", "logreg"))
  expect_identical(v$n, 300L)
  expect_equal(v$events, ref$events, tolerance = 1e-12)         # the mean over the imputations
  res <- .im_present(v, s, "logistic")
  e <- res$effects
  or <- e[e$measure == "OR" & e$role == "primary", ]
  expect_equal(or$est, exp(ref$coef$est[2L]), tolerance = 1e-10)
  expect_equal(or$lo, exp(ref$coef$lo[2L]), tolerance = 1e-10)
  expect_equal(or$hi, exp(ref$coef$hi[2L]), tolerance = 1e-10)
  expect_equal(or$mcse, ref$coef$mcse[2L], tolerance = 1e-12)   # on the pooling (log) scale
  expect_identical(or$ci_method, "rubin")
  rd <- e[e$measure == "RD", ]
  expect_equal(rd$est, ref$marg$est[1L], tolerance = 1e-10)
  expect_equal(rd$lo, ref$marg$lo[1L], tolerance = 1e-10)
  expect_equal(rd$hi, ref$marg$hi[1L], tolerance = 1e-10)
  expect_equal(rd$mcse, ref$marg$mcse[1L], tolerance = 1e-12)
  expect_identical(rd$role, "primary")
  expect_identical(rd$ci_method, "rubin")
  rr <- e[e$measure == "RR", ]
  expect_equal(rr$est, exp(ref$marg$est[2L]), tolerance = 1e-10)
  expect_equal(rr$lo, exp(ref$marg$lo[2L]), tolerance = 1e-10)
  expect_equal(rr$hi, exp(ref$marg$hi[2L]), tolerance = 1e-10)
  expect_identical(rr$ci_method, "rubin")
  expect_true("NNT" %in% e$measure)
  expect_match(res$fragments$glm_marginal, "each imputed dataset", fixed = TRUE)
  expect_match(res$fragments$mi_note, "log scale", fixed = TRUE)
  expect_match(res$fragments$mi_note, "logreg", fixed = TRUE)
  expect_match(res$fragments$mi_note, "25 row(s) with an imputed outcome", fixed = TRUE)
  # the observed risk is descriptive: rows with an observed exposure and outcome
  ob <- e[e$measure == "risk", ]
  cc <- d[!is.na(d$a) & !is.na(d$y), ]
  expect_equal(ob$n, as.numeric(table(cc$a)))
  expect_equal(ob$est, as.numeric(tapply(cc$y, cc$a, mean)), tolerance = 1e-12)
})

test_that("modified Poisson under MI pools the log RR with the robust variance of each imputation", {
  d <- .im_bin_df()
  s <- list(purpose = "explain-causal", outcome = "y", exposure = "a", terms = list(list(var = "x1")),
            layers = .im_layers(), mi = list(m = 5), seed = 3L)
  v <- .im_value(d, s, "modpois")
  ref <- .im_bin_direct(d, 3L, stats::poisson(link = "log"), se = "robust")
  expect_equal(v$coef$est, ref$coef$est, tolerance = 1e-10)
  expect_equal(v$coef$se, ref$coef$se, tolerance = 1e-10)
  res <- .im_present(v, s, "modpois")
  rr <- res$effects[res$effects$measure == "RR" & res$effects$role == "primary", ]
  expect_equal(rr$est[1L], exp(ref$coef$est[2L]), tolerance = 1e-10)
  expect_true(all(res$effects$ci_method[res$effects$measure %in% c("RR", "RD")] == "rubin"))
})

# ---- survival -------------------------------------------------------------------------------------

test_that("Cox under MI: Nelson-Aalen and the event predict, the time does not; rows without time or status are dropped", {
  d <- survival::lung
  s <- .im_cox_spec()
  v <- .im_value(d, s, "cox")
  cols <- v$mi$columns
  expect_identical(cols$column, c("t1", "t2", "t3", ".event", ".na"))
  expect_identical(cols$role, c("exposure", "term", "term", "event", "nelson_aalen"))
  expect_false("time" %in% cols$source)                         # the time itself is not in the imputation model
  expect_identical(colnames(v$mi$predictors), c("t1", "t2", "t3", ".event", ".na"))
  expect_true(all(v$mi$predictors["t3", c(".event", ".na")] == 1))
  expect_identical(cols$method, c("", "", "pmm", "", ""))
  expect_identical(v$mi$n_dropped, 0L)

  ev <- as.integer(d$status == 2)
  frame <- data.frame(t1 = as.numeric(d$sex), t2 = as.numeric(d$age), t3 = as.numeric(d$wt.loss),
                      .event = as.numeric(ev), .na = .im_na(d$time, ev))
  imp <- withr::with_seed(11L, mice::mice(frame, m = 3, maxit = 10, printFlag = FALSE))
  est <- vc <- list()
  for (i in 1:3) {
    ci <- mice::complete(imp, i)
    dd <- data.frame(.time = d$time, .event = ev, ci[c("t1", "t2", "t3")])
    fit <- survival::coxph(survival::Surv(.time, .event) ~ t1 + t2 + t3, data = dd)
    est[[i]] <- stats::coef(fit)
    vc[[i]] <- stats::vcov(fit)
  }
  ref <- .sr_h_rubin(est, vc, Inf)
  expect_equal(v$coef$est, ref$est, tolerance = 1e-10)
  expect_equal(v$coef$se, ref$se, tolerance = 1e-10)
  res <- .im_present(v, s, "cox")
  hr <- res$effects[res$effects$role == "primary", ]
  expect_identical(hr$measure, "HR")
  expect_equal(hr$est, exp(ref$est[1L]), tolerance = 1e-10)
  expect_equal(hr$lo, exp(ref$lo[1L]), tolerance = 1e-10)
  expect_identical(hr$ci_method, "rubin")
  expect_equal(hr$n, 228)
  expect_equal(hr$events, sum(ev))
  expect_match(res$fragments$mi_note, "Nelson-Aalen", fixed = TRUE)
  expect_true(all(c("km", "km_risk") %in% names(res$figures)))

  # missing time or status: dropped before imputation, never imputed, and counted
  d2 <- d
  d2$time[c(3L, 50L)] <- NA
  d2$status[7L] <- NA
  v2 <- .im_value(d2, s, "cox")
  expect_identical(v2$mi$n_dropped, 3L)
  expect_identical(v2$n, 225L)
  expect_identical(v2$n_total, 228L)
  r2 <- .im_present(v2, s, "cox")
  expect_match(r2$fragments$mi_note, "3 row(s) with a missing time or status were dropped", fixed = TRUE)
  # strata are part of the imputation model
  s3 <- .im_cox_spec(strata = "ph.ecog")
  v3 <- .im_value(d, s3, "cox")
  expect_identical(v3$mi$columns$role, c("exposure", "term", "term", "event", "nelson_aalen", "strata"))
  expect_identical(v3$coef$term, c("t1", "t2", "t3"))
})

test_that("RMST under MI pools the per-imputation difference (variance se1^2 + se0^2) on the identity scale", {
  d <- survival::lung
  d$sex[withr::with_seed(9L, sample(nrow(d), 25L))] <- NA
  d$time[c(3L, 50L)] <- NA
  s <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 2, exposure = "sex",
            tau = 365, layers = .im_layers(), mi = list(m = 4), seed = 6L)
  r <- .im_route("rmst")
  expect_true(sr_spec_validate(s, r)$ok)
  expect_identical(r$estimand(s)$population, "all analysed (MI)")
  v <- .im_value(d, s, "rmst")
  k <- d[!is.na(d$time) & !is.na(d$status), ]
  ev <- as.integer(k$status == 2)
  frame <- data.frame(t1 = factor(as.character(k$sex), levels = c("1", "2")), .event = as.numeric(ev),
                      .na = .im_na(k$time, ev))
  imp <- withr::with_seed(6L, mice::mice(frame, m = 4, maxit = 10, printFlag = FALSE))
  de <- dv <- ge <- gv <- list()
  for (i in 1:4) {
    g <- as.character(mice::complete(imp, i)$t1)
    r1 <- .sr_h_rmst(k$time[g == "1"], ev[g == "1"], 365)
    r2 <- .sr_h_rmst(k$time[g == "2"], ev[g == "2"], 365)
    de[[i]] <- c(d2 = r2[["rmst"]] - r1[["rmst"]])
    dv[[i]] <- matrix(r2[["se"]]^2 + r1[["se"]]^2, 1L, 1L, dimnames = list("d2", "d2"))
    ge[[i]] <- c(g1 = r1[["rmst"]], g2 = r2[["rmst"]])
    gv[[i]] <- diag(c(g1 = r1[["se"]], g2 = r2[["se"]])^2)
    dimnames(gv[[i]]) <- list(c("g1", "g2"), c("g1", "g2"))
  }
  rd <- .sr_h_rubin(de, dv, Inf)
  rg <- .sr_h_rubin(ge, gv, Inf)
  res <- .im_present(v, s, "rmst")
  e <- res$effects
  df <- e[e$measure == "RMST_diff", ]
  expect_equal(df$est, rd$est, tolerance = 1e-10)
  expect_equal(df$lo, rd$lo, tolerance = 1e-10)
  expect_equal(df$hi, rd$hi, tolerance = 1e-10)
  expect_equal(df$p, rd$p, tolerance = 1e-10)
  expect_equal(df$mcse, rd$mcse, tolerance = 1e-12)
  expect_identical(df$role, "primary")
  expect_identical(df$ci_method, "rubin")
  pg <- e[e$measure == "RMST", ]
  expect_equal(pg$est, rg$est, tolerance = 1e-10)
  expect_equal(pg$lo, rg$lo, tolerance = 1e-10)
  expect_identical(pg$ci_method, c("rubin", "rubin"))
  expect_identical(v$mi$n_dropped, 2L)
  expect_identical(v$mi$columns$column, c("t1", ".event", ".na"))
  expect_identical(v$mi$columns$method, c("logreg", "", ""))
  expect_match(res$fragments$mi_note, "2 row(s) with a missing time or status were dropped", fixed = TRUE)
  expect_true(all(c("rmst", "rmst_difference", "imputation") %in% names(res$tables)))
  # the horizon check still runs, on the observed rows
  expect_error(.im_value(d, utils::modifyList(s, list(tau = 5000)), "rmst"), "exceeds the follow-up")
})

# ---- sr_missing_summary ---------------------------------------------------------------------------

test_that("sr_missing_summary counts missing values per column and the most frequent patterns", {
  df <- .im_nhanes()
  p <- .fx_ready_project(df, "t", c(id = "id", chl = "outcome"))
  cols <- c("age", "bmi", "hyp", "chl")
  ms <- sr_missing_summary(p, "t", cols)
  expect_identical(names(ms), c("columns", "patterns"))
  expect_identical(names(ms$columns), c("column", "n_missing", "pct_missing"))
  expect_identical(ms$columns$column, cols)
  expect_equal(ms$columns$n_missing, unname(colSums(is.na(df[cols]))))
  expect_equal(ms$columns$pct_missing, unname(100 * colMeans(is.na(df[cols]))), tolerance = 1e-12)
  pat <- ms$patterns
  expect_true(is.data.frame(pat) && nrow(pat) >= 1L && nrow(pat) <= 10L)
  key <- apply(is.na(df[cols]), 1L, function(r) paste(cols[r], collapse = ", "))
  tb <- table(key)
  expect_equal(sum(pat$n), nrow(df))                            # nhanes has fewer than 10 patterns
  expect_equal(pat$n, sort(as.numeric(tb), decreasing = TRUE))  # most frequent first
  expect_identical(pat$missing[1L], "")                         # the complete rows are the most frequent
  expect_equal(pat$n[1L], sum(stats::complete.cases(df[cols])))
  expect_setequal(pat$missing, names(tb))
  expect_equal(pat$n[match(names(tb), pat$missing)], as.numeric(tb))
  expect_equal(pat$pct, 100 * pat$n / nrow(df), tolerance = 1e-12)
  expect_error(sr_missing_summary(p, "t", "nope"), "unknown column")
  expect_error(sr_missing_summary(p, "zz", "age"), "not in the project store")
  expect_error(sr_missing_summary(p, "t", character()), "cols")
  one <- sr_missing_summary(p, "t", "bmi")
  expect_equal(one$columns$n_missing, sum(is.na(df$bmi)))
  expect_equal(sort(one$patterns$n), sort(as.numeric(table(is.na(df$bmi)))))
})
