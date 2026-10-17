# Textbook reproductions: published values reproduced through the public path only
# (.fx_ready_project -> sr_sap_add -> sr_sap_lock -> sr_run -> sr_result). Public package data
# (survival::lung, survival::gbsg, mice::nhanes) and, when the user supplies it, the NHEFS CSV of
# Hernan & Robins (read from the path in env SR_NHEFS_CSV; it is never committed and the test skips
# when the variable is unset). Projects live under withr::local_tempdir().

.tb_layers <- function(...) {
  utils::modifyList(list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                         framework = "frequentist", clustered = FALSE), list(...))
}

# The routes, registered again when an earlier test file emptied the registry.
.tb_routes <- function() {
  make <- list(lm = .sr_lm_route, cox = .sr_cox_route, rmst = .sr_rmst_route)
  for (k in names(make)) {
    if (!(.sr_route_key(k, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make[[k]]())
  }
  invisible(TRUE)
}

# Add one primary analysis, lock the SAP, run it and return the project; the run must succeed.
.tb_run <- function(df, roles, route, spec, env = parent.frame()) {
  p <- .fx_ready_project(df, "t", roles, env = env)
  .tb_routes()
  p <- sr_sap_add(p, "a1", route, spec, "t", label = "primary")
  p <- sr_sap_lock(p)
  p <- sr_run(p, "a1")
  expect_true(p$state$runs$a1$ok, info = p$state$runs$a1$error)
  p
}

# The run's value object as saved by the engine (the unpresented numbers).
.tb_value <- function(p) {
  run <- p$state$runs$a1
  readRDS(file.path(p$paths$fits, paste0(run$ok_key, ".rds")))$value
}

# ---- survival::lung: HR for sex and the RMST difference -----------------------------------------

test_that("lung: HR for sex is 0.588 (0.424, 0.816) through the public path", {
  d <- survival::lung
  d$id <- seq_len(nrow(d))
  s <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 2,
            exposure = "sex", layers = .tb_layers(), seed = 1L)
  p <- .tb_run(d, c(id = "id", status = "outcome"), "cox", s)
  res <- sr_result(p, "a1")
  e <- res$effects[res$effects$measure == "HR" & res$effects$term == "sex", ]
  expect_equal(nrow(e), 1L)
  expect_equal(round(c(e$est, e$lo, e$hi), 3), c(0.588, 0.424, 0.816))
  # the published printing; sr_fmt_ci joins the limits with " to " (pinned in test-numfmt.R)
  expect_identical(sr_fmt_ci(e$est, e$lo, e$hi), "0.59 (0.42 to 0.82)")
  fit <- survival::coxph(survival::Surv(time, status) ~ sex, data = d)
  expect_equal(e$est, unname(exp(stats::coef(fit))), tolerance = 1e-8)
})

test_that("lung: the RMST difference by sex up to 365 days is 55.9703 through the public path", {
  d <- survival::lung
  d$id <- seq_len(nrow(d))
  s <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 2,
            exposure = "sex", tau = 365, layers = .tb_layers(), seed = 1L)
  p <- .tb_run(d, c(id = "id", status = "outcome"), "rmst", s)
  res <- sr_result(p, "a1")
  dif <- res$effects[res$effects$measure == "RMST_diff", ]
  expect_equal(nrow(dif), 1L)
  expect_equal(dif$est, 55.9703, tolerance = 1e-4)
  tab <- summary(survival::survfit(survival::Surv(time, status == 2) ~ sex, d), rmean = 365)$table
  expect_equal(dif$est, unname(tab[2, "rmean"] - tab[1, "rmean"]), tolerance = 1e-8)
})

# ---- survival::gbsg: the multivariable Cox model ------------------------------------------------

test_that("gbsg: the multivariable Cox model through the public path equals a direct coxph", {
  g <- survival::gbsg
  g$id <- seq_len(nrow(g))
  s <- list(purpose = "explain-causal", time = "rfstime", status = "status", exposure = "hormon",
            terms = list(list(var = "age"), list(var = "meno"), list(var = "size"), list(var = "grade"),
                         list(var = "nodes"), list(var = "pgr"), list(var = "er")),
            layers = .tb_layers(), seed = 1L)
  p <- .tb_run(g, c(id = "id", status = "outcome"), "cox", s)
  res <- sr_result(p, "a1")
  e <- res$effects[res$effects$measure == "HR", ]
  fit <- survival::coxph(survival::Surv(rfstime, status) ~ hormon + age + meno + size + grade + nodes + pgr + er,
                         data = g)
  se <- sqrt(diag(stats::vcov(fit)))
  expect_equal(e$term, c("hormon", "age", "meno", "size", "grade", "nodes", "pgr", "er"))
  expect_equal(e$est, unname(exp(stats::coef(fit))), tolerance = 1e-8)
  expect_equal(e$lo, unname(exp(stats::coef(fit) - stats::qnorm(0.975) * se)), tolerance = 1e-8)
  expect_equal(e$hi, unname(exp(stats::coef(fit) + stats::qnorm(0.975) * se)), tolerance = 1e-8)
  expect_identical(e$role, c("primary", rep("covariate", 7L)))
  expect_true(all(e$n == 686) && all(e$events == sum(g$status)))
})

# ---- mice::nhanes: lm under multiple imputation equals mice::pool -------------------------------

test_that("nhanes: lm(chl ~ bmi + age) under MI (m = 5, seed 1) equals mice::pool", {
  nh <- mice::nhanes
  d <- data.frame(id = seq_len(nrow(nh)), age = as.numeric(nh$age), bmi = nh$bmi, hyp = nh$hyp, chl = nh$chl)
  s <- list(purpose = "explain-causal", outcome = "chl", exposure = "bmi", terms = list(list(var = "age")),
            layers = .tb_layers(missing = "mi"), mi = list(m = 5, maxit = 10), seed = 1L)
  p <- .tb_run(d, c(id = "id", chl = "outcome"), "lm", s)
  res <- sr_result(p, "a1")
  val <- .tb_value(p)
  # the rows the run saw and the imputation frame the wrapper builds (exposure, terms, outcome)
  run <- p$state$runs$a1
  xf <- file.path(p$paths$dir, "data", "extracts", paste0(run$extract_sha, ".rds"))
  ext <- unserialize(readBin(xf, "raw", n = file.size(xf)))
  frame <- data.frame(t1 = as.numeric(ext$bmi), t2 = as.numeric(ext$age), .y = as.numeric(ext$chl))
  withr::local_preserve_seed()
  old <- RNGkind()
  withr::defer(RNGkind(old[1L], old[2L], old[3L]))
  eval(parse(text = sr_rng_preamble(1)))
  imp <- mice::mice(frame, m = 5, maxit = 10, printFlag = FALSE)
  sm <- summary(mice::pool(with(imp, lm(.y ~ t1 + t2))))
  expect_identical(val$mi$m, 5L)
  expect_equal(val$coef$est, sm$estimate, tolerance = 1e-10)
  expect_equal(val$coef$se, sm$std.error, tolerance = 1e-10)
  expect_equal(val$coef$df, sm$df, tolerance = 1e-8)
  expect_identical(res$effects$term, c("bmi", "age"))
  expect_equal(res$effects$est, sm$estimate[-1L], tolerance = 1e-10)
  expect_equal(res$effects$lo, (sm$estimate - stats::qt(0.975, sm$df) * sm$std.error)[-1L], tolerance = 1e-8)
  expect_equal(res$effects$hi, (sm$estimate + stats::qt(0.975, sm$df) * sm$std.error)[-1L], tolerance = 1e-8)
  expect_true(all(res$effects$ci_method == "rubin"))
})

# ---- NHEFS (Hernan & Robins, ch. 13): standardisation --------------------------------------------

# The book's outcome model (Programs 13.2-13.4, Chapter 13): wt82_71 ~ qsmk + sex + race + age + age^2 + education
# (factor) + smokeintensity + smokeintensity^2 + smokeyrs + smokeyrs^2 + exercise (factor) + active
# (factor) + wt71 + wt71^2 + qsmk x smokeintensity, fitted on the rows with a measured wt82_71 and
# standardised over every row. standardize_over = "all" relies on the lm code keeping the rows lost
# only to a missing outcome in the design and averaging the counterfactual predictions over them
# (test-marginal.R pins that); in NHEFS the covariates are complete, so "all" is 1629 rows and the
# fit 1566.
# The model has smokeintensity both squared and in the interaction, and P1c interactions take linear
# or factor terms only, so the square is a column of the data (smokeintensity_sq, added by
# .tb_nhefs_prep) entered as a linear term: the same model.
.tb_nhefs_spec <- function(boot = TRUE, B = 2000L, seed = 1L) {
  s <- list(purpose = "explain-causal", outcome = "wt82_71", exposure = "qsmk",
            terms = list(list(var = "sex"), list(var = "race"),
                         list(var = "education", type = "factor"),
                         list(var = "exercise", type = "factor"),
                         list(var = "active", type = "factor"),
                         list(var = "age", type = "quad"),
                         list(var = "smokeintensity"), list(var = "smokeintensity_sq"),
                         list(var = "smokeyrs", type = "quad"),
                         list(var = "wt71", type = "quad")),
            interactions = list(c("qsmk", "smokeintensity")),
            marginal = TRUE, standardize_over = "all",
            layers = .tb_layers(resampling = if (boot) "bootstrap" else "none"), seed = seed)
  if (boot) s$boot <- list(B = B, type = "percentile")
  s
}

.tb_nhefs_prep <- function(d) {
  d$id <- seq_len(nrow(d))
  d$smokeintensity_sq <- d$smokeintensity^2
  d
}

.tb_nhefs_cols <- c("qsmk", "sex", "race", "education", "exercise", "active", "age", "smokeintensity",
                    "smokeyrs", "wt71", "wt82_71")

# A synthetic stand-in with the NHEFS column names and coding (the book's CSV is not shipped):
# 0/1 qsmk, sex and race, education 1-5, exercise and active 0-2, and 4% missing outcomes.
.tb_nhefs_standin <- function(n = 300L, seed = 14L) {
  withr::with_seed(seed, {
    age <- sample(25:74, n, replace = TRUE)
    sex <- stats::rbinom(n, 1L, 0.5)
    smokeintensity <- sample(c(1:40, 60), n, replace = TRUE)
    wt71 <- round(stats::rnorm(n, 70, 12), 1)
    qsmk <- stats::rbinom(n, 1L, stats::plogis(-0.8 + 0.01 * (age - 45)))
    y <- 2 + 3 * qsmk - 0.05 * wt71 - 0.02 * smokeintensity * qsmk + stats::rnorm(n, 0, 6)
    y[sample.int(n, round(0.04 * n))] <- NA
    data.frame(id = seq_len(n), qsmk = qsmk, sex = sex, race = stats::rbinom(n, 1L, 0.15),
               education = sample(1:5, n, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.2, 0.2)),
               exercise = sample(0:2, n, replace = TRUE), active = sample(0:2, n, replace = TRUE),
               age = age, smokeintensity = smokeintensity,
               smokeyrs = pmax(1L, age - 17L - sample(0:15, n, replace = TRUE)),
               wt71 = wt71, wt82_71 = round(y, 2))
  })
}

# The book's g-computation by hand: lm on the rows with a measured outcome, counterfactual
# predictions averaged over every row.
.tb_nhefs_hand <- function(d) {
  f <- stats::lm(wt82_71 ~ qsmk + sex + race + factor(education) + factor(exercise) + factor(active) +
                   age + I(age^2) + smokeintensity + I(smokeintensity^2) + smokeyrs + I(smokeyrs^2) +
                   wt71 + I(wt71^2) + qsmk:smokeintensity, data = d)
  d1 <- d
  d1$qsmk <- 1
  d0 <- d
  d0$qsmk <- 0
  mean(stats::predict(f, d1)) - mean(stats::predict(f, d0))
}

test_that("the NHEFS spec is accepted, compiles and runs on a stand-in frame (its g-computation is the book's)", {
  d <- .tb_nhefs_prep(.tb_nhefs_standin())
  roles <- c(id = "id", wt82_71 = "outcome")
  .tb_routes()
  expect_true(sr_spec_validate(.tb_nhefs_spec(), sr_route_get("lm", 1L))$ok)
  code <- sr_compile(sr_route_get("lm", 1L), .tb_nhefs_spec())
  expect_silent(.sr_emit_check(code, "analysis"))
  # without the bootstrap, through the public path: the marginal estimate is the hand computation
  p <- .tb_run(d, roles, "lm", .tb_nhefs_spec(boot = FALSE))
  v <- .tb_value(p)
  expect_equal(v$n, sum(!is.na(d$wt82_71)))
  expect_equal(v$marginal$n_over, nrow(d))
  e <- sr_result(p, "a1")$effects
  m <- e[grepl("marginal", e$term) & e$role == "primary", ]
  expect_equal(nrow(m), 1L)
  expect_equal(m$est, .tb_nhefs_hand(d), tolerance = 1e-8)
  # with the bootstrap (small B): the estimate is unchanged and the percentile interval is finite
  p2 <- .tb_run(d, roles, "lm", .tb_nhefs_spec(B = 30L))
  e2 <- sr_result(p2, "a1")$effects
  m2 <- e2[grepl("marginal", e2$term) & e2$role == "primary", ]
  expect_equal(nrow(m2), 1L)
  expect_equal(m2$est, m$est, tolerance = 1e-10)
  expect_identical(m2$ci_method, "boot_percentile")
  expect_true(all(is.finite(c(m2$lo, m2$hi))) && m2$lo < m2$hi)
})

test_that("NHEFS: the standardised mean difference in weight change is 3.52 (2.6, 4.5), B = 2000", {
  skip_if(Sys.getenv("SR_NHEFS_CSV") == "", "SR_NHEFS_CSV is not set (the NHEFS CSV is never committed)")
  raw <- utils::read.csv(Sys.getenv("SR_NHEFS_CSV"))
  expect_true(all(.tb_nhefs_cols %in% names(raw)))
  d <- .tb_nhefs_prep(raw[, .tb_nhefs_cols])
  expect_equal(nrow(d), 1629L)
  expect_equal(sum(!is.na(d$wt82_71)), 1566L)
  p <- .tb_run(d, c(id = "id", wt82_71 = "outcome"), "lm", .tb_nhefs_spec(B = 2000L, seed = 1L))
  v <- .tb_value(p)
  expect_equal(v$n, 1566)
  expect_equal(v$marginal$n_over, 1629)
  e <- sr_result(p, "a1")$effects
  m <- e[grepl("marginal", e$term) & e$role == "primary", ]
  expect_equal(nrow(m), 1L)
  expect_lt(abs(m$est - 3.52), 0.01)
  expect_lt(abs(m$lo - 2.6), 0.15)
  expect_lt(abs(m$hi - 4.5), 0.15)
  expect_identical(m$ci_method, "boot_percentile")
})
