# Tests for diagnostics/checks.R: the diagnostic checks with effect-size verdicts (sr_diag_checks,
# sr_diag_applicable, sr_diag_compile, sr_diag_run and the diag.completed reducer). Seeded synthetic
# data only (the generators below); projects live under withr::local_tempdir().

.dg_layers <- function(...) {
  utils::modifyList(list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                         framework = "frequentist", clustered = FALSE), list(...))
}

.dg_mod <- function(s, ...) {
  mod <- list(...)
  for (nm in names(mod)) s[nm] <- list(mod[[nm]])
  s
}

.dg_lm <- function(...) {
  .dg_mod(list(purpose = "explain-causal", outcome = "y", exposure = "x", layers = .dg_layers(), seed = 11L), ...)
}
.dg_cox <- function(...) {
  .dg_mod(list(purpose = "explain-causal", time = "time", status = "status", exposure = "x",
               layers = .dg_layers(), seed = 11L), ...)
}
.dg_rmst <- function(...) .dg_mod(.dg_cox(tau = 2), ...)

# The routes, registered again when an earlier test file emptied the registry.
.dg_routes <- function() {
  make <- list(lm = .sr_lm_route, lm_describe = .sr_lm_describe_route, logistic = .sr_logistic_route,
               logistic_describe = .sr_logistic_describe_route, modpois = .sr_modpois_route,
               cox = .sr_cox_route, cox_describe = .sr_cox_describe_route, rmst = .sr_rmst_route,
               table1 = .sr_table1_route)
  for (k in names(make)) {
    if (!(.sr_route_key(k, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make[[k]]())
  }
  invisible(TRUE)
}

.dg_an <- function(route, spec) list(route = route, route_version = 1L, spec = spec)

# Compile one check, vet it as diagnostic code, run it in-process on `df`.
.dg_eval <- function(check, route, spec, df, seed = 1L) {
  .dg_routes()
  code <- sr_diag_compile(check, .dg_an(route, spec))
  expect_silent(.sr_emit_check(code, "diagnostic"))
  env <- new.env(parent = globalenv())
  env$sr_data <- df
  withr::with_seed(seed, eval(parse(text = code), envir = env))
}

# ---- seeded generators --------------------------------------------------------------------------

# Survival times with a binary exposure x: baseline hazard log(2), hazard ratio h1 before t = cut and
# h2 after it (piecewise exponential), administrative censoring at `cens`.
.dg_surv <- function(n, h1, h2, seed, cut = 1, cens = 4) {
  set.seed(seed)
  x <- rbinom(n, 1, 0.5)
  lam <- log(2)
  r1 <- lam * ifelse(x == 1, h1, 1)
  r2 <- lam * ifelse(x == 1, h2, 1)
  e <- rexp(n)
  t <- ifelse(e < r1 * cut, e / r1, cut + (e - r1 * cut) / r2)
  st <- as.integer(t <= cens)
  t <- pmin(t, cens)
  data.frame(time = t, status = st, x = x)
}

# y = 1 + f(x) + 0.3 z + error with sd sd_fun(x); x uniform on (0, 2), z standard normal.
.dg_lin <- function(n, seed, f = function(x) 0.5 * x, sd_fun = function(x) rep(1, length(x))) {
  set.seed(seed)
  x <- runif(n, 0, 2)
  z <- rnorm(n)
  data.frame(y = 1 + f(x) + 0.3 * z + rnorm(n, sd = sd_fun(x)), x = x, z = z)
}

# ---- the catalogue ------------------------------------------------------------------------------

test_that("sr_diag_checks pins the catalogue and its thresholds", {
  ck <- sr_diag_checks()
  expect_identical(names(ck), c("id", "routes", "metric", "warn", "fail", "sensitivity"))
  expect_identical(sort(unique(ck$id)), sort(SR_DIAG_IDS))
  row <- function(id, routes = NULL) {
    r <- ck[ck$id == id, , drop = FALSE]
    if (!is.null(routes)) r <- r[r$routes == routes, , drop = FALSE]
    expect_identical(nrow(r), 1L)
    r
  }
  expect_equal(row("ph")$warn, log(1.25))
  expect_equal(row("ph")$fail, log(1.5))
  expect_identical(row("ph")$routes, "cox")
  expect_identical(sum(ck$id == "influence"), 2L)
  il <- row("influence", "lm,logistic,modpois")
  expect_identical(c(il$warn, il$fail), c(0.5, 1))
  ic <- row("influence", "cox")
  expect_identical(c(ic$warn, ic$fail), c(0.5, 1))
  expect_true(is.na(row("separation")$warn))
  expect_identical(row("separation")$fail, 1)
  expect_identical(row("separation")$routes, "logistic")
  expect_identical(c(row("few_clusters")$warn, row("few_clusters")$fail), c(50, 30))
  expect_equal(c(row("inf_censoring")$warn, row("inf_censoring")$fail), c(log(1.25), log(1.5)))
  expect_match(row("inf_censoring")$metric, "Cox model for censoring")
  expect_identical(row("inf_censoring")$routes, "cox,rmst")
  expect_identical(c(row("linearity")$warn, row("linearity")$fail), c(0.1, 0.25))
  expect_identical(row("linearity")$routes, "lm,logistic,modpois,cox")
  expect_identical(c(row("heteroscedasticity")$warn, row("heteroscedasticity")$fail), c(1.2, 1.5))
  expect_identical(row("heteroscedasticity")$routes, "lm")
  expect_true(all(nzchar(ck$sensitivity)))
  expect_match(row("ph")$sensitivity, "^rmst")
  expect_match(row("separation")$sensitivity, "firth")
  expect_match(row("heteroscedasticity")$sensitivity, "robust")
  expect_match(row("few_clusters")$sensitivity, "B = 2000")
  expect_match(row("inf_censoring")$sensitivity, "limitation")
})

test_that("sr_diag_applicable follows the route and the spec", {
  .dg_routes()
  ap <- function(route, spec) sr_diag_applicable(.dg_an(route, spec))
  lm1 <- .dg_lm(terms = list(list(var = "z")))
  expect_setequal(ap("lm", lm1), c("influence", "linearity", "heteroscedasticity"))
  expect_setequal(ap("lm_describe", .dg_mod(lm1, purpose = "describe")), ap("lm", lm1))
  expect_false("heteroscedasticity" %in% ap("lm", .dg_mod(lm1, se = "robust")))
  cl <- .dg_mod(lm1, se = "cluster", cluster = "g", layers = .dg_layers(clustered = TRUE))
  expect_true("few_clusters" %in% ap("lm", cl))
  expect_false("few_clusters" %in% ap("lm", lm1))
  expect_setequal(ap("logistic", lm1), c("influence", "separation", "linearity"))
  expect_setequal(ap("logistic_describe", .dg_mod(lm1, purpose = "describe")), ap("logistic", lm1))
  # Firth: the unpenalised ML fit is not the analysis model, so no model-fit check applies
  expect_identical(ap("logistic", .dg_mod(lm1, penalty = "firth", marginal = FALSE)), character())
  expect_setequal(ap("modpois", lm1), c("influence", "linearity"))
  # a factor-only model has no linear term to scan
  expect_false("linearity" %in% ap("lm", .dg_lm(exposure_ref = "0")))
  cx <- .dg_cox(terms = list(list(var = "z")))
  expect_setequal(ap("cox", cx), c("ph", "influence", "inf_censoring", "linearity"))
  expect_setequal(ap("cox_describe", .dg_mod(cx, purpose = "describe")), ap("cox", cx))
  cxc <- .dg_mod(cx, cluster = "g", layers = .dg_layers(clustered = TRUE))
  expect_true("few_clusters" %in% ap("cox", cxc))
  expect_identical(ap("rmst", .dg_rmst()), "inf_censoring")
  t1 <- list(purpose = "describe", by = "x", vars = c("y", "z"), layers = .dg_layers(), seed = 1L)
  expect_identical(ap("table1", t1), character())
  firth <- .dg_an("logistic", .dg_mod(lm1, penalty = "firth", marginal = FALSE))
  for (ck in c("separation", "influence", "linearity")) {
    expect_error(sr_diag_compile(ck, firth), "does not apply.*Firth.*not the analysis model")
  }
  expect_error(sr_diag_compile("ph", .dg_an("lm", lm1)), "does not apply")
  expect_error(sr_diag_compile("nope", .dg_an("lm", lm1)), "unknown diagnostic")
})

test_that("the verdict meets each threshold with exactly its comparator, and NA is a warning", {
  v <- .sr_h_diag_verdict
  # gt: a value equal to a threshold does not exceed it
  expect_identical(v(0.5, 0.5, 1, "gt"), "ok")
  expect_identical(v(1, 0.5, 1, "gt"), "warn")
  expect_identical(v(1 + 1e-12, 0.5, 1, "gt"), "fail")
  # ge: a value equal to a threshold meets it
  expect_identical(v(0.5, 0.5, 1, "ge"), "warn")
  expect_identical(v(1, 0.5, 1, "ge"), "fail")
  expect_identical(v(0.5 - 1e-12, 0.5, 1, "ge"), "ok")
  # lt: a value equal to a threshold is not below it
  expect_identical(v(50, 50, 30, "lt"), "ok")
  expect_identical(v(30, 50, 30, "lt"), "warn")
  expect_identical(v(29, 50, 30, "lt"), "fail")
  # no warn threshold (separation): only the fail threshold counts
  expect_identical(v(0, NA_real_, 1, "ge"), "ok")
  expect_identical(v(1, NA_real_, 1, "ge"), "fail")
  # a value that could not be computed
  for (cmp in c("gt", "ge", "lt")) expect_identical(v(NA_real_, 0.5, 1, cmp), "warn")
})

# ---- proportional hazards -----------------------------------------------------------------------

test_that("ph fails for a hazard ratio of 3 before t = 1 and 0.8 after, and passes for a constant 2", {
  d <- .dg_surv(2000, 3, 0.8, 1)
  r <- .dg_eval("ph", "cox", .dg_cox(), d)
  expect_identical(r$verdict, "fail")
  expect_gte(r$value, log(1.5))
  ev <- d$time[d$status == 1L]
  expect_equal(r$tau_suggest, unname(stats::quantile(ev, 0.75)))
  expect_true(is.data.frame(r$reported))
  expect_true(all(c("var", "term", "chisq", "df", "p") %in% names(r$reported)))
  expect_identical(r$reported$var[1L], "x")
  expect_true(all(c("term", "time", "resid") %in% names(r$plot_data)))
  expect_identical(unique(r$plot_data$term), "x")
  expect_identical(nrow(r$plot_data), sum(d$status))
  ok <- .dg_eval("ph", "cox", .dg_cox(), .dg_surv(2000, 2, 2, 2))
  expect_identical(ok$verdict, "ok")
  expect_lt(ok$value, log(1.25))
  # the describe twin gets the same check
  expect_identical(.dg_eval("ph", "cox_describe", .dg_cox(purpose = "describe"), d)$value, r$value)
})

test_that("ph lets the effect size decide: n = 200000, HR 1.02 early vs 1.00 late is ok though cox.zph rejects", {
  d <- .dg_surv(200000, 1.02, 1.00, 5)
  r <- .dg_eval("ph", "cox", .dg_cox(), d)
  expect_lt(r$reported$p[r$reported$var == "x"], 0.05)
  expect_lt(r$value, log(1.25))
  expect_identical(r$verdict, "ok")
  expect_identical(nrow(r$plot_data), 5000L)
  expect_match(r$detail, "5000")
})

test_that("ph with a multi-level factor exposure takes the largest difference over its levels", {
  d1 <- .dg_surv(2000, 3, 0.8, 21)
  d1$g <- ifelse(d1$x == 1L, "c", "a")
  d2 <- .dg_surv(2000, 1.5, 1.5, 22)
  d2 <- d2[d2$x == 1L, ]
  d2$g <- "b"
  d <- rbind(d1, d2)
  s <- .dg_cox(exposure = "g", exposure_ref = "a")
  r <- .dg_eval("ph", "cox", s, d)
  expect_identical(r$verdict, "fail")
  expect_setequal(unique(r$plot_data$term), c("g: b", "g: c"))
  dd <- d
  dd$gb <- as.integer(dd$g == "b")
  dd$gc <- as.integer(dd$g == "c")
  tm <- stats::median(dd$time[dd$status == 1L])
  de <- dd
  de$status[de$time > tm] <- 0L
  de$time <- pmin(de$time, tm)
  fe <- survival::coxph(survival::Surv(time, status) ~ gb + gc, data = de)
  dl <- dd[dd$time > tm, ]
  dl$t0 <- tm
  fl <- survival::coxph(survival::Surv(t0, time, status) ~ gb + gc, data = dl)
  expect_equal(r$value, max(abs(stats::coef(fe) - stats::coef(fl))), tolerance = 1e-8)
})

# ---- influence ----------------------------------------------------------------------------------

test_that("influence fails for a planted point with Cook's D > 1 and names the row", {
  set.seed(31)
  d <- data.frame(x = rnorm(50))
  d$y <- d$x + rnorm(50)
  d$x[1L] <- 8
  d$y[1L] <- -10
  r <- .dg_eval("influence", "lm", .dg_lm(), d)
  expect_identical(r$verdict, "fail")
  fit <- stats::lm(y ~ x, data = d)
  expect_equal(r$value, max(stats::cooks.distance(fit)), tolerance = 1e-10)
  expect_identical(r$rows, 1L)
  expect_true(all(c("row", "leverage", "cooks") %in% names(r$plot_data)))
  clean <- .dg_eval("influence", "lm", .dg_lm(), d[-1L, ])
  expect_identical(clean$verdict, "ok")
  expect_identical(clean$rows, integer())
  # rows are positions in sr_data, also after rows with a missing value are dropped
  dm <- rbind(data.frame(x = NA_real_, y = 0), d)
  expect_identical(.dg_eval("influence", "lm", .dg_lm(), dm)$rows, 2L)
})

test_that("influence works for logistic and modpois, and for cox on the scaled dfbeta of the exposure", {
  set.seed(32)
  d <- data.frame(x = rnorm(300))
  d$y <- rbinom(300, 1, stats::plogis(0.5 * d$x))
  r <- .dg_eval("influence", "logistic", .dg_lm(), d)
  fit <- stats::glm(y ~ x, data = d, family = stats::binomial())
  expect_equal(r$value, max(stats::cooks.distance(fit)), tolerance = 1e-10)
  expect_identical(r$verdict, "ok")
  rp <- .dg_eval("influence", "modpois", .dg_lm(), d)
  fp <- suppressWarnings(stats::glm(y ~ x, data = d, family = stats::poisson(link = "log")))
  expect_equal(rp$value, max(stats::cooks.distance(fp)), tolerance = 1e-10)
  ds <- .dg_surv(300, 2, 2, 33)
  rc <- .dg_eval("influence", "cox", .dg_cox(), ds)
  fc <- survival::coxph(survival::Surv(time, status) ~ x, data = ds)
  expect_equal(rc$value, max(abs(stats::residuals(fc, type = "dfbetas"))), tolerance = 1e-10)
  expect_identical(rc$verdict, if (rc$value >= 1) "fail" else if (rc$value >= 0.5) "warn" else "ok")
  expect_true(all(c("row", "dfbeta") %in% names(rc$plot_data)))
})

# ---- separation ---------------------------------------------------------------------------------

test_that("separation fails for a perfectly separating covariate and passes for overlapping data", {
  set.seed(41)
  d <- data.frame(x = rnorm(200))
  d$y <- as.integer(d$x > 0)
  r <- .dg_eval("separation", "logistic", .dg_lm(), d)
  expect_identical(r$verdict, "fail")
  expect_identical(r$value, 1)
  expect_true(all(c("term", "beta", "beta_refit", "growth") %in% names(r$plot_data)))
  d$y <- rbinom(200, 1, stats::plogis(0.5 * d$x))
  ok <- .dg_eval("separation", "logistic", .dg_lm(), d)
  expect_identical(ok$verdict, "ok")
  expect_identical(ok$value, 0)
})

# ---- few clusters -------------------------------------------------------------------------------

test_that("few_clusters fails with 12 clusters and passes with 60", {
  mk <- function(G, seed) {
    set.seed(seed)
    g <- rep(seq_len(G), length.out = 600)
    x <- rnorm(600)
    data.frame(y = x + rnorm(G)[g] + rnorm(600), x = x, g = g)
  }
  s <- .dg_lm(se = "cluster", cluster = "g", layers = .dg_layers(clustered = TRUE))
  r <- .dg_eval("few_clusters", "lm", s, mk(12, 51))
  expect_identical(r$verdict, "fail")
  expect_identical(r$value, 12)
  expect_identical(nrow(r$plot_data), 12L)
  expect_false("g" %in% names(r$plot_data))
  ok <- .dg_eval("few_clusters", "lm", s, mk(60, 52))
  expect_identical(ok$verdict, "ok")
  expect_identical(ok$value, 60)
  mid <- .dg_eval("few_clusters", "lm", s, mk(40, 53))
  expect_identical(mid$verdict, "warn")
})

# ---- heteroscedasticity -------------------------------------------------------------------------

test_that("heteroscedasticity fails for an error sd growing with x^2 and passes when homoscedastic", {
  # x normal, so the error variance is large where the leverage is (with a uniform x the HC3 / model
  # ratio cannot exceed sqrt(3))
  set.seed(61)
  d <- data.frame(x = rnorm(1000))
  d$y <- 1 + 0.5 * d$x + rnorm(1000, sd = 0.2 + d$x^2)
  r <- .dg_eval("heteroscedasticity", "lm", .dg_lm(), d)
  fit <- stats::lm(y ~ x, data = d)
  want <- sqrt(diag(sandwich::vcovHC(fit, type = "HC3"))[["x"]]) / sqrt(diag(stats::vcov(fit))[["x"]])
  expect_equal(r$value, want, tolerance = 1e-10)
  expect_identical(r$verdict, "fail")
  expect_true(all(c("fitted", "resid") %in% names(r$plot_data)))
  ok <- .dg_eval("heteroscedasticity", "lm", .dg_lm(), .dg_lin(1000, 62))
  expect_identical(ok$verdict, "ok")
})

# ---- linearity ----------------------------------------------------------------------------------

test_that("linearity fails for a quadratic truth fitted linearly and names the term", {
  s <- .dg_lm(terms = list(list(var = "z")))
  d <- .dg_lin(1000, 71, f = function(x) 2 * (x - 1)^2)
  r <- .dg_eval("linearity", "lm", s, d)
  expect_identical(r$verdict, "fail")
  expect_identical(r$var, "x")
  expect_match(r$detail, "x")
  expect_true(all(c("var", "x", "linear", "rcs") %in% names(r$plot_data)))
  ok <- .dg_eval("linearity", "lm", s, .dg_lin(4000, 72))
  expect_identical(ok$verdict, "ok")
  # a 0/1 exposure is not a continuous term; with nothing to scan the check is ok
  db <- .dg_surv(500, 2, 2, 73)
  rb <- .dg_eval("linearity", "cox", .dg_cox(), db)
  expect_identical(rb$verdict, "ok")
  expect_match(rb$detail, "no continuous")
  # cox, on the linear-predictor scale
  set.seed(74)
  dc <- data.frame(z = runif(1500, 0, 2), x = rbinom(1500, 1, 0.5))
  dc$time <- rexp(1500, 0.3 * exp(0.4 * dc$x + 2 * (dc$z - 1)^2))
  dc$status <- 1L
  rc <- .dg_eval("linearity", "cox", .dg_cox(terms = list(list(var = "z"))), dc)
  expect_identical(rc$verdict, "fail")
  expect_identical(rc$var, "z")
})

test_that("linearity says that terms in an interaction are not scanned", {
  s <- .dg_lm(terms = list(list(var = "z"), list(var = "w")), interactions = list(c("x", "z")))
  d <- .dg_lin(1000, 75)
  d$w <- rnorm(1000)
  r <- .dg_eval("linearity", "lm", s, d)
  expect_match(r$detail, "interaction terms are not scanned")
  expect_match(r$detail, "1 term(s) scanned", fixed = TRUE)
  expect_identical(unique(r$plot_data$var), "w")
  # also when nothing is left to scan
  rb <- .dg_eval("linearity", "cox", .dg_cox(), .dg_surv(500, 2, 2, 73))
  expect_match(rb$detail, "interaction terms are not scanned")
})

test_that("linearity warns and names the terms whose spline cannot be computed", {
  # x has 11 distinct values but 990 of 1000 tied at 0: four distinct knots cannot be placed
  set.seed(76)
  tied <- sample(c(rep(0, 990), 1:10))
  # every candidate skipped: the value is NA and the verdict a warning, not a silent ok
  d <- data.frame(x = tied, y = rnorm(1000))
  r <- .dg_eval("linearity", "lm", .dg_lm(), d)
  expect_true(is.na(r$value))
  expect_identical(r$verdict, "warn")
  expect_match(r$detail, "could not be computed")
  expect_match(r$detail, "x")
  expect_false(grepl("no continuous", r$detail))
  expect_true(is.na(r$var))
  # mixed: z is scanned (a linear truth, so ok by value) but the skipped x forces at least warn
  d$z <- rnorm(1000)
  d$y <- 1 + 0.3 * d$z + rnorm(1000)
  m <- .dg_eval("linearity", "lm", .dg_lm(terms = list(list(var = "z"))), d)
  expect_true(is.finite(m$value))
  expect_lt(m$value, 0.1)
  expect_identical(m$verdict, "warn")
  expect_identical(m$var, "z")
  expect_match(m$detail, "could not be computed for x")
  expect_identical(unique(m$plot_data$var), "z")
  # a failing computed term stays a fail
  d$y <- 1 + 2 * d$z^2 + rnorm(1000)
  f <- .dg_eval("linearity", "lm", .dg_lm(terms = list(list(var = "z"))), d)
  expect_identical(f$verdict, "fail")
  expect_match(f$detail, "could not be computed for x")
})

# ---- informative censoring ----------------------------------------------------------------------

# Event times with hazard 0.3 exp(bx x + bz z) (x binary, z standard normal); censoring times with
# hazard 0.2 * cz^z (independent of the event time given x and z), administrative censoring at 6.
.dg_cens <- function(n, seed, bx, bz, cz = 1) {
  set.seed(seed)
  x <- rbinom(n, 1, 0.5)
  z <- rnorm(n)
  t <- rexp(n, 0.3 * exp(bx * x + bz * z))
  c <- pmin(rexp(n, 0.2 * cz^z), 6)
  data.frame(time = pmin(t, c), status = as.integer(t <= c), x = x, z = z)
}

test_that("inf_censoring is ok for independent censoring however strong the event effect", {
  s <- .dg_cox(terms = list(list(var = "z")))
  # event HR 4 for x and 3 per SD of z; censoring independent of both
  r <- .dg_eval("inf_censoring", "cox", s, .dg_cens(4000, 81, log(4), log(3)))
  expect_identical(r$verdict, "ok")
  expect_lt(r$value, log(1.25))
  # the reported table is the censoring model's coefficients, one row per baseline term
  expect_true(is.data.frame(r$reported))
  expect_true(all(c("term", "scale", "log_hr", "se", "hr") %in% names(r$reported)))
  expect_identical(r$reported$term, c("x", "z"))
  expect_equal(r$value, max(abs(r$reported$log_hr)))
  expect_match(r$reported$scale[2L], "per SD")
  expect_true(all(c("term", "log_hr", "lower", "upper") %in% names(r$plot_data)))
  expect_match(r$detail, "Cox model for censoring")
  expect_match(r$detail, "covariate-dependent censoring")
  expect_match(r$detail, "cannot be tested")
  # only administrative censoring at a fixed time, with an event HR of 4: ok
  a <- .dg_eval("inf_censoring", "cox", .dg_cox(), .dg_surv(2000, 4, 4, 84))
  expect_identical(a$verdict, "ok")
  # no censored row at all: nothing to screen
  dn <- .dg_cens(500, 85, log(2), 0)
  dn$status <- 1L
  n0 <- .dg_eval("inf_censoring", "cox", s, dn)
  expect_identical(n0$verdict, "ok")
  expect_identical(n0$value, 0)
  expect_match(n0$detail, "no censored row")
})

test_that("inf_censoring fails when the censoring hazard depends on a baseline term", {
  s <- .dg_cox(terms = list(list(var = "z")))
  # the censoring hazard doubles per SD of z; no event effect at all
  d <- .dg_cens(4000, 86, 0, 0, cz = 2)
  r <- .dg_eval("inf_censoring", "cox", s, d)
  expect_identical(r$verdict, "fail")
  expect_gt(r$value, log(1.5))
  zr <- r$reported[r$reported$term == "z", ]
  # per SD of the observed z (about 1)
  want <- unname(stats::coef(survival::coxph(survival::Surv(time, 1L - status) ~ x + I(z / stats::sd(z)), data = d))[2L])
  expect_equal(zr$log_hr, want, tolerance = 1e-6)
  expect_equal(zr$log_hr, log(2), tolerance = 0.15)
  expect_match(r$detail, "worst term z")
  # a factor exposure is screened per coefficient
  d$g <- ifelse(d$x == 1L, "b", "a")
  rf <- .dg_eval("inf_censoring", "cox", .dg_cox(exposure = "g", exposure_ref = "a", terms = list(list(var = "z"))), d)
  expect_identical(rf$reported$term, c("g: b", "z"))
  expect_identical(rf$verdict, "fail")
  # rmst: the exposure is the only baseline term
  set.seed(87)
  n <- 2000
  x <- rbinom(n, 1, 0.5)
  t <- rexp(n, 0.5)
  c <- pmin(rexp(n, ifelse(x == 1L, 0.6, 0.2)), 5)
  dr <- data.frame(time = pmin(t, c), status = as.integer(t <= c), x = x)
  rr <- .dg_eval("inf_censoring", "rmst", .dg_rmst(tau = 0.9), dr)
  expect_identical(rr$verdict, "fail")
  expect_identical(rr$reported$term, "x: 1")
  expect_equal(rr$reported$log_hr, log(3), tolerance = 0.2)
  ri <- .dg_eval("inf_censoring", "rmst", .dg_rmst(tau = 0.9, exposure_ref = "1"), dr)
  expect_identical(ri$reported$term, "x: 0")
  expect_match(ri$detail, "only baseline term")
})

# ---- layers and the plot cap --------------------------------------------------------------------

test_that("diagnostics ignore the bootstrap and run on complete cases under MI, saying so", {
  set.seed(91)
  d <- data.frame(x = rnorm(200), z = rnorm(200))
  d$y <- d$x + rnorm(200)
  d$z[sample(200, 20)] <- NA
  s <- .dg_lm(terms = list(list(var = "z")))
  cc <- .dg_eval("influence", "lm", s, d)
  mi <- .dg_eval("influence", "lm", .dg_mod(s, layers = .dg_layers(missing = "mi"), mi = list(m = 3)), d)
  expect_identical(mi$value, cc$value)
  expect_match(mi$detail, "complete cases")
  bt <- .dg_eval("influence", "lm", .dg_mod(s, layers = .dg_layers(resampling = "bootstrap"), boot = list(B = 50)), d)
  expect_identical(bt$value, cc$value)
  expect_match(bt$detail, "bootstrap")
})

test_that("plot data are capped at 5000 points by a seeded sample that keeps the flagged rows", {
  set.seed(92)
  d <- data.frame(x = rnorm(6000))
  d$y <- d$x + rnorm(6000)
  d$x[6000L] <- 60
  d$y[6000L] <- -80
  r <- .dg_eval("influence", "lm", .dg_lm(), d)
  expect_identical(nrow(r$plot_data), 5000L)
  expect_true(6000L %in% r$plot_data$row)
  expect_match(r$detail, "5000")
  expect_identical(.dg_eval("influence", "lm", .dg_lm(), d, seed = 3L)$plot_data,
                   .dg_eval("influence", "lm", .dg_lm(), d, seed = 3L)$plot_data)
})

# ---- emitted code -------------------------------------------------------------------------------

test_that("every emitted diagnostic passes the diagnostic emit check and is not analysis code", {
  .dg_routes()
  bt <- intToUtf8(96)
  hostile <- paste0("v", bt, "); system(", intToUtf8(39), "calc", intToUtf8(39), "); (")
  specs <- list(
    list("lm", .dg_lm(terms = list(list(var = "z"), list(var = "w", type = "rcs"), list(var = "f", type = "factor")))),
    list("lm", .dg_lm(exposure = hostile, terms = list(list(var = "z")), se = "cluster", cluster = "g",
                      layers = .dg_layers(clustered = TRUE))),
    list("lm_describe", .dg_lm(purpose = "describe", terms = list(list(var = "z")), interactions = list(c("x", "z")))),
    list("lm", .dg_lm(layers = .dg_layers(missing = "mi"), mi = list(m = 3))),
    list("lm", .dg_lm(layers = .dg_layers(resampling = "bootstrap"), boot = list(B = 50))),
    list("logistic", .dg_lm(exposure_ref = "a", terms = list(list(var = "z")))),
    list("logistic", .dg_lm(penalty = "firth", marginal = FALSE, terms = list(list(var = "z")))),
    list("logistic_describe", .dg_lm(purpose = "describe", terms = list(list(var = "z", type = "quad")))),
    list("modpois", .dg_lm(se = "cluster", cluster = "g", layers = .dg_layers(clustered = TRUE))),
    list("cox", .dg_cox(terms = list(list(var = "z")), strata = "s", cluster = "g",
                        layers = .dg_layers(clustered = TRUE), ties = "breslow")),
    list("cox", .dg_cox(exposure = hostile, exposure_ref = "a", terms = list(list(var = "z")))),
    list("cox_describe", .dg_cox(purpose = "describe", terms = list(list(var = "z")))),
    list("rmst", .dg_rmst(exposure_ref = "1")),
    list("rmst", .dg_rmst(layers = .dg_layers(missing = "mi"), mi = list(m = 3)))
  )
  n <- 0L
  for (sp in specs) {
    for (ck in sr_diag_applicable(.dg_an(sp[[1L]], sp[[2L]]))) {
      code <- sr_diag_compile(ck, .dg_an(sp[[1L]], sp[[2L]]))
      expect_silent(.sr_emit_check(code, "diagnostic"))
      expect_false("system" %in% all.names(parse(text = code)))
      n <- n + 1L
    }
  }
  expect_gte(n, 25L)
  ph <- sr_diag_compile("ph", .dg_an("cox", .dg_cox()))
  expect_error(.sr_emit_check(ph, "analysis"), "cox.zph")
})

test_that("the diagnostic taint tracking keeps its place across an assignment of NULL", {
  expect_silent(.sr_emit_check("x <- NULL\ny <- 1\nif (y > 0) x <- 2", "diagnostic"))
  code <- "a <- NULL\nz <- survival::cox.zph(f)\nw <- z\nif (w$table[1L, 3L] < 0.05) 1"
  expect_error(.sr_emit_check(code, "diagnostic"), "depends on a diagnostic")
})

test_that("no emitted analysis body of the model routes or Table 1 contains a diagnostic call", {
  .dg_routes()
  bodies <- list(
    list("lm", .dg_lm(terms = list(list(var = "z")))),
    list("lm", .dg_lm(layers = .dg_layers(missing = "mi"), mi = list(m = 3))),
    list("lm", .dg_lm(layers = .dg_layers(resampling = "bootstrap"), boot = list(B = 50))),
    list("lm_describe", .dg_lm(purpose = "describe")),
    list("logistic", .dg_lm(terms = list(list(var = "z")))),
    list("logistic", .dg_lm(penalty = "firth", marginal = FALSE)),
    list("logistic", .dg_lm(layers = .dg_layers(missing = "mi"), mi = list(m = 3))),
    list("logistic_describe", .dg_lm(purpose = "describe")),
    list("modpois", .dg_lm(layers = .dg_layers(resampling = "bootstrap"), boot = list(B = 50))),
    list("cox", .dg_cox(terms = list(list(var = "z")))),
    list("cox", .dg_cox(layers = .dg_layers(missing = "mi"), mi = list(m = 3))),
    list("cox", .dg_cox(layers = .dg_layers(resampling = "bootstrap"), boot = list(B = 50))),
    list("cox_describe", .dg_cox(purpose = "describe")),
    list("rmst", .dg_rmst()),
    list("rmst", .dg_rmst(layers = .dg_layers(missing = "mi"), mi = list(m = 3))),
    list("rmst", .dg_rmst(layers = .dg_layers(resampling = "bootstrap"), boot = list(B = 50))),
    list("table1", list(purpose = "describe", by = "x", vars = c("y", "z"), layers = .dg_layers(), seed = 1L))
  )
  for (b in bodies) {
    code <- sr_compile(sr_route_get(b[[1L]], 1L), b[[2L]])
    expect_silent(.sr_emit_check(code, "analysis"))
    nm <- all.names(parse(text = code))
    expect_false(any(nm %in% .sr_lint_diag_calls), info = b[[1L]])
    expect_false(grepl("cox.zph|dfbetas|scaledsch", code), info = b[[1L]])
  }
})

# ---- sr_diag_run ----------------------------------------------------------------------------------

test_that("sr_diag_run records diag.completed per check, cached, replayable, and adds no analysis", {
  .dg_routes()
  d <- .dg_surv(400, 3, 0.8, 101)
  set.seed(102)
  d$z <- round(rnorm(400), 3)
  d$id <- seq_len(nrow(d))
  p <- .fx_ready_project(d, "t", c(id = "id", status = "outcome"))
  s <- .dg_cox(terms = list(list(var = "z")))
  p <- sr_sap_add(p, "m1", "cox", s, "t", label = "primary")
  expect_error(sr_diag_run(p, "m1"), "not locked")
  p <- sr_sap_lock(p)
  expect_error(sr_diag_run(p, "m1"), "has not run successfully")
  expect_error(sr_diag_run(p, "nope"), "unknown analysis")
  p <- sr_run(p, "m1")
  expect_true(p$state$runs$m1$ok, info = p$state$runs$m1$error)
  expect_error(sr_diag_run(p, "m1", checks = "heteroscedasticity"), "does not apply")
  an <- names(p$state$analyses)
  p <- sr_diag_run(p, "m1")
  dg <- p$state$diag$m1
  expect_setequal(names(dg), c("ph", "influence", "inf_censoring", "linearity"))
  expect_identical(dg$ph$verdict, "fail")
  expect_true(all(vapply(dg, function(x) x$verdict %in% c("ok", "warn", "fail"), logical(1))))
  expect_equal(dg$ph$warn, log(1.25))
  expect_equal(dg$ph$fail, log(1.5))
  expect_true(is.numeric(dg$ph$tau_suggest) && dg$ph$tau_suggest > 0)
  expect_true(is.integer(dg$influence$rows))
  expect_identical(names(p$state$analyses), an)
  for (k in names(dg)) {
    expect_true(file.exists(file.path(p$paths$fits, paste0(dg[[k]]$key, ".rds"))))
    expect_false(any(grepl("^p[._]?val|^pvalue$", names(dg[[k]]))))
  }
  # live fold == replay
  expect_identical(sr_state_replay(sr_events_read(p$paths$events))$diag, p$state$diag)
  # a rerun of one check hits the cache and gives the same entry
  n0 <- length(sr_events_read(p$paths$events))
  q <- sr_diag_run(p, "m1", checks = "ph")
  ev <- sr_events_read(q$paths$events)
  expect_identical(length(ev), n0 + 1L)
  expect_identical(q$state$diag$m1$ph, dg$ph)
  last <- ev[[length(ev)]]
  expect_identical(last$type, "diag.completed")
  expect_true(all(c("analysis", "check", "verdict", "value", "warn", "fail", "detail", "key") %in%
                  names(last$payload)))
  expect_false(grepl(p$paths$dir, paste(readLines(p$paths$events, warn = FALSE), collapse = "\n"), fixed = TRUE))
  ro <- q
  ro$readonly <- TRUE
  expect_error(sr_diag_run(ro, "m1"), "read-only")
})
