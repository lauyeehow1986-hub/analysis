# Tests for the memory piecewise-exponential Cox (cox_method = "pem", P1e.2 Task 8, ruling S16/S17/S18). Public
# package data (survival::lung) and a simulated Weibull sample; helpers come from test-cox.R's conventions.

.pm_layers <- function(clustered = FALSE) {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = clustered)
}

.pm_spec <- function(...) {
  s <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 2, exposure = "sex",
            cox_method = "pem", layers = .pm_layers(), seed = 7L)
  mod <- list(...)
  for (nm in names(mod)) s[nm] <- list(mod[[nm]])
  s
}

.pm_route <- function() {
  if (!(.sr_route_key("cox", 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(.sr_cox_route())
  sr_route_get("cox", 1L)
}

.pm_value <- function(df, spec) {
  code <- sr_compile(.pm_route(), spec)
  expect_silent(.sr_emit_check(code, "analysis"))
  env <- new.env(parent = globalenv())
  env$sr_data <- df
  eval(parse(text = code), envir = env)
}

# the hand-built reference: survSplit + glm on the rows complete in the columns
.pm_ref <- function(df, cuts, cov, strata = NULL, cluster = NULL) {
  Surv <- survival::Surv
  df <- df[stats::complete.cases(df[c("time", "status", cov, strata, cluster)]), ]
  df$.time <- df$time
  df$.event <- as.integer(df$status == 2)
  sp <- survival::survSplit(Surv(.time, .event) ~ ., data = df, cut = cuts, episode = ".int")
  sp$.pt <- sp$.time - sp$tstart
  base <- if (is.null(strata)) "factor(.int)" else
    sprintf("droplevels(interaction(factor(.int), %s))", paste(strata, collapse = ", "))
  lhs <- if (length(cuts) == 0L && is.null(strata)) ".event ~" else paste(".event ~ 0 +", base, "+")
  fit <- stats::glm(stats::as.formula(paste(lhs, paste(cov, collapse = " + "), "+ offset(log(.pt))")),
                    data = sp, family = stats::poisson())
  V <- if (is.null(cluster)) stats::vcov(fit) else sandwich::vcovCL(fit, cluster = sp[[cluster]], type = "HC1")
  list(est = unname(stats::coef(fit)[cov]), se = unname(sqrt(diag(V))[cov]), n = nrow(df))
}

test_that("memory pem equals a hand-built survSplit + glm", {
  lg <- survival::lung
  spec <- .pm_spec(terms = list(list(var = "age")), strata = "ph.ecog", pem_cuts = list(rule = "explicit", at = c(100, 250, 500)))
  v <- .pm_value(lg, spec)
  ref <- .pm_ref(lg, c(100, 250, 500), c("sex", "age"), strata = "ph.ecog")
  expect_equal(v$coef$est, ref$est, tolerance = 1e-10)
  expect_equal(v$coef$se, ref$se, tolerance = 1e-10)
  expect_identical(v$n, ref$n)
  expect_identical(v$method, "pem")
  expect_identical(v$cuts, c(0, 100, 250, 500, Inf))
  expect_identical(v$k, 4L)
  expect_identical(v$aliased, character())
  expect_identical(names(v), c("coef", "aliased", "n", "n_total", "events", "level", "se_type", "n_clusters",
                               "ties", "strata", "levels", "warnings", "km_by", "km", "km_risk", "method", "cuts", "k"))
  # no strata
  v0 <- .pm_value(lg, .pm_spec(pem_cuts = list(rule = "explicit", at = c(100, 250, 500))))
  r0 <- .pm_ref(lg, c(100, 250, 500), "sex")
  expect_equal(v0$coef$est, r0$est, tolerance = 1e-10)
  expect_equal(v0$coef$se, r0$se, tolerance = 1e-10)
})

test_that("quantile cuts and explicit cuts, explicit cut >= max time dropped", {
  lg <- survival::lung
  v <- .pm_value(lg, .pm_spec(pem_cuts = list(rule = "quantile", k = 5)))
  ev <- lg$time[lg$status == 2]
  expect_identical(v$cuts, c(0, unique(stats::quantile(ev, probs = (1:4) / 5, type = 7, names = FALSE)), Inf))
  expect_identical(v$k, length(v$cuts) - 1L)
  vd <- .pm_value(lg, .pm_spec())   # default: ten quantile intervals
  expect_identical(vd$k, length(vd$cuts) - 1L)
  expect_identical(vd$cuts[c(1L, length(vd$cuts))], c(0, Inf))
  mx <- max(lg$time)
  ve <- .pm_value(lg, .pm_spec(pem_cuts = list(rule = "explicit", at = c(200, mx, mx + 50))))
  expect_identical(ve$cuts, c(0, 200, Inf))
  expect_identical(ve$k, 2L)
  # no usable cut at all: one interval, the plain Poisson model
  v1 <- .pm_value(lg, .pm_spec(pem_cuts = list(rule = "explicit", at = mx + 1)))
  expect_identical(v1$k, 1L)
  r1 <- .pm_ref(lg, numeric(0), "sex")
  expect_equal(v1$coef$est, r1$est, tolerance = 1e-10)
})

test_that("pem cluster SE equals sandwich::vcovCL HC1", {
  lg <- survival::lung
  lg <- lg[!is.na(lg$inst), ]
  spec <- .pm_spec(terms = list(list(var = "age")), cluster = "inst", layers = .pm_layers(TRUE),
                   pem_cuts = list(rule = "explicit", at = c(150, 300)))
  v <- .pm_value(lg, spec)
  ref <- .pm_ref(lg, c(150, 300), c("sex", "age"), cluster = "inst")
  expect_equal(v$coef$est, ref$est, tolerance = 1e-10)
  expect_equal(v$coef$se, ref$se, tolerance = 1e-10)
  expect_identical(v$se_type, "cluster")
  expect_equal(v$n_clusters, length(unique(lg$inst)))
})

test_that("time <= 0 stops", {
  lg <- survival::lung
  lg$time[c(3, 9)] <- 0
  expect_error(.pm_value(lg, .pm_spec()),
               "the piecewise-exponential model needs every time above zero (found 2 rows with time <= 0)",
               fixed = TRUE)
})

test_that("Weibull sanity: PEM K=10 log HR within 0.02 of coxph", {
  set.seed(11)
  n <- 20000
  x <- stats::rbinom(n, 1, 0.5)
  tt <- (stats::rexp(n) / exp(0.5 * x))^(1 / 1.5)   # Weibull shape 1.5, log HR 0.5 (scale 1)
  cens <- stats::runif(n, 0.3, 2.5)
  d <- data.frame(time = pmin(tt, cens), status = ifelse(tt <= cens, 2, 1), sex = x)
  v <- .pm_value(d, .pm_spec(pem_cuts = list(rule = "quantile", k = 10)))
  cx <- unname(stats::coef(survival::coxph(survival::Surv(time, status == 2) ~ sex, data = d)))
  expect_lt(abs(v$coef$est[1L] - cx), 0.02)
  expect_lt(abs(v$coef$est[1L] - 0.5), 0.1)
})

test_that("pem fragment names K and the cuts", {
  lg <- survival::lung
  spec <- .pm_spec(terms = list(list(var = "age")), pem_cuts = list(rule = "explicit", at = c(100, 250.5)))
  v <- .pm_value(lg, spec)
  r <- .pm_route()
  p <- sr_result_check(r$present(v, spec), r)
  expect_identical(p$fragments$pem_method,
                   paste0("Hazard ratios were estimated with a piecewise-exponential (Poisson) model with 3 ",
                          "intervals of constant baseline hazard (cut points at 100, 250.5); this approximates the Cox model."))
  expect_match(p$fragments$cox_note, "piecewise-exponential Poisson regression", fixed = TRUE)
  expect_false(grepl("Cox proportional hazards regression", p$fragments$cox_note, fixed = TRUE))
  expect_true(all(p$effects$measure == "HR"))
  expect_identical(sum(p$effects$role == "primary"), 1L)
  # exact Cox keeps its wording
  ve <- .pm_value(lg, .pm_spec(cox_method = "exact"))
  pe <- sr_result_check(r$present(ve, .pm_spec(cox_method = "exact")), r)
  expect_match(pe$fragments$cox_note, "Cox proportional hazards regression", fixed = TRUE)
  expect_null(pe$fragments$pem_method)
})

test_that("two strata columns form one baseline per combination and interval", {
  lg <- survival::lung
  spec <- .pm_spec(strata = c("ph.ecog", "inst"), pem_cuts = list(rule = "explicit", at = c(150, 300)))
  v <- .pm_value(lg, spec)
  ref <- .pm_ref(lg, c(150, 300), "sex", strata = c("ph.ecog", "inst"))
  expect_equal(v$coef$est, ref$est, tolerance = 1e-10)
  expect_equal(v$coef$se, ref$se, tolerance = 1e-10)
})
