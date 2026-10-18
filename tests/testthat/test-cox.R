# Tests for routes/cox.R: cox@1 (with its describe twin) and the Kaplan-Meier figure data. Public package data
# only (survival::lung, survival::gbsg) and synthetic ids; projects live under withr::local_tempdir().

.cx_layers <- function(clustered = FALSE) {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = clustered)
}

# lung: time, status (1 = censored, 2 = dead), sex (1/2), ph.ecog, inst, age
.cx_spec <- function(purpose = "explain-causal", ...) {
  s <- list(purpose = purpose, time = "time", status = "status", status_event = 2, exposure = "sex",
            layers = .cx_layers(), seed = 7L)
  mod <- list(...)
  for (nm in names(mod)) s[nm] <- list(mod[[nm]])
  s
}

# The routes, registered again when an earlier test file emptied the registry.
.cx_route <- function(id = "cox") {
  reg <- function(key, make) {
    if (!(.sr_route_key(key, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make())
  }
  reg("cox", .sr_cox_route)
  reg("cox_describe", .sr_cox_describe_route)
  sr_route_get(id, 1L)
}

.cx_value <- function(df, spec = .cx_spec(), id = "cox") {
  route <- .cx_route(id)
  code <- sr_compile(route, spec)
  expect_silent(.sr_emit_check(code, "analysis"))
  env <- new.env(parent = globalenv())
  env$sr_data <- df
  eval(parse(text = code), envir = env)
}

.cx_present <- function(value, spec = .cx_spec(), id = "cox") {
  r <- .cx_route(id)
  sr_result_check(r$present(value, spec), r)
}

.cx_hr <- function(e, pat = "") e[e$measure == "HR" & grepl(pat, e$term, fixed = TRUE), , drop = FALSE]

# ---- registration and validation ----------------------------------------------------------------

test_that("cox and cox_describe are registered with the documented schema", {
  r <- .cx_route("cox")
  d <- .cx_route("cox_describe")
  expect_identical(r$family, "survival")
  expect_identical(r$purpose, "explain-causal")
  expect_identical(d$purpose, "describe")
  want <- c("time", "status", "status_event", "exposure", "terms", "interactions", "strata", "cluster",
            "ties", "layers", "seed")
  expect_true(all(want %in% names(r$spec_schema)))
  expect_false("se" %in% names(r$spec_schema))
  expect_identical(r$spec_schema$ties$values, c("efron", "breslow"))
  expect_identical(r$spec_schema$strata$type, "chr")
  expect_false(isTRUE(r$spec_schema$strata$required))
  expect_true(isTRUE(r$spec_schema$time$required))
  expect_identical(d$spec_schema, r$spec_schema)
  expect_identical(r$compile, d$compile)
  expect_identical(r$present, d$present)
  expect_s3_class(r$estimand(.cx_spec()), "sr_estimand")
  expect_true(sr_spec_validate(.cx_spec(), r)$ok)
  expect_true(sr_spec_validate(.cx_spec(purpose = "describe"), d)$ok)
  expect_true(sr_spec_validate(.cx_spec(ties = "breslow", strata = c("ph.ecog", "inst")), r)$ok)
  expect_false(sr_spec_validate(.cx_spec(ties = "exact"), r)$ok)
})

test_that("cluster and layers$clustered go together; unsupported layers are refused at compile", {
  r <- .cx_route("cox")
  v <- sr_spec_validate(.cx_spec(cluster = "inst"), r)
  expect_false(v$ok)
  expect_match(paste(v$errors, collapse = " "), "clustered", fixed = TRUE)
  v2 <- sr_spec_validate(.cx_spec(layers = .cx_layers(TRUE)), r)
  expect_false(v2$ok)
  expect_match(paste(v2$errors, collapse = " "), "cluster", fixed = TRUE)
  expect_true(sr_spec_validate(.cx_spec(cluster = "inst", layers = .cx_layers(TRUE)), r)$ok)
  expect_false(sr_spec_validate(.cx_spec(strata = "sex"), r)$ok)
  expect_false(sr_spec_validate(.cx_spec(exposure = "time"), r)$ok)
  L <- .cx_layers()
  L$missing <- "mi"
  expect_error(sr_compile(r, .cx_spec(layers = L)), "does not implement")
  L <- .cx_layers()
  L$weights <- "ipw"
  expect_error(sr_compile(r, .cx_spec(layers = L)), "weights")
  L <- .cx_layers()
  L$resampling <- "bootstrap"   # implemented by resample/boot.R; its settings are validated
  expect_error(sr_compile(r, .cx_spec(layers = L, boot = list(B = 5))), "spec$boot$B", fixed = TRUE)
})

# ---- the hazard ratio ---------------------------------------------------------------------------

test_that("lung: HR for sex matches the printed values and a direct coxph to 1e-8", {
  d <- survival::lung
  val <- .cx_value(d)
  fit <- survival::coxph(survival::Surv(time, status) ~ sex, data = d)
  b <- unname(stats::coef(fit))
  se <- sqrt(unname(stats::vcov(fit)[1, 1]))
  expect_equal(val$coef$est, b, tolerance = 1e-8)
  expect_equal(val$coef$se, se, tolerance = 1e-8)
  expect_equal(round(b, 4), -0.5310)
  expect_equal(round(se, 4), 0.1672)
  res <- .cx_present(val)
  e <- .cx_hr(res$effects, "sex")
  expect_equal(nrow(e), 1L)
  expect_equal(e$est, exp(b), tolerance = 1e-8)
  expect_equal(round(c(e$est, e$lo, e$hi), 3), c(0.588, 0.424, 0.816))
  expect_equal(e$lo, exp(b - stats::qnorm(0.975) * se), tolerance = 1e-8)
  expect_identical(e$role, "primary")
  expect_identical(e$scale, "ratio")
  expect_false(e$conditional)
  expect_equal(e$n, 228)
  expect_equal(e$events, 165)
  expect_equal(e$p, 2 * stats::pnorm(-abs(b / se)), tolerance = 1e-8)
  expect_identical(nrow(res$flags[res$flags$flag == "non_collapsible", ]), 0L)
  expect_true(is.data.frame(res$tables$coefficients))
  expect_true(is.character(res$fragments$cox_note))
})

test_that("Breslow ties match a direct coxph", {
  d <- survival::lung
  s <- .cx_spec(ties = "breslow")
  val <- .cx_value(d, s)
  fit <- survival::coxph(survival::Surv(time, status) ~ sex, data = d, ties = "breslow")
  expect_equal(val$coef$est, unname(stats::coef(fit)), tolerance = 1e-8)
  expect_identical(val$ties, "breslow")
  efron <- unname(stats::coef(survival::coxph(survival::Surv(time, status) ~ sex, data = d)))
  expect_false(isTRUE(all.equal(val$coef$est, efron, tolerance = 1e-8)))
})

test_that("cluster() with layers$clustered gives the robust SE of a direct coxph", {
  d <- survival::lung
  s <- .cx_spec(cluster = "inst", layers = .cx_layers(TRUE))
  val <- .cx_value(d, s)
  dd <- d[!is.na(d$inst), ]
  fit <- survival::coxph(survival::Surv(time, status) ~ sex + cluster(inst), data = dd)
  expect_equal(val$coef$est, unname(stats::coef(fit)), tolerance = 1e-8)
  expect_equal(val$coef$se, sqrt(unname(stats::vcov(fit)[1, 1])), tolerance = 1e-8)
  expect_equal(val$n, nrow(dd))
  expect_equal(val$n_clusters, length(unique(dd$inst)))
  expect_identical(val$se_type, "cluster")
  res <- .cx_present(val, s)
  e <- .cx_hr(res$effects, "sex")
  expect_equal(e$lo, exp(unname(stats::coef(fit)) - stats::qnorm(0.975) * sqrt(unname(stats::vcov(fit)[1, 1]))),
               tolerance = 1e-8)
  expect_match(res$fragments$cox_note, "cluster", fixed = TRUE)
  # the cluster column needs two clusters
  d1 <- dd
  d1$inst <- 1
  expect_error(.cx_value(d1, s), "at least two clusters")
})

test_that("strata() gives the stratified HR of a direct coxph", {
  d <- survival::lung
  s <- .cx_spec(strata = "ph.ecog")
  val <- .cx_value(d, s)
  fit <- survival::coxph(survival::Surv(time, status) ~ sex + strata(ph.ecog), data = d)
  expect_equal(val$coef$est, unname(stats::coef(fit)), tolerance = 1e-8)
  expect_equal(val$coef$se, sqrt(unname(stats::vcov(fit)[1, 1])), tolerance = 1e-8)
  expect_equal(val$n, fit$n)
  expect_equal(val$events, fit$nevent)
  expect_match(.cx_present(val, s)$fragments$cox_note, "ph.ecog", fixed = TRUE)
  s2 <- .cx_spec(strata = c("ph.ecog", "inst"))
  val2 <- .cx_value(d, s2)
  fit2 <- survival::coxph(survival::Surv(time, status) ~ sex + strata(ph.ecog, inst), data = d)
  expect_equal(val2$coef$est, unname(stats::coef(fit2)), tolerance = 1e-8)
})

test_that("gbsg: every adjusted HR equals a direct coxph to 1e-8; adjusted rows are conditional", {
  g <- survival::gbsg
  s <- list(purpose = "explain-causal", time = "rfstime", status = "status", exposure = "hormon",
            terms = list(list(var = "age"), list(var = "meno"), list(var = "size"), list(var = "grade"),
                         list(var = "nodes"), list(var = "pgr"), list(var = "er")),
            layers = .cx_layers(), seed = 1L)
  val <- .cx_value(g, s)
  fit <- survival::coxph(survival::Surv(rfstime, status) ~ hormon + age + meno + size + grade + nodes + pgr + er,
                         data = g)
  res <- .cx_present(val, s)
  e <- .cx_hr(res$effects)
  expect_equal(nrow(e), 8L)
  expect_equal(e$term, c("hormon", "age", "meno", "size", "grade", "nodes", "pgr", "er"))
  expect_equal(e$est, unname(exp(stats::coef(fit))), tolerance = 1e-8)
  se <- sqrt(diag(stats::vcov(fit)))
  expect_equal(e$lo, unname(exp(stats::coef(fit) - stats::qnorm(0.975) * se)), tolerance = 1e-8)
  expect_equal(e$hi, unname(exp(stats::coef(fit) + stats::qnorm(0.975) * se)), tolerance = 1e-8)
  expect_identical(e$role, c("primary", rep("covariate", 7L)))
  expect_true(all(e$conditional))
  expect_true(all(e$n == 686) && all(e$events == sum(g$status)))
  expect_true(any(res$flags$flag == "non_collapsible"))
  expect_true(any(res$flags$flag == "table2"))
})

test_that("a factor exposure gives one HR per non-reference level, labelled against the reference", {
  d <- survival::lung
  d <- d[!is.na(d$ph.ecog), ]
  s <- .cx_spec(exposure = "ph.ecog", exposure_ref = "0")
  val <- .cx_value(d, s)
  fit <- survival::coxph(survival::Surv(time, status) ~ factor(ph.ecog), data = d)
  res <- .cx_present(val, s)
  e <- .cx_hr(res$effects)
  expect_equal(e$est, unname(exp(stats::coef(fit))), tolerance = 1e-8)
  expect_equal(e$term, c("ph.ecog: 1 vs 0", "ph.ecog: 2 vs 0", "ph.ecog: 3 vs 0"))
  expect_equal(e$level, c("1", "2", "3"))
  expect_true(all(e$role == "primary"))
  expect_false(any(e$conditional))
  # the KM figure data is by exposure level
  expect_setequal(unique(val$km$strata), c("0", "1", "2", "3"))
})

test_that("an interaction with the exposure marks the exposure row conditional and says so", {
  d <- survival::lung
  s <- .cx_spec(terms = list(list(var = "age")), interactions = list(c("sex", "age")))
  val <- .cx_value(d, s)
  fit <- survival::coxph(survival::Surv(time, status) ~ sex * age, data = d)
  expect_equal(sort(val$coef$est), sort(unname(stats::coef(fit))), tolerance = 1e-8)
  res <- .cx_present(val, s)
  expect_true(.cx_hr(res$effects, "sex")$conditional[1])
  expect_true("cox_interaction" %in% names(res$fragments))
})

test_that("no events and missing columns end with a plain message", {
  d <- survival::lung
  d$status <- 1
  expect_error(.cx_value(d), "no events")
  expect_error(.cx_value(survival::lung[c("time", "status")]), "column[(]s[)] not in the data")
})

# ---- Kaplan-Meier data --------------------------------------------------------------------------

test_that("KM figure data equals survfit, and the risk table equals summary(survfit) at pretty() times", {
  d <- survival::lung
  s <- .cx_spec()
  val <- .cx_value(d, s)
  res <- .cx_present(val, s)
  km <- res$figures$km
  expect_identical(names(km), c("strata", "time", "surv", "lower", "upper", "n.risk", "n.event", "n.censor"))
  fit <- survival::survfit(survival::Surv(time, status == 2) ~ sex, data = d)
  expect_equal(km$time, fit$time)
  expect_equal(km$surv, fit$surv, tolerance = 1e-10)
  expect_equal(km$lower, fit$lower, tolerance = 1e-10)
  expect_equal(km$upper, fit$upper, tolerance = 1e-10)
  expect_equal(km$n.risk, fit$n.risk)
  expect_equal(km$n.event, fit$n.event)
  expect_equal(km$n.censor, fit$n.censor)
  expect_identical(unique(km$strata), c("1", "2"))
  expect_equal(as.integer(table(km$strata)), as.integer(fit$strata))
  rk <- res$figures$km_risk
  expect_identical(names(rk), c("strata", "time", "n.risk"))
  tt <- sort(unique(rk$time))
  expect_gte(length(tt), 5L)
  expect_lte(length(tt), 7L)
  expect_identical(tt, c(0, 200, 400, 600, 800, 1000))
  sm <- summary(fit, times = tt, extend = TRUE)
  expect_equal(rk$n.risk, as.numeric(sm$n.risk))
  expect_equal(rk$time, as.numeric(sm$time))
  expect_identical(rk$strata, sub("sex=", "", as.character(sm$strata), fixed = TRUE))
})

test_that("a continuous exposure gives one overall KM curve and a note", {
  d <- survival::lung
  s <- .cx_spec(exposure = "age")
  val <- .cx_value(d, s)
  res <- .cx_present(val, s)
  expect_identical(unique(res$figures$km$strata), "All")
  fit <- survival::survfit(survival::Surv(time, status == 2) ~ 1, data = d)
  expect_equal(res$figures$km$surv, fit$surv, tolerance = 1e-10)
  expect_true("cox_km_overall" %in% names(res$fragments))
})

# ---- end to end ---------------------------------------------------------------------------------

test_that("cox runs through sr_sap_add, lock and sr_run; hostile names are data", {
  bt <- intToUtf8(96)
  hostile <- paste0("v", bt, "); system(", intToUtf8(39), "calc", intToUtf8(39), "); (")
  d <- survival::lung
  d$id <- seq_len(nrow(d))
  names(d)[names(d) == "time"] <- hostile
  names(d)[names(d) == "sex"] <- "a b"
  roles <- c(id = "id", status = "outcome")
  p <- .fx_ready_project(d, "t", roles)
  .cx_route("cox")
  s <- .cx_spec(time = hostile, exposure = "a b", terms = list(list(var = "age")))
  p <- sr_sap_add(p, "m1", "cox", s, "t", label = "primary")
  s2 <- .cx_spec(time = hostile, exposure = "a b", cluster = "inst", layers = .cx_layers(TRUE))
  p <- sr_sap_add(p, "m2", "cox", s2, "t", label = "secondary")
  p <- sr_sap_lock(.fx_feasible(p))
  p <- sr_run(p, "m1")
  p <- sr_run(p, "m2")
  expect_true(p$state$runs$m1$ok, info = p$state$runs$m1$error)
  expect_true(p$state$runs$m2$ok, info = p$state$runs$m2$error)
  res <- sr_result(p, "m1")
  fit <- survival::coxph(survival::Surv(time, status) ~ sex + age, data = survival::lung)
  expect_equal(res$effects$est[res$effects$term == "a b"], unname(exp(stats::coef(fit)["sex"])), tolerance = 1e-8)
  expect_true(is.data.frame(res$figures$km) && nrow(res$figures$km) > 0L)
  expect_true(is.data.frame(res$figures$km_risk))
  res2 <- sr_result(p, "m2")
  dd <- survival::lung[!is.na(survival::lung$inst), ]
  fit2 <- survival::coxph(survival::Surv(time, status) ~ sex + cluster(inst), data = dd)
  e2 <- res2$effects[res2$effects$term == "a b", ]
  expect_equal(e2$lo, unname(exp(stats::coef(fit2) - stats::qnorm(0.975) * sqrt(stats::vcov(fit2)[1, 1]))),
               tolerance = 1e-8)
  code <- sr_compile(.cx_route("cox"), s)
  expect_false("system" %in% all.names(parse(text = code)))
  expect_true(sr_run(p, "m1")$state$runs$m1$cached)
})

# ---- fix round 1 --------------------------------------------------------------------------------

test_that("the note says adjusted only when the model has terms beyond the exposure", {
  d <- survival::lung
  crude <- .cx_present(.cx_value(d), .cx_spec())$fragments$cox_note
  expect_false(grepl("adjusted for", crude, fixed = TRUE))
  expect_match(crude, "unadjusted", fixed = TRUE)
  s <- .cx_spec(terms = list(list(var = "age")))
  adj <- .cx_present(.cx_value(d, s), s)$fragments$cox_note
  expect_match(adj, "adjusted for the other terms of the model", fixed = TRUE)
  s2 <- .cx_spec(strata = "ph.ecog")
  strat <- .cx_present(.cx_value(d, s2), s2)$fragments$cox_note
  expect_false(grepl("adjusted for the other terms", strat, fixed = TRUE))
  expect_match(strat, "stratified by ph.ecog", fixed = TRUE)
})

test_that("strata with no other terms say 'stratified by', keep the conditional label, and the estimand agrees", {
  d <- survival::lung
  s <- .cx_spec(strata = c("ph.ecog", "inst"))
  out <- .cx_present(.cx_value(d, s), s)
  note <- out$fragments$cox_note
  expect_match(note, "stratified by ph.ecog, inst (no other model terms)", fixed = TRUE)
  expect_false(grepl("unadjusted", note, fixed = TRUE))
  expect_false(grepl("baseline hazard stratified", note, fixed = TRUE))
  expect_true(all(out$effects$conditional[out$effects$measure == "HR"]))
  est <- .cx_route("cox")$estimand(s)
  expect_identical(est$summary, "hazard ratio stratified by ph.ecog, inst (no other model terms)")
  # with other terms: adjusted, and the baseline hazard stratification is still named
  s2 <- .cx_spec(strata = "ph.ecog", terms = list(list(var = "age")))
  note2 <- .cx_present(.cx_value(d, s2), s2)$fragments$cox_note
  expect_match(note2, "adjusted for the other terms of the model", fixed = TRUE)
  expect_match(note2, "baseline hazard stratified by ph.ecog", fixed = TRUE)
  expect_identical(.cx_route("cox")$estimand(s2)$summary, "adjusted hazard ratio")
  expect_identical(.cx_route("cox")$estimand(.cx_spec())$summary, "hazard ratio")
})

test_that("a 1/2 status with no status_event is refused before the lock; with it set, it locks and runs", {
  d <- survival::lung[c("time", "status", "sex", "age")]
  d$id <- seq_len(nrow(d))
  p <- .fx_ready_project(d, "t", c(id = "id", status = "outcome"))
  .cx_route("cox")
  s <- .cx_spec()
  s$status_event <- NULL
  expect_error(sr_sap_add(p, "m1", "cox", s, "t", label = "primary"),
               "set event_value (status_event for a survival status)", fixed = TRUE)
  expect_length(p$state$analyses, 0L)
  p <- sr_sap_add(p, "m1", "cox", .cx_spec(), "t", label = "primary")
  p <- sr_sap_lock(.fx_feasible(p))
  p <- sr_run(p, "m1")
  expect_true(p$state$runs$m1$ok, info = p$state$runs$m1$error)
  fit <- survival::coxph(survival::Surv(time, status) ~ sex, data = survival::lung)
  res <- sr_result(p, "m1")
  expect_equal(res$effects$est[res$effects$term == "sex"], unname(exp(stats::coef(fit))), tolerance = 1e-8)
})

test_that("a character status works end to end with a character status_event", {
  d <- survival::lung[c("time", "status", "sex", "age")]
  d$id <- seq_len(nrow(d))
  d$vital <- ifelse(d$status == 2L, "dead", "alive")
  d$status <- NULL
  r <- .cx_route("cox")
  s <- .cx_spec(status = "vital", status_event = "dead")
  expect_identical(r$spec_schema$status_event$type, "num|chr")
  expect_true(sr_spec_validate(s, r)$ok)
  expect_false(sr_spec_validate(.cx_spec(status_event = ""), r)$ok)
  expect_match(sr_compile(r, s), "== \"dead\"", fixed = TRUE)
  p <- .fx_ready_project(d, "t", c(id = "id", vital = "outcome"))
  s0 <- s
  s0$status_event <- NULL
  expect_error(sr_sap_add(p, "m0", "cox", s0, "t"), "set event_value", fixed = TRUE)
  p <- sr_sap_add(p, "m1", "cox", s, "t", label = "primary")
  p <- sr_sap_lock(.fx_feasible(p))
  p <- sr_run(p, "m1")
  expect_true(p$state$runs$m1$ok, info = p$state$runs$m1$error)
  fit <- survival::coxph(survival::Surv(time, status) ~ sex, data = survival::lung)
  res <- sr_result(p, "m1")
  expect_equal(res$effects$est[res$effects$term == "sex"], unname(exp(stats::coef(fit))), tolerance = 1e-8)
})

test_that("cox_describe compiles, runs and presents HR rows, the same numbers as cox", {
  d <- survival::lung
  sd <- .cx_spec(purpose = "describe")
  s <- .cx_spec()
  b <- .cx_present(.cx_value(d, sd, "cox_describe"), sd, "cox_describe")
  a <- .cx_present(.cx_value(d, s, "cox"), s, "cox")
  expect_identical(a$effects, b$effects)
  expect_equal(nrow(.cx_hr(b$effects, "sex")), 1L)
  expect_true(is.data.frame(b$figures$km) && nrow(b$figures$km) > 0L)
  expect_true(any(a$flags$flag == "table2") || nrow(a$flags) == 0L)
  expect_false(any(b$flags$flag == "table2"))
})

# ---- exclude_rows (the influence sensitivity analysis) ------------------------------------------

test_that("cox exclude_rows drop those positions; fragment counts them; bad values refused", {
  d <- survival::lung
  s <- .cx_spec(exclude_rows = c(1L, 3L, 50L))
  expect_true(sr_spec_validate(s, .cx_route())$ok)
  val <- .cx_value(d, s)
  fit <- survival::coxph(survival::Surv(time, status) ~ sex, data = d[-c(1, 3, 50), ])
  expect_equal(val$coef$est, unname(stats::coef(fit)), tolerance = 1e-8)
  expect_equal(val$n, 225)
  pr <- .cx_present(val, s)
  expect_match(pr$fragments$exclude_rows, "3 row(s) excluded by position", fixed = TRUE)
  for (bad in list(0L, c(4L, 4L), 2.5, NA_real_, "x")) {
    expect_false(sr_spec_validate(.cx_spec(exclude_rows = bad), .cx_route())$ok, info = format(bad))
  }
})
