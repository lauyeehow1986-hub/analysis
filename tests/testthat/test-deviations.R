# Tests for sap/deviations.R: post-lock analyses (sr_analysis_add), acting on diagnostic fails
# (sr_diag_apply, the R35 mapper .sr_diag_sensitivity), the deviations list (sr_deviations) and the
# multiplicity adjustment of a family (sr_family_adjust). Seeded synthetic data only (the Task 11
# non-PH generator, copied below); projects live under withr::local_tempdir().

.dv_layers <- function(...) {
  utils::modifyList(list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                         framework = "frequentist", clustered = FALSE), list(...))
}

.dv_mod <- function(s, ...) {
  mod <- list(...)
  for (nm in names(mod)) s[nm] <- list(mod[[nm]])
  s
}

.dv_cox <- function(...) {
  .dv_mod(list(purpose = "explain-causal", time = "time", status = "status", exposure = "x",
               layers = .dv_layers(), seed = 11L), ...)
}
.dv_lm <- function(...) {
  .dv_mod(list(purpose = "explain-causal", outcome = "y", exposure = "x", terms = list(list(var = "z")),
               layers = .dv_layers(), seed = 11L), ...)
}
.dv_logit <- function(...) {
  .dv_mod(list(purpose = "explain-causal", outcome = "y", exposure = "x", terms = list(list(var = "z")),
               layers = .dv_layers(), seed = 11L), ...)
}

# The routes, registered again when an earlier test file emptied the registry.
.dv_routes <- function() {
  make <- list(lm = .sr_lm_route, lm_describe = .sr_lm_describe_route, logistic = .sr_logistic_route,
               logistic_describe = .sr_logistic_describe_route, modpois = .sr_modpois_route,
               cox = .sr_cox_route, cox_describe = .sr_cox_describe_route, rmst = .sr_rmst_route,
               table1 = .sr_table1_route)
  for (k in names(make)) {
    if (!(.sr_route_key(k, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make[[k]]())
  }
  invisible(TRUE)
}

# Survival times with a binary exposure x: baseline hazard log(2), hazard ratio h1 before t = cut and
# h2 after it (piecewise exponential), administrative censoring at `cens` (Task 11 generator).
.dv_surv <- function(n, h1, h2, seed, cut = 1, cens = 4) {
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

# A ready (not yet locked) project on the non-PH data: hazard ratio 3 before t = 1, 0.8 after.
.dv_project <- function(env = parent.frame()) {
  .dv_routes()
  d <- .dv_surv(400, 3, 0.8, 101)
  set.seed(102)
  d$z <- round(rnorm(400), 3)
  d$id <- seq_len(nrow(d))
  .fx_ready_project(d, "t", c(id = "id", status = "outcome"), env = env)
}

.dv_events <- function(p) length(sr_events_read(p$paths$events))

.dv_replay_ok <- function(p) {
  rp <- sr_state_replay(sr_events_read(p$paths$events))
  expect_identical(rp$analyses, p$state$analyses)
  expect_identical(rp$diag, p$state$diag)
  expect_identical(rp$sap, p$state$sap)
}

# ---- the R35 mapper -----------------------------------------------------------------------------

.dv_parent <- function(route, spec, id = "m1") {
  list(id = id, route = route, route_version = 1L, table = "t", label = "primary", spec = spec,
       prespecified = TRUE)
}

.dv_valid <- function(m) {
  r <- sr_route_get(m$route)
  v <- sr_spec_validate(m$spec, r)
  expect_true(v$ok, info = paste(v$errors, collapse = "; "))
  expect_true(sr_compose_check(m$spec)$ok)
  expect_silent(sr_compile(r, m$spec))
}

test_that("ph maps a cox parent to an unadjusted rmst with tau_suggest, keeping the layers", {
  .dv_routes()
  par <- .dv_parent("cox", .dv_cox(status_event = 1, terms = list(list(var = "z")), strata = "g"))
  m <- .sr_diag_sensitivity("ph", par, list(check = "ph", tau_suggest = 1.7, detail = "d"))
  expect_identical(m$route, "rmst")
  expect_equal(m$spec$tau, 1.7)
  expect_identical(m$spec$time, "time")
  expect_identical(m$spec$status, "status")
  expect_equal(m$spec$status_event, 1)
  expect_identical(m$spec$exposure, "x")
  expect_identical(m$spec$seed, 11L)
  expect_identical(m$spec$layers, par$spec$layers)
  expect_null(m$spec$terms)
  expect_null(m$spec$strata)
  .dv_valid(m)
  # R5: the tau of a pre-specified rmst analysis whose parent is this analysis wins over tau_suggest
  kid <- .dv_parent("rmst", .dv_cox(tau = 2.5), id = "m1_km")
  kid$parent <- "m1"
  kid$label <- "sensitivity"
  m2 <- .sr_diag_sensitivity("ph", par, list(check = "ph", tau_suggest = 1.7), analyses = list(m1_km = kid))
  expect_equal(m2$spec$tau, 2.5)
  # an rmst analysis of another parent does not count
  kid$parent <- "other"
  m3 <- .sr_diag_sensitivity("ph", par, list(check = "ph", tau_suggest = 1.7), analyses = list(m1_km = kid))
  expect_equal(m3$spec$tau, 1.7)
  # no tau at all: a limitation, not an analysis
  lim <- .sr_diag_sensitivity("ph", par, list(check = "ph", tau_suggest = NA_real_))
  expect_true(is.character(lim$limitation) && nzchar(lim$limitation))
  expect_null(lim$spec)
})

test_that("ph of a clustered cox parent gives an unclustered rmst and says so", {
  .dv_routes()
  par <- .dv_parent("cox", .dv_cox(cluster = "id", layers = .dv_layers(clustered = TRUE)))
  m <- .sr_diag_sensitivity("ph", par, list(check = "ph", tau_suggest = 2))
  expect_false(m$spec$layers$clustered)
  expect_null(m$spec$cluster)
  expect_match(m$note, "unclustered")
  .dv_valid(m)
  # a cox_describe parent still gives the rmst route (its purpose)
  pd <- .dv_parent("cox_describe", .dv_cox(purpose = "describe"))
  md <- .sr_diag_sensitivity("ph", pd, list(check = "ph", tau_suggest = 2))
  expect_identical(md$spec$purpose, sr_route_get("rmst")$purpose)
  .dv_valid(md)
})

test_that("influence keeps the route and adds exclude_rows, mapped back through an earlier exclusion", {
  .dv_routes()
  par <- .dv_parent("lm", .dv_lm())
  m <- .sr_diag_sensitivity("influence", par, list(check = "influence", rows = c(9L, 5L)))
  expect_identical(m$route, "lm")
  expect_identical(m$spec$exclude_rows, c(5L, 9L))
  expect_identical(m$spec[setdiff(names(m$spec), "exclude_rows")], par$spec)
  .dv_valid(m)
  # positions of the reduced data map back to the table: reduced 2 -> 3, reduced 5 -> 7
  par2 <- .dv_parent("lm", .dv_lm(exclude_rows = c(2L, 6L)))
  m2 <- .sr_diag_sensitivity("influence", par2, list(check = "influence", rows = c(2L, 5L)))
  expect_identical(m2$spec$exclude_rows, c(2L, 3L, 6L, 7L))
  # nothing flagged: a limitation
  expect_true(nzchar(.sr_diag_sensitivity("influence", par, list(check = "influence", rows = integer()))$limitation))
  # cox and logistic keep their route too
  mc <- .sr_diag_sensitivity("influence", .dv_parent("cox", .dv_cox()), list(check = "influence", rows = 3L))
  expect_identical(mc$route, "cox")
  .dv_valid(mc)
})

test_that("separation maps to Firth logistic with a model SE and no marginal effects", {
  .dv_routes()
  par <- .dv_parent("logistic", .dv_logit(se = "robust", marginal = TRUE))
  m <- .sr_diag_sensitivity("separation", par, list(check = "separation"))
  expect_identical(m$route, "logistic")
  expect_identical(m$spec$penalty, "firth")
  expect_identical(m$spec$se, "model")
  expect_false(isTRUE(m$spec$marginal))
  .dv_valid(m)
  m2 <- .sr_diag_sensitivity("separation", .dv_parent("logistic_describe", .dv_logit(purpose = "describe")),
                             list(check = "separation"))
  expect_identical(m2$route, "logistic_describe")
  .dv_valid(m2)
})

test_that("few_clusters maps to the cluster bootstrap with B = 2000 percentile", {
  .dv_routes()
  par <- .dv_parent("lm", .dv_lm(se = "cluster", cluster = "g", layers = .dv_layers(clustered = TRUE)))
  m <- .sr_diag_sensitivity("few_clusters", par, list(check = "few_clusters"))
  expect_identical(m$route, "lm")
  expect_identical(m$spec$layers$resampling, "bootstrap")
  expect_identical(m$spec$boot_type, "cluster")
  expect_identical(m$spec$boot, list(B = 2000L, type = "percentile"))
  .dv_valid(m)
})

test_that("linearity moves the flagged term to RCS(4); the exposure or an unknown term is a limitation", {
  .dv_routes()
  par <- .dv_parent("lm", .dv_lm(terms = list(list(var = "z"), list(var = "w", type = "linear"))))
  m <- .sr_diag_sensitivity("linearity", par, list(check = "linearity", var = "w"))
  expect_identical(m$route, "lm")
  expect_identical(m$spec$terms[[1L]], list(var = "z"))
  expect_identical(m$spec$terms[[2L]]$type, "rcs")
  expect_identical(m$spec$terms[[2L]]$knots, 4L)
  expect_identical(m$spec$terms[[2L]]$var, "w")
  .dv_valid(m)
  expect_true(nzchar(.sr_diag_sensitivity("linearity", par, list(check = "linearity", var = "x"))$limitation))
  expect_true(nzchar(.sr_diag_sensitivity("linearity", par, list(check = "linearity", var = "nope"))$limitation))
  expect_true(nzchar(.sr_diag_sensitivity("linearity", par, list(check = "linearity", var = NA_character_))$limitation))
})

test_that("heteroscedasticity maps to se = robust; inf_censoring is a limitation", {
  .dv_routes()
  par <- .dv_parent("lm", .dv_lm())
  m <- .sr_diag_sensitivity("heteroscedasticity", par, list(check = "heteroscedasticity"))
  expect_identical(m$route, "lm")
  expect_identical(m$spec$se, "robust")
  .dv_valid(m)
  lim <- .sr_diag_sensitivity("inf_censoring", .dv_parent("cox", .dv_cox()), list(check = "inf_censoring"))
  expect_match(lim$limitation, "censoring")
  expect_null(lim$spec)
})

test_that("rmst refuses exclude_rows instead of ignoring it", {
  .dv_routes()
  v <- sr_spec_validate(.dv_cox(tau = 2, exclude_rows = 3L), sr_route_get("rmst"))
  expect_false(v$ok)
  expect_match(paste(v$errors, collapse = " "), "exclude_rows")
})

test_that(".sr_diag_sensitivity_id truncates to the analysis id pattern", {
  expect_identical(.sr_diag_sensitivity_id("m1", "ph"), "m1_ph")
  long <- .sr_diag_sensitivity_id(strrep("a", 38), "heteroscedasticity")
  expect_identical(nchar(long), 40L)
  expect_true(grepl(SR_ANALYSIS_ID_RE, long))
  expect_true(grepl(SR_ANALYSIS_ID_RE, .sr_diag_sensitivity_id("Model-1", "ph")))
})

# ---- sr_family_adjust (pure: a stand-in project and sr_result) --------------------------------

.dv_effects <- function(p, measure = "HR") {
  data.frame(term = c("x", "z", "x (marginal)"), measure = c(measure, measure, "RD"), est = c(1.5, 1.1, 0.1),
             lo = c(1.1, 0.9, 0.0), hi = c(2, 1.3, 0.2), p = c(p, 0.5, 0.001), scale = "ratio",
             level = NA_character_, mcse = NA_real_, role = c("primary", "covariate", "primary"),
             conditional = FALSE, ci_method = "wald", n = 100, events = 40, stringsAsFactors = FALSE)
}

.dv_fake_family <- function(method, run = c("a1", "a2")) {
  an <- list(a1 = list(id = "a1", label = "primary", prespecified = TRUE),
             a2 = list(id = "a2", label = "secondary", prespecified = TRUE))
  runs <- lapply(run, function(id) list(analysis = id, ok_key = paste0("k_", id)))
  names(runs) <- run
  list(state = list(analyses = an, runs = runs,
                    sap = list(families = list(f1 = list(id = "f1", members = c("a1", "a2"), method = method)))))
}

.dv_stub_result <- function(env = parent.frame()) {
  old <- get("sr_result", envir = globalenv())
  assign("sr_result", function(project, analysis_id) {
    list(effects = .dv_effects(c(a1 = 0.01, a2 = 0.04)[[analysis_id]]))
  }, envir = globalenv())
  withr::defer(assign("sr_result", old, envir = globalenv()), envir = env)
}

test_that("sr_family_adjust: p 0.01 and 0.04 give holm (0.02, 0.04) and BH (0.02, 0.04), as p.adjust", {
  .dv_stub_result()
  for (m in c("holm", "BH")) {
    fa <- sr_family_adjust(.dv_fake_family(m), "f1")
    expect_identical(names(fa), c("analysis", "term", "p", "p_adj", "method"))
    expect_identical(fa$analysis, c("a1", "a2"))
    expect_identical(fa$term, c("x", "x"))
    expect_equal(fa$p, c(0.01, 0.04))
    expect_equal(fa$p_adj, c(0.02, 0.04))
    expect_equal(fa$p_adj, stats::p.adjust(c(0.01, 0.04), m))
    expect_identical(unique(fa$method), m)
  }
  expect_equal(sr_family_adjust(.dv_fake_family("bonferroni"), "f1")$p_adj, c(0.02, 0.08))
  expect_error(sr_family_adjust(.dv_fake_family("holm", run = "a1"), "f1"), "a2")
  expect_error(sr_family_adjust(.dv_fake_family("holm", run = character()), "f1"), "a1, a2")
  expect_error(sr_family_adjust(.dv_fake_family("holm"), "nope"), "unknown family")
})

# ---- on a locked project ------------------------------------------------------------------------

test_that("no rule: apply adds <id>_ph as an unplanned sensitivity analysis and never touches the primary", {
  p <- .dv_project()
  s <- .dv_cox(terms = list(list(var = "z")))
  p <- sr_sap_add(p, "m1", "cox", s, "t", label = "primary", family = "f1")
  p <- sr_sap_add(p, "m2", "cox", .dv_cox(exposure = "z"), "t", label = "secondary", family = "f1")
  p <- sr_sap_family(p, "f1", c("m1", "m2"), "holm")
  expect_error(sr_diag_apply(p, "m1"), "not locked")
  p <- sr_sap_lock(p)
  hash0 <- p$state$sap$lock$sap_hash
  p <- sr_run(p, "m1")
  expect_true(p$state$runs$m1$ok, info = p$state$runs$m1$error)
  expect_error(sr_diag_apply(p, "m1"), "sr_diag_run")
  p <- sr_diag_run(p, "m1")
  dg <- p$state$diag$m1
  expect_identical(dg$ph$verdict, "fail")
  m1_before <- p$state$analyses$m1
  p <- sr_diag_apply(p, "m1")
  expect_identical(p$state$analyses$m1, m1_before)
  expect_identical(p$state$analyses$m1$label, "primary")
  a <- p$state$analyses$m1_ph
  expect_false(is.null(a))
  expect_identical(a$label, "sensitivity")
  expect_false(a$prespecified)
  expect_identical(a$trigger, "ph")
  expect_identical(a$parent, "m1")
  expect_identical(a$route, "rmst")
  expect_equal(a$spec$tau, dg$ph$tau_suggest)
  expect_true(startsWith(a$reason, "ph verdict fail: "))
  # every analysis the apply step added is a non-pre-specified sensitivity analysis of m1
  added <- setdiff(names(p$state$analyses), c("m1", "m2"))
  for (id in added) {
    expect_false(p$state$analyses[[id]]$prespecified, info = id)
    expect_identical(p$state$analyses[[id]]$label, "sensitivity", info = id)
    expect_identical(p$state$analyses[[id]]$parent, "m1", info = id)
  }
  # every fail is either an analysis or a limitation (never skipped); an inf_censoring fail is a limitation
  for (ck in names(dg)) {
    if (!identical(dg[[ck]]$verdict, "fail")) next
    id <- .sr_diag_sensitivity_id("m1", ck)
    expect_true(!is.null(p$state$analyses[[id]]) || nzchar(p$state$diag$m1[[ck]]$limitation %||% ""), info = ck)
    if (ck == "inf_censoring") expect_null(p$state$analyses[[id]])
  }
  expect_identical(sr_sap_hash(p), hash0)
  dv <- sr_deviations(p)
  expect_identical(names(dv), c("analysis", "label", "kind", "reason", "trigger"))
  row <- dv[dv$analysis == "m1_ph", , drop = FALSE]
  expect_identical(row$kind, "unplanned_sensitivity")
  expect_identical(row$trigger, "ph")
  expect_identical(row$label, "sensitivity")
  expect_false(any(dv$kind %in% c("rule_fired", "lib_changed")))
  .dv_replay_ok(p)
  # applying again changes nothing
  n0 <- .dv_events(p)
  q <- sr_diag_apply(p, "m1")
  expect_identical(.dv_events(q), n0)
  expect_identical(q$state, p$state)
  # the sensitivity analysis runs
  p <- sr_run(p, "m1_ph")
  expect_true(p$state$runs$m1_ph$ok, info = p$state$runs$m1_ph$error)
  expect_true(any(sr_result(p, "m1_ph")$effects$measure == "RMST_diff"))
  # the family: m2 not run yet -> named; then holm over the two primary rows
  expect_error(sr_family_adjust(p, "f1"), "m2")
  p <- sr_run(p, "m2")
  fa <- sr_family_adjust(p, "f1")
  pr <- function(id) {
    e <- sr_result(p, id)$effects
    e$p[e$role == "primary"][1L]
  }
  expect_identical(fa$analysis, c("m1", "m2"))
  expect_equal(fa$p, c(pr("m1"), pr("m2")))
  expect_equal(fa$p_adj, stats::p.adjust(c(pr("m1"), pr("m2")), "holm"))
  # a run on a library snapshot other than the lock's is listed as lib_changed
  expect_false(any(sr_deviations(p)$kind == "lib_changed"))
  p$state$sap$lock$libsnap_hash <- strrep("0", 64)
  p <- sr_run(p, "m1")
  dv <- sr_deviations(p)
  lc <- dv[dv$kind == "lib_changed", , drop = FALSE]
  expect_identical(lc$analysis, "m1")
})

test_that("a pre-lock replace_primary rule makes the pre-specified RMST primary and Cox sensitivity", {
  p <- .dv_project()
  p <- sr_sap_add(p, "m1", "cox", .dv_cox(terms = list(list(var = "z"))), "t", label = "primary")
  p <- sr_sap_add(p, "m1_km", "rmst", .dv_cox(tau = 2), "t", label = "sensitivity", parent = "m1")
  p <- sr_sap_rule(p, "r1", "m1", "ph", "replace_primary", "m1_km")
  p <- sr_sap_lock(p)
  hash0 <- p$state$sap$lock$sap_hash
  p <- sr_run(p, "m1")
  p <- sr_diag_run(p, "m1", checks = "ph")
  expect_identical(p$state$diag$m1$ph$verdict, "fail")
  n_an <- length(p$state$analyses)
  p <- sr_diag_apply(p, "m1")
  expect_identical(length(p$state$analyses), n_an)
  expect_null(p$state$analyses$m1_ph)
  expect_identical(p$state$analyses$m1_km$label, "primary")
  expect_identical(p$state$analyses$m1$label, "sensitivity")
  expect_true(p$state$analyses$m1_km$prespecified)
  expect_true(p$state$analyses$m1$prespecified)
  expect_identical(sr_sap_hash(p), hash0)
  dv <- sr_deviations(p)
  expect_true("rule_fired" %in% dv$kind)
  expect_false("unplanned_sensitivity" %in% dv$kind)
  rf <- dv[dv$kind == "rule_fired", , drop = FALSE]
  expect_identical(rf$analysis, "m1")
  expect_identical(rf$trigger, "ph")
  expect_match(rf$reason, "replace_primary")
  expect_match(rf$reason, "m1_km")
  .dv_replay_ok(p)
  n0 <- .dv_events(p)
  expect_identical(.dv_events(sr_diag_apply(p, "m1")), n0)
})

test_that("an add_sensitivity rule fires without adding or relabelling anything", {
  p <- .dv_project()
  p <- sr_sap_add(p, "m1", "cox", .dv_cox(), "t", label = "primary")
  p <- sr_sap_add(p, "m1_km", "rmst", .dv_cox(tau = 2), "t", label = "sensitivity", parent = "m1")
  p <- sr_sap_rule(p, "r1", "m1", "ph", "add_sensitivity", "m1_km")
  p <- sr_sap_lock(p)
  p <- sr_run(p, "m1")
  p <- sr_diag_run(p, "m1", checks = "ph")
  an0 <- p$state$analyses
  p <- sr_diag_apply(p, "m1")
  expect_identical(p$state$analyses, an0)
  dv <- sr_deviations(p)
  expect_identical(dv$kind, "rule_fired")
  expect_match(dv$reason, "add_sensitivity")
})

test_that("sr_analysis_add: post-lock only, reason required, validated like sr_sap_add (R19)", {
  p <- .dv_project()
  s <- .dv_cox()
  expect_error(sr_analysis_add(p, "e1", "cox", s, "t", "exploratory", reason = "look"), "not locked")
  p <- sr_sap_add(p, "m1", "cox", s, "t", label = "primary")
  p <- sr_sap_lock(p)
  hash0 <- sr_sap_hash(p)
  expect_error(sr_analysis_add(p, "e1", "cox", s, "t", "exploratory"), "reason")
  expect_error(sr_analysis_add(p, "e1", "cox", s, "t", "exploratory", reason = ""), "reason")
  expect_error(sr_analysis_add(p, "e1", "cox", s, "t", "primary", reason = "x"))
  expect_error(sr_analysis_add(p, "m1", "cox", s, "t", "exploratory", reason = "x"), "already")
  expect_error(sr_analysis_add(p, "E-1", "cox", s, "t", "exploratory", reason = "x"), "id")
  expect_error(sr_analysis_add(p, "e1", "cox", .dv_cox(time = NULL), "t", "exploratory", reason = "x"), "not valid")
  expect_error(sr_analysis_add(p, "e1", "cox", .dv_cox(layers = .dv_layers(weights = "ipw")), "t",
                               "exploratory", reason = "x"), "weights")
  expect_error(sr_analysis_add(p, "e1", "cox", s, "nope", "exploratory", reason = "x"), "nope")
  expect_error(sr_analysis_add(p, "e1", "cox", s, "t", "sensitivity", reason = "x", parent = "zz"), "zz")
  expect_error(sr_analysis_add(p, "e1", "cox", s, "t", "sensitivity", reason = "x", trigger = "bogus"), "trigger")
  ro <- p
  ro$readonly <- TRUE
  expect_error(sr_analysis_add(ro, "e1", "cox", s, "t", "exploratory", reason = "x"), "read-only")
  p <- sr_analysis_add(p, "e1", "cox", .dv_cox(exposure = "z"), "t", "exploratory", reason = "reviewer asked")
  p <- sr_analysis_add(p, "h1", "cox", .dv_cox(terms = list(list(var = "z"))), "t", "post_hoc",
                       reason = "after seeing the data")
  p <- sr_analysis_add(p, "s1", "cox", .dv_cox(ties = "breslow"), "t", "sensitivity", reason = "ties",
                       parent = "m1")
  e1 <- p$state$analyses$e1
  expect_false(e1$prespecified)
  expect_identical(e1$label, "exploratory")
  expect_identical(e1$reason, "reviewer asked")
  expect_true(is.na(e1$parent))
  expect_true(is.na(e1$trigger))
  expect_identical(e1$route_version, 1L)
  expect_identical(sr_sap_hash(p), hash0)
  dv <- sr_deviations(p)
  expect_identical(dv$analysis, c("e1", "h1", "s1"))
  expect_identical(dv$kind, c("exploratory", "post_hoc", "unplanned_sensitivity"))
  expect_identical(dv$reason, c("reviewer asked", "after seeing the data", "ties"))
  .dv_replay_ok(p)
  p <- sr_run(p, "e1")
  expect_true(p$state$runs$e1$ok, info = p$state$runs$e1$error)
})
