# Tests for sap/feasibility.R: the feasibility events, assess / decide / stop, staleness, report and the
# lock gate. Synthetic data only; every project lives under withr::local_tempdir().

.ft_layers <- function(clustered = FALSE) {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = clustered)
}

# n = 500: binary x on z1, covariates z1 z2 z3, a 3-level site, a binary outcome y ("dead" / "alive"), a
# 12-level cluster cl; 10% NA in z2 and 5% NA in y.
.ft_frame <- function(n = 500L, permute_y = FALSE) {
  df <- withr::with_seed(1L, {
    z1 <- stats::rnorm(n)
    z2 <- stats::rnorm(n)
    z3 <- stats::rnorm(n)
    x <- stats::rbinom(n, 1L, stats::plogis(z1))
    site <- sample(c("a", "b", "c"), n, replace = TRUE)
    y <- ifelse(stats::rbinom(n, 1L, stats::plogis(-0.5 + 0.7 * x + 0.3 * z1)) == 1L, "dead", "alive")
    z2[sample(n, n / 10)] <- NA
    y[sample(n, n / 20)] <- NA
    data.frame(id = seq_len(n), y = y, x = x, z1 = z1, z2 = z2, z3 = z3, site = site,
               cl = sample(1:12, n, replace = TRUE), stringsAsFactors = FALSE)
  })
  if (permute_y) {
    i <- which(!is.na(df$y))
    df$y[i] <- df$y[i][withr::with_seed(2L, sample(length(i)))]
  }
  df
}

.ft_roles <- c(id = "id", y = "outcome", x = "exposure")

.ft_spec <- function(terms = list(list(var = "z1", type = "linear"), list(var = "z2", type = "linear")), ...) {
  utils::modifyList(list(purpose = "explain-causal", outcome = "y", event_value = "dead", exposure = "x",
                         terms = terms, layers = .ft_layers(), seed = 1L), list(...))
}

.ft_drive <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())

# A ready project on the frame with the logistic primary p1.
.ft_project <- function(df = .ft_frame(), env = parent.frame(), spec = .ft_spec()) {
  p <- .fx_ready_project(df, "t", .ft_roles, env = env)
  sr_sap_add(p, "p1", "logistic", spec, "t", label = "primary")
}

.ft_delta <- function(value = 1.5) list(value = value, measure = "OR", source = "MCID literature")

test_that("assess stores the verdict, replays identically and tie it to the data and the spec", {
  p <- .ft_project()
  expect_null(sr_feasibility(p, "p1")$assessment)
  expect_true(is.na(sr_feasibility(p, "p1")$stale))
  p <- sr_feasibility_assess(p, "p1")
  f <- sr_feasibility(p, "p1")
  expect_identical(f$assessment$tier, "precision_only")
  expect_null(f$decision)
  expect_true(is.na(f$stale))
  expect_identical(f$assessment$analysis, "p1")
  expect_false(any(c("pairs") %in% names(f$assessment$inputs$outcome)))
  expect_s3_class(f$assessment$contrasts, "data.frame")
  expect_identical(f$assessment$contrasts$measure, c("OR", "RD", "RR"))
  expect_identical(f$assessment$inputs$exposure$levels$level, c("0", "1"))
  expect_identical(f$assessment$data_hash, sr_store_hash(sr_store_manifest(p, "t")))
  expect_match(f$assessment$spec_hash, "^[0-9a-f]{64}$")
  expect_identical(f$assessment$delta$value, NA_real_)
  expect_identical(f$assessment$assumptions$icc$value, NA_real_)

  # live folding equals replay, and the hashes still match after a reopen
  sr_project_close(p)
  p2 <- sr_project_open(p$paths$dir)
  withr::defer(sr_project_close(p2))
  expect_identical(p2$state$feasibility, p$state$feasibility)
  expect_true(is.na(.sr_feas_stale(p2, "p1")))
})

test_that("a delta and assumptions are stored, normalised and checked", {
  p <- .ft_project()
  expect_error(sr_feasibility_assess(p, "p1", delta = list(value = 1, measure = "OR", source = "x")), "ratio delta")
  expect_error(sr_feasibility_assess(p, "p1", delta = list(value = 1.5, measure = "MD", source = "x")),
               "delta\\$measure must be one of")
  expect_error(sr_feasibility_assess(p, "p1", assumptions = list(icc = list(value = 0.05, justification = "j"))),
               "icc applies to a clustered analysis only")
  p <- sr_feasibility_assess(p, "p1", delta = .ft_delta())
  as <- sr_feasibility(p, "p1")$assessment
  expect_identical(as$delta, list(value = 1.5, measure = "OR", source = "MCID literature"))
  expect_true(as$tier %in% c("adequate", "limited", "inadequate"))
  expect_identical(as$contrasts$tier[as$contrasts$measure == "RD"], "precision_only")
  sr_project_close(p)
  p2 <- sr_project_open(p$paths$dir)
  withr::defer(sr_project_close(p2))
  expect_identical(p2$state$feasibility, p$state$feasibility)
})

test_that("the verdict is outcome-blind end to end: permuting y among its non-missing rows changes nothing", {
  a <- sr_feasibility_assess(.ft_project(.ft_frame()), "p1")$state$feasibility$p1$assessment
  b <- sr_feasibility_assess(.ft_project(.ft_frame(permute_y = TRUE)), "p1")$state$feasibility$p1$assessment
  expect_false(identical(a$data_hash, b$data_hash))
  a$data_hash <- NULL
  b$data_hash <- NULL
  expect_identical(sr_canonical_json(b), sr_canonical_json(a))
})

test_that("exclude_rows is ignored end to end (R16): outcome-blind, same verdict, one warning", {
  # the extract is sorted by the outcome first: rows 1..229 are (about) the non-events, so applying
  # exclude_rows would store the exposure distribution among the events
  sp <- .ft_spec(exclude_rows = seq_len(229L))
  assess <- function(df, spec) sr_feasibility_assess(.ft_project(df, spec = spec), "p1")$state$feasibility$p1$assessment
  a <- assess(.ft_frame(), sp)
  b <- assess(.ft_frame(permute_y = TRUE), sp)
  c0 <- assess(.ft_frame(), .ft_spec())
  a$data_hash <- b$data_hash <- NULL
  expect_identical(sr_canonical_json(b), sr_canonical_json(a))
  expect_identical(a$inputs, c0$inputs)
  expect_identical(a$contrasts, c0$contrasts)
  expect_identical(a$tier, c0$tier)
  expect_identical(a$inputs$n_rows, 500L)
  expect_identical(setdiff(a$warnings, c0$warnings),
                   "exclude_rows is ignored by the feasibility verdict (it is an influence sensitivity for the locked run)")
  expect_false(identical(a$spec_hash, c0$spec_hash))               # exclude_rows stays in the spec hash
})

test_that("an outcome column used as a term is blocked end to end and the record stays outcome-blind (R17)", {
  sp <- .ft_spec(terms = list(list(var = "z1", type = "linear"), list(var = "y", type = "factor")))
  # the route refuses such a spec when it is added (F1) ...
  expect_error(.ft_project(.ft_frame(), spec = sp), "the outcome column 'y' cannot be a term, stratum or cluster",
               fixed = TRUE)
  # ... and the engine blocker stays as defence in depth: a SAP state that holds the spec anyway (written
  # in memory here) still gets a cannot_run, outcome-blind verdict
  tenv <- environment()
  with_spec <- function(df) {
    p <- .ft_project(df, env = tenv)
    p$state$analyses$p1$spec <- sp
    sr_feasibility_assess(p, "p1")
  }
  p <- with_spec(.ft_frame())
  a <- p$state$feasibility$p1$assessment
  b <- with_spec(.ft_frame(permute_y = TRUE))$state$feasibility$p1$assessment
  expect_identical(a$tier, "cannot_run")
  expect_true("design: the outcome column 'y' cannot be a term, stratum or cluster" %in% a$blockers)
  expect_true(all(is.na(a$inputs$contrasts_in$r2x)))
  a$data_hash <- b$data_hash <- NULL
  expect_identical(sr_canonical_json(b), sr_canonical_json(a))
  expect_error(sr_feasibility_decide(p, "p1", "proceed", reason = "x"), "cannot run")
})

test_that("decide: reasons are required, a cannot_run verdict cannot proceed, re-assessing clears the decision", {
  p <- .ft_project()
  expect_error(sr_feasibility_decide(p, "p1", "proceed"), "no feasibility assessment")
  p <- sr_feasibility_assess(p, "p1")
  expect_error(sr_feasibility_decide(p, "p1", "proceed"), "a reason is required to decide proceed \\(tier precision_only\\)")
  expect_error(sr_feasibility_decide(p, "p1", "proceed", reason = "  "), "a reason is required")
  expect_error(sr_feasibility_decide(p, "p1", "exploratory"), "a reason is required to decide exploratory")
  expect_error(sr_feasibility_decide(p, "p1", "revise"), "a reason is required")
  expect_error(sr_feasibility_decide(p, "p1", "stop"), "a reason is required")
  expect_error(sr_feasibility_decide(p, "p1", "maybe", reason = "x"), "decision must be one of")
  expect_error(sr_feasibility_decide(p, "zz", "stop", reason = "x"), "unknown analysis 'zz'")
  p <- sr_feasibility_decide(p, "p1", "proceed", reason = "precision reported in the Methods")
  expect_identical(sr_feasibility(p, "p1")$decision,
                   list(decision = "proceed", reason = "precision reported in the Methods"))
  sr_project_close(p)
  p2 <- sr_project_open(p$paths$dir)
  withr::defer(sr_project_close(p2))
  expect_identical(p2$state$feasibility, p$state$feasibility)
  p <- sr_feasibility_assess(p, "p1")
  expect_null(sr_feasibility(p, "p1")$decision)

  # an adequate verdict proceeds without a reason (a loose delta on a 500-row study)
  p <- sr_feasibility_assess(p, "p1", delta = .ft_delta(50))
  expect_identical(sr_feasibility(p, "p1")$assessment$tier, "adequate")
  p <- sr_feasibility_decide(p, "p1", "proceed")
  expect_true(is.na(sr_feasibility(p, "p1")$decision$reason))
  # an inadequate one needs a reason to proceed
  p <- sr_feasibility_assess(p, "p1", delta = .ft_delta(1.01))
  expect_identical(sr_feasibility(p, "p1")$assessment$tier, "inadequate")
  expect_error(sr_feasibility_decide(p, "p1", "proceed"), "tier inadequate")
  p <- sr_feasibility_decide(p, "p1", "exploratory", reason = "hypothesis generating")
  expect_identical(sr_feasibility(p, "p1")$decision$decision, "exploratory")
})

test_that("a cannot_run verdict can only be revised or stopped", {
  # exposure level 1 has no complete case: z2 is missing wherever x = 1
  df <- .ft_frame()
  df$z2[df$x == 1L] <- NA
  p <- .ft_project(df)
  p <- sr_feasibility_assess(p, "p1")
  expect_identical(sr_feasibility(p, "p1")$assessment$tier, "cannot_run")
  expect_true("exposure level '1' is observed but has no complete-case row" %in% sr_feasibility(p, "p1")$assessment$blockers)
  expect_error(sr_feasibility_decide(p, "p1", "proceed", reason = "x"), "cannot run.*decide revise or stop")
  expect_error(sr_feasibility_decide(p, "p1", "exploratory", reason = "x"), "cannot run")
  p <- sr_feasibility_decide(p, "p1", "revise", reason = "collect z2 in the exposed")
  expect_identical(sr_feasibility(p, "p1")$decision$decision, "revise")
  sr_project_close(p)
  p2 <- sr_project_open(p$paths$dir)
  withr::defer(sr_project_close(p2))
  expect_identical(p2$state$feasibility, p$state$feasibility)
})

test_that("a stop is sticky: no re-assess, no escape except a reasoned decide, even when stale (R6)", {
  p <- .ft_project()
  p <- sr_feasibility_assess(p, "p1")
  p <- sr_feasibility_decide(p, "p1", "stop", reason = "too few events")
  expect_identical(sr_project_stopped(p), list(analysis = "p1", reason = "too few events"))
  expect_error(sr_feasibility_assess(p, "p1"),
               "analysis 'p1' is stopped: reverse the stop with sr_feasibility_decide first")
  expect_error(sr_feasibility_decide(p, "p1", "revise"), "a reason is required to decide revise \\(replacing a stop\\)")
  expect_error(sr_feasibility_decide(p, "p1", "proceed"), "a reason is required")
  sr_project_close(p)
  p2 <- sr_project_open(p$paths$dir)
  withr::defer(sr_project_close(p2))
  expect_identical(sr_project_stopped(p2)$analysis, "p1")
  p <- sr_feasibility_decide(p, "p1", "revise", reason = "pool sites")
  expect_null(sr_project_stopped(p))

  # stop, then change the spec: the assessment is stale, a stop can still be replaced, nothing else can
  p <- sr_feasibility_assess(p, "p1")
  p <- sr_feasibility_decide(p, "p1", "stop", reason = "no")
  p <- sr_sap_add(p, "p1", "logistic", .ft_spec(terms = list(list(var = "z1"), list(var = "z2"), list(var = "z3"))),
                  "t", label = "primary")
  expect_identical(.sr_feas_stale(p, "p1"), "spec")
  expect_identical(sr_project_stopped(p)$analysis, "p1")
  p <- sr_feasibility_decide(p, "p1", "revise", reason = "added z3")
  expect_null(sr_project_stopped(p))
  expect_error(sr_feasibility_decide(p, "p1", "stop", reason = "again"), "stale")
  p <- sr_feasibility_assess(p, "p1")
  expect_true(is.na(.sr_feas_stale(p, "p1")))

  # R6 with proceed: replacing a stop on a stale assessment needs a reason, and then passes
  p <- sr_feasibility_decide(p, "p1", "stop", reason = "no again")
  p <- sr_sap_add(p, "p1", "logistic", .ft_spec(terms = list(list(var = "z1"))), "t", label = "primary")
  expect_identical(.sr_feas_stale(p, "p1"), "spec")
  expect_error(sr_feasibility_decide(p, "p1", "proceed"), "a reason is required to decide proceed \\(replacing a stop\\)")
  p <- sr_feasibility_decide(p, "p1", "proceed", reason = "the stop was a mistake")
  expect_null(sr_project_stopped(p))
  expect_identical(sr_feasibility(p, "p1")$decision$decision, "proceed")
})

test_that("staleness: a changed spec or a re-imported table; the decision is refused on a stale assessment", {
  p <- .ft_project()
  p <- sr_feasibility_assess(p, "p1")
  expect_true(is.na(.sr_feas_stale(p, "p1")))
  expect_true(is.na(.sr_feas_stale(p, "nope")))
  p <- sr_feasibility_decide(p, "p1", "proceed", reason = "ok")

  # a spec change: one more term
  p_spec <- sr_sap_add(p, "p1", "logistic", .ft_spec(terms = list(list(var = "z1"), list(var = "z2"), list(var = "z3"))),
                       "t", label = "primary")
  expect_identical(.sr_feas_stale(p_spec, "p1"), "spec")
  expect_error(sr_feasibility_decide(p_spec, "p1", "proceed", reason = "ok"), "stale \\(the spec changed\\)")
  # re-assessing clears the old decision and the staleness
  p_spec <- sr_feasibility_assess(p_spec, "p1")
  expect_true(is.na(.sr_feas_stale(p_spec, "p1")))
  expect_null(sr_feasibility(p_spec, "p1")$decision)
  # a data change, last because it changes the store every object shares: one outcome value re-imported
  df <- .ft_frame()
  i <- which(df$y == "dead")[1L]
  df$y[i] <- "alive"
  csv <- file.path(dirname(p$paths$dir), "t2.csv")
  .fx_write_csv(df, csv)
  p_data <- sr_ingest(p, csv, table = "t", options = list(drive_info = .ft_drive))
  expect_identical(.sr_feas_stale(p_data, "p1"), "data")
  expect_identical(sr_feasibility(p_data, "p1")$stale, "data")
  expect_error(sr_feasibility_decide(p_data, "p1", "proceed", reason = "ok"), "stale \\(the data changed\\)")
  expect_identical(sr_feasibility(p_data, "p1")$decision$decision, "proceed")   # as recorded
})

test_that("a clustered analysis needs an icc; few clusters warn", {
  spec <- .ft_spec(se = "cluster", cluster = "cl", layers = .ft_layers(clustered = TRUE))
  p <- .ft_project(spec = spec)
  expect_error(sr_feasibility_assess(p, "p1"), "a clustered analysis needs assumptions\\$icc \\(value and justification\\)")
  p <- sr_feasibility_assess(p, "p1", assumptions = list(icc = list(value = 0.05, justification = "pilot")))
  as <- sr_feasibility(p, "p1")$assessment
  expect_true(any(grepl("fewer than 30 clusters \\(12\\)", as$warnings)))
  expect_identical(as$assumptions$icc, list(value = 0.05, justification = "pilot"))
  expect_identical(as$inputs$clusters$G, 12L)
  expect_true(all(as$contrasts$deff > 1))
  sr_project_close(p)
  p2 <- sr_project_open(p$paths$dir)
  withr::defer(sr_project_close(p2))
  expect_identical(p2$state$feasibility, p$state$feasibility)
})

test_that("a table1 primary is not applicable and can proceed without a reason", {
  p <- .fx_ready_project(.ft_frame(), "t", .ft_roles)
  p <- sr_sap_add(p, "p1", "table1", list(purpose = "describe", by = "x", vars = c("z1", "z2"), seed = 1L,
                                           layers = .ft_layers()), "t", label = "primary")
  p <- sr_feasibility_assess(p, "p1")
  as <- sr_feasibility(p, "p1")$assessment
  expect_identical(as$tier, "not_applicable")
  expect_identical(nrow(as$contrasts), 0L)
  expect_null(as$inputs$outcome)
  p <- sr_feasibility_decide(p, "p1", "proceed")
  expect_identical(sr_feasibility(p, "p1")$decision$decision, "proceed")
  expect_identical(.sr_feas_gate(p), character())
  rep <- sr_feasibility_report(p)
  expect_identical(nrow(rep), 1L)
  expect_true(is.na(rep$contrast) && is.na(rep$se))
  expect_identical(rep$tier, "not_applicable")
  sr_project_close(p)
  p2 <- sr_project_open(p$paths$dir)
  withr::defer(sr_project_close(p2))
  expect_identical(p2$state$feasibility, p$state$feasibility)
})

test_that("assess and decide are refused on a read-only or locked project", {
  p <- .ft_project()
  ro <- p
  ro$readonly <- TRUE
  expect_error(sr_feasibility_assess(ro, "p1"), "read-only")
  expect_error(sr_feasibility_decide(ro, "p1", "stop", reason = "x"), "read-only")
  p <- sr_feasibility_assess(p, "p1")
  lk <- .fx_unblind(p)
  expect_error(sr_feasibility_assess(lk, "p1"), "the SAP is locked: the feasibility verdict is frozen")
  expect_error(sr_feasibility_decide(lk, "p1", "stop", reason = "x"), "the SAP is locked: the feasibility verdict is frozen")
  expect_error(sr_feasibility_assess(p, "zz"), "unknown analysis 'zz'")
})

test_that("the report and the fragment read the stored verdict and are outcome-blind", {
  p <- .ft_project()
  expect_identical(nrow(sr_feasibility_report(p)), 0L)
  expect_error(sr_feasibility_fragment(p, "p1"), "no feasibility assessment")
  p <- sr_feasibility_assess(p, "p1", delta = .ft_delta())
  p <- sr_feasibility_decide(p, "p1", "proceed", reason = "limited precision is stated")
  rep <- sr_feasibility_report(p)
  expect_named(rep, c("analysis", "label", "route", "contrast", "measure", "n_cc", "events", "prevalence", "se",
                      "h", "mde", "se_upper", "delta", "delta_measure", "delta_source", "tier", "decision",
                      "reason", "stale", "blockers", "warnings"))
  as <- sr_feasibility(p, "p1")$assessment
  expect_identical(nrow(rep), nrow(as$contrasts))
  expect_identical(rep$measure, c("OR", "RD", "RR"))
  expect_identical(rep$analysis, rep("p1", 3L))
  expect_identical(rep$label, rep("primary", 3L))
  expect_identical(rep$n_cc, rep(as$inputs$n_cc, 3L))
  expect_equal(rep$events, rep(as$inputs$outcome$events_cc, 3L))
  # R12: prevalence is the exposure prevalence (share of complete cases with x = 1), not the outcome risk
  df <- .ft_frame()
  cc <- stats::complete.cases(df[c("y", "x", "z1", "z2")])
  expect_identical(as$inputs$n_cc, sum(cc))
  want <- sum(df$x[cc] == 1) / sum(cc)
  expect_equal(rep$prevalence, rep(want, 3L))
  expect_false(isTRUE(all.equal(want, as$inputs$outcome$risk)))
  expect_identical(rep$decision, rep("proceed", 3L))
  expect_true(all(is.na(rep$stale)))
  expect_identical(rep$delta_measure, rep("OR", 3L))
  expect_identical(rep$tier[rep$measure == "RD"], "precision_only")

  fr <- sr_feasibility_fragment(p, "p1")
  expect_identical(fr$template, "feasibility_methods")
  expect_named(fr$values, c("n", "events", "prevalence", "h", "mde", "delta", "delta_source", "tier"))
  expect_identical(fr$values$n, as$inputs$n_cc)
  expect_identical(names(fr$values$prevalence), c("0", "1"))
  expect_equal(fr$values$prevalence[["1"]], want)
  expect_equal(fr$values$prevalence[["0"]], 1 - want)
  expect_identical(fr$values$tier, as$tier)
  expect_identical(fr$values$delta, 1.5)
  expect_identical(fr$values$mde, as$contrasts$mde[as$contrasts$measure == "OR"])
  expect_identical(fr$values$h, as$contrasts$h[as$contrasts$measure == "OR"])

  spec <- .sr_spec_from_state(p$state$analyses$p1$spec)
  oc <- "y"
  oth <- setdiff(sr_spec_cols(spec), oc)
  expect_silent(.sr_assert_blind(rep, oc, oth))
  expect_silent(.sr_assert_blind(fr, oc, oth))
  expect_silent(.sr_assert_blind(as, oc, oth))
})

test_that("the lock gate lists the primary problems and every stop (R7)", {
  p <- .ft_project()
  p <- sr_sap_add(p, "s1", "logistic", .ft_spec(terms = list(list(var = "z3"))), "t", label = "secondary")
  expect_identical(.sr_feas_gate(p), "p1: no feasibility assessment")
  p <- sr_feasibility_assess(p, "p1")
  expect_identical(.sr_feas_gate(p), "p1: no feasibility decision")
  p <- sr_feasibility_decide(p, "p1", "revise", reason = "try fewer terms")
  expect_identical(.sr_feas_gate(p), "p1: decision is revise: change the design, assess and decide again")
  p <- sr_feasibility_assess(p, "p1")
  p <- sr_feasibility_decide(p, "p1", "exploratory", reason = "hypothesis generating")
  expect_identical(.sr_feas_gate(p), character())

  # a secondary is never gated, except that a stop on it blocks too (R7)
  p <- sr_feasibility_assess(p, "s1")
  expect_identical(.sr_feas_gate(p), character())
  p <- sr_feasibility_decide(p, "s1", "stop", reason = "no events")
  expect_identical(.sr_feas_gate(p), "s1: decision is stop (no events)")
  expect_identical(sr_project_stopped(p)$analysis, "s1")
  p <- sr_feasibility_decide(p, "s1", "revise", reason = "reversed")
  expect_identical(.sr_feas_gate(p), character())

  # stale and stop on the primary: both are reported, the stale one first
  p <- sr_feasibility_decide(p, "p1", "stop", reason = "design is not viable")
  expect_identical(.sr_feas_gate(p), "p1: decision is stop (design is not viable)")
  p <- sr_sap_add(p, "p1", "logistic", .ft_spec(terms = list(list(var = "z1"), list(var = "z2"), list(var = "z3"))),
                  "t", label = "primary")
  expect_identical(.sr_feas_gate(p), c("p1: the feasibility assessment is stale (the spec changed)",
                                       "p1: decision is stop (design is not viable)"))
  p <- sr_feasibility_decide(p, "p1", "revise", reason = "added z3")
  expect_identical(.sr_feas_gate(p), "p1: the feasibility assessment is stale (the spec changed)")
})

test_that("survival, rmst and continuous analyses replay identically and stay outcome-blind", {
  n <- 300L
  df <- withr::with_seed(7L, {
    z1 <- stats::rnorm(n)
    x <- stats::rbinom(n, 1L, stats::plogis(z1))
    t_ev <- stats::rexp(n, 0.1 * exp(0.4 * x))
    t_c <- stats::runif(n, 0, 15)
    data.frame(id = seq_len(n), time = round(pmin(t_ev, t_c), 3), status = as.integer(t_ev <= t_c), x = x,
               z1 = z1, bp = 120 + 10 * z1 + stats::rnorm(n, 0, 8), stringsAsFactors = FALSE)
  })
  p <- .fx_ready_project(df, "t", c(id = "id", bp = "outcome", x = "exposure"))
  lay <- .ft_layers()
  p <- sr_sap_add(p, "cx", "cox", list(purpose = "explain-causal", time = "time", status = "status", status_event = 1,
                                       exposure = "x", terms = list(list(var = "z1")), layers = lay, seed = 1L),
                  "t", label = "primary")
  p <- sr_sap_add(p, "rm", "rmst", list(purpose = "explain-causal", time = "time", status = "status", status_event = 1,
                                        exposure = "x", tau = 8, layers = lay, seed = 1L), "t", label = "secondary")
  p <- sr_sap_add(p, "bp", "lm", list(purpose = "explain-causal", outcome = "bp", exposure = "x",
                                      terms = list(list(var = "z1")), layers = lay, seed = 1L), "t", label = "secondary")
  withr::local_options(sr.feas_rmst_sims = 20)
  for (id in c("cx", "rm", "bp")) p <- sr_feasibility_assess(p, id)
  expect_identical(sr_feasibility(p, "cx")$assessment$contrasts$measure, "HR")
  expect_identical(sr_feasibility(p, "rm")$assessment$contrasts$measure, "RMST_diff")
  expect_identical(sr_feasibility(p, "bp")$assessment$contrasts$measure, "MD")
  expect_null(sr_feasibility(p, "cx")$assessment$inputs$outcome$pairs)
  rep <- sr_feasibility_report(p)
  expect_identical(rep$analysis, c("bp", "cx", "rm"))
  expect_true(all(is.finite(rep$se)))
  expect_equal(rep$prevalence[rep$analysis == "cx"], mean(df$x == 1))
  expect_equal(rep$prevalence[rep$analysis == "rm"], mean(df$x == 1))
  expect_equal(sr_feasibility_fragment(p, "cx")$values$prevalence[["1"]], mean(df$x == 1))
  expect_equal(rep$prevalence[rep$analysis == "bp"], mean(df$x == 1))
  expect_silent(.sr_assert_blind(rep, c("time", "status", "bp"), "z1"))
  # the RMST simulation count and seed are recorded with the rmst assessment only (R18)
  expect_identical(sr_feasibility(p, "rm")$assessment$inputs$rmst, list(sims = 20L, seed = 1L))
  expect_null(sr_feasibility(p, "cx")$assessment$inputs$rmst)
  expect_null(sr_feasibility(p, "bp")$assessment$inputs$rmst)
  p <- sr_feasibility_decide(p, "cx", "proceed", reason = "precision reported")
  sr_project_close(p)
  p2 <- sr_project_open(p$paths$dir)
  withr::defer(sr_project_close(p2))
  expect_identical(p2$state$feasibility, p$state$feasibility)
  expect_identical(sr_feasibility(p2, "rm")$assessment$inputs$rmst, list(sims = 20L, seed = 1L))
})

test_that("exposure prevalence (R12): a factor exposure gives every level's share, a continuous one NA", {
  df <- .ft_frame()
  # factor exposure: site against reference "a"
  p <- .ft_project(df, spec = .ft_spec(exposure = "site", exposure_ref = "a",
                                       terms = list(list(var = "z1", type = "linear"))))
  p <- sr_feasibility_assess(p, "p1")
  a <- sr_feasibility(p, "p1")$assessment
  expect_identical(a$inputs$exposure$type, "factor")
  cc <- stats::complete.cases(df[c("y", "site", "z1")])
  sh <- table(df$site[cc]) / sum(cc)
  rep <- sr_feasibility_report(p)
  expect_identical(a$inputs$n_cc, sum(cc))
  for (l in c("b", "c")) {
    expect_equal(unique(rep$prevalence[grepl(paste0(": ", l, " vs a"), rep$contrast, fixed = TRUE)]),
                 unname(sh[[l]]))
  }
  fr <- sr_feasibility_fragment(p, "p1")
  expect_identical(names(fr$values$prevalence), c("a", "b", "c"))
  expect_equal(unlist(fr$values$prevalence, use.names = FALSE), as.numeric(sh))
  expect_equal(sum(unlist(fr$values$prevalence)), 1)
  expect_silent(.sr_assert_blind(fr, "y", c("site", "z1")))

  # continuous exposure: NA in the report and the fragment, and a replay-identical state
  p <- .ft_project(df, spec = .ft_spec(exposure = "z3", terms = list(list(var = "z1", type = "linear"))))
  p <- sr_feasibility_assess(p, "p1")
  a <- sr_feasibility(p, "p1")$assessment
  expect_identical(a$inputs$exposure$type, "linear")
  rep <- sr_feasibility_report(p)
  expect_true(nrow(rep) > 0L)
  expect_true(all(is.na(rep$prevalence)))
  expect_identical(sr_feasibility_fragment(p, "p1")$values$prevalence, NA_real_)
  sr_project_close(p)
  p2 <- sr_project_open(p$paths$dir)
  withr::defer(sr_project_close(p2))
  expect_identical(p2$state$feasibility, p$state$feasibility)

  # no complete case: NA (z1 is missing in the first half of the rows, z2 in the second)
  nc <- df
  nc$z1[seq_len(250L)] <- NA
  nc$z2[251:500] <- NA
  p <- .ft_project(nc)
  p <- sr_feasibility_assess(p, "p1")
  expect_identical(sr_feasibility(p, "p1")$assessment$inputs$n_cc, 0L)
  expect_true(all(is.na(sr_feasibility_report(p)$prevalence)))
  expect_identical(sr_feasibility_fragment(p, "p1")$values$prevalence, NA_real_)
})

# ---- the lock freezes the verdict, the hash covers it, a stop is sticky ---------------------------------

# The logistic project, assessed and decided proceed, and locked.
.ft_locked <- function(env = parent.frame()) {
  p <- .ft_project(env = env)
  p <- sr_feasibility_assess(p, "p1", delta = .ft_delta())
  p <- sr_feasibility_decide(p, "p1", "proceed", reason = "adequate for the pre-specified contrast")
  sr_sap_lock(p)
}

test_that("after the lock the verdict is frozen: no assess, no decide, no engine call", {
  # a counter bound into the tracer (the tracer runs in the traced function's frame, not here)
  seen <- new.env(parent = emptyenv())
  seen$n <- 0L
  tick <- function() seen$n <- seen$n + 1L
  suppressMessages(trace(".sr_feas_compute", tracer = bquote(.(tick)()), print = FALSE, where = globalenv()))
  withr::defer(suppressMessages(untrace(".sr_feas_compute", where = globalenv())))

  # positive control: assessing an unlocked project does call the engine
  p <- .ft_project()
  p <- sr_feasibility_assess(p, "p1", delta = .ft_delta())
  expect_gt(seen$n, 0L)
  p <- sr_feasibility_decide(p, "p1", "proceed", reason = "adequate for the pre-specified contrast")
  p <- sr_sap_lock(p)
  seen$n <- 0L

  expect_identical(sr_sap_hash(p), p$state$sap$lock$sap_hash)
  expect_error(sr_feasibility_assess(p, "p1"), "frozen", fixed = TRUE)
  expect_error(sr_feasibility_decide(p, "p1", "stop", reason = "late"), "frozen", fixed = TRUE)
  f <- sr_feasibility(p, "p1")
  r <- sr_feasibility_report(p)
  expect_identical(seen$n, 0L)
  expect_identical(f$decision$decision, "proceed")
  expect_true(is.na(f$stale))
  expect_identical(r$decision, rep("proceed", nrow(r)))
})

test_that("the SAP hash covers the feasibility record, and only when there is one", {
  p <- .ft_locked()
  h <- sr_sap_hash(p)
  q <- p
  q$state$feasibility$p1$decision$reason <- "another reason"
  expect_false(identical(sr_sap_hash(q), h))
  q <- p
  q$state$feasibility$p1$decision$decision <- "exploratory"
  expect_false(identical(sr_sap_hash(q), h))
  q <- p
  q$state$feasibility$p1$assessment$tier <- "adequate"
  expect_false(identical(sr_sap_hash(q), h))
  # a feasibility record of a removed analysis is not part of the SAP
  q <- p
  q$state$feasibility$gone <- q$state$feasibility$p1
  expect_identical(sr_sap_hash(q), h)

  # a SAP locked before P1c.1 (no feasibility records) keeps the hash it had: the element is absent
  q <- p
  q$state$feasibility <- NULL
  legacy <- .sr_sha256(sr_canonical_json(list(
    analyses = unname(lapply(.sr_sap_analyses(q), identity)),
    families = list(), rules = list())))
  expect_identical(sr_sap_hash(q), legacy)
  q$state$feasibility <- list()
  expect_identical(sr_sap_hash(q), legacy)
})

test_that("a stopped analysis cannot be removed; only a recorded reversal frees it", {
  p <- .ft_project()
  p <- sr_sap_add(p, "s1", "logistic", .ft_spec(), "t", label = "sensitivity", parent = "p1")
  p <- sr_feasibility_assess(p, "s1")
  p <- sr_feasibility_decide(p, "s1", "stop", reason = "too few events")
  expect_error(sr_sap_remove(p, "s1"), "reverse the stop with sr_feasibility_decide first", fixed = TRUE)
  expect_match(tryCatch(sr_sap_remove(p, "s1"), error = conditionMessage), "s1", fixed = TRUE)
  # another analysis' stop does not stop the removal of an analysis that is not stopped
  p <- sr_sap_add(p, "s2", "logistic", .ft_spec(), "t", label = "sensitivity", parent = "p1")
  expect_null(sr_sap_remove(p, "s2")$state$analyses$s2)
  p <- sr_feasibility_decide(p, "s1", "revise", reason = "reversing the stop")
  expect_null(sr_sap_remove(p, "s1")$state$analyses$s1)
})

test_that("sr_run on a stopped project is refused before anything is extracted", {
  p <- .ft_locked()
  s <- p
  s$state$feasibility$p1$decision <- list(decision = "stop", reason = "no power")
  expect_identical(sr_project_stopped(s)$analysis, "p1")
  ex_dir <- file.path(s$paths$dir, "data", "extracts")
  expect_error(sr_run(s, "p1"), "the project is stopped: no power", fixed = TRUE)
  expect_identical(list.files(ex_dir), character())
  # positive control: the same locked project without the stop does write an extract there
  ran <- sr_run(p, "p1")
  expect_true(dir.exists(ex_dir))
  expect_gt(length(list.files(ex_dir)), 0L)
  expect_true(ran$state$runs$p1$ok)
})

test_that("a stale secondary does not block the lock; a secondary decided stop does (F2)", {
  # the gate is primary-only for missing / stale / undecided / revise (design catalogue O); only a stop
  # on any analysis blocks
  p <- .ft_project()
  p <- sr_sap_add(p, "s1", "logistic", .ft_spec(terms = list(list(var = "z3"))), "t", label = "secondary")
  p <- sr_feasibility_assess(p, "p1")
  p <- sr_feasibility_decide(p, "p1", "proceed", reason = "ok")
  p <- sr_feasibility_assess(p, "s1")
  p <- sr_feasibility_decide(p, "s1", "proceed", reason = "ok")
  p <- sr_sap_add(p, "s1", "logistic", .ft_spec(terms = list(list(var = "z1"), list(var = "z3"))), "t",
                  label = "secondary")
  expect_identical(.sr_feas_stale(p, "s1"), "spec")
  expect_identical(.sr_feas_gate(p), character())
  locked <- sr_sap_lock(p)
  expect_true(isTRUE(locked$state$sap$locked))

  # the same stale secondary, now decided stop before the spec change: the stop blocks the lock
  q <- .ft_project()
  q <- sr_sap_add(q, "s1", "logistic", .ft_spec(terms = list(list(var = "z3"))), "t", label = "secondary")
  q <- sr_feasibility_assess(q, "p1")
  q <- sr_feasibility_decide(q, "p1", "proceed", reason = "ok")
  q <- sr_feasibility_assess(q, "s1")
  q <- sr_feasibility_decide(q, "s1", "stop", reason = "no events")
  expect_identical(.sr_feas_gate(q), "s1: decision is stop (no events)")
  expect_error(sr_sap_lock(q), "s1: decision is stop (no events)", fixed = TRUE)
})

test_that("when both the data and the spec changed, the stale reason names the data (F3)", {
  p <- .ft_project()
  p <- sr_feasibility_assess(p, "p1")
  p <- sr_sap_add(p, "p1", "logistic", .ft_spec(terms = list(list(var = "z1"), list(var = "z2"), list(var = "z3"))),
                  "t", label = "primary")
  expect_identical(.sr_feas_stale(p, "p1"), "spec")
  df <- .ft_frame()
  df$y[which(df$y == "dead")[1L]] <- "alive"
  csv <- file.path(dirname(p$paths$dir), "t2.csv")
  .fx_write_csv(df, csv)
  p <- sr_ingest(p, csv, table = "t", options = list(drive_info = .ft_drive))
  expect_identical(.sr_feas_stale(p, "p1"), "data")
  expect_identical(sr_feasibility(p, "p1")$stale, "data")
  expect_error(sr_feasibility_decide(p, "p1", "proceed", reason = "ok"), "stale (the data changed)", fixed = TRUE)
})

test_that("the SAP hash of a project without feasibility records is pinned (F4)", {
  # a hand-built SAP state: a primary, a secondary with a sap_label, a non-pre-specified analysis (left
  # out), a family and a rule. The literal was checked against sr_sap_hash as it was before P1c.1 (no
  # feasibility element); a change that alters the hash of such a SAP must fail here.
  rec <- function(id, label, prespecified = TRUE, sap_label = NULL) {
    r <- list(id = id, route = "logistic", route_version = 1L, table = "t", label = label, family = NA_character_,
              spec = list(purpose = "explain-causal", outcome = "y", event_value = "dead", exposure = "x",
                          terms = list(list(var = "z1", type = "linear")), seed = 1L),
              parent = NA_character_, trigger = NA_character_, addresses = character(),
              prespecified = prespecified, reason = NA_character_)
    if (!is.null(sap_label)) r$sap_label <- sap_label
    r
  }
  p <- list(state = list(
    analyses = list(s1 = rec("s1", "secondary", sap_label = "sensitivity"), p1 = rec("p1", "primary"),
                    a9 = rec("a9", "secondary", prespecified = FALSE)),
    sap = list(families = list(f1 = list(id = "f1", members = c("p1", "s1"), method = "holm")),
               rules = list(r1 = list(id = "r1", analysis = "p1", check = "ph", action = "add_sensitivity",
                                      alternative = "s1")))))
  legacy <- "b11d9a49cfbb0f4146c4f9313af6c477464823738aca3b99fe57c571c15c4bb4"
  expect_identical(sr_sap_hash(p), legacy)
  p$state$feasibility <- list()
  expect_identical(sr_sap_hash(p), legacy)
  p$state$feasibility <- list(gone = list(assessment = list(tier = "adequate")))   # not an analysis of the SAP
  expect_identical(sr_sap_hash(p), legacy)
})

test_that("assumptions given as NULL are absent ones: r2_y = NULL warns and is not stored (F9)", {
  n <- 200L
  df <- withr::with_seed(7L, {
    z1 <- stats::rnorm(n)
    x <- stats::rbinom(n, 1L, stats::plogis(z1))
    data.frame(id = seq_len(n), x = x, z1 = z1, bp = 120 + 10 * z1 + stats::rnorm(n, 0, 8),
               stringsAsFactors = FALSE)
  })
  p <- .fx_ready_project(df, "t", c(id = "id", bp = "outcome", x = "exposure"))
  p <- sr_sap_add(p, "bp", "lm", list(purpose = "explain-causal", outcome = "bp", exposure = "x",
                                      terms = list(list(var = "z1")), layers = .ft_layers(), seed = 1L),
                  "t", label = "primary")
  w <- paste0("r2_y was not given: the MD standard error uses r2_y = 0 (the covariates are assumed to explain ",
              "none of the outcome variance, which is conservative)")
  a <- sr_feasibility(sr_feasibility_assess(p, "bp"), "bp")$assessment
  b <- sr_feasibility(sr_feasibility_assess(p, "bp", assumptions = list(r2_y = NULL)), "bp")$assessment
  expect_true(w %in% a$warnings)
  expect_identical(b, a)
  expect_identical(b$assumptions$r2_y, list(value = NA_real_, justification = NA_character_))
  g <- sr_feasibility(sr_feasibility_assess(p, "bp", assumptions = list(
    r2_y = list(value = 0.3, justification = "prior cohort"), icc = NULL)), "bp")$assessment
  expect_false(w %in% g$warnings)
  expect_identical(g$assumptions$r2_y$value, 0.3)
})
