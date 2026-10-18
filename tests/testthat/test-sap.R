# Tests for sap/sap.R: SAP drafting events, the hash and the lock. Synthetic data only (survival::lung
# reshaped); everything lives under withr::local_tempdir().

# A lung-derived analysis frame. wt_loss carries one block (range) and one warn (heaping) finding; the
# suggested action for the heaping finding is keep_sensitivity.
.sap_frame <- function() {
  lung <- survival::lung
  data.frame(id = seq_len(nrow(lung)), time = lung$time, died = as.integer(lung$status == 2L),
             age = lung$age, sex = ifelse(lung$sex == 1L, "M", "F"), ecog = lung$ph.ecog,
             wt_loss = lung$wt.loss, stringsAsFactors = FALSE)
}

.sap_roles <- c(id = "id", time = "time", died = "outcome", sex = "exposure")

.sap_drive <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())

# A ready project on the lung frame.
.sap_project <- function(env = parent.frame()) {
  .fx_ready_project(.sap_frame(), "t", .sap_roles, env = env)
}

# Register a stub route (purpose explain-causal, schema: outcome chr required) for the calling test.
.sap_route <- function(id = "stub_sap", env = parent.frame()) {
  est <- sr_estimand(population = "adults", variables = "died", intercurrent = "none",
                     summary = "stub", level = "marginal")
  r <- sr_route(id = id, version = 1L, family = "stub", purpose = "explain-causal", estimand = est,
                spec_schema = list(outcome = list(type = "chr", required = TRUE),
                                   exposure = list(type = "chr", required = FALSE)),
                compile = function(spec) "1", present = function(result, spec) list(text = "stub"))
  sr_route_register(r)
  withr::defer(rm(list = .sr_route_key(id, 1L), envir = .sr_routes), envir = env)
  invisible(r)
}

.sap_spec <- function(outcome = "died", ...) {
  layers <- list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                 framework = "frequentist", clustered = FALSE)
  c(list(purpose = "explain-causal", outcome = outcome, layers = layers), list(...))
}

# A project with the stub route registered and a primary analysis a1 and a secondary a2 added.
.sap_drafted <- function(env = parent.frame()) {
  .sap_route(env = env)
  p <- .sap_project(env = env)
  p <- sr_sap_add(p, "a1", "stub_sap", .sap_spec(), "t", label = "primary")
  sr_sap_add(p, "a2", "stub_sap", .sap_spec(exposure = "sex"), "t", label = "secondary")
}

# Replace a function of the loaded core for the calling test (the core is sourced into the global env).
.sap_mock <- function(name, fn, env = parent.frame()) {
  old <- get(name, envir = globalenv())
  assign(name, fn, envir = globalenv())
  withr::defer(assign(name, old, envir = globalenv()), envir = env)
  invisible(old)
}

.sap_heaping_id <- function(p) {
  f <- sr_dq_findings(p, "t")
  f$id[f$check == "heaping" & f$severity == "warn"][1L]
}

# ---- constants and helpers ---------------------------------------------------------------------------

test_that("SR_DIAG_IDS is the literal list of diagnostic ids", {
  expect_identical(SR_DIAG_IDS, c("ph", "influence", "separation", "few_clusters", "inf_censoring",
                                  "linearity", "heteroscedasticity"))
})

test_that(".sr_mem_method maps a route id to an sr_mem_estimate method", {
  expect_identical(.sr_mem_method(c("lm_fit", "lm", "lmm")), rep("lm", 3L))
  expect_identical(.sr_mem_method(c("logistic", "modpois")), rep("glm", 2L))
  expect_identical(.sr_mem_method(c("cox", "rmst")), rep("cox", 2L))
  expect_identical(.sr_mem_method(c("table1", "stub_sap", "anything")), rep("lm", 3L))
  expect_identical(.sr_mem_method("cox"), "cox")
})

# ---- drafting ---------------------------------------------------------------------------------------

test_that("the analysis record has the one contract shape and sr_sap_add emits sap.analysis_set", {
  .sap_route()
  p <- .sap_project()
  p <- sr_sap_add(p, "a1", "stub_sap", .sap_spec(), "t", label = "primary")
  shape <- c("id", "route", "route_version", "table", "label", "family", "spec", "parent",
             "trigger", "addresses", "prespecified", "reason")
  expect_identical(names(p$state$analyses$a1), shape)
  expect_true(is.na(p$state$analyses$a1$trigger))
  expect_true(is.na(p$state$analyses$a1$reason))
  types <- vapply(sr_events_read(p$paths$events), function(e) e$type, "")
  expect_true("sap.analysis_set" %in% types)
  p <- sr_sap_family(p, "f1", "a1")                           # joining a family keeps the shape
  expect_identical(names(p$state$analyses$a1), shape)
})

test_that("sr_sap_add records a pre-specified analysis", {
  .sap_route()
  p <- .sap_project()
  p <- sr_sap_add(p, "a1", "stub_sap", .sap_spec(), "t", label = "primary")
  a <- p$state$analyses$a1
  expect_true(a$prespecified)
  expect_identical(a$label, "primary")
  expect_identical(a$table, "t")
  expect_identical(a$route, "stub_sap")
  expect_identical(a$route_version, 1L)
  expect_identical(a$spec$outcome, "died")
  expect_true(is.na(a$family))
  expect_true(is.na(a$parent))
  expect_identical(a$addresses, character())
  expect_true(sr_blind(p))                                    # drafting does not unblind
  # re-adding an id replaces it
  p <- sr_sap_add(p, "a1", "stub_sap", .sap_spec(exposure = "sex"), "t", label = "secondary")
  expect_identical(names(p$state$analyses), "a1")
  expect_identical(p$state$analyses$a1$label, "secondary")
  expect_identical(p$state$analyses$a1$spec$exposure, "sex")
})

test_that("sr_sap_add refuses what cannot be a pre-specified analysis, with the reason", {
  .sap_route()
  p <- .sap_project()
  ok <- .sap_spec()
  expect_error(sr_sap_add(p, "a1", "no_such_route", ok, "t"), "no_such_route")
  expect_error(sr_sap_add(p, "a1", "stub_sap", .sap_spec(outcome = NULL), "t"), "outcome")
  bad_purpose <- ok
  bad_purpose$purpose <- "describe"
  expect_error(sr_sap_add(p, "a1", "stub_sap", bad_purpose, "t"), "purpose")
  expect_error(sr_sap_add(p, "a1", "stub_sap", ok, "no_such_table"), "no_such_table")
  stack <- .sap_spec()
  stack$layers$framework <- "bayesian"
  stack$layers$resampling <- "bootstrap"
  expect_error(sr_sap_add(p, "a1", "stub_sap", stack, "t"), "bayes_x_boot")
  expect_error(sr_sap_add(p, "a1", "stub_sap", ok, "t", parent = "nope"), "nope")
  expect_error(sr_sap_add(p, "a1", "stub_sap", ok, "t", parent = "a1"), "itself")
  expect_error(sr_sap_add(p, "", "stub_sap", ok, "t"), "id")
  expect_error(sr_sap_add(p, "a1", "stub_sap", ok, "t", label = "tertiary"))
  expect_error(sr_sap_add(p, "a1", "stub_sap", ok, "t", label = "primary", addresses = "x"), "sensitivity")
  expect_error(sr_sap_add(p, "a1", "stub_sap", ok, "t", version = 7L), "version")
  expect_length(p$state$analyses, 0L)                          # nothing was written by any refusal
})

test_that("sr_sap_add refuses an id that does not match SR_ANALYSIS_ID_RE", {
  .sap_route()
  p <- .sap_project()
  expect_error(sr_sap_add(p, "Bad Id With Spaces!", "stub_sap", .sap_spec(), "t"), "must match", fixed = TRUE)
  expect_error(sr_sap_add(p, "1a", "stub_sap", .sap_spec(), "t"), "must match", fixed = TRUE)
  expect_error(sr_sap_add(p, strrep("a", 41L), "stub_sap", .sap_spec(), "t"), "must match", fixed = TRUE)
  expect_length(p$state$analyses, 0L)
  p <- sr_sap_add(p, strrep("a", 40L), "stub_sap", .sap_spec(), "t")
  expect_length(p$state$analyses, 1L)
})

test_that("sr_sap_add refuses a spec the route cannot compile, naming the analysis and the reason", {
  est <- sr_estimand(population = "adults", variables = "died", intercurrent = "none",
                     summary = "stub", level = "marginal")
  r <- sr_route(id = "stub_nocompile", version = 1L, family = "stub", purpose = "explain-causal", estimand = est,
                spec_schema = list(outcome = list(type = "chr", required = TRUE)),
                compile = function(spec) stop("stub_nocompile@1 does not implement the layer(s): weights = ipw"),
                present = function(result, spec) list(text = "stub"))
  sr_route_register(r)
  withr::defer(rm(list = .sr_route_key("stub_nocompile", 1L), envir = .sr_routes))
  p <- .sap_project()
  s <- .sap_spec()
  s$layers$weights <- "ipw"
  e <- tryCatch(sr_sap_add(p, "w1", "stub_nocompile", s, "t"), error = function(e) conditionMessage(e))
  expect_match(e, "analysis 'w1' cannot run on route 'stub_nocompile'", fixed = TRUE)
  expect_match(e, "weights = ipw", fixed = TRUE)
  expect_length(p$state$analyses, 0L)
})

test_that("a binary outcome with no event value must be 0/1 or logical: refused at add, blocked at the lock", {
  est <- sr_estimand(population = "adults", variables = "died", intercurrent = "none",
                     summary = "stub", level = "marginal")
  r <- sr_route(id = "stub_bin", version = 1L, family = "stub", purpose = "explain-causal", estimand = est,
                spec_schema = list(outcome = list(type = "chr", required = TRUE),
                                   event_value = list(type = "chr", required = FALSE)),
                compile = function(spec) "1", present = function(result, spec) list(text = "stub"))
  sr_route_register(r)
  withr::defer(rm(list = .sr_route_key("stub_bin", 1L), envir = .sr_routes))
  p <- .sap_project()
  # died is 0/1 (fine); sex is "M"/"F" (needs an event value); ecog is 0-3 (needs one too)
  p <- sr_sap_add(p, "b1", "stub_bin", .sap_spec(), "t", label = "primary")
  e <- tryCatch(sr_sap_add(p, "b2", "stub_bin", .sap_spec(outcome = "sex"), "t"), error = function(e) conditionMessage(e))
  expect_match(e, "b2", fixed = TRUE)
  expect_match(e, "set event_value (status_event for a survival status)", fixed = TRUE)
  expect_false(grepl("\"M\"|'M'", e))                                            # no data value in the message
  expect_error(sr_sap_add(p, "b3", "stub_bin", .sap_spec(outcome = "ecog"), "t"), "set event_value", fixed = TRUE)
  p <- sr_sap_add(p, "b2", "stub_bin", .sap_spec(outcome = "sex", event_value = "M"), "t")
  expect_setequal(names(p$state$analyses), c("b1", "b2"))
  ck <- sr_sap_check(p)
  expect_false("event value" %in% ck$item)
  # an analysis that reached the SAP without the check (data re-imported after the add) blocks the lock
  p <- sr_project_append(p, "sap.analysis_set", list(
    id = "b4", route = "stub_bin", route_version = 1L, spec = .sap_spec(outcome = "sex"), table = "t",
    label = "secondary", family = NA_character_, parent = NA_character_, addresses = character()))
  ck <- sr_sap_check(p)
  row <- ck[ck$item == "event value", , drop = FALSE]
  expect_identical(nrow(row), 1L)
  expect_identical(row$status, "block")
  expect_match(row$detail, "b4", fixed = TRUE)
  expect_error(sr_sap_lock(p), "set event_value", fixed = TRUE)
})

test_that("an unresolved choice in the layer stack is refused too", {
  .sap_route()
  p <- .sap_project()
  s <- .sap_spec()
  s$layers$missing <- "mi"
  s$layers$resampling <- "bootstrap"                            # mi x bootstrap is a choice
  expect_error(sr_sap_add(p, "a1", "stub_sap", s, "t"), "boot")
})

test_that("sr_sap_remove drops an analysis, its family membership and its rules, before the lock", {
  p <- .sap_drafted()
  p <- sr_sap_add(p, "s1", "stub_sap", .sap_spec(), "t", label = "sensitivity", parent = "a1")
  p <- sr_sap_family(p, "f1", c("a1", "a2"))
  p <- sr_sap_rule(p, "r1", "a1", "ph", "add_sensitivity", "s1")
  expect_error(sr_sap_remove(p, "a1"), "parent")               # s1 hangs off a1
  p <- sr_sap_remove(p, "s1")
  expect_false("s1" %in% names(p$state$analyses))
  expect_length(p$state$sap$rules, 0L)                         # the rule named s1
  p <- sr_sap_remove(p, "a2")
  expect_identical(p$state$sap$families$f1$members, "a1")
  expect_error(sr_sap_remove(p, "zzz"), "zzz")
})

test_that("sr_sap_family sets a multiplicity family of primary and secondary analyses", {
  p <- .sap_drafted()
  p <- sr_sap_add(p, "s1", "stub_sap", .sap_spec(), "t", label = "sensitivity", parent = "a1")
  expect_error(sr_sap_family(p, "f1", c("a1", "s1")), "not an inferential family")
  expect_error(sr_sap_family(p, "f1", c("a1", "nope")), "nope")
  expect_error(sr_sap_family(p, "f1", character()), "members")
  expect_error(sr_sap_family(p, "f1", c("a1", "a1")), "twice|duplicate")
  expect_error(sr_sap_family(p, "f1", "a1", method = "sidak"))
  p <- sr_sap_family(p, "f1", c("a1", "a2"))
  expect_identical(p$state$sap$families$f1$members, c("a1", "a2"))
  expect_identical(p$state$sap$families$f1$method, "holm")
  expect_identical(p$state$analyses$a1$family, "f1")
  # an analysis is in at most one family
  expect_error(sr_sap_family(p, "f2", "a2"), "f1")
  # setting a family again replaces it and frees the dropped member
  p <- sr_sap_family(p, "f1", "a1", method = "bonferroni")
  expect_identical(p$state$sap$families$f1$members, "a1")
  expect_identical(p$state$sap$families$f1$method, "bonferroni")
  expect_true(is.na(p$state$analyses$a2$family))
  p <- sr_sap_family(p, "f2", "a2", method = "none")
  expect_identical(p$state$analyses$a2$family, "f2")
  # a sensitivity analysis cannot join a family by naming one either
  expect_error(sr_sap_add(p, "s2", "stub_sap", .sap_spec(), "t", label = "sensitivity", family = "f1"),
               "not an inferential family")
})

test_that("sr_sap_rule pre-specifies a response to a diagnostic", {
  p <- .sap_drafted()
  p <- sr_sap_add(p, "s1", "stub_sap", .sap_spec(), "t", label = "sensitivity", parent = "a1")
  p <- sr_sap_add(p, "s2", "stub_sap", .sap_spec(), "t", label = "sensitivity", parent = "a2")
  expect_error(sr_sap_rule(p, "r1", "a1", "ph", "add_sensitivity", "s2"), "parent")
  expect_error(sr_sap_rule(p, "r1", "a1", "ph", "add_sensitivity", "a2"), "sensitivity")
  expect_error(sr_sap_rule(p, "r1", "a1", "ph", "add_sensitivity", "nope"), "nope")
  expect_error(sr_sap_rule(p, "r1", "a1", "normality", "add_sensitivity", "s1"), "check")
  expect_error(sr_sap_rule(p, "r1", "nope", "ph", "add_sensitivity", "s1"), "nope")
  expect_error(sr_sap_rule(p, "r1", "a1", "ph", "delete_it", "s1"))
  expect_error(sr_sap_rule(p, "r1", "a1", "ph", "add_sensitivity"), "alternative")
  p <- sr_sap_rule(p, "r1", "a1", "ph", "replace_primary", "s1")
  r <- p$state$sap$rules$r1
  expect_identical(r$analysis, "a1")
  expect_identical(r$check, "ph")
  expect_identical(r$action, "replace_primary")
  expect_identical(r$alternative, "s1")
})

test_that("the SAP hash ignores the order of independent adds and changes with a spec", {
  p0 <- .sap_drafted()
  q <- .sap_project()
  q <- sr_sap_add(q, "a2", "stub_sap", .sap_spec(exposure = "sex"), "t", label = "secondary")
  q <- sr_sap_add(q, "a1", "stub_sap", .sap_spec(), "t", label = "primary")
  h <- sr_sap_hash(p0)
  expect_match(h, "^[0-9a-f]{64}$")
  expect_identical(sr_sap_hash(q), h)
  # a changed spec, label, family and rule each change it
  expect_false(identical(sr_sap_hash(sr_sap_add(p0, "a2", "stub_sap", .sap_spec(exposure = "age"), "t",
                                                label = "secondary")), h))
  expect_false(identical(sr_sap_hash(sr_sap_add(p0, "a2", "stub_sap", .sap_spec(exposure = "sex"), "t",
                                                label = "primary")), h))
  expect_false(identical(sr_sap_hash(sr_sap_family(p0, "f1", c("a1", "a2"))), h))
  p1 <- sr_sap_add(p0, "s1", "stub_sap", .sap_spec(), "t", label = "sensitivity", parent = "a1")
  h1 <- sr_sap_hash(p1)
  expect_false(identical(sr_sap_hash(sr_sap_rule(p1, "r1", "a1", "ph", "add_sensitivity", "s1")), h1))
  # an analysis that was not pre-specified (added after the lock) is not part of it
  p2 <- p0
  p2$state$analyses$late <- p0$state$analyses$a1
  p2$state$analyses$late$prespecified <- FALSE
  expect_identical(sr_sap_hash(p2), h)
})

# ---- the check and the lock ---------------------------------------------------------------------------

test_that("the lock needs a primary analysis", {
  .sap_route()
  p <- .sap_project()
  p <- sr_sap_add(p, "a2", "stub_sap", .sap_spec(), "t", label = "secondary")
  chk <- sr_sap_check(p)
  expect_identical(names(chk), c("item", "status", "detail"))
  expect_true(any(chk$status == "block" & grepl("primary", chk$detail)))
  expect_error(sr_sap_lock(p), "primary")
  expect_true(sr_blind(p))
  expect_error(sr_sap_lock(.sap_project()), "primary")         # no analysis at all
})

test_that("a kept-for-sensitivity finding needs a sensitivity analysis that addresses it", {
  p <- .sap_drafted()
  fid <- .sap_heaping_id(p)
  expect_true(nzchar(fid))
  p <- sr_action_decide(p, fid, "keep_sensitivity", reason = "round numbers are plausible here", table = "t")
  chk <- sr_sap_check(p)
  hit <- chk$status == "block" & grepl(fid, chk$detail, fixed = TRUE)
  expect_true(any(hit))
  expect_match(chk$detail[hit][1L], "none addresses it")
  expect_error(sr_sap_lock(p), fid, fixed = TRUE)
  expect_true(sr_blind(p))
  # a sensitivity analysis that addresses a different finding does not help
  p2 <- sr_sap_add(p, "s0", "stub_sap", .sap_spec(), "t", label = "sensitivity", parent = "a1",
                   addresses = "000000000000")
  expect_error(sr_sap_lock(p2), fid, fixed = TRUE)
  # one that addresses it does
  p3 <- sr_sap_add(p, "s1", "stub_sap", .sap_spec(), "t", label = "sensitivity", parent = "a1",
                   addresses = fid)
  p3 <- .fx_feasible(p3)
  expect_false(any(sr_sap_check(p3)$status == "block"))
  p4 <- sr_sap_lock(p3)
  expect_false(sr_blind(p4))
})

test_that("a table that is not ready refuses the lock, naming the table", {
  .sap_route()
  p <- .fx_project()
  withr::defer(sr_duck_close(p))
  csv <- file.path(dirname(p$paths$dir), "t.csv")
  .fx_write_csv(.sap_frame(), csv)
  p <- sr_ingest(p, csv, table = "t", options = list(drive_info = .sap_drive))
  p <- sr_sap_add(p, "a1", "stub_sap", .sap_spec(), "t", label = "primary")
  chk <- sr_sap_check(p)
  expect_true(any(chk$status == "block" & grepl("^t: ", chk$detail)))
  expect_error(sr_sap_lock(p), "t: ")
  expect_true(sr_blind(p))
})

test_that("an analysis too large for an in-memory fit refuses the lock; a warn row does not", {
  p <- .sap_drafted()
  expect_true(any(sr_readiness(p, "t")$status == "warn"))      # no data dictionary is attached
  expect_false(any(sr_sap_check(.fx_feasible(p))$status == "block"))
  .sap_mock("sr_mem_estimate", function(project, table, cols, method = "glm", ...) {
    list(fits = FALSE, reason = "mock", need_gb = 99, budget_gb = 1, route = "bigglm", rows = 1, p = 1)
  })
  chk <- sr_sap_check(p)
  expect_true(any(chk$status == "block" & grepl("too large for an in-memory fit", chk$detail)))
  expect_true(any(chk$status == "block" & grepl("P1e", chk$detail)))
  expect_error(sr_sap_lock(p), "in-memory")
})

test_that("sr_mem_estimate is asked about the columns of the spec with the method of the route", {
  .sap_route()
  seen <- new.env()
  .sap_mock("sr_mem_estimate", function(project, table, cols, method = "glm", ...) {
    seen$cols <- c(seen$cols, list(cols))
    seen$method <- c(seen$method, method)
    list(fits = TRUE, reason = "mock", need_gb = 0, budget_gb = 1, route = "in_memory", rows = 1, p = 1)
  })
  p <- .sap_project()
  p <- sr_sap_add(p, "a1", "stub_sap", .sap_spec(exposure = "sex"), "t", label = "primary")
  sr_sap_check(p)
  expect_identical(seen$cols[[1L]], c("died", "sex"))
  expect_identical(seen$method, "lm")
})

test_that("a spec column that is not in the table refuses the lock", {
  .sap_route()
  p <- .sap_project()
  p <- sr_sap_add(p, "a1", "stub_sap", .sap_spec(outcome = "no_such_col"), "t", label = "primary")
  chk <- sr_sap_check(p)
  expect_true(any(chk$status == "block" & grepl("no_such_col", chk$detail)))
  expect_error(sr_sap_lock(p), "no_such_col")
})

test_that("a rule or family that no longer fits the analyses refuses the lock", {
  p <- .sap_drafted()
  p <- sr_sap_add(p, "s1", "stub_sap", .sap_spec(), "t", label = "sensitivity", parent = "a1")
  p <- sr_sap_family(p, "f1", c("a1", "a2"))
  p <- sr_sap_rule(p, "r1", "a1", "ph", "add_sensitivity", "s1")
  p <- .fx_feasible(p)
  expect_false(any(sr_sap_check(p)$status == "block"))
  # s1 is re-added under another parent: the alternative of the rule is no longer a sensitivity analysis of a1
  q <- sr_sap_add(p, "s1", "stub_sap", .sap_spec(), "t", label = "sensitivity", parent = "a2")
  expect_true(any(sr_sap_check(q)$status == "block" & grepl("r1", sr_sap_check(q)$detail)))
  expect_error(sr_sap_lock(q), "r1")
  # a2 is re-added as a sensitivity analysis while still in family f1
  expect_error(sr_sap_add(p, "a2", "stub_sap", .sap_spec(), "t", label = "sensitivity"), "f1")
})

test_that("locking records the SAP, the store hashes and the library snapshot, and unblinds", {
  p <- .sap_drafted()
  p <- sr_sap_family(p, "f1", c("a1", "a2"), method = "holm")
  p <- .fx_feasible(p)
  h <- sr_sap_hash(p)
  expect_true(sr_blind(p))
  p <- sr_sap_lock(p)
  expect_false(sr_blind(p))
  expect_true(p$state$sap$locked)
  lk <- p$state$sap$lock
  expect_identical(lk$sap_hash, h)
  expect_identical(sr_sap_hash(p), h)                           # the lock itself is not in the hash
  expect_identical(lk$tables$t$store_hash, sr_store_hash(sr_store_manifest(p, "t")))
  expect_identical(lk$libsnap_hash, sr_libsnap_hash())
  expect_match(lk$locked_at, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T")
  expect_identical(names(lk$tables), "t")
  # after the lock the SAP is closed
  expect_error(sr_sap_add(p, "a3", "stub_sap", .sap_spec(), "t"), "the SAP is locked; use sr_analysis_add")
  expect_error(sr_sap_remove(p, "a1"), "locked")
  expect_error(sr_sap_family(p, "f2", "a1"), "locked")
  expect_error(sr_sap_rule(p, "r1", "a1", "ph", "add_sensitivity", "a2"), "locked")
  expect_error(sr_sap_lock(p), "locked")
})

test_that("a read-only project cannot draft or lock", {
  p <- .sap_drafted()
  p$readonly <- TRUE
  expect_error(sr_sap_add(p, "a3", "stub_sap", .sap_spec(), "t"), "read-only")
  expect_error(sr_sap_lock(p), "read-only")
})

test_that("the SAP state replays identically after a close and reopen", {
  p <- .sap_drafted()
  p <- sr_sap_add(p, "s1", "stub_sap", .sap_spec(exposure = "sex"), "t", label = "sensitivity",
                  parent = "a1", addresses = character())
  p <- sr_sap_family(p, "f1", c("a1", "a2"), method = "hochberg")
  p <- sr_sap_rule(p, "r1", "a1", "ph", "add_sensitivity", "s1")
  p <- sr_sap_remove(p, "s1")
  p <- sr_sap_add(p, "s1", "stub_sap", .sap_spec(exposure = "sex"), "t", label = "sensitivity", parent = "a1")
  p <- sr_sap_rule(p, "r1", "a1", "ph", "add_sensitivity", "s1")
  p <- sr_sap_lock(.fx_feasible(p))
  dir <- p$paths$dir
  sr_duck_close(p)
  sr_project_close(p)
  q <- sr_project_open(dir)
  withr::defer(sr_duck_close(q))
  withr::defer(sr_project_close(q))
  expect_identical(q$state$sap, p$state$sap)
  expect_identical(q$state$analyses, p$state$analyses)
  expect_false(sr_blind(q))
  expect_identical(sr_sap_hash(q), p$state$sap$lock$sap_hash)
})

# ---- the feasibility gate of the lock -------------------------------------------------------------------

# The lock error as one string, "" when the lock succeeds.
.sap_lock_msg <- function(p) tryCatch({ sr_sap_lock(p); "" }, error = function(e) conditionMessage(e))

test_that("the lock needs a current feasibility decision on every primary analysis", {
  p <- .sap_drafted()
  chk <- sr_sap_check(p)
  fr <- chk[chk$item == "feasibility", , drop = FALSE]
  expect_identical(fr$status, "block")
  expect_identical(fr$detail, "a1: no feasibility assessment")      # the secondary a2 does not block
  expect_match(.sap_lock_msg(p), "feasibility: a1: no feasibility assessment", fixed = TRUE)
  expect_true(sr_blind(p))

  p <- sr_feasibility_assess(p, "a1")
  expect_match(.sap_lock_msg(p), "feasibility: a1: no feasibility decision", fixed = TRUE)

  ps <- sr_feasibility_decide(p, "a1", "stop", reason = "the design cannot answer the question")
  expect_match(.sap_lock_msg(ps), "feasibility: a1: decision is stop (the design cannot answer the question)",
               fixed = TRUE)

  pr <- sr_feasibility_decide(p, "a1", "revise", reason = "add a covariate")
  expect_match(.sap_lock_msg(pr), "feasibility: a1: decision is revise", fixed = TRUE)

  # the spec is re-set after the decision: the assessment no longer describes it
  pp <- sr_feasibility_decide(p, "a1", "proceed")
  expect_false(any(sr_sap_check(pp)$status == "block"))
  pq <- sr_sap_add(pp, "a1", "stub_sap", .sap_spec(exposure = "sex"), "t", label = "primary")
  expect_match(.sap_lock_msg(pq), "feasibility: a1: the feasibility assessment is stale (the spec changed)",
               fixed = TRUE)
  expect_true(sr_blind(pq))

  chk <- sr_sap_check(pp)
  expect_identical(chk$detail[chk$item == "feasibility"], "every primary analysis has a current feasibility decision")
  expect_identical(chk$status[chk$item == "feasibility"], "ok")
  pl <- sr_sap_lock(pp)
  expect_false(sr_blind(pl))
})

test_that("a stopped secondary blocks the lock; an unassessed secondary does not", {
  p <- .sap_drafted()
  p <- .fx_feasible(p)                                              # a1 only
  expect_false(any(sr_sap_check(p)$status == "block"))
  p <- sr_feasibility_assess(p, "a2")
  p <- sr_feasibility_decide(p, "a2", "stop", reason = "no events")
  expect_match(.sap_lock_msg(p), "feasibility: a2: decision is stop (no events)", fixed = TRUE)
})

test_that("a removed analysis does not hold the lock", {
  p <- .sap_drafted()
  p <- sr_feasibility_assess(p, "a2")
  p <- .fx_feasible(p)
  p <- sr_sap_remove(p, "a2")
  expect_false(any(sr_sap_check(p)$status == "block"))
})
