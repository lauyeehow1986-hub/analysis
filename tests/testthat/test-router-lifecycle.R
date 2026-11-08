# Tests for the router over a project's life: PICO -> route -> decide -> lock -> later edits. The router's
# view of "current" must be complete (the PICO is hashed for the design and the purpose), robust (a
# vanished instance is not current, never fatal) and frozen once the SAP is locked. The REAL nodes are
# used (no isolation). Synthetic data only.

# 300 rows; death alternates 0/1; bmi_6m is a function of death (the planted post-outcome column).
.rl_df <- function() {
  n <- 300L
  death <- rep(0:1, n / 2L)
  data.frame(id = seq_len(n), age = round(50 + 10 * sin(seq_len(n)), 1),
             sex = rep(c(0L, 0L, 1L, 1L), n / 4L), bmi_6m = death * 3L + 2L, death = death,
             stringsAsFactors = FALSE)
}

.rl_roles <- c(id = "id", age = "covariate", bmi_6m = "covariate", death = "outcome", sex = "exposure")

.rl_spec <- function(terms) {
  layers <- list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                 framework = "frequentist", clustered = FALSE)
  list(purpose = "explain-causal", outcome = "death", exposure = "sex",
       terms = lapply(terms, function(v) list(var = v)), layers = layers, seed = 1L)
}

# A project with a primary logistic analysis adjusting for age and bmi_6m (not routed, not feasible).
.rl_project <- function(env = parent.frame()) {
  if (!(.sr_route_key("logistic", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
    sr_route_register(.sr_logistic_route())
  }
  p <- .fx_ready_project(.rl_df(), "t", .rl_roles, env = env)
  sr_sap_add(p, "a1", "logistic", .rl_spec(c("age", "bmi_6m")), "t", label = "primary")
}

.rl_status <- function(p, keys) {
  tab <- sr_router_table(p)
  tab$status[match(keys, tab$key)]
}

.rl_gate <- function(p, items) {
  g <- .sr_router_gate(p)
  g[match(items, g$item), , drop = FALSE]
}

# ---- I1: the PICO is part of the design and purpose hashes ------------------------------------------

test_that("a PICO edit makes the design and the purpose stale", {
  p <- .fx_feasible(.rl_project())
  expect_identical(.rl_status(p, c("design", "purpose")), c("decided", "decided"))
  expect_identical(.rl_gate(p, c("study design", "study purpose"))$status, c("ok", "ok"))
  # only the population changes: the aim (the purpose's own fact) and the roles stay as they were
  p <- sr_pico_set(p, population = "children in intensive care", outcome = "test outcome")
  expect_identical(.rl_status(p, c("design", "purpose")), c("stale", "stale"))
  g <- .rl_gate(p, c("study design", "study purpose"))
  expect_identical(g$status, c("block", "block"))
  expect_match(g$detail[1L], "changed inputs", fixed = TRUE)
  expect_match(g$detail[2L], "changed inputs", fixed = TRUE)
  expect_true(is.na(sr_router_choice(p, "design")))
  # a node without the pico flag is not affected by the PICO text
  expect_false(isTRUE(sr_node_get("guideline")$pico))
  expect_true(isTRUE(sr_node_get("design")$pico))
  expect_true(isTRUE(sr_node_get("purpose")$pico))
})

# ---- I2: the lock freezes the router's decisions ---------------------------------------------------

test_that("the lock freezes the router's view: roles and DQ re-runs after it change nothing", {
  p <- .fx_feasible(.rl_project())
  p <- sr_sap_lock(p)
  lk <- p$state$sap$lock
  expect_identical(sr_sap_hash(p), lk$sap_hash)
  # the snapshot holds every decision current at the lock
  snap <- lk$router$decisions
  expect_true(all(c("design", "purpose", "guideline", "covariate_timing@t:bmi_6m") %in% names(snap)))
  expect_identical(snap$design$choice, "cohort_retrospective")
  design <- sr_router_choice(p, "design")
  tab0 <- sr_router_table(p)
  # roles re-confirmed after the lock (exposure dropped, age gone): the design facts change
  p <- sr_roles_set(p, "t", c(id = "id", bmi_6m = "covariate", death = "outcome", sex = "covariate"))
  # a DQ re-run without the leakage check drops the finding ids bmi_6m's timing was decided under
  p <- sr_dq_run(p, "t", checks = "dup_rows")
  expect_identical(sr_sap_hash(p), lk$sap_hash)
  expect_identical(sr_router_choice(p, "design"), design)
  expect_identical(sr_router_choice(p, "covariate_timing", "t", "bmi_6m"), "baseline")
  expect_identical(sr_router_choice(p, "covariate_timing", "t", "age"), NA_character_)
  tab <- sr_router_table(p)
  dec0 <- tab0[tab0$status == "decided", c("key", "choice"), drop = FALSE]
  dec1 <- tab[tab$status == "decided", c("key", "choice"), drop = FALSE]
  srt <- function(d) {
    d <- d[order(d$key), , drop = FALSE]
    rownames(d) <- NULL
    d
  }
  expect_identical(srt(dec1), srt(dec0))
  g <- .rl_gate(p, c("study design", "study purpose"))
  expect_identical(g$status, c("ok", "ok"))
  # the writer still renders the Study design paragraph and the decision table
  reg <- sr_ph_registry()
  para <- .sr_wr_m_design(p, reg)
  expect_false(is.null(para))
  expect_match(sr_ph_resolve(para, reg), "retrospective cohort", fixed = TRUE)
  expect_false(is.null(.sr_wr_decision_table(p, reg)))
  expect_no_error(sr_router_shadow(p))
  # the same after a close and reopen (the snapshot is re-read from events.jsonl)
  dir <- p$paths$dir
  sr_duck_close(p)
  sr_project_close(p)
  q <- sr_project_open(dir)
  withr::defer(sr_duck_close(q))
  withr::defer(sr_project_close(q))
  expect_identical(sr_sap_hash(q), lk$sap_hash)
  expect_identical(sr_router_choice(q, "design"), design)
})

test_that("a lock with no snapshot (before P1d) keeps the live view and its hash", {
  q <- .rl_project()
  q <- .fx_feasible(q, routed = FALSE)
  expect_null(.sr_router_hash_part(q))
  # a pre-P1d lock: the sap.locked payload has no router snapshot
  q <- sr_project_append(q, "sap.locked", list(sap_hash = sr_sap_hash(q), tables = list(),
                                               libsnap_hash = "x", locked_at = .sr_now()))
  expect_null(q$state$sap$lock$router)
  expect_null(.sr_router_frozen(q))
  expect_null(.sr_router_hash_part(q))
  expect_identical(sr_sap_hash(q), q$state$sap$lock$sap_hash)
})

# ---- I3: a vanished instance is not current and never crashes a reader -----------------------------

test_that("a decided covariate that leaves the roles does not crash a reader", {
  p <- .fx_routed(.rl_project())
  p <- sr_router_decide(p, "covariate_timing", "baseline", reason = "measured at enrolment", table = "t",
                        column = "age")
  # a logged prediction for the instance, so the shadow report iterates its key
  p <- sr_project_append(p, "router.predicted", list(
    key = "covariate_timing@t:age", node = "covariate_timing", table = "t", column = "age", backend = "jev",
    model = "fake-model", input_hash = "h", payload_sha = "s", payload = "{}", raw = "{}", choice = "baseline",
    probabilities = list(baseline = 1), confidence = 1, would_act = TRUE, latency_ms = 1, error = NA))
  expect_identical(sr_router_choice(p, "covariate_timing", "t", "age"), "baseline")
  # roles re-confirmed before the lock: age has no role and the outcome is now bmi_6m
  p <- sr_roles_set(p, "t", c(id = "id", bmi_6m = "outcome", death = "covariate", sex = "exposure"))
  expect_true(is.na(sr_router_choice(p, "covariate_timing", "t", "age")))
  expect_no_error(sr_sap_hash(p))
  expect_no_error(sh <- sr_router_shadow(p))
  expect_true(is.na(sh$final_choice[sh$key == "covariate_timing@t:age"]))
  expect_no_error(tab <- sr_router_table(p))
  expect_false("covariate_timing@t:age" %in% tab$key)
  expect_identical(tab$status[tab$key == "outcome_col@t"], "stale")
  expect_no_error(.sr_router_gate(p))
  expect_no_error(sr_sap_check(p))
  expect_no_error(sr_router_shadow_summary(p))
  # the facts of a column with no role say "unassigned"
  ctx <- .sr_router_ctx(p, "covariate_timing", "t", "age")
  expect_identical(sr_node_get("covariate_timing")$facts(ctx)$column_role, "unassigned")
})

# ---- T9 / M4: the review-focus inputs --------------------------------------------------------------

test_that("an untimed covariate shared by two primaries is counted once", {
  p <- .rl_project()
  p <- sr_sap_add(p, "a2", "logistic", .rl_spec("age"), "t", label = "primary")
  p <- .fx_routed(p)
  expect_true(is.na(sr_router_choice(p, "covariate_timing", "t", "age")))
  expect_false(is.na(sr_router_choice(p, "covariate_timing", "t", "bmi_6m")))
  g <- .sr_router_gate(p)
  w <- g[g$item == "covariate timing" & g$status == "warn", , drop = FALSE]
  expect_identical(nrow(w), 1L)
  expect_match(w$detail, "^1 adjusted covariate")
})

test_that("a table with no covariate has no timing instance and an ok timing row", {
  # the primary adjusts for nothing (an unadjusted contrast), and no column has the covariate role
  p <- sr_sap_add(.rl_project(), "a1", "logistic", .rl_spec(character()), "t", label = "primary")
  p <- .fx_routed(p)
  p <- sr_roles_set(p, "t", c(id = "id", death = "outcome", sex = "exposure"))
  expect_false(any(sr_router_table(p)$node == "covariate_timing"))
  expect_no_error(g <- .sr_router_gate(p))
  expect_identical(g$status[g$item == "covariate timing"], "ok")
})

test_that("two columns with the same label do not settle a column choice", {
  # ingested with roles but no outcome role (no DQ run: readiness is not needed here)
  p <- .fx_project()
  withr::defer(sr_duck_close(p))
  csv <- file.path(dirname(p$paths$dir), "t.csv")
  .fx_write_csv(.rl_df(), csv)
  p <- sr_ingest(p, csv, table = "t", options = list(drive_info = function(path) {
    list(free_bytes = 1e13, remote = FALSE, warnings = character())
  }))
  p <- sr_roles_set(p, "t", c(id = "id", age = "covariate", bmi_6m = "covariate", sex = "exposure"))
  v <- names(.rl_df())
  d <- data.frame(variable = v, label = ifelse(v %in% c("death", "bmi_6m"), "Body weight", ""), type = "text",
                  units = "", min = "", max = "", stringsAsFactors = FALSE)
  d$levels <- rep(list(data.frame(code = character(), label = character(), stringsAsFactors = FALSE)),
                  length(v))
  d$missing_codes <- rep(list(character()), length(v))
  d$source <- "test"
  p <- sr_dict_set(p, "t", d, source = "test")
  p <- sr_pico_set(p, population = "adults", outcome = "body weight", aim = "explain-causal")
  p <- sr_router_run(p)
  expect_identical(.rl_status(p, "outcome_col@t"), "pending")
  pm <- sr_router_preview(p, "outcome_col", "t", mask = TRUE)
  expect_false(is.null(pm))
  expect_false(grepl("Body weight", pm$body, fixed = TRUE))
  expect_false(grepl("bmi_6m", pm$body, fixed = TRUE))
})

test_that("sr_router_editor gives a node's options and what it waits on", {
  p <- .fx_ui_project()
  ed <- sr_router_editor(p, "outcome_col", table = "t")
  expect_true("purpose" %in% ed$waiting_on)
  ed2 <- sr_router_editor(p, "purpose")
  expect_setequal(ed2$options, SR_PURPOSES)
  expect_identical(ed2$waiting_on, character())
})
