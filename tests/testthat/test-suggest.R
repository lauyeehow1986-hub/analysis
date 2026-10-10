# Tests for profile/suggest.R: the Data Action Suggestor (one routed action per warn+ DQ finding), the
# decisions recorded as `action.decided` events (and their recipe steps), and the readiness panel.
# Synthetic data only; everything lives under withr::local_tempdir().

# The roles of the cardiac study with the 30-day mortality as the outcome.
.sg_roles <- c(patient_id = "id", surgery_date = "time_zero", statin_start_date = "exposure_date",
               statin = "exposure", death_date = "event_date", mortality_30d = "outcome")

# The cardiac project with its dictionary; roles are set when `roles` is given.
.sg_cardiac <- function(roles = .sg_roles, env = parent.frame()) {
  p <- .fx_cardiac_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  csv <- file.path(dirname(p$paths$dir), "cardiac_dict.csv")
  .fx_write_csv(sr_sample_cardiac()$dict, csv)
  p <- sr_dict_set(p, "cardiac", sr_dict_read_csv(csv), "csv")
  if (length(roles)) p <- sr_roles_set(p, "cardiac", roles)
  p
}

# ... and its DQ run.
.sg_ready <- function(roles = .sg_roles, env = parent.frame()) {
  sr_dq_run(.sg_cardiac(roles, env = env), "cardiac")
}

.sg_row <- function(s, check, column) {
  s[s$check == check & s$column == column, , drop = FALSE]
}

.sg_status <- function(r, item) r$status[r$item == item]

.sg_n_events <- function(p) length(sr_events_read(p$paths$events))

# ---- sr_suggest -------------------------------------------------------------------------------------

test_that("SR_ACTIONS is the six actions", {
  expect_identical(SR_ACTIONS, c("recheck", "fix", "drop_cases", "drop_var", "set_missing_impute", "keep_sensitivity"))
})

test_that("every warn+ finding gets exactly one suggestion, and nothing else does", {
  p <- .sg_ready()
  f <- sr_dq_findings(p, "cardiac")
  s <- sr_suggest(p, "cardiac")
  expect_identical(names(s), c("finding_id", "check", "column", "action", "reason", "proposed_step"))
  warn <- f[f$severity != "info", ]
  expect_gt(nrow(warn), 10L)
  expect_identical(s$finding_id, warn$id)
  expect_false(anyDuplicated(s$finding_id) > 0L)
  expect_identical(s$check, warn$check)
  expect_identical(s$column, warn$column)
  expect_true(all(s$action %in% SR_ACTIONS))
  expect_true(all(nzchar(s$reason)))
  expect_type(s$proposed_step, "list")
  expect_length(s$proposed_step, nrow(s))
})

test_that("a suggestion has the default routing of its check", {
  p <- .sg_ready()
  s <- sr_suggest(p, "cardiac")
  act <- function(check, column) s$action[s$check == check & s$column == column]
  expect_identical(act("unit_mix", "weight_kg"), "fix")
  expect_identical(act("unit_mix", "creatinine_umol"), "fix")
  expect_identical(act("coercion", "creatinine_umol"), "recheck")
  expect_identical(act("missing_codes", "sbp"), "set_missing_impute")
  expect_identical(act("range", "age"), "recheck")
  expect_identical(act("future_date", "surgery_date"), "recheck")
  expect_identical(unique(act("date_order", "discharge_date")), "recheck")
  expect_identical(act("dup_rows", "*"), "drop_cases")
  expect_identical(act("nzv", "const_flag"), "drop_var")
  expect_identical(act("collinear", "bmi_copy"), "drop_var")
  expect_identical(act("leakage", "lab_status"), "drop_var")
  expect_identical(act("heaping", "sbp"), "keep_sensitivity")
  expect_identical(act("immortal_time", "statin_start_date"), "keep_sensitivity")
  expect_identical(act("missing_col", "death_date"), "set_missing_impute")
  expect_identical(act("dict_levels", "nyha"), "recheck")
  # info findings (outlier, high_card, soft range) get no suggestion
  expect_length(act("outlier", "age"), 0L)
  expect_length(act("high_card", "notes"), 0L)
})

test_that("the routing helper covers the whole table, including the protected-role variants", {
  r <- function(check, protected = FALSE) .sr_sg_route(check, protected)
  expect_identical(r("nzv", TRUE), "keep_sensitivity")
  expect_identical(r("collinear", TRUE), "keep_sensitivity")
  expect_identical(r("high_card", TRUE), "keep_sensitivity")
  expect_identical(r("leakage", TRUE), "recheck")
  expect_identical(r("leakage"), "drop_var")
  expect_identical(r("dup_id"), "recheck")
  expect_identical(r("separation"), "keep_sensitivity")
  expect_identical(r("outlier"), "keep_sensitivity")
  expect_identical(r("check_error"), "recheck")
  expect_identical(r("dict_range"), "recheck")
  expect_identical(r("a_future_check"), "recheck")
})

test_that("unit_mix proposes convert_unit with the inverse of the template factor, restricted by the where of the finding", {
  p <- .sg_ready()
  f <- sr_dq_findings(p, "cardiac")
  s <- sr_suggest(p, "cardiac")
  for (col in c("weight_kg", "glucose_mmol")) {
    r <- .sg_row(s, "unit_mix", col)
    st <- r$proposed_step[[1L]]
    fi <- f[f$check == "unit_mix" & f$column == col, ]
    u <- fi$detail[[1L]]$units
    expect_identical(st$op, "convert_unit")
    expect_identical(st$args$col, col)
    expect_equal(st$args$factor, 1 / u$factor, tolerance = 1e-9)
    expect_equal(st$args$offset + u$offset / u$factor, 0, tolerance = 1e-9)
    expect_identical(st$args$where, fi$where[[1L]])
    expect_identical(st$finding_id, fi$id)
    expect_true(nzchar(st$reason))
  }
  # kg from lb: the factor shrinks the values (the template factor is above 1, not a number written in suggest.R)
  expect_lt(.sg_row(s, "unit_mix", "weight_kg")$proposed_step[[1L]]$args$factor, 1)
  # creatinine_umol is stored as text (it has unparsable cells), which convert_unit cannot take: no proposal,
  # and the reason says to cast first
  cr <- .sg_row(s, "unit_mix", "creatinine_umol")
  expect_null(cr$proposed_step[[1L]])
  expect_match(cr$reason, "cast")
})

test_that("missing_codes proposes set_missing with the where of the finding; no other default routing proposes a step", {
  p <- .sg_ready()
  f <- sr_dq_findings(p, "cardiac")
  s <- sr_suggest(p, "cardiac")
  r <- .sg_row(s, "missing_codes", "sbp")
  st <- r$proposed_step[[1L]]
  expect_identical(st$op, "set_missing")
  expect_identical(st$args$col, "sbp")
  expect_identical(st$args$where, f$where[[which(f$check == "missing_codes" & f$column == "sbp")]])
  expect_identical(st$finding_id, r$finding_id)
  has <- !vapply(s$proposed_step, is.null, NA)
  expect_setequal(s$check[has], c("unit_mix", "missing_codes"))
})

test_that("a proposed step compiles: every one is accepted by sr_recipe_add", {
  p <- .sg_ready()
  s <- sr_suggest(p, "cardiac")
  for (i in which(!vapply(s$proposed_step, is.null, NA))) {
    expect_no_error(sr_recipe_add(p, "cardiac", s$proposed_step[[i]]))
  }
})

test_that("a protected column is routed to keep_sensitivity, never drop_var", {
  p <- .sg_ready(c(.sg_roles, const_flag = "exposure"))
  s <- sr_suggest(p, "cardiac")
  expect_identical(.sr_sg_route("nzv", TRUE), "keep_sensitivity")
  expect_identical(.sr_sg_route("leakage", TRUE), "recheck")
  # the nzv check itself only reports a protected column as info, which gets no suggestion
  expect_identical(nrow(.sg_row(s, "nzv", "const_flag")), 0L)
  protected <- names(sr_roles(p, "cardiac"))[sr_roles(p, "cardiac") %in% SR_PROTECTED_ROLES]
  expect_false(any(s$action == "drop_var" & s$column %in% protected))
  # ... and without the role the same column is dropped
  expect_identical(.sg_row(sr_suggest(.sg_ready(), "cardiac"), "nzv", "const_flag")$action, "drop_var")
})

test_that("every reason passes the outcome-blind gate and carries no outcome statistic", {
  p <- .sg_ready()
  s <- sr_suggest(p, "cardiac")
  outcome <- sr_role_cols(p, "cardiac", "outcome")
  other <- setdiff(names(sr_roles(p, "cardiac")), outcome)
  expect_no_error(.sr_assert_blind(s, outcome, other))
  expect_true(sr_blind(p))
  for (r in s$reason) {
    expect_length(r, 1L)
    expect_false(grepl("[\r\n]", r))
    expect_false(grepl("AUC|odds|hazard|crosstab", r, ignore.case = TRUE))
    expect_false(grepl(outcome, r, fixed = TRUE))
  }
  expect_true(any(grepl("lab_status", s$reason[s$check == "leakage"], fixed = TRUE)))
})

test_that("suggestions come only from the latest run", {
  p <- .sg_ready()
  expect_gt(nrow(sr_suggest(p, "cardiac")), 3L)
  p <- sr_dq_run(p, "cardiac", checks = "nzv")
  s <- sr_suggest(p, "cardiac")
  expect_identical(unique(s$check), "nzv")
  expect_identical(nrow(s), 1L)
})

test_that("suggest needs a DQ run; a run with no warn+ finding gives a zero-row frame of the same shape", {
  p <- .sg_cardiac()
  expect_error(sr_suggest(p, "cardiac"), "no DQ run")
  p <- sr_dq_run(p, "cardiac", checks = "outlier")
  s <- sr_suggest(p, "cardiac")
  expect_identical(nrow(s), 0L)
  expect_identical(names(s), c("finding_id", "check", "column", "action", "reason", "proposed_step"))
  expect_type(s$proposed_step, "list")
})

# ---- sr_action_decide -------------------------------------------------------------------------------

test_that("deciding fix on a unit_mix finding adds a recipe step with the finding_id and records the event", {
  p <- .sg_ready()
  s <- sr_suggest(p, "cardiac")
  r <- .sg_row(s, "unit_mix", "weight_kg")
  n0 <- .sg_n_events(p)
  p <- sr_action_decide(p, r$finding_id, "fix")
  steps <- sr_recipe_steps(p)
  expect_length(steps, 1L)
  expect_identical(steps[[1L]]$op, "convert_unit")
  expect_identical(steps[[1L]]$finding_id, r$finding_id)
  a <- p$state$actions[[r$finding_id]]
  expect_identical(a$action, "fix")
  expect_identical(a$step_id, steps[[1L]]$id)
  expect_identical(a$table, "cardiac")
  expect_false(isTRUE(a$override))
  ev <- sr_events_read(p$paths$events)
  expect_identical(vapply(ev[-seq_len(n0)], function(e) e$type, ""), c("recipe.step", "action.decided"))
  # the recipe runs
  p <- sr_recipe_apply(p, "cardiac")
  expect_true(p$state$tables$cardiac_clean$store$rows > 1000L)
})

test_that("a decision matching the suggestion needs no reason; an override needs one", {
  p <- .sg_ready()
  s <- sr_suggest(p, "cardiac")
  nz <- .sg_row(s, "nzv", "const_flag")
  expect_no_error(sr_action_decide(p, nz$finding_id, "drop_var"))
  n0 <- .sg_n_events(p)
  expect_error(sr_action_decide(p, nz$finding_id, "keep_sensitivity"), "reason")
  expect_error(sr_action_decide(p, nz$finding_id, "keep_sensitivity", reason = "   "), "reason")
  expect_error(sr_action_decide(p, nz$finding_id, "keep_sensitivity", reason = NA_character_), "reason")
  expect_identical(.sg_n_events(p), n0)    # nothing was recorded by the refused calls
  q <- sr_action_decide(p, nz$finding_id, "keep_sensitivity", reason = "constant by design of the study")
  a <- q$state$actions[[nz$finding_id]]
  expect_true(isTRUE(a$override))
  expect_identical(a$suggested_action, "drop_var")
  expect_identical(a$reason, "constant by design of the study")
  expect_length(sr_recipe_steps(q), 0L)
})

test_that("an action outside SR_ACTIONS, an unknown finding and a read-only project all error", {
  p <- .sg_ready()
  s <- sr_suggest(p, "cardiac")
  id <- s$finding_id[1L]
  expect_error(sr_action_decide(p, id, "ignore", reason = "x"), "action")
  expect_error(sr_action_decide(p, id, NA_character_), "action")
  expect_error(sr_action_decide(p, "000000000000", "recheck"), "unknown finding")
  expect_error(sr_action_decide(p, NA_character_, "recheck"), "finding_id")
  p$readonly <- TRUE
  expect_error(sr_action_decide(p, id, s$action[1L]), "read-only")
})

test_that("a decision with no proposed or supplied step is recorded with step_id NA and says the step is pending", {
  p <- .sg_ready()
  s <- sr_suggest(p, "cardiac")
  d <- .sg_row(s, "dup_rows", "*")
  p <- sr_action_decide(p, d$finding_id, "drop_cases")
  a <- p$state$actions[[d$finding_id]]
  expect_true(is.na(a$step_id))
  expect_match(a$detail, "pending")
  expect_length(sr_recipe_steps(p), 0L)
  m <- .sg_row(s, "missing_col", "death_date")
  p <- sr_action_decide(p, m$finding_id, "set_missing_impute")
  expect_true(is.na(p$state$actions[[m$finding_id]]$step_id))
  expect_match(p$state$actions[[m$finding_id]]$detail, "pending")
  # recheck and keep_sensitivity take no step: not pending
  k <- .sg_row(s, "heaping", "sbp")
  p <- sr_action_decide(p, k$finding_id, "keep_sensitivity")
  expect_false(grepl("pending", p$state$actions[[k$finding_id]]$detail))
})

test_that("a later decision for the same finding replaces the earlier one in state; the log keeps both", {
  p <- .sg_ready()
  s <- sr_suggest(p, "cardiac")
  id <- .sg_row(s, "heaping", "sbp")$finding_id
  p <- sr_action_decide(p, id, "keep_sensitivity")
  p <- sr_action_decide(p, id, "recheck", reason = "ask the clinic about the sbp device")
  expect_identical(p$state$actions[[id]]$action, "recheck")
  expect_length(p$state$actions, 1L)
  ev <- Filter(function(e) identical(e$type, "action.decided"), sr_events_read(p$paths$events))
  expect_identical(vapply(ev, function(e) e$payload$action, ""), c("keep_sensitivity", "recheck"))
})

test_that("overriding coercion to fix adds a cast step; a drop_var decision drops the column", {
  p <- .sg_ready()
  s <- sr_suggest(p, "cardiac")
  co <- .sg_row(s, "coercion", "creatinine_umol")
  p <- sr_action_decide(p, co$finding_id, "fix", reason = "the unparsable values are free-text notes")
  st <- sr_recipe_steps(p)[[1L]]
  expect_identical(st$op, "cast")
  expect_identical(st$args, list(col = "creatinine_umol", type = "DOUBLE"))
  expect_identical(st$finding_id, co$finding_id)
  expect_identical(p$state$actions[[co$finding_id]]$step_id, st$id)
  nz <- .sg_row(s, "nzv", "const_flag")
  p <- sr_action_decide(p, nz$finding_id, "drop_var")
  st2 <- sr_recipe_steps(p)[[2L]]
  expect_identical(st2$op, "drop_var")
  expect_identical(st2$args, list(col = "const_flag"))
  expect_identical(st2$finding_id, nz$finding_id)
  expect_true(nzchar(st2$reason))
})

test_that("a drop_var decision on a protected column is refused and records nothing", {
  p <- .sg_ready(c(.sg_roles, const_flag = "exposure"))
  f <- sr_dq_findings(p, "cardiac")
  nz <- list(finding_id = f$id[f$check == "nzv" & f$column == "const_flag"])
  expect_length(nz$finding_id, 1L)
  n0 <- .sg_n_events(p)
  expect_error(sr_action_decide(p, nz$finding_id, "drop_var", reason = "x"), "protected")
  expect_identical(.sg_n_events(p), n0)
})

test_that("a supplied step is added with the finding_id forced and the decision reason as its default reason", {
  p <- .sg_ready()
  s <- sr_suggest(p, "cardiac")
  ra <- .sg_row(s, "range", "age")
  step <- list(op = "set_missing", args = list(col = "age", where = list(col = "age", op = ">", value = 120)),
               finding_id = "ffffffffffff")
  p <- sr_action_decide(p, ra$finding_id, "fix", reason = "age above 120 is a typing error", step = step)
  st <- sr_recipe_steps(p)[[1L]]
  expect_identical(st$finding_id, ra$finding_id)
  expect_identical(st$reason, "age above 120 is a typing error")
  expect_identical(p$state$actions[[ra$finding_id]]$step_id, st$id)
  # a step goes with a step-taking action only
  expect_error(sr_action_decide(p, ra$finding_id, "recheck", step = step), "step")
  # a step the recipe refuses is not recorded
  bad <- list(op = "set_missing", args = list(col = "no_such_col", where = NULL))
  n0 <- .sg_n_events(p)
  expect_error(sr_action_decide(p, ra$finding_id, "fix", reason = "x", step = bad), "unknown column")
  expect_identical(.sg_n_events(p), n0)
})

test_that("deciding the same auto-proposed fix twice does not stack a second step", {
  p <- .sg_ready()
  id <- .sg_row(sr_suggest(p, "cardiac"), "unit_mix", "weight_kg")$finding_id
  p <- sr_action_decide(p, id, "fix")
  p <- sr_action_decide(p, id, "fix")
  expect_length(sr_recipe_steps(p), 1L)
  expect_identical(p$state$actions[[id]]$step_id, sr_recipe_steps(p)[[1L]]$id)
})

test_that("decisions survive a reopen and replay identically", {
  p <- .sg_ready()
  s <- sr_suggest(p, "cardiac")
  p <- sr_action_decide(p, .sg_row(s, "unit_mix", "weight_kg")$finding_id, "fix")
  p <- sr_action_decide(p, .sg_row(s, "dup_rows", "*")$finding_id, "drop_cases")
  p <- sr_action_decide(p, .sg_row(s, "nzv", "const_flag")$finding_id, "keep_sensitivity", reason = "kept on purpose")
  dir <- p$paths$dir
  sr_duck_close(p)
  sr_project_close(p)
  q <- sr_project_open(dir)
  withr::defer(sr_duck_close(q))
  withr::defer(sr_project_close(q))
  expect_identical(q$state$actions, p$state$actions)
  expect_length(q$state$actions, 3L)
})

# ---- sr_readiness -----------------------------------------------------------------------------------

test_that("readiness lists the nine items in order with a status in ok|warn|block", {
  p <- .sg_cardiac(roles = NULL)
  r <- sr_readiness(p, "cardiac")
  expect_identical(names(r), c("item", "status", "detail"))
  expect_identical(r$item, c("ingest complete", "dictionary attached", "roles confirmed", "outcome role set",
                             "DQ run on current store hash", "block findings decided", "warn findings decided",
                             "recipe applied", "source full SHA"))
  expect_true(all(r$status %in% c("ok", "warn", "block")))
  expect_true(all(nzchar(r$detail)))
})

test_that("on a fresh import, roles confirmed and the DQ run block; the dictionary is only a warning", {
  p <- .fx_cardiac_project()
  withr::defer(sr_duck_close(p))
  r <- sr_readiness(p, "cardiac")
  expect_identical(.sg_status(r, "ingest complete"), "ok")
  expect_identical(.sg_status(r, "dictionary attached"), "warn")
  expect_identical(.sg_status(r, "roles confirmed"), "block")
  expect_identical(.sg_status(r, "outcome role set"), "block")
  expect_identical(.sg_status(r, "DQ run on current store hash"), "block")
  expect_identical(.sg_status(r, "block findings decided"), "block")
  expect_identical(.sg_status(r, "recipe applied"), "ok")
  expect_identical(.sg_status(r, "source full SHA"), "ok")
})

test_that("a table that was never imported blocks ingest complete", {
  p <- .sg_cardiac(roles = NULL)
  r <- sr_readiness(p, "nothing_here")
  expect_identical(.sg_status(r, "ingest complete"), "block")
})

test_that("roles and a DQ run clear their rows; undecided block findings block and are counted exactly", {
  p <- .sg_ready()
  f <- sr_dq_findings(p, "cardiac")
  r <- sr_readiness(p, "cardiac")
  expect_identical(.sg_status(r, "dictionary attached"), "ok")
  expect_identical(.sg_status(r, "roles confirmed"), "ok")
  expect_identical(.sg_status(r, "outcome role set"), "ok")
  expect_identical(.sg_status(r, "DQ run on current store hash"), "ok")
  nb <- sum(f$severity == "block")
  expect_gt(nb, 0L)
  expect_identical(.sg_status(r, "block findings decided"), "block")
  expect_match(r$detail[r$item == "block findings decided"], sprintf("%d of %d", nb, nb))
  nw <- sum(f$severity == "warn")
  expect_identical(.sg_status(r, "warn findings decided"), "warn")
  expect_match(r$detail[r$item == "warn findings decided"], sprintf("%d of %d", nw, nw))
  # deciding one block finding lowers the count by one
  s <- sr_suggest(p, "cardiac")
  b <- s[s$finding_id %in% f$id[f$severity == "block"], ]
  p <- sr_action_decide(p, b$finding_id[1L], b$action[1L])
  r <- sr_readiness(p, "cardiac")
  expect_match(r$detail[r$item == "block findings decided"], sprintf("%d of %d", nb - 1L, nb))
})

test_that("with roles, a DQ run and every block finding decided, no row is block", {
  p <- .sg_ready()
  f <- sr_dq_findings(p, "cardiac")
  s <- sr_suggest(p, "cardiac")
  for (id in f$id[f$severity == "block"]) {
    p <- sr_action_decide(p, id, s$action[s$finding_id == id])
  }
  r <- sr_readiness(p, "cardiac")
  expect_false(any(r$status == "block"))
  expect_identical(.sg_status(r, "block findings decided"), "ok")
  expect_identical(.sg_status(r, "warn findings decided"), "warn")
  # ... and deciding the warn findings too clears that warning
  for (id in f$id[f$severity == "warn"]) {
    p <- sr_action_decide(p, id, s$action[s$finding_id == id])
  }
  r <- sr_readiness(p, "cardiac")
  expect_identical(.sg_status(r, "warn findings decided"), "ok")
})

test_that("the outcome role missing blocks while the other roles are confirmed", {
  p <- .sg_ready(roles = c(patient_id = "id", statin = "exposure"))
  r <- sr_readiness(p, "cardiac")
  expect_identical(.sg_status(r, "roles confirmed"), "ok")
  expect_identical(.sg_status(r, "outcome role set"), "block")
})

test_that("a store that changed after the DQ run makes the run stale", {
  p <- .sg_ready()
  p$state$tables$cardiac$store_hash <- strrep("0", 64L)
  r <- sr_readiness(p, "cardiac")
  expect_identical(.sg_status(r, "DQ run on current store hash"), "block")
  expect_match(r$detail[r$item == "DQ run on current store hash"], "changed|stale", ignore.case = TRUE)
})

test_that("recipe applied: ok with no steps, warn with unapplied steps, ok after apply, warn again after a later step or undo", {
  p <- .sg_ready()
  expect_identical(.sg_status(sr_readiness(p, "cardiac"), "recipe applied"), "ok")
  s <- sr_suggest(p, "cardiac")
  p <- sr_action_decide(p, .sg_row(s, "unit_mix", "weight_kg")$finding_id, "fix")
  expect_identical(.sg_status(sr_readiness(p, "cardiac"), "recipe applied"), "warn")
  p <- sr_recipe_apply(p, "cardiac")
  expect_identical(.sg_status(sr_readiness(p, "cardiac"), "recipe applied"), "ok")
  p <- sr_action_decide(p, .sg_row(s, "unit_mix", "glucose_mmol")$finding_id, "fix")
  expect_identical(.sg_status(sr_readiness(p, "cardiac"), "recipe applied"), "warn")
  p <- sr_recipe_apply(p, "cardiac")
  expect_identical(.sg_status(sr_readiness(p, "cardiac"), "recipe applied"), "ok")
  p <- sr_recipe_undo(p)
  expect_identical(.sg_status(sr_readiness(p, "cardiac"), "recipe applied"), "warn")
  p <- sr_recipe_apply(p, "cardiac")
  expect_identical(.sg_status(sr_readiness(p, "cardiac"), "recipe applied"), "ok")
})

test_that("source full SHA warns while pending and is ok once known or for a derived table", {
  p <- .sg_ready()
  expect_identical(.sg_status(sr_readiness(p, "cardiac"), "source full SHA"), "ok")
  q <- p
  q$state$tables$cardiac$source$sha256 <- NA_character_
  r <- sr_readiness(q, "cardiac")
  expect_identical(.sg_status(r, "source full SHA"), "warn")
  expect_match(r$detail[r$item == "source full SHA"], "pending")
  s <- sr_suggest(p, "cardiac")
  p <- sr_action_decide(p, .sg_row(s, "unit_mix", "weight_kg")$finding_id, "fix")
  p <- sr_recipe_apply(p, "cardiac")
  expect_identical(.sg_status(sr_readiness(p, "cardiac_clean"), "source full SHA"), "ok")
})

# ---- a superseded decision undoes its step ----------------------------------------------------------

test_that("fix then recheck undoes the convert_unit step: the recipe SQL no longer has it and the decision says so", {
  p <- .sg_ready()
  id <- .sg_row(sr_suggest(p, "cardiac"), "unit_mix", "weight_kg")$finding_id
  p <- sr_action_decide(p, id, "fix")
  sid <- sr_recipe_steps(p)[[1L]]$id
  expect_match(sr_recipe_sql(p, "cardiac"), "sr_l1", fixed = TRUE)
  p <- sr_action_decide(p, id, "recheck", reason = "the lb readings need a second look at the source")
  expect_length(sr_recipe_steps(p), 0L)
  expect_false(grepl("sr_l1", sr_recipe_sql(p, "cardiac"), fixed = TRUE))
  a <- p$state$actions[[id]]
  expect_identical(a$action, "recheck")
  expect_true(is.na(a$step_id))
  expect_identical(a$undone_step_id, sid)
  expect_match(a$detail, sid, fixed = TRUE)
  # recorded as a recipe.undo event that names the step
  ev <- Filter(function(e) identical(e$type, "recipe.undo"), sr_events_read(p$paths$events))
  expect_length(ev, 1L)
  expect_identical(ev[[1L]]$payload$step_id, sid)
  # nothing left to apply: the readiness panel is happy
  expect_identical(.sg_status(sr_readiness(p, "cardiac"), "recipe applied"), "ok")
})

test_that("a step already undone by hand is not undone again", {
  p <- .sg_ready()
  id <- .sg_row(sr_suggest(p, "cardiac"), "unit_mix", "weight_kg")$finding_id
  p <- sr_action_decide(p, id, "fix")
  p <- sr_recipe_undo(p)
  n0 <- length(Filter(function(e) identical(e$type, "recipe.undo"), sr_events_read(p$paths$events)))
  p <- sr_action_decide(p, id, "keep_sensitivity", reason = "kept as recorded")
  n1 <- length(Filter(function(e) identical(e$type, "recipe.undo"), sr_events_read(p$paths$events)))
  expect_identical(n1, n0)
  expect_true(is.na(p$state$actions[[id]]$undone_step_id))
})

test_that("fix -> recheck -> fix leaves exactly one active step; a repeat of the same fix still reuses it", {
  p <- .sg_ready()
  id <- .sg_row(sr_suggest(p, "cardiac"), "unit_mix", "weight_kg")$finding_id
  p <- sr_action_decide(p, id, "fix")
  s1 <- sr_recipe_steps(p)[[1L]]$id
  p <- sr_action_decide(p, id, "recheck", reason = "second look")
  p <- sr_action_decide(p, id, "fix")
  st <- sr_recipe_steps(p)
  expect_length(st, 1L)
  expect_false(identical(st[[1L]]$id, s1))
  expect_identical(p$state$actions[[id]]$step_id, st[[1L]]$id)
  expect_true(is.na(p$state$actions[[id]]$undone_step_id))
  p <- sr_action_decide(p, id, "fix")
  expect_length(sr_recipe_steps(p), 1L)
  expect_identical(p$state$actions[[id]]$step_id, st[[1L]]$id)
})

test_that("undoing a middle step keeps the later one; a replacement step supersedes the earlier one; replay matches", {
  p <- .sg_ready()
  s <- sr_suggest(p, "cardiac")
  w <- .sg_row(s, "unit_mix", "weight_kg")$finding_id
  g <- .sg_row(s, "unit_mix", "glucose_mmol")$finding_id
  p <- sr_action_decide(p, w, "fix")
  p <- sr_action_decide(p, g, "fix")
  gid <- p$state$actions[[g]]$step_id
  p <- sr_action_decide(p, w, "keep_sensitivity", reason = "kept as recorded")
  expect_identical(vapply(sr_recipe_steps(p), function(x) x$id, ""), gid)
  # a user step for the same finding replaces the earlier step instead of stacking
  step <- list(op = "set_missing", args = list(col = "glucose_mmol", where = list(col = "glucose_mmol", op = ">", value = 60)))
  p <- sr_action_decide(p, g, "fix", reason = "values above 60 are typing errors", step = step)
  act <- sr_recipe_steps(p)
  expect_length(act, 1L)
  expect_identical(act[[1L]]$op, "set_missing")
  expect_identical(p$state$actions[[g]]$undone_step_id, gid)
  dir <- p$paths$dir
  sr_duck_close(p)
  sr_project_close(p)
  q <- sr_project_open(dir)
  withr::defer(sr_duck_close(q))
  withr::defer(sr_project_close(q))
  expect_identical(q$state$actions, p$state$actions)
  expect_identical(q$state$recipe, p$state$recipe)
  expect_identical(vapply(sr_recipe_steps(q), function(x) x$id, ""), act[[1L]]$id)
})

# ---- user-typed reasons while blind -----------------------------------------------------------------

test_that("while blind, a typed reason that carries an outcome statistic or crosses the outcome errors and records nothing", {
  p <- .sg_ready()
  expect_true(sr_blind(p))
  id <- .sg_row(sr_suggest(p, "cardiac"), "heaping", "sbp")$finding_id
  n0 <- .sg_n_events(p)
  expect_error(sr_action_decide(p, id, "recheck", reason = "AUC 0.93 against the outcome"), "outcome-blind")
  expect_error(sr_action_decide(p, id, "recheck", reason = "sbp heaping differs by mortality_30d"), "outcome-blind")
  # a supplied step reason is screened too
  ra <- .sg_row(sr_suggest(p, "cardiac"), "range", "age")$finding_id
  step <- list(op = "set_missing", args = list(col = "age", where = list(col = "age", op = ">", value = 120)),
               reason = "odds ratio of 3 for mortality_30d")
  expect_error(sr_action_decide(p, ra, "fix", reason = "typing error", step = step), "outcome-blind")
  expect_identical(.sg_n_events(p), n0)
  # plain prose, with the common words "or" and "effect", and the outcome column alone, are fine
  p <- sr_action_decide(p, id, "recheck", reason = "ask the clinic or the device vendor; mortality_30d is the study outcome")
  expect_identical(p$state$actions[[id]]$action, "recheck")
})

# ---- the cast detail and the protected-column end-to-end case ---------------------------------------

test_that("a coercion fix says the cast turns unparsable cells into NULL and where the count is reported", {
  p <- .sg_ready()
  co <- .sg_row(sr_suggest(p, "cardiac"), "coercion", "creatinine_umol")
  p <- sr_action_decide(p, co$finding_id, "fix", reason = "the unparsable values are free-text notes")
  d <- p$state$actions[[co$finding_id]]$detail
  expect_match(d, "TRY_CAST", fixed = TRUE)
  expect_match(d, "NULL", fixed = TRUE)
  expect_match(d, "new_nulls", fixed = TRUE)
  # ... and the count is really there
  cn <- sr_recipe_counts(p, "cardiac")
  expect_gt(cn$new_nulls[cn$step_id == p$state$actions[[co$finding_id]]$step_id], 0)
})

test_that("a protected column that reaches warn through sr_suggest (collinear) is kept, never dropped", {
  roles <- c(.sg_roles, bmi_copy = "time")
  p <- .sg_ready(roles)
  s <- sr_suggest(p, "cardiac")
  r <- .sg_row(s, "collinear", "bmi_copy")
  expect_identical(nrow(r), 1L)
  expect_identical(r$action, "keep_sensitivity")
  expect_null(r$proposed_step[[1L]])
  expect_match(r$reason, "protected")
  protected <- names(roles)[roles %in% SR_PROTECTED_ROLES]
  expect_false(any(s$action == "drop_var" & s$column %in% protected))
  # without the role the same finding is a drop_var
  expect_identical(.sg_row(sr_suggest(.sg_ready(), "cardiac"), "collinear", "bmi_copy")$action, "drop_var")
  # deciding drop_var against the protected routing is an override that the recipe refuses
  n0 <- .sg_n_events(p)
  expect_error(sr_action_decide(p, r$finding_id, "drop_var", reason = "x"), "protected")
  expect_identical(.sg_n_events(p), n0)
})

# ---- a refused decision writes nothing --------------------------------------------------------------

test_that("a new step that depends on the superseded one is refused before any event is written", {
  p <- .sg_ready()
  co <- .sg_row(sr_suggest(p, "cardiac"), "coercion", "creatinine_umol")$finding_id
  p <- sr_action_decide(p, co, "fix", reason = "the unparsable values are free-text notes")
  expect_identical(sr_recipe_steps(p)[[1L]]$op, "cast")
  conv <- list(op = "convert_unit", args = list(col = "creatinine_umol", where = list(col = "creatinine_umol", op = "not_na"),
                                                factor = 0.0113, offset = 0))
  n0 <- .sg_n_events(p)
  steps0 <- sr_recipe_steps(p)
  expect_error(sr_action_decide(p, co, "fix", reason = "convert as well", step = conv), "numeric")
  # nothing was recorded: the log, the recipe and the decision are as before, in memory and on disk
  expect_identical(.sg_n_events(p), n0)
  expect_identical(sr_recipe_steps(p), steps0)
  dir <- p$paths$dir
  sr_duck_close(p)
  sr_project_close(p)
  q <- sr_project_open(dir)
  withr::defer(sr_duck_close(q))
  withr::defer(sr_project_close(q))
  expect_identical(.sg_n_events(q), n0)
  expect_identical(q$state$recipe, p$state$recipe)
  expect_identical(q$state$actions, p$state$actions)
  expect_identical(sr_recipe_steps(q)[[1L]]$id, steps0[[1L]]$id)
})

test_that("a superseding step that fails to compile on its own leaves the earlier decision and step in place", {
  p <- .sg_ready()
  id <- .sg_row(sr_suggest(p, "cardiac"), "unit_mix", "weight_kg")$finding_id
  p <- sr_action_decide(p, id, "fix")
  n0 <- .sg_n_events(p)
  bad <- list(op = "set_missing", args = list(col = "no_such_col", where = NULL))
  expect_error(sr_action_decide(p, id, "fix", reason = "x", step = bad), "unknown column")
  expect_identical(.sg_n_events(p), n0)
  expect_length(sr_recipe_steps(p), 1L)
})

# ---- the text screen and the other-branch note ------------------------------------------------------

test_that("the text screen matches the outcome column in any case and knows AUROC, area under and p-value", {
  p <- .sg_ready()
  id <- .sg_row(sr_suggest(p, "cardiac"), "heaping", "sbp")$finding_id
  n0 <- .sg_n_events(p)
  for (txt in c("Mortality_30D and sbp differ", "AUROC 0.9 for sbp", "the area under the curve was 0.8",
                "p-value 0.01 for the heaping", "p value 0.01 for the heaping")) {
    expect_error(sr_action_decide(p, id, "recheck", reason = txt), "outcome-blind")
  }
  expect_identical(.sg_n_events(p), n0)
})

test_that("a superseded step that sits on another branch is not undone, and the detail says so", {
  p <- .sg_ready()
  id <- .sg_row(sr_suggest(p, "cardiac"), "unit_mix", "weight_kg")$finding_id
  p <- sr_action_decide(p, id, "fix")
  sid <- sr_recipe_steps(p)[[1L]]$id
  p <- sr_recipe_branch(p, "alt")
  p <- sr_recipe_checkout(p, "alt")
  p <- sr_recipe_undo(p)
  p <- sr_action_decide(p, id, "recheck", reason = "look again")
  a <- p$state$actions[[id]]
  expect_true(is.na(a$undone_step_id))
  expect_match(a$detail, sid, fixed = TRUE)
  expect_match(a$detail, "not the current branch")
  expect_length(sr_recipe_steps(p, "main"), 1L)
})
