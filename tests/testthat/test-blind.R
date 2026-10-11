# Tests for the outcome-blind gate across the data layer: the outcome family (outcome, event, event_date,
# censor_date, time, offset) is kept out of every pre-lock output that crosses it with another column, in
# the DQ run, the recipe counts, the flow, derived tables (recipe, join, cohort) and the Data tab helpers.
# Synthetic data only (the clean cardiac twin); everything lives under withr::local_tempdir().

.bl_family_roles <- c("outcome", "event", "event_date", "censor_date", "time", "offset")
.bl_drive <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())

.bl_ingest <- function(p, df, table) {
  csv <- file.path(dirname(p$paths$dir), paste0(table, ".csv"))
  .fx_write_csv(df, csv)
  sr_ingest(p, csv, table = table, options = list(drive_info = .bl_drive))
}

.bl_roles <- function(p, table = "cardiac") {
  sug <- sr_roles_suggest(p, table)
  r <- stats::setNames(sug$role, sug$column)
  r[["mortality_30d"]] <- "outcome"
  r[["statin"]] <- "exposure"
  sr_roles_set(p, table, r)
}

# The clean twin with the suggested roles plus mortality_30d = outcome and statin = exposure.
.bl_project <- function(env = parent.frame(), roles = TRUE) {
  p <- .fx_cardiac_project(defects = FALSE, env = env)
  withr::defer(sr_duck_close(p), envir = env)
  if (roles) p <- .bl_roles(p)
  p
}

.bl_step <- function(op, args, reason = "r") list(op = op, args = args, reason = reason)

# Every text a finding carries: column, message, detail (names and cells) and where.
.bl_txt <- function(x) {
  if (is.null(x)) return(character())
  if (is.data.frame(x)) return(c(names(x), unlist(lapply(x, function(v) as.character(unlist(v))), use.names = FALSE)))
  if (is.list(x)) return(c(names(x), unlist(lapply(x, .bl_txt), use.names = FALSE)))
  as.character(x)
}

# The column names (whole words) that occur in `s`.
.bl_hits <- function(s, cols) {
  s <- s[!is.na(s)]
  cols[vapply(cols, function(cn) {
    any(grepl(paste0("(?<![A-Za-z0-9_])", cn, "(?![A-Za-z0-9_])"), s, perl = TRUE))
  }, NA)]
}

# The findings (as "check column") that name a family column together with any other column.
.bl_crossing <- function(f, fam, cols) {
  out <- character()
  for (i in seq_len(nrow(f))) {
    s <- c(f$column[i], f$message[i], .bl_txt(f$detail[[i]]), .bl_txt(f$where[[i]]))
    h <- .bl_hits(s, cols)
    if (any(h %in% fam) && any(!(h %in% fam))) out <- c(out, paste(f$check[i], f$column[i]))
  }
  out
}

.bl_fam <- function(p, table) {
  r <- sr_roles(p, table)
  names(r)[r %in% .bl_family_roles]
}

.bl_cols <- function(p, table) vapply(sr_store_manifest(p, table)$schema, function(s) s$name, "")

.bl_reopen <- function(p, env = parent.frame()) {
  dir <- p$paths$dir
  sr_duck_close(p)
  sr_project_close(p)
  q <- sr_project_open(dir)
  withr::defer(sr_duck_close(q), envir = env)
  withr::defer(sr_project_close(q), envir = env)
  q
}

.bl_ids <- function(p) {
  sr_store_view(p, "cardiac")
  sr_duck_query(sr_duck(p), "SELECT patient_id FROM cardiac ORDER BY patient_id")$patient_id
}

test_that("blind end to end: no DQ finding, recipe count, flow row, join or cohort output crosses the outcome family", {
  p <- .bl_project()
  expect_true(sr_blind(p))
  fam <- .bl_fam(p, "cardiac")
  expect_setequal(fam, c("death_date", "mortality_30d", "followup_days"))
  cols <- .bl_cols(p, "cardiac")

  # the default DQ run
  p <- sr_dq_run(p, "cardiac")
  f <- sr_dq_findings(p, "cardiac")
  expect_identical(.bl_crossing(f, fam, cols), character())
  expect_silent(.sr_assert_blind(f, fam, setdiff(cols, fam)))
  expect_false(any(f$check == "missing_dependence" & f$column %in% fam))
  expect_no_error(sr_suggest(p, "cardiac"))

  # recipe counts: a clean first step keeps its counts; the step that touches the outcome family and
  # every later one have NA counts
  p <- sr_recipe_add(p, "cardiac", .bl_step("set_missing", list(col = "sbp", where = list(col = "sbp", op = ">", value = 300))))
  p <- sr_recipe_add(p, "cardiac", .bl_step("set_value", list(col = "mortality_30d", value = 1,
                                                             where = list(col = "statin", op = "==", value = 1))))
  p <- sr_recipe_add(p, "cardiac", .bl_step("set_missing", list(col = "age", where = list(all = list(
    list(col = "mortality_30d", op = "==", value = 1), list(col = "statin", op = "==", value = 1))))))
  p <- sr_recipe_add(p, "cardiac", .bl_step("drop_cases", list(where = list(col = "age", op = "is_na")), "no age"))
  ct <- sr_recipe_counts(p, "cardiac")
  expect_identical(nrow(ct), 4L)
  expect_false(anyNA(unlist(ct[1L, c("rows_before", "rows_after", "cells_changed")])))
  for (k in c("rows_before", "rows_after", "cells_changed", "new_nulls")) expect_true(all(is.na(ct[[k]][2:4])), info = k)

  # the flow hides the drop that descends from the tainted steps; so does the Data tab
  fl <- sr_flow(p, "cardiac")
  expect_identical(nrow(fl), 1L)
  expect_true(is.na(fl$n_before) && is.na(fl$n_after) && is.na(fl$n_excluded))
  expect_true(endsWith(fl$label, SR_FLOW_HIDDEN))
  expect_true(.sr_data_rows_hidden(p, "cardiac"))

  # the applied table depends on the outcome: its rows, profile and DQ counts are hidden
  p <- sr_recipe_apply(p, "cardiac")
  expect_true(isTRUE(p$state$tables$cardiac_clean$outcome_dependent))
  expect_true(.sr_data_rows_hidden(p, "cardiac_clean"))
  expect_true(is.na(sr_profile_table(p, "cardiac_clean")$rows))
  p <- sr_dq_run(p, "cardiac_clean")
  fc <- sr_dq_findings(p, "cardiac_clean")
  expect_true(all(is.na(fc$n_affected)) && all(is.na(fc$p_affected)))
  expect_false(any(grepl("sample of", fc$message)))
  expect_identical(.bl_crossing(fc, .bl_fam(p, "cardiac_clean"), .bl_cols(p, "cardiac_clean")), character())
  rd <- sr_readiness(p, "cardiac_clean")
  expect_false(grepl("^[0-9,]+ rows", rd$detail[rd$item == "ingest complete"]))
  d <- sr_table_diff(p, "cardiac", "cardiac_clean", key = "patient_id")
  expect_true(is.na(d$added) && is.na(d$removed) && is.na(d$changed_rows) && is.na(d$new_rows))
  expect_true(all(is.na(d$changed_cells$n)))

  # a join carries both sides' roles (the key as id); the outcome family stays hidden in it
  ids <- .bl_ids(p)
  set.seed(7)
  lab <- data.frame(patient_id = ids, crp = round(stats::rexp(length(ids), 0.2), 1), stringsAsFactors = FALSE)
  p <- .bl_ingest(p, lab, "lab")
  p <- sr_join(p, "cardiac", "lab", by = "patient_id", into = "cj")
  rj <- sr_roles(p, "cj")
  expect_identical(unname(rj["mortality_30d"]), "outcome")
  expect_identical(unname(rj["patient_id"]), "id")
  expect_true(all(fam %in% .sr_data_hidden_cols(p, "cj")))
  expect_false(any(fam %in% names(sr_sample(p, "cj"))))
  p <- sr_dq_run(p, "cj")
  fj <- sr_dq_findings(p, "cj")
  expect_identical(.bl_crossing(fj, fam, .bl_cols(p, "cj")), character())

  # a cohort: a criterion on the outcome family hides its count and the cohort table's rows; a feature
  # made from a family column is a family column of the cohort
  spec <- sr_cohort_spec(
    base = "cardiac", id = "patient_id", index = list(table = "cardiac", date_col = "surgery_date"),
    criteria = list(
      list(label = "Has a CRP", type = "include", table = "lab", where = list(col = "crp", op = "not_na")),
      list(label = "Alive", type = "exclude", table = "cardiac", where = list(col = "death_date", op = "not_na"))),
    features = list(list(name = "fu", table = "cardiac", col = "followup_days", agg = "max")),
    name = "coh")
  p <- sr_cohort_build(p, spec)
  cf <- sr_flow(p, "coh")
  expect_false(anyNA(cf$n_after[cf$step == "c1"]))
  expect_true(is.na(cf$n_after[cf$step == "c2"]))
  expect_true(isTRUE(p$state$tables$coh$outcome_dependent))
  expect_true(.sr_data_rows_hidden(p, "coh"))
  expect_true("fu" %in% .sr_data_hidden_cols(p, "coh"))
  expect_false("fu" %in% names(sr_sample(p, "coh")))

  # replay: reopening gives the same state
  q <- .bl_reopen(p)
  for (k in c("tables", "roles", "recipe", "cohorts", "joins", "dict")) expect_identical(q$state[[k]], p$state[[k]], info = k)
})

test_that("an outcome-dependent table stays hidden after the branch that made it is switched away and undone", {
  p <- .bl_project()
  p <- sr_recipe_add(p, "cardiac", .bl_step("drop_cases", list(where = list(col = "mortality_30d", op = "==", value = 1),
                                                              confirm_outcome_dependent = TRUE), "died"))
  p <- sr_recipe_apply(p, "cardiac")
  expect_true(.sr_data_rows_hidden(p, "cardiac_clean"))
  p <- sr_recipe_branch(p, "alt")
  p <- sr_recipe_checkout(p, "alt")
  p <- sr_recipe_undo(p)
  expect_true(.sr_data_rows_hidden(p, "cardiac_clean"))
  expect_true(is.na(sr_profile_table(p, "cardiac_clean")$rows))
  expect_true(all(is.na(sr_profile_table(p, "cardiac_clean")$columns$n)))
  # once the SAP is locked the counts come back
  p2 <- p
  p2$state$sap <- list(locked = TRUE)
  expect_false(.sr_data_rows_hidden(p2, "cardiac_clean"))
  expect_false(is.na(sr_profile_table(p2, "cardiac_clean")$rows))
  q <- .bl_reopen(p)
  expect_identical(q$state$tables, p$state$tables)
  expect_true(.sr_data_rows_hidden(q, "cardiac_clean"))
})

test_that("roles set on the source after a recipe was applied reach the clean table (lineage)", {
  p <- .bl_project(roles = FALSE)
  p <- sr_recipe_add(p, "cardiac", .bl_step("set_value", list(col = "mortality_30d", value = 1,
                                                             where = list(col = "statin", op = "==", value = 1))))
  p <- sr_recipe_apply(p, "cardiac")
  p <- .bl_roles(p)
  fam <- c("mortality_30d", "death_date", "followup_days")
  expect_true(all(fam %in% .sr_data_hidden_cols(p, "cardiac_clean")))
  expect_false(any(fam %in% names(sr_sample(p, "cardiac_clean"))))
  # the step that was applied touched the outcome, so the clean table now depends on it
  expect_true(.sr_data_rows_hidden(p, "cardiac_clean"))
  p <- sr_dq_run(p, "cardiac_clean")
  fc <- sr_dq_findings(p, "cardiac_clean")
  expect_identical(.bl_crossing(fc, fam, .bl_cols(p, "cardiac_clean")), character())
  q <- .bl_reopen(p)
  expect_identical(q$state$tables, p$state$tables)
  expect_identical(q$state$roles, p$state$roles)
})

test_that("roles set on a source after a join reach the joined table, and renamed right columns follow", {
  p <- .bl_project(roles = FALSE)
  ids <- .bl_ids(p)
  # the right side has its own outcome-family column whose name is taken on the left
  set.seed(3)
  fu <- data.frame(patient_id = ids, followup_days = sample(30:900, length(ids), replace = TRUE), stringsAsFactors = FALSE)
  p <- .bl_ingest(p, fu, "fu")
  p <- sr_roles_set(p, "fu", c(patient_id = "id", followup_days = "offset"))
  p <- sr_join(p, "cardiac", "fu", by = "patient_id", into = "cj")
  expect_true("followup_days_fu" %in% .bl_cols(p, "cj"))
  expect_true("followup_days_fu" %in% .sr_data_hidden_cols(p, "cj"))
  expect_identical(unname(sr_roles(p, "cj")["followup_days_fu"]), "offset")
  p <- .bl_roles(p)
  expect_true(all(c("mortality_30d", "death_date", "followup_days", "followup_days_fu") %in% .sr_data_hidden_cols(p, "cj")))
  expect_false(any(c("mortality_30d", "death_date") %in% names(sr_sample(p, "cj"))))
  q <- .bl_reopen(p)
  expect_identical(q$state$tables, p$state$tables)
  expect_identical(q$state$roles, p$state$roles)
})


test_that("a failing check stores no detail of its error while blind", {
  p <- .bl_project()
  .sr_dq_register("zz_probe", "plausibility", function(ctx) stop("probe: outcome mean 0.123"))
  withr::defer(.sr_dq_unregister("zz_probe"))
  p <- sr_dq_run(p, "cardiac", checks = "zz_probe")
  f <- sr_dq_findings(p, "cardiac")
  expect_identical(f$check, "check_error")
  expect_false(grepl("0.123", f$message, fixed = TRUE))
  expect_match(f$message, "withheld until the SAP is locked")
  q <- .bl_reopen(p)
  expect_identical(q$state$dq, p$state$dq)
})

test_that("DQ findings stored before the roles were set are re-filtered against the current roles, and the run is stale (A)", {
  p <- .bl_project(roles = FALSE)
  p <- sr_dq_run(p, "cardiac")
  p <- .bl_roles(p)
  fam <- .bl_fam(p, "cardiac")
  cols <- .bl_cols(p, "cardiac")
  f <- sr_dq_findings(p, "cardiac")
  expect_identical(.bl_crossing(f, fam, cols), character())
  expect_false(any(f$check %in% c("missing_pattern", "missing_row")))
  expect_false(any(f$check == "missing_dependence" & f$column %in% fam))
  expect_silent(.sr_assert_blind(f, fam, setdiff(cols, fam)))
  # a family column keeps its own marginal (missing_col), with no other column
  expect_true(any(f$check == "missing_col" & f$column == "death_date"))
  rd <- sr_readiness(p, "cardiac")
  dq_row <- rd$item == "DQ run on current store hash"
  expect_identical(rd$status[dq_row], "block")
  expect_match(rd$detail[dq_row], "roles changed")
  expect_no_error(sr_suggest(p, "cardiac"))
  # reopening gives the same findings and the same readiness
  q <- .bl_reopen(p)
  expect_identical(q$state$dq, p$state$dq)
  expect_identical(sr_dq_findings(q, "cardiac"), f)
  expect_identical(sr_readiness(q, "cardiac"), rd)
  # a run made under the current roles is current again
  q <- sr_dq_run(q, "cardiac")
  rq <- sr_readiness(q, "cardiac")
  expect_identical(rq$status[rq$item == "DQ run on current store hash"], "ok")
  expect_identical(.bl_crossing(sr_dq_findings(q, "cardiac"), fam, cols), character())
  # an old dq.run event without the family fingerprint is stale, and its findings are re-filtered
  q2 <- q
  q2$state$dq$cardiac$family_hash <- NULL
  q2$state$dq$cardiac$family <- NULL
  r2 <- sr_readiness(q2, "cardiac")
  expect_identical(r2$status[r2$item == "DQ run on current store hash"], "block")
  expect_identical(.bl_crossing(sr_dq_findings(q2, "cardiac"), fam, cols), character())
  expect_false(any(sr_dq_findings(q2, "cardiac")$check %in% c("missing_pattern", "missing_row")))
})
