# Tests for profile/query.R: the data-query sheet of the findings decided as recheck (sr_query_sheet), the
# corrected source brought back as a new table version (sr_reimport) and the diff between two versions
# (sr_table_diff). Synthetic data only; everything lives under withr::local_tempdir().

.qy_roles <- c(patient_id = "id", surgery_date = "time_zero", statin_start_date = "exposure_date",
               statin = "exposure", death_date = "event_date", mortality_30d = "outcome")

# The cardiac project with its dictionary, roles and a DQ run.
.qy_ready <- function(env = parent.frame()) {
  p <- .fx_cardiac_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  csv <- file.path(dirname(p$paths$dir), "cardiac_dict.csv")
  .fx_write_csv(sr_sample_cardiac()$dict, csv)
  p <- sr_dict_set(p, "cardiac", sr_dict_read_csv(csv), "csv")
  p <- sr_roles_set(p, "cardiac", .qy_roles)
  sr_dq_run(p, "cardiac")
}

.qy_fid <- function(p, check, column, table = "cardiac") {
  f <- sr_dq_findings(p, table)
  f$id[f$check == check & f$column == column]
}

# Decide `recheck` for the finding of `check` on `column` and return the project.
.qy_recheck <- function(p, check, column, table = "cardiac") {
  ids <- .qy_fid(p, check, column, table)
  for (id in ids) p <- sr_action_decide(p, id, "recheck", table = table)
  p
}

.qy_read <- function(p, path = NULL, sheet = 1L) {
  if (is.null(path)) path <- file.path(p$paths$dir, p$state$queries[[length(p$state$queries)]]$path)
  as.data.frame(readxl::read_excel(path, sheet = sheet, col_types = "text"))
}

.qy_cols <- c("query_id", "record_id", "column", "current_value", "check", "question", "corrected_value", "comment")

.qy_ing <- function(p, df, table) {
  csv <- file.path(dirname(p$paths$dir), paste0(table, ".csv"))
  .fx_write_csv(df, csv)
  sr_ingest(p, csv, table = table,
            options = list(drive_info = function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())))
}

.qy_project <- function(env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  p
}

# ---- the sheet --------------------------------------------------------------------------------------

test_that("the sheet of range on age has the 3 planted rows, with the 8 columns in order", {
  p <- .qy_recheck(.qy_ready(), "range", "age")
  s <- sr_sample_cardiac()$data
  planted <- s$patient_id[s$age > 110]
  expect_length(planted, 3L)
  p2 <- sr_query_sheet(p, "cardiac")
  x <- p2$state$queries[[1L]]
  expect_identical(x$path, paste0("out/queries/cardiac-", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"))
  file <- file.path(p$paths$dir, x$path)
  expect_true(file.exists(file))
  expect_false(file.exists(paste0(file, ".tmp")))
  d <- .qy_read(p2)
  expect_identical(names(d), .qy_cols)
  expect_identical(nrow(d), 3L)
  expect_setequal(d$record_id, planted)
  expect_identical(d$record_id, sort(d$record_id))
  expect_identical(unique(d$column), "age")
  expect_identical(unique(d$check), "range")
  expect_identical(unique(d$current_value), "250")
  expect_identical(d$query_id, sprintf("Q%05d", 1:3))
  expect_true(all(is.na(d$corrected_value)))
  expect_true(all(is.na(d$comment)))
  expect_true(all(nzchar(d$question)))
  expect_true(all(grepl("age", d$question, fixed = TRUE)))
  # the event
  expect_identical(x$rows, 3L)
  expect_identical(x$total, 3L)
  expect_identical(x$sha256, sr_sha256_file(file))
  expect_identical(as.character(unlist(x$finding_ids)), .qy_fid(p, "range", "age"))
  expect_true(sr_project_verify(p2)$ok)
})

test_that("the default is every finding decided as recheck, and nothing undecided or decided otherwise", {
  p <- .qy_ready()
  expect_error(sr_query_sheet(p, "cardiac"), "recheck")
  p <- .qy_recheck(p, "range", "age")
  p <- .qy_recheck(p, "dup_id", "patient_id")
  p <- sr_action_decide(p, .qy_fid(p, "range", "sbp"), "keep_sensitivity", reason = "values kept for a sensitivity analysis")
  d <- .qy_read(sr_query_sheet(p, "cardiac"))
  expect_setequal(unique(d$check), c("range", "dup_id"))
  expect_identical(sum(d$check == "range"), 3L)
  expect_gte(sum(d$check == "dup_id"), 4L)
  expect_false("sbp" %in% d$column)
  # the ids of dup_id are the repeated patient ids
  expect_true(all(d$record_id[d$check == "dup_id"] == d$current_value[d$check == "dup_id"]))
  # an explicit selection
  p2 <- sr_query_sheet(p, "cardiac", finding_ids = .qy_fid(p, "dup_id", "patient_id"),
                       path = file.path(p$paths$dir, "out", "one.xlsx"))
  expect_identical(unique(.qy_read(p2)$check), "dup_id")
  expect_identical(p2$state$queries[[1L]]$path, "out/one.xlsx")
  expect_error(sr_query_sheet(p, "cardiac", finding_ids = "nope"), "unknown finding_id")
})

test_that("a finding without a filter gives one row with no record and no value", {
  p <- .qy_ready()
  ids <- .qy_fid(p, "date_order", "discharge_date")
  expect_gte(length(ids), 1L)
  p <- sr_action_decide(p, ids[1L], "recheck")
  d <- .qy_read(sr_query_sheet(p, "cardiac"))
  expect_identical(nrow(d), 1L)
  expect_true(is.na(d$record_id))
  expect_true(is.na(d$current_value))
  expect_identical(d$column, "discharge_date")
  expect_identical(d$check, "date_order")
})

test_that("a table without an id role numbers the records by an explicit order", {
  p <- .qy_ready()
  p <- sr_roles_set(p, "cardiac", c(mortality_30d = "outcome"))
  p <- .qy_recheck(p, "range", "age")
  d <- .qy_read(sr_query_sheet(p, "cardiac"))
  expect_identical(nrow(d), 3L)
  expect_true(all(grepl("^[0-9]+$", d$record_id)))
  expect_identical(d$record_id, as.character(sort(as.integer(d$record_id))))
  # the number is the position in the table sorted by every column
  sr_store_view(p, "cardiac")
  con <- sr_duck(p)
  cols <- vapply(sr_store_manifest(p, "cardiac")$schema, function(s) s$name, "")
  q <- paste0("SELECT row_number() OVER (ORDER BY ", paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", "),
              ") AS rn, age FROM cardiac")
  all <- sr_duck_query(con, paste0("SELECT rn FROM (", q, ") AS t WHERE age > 110 ORDER BY rn"))
  expect_identical(as.integer(d$record_id), as.integer(all$rn))
})

test_that("a re-export on the same day replaces the file atomically and records both exports", {
  p <- .qy_recheck(.qy_ready(), "range", "age")
  p <- sr_query_sheet(p, "cardiac")
  p <- .qy_recheck(p, "dup_id", "patient_id")
  p <- sr_query_sheet(p, "cardiac")
  expect_length(p$state$queries, 2L)
  a <- p$state$queries[[1L]]
  b <- p$state$queries[[2L]]
  expect_identical(a$path, b$path)
  expect_false(identical(a$sha256, b$sha256))
  expect_identical(b$sha256, sr_sha256_file(file.path(p$paths$dir, b$path)))
  expect_identical(list.files(file.path(p$paths$dir, "out", "queries")), basename(b$path))
  expect_true(sr_project_verify(p)$ok)
})

test_that("a path outside the project, a read-only project and a bad table are refused", {
  p <- .qy_recheck(.qy_ready(), "range", "age")
  outside <- file.path(withr::local_tempdir(), "q.xlsx")
  expect_error(sr_query_sheet(p, "cardiac", path = outside), "inside the project")
  expect_false(file.exists(outside))
  expect_error(sr_query_sheet(p, "Bad Name"), "invalid table name")
  ro <- p
  ro$readonly <- TRUE
  expect_error(sr_query_sheet(ro, "cardiac"), "read-only")
  expect_error(sr_query_sheet(p, "cardiac", path = file.path(p$paths$dir, "out", "q.csv")), "xlsx")
})

test_that("more than 50,000 rows are cut to 50,000 and a summary sheet states the total", {
  p <- .qy_project()
  n <- 50050L
  p <- .qy_ing(p, data.frame(pid = sprintf("P%06d", seq_len(n)), age = rep(250L, n), stringsAsFactors = FALSE), "big")
  p <- sr_roles_set(p, "big", c(pid = "id"))
  p <- sr_dq_run(p, "big", checks = "range")
  id <- .qy_fid(p, "range", "age", table = "big")
  expect_length(id, 1L)
  p <- sr_action_decide(p, id, "recheck", table = "big")
  p <- sr_query_sheet(p, "big")
  x <- p$state$queries[[1L]]
  expect_identical(x$rows, 50000L)
  expect_identical(x$total, n)
  expect_true(isTRUE(x$truncated))
  file <- file.path(p$paths$dir, x$path)
  expect_identical(readxl::excel_sheets(file), c("queries", "summary"))
  d <- .qy_read(p, file)
  expect_identical(nrow(d), 50000L)
  expect_identical(d$record_id[c(1L, 50000L)], c("P000001", "P050000"))
  s <- .qy_read(p, file, sheet = "summary")
  expect_true(any(grepl(as.character(n), unlist(s), fixed = TRUE)))
})

# ---- outcome-blind ----------------------------------------------------------------------------------

test_that("a blind sheet of dup_id has no outcome column and no outcome value", {
  p <- .qy_recheck(.qy_ready(), "dup_id", "patient_id")
  expect_true(sr_blind(p))
  d <- .qy_read(sr_query_sheet(p, "cardiac"))
  expect_gt(nrow(d), 0L)
  expect_false("mortality_30d" %in% names(d))
  expect_false("mortality_30d" %in% d$column)
  expect_false(any(grepl("mortality_30d", unlist(d), fixed = TRUE)))
})

test_that("an outcome value shows only for a completeness or plausibility finding on the outcome itself", {
  f <- .sr_qy_show_value
  expect_true(f(blind = TRUE, is_outcome = TRUE, domain = "completeness"))
  expect_true(f(blind = TRUE, is_outcome = TRUE, domain = "plausibility"))
  expect_false(f(blind = TRUE, is_outcome = TRUE, domain = "conformance"))
  expect_false(f(blind = TRUE, is_outcome = TRUE, domain = "uniqueness"))
  expect_false(f(blind = TRUE, is_outcome = TRUE, domain = "leakage"))
  # any other column, or a locked SAP
  expect_true(f(blind = TRUE, is_outcome = FALSE, domain = "leakage"))
  expect_true(f(blind = FALSE, is_outcome = TRUE, domain = "conformance"))
})

test_that("a completeness finding on the outcome column itself lists its records (blind: no value to show)", {
  p <- .qy_project()
  n <- 400L
  set.seed(7)
  df <- data.frame(pid = sprintf("P%04d", seq_len(n)), age = sample(30:80, n, TRUE), died = rbinom(n, 1L, 0.2),
                   stringsAsFactors = FALSE)
  df$died[sample(n, 40L)] <- NA
  p <- .qy_ing(p, df, "tiny")
  p <- sr_roles_set(p, "tiny", c(pid = "id", died = "outcome"))
  p <- sr_dq_run(p, "tiny")
  f <- sr_dq_findings(p, "tiny")
  mc <- f[f$check == "missing_col" & f$column == "died", , drop = FALSE]
  expect_identical(nrow(mc), 1L)
  expect_identical(mc$domain, "completeness")
  p <- sr_action_decide(p, mc$id, "recheck", reason = "recheck against the source", step = NULL, table = "tiny")
  d <- .qy_read(sr_query_sheet(p, "tiny"))
  expect_identical(nrow(d), 40L)
  expect_true(all(is.na(d$current_value)))
  expect_identical(unique(d$column), "died")
  expect_true(all(d$record_id %in% df$pid[is.na(df$died)]))
})

test_that("a filter that reads the outcome collapses to one row while blind", {
  w <- list(all = list(list(col = "age", op = ">", value = 100), list(col = "died", op = "==", value = 1)))
  expect_true(.sr_qy_hides_rows(w, "died", blind = TRUE, allowed_self = FALSE))
  expect_false(.sr_qy_hides_rows(w, "died", blind = TRUE, allowed_self = TRUE))
  expect_false(.sr_qy_hides_rows(w, "died", blind = FALSE, allowed_self = FALSE))
  expect_false(.sr_qy_hides_rows(list(col = "age", op = ">", value = 100), "died", blind = TRUE, allowed_self = FALSE))
  expect_false(.sr_qy_hides_rows(NULL, "died", blind = TRUE, allowed_self = FALSE))
})

test_that("every question is one templated sentence that names no value", {
  checks <- names(.sr_qy_templates)
  expect_true(all(c("range", "dup_id", "coercion", "unit_mix", "missing_codes") %in% checks))
  for (ck in c(checks, "some_new_check")) {
    q <- .sr_qy_question(ck, "age")
    expect_length(q, 1L)
    expect_true(grepl("age", q, fixed = TRUE))
    expect_false(grepl("[0-9]", q))
  }
  expect_true(grepl("this record", .sr_qy_question("dup_rows", "*"), fixed = TRUE))
})

# ---- the diff ---------------------------------------------------------------------------------------

test_that("sr_table_diff counts added and removed keys, changed rows and cells, and the schema changes", {
  p <- .qy_project()
  old <- data.frame(pid = sprintf("P%02d", 1:10), a = 1:10, b = letters[1:10], c = 1:10, d = 1L,
                    stringsAsFactors = FALSE)
  new <- old[-c(9, 10), ]                       # P09, P10 removed
  new$a[2:3] <- c(20L, 30L)                     # two rows change in a
  new$b[3] <- "z"                               # one of them also in b
  new$c <- as.character(new$c)                  # c changes type (integer -> text) but not its text
  new$e <- "x"                                  # e added
  new$d <- NULL                                 # d removed
  new <- rbind(new, data.frame(pid = c("P11", "P12"), a = 1L, b = "k", c = c("9", "x"), e = "y", stringsAsFactors = FALSE))
  p <- .qy_ing(p, old, "v0")
  p <- .qy_ing(p, new, "v1")
  p <- sr_roles_set(p, "v0", c(pid = "id"))
  df <- sr_table_diff(p, "v0", "v1")
  expect_identical(df$added, 2)
  expect_identical(df$removed, 2)
  expect_identical(df$changed_rows, 2)
  expect_identical(df$changed_cells$column, c("a", "b"))
  expect_identical(df$changed_cells$n, c(2, 1))
  expect_identical(df$schema_changes$added, "e")
  expect_identical(df$schema_changes$removed, "d")
  expect_identical(df$schema_changes$type_changed$column, "c")
  expect_identical(names(df$schema_changes$type_changed), c("column", "old_type", "new_type"))
  expect_identical(df$key, "pid")
  # no id role and no key: an error; an explicit key works
  expect_error(sr_table_diff(p, "v1", "v0"), "id role")
  expect_identical(sr_table_diff(p, "v1", "v0", key = "pid")$added, 2)
  expect_error(sr_table_diff(p, "v0", "v1", key = "nope"), "not a column")
  expect_error(sr_table_diff(p, "v0", "missing_table"), "unknown table")
})

test_that("a diff of identical tables is empty, also when the key repeats", {
  p <- .qy_project()
  df <- data.frame(pid = c("A", "A", "B", "C"), v = c(1L, 1L, 2L, 3L), stringsAsFactors = FALSE)   # A is an exact repeat
  p <- .qy_ing(p, df, "t0")
  p <- .qy_ing(p, df, "t1")
  d <- sr_table_diff(p, "t0", "t1", key = "pid")
  expect_identical(c(d$added, d$removed, d$changed_rows), c(0, 0, 0))
  expect_identical(nrow(d$changed_cells), 0L)
  expect_identical(names(d$changed_cells), c("column", "n"))
  expect_identical(d$duplicate_keys, list(old = 1, new = 1))
  # one of the two repeated rows is corrected
  df2 <- df
  df2$v[2L] <- 9L
  p <- .qy_ing(p, df2, "t2")
  d2 <- sr_table_diff(p, "t0", "t2", key = "pid")
  expect_identical(d2$changed_rows, 1)
  expect_identical(d2$changed_cells$n, 1)
})

# ---- the re-import ----------------------------------------------------------------------------------

test_that("a corrected source becomes <table>_r1, with the diff, the roles and the dictionary", {
  p <- .qy_recheck(.qy_ready(), "range", "age")
  s <- sr_sample_cardiac()$data
  bad <- which(s$age > 110)
  fixed <- s
  fixed$age[bad] <- c(61L, 72L, 58L)
  csv <- file.path(dirname(p$paths$dir), "cardiac_fixed.csv")
  .fx_write_csv(fixed, csv)
  n_ev <- length(sr_events_read(p$paths$events))
  p2 <- sr_reimport(p, "cardiac", csv)
  expect_true("cardiac_r1" %in% sr_store_tables(p2))
  expect_identical(p2$state$tables$cardiac$active, "cardiac_r1")
  ev <- sr_events_read(p2$paths$events)
  expect_identical(vapply(ev[(n_ev + 1L):length(ev)], function(e) e$type, ""),
                   c("data.imported", "roles.set", "dict.set", "data.reimported"))
  r <- p2$state$reimports$cardiac_r1
  expect_identical(r$old, "cardiac")
  expect_identical(r$version, 1L)
  expect_identical(r$diff$changed_rows, 3L)
  expect_identical(r$diff$added, 0L)
  d <- sr_table_diff(p2, "cardiac", "cardiac_r1")
  expect_identical(d$changed_rows, 3)
  expect_identical(d$changed_cells$n[d$changed_cells$column == "age"], 3)
  expect_identical(d$changed_cells$column, "age")
  expect_identical(d$added, 0)
  expect_identical(d$removed, 0)
  expect_identical(nrow(d$schema_changes$type_changed), 0L)
  # roles and the dictionary came along; the original is untouched and readable
  expect_identical(sr_roles(p2, "cardiac_r1"), sr_roles(p2, "cardiac"))
  expect_identical(sr_dict_get(p2, "cardiac_r1")$variable, sr_dict_get(p2, "cardiac")$variable)
  expect_gt(max(sr_table_page(p2, "cardiac", 0, 1000, order_by = "patient_id")$age), 110)
  expect_lte(max(sr_table_page(p2, "cardiac_r1", 0, 1000, order_by = "patient_id")$age), 110)
  expect_true(sr_project_verify(p2)$ok)
})

test_that("each re-import is the next version and is compared with the active one", {
  p <- .qy_ready()
  s <- sr_sample_cardiac()$data
  one <- s
  one$age[which(s$age > 110)] <- 60L
  csv1 <- file.path(dirname(p$paths$dir), "cardiac_1.csv")
  .fx_write_csv(one, csv1)
  p <- sr_reimport(p, "cardiac", csv1)
  two <- one
  two$sbp[5L] <- two$sbp[5L] + 1L
  two <- two[-1L, ]
  csv2 <- file.path(dirname(p$paths$dir), "cardiac_2.csv")
  .fx_write_csv(two, csv2)
  p <- sr_reimport(p, "cardiac", csv2)
  expect_identical(p$state$tables$cardiac$active, "cardiac_r2")
  r <- p$state$reimports$cardiac_r2
  expect_identical(r$old, "cardiac_r1")
  expect_identical(r$version, 2L)
  expect_identical(r$diff$changed_rows, 1L)
  expect_identical(r$diff$removed, 1L)
  expect_true(all(c("cardiac", "cardiac_r1", "cardiac_r2") %in% sr_store_tables(p)))
  # replay gives the same state
  sr_project_close(p)
  q <- sr_project_open(p$paths$dir)
  withr::defer(sr_project_close(q))
  expect_identical(q$state$tables$cardiac$active, "cardiac_r2")
  expect_identical(q$state$reimports$cardiac_r2$diff$removed, 1L)
})

test_that("a re-import needs a key, and refuses before ingesting anything", {
  p <- .qy_project()
  df <- data.frame(pid = c("A", "B"), v = 1:2, stringsAsFactors = FALSE)
  p <- .qy_ing(p, df, "t0")
  csv <- file.path(dirname(p$paths$dir), "t0_fixed.csv")
  .fx_write_csv(df, csv)
  n <- length(sr_events_read(p$paths$events))
  expect_error(sr_reimport(p, "t0", csv), "id role")
  expect_identical(length(sr_events_read(p$paths$events)), n)
  expect_false("t0_r1" %in% sr_store_tables(p))
  p2 <- sr_reimport(p, "t0", csv, key = "pid")
  expect_identical(p2$state$reimports$t0_r1$diff$changed_rows, 0L)
  expect_error(sr_reimport(p, "nope", csv), "unknown table")
  ro <- p
  ro$readonly <- TRUE
  expect_error(sr_reimport(ro, "t0", csv, key = "pid"), "read-only")
})
