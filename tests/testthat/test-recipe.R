# Tests for profile/recipe.R: the filter DSL compiled to SQL, the recipe steps with undo and branching,
# and the compile / apply to a clean store table. Synthetic data only; everything lives under
# withr::local_tempdir().

.rc_drive <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())

# The roles of the cardiac study with the 30-day mortality as the outcome.
.rc_roles <- c(patient_id = "id", surgery_date = "time_zero", statin_start_date = "exposure_date",
               statin = "exposure", death_date = "event_date", mortality_30d = "outcome")

# Ingest a data frame as `table` into an open project.
.rc_ingest <- function(p, df, table) {
  csv <- file.path(dirname(p$paths$dir), paste0(table, ".csv"))
  .fx_write_csv(df, csv)
  sr_ingest(p, csv, table = table, options = list(drive_info = .rc_drive))
}

# A throwaway project with `df` ingested as `table`.
.rc_small <- function(df, table = "small", roles = NULL, env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  p <- .rc_ingest(p, df, table)
  if (length(roles)) p <- sr_roles_set(p, table, roles)
  p
}

# The cardiac project with its dictionary and the study roles.
.rc_cardiac <- function(env = parent.frame()) {
  p <- .fx_cardiac_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  csv <- file.path(dirname(p$paths$dir), "cardiac_dict.csv")
  .fx_write_csv(sr_sample_cardiac()$dict, csv)
  p <- sr_dict_set(p, "cardiac", sr_dict_read_csv(csv), "csv")
  sr_roles_set(p, "cardiac", .rc_roles)
}

# Every row of a store table in a total order.
.rc_rows <- function(p, table) {
  m <- sr_store_manifest(p, table)
  cols <- vapply(m$schema, function(s) s$name, "")
  sr_table_page(p, table, 0, 1000, order_by = cols)
}

# The number of rows of a store table that match a filter.
.rc_count <- function(p, table, where = NULL) {
  con <- sr_duck(p)
  sr_store_view(p, table)
  cols <- vapply(sr_store_manifest(p, table)$schema, function(s) s$name, "")
  w <- sr_where_sql(con, where, cols)
  as.numeric(sr_duck_query(con, sprintf("SELECT count(*) AS n FROM %s WHERE %s",
                                        DBI::dbQuoteIdentifier(con, table), w))$n)
}

.rc_step <- function(op, args, reason = "", finding_id = NA_character_) {
  list(op = op, args = args, reason = reason, finding_id = finding_id)
}

.rc_n_events <- function(p) length(sr_events_read(p$paths$events))

.rc_drop <- function(col, value, reason = "r") {
  .rc_step("drop_cases", list(where = list(col = col, op = "==", value = value)), reason)
}

# ---- the filter DSL ---------------------------------------------------------------------------------

test_that("sr_where_sql quotes literals safely and refuses unknown columns and ops", {
  p <- .rc_small(data.frame(name = c("O'Brien", "Smith", "x; DROP TABLE small; --", NA),
                            x = c(1, 2, NA, 4), stringsAsFactors = FALSE))
  con <- sr_duck(p)
  sr_store_view(p, "small")
  cols <- c("name", "x")
  n <- function(w) {
    as.numeric(sr_duck_query(con, sprintf("SELECT count(*) AS n FROM small WHERE %s", sr_where_sql(con, w, cols)))$n)
  }
  expect_error(sr_where_sql(con, list(col = "a; DROP TABLE x", op = "==", value = "1"), cols), "unknown column")
  expect_error(sr_where_sql(con, list(col = "x", op = "~~", value = 1), cols), "unknown filter op")
  expect_error(sr_where_sql(con, list(col = "x", op = "DROP", value = 1), cols), "unknown filter op")
  # a quote inside a value is data, not syntax
  expect_identical(n(list(col = "name", op = "==", value = "O'Brien")), 1)
  expect_identical(n(list(col = "name", op = "==", value = "x; DROP TABLE small; --")), 1)
  expect_identical(n(list(col = "name", op = "==", value = "' OR '1'='1")), 0)
  expect_identical(n(list(col = "name", op = "in", value = c("Smith", "O'Brien"))), 2)
  expect_identical(n(list(col = "name", op = "not_in", value = c("Smith", "O'Brien"))), 1)   # NULL is not matched
  expect_true("small" %in% DBI::dbListTables(con))                                           # still there
  # identifiers are quoted
  expect_identical(as.character(sr_where_sql(con, list(col = "x", op = "is_na"), cols)), "(\"x\" IS NULL)")
})

test_that("sr_where_sql compiles every op, NULL, combinators, empty sets and dates", {
  df <- data.frame(x = c(1, 2, 3, NA, 5), d = c("2020-01-05", "2021-06-01", "2026-12-31", "2019-03-03", NA),
                   s = c("a", "b", "c", "d", "e"), stringsAsFactors = FALSE)
  p <- .rc_small(df)
  con <- sr_duck(p)
  sr_store_view(p, "small")
  cols <- c("x", "d", "s")
  n <- function(w) {
    as.numeric(sr_duck_query(con, sprintf("SELECT count(*) AS n FROM small WHERE %s", sr_where_sql(con, w, cols)))$n)
  }
  expect_identical(n(NULL), 5)
  expect_identical(n(list(col = "x", op = "==", value = 2)), 1)
  expect_identical(n(list(col = "x", op = "!=", value = 2)), 3)             # NULL is not "!= 2"
  expect_identical(n(list(col = "x", op = "<", value = 3)), 2)
  expect_identical(n(list(col = "x", op = "<=", value = 3)), 3)
  expect_identical(n(list(col = "x", op = ">", value = 3)), 1)
  expect_identical(n(list(col = "x", op = ">=", value = 3)), 2)
  expect_identical(n(list(col = "x", op = "between", value = c(2, 3))), 2)
  expect_identical(n(list(col = "x", op = "is_na")), 1)
  expect_identical(n(list(col = "x", op = "not_na")), 4)
  expect_identical(n(list(col = "x", op = "in", value = c(1, 5, 99))), 2)
  # empty sets are valid constant predicates
  expect_identical(n(list(col = "x", op = "in", value = numeric())), 0)
  expect_identical(n(list(col = "x", op = "not_in", value = numeric())), 5)
  expect_identical(n(list(col = "s", op = "in", value = character())), 0)
  # dates are DATE literals
  w <- sr_where_sql(con, list(col = "d", op = ">=", value = "2021-01-01"), cols)
  expect_match(as.character(w), "DATE '2021-01-01'", fixed = TRUE)
  expect_identical(n(list(col = "d", op = ">=", value = "2021-01-01")), 2)
  expect_identical(n(list(col = "d", op = "between", value = c("2019-01-01", "2020-12-31"))), 2)
  expect_identical(n(list(col = "d", op = "in", value = c("2020-01-05", "2019-03-03"))), 2)
  # combinators nest
  nested <- list(all = list(list(col = "x", op = ">=", value = 2),
                            list(any = list(list(col = "s", op = "==", value = "b"),
                                            list(col = "s", op = "==", value = "e")))))
  expect_identical(n(nested), 2)
  # the shape that comes back from a JSON round trip (values as lists) compiles the same
  rt <- .sr_parse_json(.sr_json(list(all = list(list(col = "x", op = "in", value = c(1, 5)),
                                                list(col = "d", op = "not_na")))))
  expect_identical(n(rt), 1)
  # malformed filters
  expect_error(sr_where_sql(con, list(col = "x", op = "=="), cols), "needs a value")
  expect_error(sr_where_sql(con, list(col = "x", op = "==", value = NA_real_), cols), "missing value")
  expect_error(sr_where_sql(con, list(col = "x", op = "==", value = Inf), cols), "finite")
  expect_error(sr_where_sql(con, list(col = "x", op = "between", value = 1), cols), "two values")
  expect_error(sr_where_sql(con, list(col = "x", op = "==", value = c(1, 2)), cols), "one value")
  expect_error(sr_where_sql(con, list(all = list()), cols), "empty")
  expect_error(sr_where_sql(con, list(all = list(), any = list()), cols), "exactly one")
  expect_error(sr_where_sql(con, list(foo = 1), cols), "not a valid filter")
  expect_error(sr_where_sql(con, list(col = "x", op = "==", value = 1, extra = 2), cols), "unexpected")
})

# ---- steps run in order -----------------------------------------------------------------------------

test_that("steps compile as a sequential chain: later steps see earlier ones", {
  p <- .rc_small(data.frame(a = c(1, 2, 3, NA), g = c("x", "y", "x", "y"), stringsAsFactors = FALSE))
  W <- function(col, op, value) list(col = col, op = op, value = value)
  p <- sr_recipe_add(p, "small", .rc_step("rename", list(col = "a", to = "b")))
  p <- sr_recipe_add(p, "small", .rc_step("convert_unit", list(col = "b", where = W("g", "==", "x"), factor = 10)))
  p <- sr_recipe_add(p, "small", .rc_step("set_missing", list(col = "b", where = W("b", ">", 20))))
  p <- sr_recipe_add(p, "small", .rc_step("set_value", list(col = "g", where = W("b", "==", 2), value = "z")))
  p <- sr_recipe_add(p, "small", .rc_step("drop_cases", list(where = W("g", "==", "z")), reason = "drop z"))
  p <- sr_recipe_add(p, "small", .rc_step("drop_cases", list(where = W("b", ">", 5)), reason = "drop big"))
  expect_length(sr_recipe_steps(p), 6L)
  sql <- sr_recipe_sql(p, "small")
  expect_match(sql, "^WITH ")
  expect_match(sql, "CASE WHEN", fixed = TRUE)
  expect_match(sql, "NOT coalesce", fixed = TRUE)
  p <- sr_recipe_apply(p, "small")
  r <- .rc_rows(p, "small_clean")
  # s1 rename; s2 b = 10, 2, 30, NA; s3 30 -> NULL; s4 g[b == 2] = z; s5 drops the z row; s6 drops b > 5 and
  # KEEPS the rows where b is NULL (the filter is NULL there)
  expect_identical(names(r), c("b", "g"))
  expect_identical(nrow(r), 2L)
  expect_true(all(is.na(r$b)))
  expect_setequal(r$g, c("x", "y"))
  ct <- sr_recipe_counts(p, "small")
  expect_identical(names(ct), c("step_id", "rows_before", "rows_after", "cells_changed", "new_nulls"))
  expect_identical(ct$step_id, vapply(sr_recipe_steps(p), function(s) s$id, ""))
  expect_equal(ct$rows_before, c(4, 4, 4, 4, 4, 3))
  expect_equal(ct$rows_after, c(4, 4, 4, 4, 3, 2))
  expect_equal(ct$cells_changed, c(0, 2, 1, 1, 0, 0))
  expect_true(all(is.na(ct$new_nulls)))
})

test_that("drop_cases keeps the rows where its filter is NULL", {
  p <- .rc_small(data.frame(a = c(1, 2, NA, 4)))
  p <- sr_recipe_add(p, "small", .rc_step("drop_cases", list(where = list(col = "a", op = ">", value = 1)), "gt1"))
  p <- sr_recipe_apply(p, "small")
  r <- .rc_rows(p, "small_clean")
  expect_identical(nrow(r), 2L)                       # 1 and the NA stay
  expect_identical(sum(is.na(r$a)), 1L)
  expect_false(any(r$a > 1, na.rm = TRUE))
})

test_that("recode, cast and new_nulls", {
  p <- .rc_small(data.frame(sex = c("M", "F", "male", "F", NA), v = c("1", "2", "abc", "4", "oops"),
                            k = c(1, 2, 999, 4, 999), stringsAsFactors = FALSE))
  p <- sr_recipe_add(p, "small", .rc_step("recode", list(col = "sex", map = list(male = "M", F = "female"))))
  p <- sr_recipe_add(p, "small", .rc_step("recode", list(col = "k", map = list("999" = NA))))
  p <- sr_recipe_add(p, "small", .rc_step("cast", list(col = "v", type = "INTEGER")))
  p <- sr_recipe_apply(p, "small")
  r <- .rc_rows(p, "small_clean")
  expect_identical(sum(r$sex == "M", na.rm = TRUE), 2L)
  expect_identical(sum(r$sex == "female", na.rm = TRUE), 2L)
  expect_identical(sum(is.na(r$sex)), 1L)
  expect_identical(sum(is.na(r$k)), 2L)
  expect_identical(sum(is.na(r$v)), 2L)
  expect_match(sr_store_manifest(p, "small_clean")$schema[[2]]$type, "INT")
  ct <- sr_recipe_counts(p, "small")
  expect_equal(ct$new_nulls, c(NA, NA, 2))
  expect_equal(ct$cells_changed, c(3, 2, 3))
  expect_error(sr_recipe_add(p, "small", .rc_step("cast", list(col = "v", type = "INTEGER; DROP TABLE x"))), "type")
})

# ---- validation at add time -------------------------------------------------------------------------

test_that("steps are validated when they are added and record nothing on failure", {
  p <- .rc_small(data.frame(a = 1:3, b = 4:6, c = 7:9), roles = c(a = "id", b = "outcome", c = "covariate"))
  n0 <- .rc_n_events(p)
  add <- function(...) sr_recipe_add(p, "small", .rc_step(...))
  expect_error(add("explode", list(col = "c")), "unknown recipe op")
  expect_error(add("set_value", list(col = "nope", where = NULL, value = 1)), "unknown column")
  expect_error(add("set_value", list(col = "c", where = list(col = "zz", op = "is_na"), value = 1)), "unknown column")
  expect_error(add("drop_cases", list(where = list(col = "c", op = "is_na"))), "reason")
  expect_error(add("drop_cases", list(where = NULL), "all"), "where")
  expect_error(add("drop_var", list(col = "c")), "reason")
  expect_error(add("drop_var", list(col = "a"), "no"), "protected")
  expect_error(add("drop_var", list(col = "b"), "no"), "protected")
  expect_error(add("rename", list(col = "a", to = "aa")), "role")
  expect_error(add("rename", list(col = "c", to = "a")), "already")
  expect_error(add("rename", list(col = "c", to = "bad name; x")), "name")
  expect_error(add("convert_unit", list(col = "c", where = NULL, factor = 0)), "factor")
  expect_error(add("convert_unit", list(col = "c", where = NULL, factor = NA_real_)), "factor")
  expect_error(add("set_value", list(col = "c", where = NULL, value = NA)), "set_missing")
  expect_error(add("recode", list(col = "c", map = list())), "map")
  expect_error(sr_recipe_add(p, "nope", .rc_step("set_missing", list(col = "c", where = NULL))),
               "not in the store|unknown table")
  expect_identical(.rc_n_events(p), n0)
  expect_length(sr_recipe_steps(p), 0L)
  # a step that is fine
  p <- sr_recipe_add(p, "small", .rc_step("drop_var", list(col = "c"), "not needed"))
  expect_identical(.rc_n_events(p), n0 + 1L)
  # a dropped column is gone for later steps
  expect_error(sr_recipe_add(p, "small", .rc_step("set_missing", list(col = "c", where = NULL))), "unknown column")
})

test_that("a branch is bound to the table of its first step", {
  p <- .rc_small(data.frame(a = 1:3, b = 4:6), table = "one")
  p <- .rc_ingest(p, data.frame(a = 1:3, b = 4:6), "two")
  expect_null(p$state$recipe)
  p <- sr_recipe_add(p, "one", .rc_step("set_missing", list(col = "a", where = NULL)))
  expect_error(sr_recipe_add(p, "two", .rc_step("set_missing", list(col = "a", where = NULL))), "bound to table 'one'")
  expect_error(sr_recipe_sql(p, "two"), "bound to table 'one'")
  expect_error(sr_recipe_apply(p, "two"), "bound to table 'one'")
  p <- sr_recipe_branch(p, "fresh")
  expect_identical(p$state$recipe$branches$fresh$table, "one")
  p <- sr_recipe_checkout(p, "fresh")
  expect_length(sr_recipe_steps(p), 1L)
})

# ---- undo, branch, checkout -------------------------------------------------------------------------

test_that("undo marks the last active step undone, branches copy and checkout switches", {
  p <- .rc_small(data.frame(a = c(1, 2, 3, 4), b = c(5, 6, 7, 8)))
  expect_error(sr_recipe_undo(p), "nothing to undo")
  p <- sr_recipe_add(p, "small", .rc_drop("a", 1))
  p <- sr_recipe_add(p, "small", .rc_drop("a", 2))
  p <- sr_recipe_add(p, "small", .rc_drop("a", 3))
  ids <- vapply(sr_recipe_steps(p), function(x) x$id, "")
  expect_length(unique(ids), 3L)
  p <- sr_recipe_undo(p)
  expect_identical(vapply(sr_recipe_steps(p), function(x) x$id, ""), ids[1:2])
  st <- p$state$recipe$branches$main$steps
  expect_length(st, 3L)                                  # marked, not deleted
  expect_identical(vapply(st, function(x) isTRUE(x$undone), NA), c(FALSE, FALSE, TRUE))
  expect_identical(p$state$recipe$branches$main$table, "small")
  expect_identical(p$state$recipe$current, "main")
  p <- sr_recipe_undo(p)
  p <- sr_recipe_undo(p)
  expect_error(sr_recipe_undo(p), "nothing to undo")
  expect_length(sr_recipe_steps(p), 0L)
  # a step added after undos gets a fresh id
  p <- sr_recipe_add(p, "small", .rc_drop("a", 4))
  expect_false(sr_recipe_steps(p)[[1L]]$id %in% ids)
  p <- sr_recipe_undo(p)
  p <- sr_recipe_add(p, "small", .rc_drop("a", 1))
  p <- sr_recipe_add(p, "small", .rc_drop("a", 2))

  # branch copies the active steps (up to from_step) and does not switch
  p <- sr_recipe_branch(p, "head", from_step = sr_recipe_steps(p)[[1L]]$id)
  expect_identical(p$state$recipe$current, "main")
  expect_length(sr_recipe_steps(p, "head"), 1L)
  expect_length(sr_recipe_steps(p, "main"), 2L)
  p <- sr_recipe_branch(p, "alt")
  expect_length(sr_recipe_steps(p, "alt"), 2L)
  expect_error(sr_recipe_branch(p, "alt"), "already exists")
  expect_error(sr_recipe_branch(p, "bad name"), "branch name")
  expect_error(sr_recipe_branch(p, "x", from_step = "s999"), "not an active step")
  expect_error(sr_recipe_checkout(p, "nope"), "no such branch")
  expect_error(sr_recipe_steps(p, "nope"), "no such branch")
  p <- sr_recipe_checkout(p, "alt")
  expect_identical(p$state$recipe$current, "alt")
  p <- sr_recipe_add(p, "small", .rc_drop("a", 4))
  expect_length(sr_recipe_steps(p, "alt"), 3L)
  expect_length(sr_recipe_steps(p, "main"), 2L)
  p <- sr_recipe_undo(p)                                  # undo acts on the current branch only
  expect_length(sr_recipe_steps(p, "alt"), 2L)
  expect_length(sr_recipe_steps(p, "main"), 2L)
})

# ---- apply ------------------------------------------------------------------------------------------

test_that("apply materialises a recipe table, replaces it on re-apply, and refuses other targets", {
  p <- .rc_small(data.frame(a = c(1, 2, 3, 4)))
  p <- sr_recipe_add(p, "small", .rc_drop("a", 1))
  p <- sr_recipe_apply(p, "small")
  e <- p$state$tables$small_clean
  expect_identical(e$format, "recipe")
  expect_identical(e$recipe_sql, sr_recipe_sql(p, "small"))
  expect_identical(e$recipe_hash, .sr_sha256(sr_recipe_sql(p, "small")))
  expect_identical(e$derived_from, list("small"))
  expect_equal(e$store$rows, 3)
  expect_identical(e$store_hash, sr_store_hash(sr_store_manifest(p, "small_clean")))
  expect_identical(sr_source_status(p, "small_clean")$status, "derived")
  # the same recipe into another name gives the same bytes (a total order is part of the query)
  p <- sr_recipe_apply(p, "small", into = "again")
  expect_identical(p$state$tables$again$store_hash, e$store_hash)
  # re-applying the same target after another step replaces it
  p <- sr_recipe_add(p, "small", .rc_drop("a", 2))
  p <- sr_recipe_apply(p, "small")
  expect_equal(p$state$tables$small_clean$store$rows, 2)
  expect_identical(nrow(.rc_rows(p, "small_clean")), 2L)
  expect_false(identical(p$state$tables$small_clean$recipe_hash, e$recipe_hash))
  # the source is untouched; other targets are refused
  expect_equal(p$state$tables$small$store$rows, 4)
  expect_error(sr_recipe_apply(p, "small", into = "small"), "must differ")
  expect_error(sr_recipe_apply(p, "small", into = "Bad Name"), "invalid table name")
  p <- sr_join(p, "small", "again", c(a = "a"), type = "inner", into = "joined")
  expect_error(sr_recipe_apply(p, "small", into = "joined"), "already exists")
  # no staging directory is left behind
  expect_false(dir.exists(file.path(sr_project_paths(p$paths$dir)$data, "ingest", "small_clean")))
})

test_that("a recipe with no active step copies the table; dropping every column is refused", {
  p <- .rc_small(data.frame(a = c(1, 2), b = c(3, 4)))
  expect_identical(sr_recipe_sql(p, "small"), "SELECT * FROM \"small\"")
  p0 <- sr_recipe_apply(p, "small")
  expect_equal(p0$state$tables$small_clean$store$rows, 2)
  expect_identical(nrow(sr_recipe_counts(p, "small")), 0L)
  p <- sr_recipe_add(p, "small", .rc_step("drop_var", list(col = "a"), "r"))
  expect_error(sr_recipe_add(p, "small", .rc_step("drop_var", list(col = "b"), "r")), "every column")
})

# ---- the cardiac table ------------------------------------------------------------------------------

test_that("converting the pounds removes the unit_mix finding; drop_cases removes the age 250 rows", {
  p <- .rc_cardiac()
  p <- sr_dq_run(p, "cardiac", checks = "unit_mix")
  f <- sr_dq_findings(p, "cardiac")
  fw <- f[f$check == "unit_mix" & f$column == "weight_kg", ]
  expect_identical(nrow(fw), 1L)
  lb <- fw$where[[1L]]
  n_lb <- fw$n_affected
  expect_gte(n_lb, 3L)

  p <- sr_recipe_add(p, "cardiac", .rc_step("convert_unit", list(col = "weight_kg", where = lb, factor = 1 / 2.20462),
                                           "pounds entered as kilograms", finding_id = fw$id))
  expect_identical(sr_recipe_steps(p)[[1L]]$finding_id, fw$id)
  p <- sr_recipe_apply(p, "cardiac")
  n0 <- p$state$tables$cardiac$store$rows
  expect_equal(p$state$tables$cardiac_clean$store$rows, n0)
  ct <- sr_recipe_counts(p, "cardiac")
  expect_equal(ct$cells_changed, n_lb)
  expect_equal(.rc_count(p, "cardiac_clean", list(col = "weight_kg", op = ">", value = 200)), 0)
  p <- sr_dq_run(p, "cardiac_clean", checks = "unit_mix")
  f2 <- sr_dq_findings(p, "cardiac_clean")
  expect_false("weight_kg" %in% f2$column[f2$check == "unit_mix"])

  # three rows carry an age of 250
  a250 <- list(col = "age", op = "==", value = 250)
  expect_equal(.rc_count(p, "cardiac", a250), 3)
  p <- sr_recipe_add(p, "cardiac", .rc_step("drop_cases", list(where = a250), "implausible age"))
  p <- sr_recipe_apply(p, "cardiac")
  expect_equal(p$state$tables$cardiac_clean$store$rows, n0 - 3)
  ct <- sr_recipe_counts(p, "cardiac")
  expect_equal(ct$rows_before, c(n0, n0))
  expect_equal(ct$rows_after, c(n0, n0 - 3))
  expect_equal(.rc_count(p, "cardiac_clean", a250), 0)

  # undo, then apply: the earlier row count
  p <- sr_recipe_undo(p)
  p <- sr_recipe_apply(p, "cardiac")
  expect_equal(p$state$tables$cardiac_clean$store$rows, n0)
})

test_that("protected roles cannot be dropped and the outcome cannot be used to exclude cases", {
  p <- .rc_cardiac()
  expect_error(sr_recipe_add(p, "cardiac", .rc_step("drop_var", list(col = "patient_id"), "not needed")), "protected")
  expect_error(sr_recipe_add(p, "cardiac", .rc_step("drop_var", list(col = "mortality_30d"), "not needed")), "protected")
  expect_error(sr_recipe_add(p, "cardiac", .rc_step("rename", list(col = "patient_id", to = "pid"))), "role")
  n0 <- .rc_n_events(p)
  dead <- list(col = "mortality_30d", op = "==", value = 1)
  expect_error(sr_recipe_add(p, "cardiac", .rc_step("drop_cases", list(where = dead), "drop deaths")),
               "outcome-dependent exclusion")
  # at any depth
  deep <- list(all = list(list(col = "age", op = ">", value = 60),
                          list(any = list(dead, list(col = "sbp", op = "is_na")))))
  expect_error(sr_recipe_add(p, "cardiac", .rc_step("drop_cases", list(where = deep), "x")),
               "outcome-dependent exclusion")
  expect_error(sr_recipe_add(p, "cardiac",
                             .rc_step("drop_cases", list(where = dead, confirm_outcome_dependent = FALSE), "x")),
               "outcome-dependent exclusion")
  expect_identical(.rc_n_events(p), n0)
  # a filter on another column needs no confirmation
  p <- sr_recipe_add(p, "cardiac", .rc_drop("age", 250, "age"))
  # confirmed
  p <- sr_recipe_add(p, "cardiac",
                     .rc_step("drop_cases", list(where = dead, confirm_outcome_dependent = TRUE), "deaths, confirmed"))
  p <- sr_recipe_apply(p, "cardiac")
  expect_equal(.rc_count(p, "cardiac_clean", dead), 0)
  # a role set AFTER the step was recorded is honoured at compile time
  q <- .rc_small(data.frame(y = c(0, 1, 0, 1), x = 1:4))
  q <- sr_recipe_add(q, "small", .rc_drop("y", 1))
  q <- sr_roles_set(q, "small", c(y = "outcome"))
  expect_error(sr_recipe_sql(q, "small"), "outcome-dependent exclusion")
  q <- sr_recipe_undo(q)
  expect_match(sr_recipe_sql(q, "small"), "SELECT")
})

test_that("a branch alt, then main, applies the same result as main did", {
  p <- .rc_cardiac()
  n0 <- p$state$tables$cardiac$store$rows
  p <- sr_recipe_add(p, "cardiac", .rc_drop("age", 250, "age"))
  p <- sr_recipe_apply(p, "cardiac", into = "main_clean")
  p <- sr_recipe_branch(p, "alt")
  p <- sr_recipe_checkout(p, "alt")
  p <- sr_recipe_add(p, "cardiac", .rc_step("drop_cases", list(where = list(col = "sbp", op = ">", value = 150)), "high sbp"))
  p <- sr_recipe_apply(p, "cardiac", into = "alt_clean")
  expect_lt(p$state$tables$alt_clean$store$rows, p$state$tables$main_clean$store$rows)
  p <- sr_recipe_checkout(p, "main")
  p <- sr_recipe_apply(p, "cardiac", into = "main_again")
  expect_equal(p$state$tables$main_again$store$rows, n0 - 3)
  expect_identical(p$state$tables$main_again$store_hash, p$state$tables$main_clean$store_hash)
  expect_identical(p$state$tables$main_again$recipe_hash, p$state$tables$main_clean$recipe_hash)
  expect_false(identical(p$state$tables$alt_clean$recipe_hash, p$state$tables$main_clean$recipe_hash))
})

test_that("the recipe survives a reopen and replays identically", {
  p <- .rc_cardiac()
  p <- sr_recipe_add(p, "cardiac", .rc_drop("age", 250, "age"))
  p <- sr_recipe_add(p, "cardiac", .rc_step("recode", list(col = "sex", map = list(M = "male")), "labels"))
  p <- sr_recipe_undo(p)
  p <- sr_recipe_branch(p, "alt")
  p <- sr_recipe_checkout(p, "alt")
  p <- sr_recipe_add(p, "cardiac",
                     .rc_step("set_missing", list(col = "sbp", where = list(col = "sbp", op = ">", value = 200))))
  p <- sr_recipe_apply(p, "cardiac")
  dir <- p$paths$dir
  sql <- sr_recipe_sql(p, "cardiac")
  sr_duck_close(p)
  sr_project_close(p)
  q <- sr_project_open(dir)
  withr::defer(sr_duck_close(q))
  withr::defer(sr_project_close(q))
  expect_identical(q$state$recipe, p$state$recipe)
  expect_identical(q$state$tables$cardiac_clean, p$state$tables$cardiac_clean)
  expect_identical(sr_recipe_sql(q, "cardiac"), sql)
  expect_identical(vapply(sr_recipe_steps(q), function(s) s$op, ""), c("drop_cases", "set_missing"))
  expect_identical(q$state$recipe$current, "alt")
  expect_identical(nrow(sr_recipe_counts(q, "cardiac")), 2L)
  expect_gt(q$state$tables$cardiac_clean$store$rows, 1000L)
})

# ---- the DQ filters ---------------------------------------------------------------------------------

test_that("every filter a full DQ run emits compiles through sr_where_sql and executes", {
  p <- .rc_cardiac()
  p <- sr_dq_run(p, "cardiac")
  f <- sr_dq_findings(p, "cardiac")
  con <- sr_duck(p)
  sr_store_view(p, "cardiac")
  man <- sr_store_manifest(p, "cardiac")
  cols <- vapply(man$schema, function(s) s$name, "")
  types <- stats::setNames(vapply(man$schema, function(s) s$type, ""), cols)
  count <- function(w) {
    as.numeric(sr_duck_query(con, sprintf("SELECT count(*) AS n FROM cardiac WHERE %s", w))$n)
  }
  has <- which(!vapply(f$where, is.null, NA))
  expect_gte(length(has), 8L)
  for (i in has) {
    info <- paste(f$check[i], f$column[i])
    w <- tryCatch(sr_where_sql(con, f$where[[i]], cols, types), error = function(e) e)
    expect_false(inherits(w, "error"), info = paste(info, if (inherits(w, "error")) conditionMessage(w)))
    if (inherits(w, "error")) next
    r <- tryCatch(count(w), error = function(e) e)
    expect_false(inherits(r, "error"), info = paste(info, if (inherits(r, "error")) conditionMessage(r)))
    # the filter without the column types (value shape only) compiles too
    expect_no_error(sr_where_sql(con, f$where[[i]], cols))
  }
  # the checks that count with the very same predicate select exactly the rows they report
  for (chk in c("future_date", "old_date", "unit_mix")) {
    for (i in which(f$check == chk & !vapply(f$where, is.null, NA))) {
      expect_equal(count(sr_where_sql(con, f$where[[i]], cols, types)), f$n_affected[i],
                   info = paste(chk, f$column[i]))
    }
  }
})
