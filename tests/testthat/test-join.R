# Tests for io/join.R: the key check (sr_join_check) and the materialised join (sr_join).
# Synthetic data only; everything lives under withr::local_tempdir().

.jn_project <- function(env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  p
}

.jn_ing <- function(p, df, table) {
  csv <- file.path(dirname(p$paths$dir), paste0(table, ".csv"))
  .fx_write_csv(df, csv)
  sr_ingest(p, csv, table = table,
            options = list(drive_info = function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())))
}

# patients: 100 unique ids. labs: 250 rows over the first 90 of them plus 5 orphan rows (ids not in
# patients). Both have a `site` column so the join has a name clash.
.jn_fixture <- function(env = parent.frame()) {
  p <- .jn_project(env)
  ids <- sprintf("P%03d", 1:100)
  patients <- data.frame(patient_id = ids, site = rep(c("A", "B"), 50), age = 20 + (1:100) %% 60,
                         stringsAsFactors = FALSE)
  labs <- data.frame(pid = c(ids[(0:249 %% 90) + 1L], sprintf("X%03d", 1:5)),
                     site = "lab", value = seq_len(255) / 10, stringsAsFactors = FALSE)
  p <- .jn_ing(p, patients, "patients")
  p <- .jn_ing(p, labs, "labs")
  p
}

.jn_rows <- function(p, table) {
  m <- sr_store_manifest(p, table)
  cols <- vapply(m$schema, function(s) s$name, "")
  sr_table_page(p, table, 0, 1000, order_by = cols)
}

.jn_n_events <- function(p) length(sr_events_read(p$paths$events))

test_that("the key check reports counts, uniqueness and the relationship", {
  p <- .jn_fixture()
  n0 <- .jn_n_events(p)
  r <- sr_join_check(p, "patients", "labs", c(patient_id = "pid"))
  expect_identical(r$left_rows, 100)
  expect_identical(r$right_rows, 255)
  expect_true(r$left_key_unique)
  expect_false(r$right_key_unique)
  expect_identical(r$left_key_na, 0)
  expect_identical(r$right_key_na, 0)
  expect_identical(r$matched_left, 90)
  expect_identical(r$unmatched_left, 10)
  expect_identical(r$unmatched_right, 5)
  expect_identical(r$relationship, "one_to_many")
  # reversing the sides reverses the relationship
  r2 <- sr_join_check(p, "labs", "patients", c(pid = "patient_id"))
  expect_identical(r2$relationship, "many_to_one")
  expect_identical(r2$unmatched_left, 5)
  expect_identical(r2$unmatched_right, 10)
  # the check records nothing
  expect_identical(.jn_n_events(p), n0)
})

test_that("left, inner and full joins give the expected rows and columns", {
  p <- .jn_fixture()
  by <- c(patient_id = "pid")
  p <- sr_join(p, "patients", "labs", by, type = "left", into = "j_left")
  p <- sr_join(p, "patients", "labs", by, type = "inner", into = "j_inner")
  p <- sr_join(p, "patients", "labs", by, type = "full", into = "j_full")
  expect_equal(p$state$tables$j_left$store$rows, 260)
  expect_equal(p$state$tables$j_inner$store$rows, 250)
  expect_equal(p$state$tables$j_full$store$rows, 265)
  l <- .jn_rows(p, "j_left")
  expect_identical(nrow(l), 260L)
  # left key kept, right key dropped, the clashing `site` suffixed with the right table's name
  expect_identical(names(l), c("patient_id", "site", "age", "site_labs", "value"))
  expect_identical(sum(is.na(l$value)), 10L)
  expect_identical(sum(!is.na(l$value)), 250L)
  f <- .jn_rows(p, "j_full")
  expect_identical(nrow(f), 265L)
  # the orphan rows keep their key (the full join coalesces the two key columns)
  expect_identical(sum(is.na(f$patient_id)), 0L)
  expect_identical(sum(grepl("^X", f$patient_id)), 5L)
  expect_identical(sum(is.na(f$age)), 5L)
})

test_that("the payload SQL is over view names and re-runs to the same row count", {
  p <- .jn_fixture()
  p <- sr_join(p, "patients", "labs", c(patient_id = "pid"), type = "left", into = "j")
  evs <- sr_events_read(p$paths$events)
  ev <- evs[[length(evs)]]
  expect_identical(ev$type, "join.created")
  pl <- ev$payload
  expect_identical(pl$into, "j")
  expect_identical(pl$left, "patients")
  expect_identical(pl$right, "labs")
  expect_identical(pl$type, "left")
  expect_identical(pl$store_hash, p$state$tables$j$store_hash)
  expect_identical(pl$report$relationship, "one_to_many")
  expect_false(grepl("parquet|[/\\\\]|:", pl$sql))
  sr_store_view(p, "patients")
  sr_store_view(p, "labs")
  con <- sr_duck(p)
  n <- sr_duck_query(con, paste0("SELECT count(*) AS n FROM (", pl$sql, ") AS q"))$n
  expect_equal(as.numeric(n), as.numeric(p$state$tables$j$store$rows))
  # no absolute project path anywhere in the event log
  txt <- paste(readLines(p$paths$events, warn = FALSE), collapse = "\n")
  base <- normalizePath(p$paths$dir, winslash = "/")
  expect_false(grepl(base, gsub("\\\\", "/", txt), fixed = TRUE))
})

test_that("NA keys are counted and never match, in any join type", {
  p <- .jn_project()
  a <- data.frame(k = c(1L, 2L, NA, NA), x = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
  b <- data.frame(k = c(1L, NA, 3L), y = c("p", "q", "r"), stringsAsFactors = FALSE)
  p <- .jn_ing(p, a, "ta")
  p <- .jn_ing(p, b, "tb")
  r <- sr_join_check(p, "ta", "tb", c(k = "k"))
  expect_identical(r$left_key_na, 2)
  expect_identical(r$right_key_na, 1)
  expect_identical(r$matched_left, 1)
  expect_identical(r$unmatched_left, 3)
  expect_identical(r$unmatched_right, 2)
  expect_identical(r$relationship, "one_to_one")
  p <- sr_join(p, "ta", "tb", c(k = "k"), type = "left", into = "jl")
  p <- sr_join(p, "ta", "tb", c(k = "k"), type = "inner", into = "ji")
  p <- sr_join(p, "ta", "tb", c(k = "k"), type = "full", into = "jf")
  expect_equal(p$state$tables$jl$store$rows, 4)
  expect_equal(p$state$tables$ji$store$rows, 1)
  expect_equal(p$state$tables$jf$store$rows, 6)
  f <- .jn_rows(p, "jf")
  expect_identical(sum(!is.na(f$x) & !is.na(f$y)), 1L)
})

test_that("a multi-column key works", {
  p <- .jn_project()
  a <- data.frame(a = c(1L, 1L, 2L, 2L), b = c("x", "y", "x", NA), v = 1:4, stringsAsFactors = FALSE)
  b <- data.frame(c = c(1L, 2L, 2L), d = c("x", "x", NA), w = c(10, 20, 30), stringsAsFactors = FALSE)
  p <- .jn_ing(p, a, "ma")
  p <- .jn_ing(p, b, "mb")
  by <- c(a = "c", b = "d")
  r <- sr_join_check(p, "ma", "mb", by)
  expect_identical(r$left_key_na, 1)
  expect_identical(r$right_key_na, 1)
  expect_identical(r$matched_left, 2)
  expect_identical(r$unmatched_left, 2)
  expect_identical(r$unmatched_right, 1)
  p <- sr_join(p, "ma", "mb", by, type = "inner", into = "mj")
  d <- .jn_rows(p, "mj")
  expect_identical(names(d), c("a", "b", "v", "w"))
  expect_identical(nrow(d), 2L)
})

test_that("many-to-many is refused before anything is written, unless allowed", {
  p <- .jn_project()
  a <- data.frame(k = c(1L, 1L, 2L), x = 1:3)
  b <- data.frame(k = c(1L, 1L, 2L), y = 4:6)
  p <- .jn_ing(p, a, "ta")
  p <- .jn_ing(p, b, "tb")
  expect_identical(sr_join_check(p, "ta", "tb", c(k = "k"))$relationship, "many_to_many")
  n0 <- .jn_n_events(p)
  expect_error(sr_join(p, "ta", "tb", c(k = "k"), into = "mm"), "many-to-many")
  expect_error(sr_join(p, "ta", "tb", c(k = "k"), into = "mm"), "may multiply")
  expect_identical(.jn_n_events(p), n0)
  expect_false(dir.exists(sr_store_dir(p, "mm")))
  expect_false(dir.exists(file.path(sr_project_paths(p$paths$dir)$data, "ingest", "mm")))
  expect_null(p$state$tables$mm)
  p <- sr_join(p, "ta", "tb", c(k = "k"), into = "mm", allow_many_to_many = TRUE)
  expect_equal(p$state$tables$mm$store$rows, 5)   # 2 x 2 for key 1, 1 for key 2
  expect_identical(p$state$joins$mm$report$relationship, "many_to_many")
})

test_that("the target name and the arguments are validated before any work", {
  p <- .jn_fixture()
  by <- c(patient_id = "pid")
  n0 <- .jn_n_events(p)
  expect_error(sr_join(p, "patients", "labs", by, into = "patients"), "already exists")
  expect_error(sr_join(p, "patients", "labs", by, into = "Bad Name"), "invalid table name")
  expect_error(sr_join(p, "patients", "labs", by, into = "9x"), "invalid table name")
  expect_error(sr_join(p, "patients", "labs", by, type = "outer", into = "j"), "should be one of")
  expect_error(sr_join(p, "patients", "nope", by, into = "j"), "unknown table")
  expect_error(sr_join(p, "patients", "labs", c(patient_id = "nope"), into = "j"), "not a column")
  expect_error(sr_join(p, "patients", "labs", c(nope = "pid"), into = "j"), "not a column")
  expect_error(sr_join(p, "patients", "labs", character(), into = "j"), "by")
  expect_identical(.jn_n_events(p), n0)
  expect_false(dir.exists(sr_store_dir(p, "j")))
  p <- sr_join(p, "patients", "labs", by, into = "j")
  expect_error(sr_join(p, "patients", "labs", by, into = "j"), "already exists")
  ro <- p
  ro$readonly <- TRUE
  expect_error(sr_join(ro, "patients", "labs", by, into = "j2"), "read-only")
})

test_that("key columns of different kinds are refused with a clear message", {
  p <- .jn_project()
  p <- .jn_ing(p, data.frame(k = c(1L, 2L), x = 1:2), "ta")
  p <- .jn_ing(p, data.frame(k = c("a", "b"), y = 1:2, stringsAsFactors = FALSE), "tb")
  expect_error(sr_join_check(p, "ta", "tb", c(k = "k")), "different types")
  expect_error(sr_join(p, "ta", "tb", c(k = "k"), into = "j"), "different types")
})

test_that("column names with spaces and quotes are quoted safely", {
  p <- .jn_project()
  a <- data.frame(`my key` = c("a", "b", "c"), `o'brien` = 1:3, `say "hi"` = 4:6, check.names = FALSE,
                  stringsAsFactors = FALSE)
  b <- data.frame(`Other Key` = c("a", "a", "b"), `o'brien` = 7:9, `SAY "HI"` = 1:3, check.names = FALSE,
                  stringsAsFactors = FALSE)
  p <- .jn_ing(p, a, "qa")
  p <- .jn_ing(p, b, "qb")
  by <- c(`my key` = "Other Key")
  r <- sr_join_check(p, "qa", "qb", by)
  expect_identical(r$relationship, "one_to_many")
  p <- sr_join(p, "qa", "qb", by, type = "left", into = "qj")
  d <- .jn_rows(p, "qj")
  expect_identical(nrow(d), 4L)
  expect_identical(names(d), c("my key", "o'brien", "say \"hi\"", "o'brien_qb", "SAY \"HI\"_qb"))
})

test_that("a suffixed name that still clashes gets a counter", {
  p <- .jn_project()
  p <- .jn_ing(p, data.frame(id = 1:3, v = 1:3, v_b = 4:6), "a")
  p <- .jn_ing(p, data.frame(id = c(1L, 2L), v = 7:8), "b")
  p <- sr_join(p, "a", "b", c(id = "id"), into = "ab")
  expect_identical(names(.jn_rows(p, "ab")), c("id", "v", "v_b", "v_b_2"))
})

test_that("the join table is a first-class table in the state", {
  p <- .jn_fixture()
  p <- sr_join(p, "patients", "labs", c(patient_id = "pid"), type = "left", into = "j")
  e <- p$state$tables$j
  expect_identical(e$format, "join")
  expect_null(e$source)
  expect_identical(e$store_hash, sr_store_hash(sr_store_manifest(p, "j")))
  expect_equal(e$store$ncol, 5)
  expect_equal(as.numeric(e$store$rows), sr_store_manifest(p, "j")$rows)
  j <- p$state$joins$j
  expect_identical(j$left, "patients")
  expect_identical(j$right, "labs")
  expect_identical(j$type, "left")
  expect_equal(j$report$unmatched_left, 10)
  expect_identical("j", sr_store_view(p, "j"))
  expect_true("j" %in% sr_store_tables(p))
  m <- sr_dict_match(p, "j")
  expect_identical(m$variable, c("patient_id", "site", "age", "site_labs", "value"))
  expect_true(all(m$in_data))
  # no hash is owed for a derived table, and its source status is "derived"
  expect_identical(sr_tables_needing_hash(p), character(0))
  expect_identical(sr_source_status(p, "j")$status, "derived")
  expect_error(sr_source_relocate(p, "j", "x.csv"), "join")
  expect_identical(sr_source_hash(p, "j"), p)
  # staging is tidied away
  expect_false(dir.exists(file.path(sr_project_paths(p$paths$dir)$data, "ingest", "j")))
  # a join of a join
  p <- sr_join(p, "j", "patients", c(patient_id = "patient_id"), type = "inner", into = "jj")
  expect_equal(p$state$tables$jj$store$rows, 260)   # j (260 rows) x patients (unique key)
})

test_that("a reopened project replays the join and keeps the table readable", {
  p <- .jn_fixture()
  p <- sr_join(p, "patients", "labs", c(patient_id = "pid"), type = "full", into = "j")
  dir <- p$paths$dir
  sr_duck_close(p)
  sr_project_close(p)
  q <- sr_project_open(dir)
  withr::defer(sr_duck_close(q))
  withr::defer(sr_project_close(q))
  expect_identical(q$state$tables$j, p$state$tables$j)
  expect_identical(q$state$joins$j, p$state$joins$j)
  expect_identical(q$state$tables$j$format, "join")
  expect_identical(nrow(.jn_rows(q, "j")), 265L)
  expect_identical(sr_source_status(q, "j")$status, "derived")
})

test_that("an inner join with no matching keys gives an empty but valid table", {
  p <- .jn_project()
  p <- .jn_ing(p, data.frame(k = 1:3, x = 1:3), "ta")
  p <- .jn_ing(p, data.frame(k = 7:9, y = 1:3), "tb")
  p <- sr_join(p, "ta", "tb", c(k = "k"), type = "inner", into = "e")
  expect_equal(p$state$tables$e$store$rows, 0)
  expect_identical(nrow(.jn_rows(p, "e")), 0L)
  expect_identical(names(.jn_rows(p, "e")), c("k", "x", "y"))
})

test_that("the same join twice gives the same store hash (the output has a total order)", {
  p <- .jn_project()
  # many right rows per key with differing non-key values, enough rows for a parallel join
  nl <- 5000L
  nr <- 200000L
  left <- data.frame(id = sprintf("L%05d", seq_len(nl)), grp = rep(c("a", "b", "c", "d"), length.out = nl),
                     stringsAsFactors = FALSE)
  right <- data.frame(id = sprintf("L%05d", (seq_len(nr) %% (nl + 100L)) + 1L), val = (seq_len(nr) * 7919L) %% 10007L,
                      note = sprintf("n%d", (seq_len(nr) * 31L) %% 977L), stringsAsFactors = FALSE)
  p <- .jn_ing(p, left, "lt")
  p <- .jn_ing(p, right, "rt")
  by <- c(id = "id")
  p <- sr_join(p, "lt", "rt", by, type = "full", into = "j1")
  p <- sr_join(p, "lt", "rt", by, type = "full", into = "j2")
  expect_identical(p$state$tables$j1$store_hash, p$state$tables$j2$store_hash)
  expect_equal(p$state$tables$j1$store$rows, p$state$tables$j2$store$rows)
  # the recorded SQL is the exact recipe: it carries the ORDER BY
  expect_match(p$state$joins$j1$sql, "ORDER BY", fixed = TRUE)
  # and the table is sorted by the left key first
  d <- sr_table_page(p, "j1", 0, 1000, order_by = "id")
  expect_false(is.unsorted(d$id))
})

test_that("an existing staging directory for the target is refused and left untouched", {
  p <- .jn_fixture()
  stage <- file.path(sr_project_paths(p$paths$dir)$data, "ingest", "pending")
  dir.create(stage, recursive = TRUE)
  ck <- file.path(stage, "checkpoint.json")
  writeLines("{\"done\": false}", ck)
  before <- unname(tools::md5sum(ck))
  n0 <- .jn_n_events(p)
  expect_error(sr_join(p, "patients", "labs", c(patient_id = "pid"), into = "pending"), "pending.*ingest")
  expect_true(file.exists(ck))
  expect_identical(unname(tools::md5sum(ck)), before)
  expect_identical(.jn_n_events(p), n0)
  expect_false(dir.exists(sr_store_dir(p, "pending")))
})
