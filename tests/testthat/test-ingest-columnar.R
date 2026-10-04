# Tests for io/read_columnar.R: parquet, feather, R (rds / RData), fst, NDJSON, JSON, SQLite and DuckDB
# sources behind the ingest reader contract. Synthetic data only; everything lives under
# withr::local_tempdir(). The non-ASCII value is built with intToUtf8() (source files are ASCII).

CG_NL <- intToUtf8(10L)
CG_CR <- intToUtf8(13L)
CG_UNI <- paste0("Z", intToUtf8(c(252L, 8364L, 960L)), "x")        # u-umlaut, euro sign, pi

.cg_project <- function(env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  p
}

.cg_drive <- function(free = 1e13) function(path) list(free_bytes = free, remote = FALSE, warnings = character())

.cg_ing <- function(p, path, ..., options = list()) {
  if (is.null(options$drive_info)) options$drive_info <- .cg_drive()
  sr_ingest(p, path, ..., options = options)
}

# 200 rows: int, double, text (one non-ASCII), Date, logical, and an NA in every column but id.
.cg_frame <- function(n = 200L) {
  d <- data.frame(id = seq_len(n), x = seq_len(n) * 1.5 + 0.25, s = paste0("row", seq_len(n)),
                  d = as.Date("2020-01-01") + seq_len(n), b = seq_len(n) %% 3L == 0L,
                  stringsAsFactors = FALSE)
  d$s[5] <- CG_UNI
  d$x[7] <- NA
  d$s[9] <- NA
  d$d[11] <- NA
  d$b[13] <- NA
  d
}

.cg_all <- function(p, table) {
  m <- sr_store_manifest(p, table)
  cols <- vapply(m$schema, function(s) s$name, "")
  offs <- seq(0, max(m$rows - 1, 0), by = 1000)
  do.call(rbind, lapply(offs, function(o) sr_table_page(p, table, o, 1000, order_by = cols)))
}

.cg_data <- function(p) sr_project_paths(p$paths$dir)$data
.cg_ck <- function(p, table) .sr_read_json_file(file.path(.cg_data(p), "ingest", table, "checkpoint.json"))
.cg_hash <- function(p, table) p$state$tables[[table]]$store_hash
.cg_schema <- function(p, table) {
  t <- p$state$tables[[table]]$store$schema
  stats::setNames(vapply(t, function(x) x$type, ""), vapply(t, function(x) x$name, ""))
}
.cg_dir <- function(p) file.path(dirname(p$paths$dir), "src")

.cg_write_lines <- function(lines, path) {
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  writeBin(charToRaw(paste0(paste(lines, collapse = CG_NL), CG_NL)), path)
  invisible(path)
}

# A SQLite file with the given tables (a named list of data frames), extended types on.
.cg_sqlite <- function(tables, path) {
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  db <- DBI::dbConnect(RSQLite::SQLite(), path, extended_types = TRUE)
  for (nm in names(tables)) DBI::dbWriteTable(db, nm, tables[[nm]])
  DBI::dbDisconnect(db)
  invisible(path)
}

.cg_settings <- function(p) {
  DBI::dbGetQuery(sr_duck(p), paste(
    "SELECT current_setting('lock_configuration') AS l, current_setting('autoload_known_extensions') AS a,",
    "current_setting('autoinstall_known_extensions') AS i, current_setting('allow_community_extensions') AS c,",
    "current_setting('preserve_insertion_order') AS o"))
}

.cg_leftovers <- function(p) {
  con <- sr_duck(p)
  tabs <- DBI::dbGetQuery(con, "SELECT table_name FROM duckdb_tables() WHERE table_name LIKE 'sr_df_%'")$table_name
  dbs <- DBI::dbGetQuery(con, "SELECT database_name FROM duckdb_databases() WHERE database_name LIKE 'sr_src_%'")$database_name
  views <- DBI::dbGetQuery(con, "SELECT view_name FROM duckdb_views() WHERE view_name LIKE 'sr_df_%' OR view_name LIKE 'sr_arrow_%'")$view_name
  c(tabs, dbs, views)
}

# ---- the registry -----------------------------------------------------------------------------------

test_that("every columnar, JSON and database format has a registered reader", {
  for (f in c("parquet", "feather", "rds", "rdata", "fst", "ndjson", "json", "sqlite", "duckdb")) {
    expect_true(is.function(.sr_reader_get(f)), info = f)
  }
})

# ---- every format round-trips ------------------------------------------------------------------------

test_that("each format ingests the same frame: values, NAs, non-ASCII text, manifest hash", {
  p <- .cg_project()
  ref <- .cg_frame()
  files <- .fx_write_formats(ref, .cg_dir(p))
  expect_true(all(c("parquet", "feather", "rds", "rdata", "ndjson", "json", "sqlite", "duckdb") %in% names(files)))
  before <- .cg_settings(p)
  for (fmt in names(files)) {
    tb <- paste0("t_", fmt)
    p <- .cg_ing(p, files[[fmt]], table = tb)
    t <- p$state$tables[[tb]]
    expect_identical(t$format, fmt)
    expect_equal(t$store$rows, 200, info = fmt)
    expect_match(t$store_hash, "^[0-9a-f]{64}$")
    got <- .fx_norm(.cg_all(p, tb), ref)
    got <- got[order(got$id), , drop = FALSE]
    attr(got, "row.names") <- seq_len(nrow(got))
    expect_equal(got, ref, info = fmt)
    expect_identical(got$s[5], CG_UNI, info = fmt)
    expect_identical(sum(is.na(got$x)), 1L)
    m <- sr_store_manifest(p, tb)
    dir <- sr_store_dir(p, tb)
    for (part in m$parts) expect_identical(sr_sha256_file(file.path(dir, part$file)), part$sha256)
  }
  expect_identical(.cg_settings(p), before)
  expect_true(.cg_settings(p)$l)
  expect_length(.cg_leftovers(p), 0L)
})

test_that("a repeated ingest of a single-part source gives the same store hash", {
  p <- .cg_project()
  files <- .fx_write_formats(.cg_frame(), .cg_dir(p))
  for (fmt in c("parquet", "feather", "rds", "rdata", "json", "sqlite", "duckdb")) {
    a <- .cg_ing(p, files[[fmt]], table = paste0("a_", fmt))
    b <- .cg_ing(a, files[[fmt]], table = paste0("b_", fmt))
    expect_identical(.cg_hash(b, paste0("a_", fmt)), .cg_hash(b, paste0("b_", fmt)), info = fmt)
    p <- b
  }
})

test_that("a multi-batch feather file keeps its row order", {
  skip_if_not_installed("arrow")
  p <- .cg_project()
  ref <- .cg_frame(1000L)
  f <- file.path(.cg_dir(p), "multi.feather")
  dir.create(dirname(f), recursive = TRUE)
  tb <- arrow::arrow_table(ref)
  sink <- arrow::FileOutputStream$create(f)
  w <- arrow::RecordBatchFileWriter$create(sink, tb$schema)
  for (i in 0:9) w$write(arrow::record_batch(ref[(i * 100 + 1):(i * 100 + 100), ]))
  w$close()
  sink$close()
  p <- .cg_ing(p, f, table = "mb")
  got <- .fx_norm(.cg_all(p, "mb"), ref)
  expect_equal(sort(got$id), as.numeric(1:1000))
  con <- sr_duck(p)
  part <- file.path(sr_store_dir(p, "mb"), "part-00001.parquet")
  ids <- DBI::dbGetQuery(con, sprintf("SELECT id FROM read_parquet(%s)", DBI::dbQuoteString(con, part)))$id
  expect_identical(as.numeric(ids), as.numeric(1:1000))
})

# ---- R objects ----------------------------------------------------------------------------------------

test_that("an RDS that is not a data frame is refused; factor and list columns are read", {
  p <- .cg_project()
  dir.create(.cg_dir(p), recursive = TRUE)
  f <- file.path(.cg_dir(p), "vec.rds")
  saveRDS(1:10, f)
  expect_error(.cg_ing(p, f, table = "vec"), "data frame")
  expect_null(p$state$tables$vec)
  df <- data.frame(id = 1:3, f = factor(c("a", "b", NA)))
  df$l <- list(1:2, list(a = 1, b = "x"), NULL)
  g <- file.path(.cg_dir(p), "fl.rds")
  saveRDS(df, g)
  p <- .cg_ing(p, g, table = "fl")
  got <- .cg_all(p, "fl")
  got <- got[order(got$id), ]
  expect_identical(as.character(got$f), c("a", "b", NA))
  expect_identical(got$l[1], "[1,2]")
  expect_match(got$l[2], r"("a":1)", fixed = TRUE)
})

test_that("an RData file with two data frames errors naming both; options$object picks one", {
  p <- .cg_project()
  dir.create(.cg_dir(p), recursive = TRUE)
  f <- file.path(.cg_dir(p), "two.RData")
  first <- data.frame(id = 1:3)
  second <- data.frame(id = 4:9)
  note <- "not a frame"
  save(first, second, note, file = f)
  expect_error(.cg_ing(p, f, table = "two_a"), "first.*second|second.*first")
  expect_null(p$state$tables$two)
  p <- .cg_ing(p, f, table = "two", options = list(object = "second"))
  expect_equal(p$state$tables$two$store$rows, 6)
  expect_error(.cg_ing(p, f, table = "two_b", options = list(object = "note")), "data frame")
  g <- file.path(.cg_dir(p), "none.RData")
  save(note, file = g)
  expect_error(.cg_ing(p, g, table = "none"), "data frame")
})

test_that("fst: without the package the error says so (and nothing is created)", {
  skip_if(requireNamespace("fst", quietly = TRUE), "fst is installed")
  p <- .cg_project()
  dir.create(.cg_dir(p), recursive = TRUE)
  f <- file.path(.cg_dir(p), "x.fst")
  writeBin(as.raw(sample(0:255, 400, replace = TRUE)), f)
  expect_error(.cg_ing(p, f, table = "fs"), "fst package required")
  expect_null(p$state$tables$fs)
})

test_that("fst round-trips when the package is installed", {
  skip_if_not_installed("fst")
  p <- .cg_project()
  dir.create(.cg_dir(p), recursive = TRUE)
  f <- file.path(.cg_dir(p), "x.fst")
  ref <- .cg_frame()
  fst::write_fst(ref, f)
  p <- .cg_ing(p, f, table = "fs")
  expect_equal(p$state$tables$fs$store$rows, 200)
  got <- .fx_norm(.cg_all(p, "fs"), ref)
  expect_identical(sort(got$s)[length(na.omit(got$s))], sort(ref$s)[length(na.omit(ref$s))])
})

# ---- NDJSON ---------------------------------------------------------------------------------------------

.cg_ndjson <- function(p, df = .cg_frame(), name = "m.ndjson") {
  f <- file.path(.cg_dir(p), name)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  con <- file(f, "wb")
  jsonlite::stream_out(df, con, verbose = FALSE, digits = NA)
  close(con)
  f
}

test_that("NDJSON in small chunks gives several parts, is resumable and re-runs to the same hash", {
  p <- .cg_project()
  ref <- .cg_frame()
  f <- .cg_ndjson(p, ref)
  p <- .cg_ing(p, f, table = "nd", chunk_bytes = 2000)
  t <- p$state$tables$nd
  expect_gt(length(t$store$parts), 1L)
  expect_true(t$resumable)
  expect_equal(t$store$rows, 200)
  got <- .fx_norm(.cg_all(p, "nd"), ref)
  got <- got[order(got$id), , drop = FALSE]
  attr(got, "row.names") <- seq_len(nrow(got))
  expect_equal(got, ref)
  p <- .cg_ing(p, f, table = "nd2", chunk_bytes = 2000)
  expect_identical(.cg_hash(p, "nd2"), .cg_hash(p, "nd"))
  one <- .cg_ing(p, f, table = "nd1")
  expect_length(one$state$tables$nd1$store$parts, 1L)
  expect_false(one$state$tables$nd1$resumable)
})

test_that("a cancelled NDJSON ingest resumes after its checkpoint and leaves the parts untouched", {
  p <- .cg_project()
  f <- .cg_ndjson(p)
  clean <- .cg_ing(p, f, table = "clean", chunk_bytes = 2000)
  calls <- 0L
  cancel <- function() { calls <<- calls + 1L; calls >= 2L }
  expect_error(.cg_ing(p, f, table = "r", chunk_bytes = 2000, cancel = cancel), class = "sr_ingest_cancelled")
  ck <- .cg_ck(p, "r")
  expect_false(ck$done)
  expect_length(ck$parts, 2L)
  expect_gt(ck$next_offset, 0)
  stage <- file.path(.cg_data(p), "ingest", "r", "store")
  parts <- file.path(stage, vapply(ck$parts, function(x) x$file, ""))
  sha_before <- vapply(parts, sr_sha256_file, "")
  seen <- numeric()
  done <- .cg_ing(p, f, table = "r", chunk_bytes = 2000, progress = function(d, total) seen <<- c(seen, d))
  expect_gt(seen[1L], ck$next_offset)
  expect_equal(seen[length(seen)], file.size(f))
  final <- sr_store_dir(done, "r")
  expect_identical(unname(vapply(file.path(final, basename(parts)), sr_sha256_file, "")), unname(sha_before))
  expect_identical(.cg_hash(done, "r"), .cg_hash(clean, "clean"))
  expect_equal(done$state$tables$r$store$rows, 200)
})

test_that("NDJSON: BOM, CRLF and blank lines are tolerated", {
  p <- .cg_project()
  lines <- c(r"({"id":1,"s":"a"})", "", r"({"id":2,"s":"b"})", "   ", r"({"id":3,"s":null})")
  f <- file.path(.cg_dir(p), "bom.ndjson")
  dir.create(dirname(f), recursive = TRUE)
  body <- paste0(paste(lines, collapse = paste0(CG_CR, CG_NL)), CG_CR, CG_NL)
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw(body)), f)
  p <- .cg_ing(p, f, table = "bom")
  got <- .cg_all(p, "bom")
  got <- got[order(got$id), ]
  expect_equal(got$id, c(1, 2, 3))
  expect_identical(got$s, c("a", "b", NA))
})

test_that("NDJSON: a later type mismatch widens the column and is recorded; nothing becomes NULL", {
  p <- .cg_project()
  tmpl <- r"({"id":%d,"v":%d,"w":true,"big":%d,"n":%d})"
  rows <- c(sprintf(tmpl, 1:30, 1:30, 1:30, 1:30),
            r"({"id":31,"v":1.5,"w":"yes","big":9007199254740993,"n":null})",
            r"({"id":32,"v":"text","w":false,"big":2,"n":3,"late":"x"})")
  f <- .cg_write_lines(rows, file.path(.cg_dir(p), "wid.ndjson"))
  p <- .cg_ing(p, f, table = "wid", chunk_bytes = 400)
  ty <- .cg_schema(p, "wid")
  expect_identical(ty[["v"]], "VARCHAR")
  expect_identical(ty[["w"]], "VARCHAR")
  expect_identical(ty[["late"]], "VARCHAR")
  expect_true(all(c("v", "w") %in% unlist(p$state$tables$wid$type_widened)))
  got <- .cg_all(p, "wid")
  got <- got[order(got$id), ]
  expect_identical(got$v[c(1, 30, 31, 32)], c("1", "30", "1.5", "text"))
  expect_identical(got$w[c(1, 31, 32)], c("true", "yes", "false"))
  expect_identical(as.character(got$big[31]), "9007199254740993")
  expect_identical(got$late[32], "x")
  expect_true(is.na(got$late[1]))
  expect_equal(sum(!is.na(got$v)), 32)
  expect_equal(nrow(got), 32L)
})

test_that("NDJSON: a malformed line fails loudly and the previous table survives", {
  p <- .cg_project()
  good <- .cg_ndjson(p, data.frame(id = 1:5, s = letters[1:5]), "good.ndjson")
  p <- .cg_ing(p, good, table = "keep")
  h <- .cg_hash(p, "keep")
  bad <- .cg_write_lines(c(r"({"id":1})", r"({"id":2)", r"({"id":3})"), file.path(.cg_dir(p), "bad.ndjson"))
  expect_error(.cg_ing(p, bad, table = "keep"), "JSON")
  expect_identical(.cg_hash(p, "keep"), h)
  expect_equal(nrow(.cg_all(p, "keep")), 5L)
  scalar <- .cg_write_lines(c(r"({"id":1})", "42"), file.path(.cg_dir(p), "scalar.ndjson"))
  suppressWarnings(expect_error(.cg_ing(p, scalar, table = "keep"), "object"))   # (the failed bad.ndjson run left a checkpoint)
  expect_identical(.cg_hash(p, "keep"), h)
})

# ---- JSON ---------------------------------------------------------------------------------------------------

test_that("JSON: nested objects flatten to a.b, arrays become JSON text, big integers stay exact", {
  p <- .cg_project()
  txt <- c(r"([{"id":1,"a":{"b":1,"c":{"d":"deep"}},"tags":["x","y"],"big":9007199254740993},)",
           r"( {"id":2,"a":{"b":2},"tags":[],"big":5}])")
  f <- .cg_write_lines(txt, file.path(.cg_dir(p), "n.json"))
  p <- .cg_ing(p, f, table = "nj")
  got <- .cg_all(p, "nj")
  got <- got[order(got$id), ]
  expect_true(all(c("a.b", "a.c.d", "tags", "big") %in% names(got)))
  expect_equal(got$a.b, c(1, 2))
  expect_identical(got$a.c.d, c("deep", NA))
  expect_identical(got$tags, c(r"(["x","y"])", "[]"))
  expect_identical(as.character(got$big), c("9007199254740993", "5"))
  expect_equal(p$state$tables$nj$store$rows, 2)
})

test_that("JSON: a non-array, a non-object element and an empty array are refused", {
  p <- .cg_project()
  one <- .cg_write_lines(r"({"id":1})", file.path(.cg_dir(p), "one.json"))
  expect_error(.cg_ing(p, one, table = "j1"), "array of objects")
  nums <- .cg_write_lines("[1,2,3]", file.path(.cg_dir(p), "nums.json"))
  expect_error(.cg_ing(p, nums, table = "j2"), "array of objects")
  empty <- .cg_write_lines("[]", file.path(.cg_dir(p), "empty.json"))
  expect_error(.cg_ing(p, empty, table = "j3"), "no records|format")
  expect_length(p$state$tables, 0L)
})

test_that("a JSON document over 512 MB is refused in the pre-flight: nothing is created under data/", {
  p <- .cg_project()
  f <- .cg_write_lines(c("[", r"({"id":1})", "]"), file.path(.cg_dir(p), "huge.json"))
  sn <- sr_sniff(f)
  expect_identical(sn$format, "json")
  sn$size_bytes <- 600 * 1024^2
  snap <- function() sort(list.files(.cg_data(p), recursive = TRUE, all.files = TRUE, include.dirs = TRUE))
  before <- snap()
  expect_error(.cg_ing(p, f, table = "huge", sniff = sn), "NDJSON")
  expect_identical(snap(), before)
  # the same size as NDJSON is not refused for being a JSON document
  nd <- .cg_write_lines(r"({"id":1})", file.path(.cg_dir(p), "big.ndjson"))
  sn2 <- sr_sniff(nd)
  sn2$size_bytes <- 600 * 1024^2
  expect_no_error(sr_preflight(p, nd, sn2, drive_info = .cg_drive()))
  expect_true(sr_preflight(p, nd, sn2, drive_info = .cg_drive())$ok)
})

# ---- SQLite ---------------------------------------------------------------------------------------------------

test_that("SQLite: several tables need options$table and the error lists them (not sqlite_sequence)", {
  p <- .cg_project()
  db <- file.path(.cg_dir(p), "two.sqlite")
  .cg_sqlite(list(alpha = data.frame(id = 1:3), sqlitex = data.frame(id = 4:9)), db)
  lite <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(lite, "CREATE TABLE auto (id INTEGER PRIMARY KEY AUTOINCREMENT, v TEXT)")
  DBI::dbExecute(lite, "INSERT INTO auto (v) VALUES ('a'), ('b')")
  DBI::dbDisconnect(lite)
  err <- tryCatch(.cg_ing(p, db, table = "s_a"), error = function(e) conditionMessage(e))
  expect_match(err, "alpha")
  expect_match(err, "sqlitex")
  expect_match(err, "auto")
  expect_false(grepl("sqlite_sequence", err, fixed = TRUE))
  expect_error(.cg_ing(p, db, table = "s_b", options = list(table = "nope")), "alpha")
  p <- .cg_ing(p, db, table = "s", options = list(table = "sqlitex"))
  expect_equal(p$state$tables$s$store$rows, 6)
  # one real table plus sqlite_sequence: no choice needed
  one <- file.path(.cg_dir(p), "one.sqlite")
  lite <- DBI::dbConnect(RSQLite::SQLite(), one)
  DBI::dbExecute(lite, "CREATE TABLE auto (id INTEGER PRIMARY KEY AUTOINCREMENT, v TEXT)")
  DBI::dbExecute(lite, "INSERT INTO auto (v) VALUES ('a'), ('b')")
  DBI::dbDisconnect(lite)
  p <- .cg_ing(p, one, table = "o")
  expect_equal(p$state$tables$o$store$rows, 2)
})

test_that("SQLite: a cancelled ingest resumes by row offset and matches a clean run", {
  p <- .cg_project()
  db <- .cg_sqlite(list(fx = .cg_frame()), file.path(.cg_dir(p), "rows.sqlite"))
  clean <- .cg_ing(p, db, table = "clean", options = list(chunk_rows = 60))
  expect_length(clean$state$tables$clean$store$parts, 4L)
  expect_true(clean$state$tables$clean$resumable)
  calls <- 0L
  cancel <- function() { calls <<- calls + 1L; calls >= 2L }
  expect_error(.cg_ing(p, db, table = "r", options = list(chunk_rows = 60), cancel = cancel),
               class = "sr_ingest_cancelled")
  ck <- .cg_ck(p, "r")
  expect_length(ck$parts, 2L)
  expect_equal(ck$next_offset, 120)
  stage <- file.path(.cg_data(p), "ingest", "r", "store")
  parts <- file.path(stage, vapply(ck$parts, function(x) x$file, ""))
  sha_before <- unname(vapply(parts, sr_sha256_file, ""))
  seen <- numeric()
  done <- .cg_ing(p, db, table = "r", options = list(chunk_rows = 60), progress = function(d, total) seen <<- c(seen, d))
  expect_equal(seen, c(180, 200))
  final <- sr_store_dir(done, "r")
  expect_identical(unname(vapply(file.path(final, basename(parts)), sr_sha256_file, "")), sha_before)
  expect_identical(.cg_hash(done, "r"), .cg_hash(clean, "clean"))
  expect_equal(done$state$tables$r$store$rows, 200)
})

test_that("SQLite: mixed storage classes and 64-bit integers lose nothing; the source is read-only", {
  p <- .cg_project()
  db <- file.path(.cg_dir(p), "mixed.sqlite")
  dir.create(dirname(db), recursive = TRUE)
  lite <- DBI::dbConnect(RSQLite::SQLite(), db, extended_types = TRUE)
  DBI::dbExecute(lite, "CREATE TABLE m (id INTEGER, v, n INTEGER, r REAL, ts TIMESTAMP)")
  DBI::dbExecute(lite, "INSERT INTO m VALUES (1, 5, 9223372036854775807, 1.5, '2020-01-02 03:04:05')")
  DBI::dbExecute(lite, "INSERT INTO m VALUES (2, 'text', -9223372036854775807, 2, NULL)")
  DBI::dbExecute(lite, "INSERT INTO m VALUES (3, 2.5, NULL, NULL, 'not a time')")
  DBI::dbExecute(lite, "INSERT INTO m VALUES (4, x'00ff', 0, 7, NULL)")
  DBI::dbDisconnect(lite)
  sha <- sr_sha256_file(db)
  p <- .cg_ing(p, db, table = "mx")
  got <- .cg_all(p, "mx")
  got <- got[order(got$id), ]
  expect_identical(as.character(got$v), c("5", "text", "2.5", "00FF"))
  expect_identical(.cg_schema(p, "mx")[["n"]], "BIGINT")
  con <- sr_duck(p)
  part <- DBI::dbQuoteString(con, file.path(sr_store_dir(p, "mx"), "part-00001.parquet"))
  exact <- DBI::dbGetQuery(con, sprintf("SELECT CAST(n AS VARCHAR) AS n FROM read_parquet(%s) ORDER BY id", part))$n
  expect_identical(exact, c("9223372036854775807", "-9223372036854775807", NA, "0"))
  expect_equal(as.numeric(got$r), c(1.5, 2, NA, 7))
  expect_identical(as.character(got$ts[3]), "not a time")
  expect_identical(sr_sha256_file(db), sha)
  expect_identical(sr_sha256_file(file.path(.cg_data(p), "source", "mx.sqlite")), sha)
})

test_that("SQLite: an empty table gives an empty table with its columns", {
  p <- .cg_project()
  db <- file.path(.cg_dir(p), "empty.sqlite")
  dir.create(dirname(db), recursive = TRUE)
  lite <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(lite, "CREATE TABLE e (id INTEGER, name TEXT)")
  DBI::dbDisconnect(lite)
  p <- .cg_ing(p, db, table = "em")
  expect_equal(p$state$tables$em$store$rows, 0)
  expect_identical(names(.cg_schema(p, "em")), c("id", "name"))
})

# ---- DuckDB ---------------------------------------------------------------------------------------------------

.cg_duckdb <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  dk <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  DBI::dbWriteTable(dk, "alpha", data.frame(id = 1:3, s = c("a", "b", CG_UNI)))
  DBI::dbWriteTable(dk, "beta", data.frame(id = 4:9))
  DBI::dbExecute(dk, "CREATE VIEW vw AS SELECT * FROM alpha")
  DBI::dbDisconnect(dk, shutdown = TRUE)
  invisible(path)
}

test_that("DuckDB: the table is chosen; views are not candidates; nothing stays attached", {
  p <- .cg_project()
  db <- .cg_duckdb(file.path(.cg_dir(p), "two.duckdb"))
  err <- tryCatch(.cg_ing(p, db, table = "d_a"), error = function(e) conditionMessage(e))
  expect_match(err, "alpha")
  expect_match(err, "beta")
  expect_false(grepl("vw", err, fixed = TRUE))
  expect_error(.cg_ing(p, db, table = "d_b", options = list(table = "vw")), "alpha")
  p <- .cg_ing(p, db, table = "d", options = list(table = "alpha"))
  got <- .cg_all(p, "d")
  got <- got[order(got$id), ]
  expect_equal(got$id, 1:3 + 0)
  expect_identical(got$s[3], CG_UNI)
  expect_length(.cg_leftovers(p), 0L)
  expect_true(.cg_settings(p)$l)
})

test_that("DuckDB: with external = TRUE the source file can be removed after the ingest", {
  p <- .cg_project()
  db <- .cg_duckdb(file.path(.cg_dir(p), "ext.duckdb"))
  p <- .cg_ing(p, db, table = "x", external = TRUE, options = list(table = "beta"))
  expect_equal(p$state$tables$x$store$rows, 6)
  expect_true(isTRUE(unlink(db) == 0))
  expect_false(file.exists(db))
  expect_equal(nrow(sr_table_page(p, "x", 0, 10, order_by = "id")), 6L)
})

# ---- ATTACH stays internal -------------------------------------------------------------------------------

test_that("ATTACH and DETACH stay refused for user SQL; .sr_duck_attach is internal and read-only", {
  p <- .cg_project()
  con <- sr_duck(p)
  db <- .cg_duckdb(file.path(.cg_dir(p), "att.duckdb"))
  expect_false(any(c("ATTACH", "DETACH") %in% .sr_duck_allowed))
  expect_error(sr_duck_exec(con, sprintf("ATTACH '%s' AS evil (READ_ONLY)", db)), "refused|not allowed")
  expect_error(sr_duck_exec(con, "DETACH evil"), "refused|not allowed")
  alias <- .sr_duck_attach(con, db)
  expect_match(alias, "^sr_src_[0-9a-f]{12}$")
  expect_error(DBI::dbExecute(con, sprintf("INSERT INTO %s.main.beta VALUES (99)", alias)))
  .sr_duck_detach(con, alias)
  expect_length(.cg_leftovers(p), 0L)
  expect_error(.sr_duck_attach(con, "https://example.invalid/x.duckdb"), "URL")
  expect_error(.sr_duck_attach(con, file.path(.cg_dir(p), "missing.duckdb")), "not found")
  expect_error(.sr_duck_attach(con, db, alias = "x; DROP TABLE y"), "alias")
})
