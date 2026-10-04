# Tests for io/fingerprint.R and io/store.R: file fingerprints and the Parquet store.

.store_project <- function(env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)   # runs before the project close and the temp dir removal
  p
}

# Write a table as `n_parts` Parquet parts of `rows_per` rows (id = running integer, v = text) through
# DuckDB COPY, then build and write the manifest. Returns the manifest.
.store_make <- function(p, table, n_parts = 2L, rows_per = 5L, tweak_part = 0L) {
  con <- sr_duck(p)
  dir <- sr_store_dir(p, table)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  parts <- list()
  for (i in seq_len(n_parts)) {
    f <- sprintf("part-%04d.parquet", i)
    lo <- (i - 1L) * rows_per + 1L
    hi <- i * rows_per
    tag <- if (i == tweak_part) "z" else "a"
    sr_duck_exec(con, sprintf(
      "COPY (SELECT i AS id, %s || CAST(i AS VARCHAR) AS v FROM range(%d, %d) t(i)) TO %s (FORMAT parquet)",
      DBI::dbQuoteString(con, tag), lo, hi + 1L, DBI::dbQuoteString(con, file.path(dir, f))))
    parts[[i]] <- list(file = f, rows = rows_per, sha256 = sr_sha256_file(file.path(dir, f)))
  }
  m <- list(table = table, parts = parts,
            schema = list(list(name = "id", type = "BIGINT"), list(name = "v", type = "VARCHAR")),
            rows = n_parts * rows_per)
  sr_store_write_manifest(p, table, m)
  m
}

test_that("manifest round-trips and the store hash is stable and sensitive to part bytes", {
  p <- .store_project()
  m <- .store_make(p, "visits")
  expect_true(file.exists(file.path(sr_store_dir(p, "visits"), "manifest.json")))
  back <- sr_store_manifest(p, "visits")
  expect_identical(back$table, "visits")
  expect_equal(back$rows, 10)
  expect_length(back$parts, 2L)
  expect_identical(back$parts[[1]]$file, "part-0001.parquet")
  expect_identical(back$schema[[2]]$name, "v")
  h <- sr_store_hash(back)
  expect_match(h, "^[0-9a-f]{64}$")
  expect_identical(sr_store_hash(back), h)
  expect_identical(sr_store_hash(sr_store_manifest(p, "visits")), h)
  expect_identical(sr_store_hash(m), h)                      # in-memory and re-read manifests agree
  # same shape, different bytes in part 2 -> different hash
  m2 <- .store_make(p, "visits", tweak_part = 2L)
  expect_false(identical(sr_store_hash(sr_store_manifest(p, "visits")), h))
  expect_identical(sr_store_hash(m2), sr_store_hash(sr_store_manifest(p, "visits")))
  # the schema is part of the hash too
  m3 <- back
  m3$schema[[2]]$type <- "INTEGER"
  expect_false(identical(sr_store_hash(m3), sr_store_hash(back)))
  # part order matters
  m4 <- back
  m4$parts <- rev(m4$parts)
  expect_false(identical(sr_store_hash(m4), sr_store_hash(back)))
})

test_that("a store with a single part hashes (length-1 vectors stay arrays)", {
  p <- .store_project()
  m <- .store_make(p, "one", n_parts = 1L)
  expect_match(sr_store_hash(m), "^[0-9a-f]{64}$")
  expect_false(identical(sr_store_hash(m), sr_store_hash(.store_make(p, "two", n_parts = 2L))))
})

test_that("sr_store_view exposes the parts and is replaced when the manifest changes", {
  p <- .store_project()
  .store_make(p, "visits", n_parts = 2L)
  expect_identical(sr_store_view(p, "visits"), "visits")
  con <- sr_duck(p)
  expect_equal(sr_duck_query(con, "SELECT count(*) AS n FROM \"visits\"")$n, 10)
  .store_make(p, "visits", n_parts = 3L)
  sr_store_view(p, "visits")
  expect_equal(sr_duck_query(con, "SELECT count(*) AS n FROM \"visits\"")$n, 15)
})

test_that("sr_store_tables lists tables that have a manifest", {
  p <- .store_project()
  expect_identical(sr_store_tables(p), character())
  .store_make(p, "b_tab")
  .store_make(p, "a_tab")
  dir.create(sr_store_dir(p, "empty_dir"), recursive = TRUE)   # no manifest: not a table
  expect_identical(sr_store_tables(p), c("a_tab", "b_tab"))
})

test_that("store functions validate table names and missing manifests", {
  p <- .store_project()
  expect_error(sr_store_dir(p, "Bad Name"), "table name")
  expect_error(sr_store_dir(p, "../x"), "table name")
  expect_error(sr_store_dir(p, "1abc"), "table name")
  expect_error(sr_store_dir(p, strrep("a", 64)), "table name")
  expect_no_error(sr_store_dir(p, strrep("a", 63)))
  expect_error(sr_store_manifest(p, "nope"), "no manifest")
  expect_error(sr_store_view(p, "nope"), "no manifest")
  expect_error(sr_store_write_manifest(p, "t", list(table = "t")), "manifest")
})

test_that("sr_table_page pages server-side under order_by", {
  p <- .store_project()
  .store_make(p, "visits")
  d <- sr_table_page(p, "visits", offset = 2, limit = 3, order_by = "id")
  expect_s3_class(d, "data.frame")
  expect_identical(as.integer(d$id), 3:5)
  expect_identical(d$v, c("a3", "a4", "a5"))
  d <- sr_table_page(p, "visits", offset = 8, limit = 5, order_by = "id")
  expect_identical(as.integer(d$id), 9:10)
  d <- sr_table_page(p, "visits", offset = 0, limit = 2, order_by = "id", desc = TRUE)
  expect_identical(as.integer(d$id), c(10L, 9L))
  expect_identical(nrow(sr_table_page(p, "visits", offset = 0, limit = 1000)), 10L)
  expect_identical(nrow(sr_table_page(p, "visits", offset = 50, limit = 5, order_by = "id")), 0L)
})

test_that("sr_table_page refuses a large limit, bad paging and raw SQL in order_by", {
  p <- .store_project()
  .store_make(p, "visits")
  expect_error(sr_table_page(p, "visits", 0, 1001), "limit")
  expect_error(sr_table_page(p, "visits", 0, -1), "limit")
  expect_error(sr_table_page(p, "visits", -1, 5), "offset")
  expect_error(sr_table_page(p, "visits", NA, 5), "offset")
  expect_error(sr_table_page(p, "visits", 0, c(1, 2)), "limit")
  expect_error(sr_table_page(p, "visits", 0, 5, order_by = "nope"), "order_by")
  expect_error(sr_table_page(p, "visits", 0, 5, order_by = "id; DROP VIEW visits"), "order_by")
  expect_error(sr_table_page(p, "visits", 0, 5, order_by = "id DESC, v"), "order_by")
  expect_error(sr_table_page(p, "no_such", 0, 5), "no manifest")
  # the view survived: nothing was injected
  expect_identical(nrow(sr_table_page(p, "visits", 0, 100, order_by = "id")), 10L)
})

test_that("sr_table_page orders by a column whose name needs quoting", {
  p <- .store_project()
  con <- sr_duck(p)
  dir <- sr_store_dir(p, "odd")
  dir.create(dir, recursive = TRUE)
  f <- file.path(dir, "part-0001.parquet")
  sr_duck_exec(con, sprintf(
    "COPY (SELECT i AS \"my col\", 10 - i AS \"x\"\"y\" FROM range(0, 6) t(i)) TO %s (FORMAT parquet)",
    DBI::dbQuoteString(con, f)))
  sr_store_write_manifest(p, "odd", list(
    table = "odd", rows = 6,
    parts = list(list(file = "part-0001.parquet", rows = 6, sha256 = sr_sha256_file(f))),
    schema = list(list(name = "my col", type = "BIGINT"), list(name = "x\"y", type = "BIGINT"))))
  d <- sr_table_page(p, "odd", 0, 2, order_by = "x\"y")
  expect_identical(as.integer(d[["my col"]]), c(5L, 4L))
})

# ---- fingerprints ----------------------------------------------------------------------------

.big_file <- function(bytes, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".bin", .local_envir = env)
  set.seed(7)
  writeBin(as.raw(sample.int(256L, bytes, replace = TRUE) - 1L), path)
  path
}

.flip <- function(path, pos) {
  b <- readBin(path, "raw", file.size(path))
  b[pos] <- as.raw(bitwXor(as.integer(b[pos]), 0xFFL))
  writeBin(b, path)
}

test_that("sr_fingerprint is stable and changes when a byte in the middle changes", {
  f <- .big_file(5 * 1024 * 1024)
  a <- sr_fingerprint(f)
  expect_named(a, c("size", "mtime", "sample_sha"))
  expect_identical(a$size, as.numeric(file.size(f)))
  expect_match(a$sample_sha, "^[0-9a-f]{64}$")
  expect_match(a$mtime, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+Z$")
  expect_identical(sr_fingerprint(f), a)
  .flip(f, floor(file.size(f) / 2))
  b <- sr_fingerprint(f)
  expect_identical(b$size, a$size)
  expect_false(identical(b$sample_sha, a$sample_sha))
  # start and end blocks count too
  f2 <- .big_file(5 * 1024 * 1024)
  a2 <- sr_fingerprint(f2)
  .flip(f2, 10)
  expect_false(identical(sr_fingerprint(f2)$sample_sha, a2$sample_sha))
  .flip(f2, 10)
  expect_identical(sr_fingerprint(f2)$sample_sha, a2$sample_sha)
  .flip(f2, file.size(f2) - 3)
  expect_false(identical(sr_fingerprint(f2)$sample_sha, a2$sample_sha))
})

test_that("sr_fingerprint handles tiny, empty and missing files", {
  f <- withr::local_tempfile()
  writeBin(charToRaw("hello"), f)
  a <- sr_fingerprint(f)
  expect_identical(a$size, 5)
  expect_identical(a$sample_sha, .sr_sha256(charToRaw("hello")))
  writeBin(raw(0), f)
  expect_identical(sr_fingerprint(f)$size, 0)
  expect_match(sr_fingerprint(f)$sample_sha, "^[0-9a-f]{64}$")
  expect_error(sr_fingerprint(file.path(tempdir(), "no_such_file_zz.bin")), "not found")
})

test_that("sr_sha256_file matches openssl over the whole file, with and without progress", {
  f <- .big_file(20 * 1024 * 1024 + 123)
  rc <- file(f, "rb")
  ref <- paste(as.character(unclass(openssl::sha256(rc))), collapse = "")
  close(rc)
  expect_identical(sr_sha256_file(f), ref)
  seen <- list()
  expect_identical(sr_sha256_file(f, progress = function(done, total) seen[[length(seen) + 1L]] <<- c(done, total)),
                   ref)
  done <- vapply(seen, function(s) s[1L], numeric(1))
  expect_gte(length(done), 3L)                             # 8 MiB blocks over 20 MiB
  expect_true(all(diff(done) > 0))
  expect_identical(done[length(done)], as.numeric(file.size(f)))
  expect_true(all(vapply(seen, function(s) s[2L], numeric(1)) == file.size(f)))
  e <- withr::local_tempfile()
  writeBin(raw(0), e)
  expect_identical(sr_sha256_file(e, progress = function(d, t) NULL), .sr_sha256(raw(0)))
  expect_error(sr_sha256_file(file.path(tempdir(), "no_such_file_zz.bin")), "not found")
})
