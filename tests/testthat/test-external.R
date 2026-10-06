# Tests for the external-data lifecycle in io/ingest.R: sr_source_status, sr_source_relocate,
# sr_source_hash and sr_tables_needing_hash. Synthetic data only, under withr::local_tempdir().

.ex_project <- function(env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  p
}

.ex_drive <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())

# A ~220 KB synthetic CSV (smaller than one fingerprint block, so every byte is sampled) in its own
# temp dir; returns its path.
.ex_csv <- function(name = "ext.csv", n = 8000L, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, name)
  df <- data.frame(id = seq_len(n), v = sprintf("value_%05d", seq_len(n)), stringsAsFactors = FALSE)
  .fx_write_csv(df, path)
  path
}

.ex_ingest <- function(p, csv, table, external = TRUE) {
  sr_ingest(p, csv, table = table, external = external, options = list(drive_info = .ex_drive))
}

# Flip one byte in the middle of a file in place. The size is unchanged.
.ex_edit_middle <- function(path) {
  size <- file.size(path)
  con <- file(path, "r+b")
  on.exit(close(con), add = TRUE)
  at <- floor(size / 2)
  seek(con, where = at, origin = "start", rw = "read")
  b <- readBin(con, "raw", n = 1L)
  seek(con, where = at, origin = "start", rw = "write")
  writeBin(as.raw(bitwXor(as.integer(b), 1L)), con)
  invisible(path)
}

test_that("an external source is ok straight after ingest", {
  p <- .ex_project()
  csv <- .ex_csv()
  p <- .ex_ingest(p, csv, "t")
  st <- sr_source_status(p, "t")
  expect_identical(st$status, "ok")
  expect_identical(st$path, normalizePath(csv, winslash = "/"))
  expect_identical(st$detail, "")
  expect_error(sr_source_status(p, "nope"), "unknown table")
})

test_that("a moved source is missing, an edited one is changed (same size, fingerprint differs)", {
  p <- .ex_project()
  csv <- .ex_csv()
  p <- .ex_ingest(p, csv, "t")
  fp0 <- sr_fingerprint(csv)
  # edit one byte in the middle; the file is under one fingerprint block so the byte is sampled
  .ex_edit_middle(csv)
  fp1 <- sr_fingerprint(csv)
  expect_identical(fp1$size, fp0$size)
  expect_false(identical(fp1$sample_sha, fp0$sample_sha))
  st <- sr_source_status(p, "t")
  expect_identical(st$status, "changed")
  expect_identical(st$detail, "size/mtime/sampled blocks differ from import")
  # a file renamed away is missing, and the detail names the path
  moved <- paste0(csv, ".moved")
  expect_true(file.rename(csv, moved))
  st <- sr_source_status(p, "t")
  expect_identical(st$status, "missing")
  expect_identical(st$detail, paste0("source file not found at ", st$path))
})

test_that("a changed mtime alone is also changed for an external source", {
  p <- .ex_project()
  csv <- .ex_csv()
  p <- .ex_ingest(p, csv, "t")
  Sys.setFileTime(csv, Sys.time() + 3600)
  expect_identical(sr_source_status(p, "t")$status, "changed")
})

test_that("relocating to a byte-identical copy restores ok and appends the event; a different file errors", {
  p <- .ex_project()
  csv <- .ex_csv()
  p <- .ex_ingest(p, csv, "t")
  fp_old <- p$state$tables$t$source$fingerprint
  copy <- file.path(withr::local_tempdir(), "moved_copy.csv")
  expect_true(file.copy(csv, copy))           # a plain copy: new mtime, same bytes
  expect_true(file.remove(csv))
  expect_identical(sr_source_status(p, "t")$status, "missing")
  n0 <- length(sr_events_read(p$paths$events))
  p <- sr_source_relocate(p, "t", copy)
  expect_identical(sr_source_status(p, "t")$status, "ok")
  evs <- sr_events_read(p$paths$events)
  expect_length(evs, n0 + 1L)
  ev <- evs[[length(evs)]]
  expect_identical(ev$type, "data.source_relocated")
  expect_identical(ev$payload$table, "t")
  expect_identical(ev$payload$old_path, normalizePath(csv, winslash = "/", mustWork = FALSE))
  expect_identical(ev$payload$new_path, normalizePath(copy, winslash = "/"))
  expect_identical(ev$payload$fingerprint$sample_sha, fp_old$sample_sha)
  expect_identical(p$state$tables$t$source$path, normalizePath(copy, winslash = "/"))
  # a different file (other bytes) is refused and records nothing
  other <- file.path(withr::local_tempdir(), "other.csv")
  .fx_write_csv(data.frame(id = 1:50, v = "x"), other)
  n1 <- length(sr_events_read(p$paths$events))
  expect_error(sr_source_relocate(p, "t", other), "fingerprint mismatch")
  expect_length(sr_events_read(p$paths$events), n1)
  # an edited copy (same size, one byte flipped) is also a mismatch
  bad <- file.path(withr::local_tempdir(), "bad.csv")
  file.copy(copy, bad)
  .ex_edit_middle(bad)
  expect_error(sr_source_relocate(p, "t", bad), "fingerprint mismatch")
  # a missing target and a URL are refused
  expect_error(sr_source_relocate(p, "t", file.path(dirname(copy), "no_such.csv")), "file not found")
  expect_error(sr_source_relocate(p, "t", "https://example.org/x.csv"), "URL")
})

test_that("relocating an in-project source is refused: re-import instead", {
  p <- .ex_project()
  csv <- .ex_csv()
  p <- .ex_ingest(p, csv, "t", external = FALSE)
  expect_false(p$state$tables$t$source$external)
  n0 <- length(sr_events_read(p$paths$events))
  expect_error(sr_source_relocate(p, "t", csv), "stored inside the project.*re-import")
  expect_length(sr_events_read(p$paths$events), n0)
})

test_that("sr_source_hash sets sha256 to the full-file hash, records the event, and survives a reopen", {
  p <- .ex_project()
  csv <- .ex_csv()
  p <- .ex_ingest(p, csv, "t")
  expect_identical(sr_tables_needing_hash(p), "t")
  seen <- 0L
  p <- sr_source_hash(p, "t", progress = function(done, total) seen <<- seen + 1L)
  expect_gt(seen, 0L)
  expect_identical(p$state$tables$t$source$sha256, sr_sha256_file(csv))
  ev <- sr_events_read(p$paths$events)
  ev <- ev[[length(ev)]]
  expect_identical(ev$type, "data.source_hashed")
  expect_identical(ev$payload$sha256, sr_sha256_file(csv))
  expect_identical(sr_tables_needing_hash(p), character(0))
  # the reducer merges: the rest of the entry survives
  expect_true(p$state$tables$t$source$external)
  expect_equal(p$state$tables$t$store$rows, 8000)
  expect_identical(p$state$tables$t$format, "csv")
  # and a replay from disk gives the same state
  dir <- p$paths$dir
  sr_duck_close(p)
  sr_project_close(p)
  q <- sr_project_open(dir)
  withr::defer({ sr_duck_close(q); sr_project_close(q) })
  expect_identical(q$state$tables$t$source$sha256, sr_sha256_file(csv))
  expect_identical(sr_tables_needing_hash(q), character(0))
})

test_that("the hash reducers merge into the entry and keep keys they do not carry", {
  p <- .ex_project()
  csv <- .ex_csv()
  p <- .ex_ingest(p, csv, "t")
  sr_reducer_register("test.ext_mark", function(state, event) {
    state$tables$t$active <- "marker"
    state
  })
  p <- sr_project_append(p, "test.ext_mark", list())
  p <- sr_source_hash(p, "t")
  expect_identical(p$state$tables$t$active, "marker")
  copy <- file.path(withr::local_tempdir(), "c.csv")
  file.copy(csv, copy)
  p <- sr_source_relocate(p, "t", copy)
  expect_identical(p$state$tables$t$active, "marker")
  expect_equal(p$state$tables$t$store$rows, 8000)
})

test_that("sr_source_hash refuses a changed or missing source and records nothing", {
  p <- .ex_project()
  csv <- .ex_csv()
  p <- .ex_ingest(p, csv, "t")
  .ex_edit_middle(csv)
  n0 <- length(sr_events_read(p$paths$events))
  expect_error(sr_source_hash(p, "t"), "changed since import")
  expect_length(sr_events_read(p$paths$events), n0)
  expect_identical(sr_tables_needing_hash(p), "t")
  unlink(csv)
  expect_error(sr_source_hash(p, "t"), "source file not found")
  expect_length(sr_events_read(p$paths$events), n0)
})

test_that("sr_source_hash refuses a source that changes while it is being hashed", {
  p <- .ex_project()
  csv <- .ex_csv()
  p <- .ex_ingest(p, csv, "t")
  n0 <- length(sr_events_read(p$paths$events))
  edited <- FALSE
  expect_error(
    sr_source_hash(p, "t", progress = function(done, total) {
      if (!edited) {
        .ex_edit_middle(csv)
        edited <<- TRUE
      }
    }),
    "changed while")
  expect_true(edited)
  expect_length(sr_events_read(p$paths$events), n0)
  expect_identical(sr_tables_needing_hash(p), "t")
})

test_that("an in-project source with a known sha256 is returned unchanged by sr_source_hash", {
  p <- .ex_project()
  csv <- .ex_csv()
  p <- .ex_ingest(p, csv, "t", external = FALSE)
  expect_identical(p$state$tables$t$source$sha256, sr_sha256_file(csv))
  n0 <- length(sr_events_read(p$paths$events))
  p2 <- sr_source_hash(p, "t")
  expect_identical(p2$state, p$state)
  expect_length(sr_events_read(p$paths$events), n0)
})

test_that("a relocate sets sha256 back to NA so the table needs hashing again", {
  p <- .ex_project()
  csv <- .ex_csv()
  p <- .ex_ingest(p, csv, "t")
  p <- sr_source_hash(p, "t")
  expect_identical(sr_tables_needing_hash(p), character(0))
  copy <- file.path(withr::local_tempdir(), "again.csv")
  file.copy(csv, copy)
  p <- sr_source_relocate(p, "t", copy)
  expect_true(is.na(p$state$tables$t$source$sha256))
  expect_identical(sr_tables_needing_hash(p), "t")
  p <- sr_source_hash(p, "t")
  expect_identical(p$state$tables$t$source$sha256, sr_sha256_file(copy))
})

test_that("sr_tables_needing_hash lists tables in table order; an empty project gives character(0)", {
  p <- .ex_project()
  expect_identical(sr_tables_needing_hash(p), character(0))
  a <- .ex_csv("a.csv")
  b <- .ex_csv("b.csv", n = 500L)
  d <- .ex_csv("d.csv", n = 700L)
  p <- .ex_ingest(p, a, "t_a")
  p <- .ex_ingest(p, b, "t_b", external = FALSE)     # in-project: hash known at import
  p <- .ex_ingest(p, d, "t_d")
  expect_identical(sr_tables_needing_hash(p), c("t_a", "t_d"))
  p <- sr_source_hash(p, "t_a")
  expect_identical(sr_tables_needing_hash(p), "t_d")
})

test_that("a copied project folder with an in-project source reopens with status ok", {
  p <- .ex_project()
  csv <- .ex_csv()
  p <- .ex_ingest(p, csv, "t", external = FALSE)
  expect_identical(sr_source_status(p, "t")$status, "ok")
  src <- p$paths$dir
  sr_duck_close(p)
  sr_project_close(p)
  dest_parent <- withr::local_tempdir()
  expect_true(file.copy(src, dest_parent, recursive = TRUE))
  dest <- file.path(dest_parent, basename(src))
  q <- sr_project_open(dest)
  withr::defer({ sr_duck_close(q); sr_project_close(q) })
  expect_false(q$readonly)
  st <- sr_source_status(q, "t")
  expect_identical(st$status, "ok")
  expect_identical(st$path, normalizePath(file.path(dest, "data", "source", "t.csv"), winslash = "/"))
  # damage the copied source: changed; remove it: missing
  .ex_edit_middle(st$path)
  expect_identical(sr_source_status(q, "t")$status, "changed")
  expect_identical(sr_source_status(q, "t")$detail, "size/sampled blocks differ from import")
  unlink(st$path)
  expect_identical(sr_source_status(q, "t")$status, "missing")
})
