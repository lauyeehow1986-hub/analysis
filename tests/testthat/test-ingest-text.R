# Tests for io/ingest.R and io/read_text.R: chunked, resumable delimited-text ingest into the Parquet
# store. Synthetic data only; everything lives under withr::local_tempdir().

NL <- intToUtf8(10L)
CR <- intToUtf8(13L)
CRNL <- paste0(CR, NL)

# A fresh project whose DuckDB connection is closed before the temp dir is removed.
.ing_project <- function(env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  p
}

# A drive probe with plenty of space (the real one depends on the machine running the tests).
.ing_drive <- function(free = 1e13) {
  function(path) list(free_bytes = free, remote = FALSE, warnings = character())
}

# sr_ingest with the stub drive probe unless the caller passes its own.
.ing <- function(p, csv, ..., options = list()) {
  if (is.null(options$drive_info)) options$drive_info <- .ing_drive()
  sr_ingest(p, csv, ..., options = options)
}

# Write lines to a file as UTF-8 bytes with LF endings (no platform newline translation).
.ing_lines <- function(lines, path) {
  writeBin(charToRaw(paste0(paste(lines, collapse = NL), NL)), path)
  invisible(path)
}

.ing_cardiac_csv <- function(p, n = 2000L, name = "cardiac.csv") {
  s <- sr_sample_cardiac(n = n)
  csv <- file.path(dirname(p$paths$dir), name)
  .fx_write_csv(s$data, csv)
  list(csv = csv, data = s$data)
}

.ing_schema <- function(p, table) {
  t <- p$state$tables[[table]]$store$schema
  stats::setNames(vapply(t, function(x) x$type, ""), vapply(t, function(x) x$name, ""))
}

# The whole table, in a fixed order (every column), through sr_table_page.
.ing_all <- function(p, table) {
  m <- sr_store_manifest(p, table)
  cols <- vapply(m$schema, function(s) s$name, "")
  offs <- seq(0, max(m$rows - 1, 0), by = 1000)
  do.call(rbind, lapply(offs, function(o) sr_table_page(p, table, o, 1000, order_by = cols)))
}

.ing_data <- function(p) sr_project_paths(p$paths$dir)$data
.ing_ck <- function(p, table) .sr_read_json_file(file.path(.ing_data(p), "ingest", table, "checkpoint.json"))
.ing_hash <- function(p, table) p$state$tables[[table]]$store_hash

# ---- the cardiac cohort ----------------------------------------------------------------------------

test_that("the cardiac cohort ingests: rows, columns, planted text in a numeric column, types", {
  p <- .ing_project()
  fx <- .ing_cardiac_csv(p)
  p <- sr_ingest(p, fx$csv, table = "cardiac")                  # the real drive probe
  t <- p$state$tables$cardiac
  expect_identical(t$table, "cardiac")
  expect_identical(t$format, "csv")
  expect_equal(t$store$rows, 2000)
  expect_equal(t$store$ncol, ncol(fx$data))
  expect_match(t$store_hash, "^[0-9a-f]{64}$")
  ty <- .ing_schema(p, "cardiac")
  expect_identical(names(ty), names(fx$data))
  expect_true("creatinine_umol" %in% unlist(t$type_widened))   # the planted "pending"
  expect_identical(ty[["creatinine_umol"]], "VARCHAR")
  expect_true(ty[["age"]] %in% c("BIGINT", "INTEGER", "DOUBLE"))
  expect_identical(ty[["surgery_date"]], "DATE")
  # the content is the source's: ages (with their NAs) and the planted text survive
  d <- .ing_all(p, "cardiac")
  expect_equal(nrow(d), 2000L)
  expect_equal(sort(as.numeric(d$age)), sort(as.numeric(fx$data$age)))
  expect_identical(sum(is.na(d$age)), sum(is.na(fx$data$age)))
  expect_identical(sum(d$creatinine_umol == "pending", na.rm = TRUE),
                   sum(fx$data$creatinine_umol == "pending", na.rm = TRUE))
  expect_identical(sort(format(d$surgery_date)), sort(format(fx$data$surgery_date)))
  # the ledger: the manifest and the replayed state agree
  m <- sr_store_manifest(p, "cardiac")
  expect_identical(sr_store_hash(m), t$store_hash)
  expect_equal(m$rows, 2000)
})

test_that("the event is replayed identically after reopening the project", {
  p <- .ing_project()
  fx <- .ing_cardiac_csv(p, n = 300L)
  p <- .ing(p, fx$csv, table = "cardiac")
  sr_duck_close(p)
  p2 <- sr_project_open(p$paths$dir)
  withr::defer(sr_project_close(p2))
  expect_identical(p2$state$tables$cardiac$store_hash, p$state$tables$cardiac$store_hash)
  expect_equal(p2$state$tables$cardiac$store$rows, 300)
})

# ---- chunking and resuming -------------------------------------------------------------------------

test_that("chunking gives several parts and the same rows and content as a single part", {
  p <- .ing_project()
  fx <- .ing_cardiac_csv(p)
  prog <- numeric()
  p <- .ing(p, fx$csv, table = "one")
  p <- .ing(p, fx$csv, table = "many", chunk_bytes = 20000,
            progress = function(done, total) prog <<- c(prog, done))
  one <- p$state$tables$one
  many <- p$state$tables$many
  expect_length(one$store$parts, 1L)
  expect_gt(length(many$store$parts), 1L)
  expect_false(one$resumable)
  expect_true(many$resumable)
  expect_equal(many$store$rows, 2000)
  expect_equal(sum(vapply(many$store$parts, function(x) x$rows, numeric(1))), 2000)
  expect_equal(.ing_all(p, "many"), .ing_all(p, "one"))
  expect_identical(.ing_schema(p, "many"), .ing_schema(p, "one"))
  expect_true(all(diff(prog) > 0))
  expect_equal(prog[length(prog)], file.size(fx$csv))
  # nothing is left behind next to the finished table except its checkpoint
  expect_identical(list.files(file.path(.ing_data(p), "ingest", "many")), "checkpoint.json")
  expect_false(any(grepl("[.]tmp$", list.files(sr_store_dir(p, "many")))))
  expect_true(.ing_ck(p, "many")$done)
  # same chunking, same bytes: the same ingest twice gives the same store hash
  p <- .ing(p, fx$csv, table = "many2", chunk_bytes = 20000)
  expect_identical(.ing_hash(p, "many2"), .ing_hash(p, "many"))
})

test_that("a cancelled ingest leaves a checkpoint and resumes without duplicating a part", {
  p <- .ing_project()
  fx <- .ing_cardiac_csv(p)
  clean <- .ing(p, fx$csv, table = "clean", chunk_bytes = 20000)
  calls <- 0L
  cancel <- function() { calls <<- calls + 1L; calls >= 2L }
  expect_error(.ing(p, fx$csv, table = "r", chunk_bytes = 20000, cancel = cancel), class = "sr_ingest_cancelled")
  ck <- .ing_ck(p, "r")
  expect_false(ck$done)
  expect_length(ck$parts, 2L)
  expect_null(p$state$tables$r)                                  # nothing was announced
  done <- .ing(p, fx$csv, table = "r", chunk_bytes = 20000)
  r <- done$state$tables$r
  expect_equal(sum(vapply(r$store$parts, function(x) x$rows, numeric(1))), 2000)
  expect_equal(r$store$rows, 2000)
  files <- vapply(r$store$parts, function(x) x$file, "")
  expect_identical(files, sprintf("part-%05d.parquet", seq_along(files)))
  expect_identical(sort(list.files(sr_store_dir(done, "r"), pattern = "parquet$")), sort(files))
  expect_identical(r$store_hash, clean$state$tables$clean$store_hash)
  expect_equal(.ing_all(done, "r"), .ing_all(done, "clean"))
  expect_true(.ing_ck(done, "r")$done)
})

test_that("a checkpoint is not resumed when a part or the source changed: warning, then a clean restart", {
  p <- .ing_project()
  fx <- .ing_cardiac_csv(p)
  clean <- .ing(p, fx$csv, table = "clean", chunk_bytes = 20000)
  calls <- 0L
  cancel <- function() { calls <<- calls + 1L; calls >= 2L }
  expect_error(.ing(p, fx$csv, table = "t1", chunk_bytes = 20000, cancel = cancel), class = "sr_ingest_cancelled")
  part <- file.path(sr_store_dir(p, "t1"), "part-00001.parquet")
  con <- file(part, "ab")
  writeBin(as.raw(0L), con)
  close(con)
  expect_warning(r1 <- .ing(p, fx$csv, table = "t1", chunk_bytes = 20000), "not resuming")
  expect_identical(r1$state$tables$t1$store_hash, clean$state$tables$clean$store_hash)
  # the source itself changed
  calls <- 0L
  expect_error(.ing(p, fx$csv, table = "t2", chunk_bytes = 20000, cancel = cancel), class = "sr_ingest_cancelled")
  ln <- readLines(fx$csv)
  .ing_lines(c(ln, ln[length(ln)]), fx$csv)
  expect_warning(.ing(p, fx$csv, table = "t2", chunk_bytes = 20000), "not resuming")
})

# ---- types -----------------------------------------------------------------------------------------

test_that("a value that breaks an integer column in the LAST chunk widens the column, NULLs nothing", {
  p <- .ing_project()
  n <- 5000L
  v <- as.character(seq_len(n))
  v[c(5L, 50L)] <- ""                                            # real NAs
  v[n] <- "7.5"
  csv <- .ing_lines(c("id,n,t", paste(seq_len(n), v, paste0("row", seq_len(n)), sep = ",")),
                    file.path(dirname(p$paths$dir), "late.csv"))
  p <- .ing(p, csv, table = "late", chunk_bytes = 20000, options = list(sample_size = 100))
  ck <- .ing_ck(p, "late")
  expect_gte(ck$restarts, 1L)                                    # the restart really happened
  expect_gt(length(ck$widen_log), 0L)
  expect_identical(ck$widen_log[[1L]]$column, "n")
  t <- p$state$tables$late
  expect_true("n" %in% unlist(t$type_widened))
  ty <- .ing_schema(p, "late")
  expect_true(ty[["n"]] %in% c("DOUBLE", "VARCHAR"))
  expect_identical(ty[["id"]], "BIGINT")
  d <- .ing_all(p, "late")
  expect_equal(nrow(d), n)
  expect_identical(sum(is.na(d$n)), 2L)                          # only the two real NAs
  expect_true(7.5 %in% as.numeric(d$n))
  expect_equal(sort(as.numeric(d$n)), sort(as.numeric(ifelse(v == "", NA, v))))
  expect_gt(length(t$store$parts), 1L)
  expect_equal(sum(vapply(t$store$parts, function(x) x$rows, numeric(1))), n)
})

test_that("text late in a numeric column makes it VARCHAR with every value kept", {
  p <- .ing_project()
  v <- as.character(seq_len(4000L))
  v[3990L] <- "pending"
  csv <- .ing_lines(c("id,lab", paste(seq_len(4000L), v, sep = ",")), file.path(dirname(p$paths$dir), "lab.csv"))
  p <- .ing(p, csv, table = "lab", chunk_bytes = 10000, options = list(sample_size = 50))
  expect_identical(.ing_schema(p, "lab")[["lab"]], "VARCHAR")
  expect_gte(.ing_ck(p, "lab")$restarts, 1L)
  expect_true("lab" %in% unlist(p$state$tables$lab$type_widened))
  d <- .ing_all(p, "lab")
  expect_identical(sort(d$lab), sort(v))
})

test_that("integers with leading zeros stay text, a mostly numeric text column is flagged", {
  p <- .ing_project()
  csv <- .ing_lines(c("id,zip,score",
                      paste(1:60, sprintf("%05d", 100L + 1:60), c(as.character(1:57), "n/a", "x", "y"), sep = ",")),
                    file.path(dirname(p$paths$dir), "zip.csv"))
  p <- .ing(p, csv, table = "zip")
  ty <- .ing_schema(p, "zip")
  expect_identical(ty[["zip"]], "VARCHAR")
  expect_identical(ty[["score"]], "VARCHAR")
  expect_true(all(c("zip", "score") %in% unlist(p$state$tables$zip$type_widened)))
  expect_false("id" %in% unlist(p$state$tables$zip$type_widened))
  d <- .ing_all(p, "zip")
  expect_true("00101" %in% d$zip)
})

test_that("a semicolon file with decimal commas reads as DOUBLE", {
  p <- .ing_project()
  csv <- .ing_lines(c("id;val;note", paste(1:40, sprintf("%d,%d", 1:40, 5L), paste0("n", 1:40), sep = ";")),
                    file.path(dirname(p$paths$dir), "dec.csv"))
  p <- .ing(p, csv, table = "dec")
  expect_identical(.ing_schema(p, "dec")[["val"]], "DOUBLE")
  d <- .ing_all(p, "dec")
  expect_true(all(abs(sort(d$val) - sort(1:40 + 0.5)) < 1e-9))
  expect_identical(p$state$tables$dec$sniff$decimal, ",")
})

# ---- dates -----------------------------------------------------------------------------------------

test_that("dmy and mdy columns become DATE with the right values; an ambiguous one stays text", {
  p <- .ing_project()
  csv <- .ing_lines(c("id,admit,booked,visit",
                      "1,13/02/2020,02/13/2020,01/02/2020",
                      "2,01/03/2020,01/03/2020,03/04/2020",
                      "3,05/04/2020,05/04/2020,12/12/2021",
                      "4,,,"),
                    file.path(dirname(p$paths$dir), "dates.csv"))
  p <- .ing(p, csv, table = "dates")
  ty <- .ing_schema(p, "dates")
  expect_identical(ty[["admit"]], "DATE")
  expect_identical(ty[["booked"]], "DATE")
  expect_identical(ty[["visit"]], "VARCHAR")
  d <- .ing_all(p, "dates")
  d <- d[order(d$id), ]
  expect_identical(format(d$admit), c("2020-02-13", "2020-03-01", "2020-04-05", NA))
  expect_identical(format(d$booked), c("2020-02-13", "2020-01-03", "2020-05-04", NA))
  expect_identical(d$visit, c("01/02/2020", "03/04/2020", "12/12/2021", NA))
  expect_identical(p$state$tables$dates$sniff$date_order$visit, "ambiguous")
  expect_identical(p$state$tables$dates$sniff$date_order$admit, "dmy")
})

test_that("dotted dmy dates parse; an impossible date widens the column to text, never to NULL", {
  p <- .ing_project()
  csv <- .ing_lines(c("id,d,e", "1,13.02.2020,13/02/2020", "2,01.03.2020,31/04/2020", "3,05.04.2020,01/03/2020"),
                    file.path(dirname(p$paths$dir), "dots.csv"))
  p <- .ing(p, csv, table = "dots")
  ty <- .ing_schema(p, "dots")
  expect_identical(ty[["d"]], "DATE")
  expect_identical(ty[["e"]], "VARCHAR")
  expect_true("e" %in% unlist(p$state$tables$dots$type_widened))
  d <- .ing_all(p, "dots")
  d <- d[order(d$id), ]
  expect_identical(format(d$d), c("2020-02-13", "2020-03-01", "2020-04-05"))
  expect_identical(d$e, c("13/02/2020", "31/04/2020", "01/03/2020"))
})

# ---- quoted newlines -------------------------------------------------------------------------------

test_that("quoted newlines force a single pass (resumable = FALSE) and the text round-trips", {
  p <- .ing_project()
  n <- 300L
  notes <- paste0("line one ", seq_len(n), NL, 'she said "hi", then left')
  df <- data.frame(id = seq_len(n), notes = notes, stringsAsFactors = FALSE)
  csv <- file.path(dirname(p$paths$dir), "notes.csv")
  .fx_write_csv(df, csv)
  p <- .ing(p, csv, table = "notes", chunk_bytes = 1000)
  t <- p$state$tables$notes
  expect_false(t$resumable)
  expect_length(t$store$parts, 1L)
  expect_equal(t$store$rows, n)
  d <- .ing_all(p, "notes")
  expect_identical(sort(gsub(CR, "", d$notes)), sort(notes))
})

test_that("a quoted newline the sniff missed does not split a record when chunking", {
  p <- .ing_project()
  n <- 300L
  notes <- paste0("line one ", seq_len(n), NL, 'she said "hi", then left')
  df <- data.frame(id = seq_len(n), notes = notes, stringsAsFactors = FALSE)
  csv <- file.path(dirname(p$paths$dir), "notes2.csv")
  .fx_write_csv(df, csv)
  sn <- sr_sniff(csv)
  sn$quoted_newlines <- FALSE                                    # pretend the sniff sample missed them
  p <- .ing(p, csv, table = "notes2", sniff = sn, chunk_bytes = 1000)
  t <- p$state$tables$notes2
  expect_true(t$resumable)
  expect_gt(length(t$store$parts), 1L)
  expect_equal(t$store$rows, n)
  d <- .ing_all(p, "notes2")
  expect_identical(sort(gsub(CR, "", d$notes)), sort(notes))
})

test_that("CRLF inside a quoted field is kept byte for byte", {
  p <- .ing_project()
  body <- paste0("id,notes", CR, NL, '1,"a', CR, NL, 'b"', CR, NL, "2,plain", CR, NL)
  csv <- file.path(dirname(p$paths$dir), "crlf.csv")
  writeBin(charToRaw(body), csv)
  p <- .ing(p, csv, table = "crlf")
  d <- .ing_all(p, "crlf")
  d <- d[order(d$id), ]
  expect_identical(d$notes, c(paste0("a", CR, NL, "b"), "plain"))
})

# ---- compression and encodings ---------------------------------------------------------------------

.ing_compress <- function(csv, kind) {
  out <- paste0(csv, ".", kind)
  bytes <- readBin(csv, "raw", n = file.size(csv))
  if (kind == "zip") {
    zip::zipr(out, csv, mode = "cherry-pick")
    return(out)
  }
  con <- switch(kind, gz = gzfile(out, "wb"), bz2 = bzfile(out, "wb"), xz = xzfile(out, "wb"))
  writeBin(bytes, con)
  close(con)
  out
}

test_that("gz, bz2, xz and zip sources give the same store as the plain CSV", {
  p <- .ing_project()
  fx <- .ing_cardiac_csv(p, n = 500L)
  p <- .ing(p, fx$csv, table = "plain")
  p <- .ing(p, fx$csv, table = "plain_c", chunk_bytes = 20000)
  for (kind in c("gz", "bz2", "xz", "zip")) {
    f <- .ing_compress(fx$csv, kind)
    tb <- paste0("c_", kind)
    p <- .ing(p, f, table = tb)
    expect_identical(.ing_hash(p, tb), .ing_hash(p, "plain"), info = kind)
    expect_equal(p$state$tables[[tb]]$store$rows, 500, info = kind)
    expect_identical(p$state$tables[[tb]]$resumable, FALSE, info = kind)
    if (kind != "gz") {
      tc <- paste0(tb, "_chunked")
      p <- .ing(p, f, table = tc, chunk_bytes = 20000)
      expect_identical(.ing_hash(p, tc), .ing_hash(p, "plain_c"), info = kind)
      expect_true(p$state$tables[[tc]]$resumable, info = kind)
      expect_identical(list.files(file.path(.ing_data(p), "ingest", tc)), "checkpoint.json", info = kind)
    }
  }
  expect_gt(length(p$state$tables$plain_c$store$parts), 1L)
})

test_that("Latin-1 and UTF-16LE sources give the same UTF-8 values, a micro sign in the header included", {
  p <- .ing_project()
  mu <- intToUtf8(181L)
  eacute <- intToUtf8(233L)
  n <- 120L
  hdr <- paste0("id,creat_", mu, "mol_l,name")
  rows <- paste(seq_len(n), 50 + seq_len(n), paste0("caf", eacute, seq_len(n)), sep = ",")
  utf8 <- file.path(dirname(p$paths$dir), "u8.csv")
  writeBin(charToRaw(paste0(paste(c(hdr, rows), collapse = CRNL), CR, NL)), utf8)
  lat <- file.path(dirname(p$paths$dir), "lat.csv")
  writeBin(iconv(paste0(paste(c(hdr, rows), collapse = CRNL), CR, NL), "UTF-8", "latin1", toRaw = TRUE)[[1L]], lat)
  u16 <- file.path(dirname(p$paths$dir), "u16.csv")
  writeBin(c(as.raw(c(0xFF, 0xFE)),
             iconv(paste0(paste(c(hdr, rows), collapse = CRNL), CR, NL), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1L]]), u16)
  expect_identical(sr_sniff(lat)$encoding, "latin1")
  expect_identical(sr_sniff(u16)$encoding, "UTF-16LE")
  p <- .ing(p, utf8, table = "u8", chunk_bytes = 300)
  p <- .ing(p, lat, table = "lat", chunk_bytes = 300)
  p <- .ing(p, u16, table = "u16", chunk_bytes = 300)
  expect_gt(length(p$state$tables$u8$store$parts), 1L)
  expect_identical(.ing_hash(p, "lat"), .ing_hash(p, "u8"))
  expect_identical(.ing_hash(p, "u16"), .ing_hash(p, "u8"))
  expect_true(paste0("creat_", mu, "mol_l") %in% names(.ing_schema(p, "lat")))
  d <- .ing_all(p, "u16")
  expect_true(paste0("caf", eacute, "7") %in% d$name)
  expect_identical(list.files(file.path(.ing_data(p), "ingest", "u16")), "checkpoint.json")
  # a one-pass read of the converted file agrees too
  p <- .ing(p, lat, table = "lat1")
  p <- .ing(p, utf8, table = "u81")
  expect_identical(.ing_hash(p, "lat1"), .ing_hash(p, "u81"))
})

# ---- the source: copy or reference -----------------------------------------------------------------

test_that("external = TRUE references the file in place; FALSE copies it with its SHA-256", {
  p <- .ing_project()
  fx <- .ing_cardiac_csv(p, n = 200L)
  src <- file.path(.ing_data(p), "source")
  p <- .ing(p, fx$csv, table = "ext", external = TRUE)
  e <- p$state$tables$ext$source
  expect_true(e$external)
  expect_identical(e$path, normalizePath(fx$csv, winslash = "/"))
  expect_true(is.null(e$sha256) || is.na(e$sha256))
  expect_length(list.files(src), 0L)
  expect_match(e$fingerprint$sample_sha, "^[0-9a-f]{64}$")
  p <- .ing(p, fx$csv, table = "cp", external = FALSE)
  c1 <- p$state$tables$cp$source
  expect_false(c1$external)
  expect_identical(c1$path, "data/source/cp.csv")
  expect_identical(c1$sha256, sr_sha256_file(fx$csv))
  expect_identical(sr_sha256_file(file.path(src, "cp.csv")), c1$sha256)
  expect_identical(p$state$tables$cp$store_hash, p$state$tables$ext$store_hash)
  # a compressed source keeps its suffixes
  p <- .ing(p, .ing_compress(fx$csv, "gz"), table = "cpgz", external = FALSE)
  expect_identical(p$state$tables$cpgz$source$path, "data/source/cpgz.csv.gz")
})

test_that("external = NULL follows the pre-flight's suggestion (a source over 2 GB is referenced)", {
  p <- .ing_project()
  fx <- .ing_cardiac_csv(p, n = 200L)
  p <- .ing(p, fx$csv, table = "small")
  expect_false(p$state$tables$small$source$external)
  sn <- sr_sniff(fx$csv)
  sn$size_bytes <- 3 * 1024^3
  p <- .ing(p, fx$csv, table = "big", sniff = sn)
  expect_true(p$state$tables$big$source$external)
  expect_length(list.files(file.path(.ing_data(p), "source"), pattern = "^big"), 0L)
})

# ---- the pre-flight --------------------------------------------------------------------------------

test_that("a pre-flight refusal stops before anything is created under data/", {
  p <- .ing_project()
  fx <- .ing_cardiac_csv(p, n = 200L)
  data <- .ing_data(p)
  snap <- function() sort(list.files(data, recursive = TRUE, include.dirs = TRUE, all.files = TRUE))
  before <- snap()
  tight <- .ing_drive(free = 1000)
  expect_error(sr_ingest(p, fx$csv, table = "nope", options = list(drive_info = tight)), "insufficient space")
  expect_error(sr_ingest(p, fx$csv, table = "nope", external = TRUE, options = list(drive_info = tight)),
               "insufficient space")
  expect_identical(snap(), before)
  expect_false(dir.exists(file.path(data, "store", "nope")))
  expect_false(dir.exists(file.path(data, "ingest", "nope")))
  unknown <- function(path) list(free_bytes = NA_real_, remote = FALSE, warnings = character())
  expect_error(sr_ingest(p, fx$csv, table = "nope", options = list(drive_info = unknown)), "free space")
  expect_identical(snap(), before)
  expect_null(p$state$tables$nope)
})

# ---- readers, names, state -------------------------------------------------------------------------

test_that("tsv and txt map to the csv reader and an unknown format is refused", {
  expect_true(all(c("csv", "tsv", "txt") %in% ls(.sr_readers)))
  expect_identical(.sr_reader_get("tsv"), .sr_reader_get("csv"))
  p <- .ing_project()
  fx <- .ing_cardiac_csv(p, n = 100L)
  sn <- sr_sniff(fx$csv)
  sn$format <- "weirdformat"
  expect_error(.ing(p, fx$csv, table = "w", sniff = sn), "no reader registered for format weirdformat")
  expect_error(.sr_reader_get(NA_character_), "no reader registered for format NA")
  tab <- file.path(dirname(p$paths$dir), "t.tsv")
  .ing_lines(c("id\tx", paste(1:30, paste0("v", 1:30), sep = "\t")), tab)
  p <- .ing(p, tab, table = "tabbed")
  expect_equal(p$state$tables$tabbed$store$rows, 30)
  expect_identical(p$state$tables$tabbed$sniff$delim, "\t")
})

test_that("sr_table_name derives a valid, unique name", {
  expect_identical(sr_table_name("C:/data/My Labs (v2).csv"), "my_labs__v2_")
  expect_identical(sr_table_name("2020 labs.csv.gz"), "t_2020_labs")
  expect_identical(sr_table_name("labs.csv", existing = c("labs", "labs_2")), "labs_3")
  expect_identical(sr_table_name("visits.CSV", existing = "other"), "visits")
  for (nm in c("x.csv", "123.csv", "A-B_C.txt", paste0(strrep("long", 30), ".csv"))) {
    expect_match(sr_table_name(nm), "^[a-z][a-z0-9_]{0,62}$")
  }
  expect_error(sr_table_name(""), "path")
})

test_that("re-ingesting a table replaces its entry but keeps keys the payload does not carry", {
  p <- .ing_project()
  fx <- .ing_cardiac_csv(p, n = 150L)
  p <- .ing(p, fx$csv, table = "again")
  p$state$tables$again$active <- list(rows = "kept")
  p <- .ing(p, fx$csv, table = "again", chunk_bytes = 5000)
  t <- p$state$tables$again
  expect_identical(t$active, list(rows = "kept"))
  expect_equal(t$store$rows, 150)
  expect_gt(length(t$store$parts), 1L)
  expect_equal(length(sr_store_manifest(p, "again")$parts), length(t$store$parts))
})

test_that("bad arguments are refused before any work", {
  p <- .ing_project()
  fx <- .ing_cardiac_csv(p, n = 100L)
  expect_error(.ing(p, fx$csv, table = "Bad Name"), "invalid table name")
  expect_error(.ing(p, fx$csv, table = "ok", chunk_bytes = 0), "chunk_bytes")
  expect_error(.ing(p, fx$csv, table = "ok", progress = 1), "progress")
  expect_error(.ing(p, fx$csv, table = "ok", external = NA), "external")
  expect_error(.ing(p, file.path(dirname(fx$csv), "missing.csv"), table = "ok"), "not found")
  expect_error(.ing(p, fx$csv, table = "ok", options = list(sample_size = 0)), "sample_size")
  expect_false(dir.exists(file.path(.ing_data(p), "store", "ok")) && length(list.files(sr_store_dir(p, "ok"))) > 0L)
})
