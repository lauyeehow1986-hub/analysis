# Tests for format, encoding and dialect sniffing (app/R/io/sniff.R).

# --- fixture writers (all under a per-test tempdir) -------------------------------------------

.sn_dir <- function(env = parent.frame()) withr::local_tempdir(.local_envir = env)

.sn_text <- function(lines, path, eol = "\n") {
  writeBin(charToRaw(paste0(paste(lines, collapse = eol), eol)), path)
  invisible(path)
}

.sn_raw <- function(bytes, path) {
  writeBin(bytes, path)
  invisible(path)
}

.sn_comma <- c("id,name,age", "1,alice,34", "2,bob,41", "3,carol,29")

.sn_bytes <- function(...) as.raw(c(...))

# --- delimiter, decimal, header ---------------------------------------------------------------

test_that("comma, semicolon, tab and pipe files give the right delim and decimal", {
  d <- .sn_dir()
  s <- sr_sniff(.sn_text(.sn_comma, file.path(d, "a.csv")))
  expect_identical(s$format, "csv")
  expect_identical(s$compression, "none")
  expect_identical(s$delim, ",")
  expect_identical(s$decimal, ".")
  expect_true(s$header)
  expect_identical(s$encoding, "UTF-8")
  expect_false(s$bom)
  expect_identical(s$quote, "\"")
  expect_false(s$quoted_newlines)
  expect_identical(s$size_bytes, as.numeric(file.size(file.path(d, "a.csv"))))
  expect_identical(s$path, normalizePath(file.path(d, "a.csv"), winslash = "/"))
  expect_length(s$warnings, 0L)

  semi <- c("id;weight;sbp", "1;70,5;120", "2;82,25;135", "3;64,1;110", "4;90,0;142")
  s2 <- sr_sniff(.sn_text(semi, file.path(d, "b.csv")))
  expect_identical(s2$delim, ";")
  expect_identical(s2$decimal, ",")
  expect_true(s2$header)

  expect_identical(sr_sniff(.sn_text(gsub(",", "\t", .sn_comma, fixed = TRUE),
                                     file.path(d, "c.tsv")))$delim, "\t")
  expect_identical(sr_sniff(.sn_text(gsub(",", "|", .sn_comma, fixed = TRUE),
                                     file.path(d, "d.txt")))$delim, "|")
})

test_that("the delimiter is counted outside quotes and a decimal point stays a point", {
  d <- .sn_dir()
  lines <- c("id,note,score", "1,\"a;b;c;d\",1.5", "2,\"x;y;z;w\",2.5", "3,plain,3.5")
  s <- sr_sniff(.sn_text(lines, file.path(d, "q.csv")))
  expect_identical(s$delim, ",")
  expect_identical(s$decimal, ".")
})

test_that("a header made of data values is reported as no header", {
  d <- .sn_dir()
  s <- sr_sniff(.sn_text(c("1,34,70.5", "2,41,82.1", "3,29,64.3"), file.path(d, "nh.csv")))
  expect_false(s$header)
})

test_that("CRLF line endings do not change the dialect", {
  d <- .sn_dir()
  s <- sr_sniff(.sn_text(.sn_comma, file.path(d, "crlf.csv"), eol = "\r\n"))
  expect_identical(s$delim, ",")
  expect_true(s$header)
  expect_false(s$quoted_newlines)
})

# --- compression ------------------------------------------------------------------------------

test_that("gz, bz2, xz and single-member zip give the same dialect as the plain file", {
  skip_if_not_installed("zip")
  d <- .sn_dir()
  plain <- .sn_text(.sn_comma, file.path(d, "data.csv"))
  txt <- readBin(plain, "raw", n = file.size(plain))

  gzp <- file.path(d, "data.csv.gz")
  con <- gzfile(gzp, "wb"); writeBin(txt, con); close(con)
  bzp <- file.path(d, "data.csv.bz2")
  con <- bzfile(bzp, "wb"); writeBin(txt, con); close(con)
  xzp <- file.path(d, "data.csv.xz")
  con <- xzfile(xzp, "wb"); writeBin(txt, con); close(con)
  zp <- file.path(d, "data.zip")
  zip::zipr(zp, plain, mode = "cherry-pick")

  ref <- sr_sniff(plain)
  for (spec in list(list(gzp, "gz"), list(bzp, "bz2"), list(xzp, "xz"), list(zp, "zip"))) {
    s <- sr_sniff(spec[[1L]])
    expect_identical(s$compression, spec[[2L]], info = spec[[2L]])
    expect_identical(s$format, "csv", info = spec[[2L]])
    expect_identical(s$delim, ref$delim, info = spec[[2L]])
    expect_identical(s$header, ref$header, info = spec[[2L]])
    expect_identical(s$encoding, "UTF-8", info = spec[[2L]])
  }
  s <- sr_sniff(zp)
  expect_identical(s$zip_members, "data.csv")
  expect_identical(s$member, "data.csv")
})

test_that("a multi-member zip returns its members and no format until a member is chosen", {
  skip_if_not_installed("zip")
  d <- .sn_dir()
  a <- .sn_text(.sn_comma, file.path(d, "a.csv"))
  b <- .sn_text(gsub(",", ";", .sn_comma, fixed = TRUE), file.path(d, "b.csv"))
  zp <- file.path(d, "two.zip")
  zip::zipr(zp, c(a, b), mode = "cherry-pick")

  s <- sr_sniff(zp)
  expect_identical(s$compression, "zip")
  expect_setequal(s$zip_members, c("a.csv", "b.csv"))
  expect_true(is.na(s$format))
  expect_true(is.na(s$member))
  expect_true(any(grepl("member", s$warnings)))

  s2 <- sr_sniff(zp, member = "b.csv")
  expect_identical(s2$format, "csv")
  expect_identical(s2$delim, ";")
  expect_identical(s2$member, "b.csv")
  expect_error(sr_sniff(zp, member = "nope.csv"), "member")
})

# --- encoding ---------------------------------------------------------------------------------

test_that("a UTF-16LE BOM file is detected and opens as UTF-8 text", {
  d <- .sn_dir()
  txt <- paste0(paste(c("id,name,age", "1,alice,34", "2,bob,41"), collapse = "\n"), "\n")
  body <- iconv(txt, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1L]]
  p <- .sn_raw(c(.sn_bytes(0xFF, 0xFE), body), file.path(d, "u16.csv"))
  s <- sr_sniff(p)
  expect_identical(s$encoding, "UTF-16LE")
  expect_true(s$bom)
  expect_identical(s$delim, ",")
  expect_true(s$header)
  con <- .sr_open_text(p, s)
  on.exit(close(con), add = TRUE)
  expect_identical(readLines(con), c("id,name,age", "1,alice,34", "2,bob,41"))
})

test_that("a UTF-16BE BOM file is detected and opens as UTF-8 text", {
  d <- .sn_dir()
  txt <- paste0(paste(c("id,name", "1,alice"), collapse = "\n"), "\n")
  body <- iconv(txt, "UTF-8", "UTF-16BE", toRaw = TRUE)[[1L]]
  p <- .sn_raw(c(.sn_bytes(0xFE, 0xFF), body), file.path(d, "u16be.csv"))
  s <- sr_sniff(p)
  expect_identical(s$encoding, "UTF-16BE")
  expect_true(s$bom)
  con <- .sr_open_text(p, s)
  on.exit(close(con), add = TRUE)
  expect_identical(readLines(con), c("id,name", "1,alice"))
})

test_that("a UTF-8 BOM is stripped on open", {
  d <- .sn_dir()
  p <- .sn_raw(c(.sn_bytes(0xEF, 0xBB, 0xBF), charToRaw("id,name\n1,alice\n")),
               file.path(d, "bom.csv"))
  s <- sr_sniff(p)
  expect_identical(s$encoding, "UTF-8")
  expect_true(s$bom)
  con <- .sr_open_text(p, s)
  expect_identical(readLines(con)[1L], "id,name")
  close(con)
})

test_that("a Latin-1 file is detected and its header round-trips to UTF-8", {
  d <- .sn_dir()
  mu <- intToUtf8(c(0xB5, 0x6D, 0x6F, 0x6C))
  hdr <- paste0("id,creat_", mu, "L")
  raw <- iconv(paste0(hdr, "\n1,88\n2,91\n"), "UTF-8", "latin1", toRaw = TRUE)[[1L]]
  expect_false(validUTF8(rawToChar(raw)))
  p <- .sn_raw(raw, file.path(d, "l1.csv"))
  s <- sr_sniff(p)
  expect_identical(s$encoding, "latin1")
  expect_false(s$bom)
  con <- .sr_open_text(p, s)
  lines <- readLines(con)
  close(con)
  expect_identical(lines[1L], hdr)
  expect_identical(charToRaw(enc2utf8(lines[1L])), charToRaw(enc2utf8(hdr)))
})

test_that("a multi-byte UTF-8 character cut by the sample boundary is not mistaken for latin1", {
  d <- .sn_dir()
  e <- intToUtf8(0xE9)
  lines <- c("id,name", rep(paste0("1,caf", e, e, e), 40L))
  p <- .sn_text(lines, file.path(d, "u8.csv"))
  for (n in 100:112) {
    expect_identical(sr_sniff(p, sample_bytes = n)$encoding, "UTF-8", info = n)
  }
})

test_that(".sr_open_text decompresses and re-encodes together", {
  d <- .sn_dir()
  mu <- intToUtf8(c(0xB5, 0x6D, 0x6F, 0x6C))
  raw <- iconv(paste0("id,", mu, "\n1,2\n"), "UTF-8", "latin1", toRaw = TRUE)[[1L]]
  p <- file.path(d, "l1.csv.gz")
  con <- gzfile(p, "wb"); writeBin(raw, con); close(con)
  s <- sr_sniff(p)
  expect_identical(s$compression, "gz")
  expect_identical(s$encoding, "latin1")
  con <- .sr_open_text(p, s)
  expect_identical(readLines(con), c(paste0("id,", mu), "1,2"))
  close(con)
})

test_that(".sr_open_text reads the selected zip member", {
  skip_if_not_installed("zip")
  d <- .sn_dir()
  a <- .sn_text(.sn_comma, file.path(d, "a.csv"))
  b <- .sn_text(c("x;y", "1;2"), file.path(d, "b.csv"))
  zp <- file.path(d, "two.zip")
  zip::zipr(zp, c(a, b), mode = "cherry-pick")
  s <- sr_sniff(zp, member = "b.csv")
  con <- .sr_open_text(zp, s)
  expect_identical(readLines(con), c("x;y", "1;2"))
  close(con)
})

# --- dates ------------------------------------------------------------------------------------

test_that("date order is dmy, mdy or ymd when a value decides it", {
  d <- .sn_dir()
  s <- sr_sniff(.sn_text(c("id,admit", "1,13/02/2020", "2,01/03/2020", "3,05/04/2020"),
                         file.path(d, "dmy.csv")))
  expect_identical(s$date_order, c(admit = "dmy"))
  expect_length(s$warnings, 0L)

  s <- sr_sniff(.sn_text(c("id,admit", "1,02/13/2020", "2,01/03/2020", "3,05/04/2020"),
                         file.path(d, "mdy.csv")))
  expect_identical(s$date_order, c(admit = "mdy"))

  s <- sr_sniff(.sn_text(c("id,admit,note", "1,2020-02-13,a", "2,2020-03-01,b"),
                         file.path(d, "ymd.csv")))
  expect_identical(s$date_order, c(admit = "ymd"))
})

test_that("a date column with no disambiguating value is ambiguous and warned about", {
  d <- .sn_dir()
  s <- sr_sniff(.sn_text(c("id,proc_date", "1,01/02/2020", "2,03/04/2020", "3,12/12/2021"),
                         file.path(d, "amb.csv")))
  expect_identical(s$date_order, c(proc_date = "ambiguous"))
  expect_length(s$warnings, 1L)
  expect_match(s$warnings, "proc_date")
})

test_that("a date column that is day-first and month-first at once is ambiguous and warned about", {
  d <- .sn_dir()
  s <- sr_sniff(.sn_text(c("id,visit", "1,13/02/2020", "2,02/13/2020", "3,01/02/2020"),
                         file.path(d, "mixed.csv")))
  expect_identical(s$date_order, c(visit = "ambiguous"))
  expect_length(s$warnings, 1L)
  expect_match(s$warnings, "visit")
})

test_that("non-date columns are not reported and a mostly-not-date column is skipped", {
  d <- .sn_dir()
  s <- sr_sniff(.sn_text(c("id,code,mixed", "1,AB-1,13/02/2020", "2,CD-2,hello", "3,EF-3,world"),
                         file.path(d, "nd.csv")))
  expect_length(s$date_order, 0L)
  expect_length(s$warnings, 0L)
})

# --- quoted newlines --------------------------------------------------------------------------

test_that("a quoted field with an embedded newline sets quoted_newlines", {
  d <- .sn_dir()
  lines <- c("id,notes,n", "1,\"first line", "second line\",5", "2,plain,6",
             "3,\"doubled \"\"q\"\" ok\",7")
  s <- sr_sniff(.sn_text(lines, file.path(d, "qn.csv")))
  expect_true(s$quoted_newlines)
  expect_identical(s$delim, ",")
  expect_true(s$header)
})

test_that("quoted newlines do not break the record count used for dialect detection", {
  d <- .sn_dir()
  lines <- c("a;b", "1;\"x", "y\"", "2;z")
  s <- sr_sniff(.sn_text(lines, file.path(d, "qn2.csv")))
  expect_identical(s$delim, ";")
  expect_true(s$quoted_newlines)
})

# --- format by magic bytes and extension ------------------------------------------------------

test_that("parquet and sqlite magic bytes beat a wrong .csv extension", {
  d <- .sn_dir()
  pq <- .sn_raw(c(charToRaw("PAR1"), as.raw(rep(0L, 60L)), charToRaw("PAR1")),
                file.path(d, "t.csv"))
  expect_identical(sr_sniff(pq)$format, "parquet")
  expect_identical(.sr_magic(pq), "parquet")
  sq <- .sn_raw(c(charToRaw("SQLite format 3"), as.raw(0L), as.raw(rep(0L, 100L))),
                file.path(d, "s.csv"))
  s <- sr_sniff(sq)
  expect_identical(s$format, "sqlite")
  expect_true(is.na(s$delim))
  expect_true(is.na(s$encoding))
})

test_that("the other magic-byte formats are recognised", {
  d <- .sn_dir()
  chk <- function(bytes, fmt, name = "f.bin") {
    p <- .sn_raw(c(bytes, as.raw(rep(0L, 64L))), file.path(d, name))
    expect_identical(sr_sniff(p)$format, fmt, info = fmt)
  }
  chk(charToRaw("ARROW1"), "feather")
  chk(c(as.raw(rep(0L, 8L)), charToRaw("DUCK")), "duckdb")
  chk(charToRaw("$FL2"), "sav")
  chk(charToRaw("$FL3"), "sav")
  chk(.sn_bytes(0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1), "xls")
  chk(charToRaw("<stata_dta>"), "dta")
})

test_that("an <ODM root element is recognised as odm", {
  d <- .sn_dir()
  p <- .sn_text(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                  "<ODM xmlns=\"http://www.cdisc.org/ns/odm/v1.3\" ODMVersion=\"1.3.2\">",
                  "</ODM>"), file.path(d, "study.xml"))
  s <- sr_sniff(p)
  expect_identical(s$format, "odm")
  expect_identical(s$compression, "none")
})

test_that("a zip is xlsx only when the extension says so", {
  skip_if_not_installed("zip")
  d <- .sn_dir()
  inner <- .sn_text("x", file.path(d, "a.txt"))
  zp <- file.path(d, "book.xlsx")
  zip::zipr(zp, inner, mode = "cherry-pick")
  s <- sr_sniff(zp)
  expect_identical(s$format, "xlsx")
  expect_identical(s$compression, "none")
  zm <- file.path(d, "book.xlsm")
  file.copy(zp, zm)
  expect_identical(sr_sniff(zm)$format, "xlsx")
  zz <- file.path(d, "book.zip")
  file.copy(zp, zz)
  expect_identical(sr_sniff(zz)$compression, "zip")
})

test_that("formats without magic bytes fall back to the extension", {
  d <- .sn_dir()
  for (spec in list(c("dta", "dta"), c("sas7bdat", "sas7bdat"), c("xpt", "xpt"),
                    c("por", "por"), c("xls", "xls"), c("fst", "fst"),
                    c("feather", "feather"), c("rdata", "rdata"), c("rda", "rdata"))) {
    p <- .sn_raw(as.raw(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L)),
                 file.path(d, paste0("f.", spec[1L])))
    expect_identical(sr_sniff(p)$format, spec[2L], info = spec[1L])
  }
})

test_that("a gzip-compressed rds stays rds and is not reported as compressed csv", {
  d <- .sn_dir()
  p <- file.path(d, "x.rds")
  saveRDS(data.frame(a = 1:3), p)
  expect_identical(.sr_magic(p), "gz")
  s <- sr_sniff(p)
  expect_identical(s$format, "rds")
  expect_true(is.na(s$delim))
  p2 <- file.path(d, "y.rds")
  saveRDS(data.frame(a = 1:3), p2, compress = FALSE)
  expect_identical(sr_sniff(p2)$format, "rds")
  p3 <- file.path(d, "z.rdata")
  x <- 1
  save(x, file = p3)
  expect_identical(sr_sniff(p3)$format, "rdata")
})

test_that("json and ndjson are told apart by content", {
  d <- .sn_dir()
  expect_identical(sr_sniff(.sn_text("[{\"a\":1},{\"a\":2}]", file.path(d, "a.json")))$format,
                   "json")
  expect_identical(sr_sniff(.sn_text(c("{\"a\":1}", "{\"a\":2}", "{\"a\":3}"),
                                     file.path(d, "b.json")))$format, "ndjson")
  expect_identical(sr_sniff(.sn_text(c("{", "  \"a\": 1,", "  \"b\": 2", "}"),
                                     file.path(d, "c.json")))$format, "json")
  expect_identical(sr_sniff(.sn_text(c("{\"a\":1}", "{\"a\":2}"),
                                     file.path(d, "d.ndjson")))$format, "ndjson")
  expect_identical(sr_sniff(.sn_text(c("{\"a\":1}", "{\"a\":2}"),
                                     file.path(d, "e.jsonl")))$format, "ndjson")
  s <- sr_sniff(.sn_text("[1,2]", file.path(d, "f.json")))
  expect_true(is.na(s$delim))
  expect_identical(s$encoding, "UTF-8")
})

test_that("an extension that promises a binary format the bytes do not carry is warned about", {
  d <- .sn_dir()
  s <- sr_sniff(.sn_text(.sn_comma, file.path(d, "fake.parquet")))
  expect_identical(s$format, "csv")
  expect_length(s$warnings, 1L)
  expect_match(s$warnings, "parquet")
})

# --- cheapness and edge cases -----------------------------------------------------------------

test_that("sniffing reads only a bounded head of a large file", {
  d <- .sn_dir()
  p <- file.path(d, "big.csv")
  con <- file(p, "wb")
  writeBin(charToRaw("id,v\n"), con)
  line <- charToRaw("123456,7.5\n")
  for (i in 1:2000) writeBin(rep(line, 500L), con)
  close(con)
  expect_gt(file.size(p), 10e6)
  s <- sr_sniff(p, sample_bytes = 4096L)
  expect_identical(s$delim, ",")
  expect_true(s$header)
  expect_identical(s$size_bytes, as.numeric(file.size(p)))
})

test_that("an empty file is sniffed with a warning", {
  d <- .sn_dir()
  p <- file.path(d, "empty.csv")
  file.create(p)
  s <- sr_sniff(p)
  expect_identical(s$format, "csv")
  expect_false(s$header)
  expect_true(any(grepl("empty", s$warnings)))
})

test_that("a single-column file defaults to the comma delimiter", {
  d <- .sn_dir()
  s <- sr_sniff(.sn_text(c("id", "1", "2", "3"), file.path(d, "one.csv")))
  expect_identical(s$delim, ",")
  expect_true(s$header)
})

# --- refusals ---------------------------------------------------------------------------------

test_that("URLs, missing paths and directories are refused", {
  spy <- local_fetch_spy(c("https://x/y.csv", "ftp://host/data.csv"))
  expect_error(sr_sniff("https://x/y.csv"), "URL")
  expect_error(sr_sniff("ftp://host/data.csv"), "URL")
  expect_length(spy$calls, 0L)
  d <- .sn_dir()
  expect_error(sr_sniff(file.path(d, "missing.csv")), "not found")
  expect_error(sr_sniff(d), "directory")
  expect_error(sr_sniff(NA_character_), "path")
  expect_error(sr_sniff(c("a", "b")), "path")
})

test_that("a UNC path is recognised and is not refused as a URL", {
  expect_true(.sr_is_unc("\\\\server\\share\\data.csv"))
  expect_true(.sr_is_unc("//server/share/data.csv"))
  expect_false(.sr_is_unc("C:/data/x.csv"))
  expect_false(.sr_is_unc("/tmp/x.csv"))
  expect_false(.sr_is_unc("///x.csv"))
  # Not a URL: it fails on existence, not on the URL refusal.
  msg <- tryCatch(sr_sniff("\\\\localhost\\no-such-share-xyz\\share\\data.csv"),
                  error = function(e) conditionMessage(e))
  expect_match(msg, "not found")
  expect_false(grepl("URL", msg))
})

test_that("a path on a network share is sniffed with a warning", {
  d <- .sn_dir()
  p <- .sn_text(.sn_comma, file.path(d, "u.csv"))
  expect_length(sr_sniff(p)$warnings, 0L)
  # No share is reachable in a test, so treat the local file as if it were on one.
  orig <- .sr_is_unc
  assign(".sr_is_unc", function(path) TRUE, envir = globalenv())
  withr::defer(assign(".sr_is_unc", orig, envir = globalenv()))
  s <- sr_sniff(p)
  expect_identical(s$format, "csv")
  expect_true(any(grepl("network share", s$warnings)))
})
