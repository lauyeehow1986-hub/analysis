# Tests for io/read_sheet.R: Excel (xlsx / xls / xlsm) and SPSS / Stata / SAS sources behind the ingest
# reader contract. Synthetic data only; everything lives under withr::local_tempdir(). Non-ASCII text is
# built with intToUtf8() (source files are ASCII).

SH_UNI <- paste0("Z", intToUtf8(c(252L, 8364L, 960L)), "x")          # u-umlaut, euro sign, pi
SH_LAB <- paste0("M", intToUtf8(c(228L, 8211L, 960L)), "nner")       # a non-ASCII value label

.sh_project <- function(env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  p
}

.sh_drive <- function(free = 1e13) function(path) list(free_bytes = free, remote = FALSE, warnings = character())

.sh_ing <- function(p, path, ..., options = list()) {
  if (is.null(options$drive_info)) options$drive_info <- .sh_drive()
  sr_ingest(p, path, ..., options = options)
}

.sh_dir <- function(p) file.path(dirname(p$paths$dir), "src")
.sh_data <- function(p) sr_project_paths(p$paths$dir)$data
.sh_ck <- function(p, table) .sr_read_json_file(file.path(.sh_data(p), "ingest", table, "checkpoint.json"))
.sh_hash <- function(p, table) p$state$tables[[table]]$store_hash
.sh_schema <- function(p, table) {
  t <- p$state$tables[[table]]$store$schema
  stats::setNames(vapply(t, function(x) x$type, ""), vapply(t, function(x) x$name, ""))
}

.sh_all <- function(p, table) {
  m <- sr_store_manifest(p, table)
  cols <- vapply(m$schema, function(s) s$name, "")
  offs <- seq(0, max(m$rows - 1, 0), by = 1000)
  do.call(rbind, lapply(offs, function(o) sr_table_page(p, table, o, 1000, order_by = cols)))
}

# Write an xlsx from a character matrix of cells (no header handling: every row is a data row).
.sh_xlsx <- function(cells, path, sheets = NULL) {
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  x <- sheets
  if (is.null(x)) {
    x <- as.data.frame(cells, stringsAsFactors = FALSE)
    names(x) <- paste0("V", seq_len(ncol(x)))
  }
  writexl::write_xlsx(x, path, col_names = FALSE)
  invisible(path)
}

# Put `value` in the global environment under `name` (NULL removes it) until `env` ends, then restore what
# was there before. The real dictionary functions live in the global environment too, so a stub must
# never leave them removed for the test files that run after this one.
.sh_swap <- function(name, value, env = parent.frame()) {
  had <- exists(name, envir = globalenv(), inherits = FALSE)
  orig <- if (had) get(name, envir = globalenv(), inherits = FALSE) else NULL
  if (is.null(value)) {
    suppressWarnings(rm(list = name, envir = globalenv()))
  } else {
    assign(name, value, envir = globalenv())
  }
  withr::defer({
    if (had) assign(name, orig, envir = globalenv()) else suppressWarnings(rm(list = name, envir = globalenv()))
  }, envir = env)
  invisible(NULL)
}

# A stub for the dictionary hook; records what sr_ingest passes it (the raw label list, not the converted
# dictionary). The previous sr_dict_set and sr_dict_from_labels return when `env` ends.
.sh_stub_dict <- function(env = parent.frame()) {
  rec <- new.env()
  rec$n <- 0L
  # the raw label list reaches the stub as before: these tests read its per-column entries
  .sh_swap("sr_dict_from_labels", function(dict_labels) dict_labels, env = env)
  .sh_swap("sr_dict_set", function(project, table, dict, source = NULL) {
    rec$n <- rec$n + 1L
    rec$table <- table
    rec$dict <- dict
    rec$source <- source
    project
  }, env = env)
  rec
}

# 200 rows: id, a labelled sex (code 3 has no label), an SPSS user-missing score (-9), age, text, Date.
.sh_frame <- function(n = 200L, uni = FALSE) {
  sex <- rep(c(1, 2), length.out = n)
  sex[7] <- 3
  sex[11] <- NA
  score <- seq_len(n) * 0.5
  score[c(4, 150)] <- -9
  lab <- c(Male = 1, Female = 2)
  if (uni) lab <- c(stats::setNames(1, SH_LAB), Female = 2)
  d <- data.frame(id = seq_len(n))
  d$sex <- haven::labelled(sex, lab, label = "Sex of patient")
  d$score <- haven::labelled_spss(score, na_values = -9, label = "Score")
  d$age <- seq_len(n) + 20
  d$nm <- paste0("row", seq_len(n))
  if (uni) d$nm[5] <- SH_UNI
  d$visit <- as.Date("2020-01-01") + seq_len(n)
  d
}

# ---- registration ---------------------------------------------------------------------------------

test_that("every Excel and statistical-package format has a reader", {
  for (f in c("xlsx", "xls", "xlsm", "sav", "por", "dta", "sas7bdat", "xpt")) {
    expect_true(is.function(.sr_reader_get(f)), info = f)
  }
  expect_equal(.sr_xl_max_rows, 1048576)
})

# ---- Excel ----------------------------------------------------------------------------------------

test_that("Excel: a two-row header with a merged span flattens to bp_systolic / bp_diastolic", {
  p <- .sh_project()
  f <- .sh_xlsx(rbind(c("id", "bp", "", "age"), c("", "systolic", "diastolic", ""),
                      c("1", "120", "80", "40"), c("2", "130", "85", "50"), c("3", "140", "90", "60")),
                file.path(.sh_dir(p), "bp.xlsx"))
  expect_equal(sr_sniff(f)$format, "xlsx")
  p <- .sh_ing(p, f, table = "bp", options = list(header_rows = 2L))
  s <- .sh_schema(p, "bp")
  expect_equal(names(s), c("id", "bp_systolic", "bp_diastolic", "age"))
  expect_true(all(s %in% c("BIGINT", "INTEGER", "DOUBLE")))
  expect_false(p$state$tables$bp$resumable)
  expect_length(p$state$tables$bp$store$parts, 1L)
  d <- .sh_all(p, "bp")
  expect_equal(nrow(d), 3L)
  expect_equal(as.numeric(d$bp_systolic), c(120, 130, 140))
  expect_false(p$state$tables$bp$dict_from_source)
})

test_that("Excel: duplicate header names are made unique; an empty header becomes column_<j>", {
  p <- .sh_project()
  f <- .sh_xlsx(rbind(c("a", "a", ""), c("1", "2", "3")), file.path(.sh_dir(p), "dup.xlsx"))
  p <- .sh_ing(p, f, table = "dup")
  expect_equal(names(.sh_schema(p, "dup")), c("a", "a_1", "column_3"))
})

test_that("Excel: options$sheet picks a sheet and sr_excel_sheets lists them", {
  p <- .sh_project()
  f <- file.path(.sh_dir(p), "two.xlsx")
  .sh_xlsx(NULL, f, sheets = list(first = data.frame(a = c("x", "1")), second = data.frame(b = c("k", "9"), c = c("m", "8"))))
  expect_equal(sr_excel_sheets(f), c("first", "second"))
  p <- .sh_ing(p, f, table = "s2", options = list(sheet = "second"))
  expect_equal(names(.sh_schema(p, "s2")), c("k", "m"))
  p <- .sh_ing(p, f, table = "s1")
  expect_equal(names(.sh_schema(p, "s1")), "x")
  p <- .sh_ing(p, f, table = "s2n", options = list(sheet = 2))
  expect_equal(names(.sh_schema(p, "s2n")), c("k", "m"))
  expect_error(.sh_ing(p, f, table = "s3", options = list(sheet = "nope")), "nope")
  expect_error(sr_excel_sheets("https://example.org/x.xlsx"), "URL|url")
})

test_that("Excel: the sheet argument of sr_ingest selects the sheet and a different sheet does not resume", {
  p <- .sh_project()
  f <- file.path(.sh_dir(p), "two.xlsx")
  .sh_xlsx(NULL, f, sheets = list(first = data.frame(a = c("x", "1")), second = data.frame(b = c("k", "9"))))
  p <- .sh_ing(p, f, table = "t", sheet = "second")
  expect_equal(names(.sh_schema(p, "t")), "k")
  expect_equal(p$state$tables$t$store$rows, 1)
})

test_that("Excel: skip drops title rows before the header", {
  p <- .sh_project()
  f <- .sh_xlsx(rbind(c("Study title", ""), c("exported 2020", ""), c("id", "val"), c("1", "5"), c("2", "6")),
                file.path(.sh_dir(p), "titles.xlsx"))
  p <- .sh_ing(p, f, table = "ti", options = list(skip = 2L))
  expect_equal(names(.sh_schema(p, "ti")), c("id", "val"))
  expect_equal(p$state$tables$ti$store$rows, 2)
  p <- .sh_ing(p, f, table = "ti2", options = list(range = "A3:B5"))
  expect_equal(names(.sh_schema(p, "ti2")), c("id", "val"))
  expect_equal(p$state$tables$ti2$store$rows, 2)
})

test_that("Excel: identifier-like text is never turned into a number", {
  p <- .sh_project()
  big <- "12345678901234567"                                  # 17 digits: does not survive a double
  f <- .sh_xlsx(cbind(c("lead", "007", "12"), c("big", big, "1"), c("zero", "0", "0.5"),
                      c("int", "1", "2"), c("tf", "T", "F"), c("na", "NA", "x"), c("hex", "0x1F", "3")),
                file.path(.sh_dir(p), "ids.xlsx"))
  # the header sits in row 1 of each column
  p <- .sh_ing(p, f, table = "ids")
  s <- .sh_schema(p, "ids")
  expect_equal(s[["lead"]], "VARCHAR")
  expect_equal(s[["big"]], "VARCHAR")
  expect_equal(s[["zero"]], "DOUBLE")
  expect_true(s[["int"]] %in% c("INTEGER", "BIGINT"))
  expect_equal(s[["tf"]], "VARCHAR")
  expect_equal(s[["na"]], "VARCHAR")
  expect_equal(s[["hex"]], "VARCHAR")
  d <- .sh_all(p, "ids")
  expect_true("007" %in% d$lead)
  expect_true(big %in% d$big)
  expect_true(is.na(d$na[1]) && d$na[2] == "x")                 # a text "NA" is null (the CSV default), the column stays text
  expect_true(all(c("T", "F") %in% d$tf))
})

test_that("Excel: a Date and a date-time column written by Excel come back as DATE and TIMESTAMP", {
  p <- .sh_project()
  f <- file.path(.sh_dir(p), "dates.xlsx")
  dir.create(dirname(f), recursive = TRUE)
  d0 <- data.frame(id = 1:4, d = as.Date("2020-01-01") + c(0, 31, NA, 400),
                   ts = as.POSIXct("2021-03-04 10:30:00", tz = "UTC") + c(0, 3600, 60, NA),
                   n = c(43831, 2, 3, 4))
  writexl::write_xlsx(d0, f)
  p <- .sh_ing(p, f, table = "dt")
  s <- .sh_schema(p, "dt")
  expect_equal(s[["d"]], "DATE")
  expect_match(s[["ts"]], "^TIMESTAMP")
  expect_true(s[["n"]] %in% c("INTEGER", "BIGINT"))              # a plain number that looks like a serial is not a date
  d <- .sh_all(p, "dt")
  expect_equal(as.character(d$d[order(d$id)]), c("2020-01-01", "2020-02-01", NA, "2021-02-04"))
  tsv <- format(d$ts[order(d$id)], "%Y-%m-%d %H:%M:%S", tz = "UTC")
  expect_equal(tsv, c("2021-03-04 10:30:00", "2021-03-04 11:30:00", "2021-03-04 10:31:00", NA))
})

test_that("Excel: more rows than the cap is refused before any part, naming the cap and CSV", {
  p <- .sh_project()
  f <- .sh_xlsx(cbind(c("v", as.character(1:150))), file.path(.sh_dir(p), "big.xlsx"))
  err <- tryCatch(.sh_ing(p, f, table = "big", options = list(max_rows = 100)), error = function(e) conditionMessage(e))
  expect_match(err, "more than 100 rows")
  expect_match(err, "CSV")
  expect_false(dir.exists(sr_store_dir(p, "big")))
  expect_null(p$state$tables$big)
  expect_length(list.files(file.path(.sh_data(p), "ingest", "big", "store"), pattern = "part-"), 0L)
  # an existing table survives a refused re-ingest
  ok <- .sh_xlsx(cbind(c("v", as.character(1:5))), file.path(.sh_dir(p), "ok.xlsx"))
  p <- .sh_ing(p, ok, table = "keep")
  h <- .sh_hash(p, "keep")
  expect_error(.sh_ing(p, ok, table = "keep", options = list(max_rows = 3)), "more than 3 rows")
  expect_identical(.sh_hash(p, "keep"), h)
  expect_equal(sr_store_manifest(p, "keep")$rows, 5)
  # exactly at the cap is fine; the default cap is Excel's own
  p <- .sh_ing(p, ok, table = "edge", options = list(max_rows = 6))
  expect_equal(p$state$tables$edge$store$rows, 5)
})

test_that("Excel: an empty sheet, too few header rows and bad options are refused", {
  p <- .sh_project()
  f <- .sh_xlsx(rbind(c("a", "b"), c("1", "2")), file.path(.sh_dir(p), "few.xlsx"))
  expect_error(.sh_ing(p, f, table = "few0", options = list(header_rows = 3L)), "header_rows")
  expect_error(.sh_ing(p, f, table = "few1", options = list(header_rows = 0L)), "header_rows")
  expect_error(.sh_ing(p, f, table = "few2", options = list(skip = -1)), "skip")
  expect_error(.sh_ing(p, f, table = "few3", options = list(range = c("A1", "B2"))), "range")
})

test_that("Excel: the changed sheet or range settings invalidate a checkpoint instead of resuming it", {
  p <- .sh_project()
  f <- file.path(.sh_dir(p), "two.xlsx")
  .sh_xlsx(NULL, f, sheets = list(first = data.frame(a = c("x", "1")), second = data.frame(b = c("k", "9"))))
  h1 <- .sr_ingest_sniff_hash(sr_sniff(f), list(sheet = "first"))
  h2 <- .sr_ingest_sniff_hash(sr_sniff(f), list(sheet = "second"))
  h3 <- .sr_ingest_sniff_hash(sr_sniff(f), list(sheet = "second", header_rows = 2L))
  h4 <- .sr_ingest_sniff_hash(sr_sniff(f), list(sheet = "second", range = "A1:A2"))
  h5 <- .sr_ingest_sniff_hash(sr_sniff(f), list(sheet = "second", skip = 1L))
  expect_equal(length(unique(c(h1, h2, h3, h4, h5))), 5L)
  # and a text-format hash is unchanged by the new keys when they are absent
  expect_identical(.sr_ingest_sniff_hash(sr_sniff(f), list()), .sr_ingest_sniff_hash(sr_sniff(f), list(sample_size = 1e6)))
})

# ---- SPSS / Stata / SAS ---------------------------------------------------------------------------

test_that("SAV: value labels become text, an unlabelled code keeps its value, other columns stay numeric", {
  p <- .sh_project()
  f <- file.path(.sh_dir(p), "fx.sav")
  dir.create(dirname(f))
  haven::write_sav(.sh_frame(), f)
  expect_equal(sr_sniff(f)$format, "sav")
  p <- .sh_ing(p, f, table = "fx")
  s <- .sh_schema(p, "fx")
  expect_equal(s[["sex"]], "VARCHAR")
  expect_true(s[["age"]] %in% c("DOUBLE", "BIGINT", "INTEGER"))
  expect_equal(s[["score"]], "DOUBLE")
  expect_equal(s[["visit"]], "DATE")
  d <- .sh_all(p, "fx")
  d <- d[order(d$id), ]
  expect_equal(d$sex[1:3], c("Male", "Female", "Male"))
  expect_equal(d$sex[7], "3")                                    # no label for code 3: the value survives
  expect_true(is.na(d$sex[11]))
  expect_equal(sum(is.na(d$sex)), 1L)
  expect_equal(sum(d$sex == "3", na.rm = TRUE), 1L)
  expect_equal(nrow(d), 200L)
  expect_equal(d$age[1], 21)
})

test_that("SAV: user-missing -9 becomes NA and is listed in missing_codes; labels reach the dictionary hook", {
  p <- .sh_project()
  f <- file.path(.sh_dir(p), "fx.sav")
  dir.create(dirname(f))
  haven::write_sav(.sh_frame(), f)
  rec <- .sh_stub_dict()
  p <- .sh_ing(p, f, table = "fx")
  d <- .sh_all(p, "fx")
  d <- d[order(d$id), ]
  expect_true(is.na(d$score[4]) && is.na(d$score[150]))
  expect_equal(sum(is.na(d$score)), 2L)
  expect_equal(d$score[1], 0.5)
  expect_true(p$state$tables$fx$dict_from_source)
  expect_equal(rec$n, 1L)
  expect_equal(rec$table, "fx")
  dl <- rec$dict
  expect_equal(dl$sex$label, "Sex of patient")
  expect_equal(dl$score$label, "Score")
  expect_equal(dl$score$missing_codes, "-9")
  expect_equal(dl$sex$values$code, c("1", "2"))
  expect_equal(dl$sex$values$label, c("Male", "Female"))
  expect_equal(dl$sex$values$order, 1:2)
  expect_null(dl$age)                                           # a column with no labels has no entry
  expect_false("id" %in% names(dl))
})

test_that("the dictionary hook is optional: without sr_dict_set the labels are dropped without an error", {
  p <- .sh_project()
  f <- file.path(.sh_dir(p), "fx.sav")
  dir.create(dirname(f))
  haven::write_sav(.sh_frame(), f)
  .sh_swap("sr_dict_set", NULL)                                  # as before the dictionary module is loaded
  expect_false(exists("sr_dict_set", envir = globalenv(), inherits = FALSE))
  p <- .sh_ing(p, f, table = "fx")
  expect_true(p$state$tables$fx$dict_from_source)
  # a file with no labels at all is not "from source"
  g <- file.path(.sh_dir(p), "plain.sav")
  haven::write_sav(data.frame(a = 1:3, b = c("x", "y", "z")), g)
  rec <- .sh_stub_dict()
  p <- .sh_ing(p, g, table = "plain")
  expect_false(p$state$tables$plain$dict_from_source)
  expect_equal(rec$n, 0L)
})

test_that("the dict.set hook goes through sr_dict_from_labels when Task 9 provides it", {
  p <- .sh_project()
  f <- file.path(.sh_dir(p), "fx.sav")
  dir.create(dirname(f))
  haven::write_sav(.sh_frame(), f)
  rec <- .sh_stub_dict()
  .sh_swap("sr_dict_from_labels", function(dict_labels) list(wrapped = dict_labels))
  p <- .sh_ing(p, f, table = "fx")
  expect_equal(rec$n, 1L)
  expect_true(is.list(rec$dict$wrapped))
  expect_equal(rec$dict$wrapped$score$missing_codes, "-9")
})

test_that("ingesting a labelled file keeps what an existing state entry carries (merge, not replace)", {
  p <- .sh_project()
  f <- file.path(.sh_dir(p), "fx.sav")
  dir.create(dirname(f))
  haven::write_sav(.sh_frame(), f)
  p <- .sh_ing(p, f, table = "fx")
  p$state$tables$fx$active <- "keepme"
  ev <- sr_project_append(p, "data.imported", list(table = "fx", format = "sav", dict_from_source = FALSE))
  expect_equal(ev$state$tables$fx$active, "keepme")
  expect_false(ev$state$tables$fx$dict_from_source)
})

test_that("SAV: chunk_rows = 50 on 200 rows gives 4 parts and the same rows as one chunk", {
  p <- .sh_project()
  f <- file.path(.sh_dir(p), "fx.sav")
  dir.create(dirname(f))
  haven::write_sav(.sh_frame(), f)
  one <- .sh_ing(p, f, table = "one")
  four <- .sh_ing(one, f, table = "four", options = list(chunk_rows = 50))
  expect_length(four$state$tables$four$store$parts, 4L)
  expect_true(four$state$tables$four$resumable)
  expect_length(four$state$tables$one$store$parts, 1L)
  expect_false(four$state$tables$one$resumable)
  expect_equal(four$state$tables$four$store$rows, 200)
  expect_equal(.sh_schema(four, "four"), .sh_schema(four, "one"))
  expect_identical(.sh_all(four, "four"), .sh_all(four, "one"))
})

test_that("SAV: a cancelled chunked ingest resumes by row offset and matches a clean run", {
  p <- .sh_project()
  f <- file.path(.sh_dir(p), "fx.sav")
  dir.create(dirname(f))
  haven::write_sav(.sh_frame(), f)
  clean <- .sh_ing(p, f, table = "clean", options = list(chunk_rows = 60))
  expect_length(clean$state$tables$clean$store$parts, 4L)
  calls <- 0L
  cancel <- function() { calls <<- calls + 1L; calls >= 2L }
  expect_error(.sh_ing(p, f, table = "r", options = list(chunk_rows = 60), cancel = cancel),
               class = "sr_ingest_cancelled")
  ck <- .sh_ck(p, "r")
  expect_length(ck$parts, 2L)
  expect_equal(ck$next_offset, 120)
  stage <- file.path(.sh_data(p), "ingest", "r", "store")
  parts <- file.path(stage, vapply(ck$parts, function(x) x$file, ""))
  sha_before <- unname(vapply(parts, sr_sha256_file, ""))
  seen <- numeric()
  done <- .sh_ing(p, f, table = "r", options = list(chunk_rows = 60), progress = function(d, total) seen <<- c(seen, d))
  expect_equal(seen, c(180, 200))
  final <- sr_store_dir(done, "r")
  expect_identical(unname(vapply(file.path(final, basename(parts)), sr_sha256_file, "")), sha_before)
  expect_identical(.sh_hash(done, "r"), .sh_hash(clean, "clean"))
  expect_equal(done$state$tables$r$store$rows, 200)
})

test_that("SAV: a labelled column that is all NA in the first chunk is still text in every part", {
  p <- .sh_project()
  d <- .sh_frame()
  sex <- as.numeric(haven::zap_labels(d$sex))
  sex[1:60] <- NA
  d$sex <- haven::labelled(sex, c(Male = 1, Female = 2), label = "Sex of patient")
  f <- file.path(.sh_dir(p), "na.sav")
  dir.create(dirname(f))
  haven::write_sav(d, f)
  p <- .sh_ing(p, f, table = "na", options = list(chunk_rows = 50))   # no "parts disagree" error
  expect_length(p$state$tables$na$store$parts, 4L)
  expect_equal(.sh_schema(p, "na")[["sex"]], "VARCHAR")
  r <- .sh_all(p, "na")
  r <- r[order(r$id), ]
  expect_equal(sum(is.na(r$sex)), 60L)                          # rows 1-60 (the original NA at row 11 is among them)
  expect_equal(r$sex[61], "Male")
  expect_equal(r$sex[100], "Female")
})

test_that("SAV: a whole-number unlabelled code reads as digits, not scientific notation", {
  p <- .sh_project()
  d <- data.frame(id = 1:3)
  d$g <- haven::labelled(c(1, 100000, 2), c(One = 1))
  f <- file.path(.sh_dir(p), "big.sav")
  dir.create(dirname(f))
  haven::write_sav(d, f)
  p <- .sh_ing(p, f, table = "g")
  r <- .sh_all(p, "g")
  expect_true(all(c("One", "100000", "2") %in% r$g))
})

test_that("SAV and DTA: a non-ASCII value and a non-ASCII value label round-trip", {
  p <- .sh_project()
  d <- .sh_frame(uni = TRUE)
  dir.create(.sh_dir(p))
  for (fmt in c("sav", "dta")) {
    f <- file.path(.sh_dir(p), paste0("u.", fmt))
    if (fmt == "sav") haven::write_sav(d, f) else haven::write_dta(d[, c("id", "sex", "age", "nm", "visit")], f)
    tb <- paste0("u_", fmt)
    rec <- .sh_stub_dict()
    p <- .sh_ing(p, f, table = tb)
    r <- .sh_all(p, tb)
    r <- r[order(r$id), ]
    expect_equal(r$nm[5], SH_UNI, info = fmt)
    expect_equal(r$sex[1], SH_LAB, info = fmt)                  # the label, not the code
    expect_equal(rec$dict$sex$values$label[1], SH_LAB, info = fmt)
    expect_true(all(validUTF8(r$nm)) && all(validUTF8(r$sex[!is.na(r$sex)])), info = fmt)
  }
})

test_that("DTA: value labels, a Date column and variable labels", {
  p <- .sh_project()
  d <- .sh_frame()[, c("id", "sex", "age", "nm", "visit")]
  f <- file.path(.sh_dir(p), "fx.dta")
  dir.create(dirname(f))
  haven::write_dta(d, f)
  expect_equal(sr_sniff(f)$format, "dta")
  rec <- .sh_stub_dict()
  p <- .sh_ing(p, f, table = "st", options = list(chunk_rows = 50))
  expect_length(p$state$tables$st$store$parts, 4L)
  r <- .sh_all(p, "st")
  r <- r[order(r$id), ]
  expect_equal(r$sex[1:2], c("Male", "Female"))
  expect_equal(r$sex[7], "3")
  expect_equal(.sh_schema(p, "st")[["visit"]], "DATE")
  expect_equal(as.character(r$visit[1]), "2020-01-02")
  expect_equal(rec$dict$sex$label, "Sex of patient")
})

test_that("XPT: a transport file with variable labels reads, and a chunk boundary on an exact multiple ends cleanly", {
  p <- .sh_project()
  d <- data.frame(id = 1:100, v = seq_len(100) / 4, s = paste0("r", 1:100), stringsAsFactors = FALSE)
  attr(d$v, "label") <- "A measure"
  f <- file.path(.sh_dir(p), "fx.xpt")
  dir.create(dirname(f))
  haven::write_xpt(d, f)
  expect_equal(sr_sniff(f)$format, "xpt")
  rec <- .sh_stub_dict()
  p <- .sh_ing(p, f, table = "x", options = list(chunk_rows = 50))    # 100 = 2 x 50 exactly
  expect_length(p$state$tables$x$store$parts, 2L)
  expect_equal(p$state$tables$x$store$rows, 100)
  expect_equal(rec$dict$v$label, "A measure")
  r <- .sh_all(p, "x")
  expect_equal(r$v[order(r$id)][4], 1)
})

test_that("a file with no rows still gets a (typed) empty part", {
  p <- .sh_project()
  d <- data.frame(id = integer(), s = character(), stringsAsFactors = FALSE)
  f <- file.path(.sh_dir(p), "empty.sav")
  dir.create(dirname(f))
  haven::write_sav(d, f)
  p <- .sh_ing(p, f, table = "e")
  expect_equal(p$state$tables$e$store$rows, 0)
  expect_equal(names(.sh_schema(p, "e")), c("id", "s"))
})

test_that("SAS: a sas7bdat fixture reads when one is supplied (haven cannot write one)", {
  fx <- Sys.glob(file.path(testthat::test_path("fixtures", "data"), "*.sas7bdat"))
  skip_if_not(length(fx) > 0L, "no sas7bdat fixture is shipped (no third-party data is committed)")
  p <- .sh_project()
  p <- .sh_ing(p, fx[1L], table = "sas")
  expect_gt(p$state$tables$sas$store$rows, 0)
})

test_that("bad options are refused", {
  p <- .sh_project()
  f <- file.path(.sh_dir(p), "fx.sav")
  dir.create(dirname(f))
  haven::write_sav(.sh_frame(), f)
  expect_error(.sh_ing(p, f, table = "b1", options = list(chunk_rows = 0)), "chunk_rows")
  expect_error(.sh_ing(p, f, table = "b2", options = list(encoding = 5)), "encoding")
})

# ---- fix round 1 ----------------------------------------------------------------------------------

# An xlsx whose table starts at row `start` (blank rows above it), written with openxlsx.
.sh_xlsx_at <- function(df, path, start) {
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "S")
  openxlsx::writeData(wb, "S", df, startRow = start)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

test_that("Excel: a Date column is still DATE when the sheet has leading blank rows (skip = 0 and skip > 0)", {
  p <- .sh_project()
  d0 <- data.frame(id = 1:3, d = as.Date("2020-01-01") + 0:2, s = c("a", "b", NA), stringsAsFactors = FALSE)
  f3 <- .sh_xlsx_at(d0, file.path(.sh_dir(p), "r3.xlsx"), 3)
  p <- .sh_ing(p, f3, table = "r3")
  expect_equal(.sh_schema(p, "r3")[["d"]], "DATE")
  expect_equal(as.character(.sh_all(p, "r3")$d[1:3]), c("2020-01-01", "2020-01-02", "2020-01-03"))
  f4 <- .sh_xlsx_at(d0, file.path(.sh_dir(p), "r4.xlsx"), 4)
  p <- .sh_ing(p, f4, table = "r4", options = list(skip = 1L))
  expect_equal(.sh_schema(p, "r4")[["d"]], "DATE")
  p <- .sh_ing(p, f4, table = "r4b", options = list(skip = 2L))
  expect_equal(.sh_schema(p, "r4b")[["d"]], "DATE")
  # title rows and a blank row above a two-row header
  p <- .sh_ing(p, f3, table = "r3h", options = list(header_rows = 1L, skip = 1L))
  expect_equal(.sh_schema(p, "r3h")[["d"]], "DATE")
  # an explicit range that starts below blank rows
  p <- .sh_ing(p, f3, table = "r3g", options = list(range = "A3:C6"))
  expect_equal(.sh_schema(p, "r3g")[["d"]], "DATE")
})

test_that("Excel: a text cell among dates keeps the column as text", {
  p <- .sh_project()
  f <- .sh_xlsx_at(data.frame(d = c("x", "y")), file.path(.sh_dir(p), "txt.xlsx"), 2)
  p <- .sh_ing(p, f, table = "txt")
  expect_equal(.sh_schema(p, "txt")[["d"]], "VARCHAR")
})

test_that("Excel: a text NA cell is null like the CSV default, and options$nullstr overrides it", {
  p <- .sh_project()
  f <- .sh_xlsx(cbind(c("v", "1", "2", "NA", ""), c("w", "a", "NA", "b", "c")), file.path(.sh_dir(p), "na.xlsx"))
  p <- .sh_ing(p, f, table = "na")
  s <- .sh_schema(p, "na")
  expect_true(s[["v"]] %in% c("INTEGER", "BIGINT"))
  d <- .sh_all(p, "na")
  expect_equal(sum(is.na(d$v)), 2L)
  expect_equal(sum(is.na(d$w)), 1L)
  p <- .sh_ing(p, f, table = "na2", options = list(nullstr = ""))
  expect_equal(.sh_schema(p, "na2")[["v"]], "VARCHAR")
  expect_true("NA" %in% .sh_all(p, "na2")$v)
  expect_error(.sh_ing(p, f, table = "na3", options = list(nullstr = 5)), "nullstr")
})

test_that("Excel: headers that differ only by case are made distinct", {
  p <- .sh_project()
  f <- .sh_xlsx(rbind(c("ID", "id", "Id", "x"), c("1", "2", "3", "4")), file.path(.sh_dir(p), "case.xlsx"))
  p <- .sh_ing(p, f, table = "cs")
  nm <- names(.sh_schema(p, "cs"))
  expect_equal(nm, c("ID", "id_1", "Id_2", "x"))
  expect_equal(anyDuplicated(tolower(nm)), 0L)
})

test_that("SAV: a value-label code of 2^31 or more neither warns nor becomes NA", {
  p <- .sh_project()
  d <- data.frame(id = 1:3)
  d$g <- haven::labelled(c(1, 3000000000, 2), c(One = 1, Huge = 3000000000))
  d$h <- haven::labelled(c(1, 1, 2), c(One = 1, Huge = 3000000000))
  f <- file.path(.sh_dir(p), "huge.sav")
  dir.create(dirname(f), showWarnings = FALSE)
  haven::write_sav(d, f)
  rec <- .sh_stub_dict()
  expect_no_warning(p <- .sh_ing(p, f, table = "hg"))
  r <- .sh_all(p, "hg")
  r <- r[order(r$id), ]
  expect_equal(r$g, c("One", "Huge", "2"))
  expect_equal(r$h, c("One", "One", "2"))
  expect_equal(rec$dict$g$values$code, c("1", "3000000000"))
})
