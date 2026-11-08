# Tests for writer/export.R: the export folder, its manifest, the doc.exported event.
# Synthetic data only (the writer fixture of helper-writer.R). The shared fixture is copied to a folder under
# tempdir() before anything is written; two exports are made once per file and read by several tests.

.ex_cache <- new.env(parent = emptyenv())

# A copy of the fixture project folder (the shared template is never written to); removed when the file ends.
.ex_copy <- function() {
  tpl <- .fx_writer_template()
  dest <- tempfile("ex-proj-")
  dir.create(dest)
  file.copy(tpl, dest, recursive = TRUE)
  withr::defer(unlink(dest, recursive = TRUE), envir = testthat::teardown_env())
  file.path(dest, basename(tpl))
}

.ex_open <- function(dir, env = parent.frame()) {
  p <- sr_project_open(dir)
  withr::defer(sr_duck_close(p), envir = env)
  withr::defer(sr_project_close(p), envir = env)
  p
}

# Two exports of one project, made once: the audit tips around them and the project folder.
.ex_two <- function() {
  if (is.null(.ex_cache$two)) {
    build <- function() {
      dir <- .ex_copy()
      p <- .ex_open(dir, environment())
      tip0 <- sr_audit_tip(p$paths$audit)
      p1 <- sr_export(p)
      tip1 <- sr_audit_tip(p$paths$audit)
      p2 <- sr_export(p1)
      list(dir = dir, tip0 = tip0, tip1 = tip1, tip2 = sr_audit_tip(p$paths$audit), exports = sr_exports(p2))
    }
    .ex_cache$two <- build()
  }
  .ex_cache$two
}

.ex_dir <- function(id) file.path(.ex_two()$dir, "out", "exports", id)

.ex_json <- function(f) .sr_parse_json(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))

# Rebind a core function (they live in the global environment) until `env` finishes.
.ex_rebind <- function(name, fn, env) {
  g <- globalenv()
  old <- get(name, envir = g)
  assign(name, fn, envir = g)
  withr::defer(assign(name, old, envir = g), envir = env)
  invisible(TRUE)
}

.ex_kinds <- c("manuscript.docx", "supplement.docx", "analysis.R", "renv.lock", "references.json", "references.ris",
               "placeholders.json", "checklist_strobe.csv", "manifest.json")

test_that("the first export writes the ten kinds of file and a manifest that matches them", {
  two <- .ex_two()
  d <- .ex_dir("export-001")
  expect_true(dir.exists(d))
  files <- list.files(d, recursive = TRUE, all.files = TRUE)
  expect_true(all(.ex_kinds %in% files))
  png <- grep("^figures/.+\\.png$", files, value = TRUE)
  expect_gt(length(png), 0L)
  expect_setequal(files, c(.ex_kinds, png))
  # no staging folder is left behind
  expect_false(any(grepl("partial", list.files(file.path(two$dir, "out", "exports"), all.files = TRUE))))

  man <- .ex_json(file.path(d, "manifest.json"))
  expect_identical(man$id, "export-001")
  expect_identical(man$audit_tip, two$tip0)
  expect_match(man$sap_hash, "^[0-9a-f]{64}$")
  expect_identical(man$preset, "default")
  expect_match(man$created, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+Z$")
  listed <- vapply(man$files, function(f) f$path, "")
  expect_setequal(listed, setdiff(files, "manifest.json"))
  for (f in man$files) {
    expect_identical(f$sha, .sr_sha256(.sr_read_bytes(file.path(d, f$path))), info = f$path)
  }
  # relative paths only
  expect_false(any(grepl("^(/|[A-Za-z]:)", listed)))
})

test_that("the export is an event: state$exports, the files with their SHA-256, and it survives a replay", {
  two <- .ex_two()
  ex <- two$exports
  expect_identical(names(ex), c("kind", "id", "n_files", "audit_tip", "sap_hash"))
  expect_identical(ex$id, c("export-001", "export-002"))
  expect_identical(ex$kind, c("manuscript", "manuscript"))
  expect_identical(ex$audit_tip, c(two$tip0, two$tip1))

  p <- .ex_open(two$dir)
  re <- sr_exports(p)
  expect_identical(re, ex)
  e1 <- p$state$exports[[1L]]
  man <- .ex_json(file.path(.ex_dir("export-001"), "manifest.json"))
  paths <- vapply(e1$files, function(f) f$path, "")
  expect_true("out/exports/export-001/manifest.json" %in% paths)
  expect_true("out/exports/export-001/manuscript.docx" %in% paths)
  expect_false(any(grepl("^(/|[A-Za-z]:)", paths)))
  for (f in e1$files) {
    expect_identical(f$sha, .sr_sha256(.sr_read_bytes(file.path(two$dir, f$path))), info = f$path)
  }
  expect_identical(e1$sap_hash, man$sap_hash)
  expect_identical(e1$preset, "default")
  expect_identical(ex$n_files[1L], length(man$files) + 1L)
  # the event chain verifies
  expect_true(sr_project_verify(p)$ok)
})

test_that("the audit tip in manuscript.docx is the tip before the export; the tip after is the doc.exported entry", {
  two <- .ex_two()
  cu <- function(docx) {
    d <- tempfile("ex-unz-")
    dir.create(d)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    zip::unzip(docx, exdir = d)
    paste(readLines(file.path(d, "docProps", "custom.xml"), warn = FALSE), collapse = "")
  }
  props <- cu(file.path(.ex_dir("export-001"), "manuscript.docx"))
  expect_match(props, two$tip0, fixed = TRUE)
  expect_match(props, "export-001", fixed = TRUE)
  expect_false(grepl(two$tip1, props, fixed = TRUE))
  props2 <- cu(file.path(.ex_dir("export-002"), "supplement.docx"))
  expect_match(props2, two$tip1, fixed = TRUE)
  # the tip after the first export is the hash of its doc.exported audit entry
  lines <- readLines(file.path(two$dir, "audit.log"), warn = FALSE, encoding = "UTF-8")
  ents <- lapply(lines, .sr_parse_json)
  hit <- Filter(function(e) identical(e$hash, two$tip1), ents)
  expect_length(hit, 1L)
  expect_identical(hit[[1L]]$action, "event:doc.exported")
  expect_identical(hit[[1L]]$prev_hash, two$tip0)
  expect_identical(ents[[length(ents)]]$hash, two$tip2)
})

test_that("a second export is a new folder; the first is untouched; placeholders and the docx digest agree", {
  two <- .ex_two()
  d1 <- .ex_dir("export-001")
  d2 <- .ex_dir("export-002")
  expect_true(dir.exists(d2))
  p <- .ex_open(two$dir)
  for (f in p$state$exports[[1L]]$files) {
    expect_identical(f$sha, .sr_sha256(.sr_read_bytes(file.path(two$dir, f$path))), info = f$path)
  }
  ph <- function(d) vapply(.ex_json(file.path(d, "placeholders.json")), function(r) r$id, "")
  expect_identical(ph(d1), ph(d2))
  for (part in c("manuscript.docx", "supplement.docx")) {
    expect_identical(sr_docx_digest(file.path(d1, part)), sr_docx_digest(file.path(d2, part)), info = part)
  }
  # the files that carry the export are what differ
  m1 <- .ex_json(file.path(d1, "manifest.json"))
  m2 <- .ex_json(file.path(d2, "manifest.json"))
  expect_identical(m2$id, "export-002")
  expect_identical(m2$audit_tip, two$tip1)
  expect_false(identical(m1$audit_tip, m2$audit_tip))
  expect_identical(.sr_sha256(.sr_read_bytes(file.path(d1, "analysis.R"))), .sr_sha256(.sr_read_bytes(file.path(d2, "analysis.R"))))
})

test_that("placeholders.json holds no value on a suppressed row, and its rows re-verify against the project", {
  two <- .ex_two()
  d <- .ex_dir("export-001")
  rows <- .ex_json(file.path(d, "placeholders.json"))
  expect_gt(length(rows), 0L)
  for (r in Filter(function(r) isTRUE(r$suppressed), rows)) expect_false("value" %in% names(r))
  live <- Filter(function(r) !isTRUE(r$suppressed), rows)
  expect_true(all(vapply(live, function(r) "value" %in% names(r), NA)))
  expect_setequal(unique(unlist(lapply(rows, names))), c("id", "kind", "text", "value", "analysis", "source", "suppressed"))

  # re-read into a registry and re-derive every row from the project
  man <- .ex_json(file.path(d, "manifest.json"))
  reg <- sr_ph_registry(k = man$suppress_k)
  num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  reg$rows <- data.frame(
    id = vapply(rows, function(r) r$id, ""), kind = vapply(rows, function(r) r$kind, ""),
    text = vapply(rows, function(r) r$text, ""), value = vapply(rows, function(r) num(r$value), 0),
    analysis = vapply(rows, function(r) chr(r$analysis), ""), source = vapply(rows, function(r) r$source, ""),
    scale = NA_character_, null = NA_real_, suppressed = vapply(rows, function(r) isTRUE(r$suppressed), NA),
    stringsAsFactors = FALSE)
  p <- .ex_open(two$dir)
  v <- sr_ph_verify(reg, p)
  expect_equal(nrow(v), length(rows))
  expect_true(all(v$ok), info = paste(v$id[!v$ok], v$got[!v$ok], collapse = "; "))
})

test_that("a suppressed placeholder is written without its value; a live one keeps it", {
  reg <- sr_ph_registry()
  txt <- sr_ph_lift("3 patients died and 120 patients lived", reg, "fragment:a_lr:demo", "a_lr")
  expect_equal(sum(reg$rows$suppressed), 1L)
  rows <- .sr_parse_json(.sr_ex_placeholders_json(reg))
  s <- Filter(function(r) isTRUE(r$suppressed), rows)
  l <- Filter(function(r) !isTRUE(r$suppressed), rows)
  expect_length(s, 1L)
  expect_identical(s[[1L]]$text, "<5")
  expect_false("value" %in% names(s[[1L]]))
  expect_false(grepl('"value":3[,}]', .sr_ex_placeholders_json(reg)))
  expect_length(l, 1L)
  expect_equal(l[[1L]]$value, 120)
  expect_identical(.sr_ex_placeholders_json(sr_ph_registry()), "[]")
})

test_that("the other files are well-formed: the CSL-JSON, RIS, lockfile, checklist and the script", {
  d <- .ex_dir("export-001")
  csl <- .ex_json(file.path(d, "references.json"))
  expect_gt(length(csl), 0L)
  expect_true(all(vapply(csl, function(it) !is.null(it$title), NA)))
  ris <- readLines(file.path(d, "references.ris"), warn = FALSE)
  expect_identical(sum(ris == "ER  - "), length(csl))
  lock <- .ex_json(file.path(d, "renv.lock"))
  expect_identical(lock$R$Version, as.character(getRversion()))
  expect_true(all(c("officer", "flextable") %in% names(lock$Packages)))
  ck <- utils::read.csv(file.path(d, "checklist_strobe.csv"), stringsAsFactors = FALSE)
  expect_identical(names(ck), c("item", "section", "label", "status", "detail"))
  expect_true(all(ck$status %in% c("green", "amber", "red")))
  expect_false(any(grepl("\r", readLines(file.path(d, "checklist_strobe.csv"), warn = FALSE), fixed = TRUE)))
  sc <- paste(readLines(file.path(d, "analysis.R"), warn = FALSE), collapse = "\n")
  expect_match(sc, "sr_results[[\"a_lr\"]]", fixed = TRUE)
  # only the reported analyses are in the script
  expect_no_error(parse(text = sc, keep.source = FALSE))
  # the PNGs are PNGs
  pngs <- list.files(file.path(d, "figures"), full.names = TRUE)
  expect_gt(length(pngs), 0L)
  expect_true(all(vapply(pngs, function(f) identical(readBin(f, "raw", 4L), as.raw(c(0x89, 0x50, 0x4e, 0x47))), NA)))
})

test_that("no file of an export holds an absolute path or the user name", {
  d <- .ex_dir("export-001")
  texts <- c("analysis.R", "renv.lock", "references.json", "references.ris", "placeholders.json",
             "checklist_strobe.csv", "manifest.json")
  bad <- c(normalizePath(tempdir(), winslash = "/"), normalizePath(tempdir(), winslash = "\\"), "C:/Users", "C:\\Users",
           Sys.info()[["user"]])
  scan <- function(txt, what) {
    for (b in bad) expect_false(grepl(b, txt, fixed = TRUE), info = paste(what, "holds", b))
  }
  for (f in texts) scan(paste(readLines(file.path(d, f), warn = FALSE), collapse = "\n"), f)
  for (docx in c("manuscript.docx", "supplement.docx")) {
    u <- tempfile("ex-unz-")
    dir.create(u)
    zip::unzip(file.path(d, docx), exdir = u)
    for (f in list.files(u, recursive = TRUE, pattern = "\\.(xml|rels)$", all.files = TRUE)) {
      scan(paste(readLines(file.path(u, f), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), paste(docx, f))
    }
    unlink(u, recursive = TRUE)
  }
})

test_that("the claim guard blocks the export: the rules are listed and no folder is left behind", {
  dir <- .ex_copy()
  p <- .ex_open(dir)
  n_before <- length(p$state$exports)
  real <- sr_doc_model
  .ex_rebind("sr_doc_model", function(project, opts = sr_writer_opts()) {
    m <- real(project, opts)
    bad <- list(type = "para", id = "injected_para", text = "We enrolled 42 patients in the trial.",
                analysis = NA_character_, section = "results")
    m$main <- c(m$main[-length(m$main)], list(bad), m$main[length(m$main)])
    m
  }, environment())
  err <- expect_error(sr_export(p), "the claim guard blocks the export:", fixed = FALSE)
  expect_match(conditionMessage(err), "digits in injected_para: ", fixed = TRUE)
  ex <- file.path(p$paths$out, "exports")
  expect_length(list.files(ex, all.files = TRUE, no.. = TRUE), 0L)
  expect_identical(length(p$state$exports), n_before)
  # nothing was appended either
  expect_false(any(vapply(sr_events_read(p$paths$events), function(e) identical(e$type, "doc.exported"), NA)))
})

test_that("a failure while writing removes the staging folder and records nothing", {
  dir <- .ex_copy()
  p <- .ex_open(dir)
  .ex_rebind("sr_analysis_script", function(project, ids) stop("script boom", call. = FALSE), environment())
  expect_error(sr_export(p), "script boom")
  expect_length(list.files(file.path(p$paths$out, "exports"), all.files = TRUE, no.. = TRUE), 0L)
  expect_false(any(vapply(sr_events_read(p$paths$events), function(e) identical(e$type, "doc.exported"), NA)))
})

test_that("an existing export folder is never overwritten: the id skips it; a stale staging folder is replaced", {
  dir <- .ex_copy()
  p <- .ex_open(dir)
  ex <- file.path(p$paths$out, "exports")
  dir.create(file.path(ex, "export-001"), recursive = TRUE)
  writeLines("keep", file.path(ex, "export-001", "note.txt"))
  p1 <- sr_export(p)
  expect_identical(readLines(file.path(ex, "export-001", "note.txt")), "keep")
  expect_identical(sr_exports(p1)$id, "export-002")
  expect_true(file.exists(file.path(ex, "export-002", "manifest.json")))
  # a stale staging folder from a crash is replaced when the folder is free
  dir.create(file.path(ex, ".export-003.partial"))
  writeLines("stale", file.path(ex, ".export-003.partial", "old.txt"))
  .ex_rebind("sr_analysis_script", function(project, ids) stop("stop here", call. = FALSE), environment())
  expect_error(sr_export(p1), "stop here")
  expect_false(dir.exists(file.path(ex, ".export-003.partial")))
})

test_that("F5: the manifest records lockfile_matches_runs; a mismatch puts a loud note in the lockfile", {
  man <- .ex_json(file.path(.ex_dir("export-001"), "manifest.json"))
  expect_true(isTRUE(man$lockfile_matches_runs))
  lock <- .ex_json(file.path(.ex_dir("export-001"), "renv.lock"))
  expect_null(lock$StatRouter$WARNING)
  dir <- .ex_copy()
  p <- .ex_open(dir)
  expect_identical(sr_lockfile_matches_runs(p, character(), sr_libsnap()), FALSE)
  .ex_rebind("sr_lockfile_matches_runs", function(project, ids, snap) FALSE, environment())
  p1 <- sr_export(p)
  d <- file.path(p$paths$out, "exports", "export-001")
  man2 <- .ex_json(file.path(d, "manifest.json"))
  expect_identical(man2$lockfile_matches_runs, FALSE)
  lock2 <- .ex_json(file.path(d, "renv.lock"))
  expect_match(lock2$StatRouter$WARNING, "NOT the library the results were computed with", fixed = TRUE)
  # the export still succeeds: provenance is recorded, not enforced
  expect_identical(sr_exports(p1)$id, "export-001")
})

test_that("a read-only project refuses, an unlocked SAP refuses, and sr_exports is empty before any export", {
  dir <- .ex_copy()
  p <- .ex_open(dir)
  ex0 <- sr_exports(p)
  expect_identical(nrow(ex0), 0L)
  expect_identical(names(ex0), c("kind", "id", "n_files", "audit_tip", "sap_hash"))
  ro <- p
  ro$readonly <- TRUE
  expect_error(sr_export(ro), "read-only")
  expect_false(dir.exists(file.path(p$paths$out, "exports", "export-001")))
  # the model refusals propagate
  un <- p
  un$state$sap$locked <- FALSE
  expect_error(sr_export(un), "not locked")
  expect_length(list.files(file.path(p$paths$out, "exports"), all.files = TRUE, no.. = TRUE), 0L)
})

test_that("the doc.exported reducer appends in order and rejects a bad kind", {
  red <- .sr_reducer_get("doc.exported")
  ev <- function(id, kind = "manuscript") {
    list(payload = list(kind = kind, id = id, files = list(list(path = "out/exports/x/a.docx", sha = "ab")),
                        audit_tip = "t", sap_hash = "s", preset = "default"))
  }
  st <- red(red(list(), ev("export-001")), ev("export-002", "sap"))
  expect_identical(vapply(st$exports, function(e) e$id, ""), c("export-001", "export-002"))
  expect_identical(st$exports[[2L]]$kind, "sap")
  expect_identical(st$exports[[1L]]$files[[1L]]$sha, "ab")
  expect_error(red(list(), ev("export-001", "nope")), "kind must be one of")
  expect_error(red(list(), list(payload = list(kind = "manuscript"))), "needs an id")
})
test_that("a CONSORT (rct) project exports without a checklist file", {
  p <- .fx_writer_project(environment(), design = "rct")
  p1 <- sr_export(p)
  ex <- sr_exports(p1)
  dir <- file.path(p$paths$dir, "out", "exports", ex$id[nrow(ex)])
  expect_true(file.exists(file.path(dir, "manuscript.docx")))
  expect_false(file.exists(file.path(dir, "checklist_strobe.csv")))
  man <- .sr_parse_json(paste(readLines(file.path(dir, "manifest.json"), warn = FALSE), collapse = "
"))
  expect_false("checklist_strobe.csv" %in% vapply(man$files, function(f) f$path, ""))
  s <- officer::docx_summary(officer::read_docx(file.path(dir, "supplement.docx")), remove_fields = TRUE)
  expect_true(any(grepl("CONSORT checklist", s$text, fixed = TRUE)))
})

