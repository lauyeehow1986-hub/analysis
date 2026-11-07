# Tests for writer/sapdoc.R: SAP_v1.docx (and the draft) and the feasibility-report docx.
# Synthetic data only (the writer fixture of helper-writer.R); every project is a copy made under tempdir().

.sd_cache <- new.env(parent = emptyenv())

# A copy of the fixture project folder (the shared template is never written to); removed when the file ends.
.sd_copy <- function() {
  tpl <- .fx_writer_template()
  dest <- tempfile("sd-proj-")
  dir.create(dest)
  file.copy(tpl, dest, recursive = TRUE)
  withr::defer(unlink(dest, recursive = TRUE), envir = testthat::teardown_env())
  file.path(dest, basename(tpl))
}

.sd_open <- function(dir, env = parent.frame()) {
  p <- sr_project_open(dir)
  withr::defer(sr_duck_close(p), envir = env)
  withr::defer(sr_project_close(p), envir = env)
  p
}

# All the text of a .docx: body paragraphs and table cells (docx_summary), plus the footer XML as one string.
.sd_text <- function(path) {
  s <- officer::docx_summary(officer::read_docx(path), remove_fields = TRUE)
  s$text[!is.na(s$text)]
}

.sd_footer <- function(path) {
  d <- tempfile("sd-unz-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  zip::unzip(path, exdir = d)
  ft <- list.files(file.path(d, "word"), pattern = "^footer[0-9]*[.]xml$", full.names = TRUE)
  paste(unlist(lapply(ft, readLines, warn = FALSE)), collapse = "")
}

# The locked fixture, exported once: the project, its SAP docx and the second call's error.
.sd_locked <- function() {
  if (is.null(.sd_cache$locked)) {
    build <- function() {
      dir <- .sd_copy()
      p <- .sd_open(dir, environment())
      p1 <- sr_sap_docx(p)
      err <- tryCatch({ sr_sap_docx(p1); NA_character_ }, error = function(e) conditionMessage(e))
      list(dir = dir, project = p1, err = err, path = file.path(p$paths$out, "sap", "SAP_v1.docx"),
           hash = p$state$sap$lock$sap_hash, ids = sort(names(.sr_sap_analyses(p))))
    }
    .sd_cache$locked <- build()
  }
  .sd_cache$locked
}


test_that("the locked SAP is written to out/sap/SAP_v1.docx with the hash, every analysis id and holm", {
  L <- .sd_locked()
  expect_true(file.exists(L$path))
  txt <- paste(.sd_text(L$path), collapse = "\n")
  expect_true(grepl(L$hash, txt, fixed = TRUE))
  expect_match(L$hash, "^[0-9a-f]{64}$")
  for (id in L$ids) expect_true(grepl(id, txt, fixed = TRUE), info = id)
  expect_true(grepl("holm", txt, fixed = TRUE))
  expect_true(grepl("Statistical analysis plan v1 (locked)", txt, fixed = TRUE))
  expect_false(grepl("DRAFT", .sd_footer(L$path), fixed = TRUE))
  dp <- officer::doc_properties(officer::read_docx(L$path))
  expect_identical(dp$value[dp$tag == "creator"], "StatRouter")
})

test_that("a second call for a locked SAP refuses, and the exports state holds one sap entry", {
  L <- .sd_locked()
  expect_match(L$err, "the locked SAP was already exported", fixed = TRUE)
  ex <- sr_exports(L$project)
  expect_identical(ex$kind, "sap")
  expect_identical(ex$id, "SAP_v1")
  expect_identical(ex$sap_hash, L$hash)
  rec <- L$project$state$exports[[1L]]
  expect_identical(rec$files[[1L]]$path, "out/sap/SAP_v1.docx")
  expect_identical(rec$files[[1L]]$sha, .sr_sha256(.sr_read_bytes(L$path)))
})

test_that("the draft SAP of an unlocked project is SAP_draft.docx with a DRAFT footer and no result number", {
  # the fixture's own result numbers: the draft must hold none of them
  pf <- .fx_writer_project(environment())
  res <- sr_result(pf, "a_lr")$effects
  leak <- unique(c(sr_fmt_num(res$est), sr_fmt_p(res$p), sr_fmt_num(res$lo), sr_fmt_num(res$hi)))
  leak <- leak[nchar(leak) >= 4L]
  expect_gt(length(leak), 0L)

  df <- .fx_writer_frame()
  p <- .fx_ready_project(df, "t", c(id = "id", y = "outcome"), env = environment())
  spec <- list(purpose = "explain-causal", outcome = "y", exposure = "grp", exposure_ref = "A",
               terms = list(list(var = "age"), list(var = "z1")), marginal = TRUE, layers = .fx_writer_layers(),
               seed = 42L)
  p <- sr_sap_add(p, "a_lr", "logistic", spec, "t", label = "primary")
  expect_false(isTRUE(p$state$sap$locked))
  p <- sr_sap_docx(p)
  path <- file.path(p$paths$out, "sap", "SAP_draft.docx")
  expect_true(file.exists(path))
  expect_false(file.exists(file.path(p$paths$out, "sap", "SAP_v1.docx")))
  expect_true(grepl("DRAFT", .sd_footer(path), fixed = TRUE))
  txt <- paste(.sd_text(path), collapse = "\n")
  expect_true(grepl("Statistical analysis plan (DRAFT, not locked)", txt, fixed = TRUE))
  expect_true(grepl("a_lr", txt, fixed = TRUE))
  for (v in leak) expect_false(grepl(v, txt, fixed = TRUE), info = v)
  expect_identical(sr_exports(p)$id, "SAP_draft")
  # the draft is overwritten freely
  p <- sr_sap_docx(p)
  expect_identical(sr_exports(p)$id, c("SAP_draft", "SAP_draft"))
  expect_true(file.exists(path))
})

test_that("the feasibility docx holds a_lr's mde as the model's feasibility sentence prints it, and its tier", {
  dir <- .sd_copy()
  p <- .sd_open(dir, environment())
  p <- sr_feasibility_docx(p)
  path <- file.path(p$paths$out, "feasibility", "feasibility-001.docx")
  expect_true(file.exists(path))
  txt <- paste(.sd_text(path), collapse = "\n")

  fr <- sr_feasibility_fragment(p, "a_lr")$values
  mde <- sr_fmt_num(fr$mde)
  m <- sr_doc_model(p)
  sent <- Filter(function(b) identical(b$id, "methods_feas_a_lr"), m$main)[[1L]]$text
  sent <- sr_ph_resolve(sent, m$reg, cite = sr_cite_fn(sr_cite_order(m)))
  expect_true(grepl(paste0("power ", mde), sent, fixed = TRUE))
  expect_true(grepl(paste0("power ", mde), txt, fixed = TRUE))
  rep <- sr_feasibility_report(p)
  tier <- rep$tier[rep$analysis == "a_lr"][1L]
  expect_true(grepl(gsub("_", " ", tier, fixed = TRUE), txt, fixed = TRUE))
  expect_true(grepl("Feasibility report", txt, fixed = TRUE))
  expect_identical(sr_exports(p)$kind, "feasibility")
  expect_identical(sr_exports(p)$id, "feasibility-001")
  # the next one is numbered 002
  p <- sr_feasibility_docx(p)
  expect_true(file.exists(file.path(p$paths$out, "feasibility", "feasibility-002.docx")))
})

test_that("the SAP and feasibility docs refuse a read-only project", {
  dir <- .sd_copy()
  p <- sr_project_open(dir)
  withr::defer(sr_duck_close(p))
  withr::defer(sr_project_close(p))
  p$readonly <- TRUE
  expect_error(sr_sap_docx(p), "read-only", fixed = TRUE)
  expect_error(sr_feasibility_docx(p), "read-only", fixed = TRUE)
})
