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

.sd_rebind <- function(name, fn, env) {
  g <- globalenv()
  old <- get(name, envir = g)
  assign(name, fn, envir = g)
  withr::defer(assign(name, old, envir = g), envir = env)
  invisible(TRUE)
}

test_that("F3/N1/N5: with the default k = 5 the SAP and the feasibility docx hide Events and Prevalence of a small n_k; n stays", {
  dir <- .sd_copy()
  p <- .sd_open(dir, environment())
  real <- get("sr_feasibility_report", envir = globalenv())
  small <- function(project) {
    r <- real(project)
    r$events[1L] <- 3
    r$prevalence[1L] <- 3 / r$n_cc[1L]
    r
  }
  rep <- small(p)
  n1 <- as.character(as.integer(rep$n_cc[1L]))
  pv <- sr_fmt_num(rep$prevalence[1L])
  expect_true(nchar(pv) >= 4L)
  .sd_rebind("sr_feasibility_report", small, environment())
  p <- sr_feasibility_docx(p)
  cells <- function(path) {
    s <- officer::docx_summary(officer::read_docx(path))
    s$text[!is.na(s$text) & s$content_type == "table cell"]
  }
  fc <- cells(file.path(p$paths$out, "feasibility", "feasibility-001.docx"))
  expect_true("<5" %in% fc)
  expect_false("3" %in% fc)
  expect_false(pv %in% fc)
  expect_true(n1 %in% fc)
  # the SAP of a locked project, same default k
  dir2 <- .sd_copy()
  q <- .sd_open(dir2, environment())
  q <- sr_sap_docx(q)
  sc <- .sd_text(file.path(q$paths$out, "sap", "SAP_v1.docx"))
  expect_true("<5" %in% sc)
  expect_false("3" %in% sc)
  expect_false(pv %in% sc)
  expect_error(sr_sap_docx(q, suppress_k = 2L), "suppress_k", fixed = TRUE)
})

test_that("N1: .sr_sd_prev hides a prevalence whose n_k is 1 to k - 1, and nothing else", {
  expect_identical(.sr_sd_prev(c(3 / 400, 4 / 400, 5 / 400, 0, 0.5, NA), rep(400, 6L), 5L),
                   c("<5", "<5", sr_fmt_num(5 / 400), sr_fmt_num(0), sr_fmt_num(0.5), ""))
  expect_identical(.sr_sd_prev(0.25, NA_real_, 5L), sr_fmt_num(0.25))
})

test_that("minor 9: a locked SAP is refused a second time even with a custom path", {
  lk <- .sd_locked()
  other <- tempfile("sd-other-", fileext = ".docx")
  expect_error(sr_sap_docx(lk$project, path = other), "already")
  expect_false(file.exists(other))
})

test_that("minor 8/14: a failed doc.exported append removes the file just written and re-raises the condition", {
  dir <- .sd_copy()
  p <- .sd_open(dir, environment())
  boom <- simpleError("append boom", call = quote(sr_project_append(x)))
  .sd_rebind("sr_project_append", function(project, type, payload) stop(boom), environment())
  path <- file.path(p$paths$out, "feasibility", "feasibility-001.docx")
  cond <- tryCatch(sr_feasibility_docx(p), error = function(e) e)
  expect_identical(conditionMessage(cond), "append boom")
  expect_identical(conditionCall(cond), quote(sr_project_append(x)))
  expect_false(file.exists(path))
})

test_that("minor 16: the draft SAP of a project with results holds none of its numbers", {
  dir <- .sd_copy()
  p <- .sd_open(dir, environment())
  res <- sr_result(p, "a_lr")$effects
  leak <- unique(c(sr_fmt_num(res$est), sr_fmt_p(res$p), sr_fmt_num(res$lo), sr_fmt_num(res$hi)))
  leak <- leak[nchar(leak) >= 4L]
  expect_gt(length(leak), 0L)
  p$state$sap$locked <- FALSE
  p <- sr_sap_docx(p)
  path <- file.path(p$paths$out, "sap", "SAP_draft.docx")
  expect_true(file.exists(path))
  txt <- paste(.sd_text(path), collapse = "\n")
  for (v in leak) expect_false(grepl(v, txt, fixed = TRUE), info = v)
})

test_that("minor 17: a clustered analysis gets the eldridge2006 citation line in the feasibility docx", {
  dir <- .sd_copy()
  p <- .sd_open(dir, environment())
  .sd_rebind(".sr_wr_clustered", function(project, id) TRUE, environment())
  p <- sr_feasibility_docx(p)
  txt <- .sd_text(file.path(p$paths$out, "feasibility", "feasibility-001.docx"))
  expect_true(any(startsWith(txt, "[1] ")))
  expect_true(any(grepl("Eldridge", txt, fixed = TRUE)))
})
