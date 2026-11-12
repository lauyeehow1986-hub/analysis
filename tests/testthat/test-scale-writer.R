# The writer on streamed analyses (P1e.1 Task 13): the Engine column of SAP_v1, the engine Methods sentence
# through placeholders, an lm stream result with no figure, and the engine switch in the deviations text.
# Synthetic data only (.sc_frame).

.scw_layers <- function() {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = FALSE)
}

.scw_spec <- function(outcome = "y", ...) {
  s <- list(purpose = "explain-causal", outcome = outcome, exposure = "sex", exposure_ref = "F",
            terms = list(list(var = "x1")), layers = .scw_layers(), seed = 42L)
  x <- list(...)
  for (nm in names(x)) s[[nm]] <- x[[nm]]
  s
}

# Locked (and optionally run) project, 3000 rows, chunks of 1000.
.scw_project <- function(spec, route = "logistic", run = TRUE, env = parent.frame()) {
  withr::local_options(sr.stream_chunk_rows = 1000, .local_envir = env)
  p <- .sc_locked(spec, route = route, env = env, df = .sc_frame(3000L))
  if (run) {
    p <- sr_run(p, "a1")
    expect_true(isTRUE(p$state$runs$a1$ok), info = p$state$runs$a1$error)
  }
  p
}

.scw_docx_text <- function(path) {
  s <- officer::docx_summary(officer::read_docx(path), remove_fields = TRUE)
  s$text[!is.na(s$text)]
}

.scw_methods <- function(m, id = "a1") {
  b <- Filter(function(b) identical(b$type, "para") && identical(b$id, paste0("methods_", id)), m$main)
  b[[1L]]$text
}

test_that("SAP_v1 shows the Engine column", {
  withr::local_envvar(SR_RAM_BUDGET_GB = "0.0001")
  p <- .scw_project(.scw_spec(), run = FALSE, env = environment())
  expect_identical(p$state$sap$lock$engines$a1$engine, "stream")
  p <- sr_sap_docx(p)
  txt <- .scw_docx_text(file.path(p$paths$out, "sap", "SAP_v1.docx"))
  expect_true("Engine" %in% txt)
  expect_true("Streaming" %in% txt)
  expect_true(any(grepl("auto -> stream", txt, fixed = TRUE)))
  expect_false("In memory" %in% txt)
})

test_that("SAP_v1 shows an in-memory analysis as In memory", {
  p <- .scw_project(.scw_spec(), run = FALSE, env = environment())
  expect_identical(p$state$sap$lock$engines$a1$engine, "memory")
  p <- sr_sap_docx(p)
  txt <- .scw_docx_text(file.path(p$paths$out, "sap", "SAP_v1.docx"))
  expect_true("Engine" %in% txt)
  expect_true("In memory" %in% txt)
  expect_false("Streaming" %in% txt)
  expect_true(any(grepl("auto -> memory", txt, fixed = TRUE)))
})

test_that("a SAP locked before P1e has no Engine column", {
  p <- .scw_project(.scw_spec(), run = FALSE, env = environment())
  p$state$sap$lock$engines <- NULL
  expect_identical(.sr_sd_engine_table(p), NULL)
})

test_that("manuscript Methods carry the engine sentence through placeholders", {
  p <- .scw_project(.scw_spec(engine = "stream"), env = environment())
  m <- sr_doc_model(p)
  tpl <- .scw_methods(m)
  expect_identical(sr_ph_digits(tpl), character(0))
  txt <- sr_ph_resolve(tpl, m$reg, cite = sr_cite_fn(m$main[[length(m$main)]]$keys))
  expect_match(txt, "fitted by streaming iteratively reweighted least squares", fixed = TRUE)
  expect_match(txt, "in chunks of 1000 rows", fixed = TRUE)
  expect_match(txt, "over all 3000 rows", fixed = TRUE)
  rows <- sr_ph_table(m$reg)
  expect_true(any(grepl("^fragment:a1:engine", rows$source)))
  v <- sr_ph_verify(m$reg, p)
  expect_true(all(v$ok), info = paste(v$source[!v$ok], v$got[!v$ok], collapse = "; "))
  g <- sr_claim_guard(m)
  expect_identical(nrow(g), 0L, info = paste(g$block, g$rule, g$detail, collapse = "\n"))
  # the Word file carries the sentence
  out <- tempfile("scw-docx-")
  dir.create(out)
  withr::defer(unlink(out, recursive = TRUE))
  props <- list(audit_tip = "a1b2c3d4e5f60718293a4b5c6d7e8f90", sap_hash = "f5bdde8442476060aabbccddeeff0011",
                export_id = "export-001", created = "2026-10-18T09:30:00Z")
  f <- sr_docx_render(m, "main", file.path(out, "main.docx"), file.path(out, "fig"), props)
  dt <- paste(.scw_docx_text(if (is.character(f)) f else file.path(out, "main.docx")), collapse = "\n")
  expect_match(dt, "fitted by streaming iteratively reweighted least squares", fixed = TRUE)
})

test_that("every stream-capable route has an engine sentence kind, survival routes included", {
  for (r in c("lm", "lm_describe", "logistic", "logistic_describe", "modpois", "table1")) {
    expect_false(is.null(.sr_engine_kind(r)), info = r)
  }
  expect_identical(.sr_engine_kind("lm_describe"), "lm")
  expect_identical(.sr_engine_kind("cox"), "cox")
  expect_identical(.sr_engine_kind("cox_describe"), "cox")
  expect_identical(.sr_engine_kind("rmst"), "rmst")
})

test_that("a streamed lm_describe carries the engine sentence through placeholders", {
  p <- .scw_project(.scw_spec(outcome = "yc", purpose = "describe", engine = "stream"), route = "lm_describe",
                    env = environment())
  expect_match(sr_result(p, "a1")$fragments$engine, "least squares", fixed = TRUE)
  m <- sr_doc_model(p)
  tpl <- .scw_methods(m)
  expect_identical(sr_ph_digits(tpl), character(0))
  txt <- sr_ph_resolve(tpl, m$reg, cite = sr_cite_fn(m$main[[length(m$main)]]$keys))
  expect_match(txt, "fitted by streaming least squares", fixed = TRUE)
  expect_match(txt, "in chunks of 1000 rows", fixed = TRUE)
  expect_true(any(grepl("^fragment:a1:engine", sr_ph_table(m$reg)$source)))
  v <- sr_ph_verify(m$reg, p)
  expect_true(all(v$ok), info = paste(v$source[!v$ok], v$got[!v$ok], collapse = "; "))
  g <- sr_claim_guard(m)
  expect_identical(nrow(g), 0L, info = paste(g$block, g$rule, g$detail, collapse = "\n"))
})

test_that("a memory analysis has no engine sentence in the Methods", {
  p <- .scw_project(.scw_spec(), env = environment())
  m <- sr_doc_model(p)
  expect_false(grepl("streaming", .scw_methods(m), fixed = TRUE))
  expect_false(any(grepl("^fragment:a1:engine", sr_ph_table(m$reg)$source)))
})

test_that("an lm stream analysis has no figure slot and the document still renders", {
  p <- .scw_project(.scw_spec(outcome = "yc", engine = "stream"), route = "lm", env = environment())
  expect_null(sr_result(p, "a1")$figures$resid)
  m <- sr_doc_model(p)
  figs <- Filter(function(b) identical(b$type, "figure"), c(m$main, m$supplement))
  for (b in figs) {
    expect_true(b$kind %in% c("km", "forest", "flow"))
    expect_true(is.list(b$data) || is.data.frame(b$data))
    expect_true(nzchar(b$caption))
  }
  expect_false(any(grepl("esidual", vapply(figs, function(b) b$caption, ""))))
  out <- tempfile("scw-docx-")
  dir.create(out)
  withr::defer(unlink(out, recursive = TRUE))
  props <- list(audit_tip = "a1b2c3d4e5f60718293a4b5c6d7e8f90", sap_hash = "f5bdde8442476060aabbccddeeff0011",
                export_id = "export-001", created = "2026-10-18T09:30:00Z")
  f <- sr_docx_render(m, "main", file.path(out, "main.docx"), file.path(out, "fig"), props)
  expect_true(file.exists(if (is.character(f)) f else file.path(out, "main.docx")))
  expect_match(.scw_methods(m), "least squares", fixed = TRUE)
})

test_that("engine switch appears in the deviations text", {
  withr::local_options(sr.stream_chunk_rows = 1000)
  p <- .sc_locked(.scw_spec(), env = environment(), df = .sc_frame(3000L))
  p <- sr_engine_switch(p, "a1", "stream", "moved to the locked-down laptop")
  p <- sr_run(p, "a1")
  expect_true(isTRUE(p$state$runs$a1$ok), info = p$state$runs$a1$error)
  m <- sr_doc_model(p)
  b <- Filter(function(b) identical(b$type, "para") && identical(b$id, "methods_deviations"), m$main)[[1L]]
  expect_match(b$text, "engine switch", fixed = TRUE)
  expect_match(b$text, "computational", fixed = TRUE)
  expect_identical(sr_ph_digits(b$text), character(0))
  tb <- Filter(function(b) identical(b$type, "table") && identical(b$id, "deviations"), m$supplement)[[1L]]
  expect_true("computational" %in% tb$df$Kind)
  expect_true(any(grepl("engine changed from memory to stream", tb$df$Reason, fixed = TRUE)))
  g <- sr_claim_guard(m)
  expect_identical(nrow(g), 0L, info = paste(g$block, g$rule, g$detail, collapse = "\n"))
  # no switch, no computational sentence
  p0 <- .sc_locked(.scw_spec(), env = environment(), df = .sc_frame(3000L))
  expect_false(grepl("computational", .sr_wr_m_deviations(p0), fixed = TRUE))
})
