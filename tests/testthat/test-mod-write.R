# Tests for the Write panel module (ui/mod_write.R), driven through shiny::testServer. Synthetic data only.

.mw_files <- function(p) list.files(p$paths$dir, recursive = TRUE, all.files = TRUE)
.mw_html <- function(x) paste(as.character(x$html %||% x), collapse = " ")

# Replace the global function `name` for the rest of the calling test; returns the original.
.mw_stub <- function(name, fn, env = parent.frame()) {
  old <- get(name, envir = globalenv())
  assign(name, fn, envir = globalenv())
  withr::defer(assign(name, old, envir = globalenv()), envir = env)
  invisible(old)
}

test_that("buttons are disabled with the gate reason on an unlocked project", {
  withr::local_options(sr.mode = "airgap")
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_write_server, args = list(project = rv, busy = function() FALSE), {
    session$setInputs(check = 1)
    expect_equal(seen$msg, "Check manuscript: lock the SAP first")
    expect_false(grepl("<table", .mw_html(output$guard)))
    session$setInputs(preview = 1, export = 1)
    expect_equal(seen$msg[-1L], c("Preview: lock the SAP first", "Export: lock the SAP first"))
    expect_match(.mw_html(output$gates), "lock the SAP first")
  })
})

test_that("a busy app refuses with the shared busy text", {
  withr::local_options(sr.mode = "airgap")
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_writer_project())
  shiny::testServer(mod_write_server, args = list(project = rv, busy = function() TRUE), {
    session$setInputs(export = 1)
    expect_equal(seen$msg, paste0("Export: ", .sr_ui_busy_text))
  })
})

test_that("Check shows findings and writes no file", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  p <- .fx_writer_project()
  rv <- shiny::reactiveVal(p)
  before <- .mw_files(p)
  shiny::testServer(mod_write_server, args = list(project = rv, busy = function() FALSE), {
    session$setInputs(check = 1)
    html <- .mw_html(output$guard)
    expect_true(grepl("<table|found nothing", html))
    expect_match(.mw_html(output$checklist), "<table|no reporting checklist")
  })
  expect_identical(.mw_files(shiny::isolate(rv())), before)
})

test_that("an invalid suppress_k shows the core message and disables Check", {
  withr::local_options(sr.mode = "airgap")
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_writer_project())
  shiny::testServer(mod_write_server, args = list(project = rv, busy = function() FALSE), {
    session$setInputs(suppress_k = 1)
    msg <- .sr_wp_opts("default", 1, TRUE, 0.05)$error
    expect_true(nzchar(msg))
    expect_equal(output$opts_error, msg)
    session$setInputs(check = 1)
    expect_equal(seen$msg, paste0("Check manuscript: ", msg))
  })
})

test_that("the model cache drops after a project change", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  n <- 0L
  real <- .mw_stub("sr_doc_model", function(...) {
    n <<- n + 1L
    real(...)
  })
  rv <- shiny::reactiveVal(.fx_writer_project())
  shiny::testServer(mod_write_server, args = list(project = rv, busy = function() FALSE), {
    session$setInputs(check = 1)
    expect_equal(n, 1L)
    session$setInputs(check = 2)
    expect_equal(n, 1L)
    rv(sr_project_append(rv(), "note", list(text = "a change")))
    session$setInputs(check = 3)
    expect_equal(n, 2L)
    session$setInputs(check = 4)
    expect_equal(n, 2L)
  })
})

test_that("Export adds export-001 and a history row", {
  withr::local_options(sr.mode = "airgap")
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_writer_project())
  shiny::testServer(mod_write_server, args = list(project = rv, busy = function() FALSE), {
    expect_match(.mw_html(output$history), "Nothing has been exported")
    session$setInputs(export = 1)
    expect_equal(seen$type, "message")
    expect_true(dir.exists(file.path(rv()$paths$out, "exports", "export-001")))
    expect_match(.mw_html(output$history), "export-001")
    expect_match(.mw_html(output$history), "dl_export_001")
  })
})

test_that("Export refused on a read-only project is shown verbatim and writes nothing", {
  withr::local_options(sr.mode = "airgap")
  seen <- .ui_spy_notify()
  p <- .fx_writer_project()
  p$readonly <- TRUE
  rv <- shiny::reactiveVal(p)
  before <- .mw_files(p)
  shiny::testServer(mod_write_server, args = list(project = rv, busy = function() FALSE), {
    session$setInputs(export = 1)
    expect_equal(seen$msg, "Export: the project is read-only (locked by another session)")
  })
  expect_identical(.mw_files(p), before)
})

test_that("the zip download holds the export files", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_writer_project())
  shiny::testServer(mod_write_server, args = list(project = rv, busy = function() FALSE), {
    session$setInputs(export = 1)
    z <- output$dl_export_001
    expect_true(file.exists(z))
    ent <- utils::unzip(z, list = TRUE)$Name
    expect_true("export-001/manifest.json" %in% ent)
    expect_true("export-001/manuscript.docx" %in% ent)
  })
})

test_that("Verify on a changed file shows a red problem line", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_writer_project())
  shiny::testServer(mod_write_server, args = list(project = rv, busy = function() FALSE), {
    session$setInputs(export = 1)
    session$setInputs(verify_export_001 = 1)
    expect_match(.mw_html(output$verify), "matches its recorded hash")
    f <- file.path(rv()$paths$out, "exports", "export-001", "analysis.R")
    cat("# tampered\n", file = f, append = TRUE)
    session$setInputs(verify_export_001 = 2)
    html <- .mw_html(output$verify)
    expect_match(html, "changed:")
    expect_match(html, "text-danger")
  })
})

test_that("Open folder calls the OS opener with the export folder", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  got <- character()
  .mw_stub(".sr_ui_open_folder", function(dir) got <<- c(got, dir))
  rv <- shiny::reactiveVal(.fx_writer_project())
  shiny::testServer(mod_write_server, args = list(project = rv, busy = function() FALSE), {
    session$setInputs(export = 1)
    session$setInputs(open_export_001 = 1)
    expect_equal(normalizePath(got, winslash = "/"),
                 normalizePath(file.path(rv()$paths$out, "exports", "export-001"), winslash = "/"))
  })
})

test_that("Preview renders once per model and shows the manuscript", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  n <- 0L
  real <- .mw_stub(".sr_wp_preview", function(...) {
    n <<- n + 1L
    real(...)
  })
  rv <- shiny::reactiveVal(.fx_writer_project())
  shiny::testServer(mod_write_server, args = list(project = rv, busy = function() FALSE), {
    session$setInputs(preview = 1)
    expect_match(.mw_html(output$preview), "Supplement")
    session$setInputs(preview = 2)
    expect_equal(n, 1L)
  })
})
