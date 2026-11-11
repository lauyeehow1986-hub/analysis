# Tests for the Archive panel module (ui/mod_archive.R), driven through shiny::testServer. Jobs run inline
# (option sr.run_sync = TRUE) unless a test says otherwise; the background tests replace .sr_job_spawn with a
# pending promise (helper-ui.R: .ui_pending_spawn) and .sr_job_kill with a stub.

.ar_args <- function(rv, busy = FALSE, ...) list(project = rv, busy = shiny::reactive(busy), ...)

# A writable project with one freeze "v1".
.ar_frozen <- function(env = parent.frame()) sr_freeze(.fx_writer_project(env), "v1")

.ar_n_events <- function(rv) length(sr_events_read(rv()$paths$events))

test_that("the freeze dialog needs exactly FREEZE", {
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_writer_project())
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    expect_false(running())
    session$setInputs(label = "freeze-001", freeze = 1)
    session$setInputs(confirm = "freeze", do_freeze = 1)
    session$setInputs(confirm = "Freeze", do_freeze = 2)
    expect_identical(nrow(sr_freezes(rv())), 0L)
    session$setInputs(confirm = "FREEZE", do_freeze = 3)
    expect_identical(sr_freezes(rv())$label, "freeze-001")
    expect_false(rv()$readonly)
  })
})

test_that("a bad label and a used label are refused before the dialog", {
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(.ar_frozen())
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(label = "Bad Label", freeze = 1)
    expect_true(any(grepl("the label must match", seen$msg, fixed = TRUE)))
    session$setInputs(label = "v1", freeze = 2)
    expect_true(any(grepl("freeze label 'v1' is already used", seen$msg, fixed = TRUE)))
    # no dialog was opened, so a typed FREEZE freezes nothing
    session$setInputs(confirm = "FREEZE", do_freeze = 1)
    expect_identical(sr_freezes(rv())$label, "v1")
  })
})

test_that("a freeze the core refuses is shown verbatim and the dialog does not freeze", {
  seen <- .ui_spy_notify()
  p <- .fx_ui_project()                         # SAP not locked
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(label = "v1", freeze = 1)
    session$setInputs(confirm = "FREEZE", do_freeze = 1)
    expect_true(any(grepl("the SAP is not locked", seen$msg, fixed = TRUE)))
    expect_identical(nrow(sr_freezes(rv())), 0L)
  })
})

test_that("quick verify shows ok", {
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(.ar_frozen())
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(freeze_pick = "v1", quick_verify = 1)
    expect_match(paste(output$verify_out$html, collapse = " "), "data-status=\"ok\"", fixed = TRUE)
    expect_false(running())
  })
})

test_that("verify with data runs the worker inline and lists absent files as information", {
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(.ar_frozen())
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(freeze_pick = "v1", data_verify = 1)
    expect_match(paste(output$verify_out$html, collapse = " "), "data-status=\"ok\"", fixed = TRUE)
    expect_false(running())
    expect_false(rv()$readonly)
  })
})

test_that("diff against project now shows the results diff and hides unchanged analyses", {
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(.ar_frozen())
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(diff_from = "v1", diff_to = "now", diff = 1)
    html <- paste(output$diff_out$html, collapse = " ")
    expect_match(html, "same_run", fixed = TRUE)
    expect_match(html, "a_lr", fixed = TRUE)
    expect_false(grepl(">same<", html, fixed = TRUE))   # the analyses rows that did not change are filtered out
  })
})

test_that("diff between two freezes has no results diff", {
  seen <- .ui_spy_notify()
  p <- .ar_frozen()
  p <- sr_freeze(p, "v2")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(diff_from = "v1", diff_to = "v2", diff = 1)
    html <- paste(output$diff_out$html, collapse = " ")
    expect_false(grepl("same_run", html, fixed = TRUE))
    expect_match(html, "No differences", fixed = TRUE)
  })
})

test_that(".srproj path checks refuse before any job starts", {
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  td <- withr::local_tempdir()
  existing <- file.path(td, "there.srproj")
  writeLines("x", existing)
  rv <- shiny::reactiveVal(.ar_frozen())
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    n0 <- .ar_n_events(rv)
    for (bad in list(file.path(td, "a.zip"), existing, file.path(td, "no-such-folder", "a.srproj"), "")) {
      session$setInputs(ex_label = "v1", ex_data = "auto", ex_path = bad, export = sample.int(1e6, 1))
      expect_false(running())
      expect_false(rv()$readonly)
    }
    expect_length(ctl$calls, 0L)
    expect_identical(.ar_n_events(rv), n0)
    expect_true(any(grepl("the path must end in .srproj", seen$msg, fixed = TRUE)))
    expect_true(any(grepl("already exists", seen$msg, fixed = TRUE)))
    expect_true(any(grepl("does not exist", seen$msg, fixed = TRUE)))
  })
})

test_that("a cancelled .srproj export leaves no file, no staging folder and no event", {
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  .ui_stub(".sr_job_kill", function(job_file, wait = 10) TRUE)
  p <- .ar_frozen()
  dir <- p$paths$dir
  td <- withr::local_tempdir()
  path <- file.path(td, "a.srproj")
  staging <- file.path(dir, "out", ".srproj-v1.partial")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    n0 <- .ar_n_events(rv)
    session$setInputs(ex_label = "v1", ex_data = "full", ex_path = path, export = 1)
    expect_true(running())
    expect_true(rv()$readonly)                            # the lock is released while the worker runs
    expect_length(ctl$calls, 1L)
    expect_identical(ctl$calls[[1L]]$fn, ".sr_ar_export_worker")
    # what a half-done worker leaves behind
    writeLines("x", paste0(path, ".partial"))
    dir.create(staging, recursive = TRUE)
    writeLines("y", file.path(staging, "events.jsonl"))
    session$setInputs(ex_cancel = 1)
    expect_false(file.exists(path))
    expect_false(file.exists(paste0(path, ".partial")))
    expect_false(dir.exists(staging))
    expect_identical(.ar_n_events(rv), n0)
    expect_false(running())
    expect_false(isTRUE(rv()$readonly))                  # the project is writable again
    expect_false(.sr_ar_exported_after(dir, n0))
  })
})

test_that("a cancelled export never removes a path that existed before the job", {
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  .ui_stub(".sr_job_kill", function(job_file, wait = 10) TRUE)
  p <- .ar_frozen()
  dir <- p$paths$dir
  td <- withr::local_tempdir()
  path <- file.path(td, "a.srproj")
  staging <- file.path(dir, "out", ".srproj-v1.partial")
  dir.create(staging, recursive = TRUE)                  # a staging folder that was there before the click
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(ex_label = "v1", ex_data = "full", ex_path = path, export = 1)
    expect_true(running())
    session$setInputs(ex_cancel = 1)
    expect_true(dir.exists(staging))
    expect_false(running())
  })
})

test_that("a cancel that comes after the export finished reports it and keeps the archive", {
  seen <- .ui_spy_notify()
  withr::local_options(sr.run_sync = TRUE)
  p <- .ar_frozen()
  td <- withr::local_tempdir()
  path <- file.path(td, "a.srproj")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(ex_label = "v1", ex_data = "full", ex_path = path, export = 1)
    expect_true(file.exists(path))
    # the cleanup, called as a cancel would call it, sees the srproj.exported event and removes nothing
    expect_identical(.sr_ar_export_cleanup(rv()$paths$dir, path, "v1", 0L), "finished")
    expect_true(file.exists(path))
  })
})

test_that("the export succeeds and the project is writable again", {
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  p <- .ar_frozen()
  td <- withr::local_tempdir()
  path <- file.path(td, "a.srproj")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(ex_label = "v1", ex_data = "auto", ex_path = paste0("\"", path, "\""), export = 1)
    expect_true(file.exists(path))
    expect_false(isTRUE(rv()$readonly))
    expect_length(rv()$state$srproj_exports, 1L)
    expect_identical(rv()$state$srproj_exports[[1L]]$label, "v1")
    expect_true(any(grepl("Exported", seen$msg, fixed = TRUE)))
    expect_false(running())
    expect_false(file.exists(paste0(path, ".partial")))
    expect_false(dir.exists(file.path(rv()$paths$dir, "out", ".srproj-v1.partial")))
  })
})

test_that("a failing export shows the core message verbatim and cleans up", {
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  p <- .ar_frozen()
  td <- withr::local_tempdir()
  path <- file.path(td, "a.srproj")
  .ui_stub(".sr_ar_export_worker", function(project_dir, path, label, data, job_file) {
    list(ok = FALSE, error = "the archive cannot be built today")
  })
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(ex_label = "v1", ex_data = "auto", ex_path = path, export = 1)
    expect_true(any(grepl("the archive cannot be built today", seen$msg, fixed = TRUE)))
    expect_false(isTRUE(rv()$readonly))
    expect_false(running())
  })
})

test_that("the export plan says what auto will pick, with the actor warning and a link to Settings", {
  withr::local_options(sr.user = NULL)
  withr::local_envvar(SR_USER = NA)
  rv <- shiny::reactiveVal(.ar_frozen())
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(ex_label = "v1", ex_data = "auto")
    html <- paste(output$ex_plan$html, collapse = " ")
    expect_match(html, "auto will pick (full|lean) \\(", perl = TRUE)
    expect_match(html, "sr.user", fixed = TRUE)
    expect_match(html, "go_settings", fixed = TRUE)
    withr::local_options(sr.user = "analyst-role")
    session$setInputs(ex_data = "lean")
    html2 <- paste(output$ex_plan$html, collapse = " ")
    expect_false(grepl("go_settings", html2, fixed = TRUE))
  })
})

test_that("re-baseline needs a reason and is refused when the library matches", {
  seen <- .ui_spy_notify()
  # no freeze yet
  rv0 <- shiny::reactiveVal(.fx_writer_project())
  shiny::testServer(mod_archive_server, args = .ar_args(rv0), {
    session$setInputs(rb_reason = "why", rebaseline = 1)
    expect_true(any(grepl("Re-baseline: freeze the project first", seen$msg, fixed = TRUE)))
  })
  seen$msg <- character()
  rv <- shiny::reactiveVal(.ar_frozen())
  sv <- shiny::reactiveVal(0L)
  shiny::testServer(mod_archive_server, args = .ar_args(rv, settings = function() sv()), {
    session$setInputs(rb_reason = "why", rebaseline = 1)
    expect_true(any(grepl("Re-baseline: the library already matches the baseline", seen$msg, fixed = TRUE)))
    expect_match(paste(output$rb_diff$html, collapse = " "), "the library already matches the baseline",
                 fixed = TRUE)
    # the library moves: now a reason is needed
    withr::local_options(sr.cran_snapshot = "2099-01-01")
    sv(1L)                                                # what a change in Settings does
    session$setInputs(rb_reason = "  ", rebaseline = 2)
    expect_true(any(grepl("Re-baseline: give a reason", seen$msg, fixed = TRUE)))
    expect_identical(sr_lib_baseline(rv())$from, "freeze")
    expect_match(paste(output$rb_diff$html, collapse = " "), "snapshot_date", fixed = TRUE)
    session$setInputs(rb_reason = "CRAN snapshot moved for the revision", rebaseline = 3)
    expect_identical(sr_lib_baseline(rv())$from, "rebaseline")
  })
})

test_that("a second job is refused while one runs, and a busy app refuses to start one", {
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  p <- .ar_frozen()
  td <- withr::local_tempdir()
  rv <- shiny::reactiveVal(p)
  busy <- shiny::reactiveVal(FALSE)
  shiny::testServer(mod_archive_server, args = list(project = rv, busy = busy), {
    session$setInputs(ex_label = "v1", ex_data = "full", ex_path = file.path(td, "a.srproj"), export = 1)
    expect_true(running())
    n_msg <- length(seen$msg)
    session$setInputs(ex_path = file.path(td, "b.srproj"), export = 2)
    session$setInputs(freeze_pick = "v1", data_verify = 1)
    expect_length(ctl$calls, 1L)
    new <- seen$msg[seq_along(seen$msg) > n_msg]
    expect_length(new, 2L)
    expect_true(all(grepl(.sr_ui_busy_text, new, fixed = TRUE)))
    # a freeze is refused too
    n_msg <- length(seen$msg)
    session$setInputs(label = "v9", freeze = 1)
    expect_true(any(grepl(.sr_ui_busy_text, seen$msg[seq_along(seen$msg) > n_msg], fixed = TRUE)))
    # settle the job so the project is released
    ctl$reject(simpleError("worker died"))
    for (i in 1:50) {
      later::run_now(0.1)
      session$flushReact()
      if (!running()) break
    }
    expect_false(running())
    expect_false(isTRUE(rv()$readonly))
    expect_true(any(grepl("worker died", seen$msg, fixed = TRUE)))
    # the app-wide busy flag blocks a start
    busy(TRUE)
    session$flushReact()
    n_msg <- length(seen$msg)
    session$setInputs(ex_path = file.path(td, "c.srproj"), export = 3)
    expect_length(ctl$calls, 1L)
    expect_true(any(grepl(.sr_ui_busy_text, seen$msg[seq_along(seen$msg) > n_msg], fixed = TRUE)))
  })
})

test_that("the worker gets the session's settings (the actor) and never the Jev key", {
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  withr::local_options(sr.user = "reviewer-role", sr.jev_key = "SENTINEL-JEV-KEY-123")
  p <- .ar_frozen()
  td <- withr::local_tempdir()
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(ex_label = "v1", ex_data = "full", ex_path = file.path(td, "a.srproj"), export = 1)
    expect_length(ctl$calls, 1L)
    o <- ctl$calls[[1L]]$opts
    expect_identical(o[["sr.user"]], "reviewer-role")
    expect_false("sr.jev_key" %in% names(o))
    expect_false(grepl("SENTINEL-JEV-KEY-123", paste(unlist(ctl$calls[[1L]]), collapse = " "), fixed = TRUE))
    for (nm in c("gates", "verify_out", "diff_out", "ex_plan", "rb_diff", "job_status")) {
      expect_false(grepl("SENTINEL-JEV-KEY-123", paste(output[[nm]]$html, collapse = " "), fixed = TRUE))
    }
    ctl$reject(simpleError("stop"))
    for (i in 1:50) {
      later::run_now(0.1)
      session$flushReact()
      if (!running()) break
    }
  })
})

test_that("a background export that ends ok is settled with the project writable", {
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  p <- .ar_frozen()
  td <- withr::local_tempdir()
  path <- file.path(td, "a.srproj")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(ex_label = "v1", ex_data = "full", ex_path = path, export = 1)
    expect_true(running())
    # run the real worker body here, as the mirai would, then resolve the job with its value
    res <- .sr_ar_export_worker(rv()$paths$dir, path, "v1", "full", NULL)
    expect_true(res$ok)
    ctl$resolve(res)
    for (i in 1:50) {
      later::run_now(0.1)
      session$flushReact()
      if (!running()) break
    }
    expect_false(running())
    expect_false(isTRUE(rv()$readonly))
    expect_true(file.exists(path))
    expect_length(rv()$state$srproj_exports, 1L)
    expect_true(any(grepl(paste0("Exported ", normalizePath(path, winslash = "/", mustWork = FALSE)), seen$msg, fixed = TRUE)))
  })
})

# Start a pending export in the running testServer session; returns the destination path.
.ar_start_pending <- function(session, td, name = "a.srproj") {
  path <- file.path(td, name)
  session$setInputs(ex_label = "v1", ex_data = "full", ex_path = path, export = sample.int(1e6, 1))
  path
}

test_that("a cleanup that throws still releases the busy flag and the project", {
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  .ui_stub(".sr_job_kill", function(job_file, wait = 10) TRUE)
  .ui_stub(".sr_ar_export_cleanup", function(...) stop("disk on fire"))
  td <- withr::local_tempdir()
  rv <- shiny::reactiveVal(.ar_frozen())
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    .ar_start_pending(session, td)
    expect_true(running())
    session$setInputs(ex_cancel = 1)
    expect_false(running())
    expect_false(isTRUE(rv()$readonly))
    expect_true(any(grepl("disk on fire", seen$msg, fixed = TRUE)))
  })
})

test_that("a verify whose display throws still releases the busy flag", {
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  .ui_stub(".sr_ar_verify_rows", function(v) stop("cannot show"))
  rv <- shiny::reactiveVal(.ar_frozen())
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    session$setInputs(freeze_pick = "v1", data_verify = 1)
    expect_false(running())
    expect_true(any(grepl("cannot show", seen$msg, fixed = TRUE)))
    # the next job is not refused as busy
    session$setInputs(data_verify = 2)
    expect_false(any(grepl(.sr_ui_busy_text, seen$msg, fixed = TRUE)))
  })
})

test_that("cancel settles when the kill finds no worker, and stays cancellable when the kill fails", {
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  kill <- "gone"
  .ui_stub(".sr_job_kill", function(job_file, wait = 10) {
    if (identical(kill, "fail")) stop("could not stop the analysis worker (process 1 still running)")
    identical(kill, "killed")
  })
  td <- withr::local_tempdir()
  rv <- shiny::reactiveVal(.ar_frozen())
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    n0 <- .ar_n_events(rv)
    path <- .ar_start_pending(session, td)
    writeLines("x", paste0(path, ".partial"))
    # a live process the kill could not stop: nothing is cleaned, the job stays and can be cancelled again
    kill <<- "fail"
    session$setInputs(ex_cancel = 1)
    expect_true(running())
    expect_true(rv()$readonly)
    expect_true(file.exists(paste0(path, ".partial")))
    expect_true(any(grepl("still running", seen$msg, fixed = TRUE)))
    # the worker never started / is gone (the kill returns FALSE): cleanup and release
    kill <<- "gone"
    session$setInputs(ex_cancel = 2)
    expect_false(running())
    expect_false(isTRUE(rv()$readonly))
    expect_false(file.exists(paste0(path, ".partial")))
    expect_identical(.ar_n_events(rv), n0)
  })
})

test_that("closing the session mid-export kills the worker and removes the partial files", {
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  killed <- FALSE
  .ui_stub(".sr_job_kill", function(job_file, wait = 10) {
    killed <<- TRUE
    TRUE
  })
  p <- .ar_frozen()
  staging <- file.path(p$paths$dir, "out", ".srproj-v1.partial")
  td <- withr::local_tempdir()
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_archive_server, args = .ar_args(rv), {
    path <- .ar_start_pending(session, td)
    writeLines("x", paste0(path, ".partial"))
    dir.create(staging, recursive = TRUE)
    session$close()
    expect_true(killed)
    expect_false(file.exists(paste0(path, ".partial")))
    expect_false(dir.exists(staging))
  })
})
