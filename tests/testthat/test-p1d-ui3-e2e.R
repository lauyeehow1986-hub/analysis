# End-to-end tests for the P1d panels (Write, Archive, Import, Settings): the Jev key never leaves the
# session, and the background jobs run in real mirai workers. Synthetic data only; the key is a fake string
# and nothing here contacts any host.

.e3_key <- "SENTINEL-KEY-7f3a"

.e3_app <- function() {
  old <- setwd(file.path(normalizePath(testthat::test_path("../.."), winslash = "/"), "app"))
  on.exit(setwd(old))
  shiny::shinyAppDir(".")
}

# Run the event loop until `done()` is TRUE or `secs` pass.
.e3_wait <- function(done, session, secs = 120) {
  t0 <- Sys.time()
  while (!isTRUE(done()) && as.numeric(difftime(Sys.time(), t0, units = "secs")) < secs) {
    later::run_now(0.2)
    session$flushReact()
  }
  invisible(done())
}

# TRUE when the raw bytes of `f`, or of anything zipped inside it (a docx, a zip), hold `key`.
.e3_file_has <- function(f, key, depth = 0L) {
  b <- readBin(f, "raw", file.size(f))
  if (length(grepRaw(key, b, fixed = TRUE))) return(TRUE)
  if (depth < 3L && length(b) > 4L && identical(b[1:2], as.raw(c(0x50, 0x4b)))) {
    td <- tempfile("e3-unz-")
    dir.create(td)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    ok <- tryCatch({
      utils::unzip(f, exdir = td)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (ok) {
      for (g in list.files(td, recursive = TRUE, all.files = TRUE, full.names = TRUE)) {
        if (.e3_file_has(g, key, depth + 1L)) return(TRUE)
      }
    }
  }
  FALSE
}

# The files (full paths) under `dir` or in `files` that hold `key`.
.e3_leaks <- function(key, dir = NULL, files = character()) {
  all <- c(if (!is.null(dir)) list.files(dir, recursive = TRUE, all.files = TRUE, full.names = TRUE), files)
  all <- all[file.exists(all) & !dir.exists(all)]
  all[vapply(all, .e3_file_has, NA, key = key)]
}

# After a Cancel killed a real worker the session is settled at once, but the worker's mirai resolves later
# with an error ("Connection reset") and its ExtendedTask warns. Run the event loop here, in the test that
# killed it, until `task` is no longer running, and consume that warning (only that one), so that it does not
# surface in the next test, or as an unhandled promise error once this session is gone. Returns the warnings.
.e3_drain <- function(task, session, secs = 60) {
  seen <- character()
  withCallingHandlers(
    .e3_wait(function() !identical(shiny::isolate(task$status()), "running"), session, secs),
    warning = function(w) {
      if (grepl("ExtendedTask", conditionMessage(w), fixed = TRUE)) {
        seen <<- c(seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    })
  seen
}

.e3_events <- function(dir) sr_events_read(sr_project_paths(dir)$events)

# An archive of the frozen writer project (label v1), written to a fresh temp file.
.e3_srproj <- function(env = parent.frame()) {
  p <- sr_freeze(.fx_writer_project(env), "v1")
  f <- withr::local_tempfile(fileext = ".srproj", .local_envir = env)
  sr_srproj_export(p, f, "v1", "full")
  f
}

test_that("key leak: the sentinel key is in no rendered HTML and no file under the project", {
  skip_on_cran()
  withr::local_options(sr.mode = "airgap", sr.run_sync = TRUE, sr.jev_key = NULL, sr.user = "e3-actor")
  withr::local_envvar(SR_JEV_KEY = NA)
  .ui_spy_notify()
  p <- .fx_writer_project()
  out <- file.path(withr::local_tempdir(), "leak.srproj")
  shiny::testServer(.e3_app(), {
    project(p)
    session$flushReact()
    session$setInputs(`settings-val_jev_key` = .e3_key, `settings-set_jev_key` = 1)
    expect_identical(getOption("sr.jev_key"), .e3_key)
    session$setInputs(`write-check` = 1)
    session$setInputs(`write-preview` = 1)
    session$setInputs(`write-export` = 1)
    session$flushReact()
    expect_true(dir.exists(file.path(project()$paths$out, "exports", "export-001")))
    session$setInputs(`archive-label` = "v1", `archive-freeze` = 1)
    session$setInputs(`archive-confirm` = "FREEZE", `archive-do_freeze` = 1)
    session$flushReact()
    expect_identical(sr_freezes(project())$label, "v1")
    session$setInputs(`archive-ex_label` = "v1", `archive-ex_data` = "full", `archive-ex_path` = out,
                      `archive-export` = 1)
    session$flushReact()
    expect_true(file.exists(out))
    expect_false(isTRUE(project()$readonly))
    # every output rendered by now, as text
    # the defined outputs, module ones prefixed. shiny 1.14 has no public way to list them: MockShinySession
    # only offers getOutput(name), and names(output) gives the proxy's own fields. So this reads the mock
    # session's private list (test-only; revisit if shiny adds a public accessor).
    nms <- names(session$.__enclos_env__$private$outs)
    expect_gt(length(nms), 10L)
    txt <- vapply(nms, function(n) {
      v <- tryCatch(output[[n]], error = function(e) "")
      paste(tryCatch(as.character(unlist(v)), error = function(e) ""), collapse = " ")
    }, "")
    expect_false(any(grepl(.e3_key, txt, fixed = TRUE)))
    zip <- output$`write-dl_export_001`
    expect_true(file.exists(zip))
    expect_length(.e3_leaks(.e3_key, dir = project()$paths$dir, files = c(out, zip)), 0L)
    # the key is still set in this session: nothing cleared it to make the scan pass
    expect_identical(getOption("sr.jev_key"), .e3_key)
    try(sr_duck_close(project()), silent = TRUE)
    try(sr_project_close(project()), silent = TRUE)
  })
})

test_that("real worker: verify with data completes through a mirai worker", {
  skip_on_cran(); skip_if_not_installed("mirai"); skip_if_not_installed("later")
  withr::local_options(sr.mode = "airgap", sr.run_sync = FALSE, sr.user = "e3-actor")
  .ui_spy_notify()
  p <- sr_freeze(.fx_writer_project(), "v1")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_archive_server, args = list(project = rv, busy = function() FALSE), {
    session$flushReact()
    session$setInputs(freeze_pick = "v1", data_verify = 1)
    expect_true(running())
    .e3_wait(function() !running(), session)
    expect_false(running())
    html <- paste(as.character(output$verify_out$html), collapse = " ")
    expect_match(html, "with data files", fixed = TRUE)
    expect_match(html, "Every file matches the freeze", fixed = TRUE)
    expect_false(isTRUE(rv()$readonly))
  })
})

test_that("real worker: a killed .srproj export leaves nothing behind and the project reopens", {
  skip_on_cran(); skip_if_not_installed("mirai"); skip_if_not_installed("later")
  withr::local_options(sr.mode = "airgap", sr.run_sync = FALSE, sr.user = "e3-actor")
  seen <- .ui_spy_notify()
  p <- sr_freeze(.fx_writer_project(), "v1")
  dir <- p$paths$dir
  td <- withr::local_tempdir()
  out <- file.path(td, "killed.srproj")
  staging <- file.path(dir, "out", ".srproj-v1.partial")
  n0 <- .sr_ar_n_events(dir)
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_archive_server, args = list(project = rv, busy = function() FALSE), {
    session$flushReact()
    session$setInputs(ex_label = "v1", ex_data = "full", ex_path = out, export = 1)
    expect_true(running())
    expect_true(isTRUE(rv()$readonly))
    .e3_wait(function() file.exists(job$job_file), session, secs = 120)
    expect_true(file.exists(job$job_file))
    task <- job$task
    session$setInputs(ex_cancel = 1)
    session$flushReact()
    .e3_wait(function() !running(), session, secs = 60)
    expect_false(running())
    w <- .e3_drain(task, session)                          # the killed worker's mirai, settled here
    expect_false(identical(shiny::isolate(task$status()), "running"))
    expect_true(all(grepl("ExtendedTask", w, fixed = TRUE)))
    n1 <- .sr_ar_n_events(dir)
    # exactly two outcomes, never one in between
    kept <- file.exists(out) && n1 == n0 + 1L && identical(.sr_ar_last_type(dir), "srproj.exported")
    gone <- !file.exists(out) && !file.exists(paste0(out, ".partial")) && !dir.exists(staging) && n1 == n0
    expect_true(kept || gone, info = sprintf("archive=%s n0=%d n1=%d", file.exists(out), n0, n1))
    expect_false(file.exists(paste0(out, ".partial")))
    expect_false(dir.exists(staging))
    # no stale lock: the session took the project back and a second open is read-only only while we hold it
    expect_false(isTRUE(rv()$readonly))
    expect_true(file.exists(rv()$paths$lock))
    expect_false(any(seen$type == "error"))
    try(sr_duck_close(rv()), silent = TRUE)
    sr_project_close(rv())
  })
  q <- sr_project_open(dir)
  withr::defer(sr_project_close(q))
  expect_false(isTRUE(q$readonly))
})

test_that("real worker: the actor set in Settings is the actor of the srproj.exported event", {
  skip_on_cran(); skip_if_not_installed("mirai"); skip_if_not_installed("later")
  withr::local_options(sr.mode = "airgap", sr.run_sync = FALSE, sr.user = "role-before", sr.jev_key = NULL)
  withr::local_envvar(SR_JEV_KEY = NA)
  .ui_spy_notify()
  p <- sr_freeze(.fx_writer_project(), "v1")
  dir <- p$paths$dir
  out <- file.path(withr::local_tempdir(), "actor.srproj")
  shiny::testServer(.e3_app(), {
    project(p)
    session$flushReact()
    session$setInputs(`settings-val_user` = "actor-rf5", `settings-set_user` = 1)
    session$setInputs(`settings-val_jev_key` = .e3_key, `settings-set_jev_key` = 1)
    expect_identical(getOption("sr.user"), "actor-rf5")
    # the snapshot a worker gets: the actor, never the key
    o <- .sr_job_opts()
    expect_identical(o[["sr.user"]], "actor-rf5")
    expect_false(grepl(.e3_key, paste(deparse(o), collapse = " "), fixed = TRUE))
    session$setInputs(`archive-ex_label` = "v1", `archive-ex_data` = "full", `archive-ex_path` = out,
                      `archive-export` = 1)
    session$flushReact()
    expect_true(ar$running())
    .e3_wait(function() !ar$running(), session)
    expect_false(ar$running())
    expect_true(file.exists(out))
    ev <- .e3_events(dir)
    last <- ev[[length(ev)]]
    expect_identical(last$type, "srproj.exported")
    expect_identical(last$actor, "actor-rf5")
    # the key reached no file the real worker wrote either
    expect_length(.e3_leaks(.e3_key, dir = dir, files = out), 0L)
    try(sr_duck_close(project()), silent = TRUE)
    try(sr_project_close(project()), silent = TRUE)
  })
})

test_that("real worker: import through a mirai worker opens the project", {
  skip_on_cran(); skip_if_not_installed("mirai"); skip_if_not_installed("later")
  withr::local_options(sr.mode = "airgap", sr.run_sync = FALSE, sr.user = "e3-actor")
  .ui_spy_notify()
  arch <- .e3_srproj()
  dir <- file.path(withr::local_tempdir(), "imported")
  rv <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_import_server, args = list(project = rv, busy = function() FALSE), {
    session$setInputs(archive = arch, target = dir, import = 1)
    expect_true(running())
    .e3_wait(function() !running(), session)
    expect_false(running())
    expect_false(is.null(rv()))
    expect_true(.sr_data_same_dir(rv()$paths$dir, dir))
    expect_false(isTRUE(rv()$readonly))
    expect_match(paste(output$summary$html, collapse = " "), "v1", fixed = TRUE)
    expect_false(any(file.exists(paste0(dir, c(".partial", ".partial.src")))))
    try(sr_duck_close(rv()), silent = TRUE)
    sr_project_close(rv())
  })
})

test_that("a cancelled import never removes a pre-existing target folder", {
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  .ui_stub(".sr_job_kill", function(job_file, wait = 10) TRUE)
  dir <- file.path(withr::local_tempdir(), "imported")
  dir.create(dir)                                          # an empty folder the user chose
  arch <- .e3_srproj()
  rv <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_import_server, args = list(project = rv, busy = function() FALSE), {
    session$setInputs(archive = arch, target = dir, import = 1)
    expect_true(running())
    writeLines("half done", file.path(dir, "left-by-the-job.txt"))
    dir.create(paste0(dir, ".partial"))
    session$setInputs(cancel = 1)
    expect_false(running())
    expect_true(dir.exists(dir))
    expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)
    expect_false(dir.exists(paste0(dir, ".partial")))
    expect_null(rv())
  })
})

test_that("real worker: a killed import leaves the chosen empty folder and no partial", {
  skip_on_cran(); skip_if_not_installed("mirai"); skip_if_not_installed("later")
  withr::local_options(sr.mode = "airgap", sr.run_sync = FALSE, sr.user = "e3-actor")
  .ui_spy_notify()
  arch <- .e3_srproj()
  dir <- file.path(withr::local_tempdir(), "imported")
  dir.create(dir)
  rv <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_import_server, args = list(project = rv, busy = function() FALSE), {
    session$setInputs(archive = arch, target = dir, import = 1)
    expect_true(running())
    .e3_wait(function() file.exists(job$job_file), session, secs = 120)
    task <- job$task
    session$setInputs(cancel = 1)
    session$flushReact()
    .e3_wait(function() !running(), session, secs = 60)
    expect_false(running())
    w <- .e3_drain(task, session)                          # the killed worker's mirai, settled here
    expect_false(identical(shiny::isolate(task$status()), "running"))
    expect_true(all(grepl("ExtendedTask", w, fixed = TRUE)))
    expect_true(dir.exists(dir))
    expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)
    expect_false(any(file.exists(paste0(dir, c(".partial", ".partial.src")))))
    expect_null(rv())
  })
})

# Cancel after the worker has finished the import (its result not yet taken by the session): nothing is killed,
# the job waits for the worker's mirai, and the finished import is kept and opened, as a finished export is kept.
test_that("a cancel after the worker finished keeps the finished import and opens it", {
  skip_on_cran()
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  .ui_stub(".sr_job_kill", function(job_file, wait = 10) FALSE)   # the worker process is already gone
  arch <- .e3_srproj()
  dir <- file.path(withr::local_tempdir(), "imported")
  rv <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_import_server, args = list(project = rv, busy = function() FALSE), {
    session$setInputs(archive = arch, target = dir, import = 1)
    expect_true(running())
    res <- .sr_im_worker(arch, dir, NULL)                  # the worker finishes ...
    expect_true(isTRUE(res$ok))
    session$setInputs(cancel = 1)                          # ... and the user cancels before the session sees it
    expect_true(running())                                 # nothing was killed: wait for the worker's value
    ctl$resolve(res)
    .e3_wait(function() !running(), session, secs = 30)
    expect_false(running())
    expect_true(dir.exists(dir))
    expect_false(is.null(rv()))
    expect_true(.sr_data_same_dir(rv()$paths$dir, dir))
    expect_false(isTRUE(rv()$readonly))
    expect_true(any(seen$msg == "Import already finished; kept"))
    expect_false(any(grepl("cancelled", seen$msg, fixed = TRUE)))
    expect_match(paste(output$summary$html, collapse = " "), "v1", fixed = TRUE)
    try(sr_duck_close(rv()), silent = TRUE)
    sr_project_close(rv())
  })
})

test_that(".sr_im_result_from_disk rebuilds the worker result of a finished import", {
  arch <- .e3_srproj()
  dir <- file.path(withr::local_tempdir(), "imported")
  res <- .sr_im_worker(arch, dir, NULL)
  expect_true(isTRUE(res$ok))
  r2 <- .sr_im_result_from_disk(dir)
  expect_true(r2$ok)
  expect_identical(r2$import$label, res$import$label)
  expect_identical(r2$import$archive_sha256, res$import$archive_sha256)
  expect_true(isTRUE(r2$import$verify$ok))
  q <- sr_project_open(dir)                                # the lock was released
  withr::defer(sr_project_close(q))
  expect_false(isTRUE(q$readonly))
})
