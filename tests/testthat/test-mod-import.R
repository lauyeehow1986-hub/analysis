# Tests for the Import module (ui/mod_import.R) and its worker helpers (ui/archive_job.R). Jobs run inline
# (option sr.run_sync = TRUE) unless a test says otherwise; the background tests replace .sr_job_spawn with a
# pending promise (helper-ui.R: .ui_pending_spawn) and .sr_job_kill with a stub.

.im_cache <- new.env()

# A .srproj of the frozen writer project (label v1), built once per file.
.im_archive <- function() {
  if (is.null(.im_cache$path) || !file.exists(.im_cache$path)) {
    p <- sr_freeze(.fx_writer_project(), "v1")
    f <- tempfile("im-", fileext = ".srproj")
    sr_srproj_export(p, f, "v1", "full")
    .im_cache$path <- f
  }
  .im_cache$path
}

.im_args <- function(rv, busy = FALSE) list(project = rv, busy = shiny::reactive(busy))

.im_paths <- function(dir) c(dir, paste0(dir, ".partial"), paste0(dir, ".partial.src"))

test_that("import cleanup: removes only the paths the job created", {
  td <- withr::local_tempdir()
  dir <- file.path(td, "proj")
  before <- .sr_im_snapshot(dir)
  expect_identical(names(before), c("dir", "partial", "src"))
  expect_false(any(before))
  for (x in .im_paths(dir)) dir.create(x)
  expect_true(all(.sr_im_snapshot(paste0(dir, "/"))))
  gone <- .sr_im_cleanup(dir, before)
  expect_length(gone, 3L)
  expect_false(any(file.exists(.im_paths(dir))))
})

test_that("import cleanup: a target folder that existed empty before the job is kept", {
  td <- withr::local_tempdir()
  dir <- file.path(td, "proj")
  dir.create(dir)
  before <- .sr_im_snapshot(dir)
  expect_identical(unname(before), c(TRUE, FALSE, FALSE))
  dir.create(paste0(dir, ".partial"))
  dir.create(paste0(dir, ".partial.src"))
  .sr_im_cleanup(dir, before)
  expect_true(dir.exists(dir))
  expect_false(dir.exists(paste0(dir, ".partial")))
  expect_false(dir.exists(paste0(dir, ".partial.src")))
  # a killed worker that had already moved a project into it, or removed it to rename over it
  writeLines("x", file.path(dir, "left-by-the-job.txt"))
  .sr_im_cleanup(dir, before)
  expect_true(dir.exists(dir))
  expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)
  unlink(dir, recursive = TRUE)
  .sr_im_cleanup(dir, before)
  expect_true(dir.exists(dir))
})

test_that("import cleanup: a pre-existing <dir>.partial is never removed", {
  td <- withr::local_tempdir()
  dir <- file.path(td, "proj")
  dir.create(paste0(dir, ".partial"))
  writeLines("mine", file.path(paste0(dir, ".partial"), "keep.txt"))
  before <- .sr_im_snapshot(dir)
  expect_identical(unname(before), c(FALSE, TRUE, FALSE))
  dir.create(paste0(dir, ".partial.src"))
  dir.create(dir)
  .sr_im_cleanup(dir, before)
  expect_true(file.exists(file.path(paste0(dir, ".partial"), "keep.txt")))
  expect_false(dir.exists(paste0(dir, ".partial.src")))
  expect_false(dir.exists(dir))
})

test_that("import opens the project and shows the summary", {
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  arch <- .im_archive()
  dir <- file.path(withr::local_tempdir(), "imported")
  rv <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_import_server, args = .im_args(rv), {
    session$setInputs(archive = arch, target = dir, import = 1)
    expect_false(running())
    expect_false(is.null(rv()))
    expect_true(.sr_data_same_dir(rv()$paths$dir, dir))
    expect_false(isTRUE(rv()$readonly))
    html <- paste(output$summary$html, collapse = " ")
    expect_match(html, "v1", fixed = TRUE)
    expect_match(html, substr(sr_sha256_file(arch), 1L, 16L), fixed = TRUE)
    expect_length(rv()$state$srproj_imports, 1L)
    try(sr_duck_close(rv()), silent = TRUE)
    sr_project_close(rv())
  })
})

test_that(".sr_im_summary reads the import list", {
  imp <- list(label = "v1", data_mode = "lean", archive_sha256 = strrep("ab", 32), parts_missing = c("a", "b"),
              sources_missing = character(), verify = list(ok = TRUE, problems = character()))
  s <- .sr_im_summary(imp)
  expect_identical(names(s), c("field", "value"))
  expect_identical(s$value[s$field == "label"], "v1")
  expect_identical(s$value[s$field == "archive sha256"], strrep("ab", 8))
  expect_identical(s$value[s$field == "parts missing"], "a, b")
  expect_identical(s$value[s$field == "sources missing"], "none")
  expect_identical(s$value[s$field == "verify"], "ok")
})

test_that("a failed import keeps the previous project open", {
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  bad <- tempfile("im-bad-", fileext = ".srproj")
  withr::defer(unlink(bad))
  writeBin(readBin(.im_archive(), "raw", 1000L), bad)
  td <- withr::local_tempdir()
  dir <- file.path(td, "imported")
  core <- tryCatch(sr_srproj_import(bad, file.path(td, "probe")), error = function(e) conditionMessage(e))
  expect_type(core, "character")
  unlink(file.path(td, c("probe", "probe.partial", "probe.partial.src")), recursive = TRUE)
  p0 <- .fx_writer_project()
  rv <- shiny::reactiveVal(p0)
  shiny::testServer(mod_import_server, args = .im_args(rv), {
    session$setInputs(archive = bad, target = dir, import = 1)
    expect_false(running())
    expect_identical(rv(), p0)
    expect_true(any(grepl(core, seen$msg, fixed = TRUE)))
    expect_false(any(file.exists(.im_paths(dir))))
  })
})

test_that("the paths are checked before any job starts", {
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  td <- withr::local_tempdir()
  full <- file.path(td, "full")
  dir.create(full)
  writeLines("x", file.path(full, "f.txt"))
  rv <- shiny::reactiveVal(NULL)
  arch <- .im_archive()
  shiny::testServer(mod_import_server, args = .im_args(rv), {
    session$setInputs(archive = file.path(td, "none.srproj"), target = file.path(td, "new"), import = 1)
    session$setInputs(archive = arch, target = full, import = 2)
    session$setInputs(archive = arch, target = file.path(td, "nofolder", "new"), import = 3)
    session$setInputs(archive = arch, target = "", import = 4)
    expect_false(running())
    expect_length(ctl$calls, 0L)
    expect_true(any(grepl("archive not found", seen$msg, fixed = TRUE)))
    expect_true(any(grepl("the target folder is not empty", seen$msg, fixed = TRUE)))
    expect_true(any(grepl("does not exist", seen$msg, fixed = TRUE)))
  })
})

test_that("a cancelled import removes what it created and keeps the previous project", {
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  .ui_stub(".sr_job_kill", function(job_file, wait = 10) TRUE)
  td <- withr::local_tempdir()
  dir <- file.path(td, "imported")
  dir.create(paste0(dir, ".partial.src"))                 # there before the click: never removed
  p0 <- .fx_writer_project()
  rv <- shiny::reactiveVal(p0)
  arch <- .im_archive()
  shiny::testServer(mod_import_server, args = .im_args(rv), {
    session$setInputs(archive = arch, target = dir, import = 1)
    expect_false(running())                               # refused: the pre-existing .partial.src blocks it
  })
  unlink(paste0(dir, ".partial.src"), recursive = TRUE)
  shiny::testServer(mod_import_server, args = .im_args(rv), {
    session$setInputs(archive = arch, target = dir, import = 2)
    expect_true(running())
    expect_length(ctl$calls, 1L)
    expect_identical(ctl$calls[[1L]]$fn, ".sr_im_worker")
    dir.create(paste0(dir, ".partial"))                   # what the half-done worker leaves behind
    session$setInputs(cancel = 1)
    expect_false(running())
    expect_false(dir.exists(paste0(dir, ".partial")))
    expect_identical(rv(), p0)
  })
})

test_that("a cancel whose kill raises stays cancellable; a rejected job releases the flag", {
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  .ui_stub(".sr_job_kill", function(job_file, wait = 10) stop("still running"))
  td <- withr::local_tempdir()
  dir <- file.path(td, "imported")
  rv <- shiny::reactiveVal(NULL)
  arch <- .im_archive()
  shiny::testServer(mod_import_server, args = .im_args(rv), {
    session$setInputs(archive = arch, target = dir, import = 1)
    expect_true(running())
    session$setInputs(cancel = 1)
    expect_true(running())
    expect_true(any(grepl("still running", seen$msg, fixed = TRUE)))
    ctl$reject(simpleError("the worker died"))
    for (i in 1:50) {
      later::run_now(0.1)
      session$flushReact()
      if (!running()) break
    }
    expect_false(running())
    expect_true(any(grepl("the worker died", seen$msg, fixed = TRUE)))
    expect_false(any(file.exists(.im_paths(dir))))
  })
})

test_that("import is refused while another job runs", {
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  dir <- file.path(withr::local_tempdir(), "imported")
  rv <- shiny::reactiveVal(NULL)
  arch <- .im_archive()
  shiny::testServer(mod_import_server, args = .im_args(rv, busy = TRUE), {
    session$setInputs(archive = arch, target = dir, import = 1)
    expect_false(running())
    expect_length(ctl$calls, 0L)
    expect_true(any(grepl(.sr_ui_busy_text, seen$msg, fixed = TRUE)))
  })
})
