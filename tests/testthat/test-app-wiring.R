# The real app (app/app.R) under testServer: the Data, Design and SAP modules share one project(), so a
# change made through the Data module must reach the SAP lock checklist, and the slow checklist must not be
# computed while no SAP output is being read.

skip_on_cran()

.aw_app <- function() {
  old <- setwd(file.path(normalizePath(testthat::test_path("../.."), winslash = "/"), "app"))
  on.exit(setwd(old))
  shiny::shinyAppDir(".")
}

.aw_blocks <- function(html) {
  s <- paste(html, collapse = " ")
  m <- regmatches(s, regexpr("Lock SAP [(][0-9]+ blocks[)]", s))
  if (length(m)) m else NA_character_
}


test_that("a DQ run and decisions made in the Data tab reach the SAP lock checklist", {
  withr::local_options(sr.mode = "airgap", sr.ingest_sync = TRUE)
  .fx_writer_routes()
  td <- withr::local_tempdir()
  csv <- file.path(td, "t.csv")
  .fx_write_csv(.fx_writer_frame(), csv)
  pdir <- file.path(td, "proj")

  shiny::testServer(.aw_app(), {
    session$setInputs(`project-dir` = pdir, `project-name` = "probe")
    session$setInputs(`project-create` = 1)
    session$setInputs(`data-path` = csv, `data-tname` = "t")
    session$setInputs(`data-import` = 1)
    session$setInputs(`data-table` = "t")
    project(sr_roles_set(project(), "t", c(id = "id", y = "outcome", grp = "exposure", age = "covariate",
                                            z1 = "covariate", time = "time", status = "event")))
    project(sr_sap_add(project(), "a1", "logistic", .ui_bin_spec(), "t", label = "primary"))
    project(.fx_routed(project()))
    session$flushReact()

    n0 <- sum(sr_sap_check(project())$status == "block")
    expect_gt(n0, 0L)
    expect_identical(.aw_blocks(output$`sap-lock_btn`$html), sprintf("Lock SAP (%d blocks)", n0))
    expect_match(paste(output$`sap-check`, collapse = " "), "no DQ run recorded", fixed = TRUE)

    session$setInputs(`data-dq_run` = 1)
    session$flushReact()

    n1 <- sum(sr_sap_check(project())$status == "block")
    expect_lt(n1, n0)
    expect_identical(.aw_blocks(output$`sap-lock_btn`$html), sprintf("Lock SAP (%d blocks)", n1))
    expect_false(grepl("no DQ run recorded", paste(output$`sap-check`, collapse = " "), fixed = TRUE))
    try(sr_project_close(project()), silent = TRUE)
  })
})

# testServer evaluates every output on each flush (it has no hidden tabs), so "no computation while the SAP tab
# is hidden" cannot be driven here. This pins the mechanism instead: the checklist is a lazy reactive, not an
# observe-based flag (which would run sr_sap_check on every project change made in any tab).
test_that("the lock checklist is a lazy reactive, not an observer-driven flag", {
  src <- paste(readLines(file.path(normalizePath(testthat::test_path("../.."), winslash = "/"),
                                   "app", "R", "ui", "mod_sap.R"), warn = FALSE), collapse = "
")
  expect_match(src, "chk <- shiny::reactive(.sr_sap_check_view(project()))", fixed = TRUE)
  expect_false(grepl("chk <- .sr_sap_flag", src, fixed = TRUE))
})

test_that("the Analyses tab is wired and a run blocks Data, Design and SAP edits", {
  skip_if_not_installed("later")
  withr::local_options(sr.mode = "airgap", sr.run_sync = FALSE)
  seen <- .ui_spy_notify()
  old <- get(".sr_an_spawn", envir = globalenv())
  ctl <- new.env()
  assign(".sr_an_spawn", function(app_dir, project_dir, id, job, progress_file, job_file, cancel_file) {
    promises::promise(function(resolve, reject) ctl$reject <- reject)   # pending until Cancel kills it
  }, envir = globalenv())
  withr::defer(assign(".sr_an_spawn", old, envir = globalenv()))
  oldk <- get(".sr_an_kill", envir = globalenv())
  assign(".sr_an_kill", function(job_file, wait = 10) {
    ctl$reject(simpleError("19 | Connection reset"))     # what a killed worker's mirai gives
    TRUE
  }, envir = globalenv())
  withr::defer(assign(".sr_an_kill", oldk, envir = globalenv()))
  p <- .fx_an_project()
  shiny::testServer(.aw_app(), {
    project(p)
    session$flushReact()
    expect_false(isTRUE(shiny::isolate(busy_for()())))
    session$setInputs(`analyses-run_all` = 1)
    expect_true(a$running())
    expect_true(shiny::isolate(busy_for()()))
    expect_false(isTRUE(d$busy()))                          # the import flag is untouched
    expect_match(paste(output$`analyses-badge`$html), "running", fixed = TRUE)
    n_ev <- function() length(sr_events_read(project()$paths$events))
    n0 <- n_ev()
    session$setInputs(`data-path` = "x.csv", `data-import` = 1)
    expect_true(any(grepl(paste0("Import: ", .sr_ui_busy_text), seen$msg, fixed = TRUE)))
    session$setInputs(`design-pico_population` = "a", `design-pico_outcome` = "b", `design-pico_save` = 1)
    expect_true(any(grepl(paste0("Save research question: ", .sr_ui_busy_text), seen$msg, fixed = TRUE)))
    n_sap <- sum(grepl(.sr_ui_busy_text, seen$msg, fixed = TRUE))
    session$setInputs(`sap-add` = 1)
    expect_gt(sum(grepl(.sr_ui_busy_text, seen$msg, fixed = TRUE)), n_sap)
    expect_equal(n_ev(), n0)                                # the refused edits wrote nothing
    session$setInputs(`analyses-cancel` = 1)
    t0 <- Sys.time()
    while (isTRUE(a$running()) && as.numeric(difftime(Sys.time(), t0, units = "secs")) < 30) {
      later::run_now(0.2)
      session$flushReact()
    }
    expect_false(a$running())
    expect_false(shiny::isolate(busy_for()()))
    try(sr_project_close(project()), silent = TRUE)
  })
})

test_that("the Archive tab is wired and an archive job blocks Data, Design, SAP and Analyses edits", {
  skip_if_not_installed("later")
  withr::local_options(sr.mode = "airgap", sr.run_sync = FALSE)
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ui_stub(".sr_job_spawn", .ui_pending_spawn(ctl))
  .ui_stub(".sr_job_kill", function(job_file, wait = 10) TRUE)
  p <- sr_freeze(.fx_writer_project(), "v1")
  out <- file.path(withr::local_tempdir(), "a.srproj")
  shiny::testServer(.aw_app(), {
    project(p)
    session$flushReact()
    expect_false(isTRUE(shiny::isolate(busy_for()())))
    session$setInputs(`archive-ex_label` = "v1", `archive-ex_data` = "full", `archive-ex_path` = out,
                      `archive-export` = 1)
    session$flushReact()
    expect_true(ar$running())
    expect_true(shiny::isolate(busy_for()()))
    expect_false(isTRUE(shiny::isolate(busy_for("archive")())))   # the archive module itself is not blocked
    n_ev <- function() length(sr_events_read(project()$paths$events))
    n0 <- n_ev()
    refused <- function(label, ...) {
      n <- length(seen$msg)
      session$setInputs(...)
      session$flushReact()
      new <- seen$msg[seq_along(seen$msg) > n]
      expect_true(any(startsWith(new, paste0(label, ": ")) & endsWith(new, .sr_ui_busy_text)),
                  info = label)
    }
    refused("Import", `data-path` = "x.csv", `data-import` = 1)
    refused("Save research question", `design-pico_population` = "a", `design-pico_outcome` = "b",
            `design-pico_save` = 1)
    n_sap <- sum(grepl(.sr_ui_busy_text, seen$msg, fixed = TRUE))
    session$setInputs(`sap-add` = 1)
    expect_gt(sum(grepl(.sr_ui_busy_text, seen$msg, fixed = TRUE)), n_sap)
    refused("Run", `analyses-run_all` = 1)
    expect_equal(n_ev(), n0)                                # the refused edits wrote nothing
    session$setInputs(`archive-ex_cancel` = 1)
    session$flushReact()
    expect_false(ar$running())
    expect_false(shiny::isolate(busy_for()()))
    expect_false(isTRUE(project()$readonly))
    try(sr_project_close(project()), silent = TRUE)
  })
})
