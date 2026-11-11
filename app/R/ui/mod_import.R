# ui/mod_import.R - Shiny module: import a .srproj archive into a new project folder (Project tab).
#
# mod_import_server(id, project, busy) takes the project reactiveVal of mod_project_server() and `busy` (a
# reactive that is TRUE while any OTHER job runs; app.R passes busy_for("import")). It returns list(running):
# TRUE while the import job runs.
#
# The import runs in a background worker (.sr_im_worker, started by .sr_job_spawn with the options of
# .sr_job_opts(); the Jev key never reaches the worker). option sr.run_sync = TRUE calls the worker inline
# instead (tests). Before the job starts the module records which of <dir>, <dir>.partial and
# <dir>.partial.src already exist; a failed or cancelled import removes only what the job itself created
# (.sr_im_cleanup). On success the project that was open is closed and the imported one becomes project().
# Cancel creates a cancel file the worker checks (.sr_im_worker) before it kills the worker; when no worker
# can be killed yet, the job settles (as a cancel) only when the worker's mirai resolves.
# Every exit path (success, error, cancel, a rejected job) releases the busy flag.

mod_import_ui <- function(id) {
  ns <- shiny::NS(id)
  .sr_ui_card("Import .srproj",
    shiny::textInput(ns("archive"), "Archive (a .srproj file)", value = "", width = "100%"),
    shiny::textInput(ns("target"), "Target folder (new, or an empty folder)", value = "", width = "100%"),
    shiny::div(class = "d-flex gap-2 mb-2",
      shiny::actionButton(ns("import"), "Import", class = "btn-primary"),
      shiny::actionButton(ns("cancel"), "Cancel", class = "btn-outline-danger")
    ),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("summary"))
  )
}

mod_import_server <- function(id, project, busy) {
  shiny::moduleServer(id, function(input, output, session) {
    job <- new.env(parent = emptyenv())
    active <- shiny::reactiveVal(FALSE)
    running <- shiny::reactive(isTRUE(active()))
    summary <- shiny::reactiveVal(NULL)
    busy_now <- function() isTRUE(tryCatch(shiny::isolate(busy()), error = function(e) FALSE))
    blocked <- function() isTRUE(shiny::isolate(active())) || busy_now()

    output$status <- shiny::renderUI({
      if (!isTRUE(running())) return(NULL)
      shiny::p(class = "text-info small", shiny::tags$span(class = "spinner-border spinner-border-sm me-1"),
               "Importing...")
    })
    output$summary <- shiny::renderUI({
      s <- summary()
      if (is.null(s)) return(NULL)
      shiny::tagList(shiny::h6("Imported"), .sr_wm_table(s))
    })

    # The problem with the typed paths, as the core words it, or NULL.
    path_problem <- function(arch, dir) {
      if (is.null(arch) || !file.exists(arch) || dir.exists(arch)) {
        return(paste0("archive not found: ", arch %||% ""))
      }
      if (is.null(dir)) return("the target folder must be a path")
      if (file.exists(dir) && (!dir.exists(dir) || length(list.files(dir, all.files = TRUE, no.. = TRUE)))) {
        return(paste0("the target folder is not empty: ", dir))
      }
      parent <- dirname(dir)
      if (!dir.exists(parent)) return(paste0("the folder ", parent, " does not exist"))
      for (sfx in c(".partial", ".partial.src")) {
        if (file.exists(paste0(dir, sfx))) {
          return(paste0(basename(dir), sfx, " already exists; remove it first"))
        }
      }
      NULL
    }

    # The job is done: tear it down, clean up after a failure, open the project after a success, report.
    # `res` is the worker's value or a condition; `cancelled` is TRUE when the user asked to cancel.
    settle <- function(res, cancelled) {
      dir <- job$dir
      before <- job$before
      jf <- job$job_file
      cf <- job$cancel_file
      # whatever happens below, the job is released: the busy flag drops and the job env is emptied
      on.exit({
        for (nm in ls(job, all.names = TRUE)) rm(list = nm, envir = job)
        active(FALSE)
      }, add = TRUE)
      tryCatch({
        if (!is.null(job$obs)) job$obs$destroy()
        job$obs <- job$task <- NULL
        if (.sr_job_path_ok(jf)) try(unlink(c(jf, paste0(jf, ".part"))), silent = TRUE)
        if (.sr_job_path_ok(cf)) try(unlink(cf), silent = TRUE)
        good <- !cancelled && is.list(res) && !inherits(res, "condition") && isTRUE(res$ok)
        # a cancel that came after the worker had finished: its last step (the srproj.imported event) is on
        # disk, so the import is kept and opened (as an export the worker finished is kept)
        kept <- FALSE
        if (cancelled && !good && identical(.sr_ar_last_type(dir), "srproj.imported")) {
          res <- tryCatch(.sr_im_result_from_disk(dir), error = function(e) NULL)
          if (!is.null(res)) good <- kept <- TRUE
        }
        if (good) {
          q <- .sr_ui_try("Import", function() sr_project_open(dir))
          if (is.null(q)) {
            tryCatch(.sr_im_cleanup(dir, before), error = function(e) NULL)
          } else {
            old <- shiny::isolate(project())
            if (!is.null(old)) {
              try(sr_duck_close(old), silent = TRUE)
              try(sr_project_close(old), silent = TRUE)
            }
            project(q)
            summary(.sr_im_summary(res$import))
            .sr_ui_notify(if (kept) "Import already finished; kept" else paste0("Imported ", dir), "message")
          }
        } else {
          tryCatch(.sr_im_cleanup(dir, before), error = function(e) {
            .sr_ui_notify(paste0("Import cleanup: ", conditionMessage(e)), "error")
          })
          if (cancelled) {
            .sr_ui_notify("Import cancelled: nothing was imported", "message")
          } else {
            .sr_ui_notify(paste0("Import: ", .sr_am_error_text(res)), "error")
          }
        }
      }, error = function(e) {
        try(.sr_ui_notify(paste0("Import: ", conditionMessage(e)), "error"), silent = TRUE)
      })
      invisible(NULL)
    }

    launch <- function(args) {
      if (isTRUE(as.logical(sr_opt("run_sync", FALSE)))) {
        res <- tryCatch(do.call(".sr_im_worker", args), error = function(e) e)
        return(settle(res, FALSE))
      }
      task <- shiny::ExtendedTask$new(function(...) .sr_job_spawn(...))
      job$task <- task
      job$cancelling <- FALSE
      job$obs <- shiny::observeEvent(task$status(), {
        st <- task$status()
        if (!(st %in% c("success", "error"))) return(invisible(NULL))
        res <- tryCatch(task$result(), error = function(e) e)
        # a worker that stopped at the cancel file is a cancel even if a later kill error reset the flag
        settle(res, isTRUE(job$cancelling) || (is.list(res) && isTRUE(res$cancelled)))
      }, ignoreInit = TRUE)
      started <- .sr_ui_try("Import", function() {
        task$invoke(app_dir = sr_root, fn = ".sr_im_worker", args = args, opts = .sr_job_opts())
        TRUE
      })
      if (!isTRUE(started)) settle(simpleError("the worker could not be started"), TRUE)
      invisible(NULL)
    }

    shiny::observeEvent(input$import, {
      if (blocked()) return(.sr_ui_notify(paste0("Import: ", .sr_ui_busy_text), "error"))
      arch <- .sr_am_path(input$archive)
      dir <- .sr_am_path(input$target)
      if (!is.null(dir)) dir <- sub("[/\\]+$", "", dir)
      why <- path_problem(arch, dir)
      if (!is.null(why)) return(.sr_ui_notify(paste0("Import: ", why), "error"))
      summary(NULL)
      jf <- tempfile("sr-im-job-")
      cf <- tempfile("sr-im-cancel-")
      job$dir <- dir
      job$job_file <- jf
      job$cancel_file <- cf
      job$before <- .sr_im_snapshot(dir)                 # before anything is created
      active(TRUE)
      launch(list(arch, dir, jf, cf))
    })

    # Cancel creates the cancel file first, then kills the worker recorded in the job file. When there is no
    # worker to kill (it has not recorded itself yet, or it is already done) the job is NOT settled here: the
    # worker stops at the cancel file (or has finished), and its mirai is settled as a cancel when it resolves,
    # so an import can never complete after "Import cancelled".
    shiny::observeEvent(input$cancel, {
      if (!isTRUE(shiny::isolate(active()))) return(.sr_ui_notify("Cancel: nothing is running", "message"))
      job$cancelling <- TRUE
      if (.sr_job_path_ok(job$cancel_file)) try(file.create(job$cancel_file), silent = TRUE)
      k <- tryCatch(.sr_job_kill(job$job_file), error = function(e) e)
      if (inherits(k, "error")) {
        # the kill failed: a process may still be running, so the job stays (and can be cancelled again)
        job$cancelling <- FALSE
        if (.sr_job_path_ok(job$cancel_file)) try(unlink(job$cancel_file), silent = TRUE)
        return(.sr_ui_notify(paste0("Cancel: ", conditionMessage(k)), "error"))
      }
      # TRUE: the worker was killed, nothing runs any more
      if (isTRUE(k)) return(settle(NULL, TRUE))
      .sr_ui_notify("Cancelling: no running import worker was found; the import is settled when its worker ends",
                    "message")
    })

    # Closing the session mid-job kills the worker and removes what it created (never a path that existed
    # before it). It never raises; a worker that could not be killed is left alone. The cancel file stops a
    # worker that has not recorded itself yet.
    session$onSessionEnded(function() {
      try({
        if (isTRUE(shiny::isolate(active())) && !is.null(job$job_file)) {
          if (.sr_job_path_ok(job$cancel_file)) try(file.create(job$cancel_file), silent = TRUE)
          k <- tryCatch(.sr_job_kill(job$job_file), error = function(e) e)
          if (!inherits(k, "error")) .sr_im_cleanup(job$dir, job$before)
        }
      }, silent = TRUE)
    })

    list(running = running)
  })
}
