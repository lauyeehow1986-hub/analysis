# ui/mod_archive.R - Shiny module: the Archive tab. Cards: Freeze, Freezes and verify, Diff, Export .srproj,
# Re-baseline.
#
# mod_archive_server(id, project, busy, settings = function() 0L) takes the project reactiveVal of
# mod_project_server(), `busy` (a reactive that is TRUE while any OTHER job runs; app.R passes
# busy_for("archive")) and `settings`, a function whose value changes when the settings change (it only
# re-renders the export plan). It returns list(running): TRUE while a verify or export job runs.
#
# One job at a time (kind "verify" or "export"), held in the session environment `job`. Each job gets its own
# shiny::ExtendedTask around .sr_job_spawn(); the worker options come from .sr_job_opts() (the actor and the
# digits edited in Settings apply to events written by the worker; the Jev key is never in the snapshot).
# option sr.run_sync = TRUE calls the worker inline instead (tests).
#
# Verify with data opens the project read-only in the worker and keeps the session's lock. An export needs
# the lock: the session releases it, keeps a read-only copy as project() while the worker runs and re-opens
# the project when the job ends. Cancel kills the worker (.sr_job_kill) and settles at once: unless the
# srproj.exported event was written, the archive, its .partial and the staging folder are removed
# (.sr_ar_export_cleanup) - never one that existed before the job started.
#
# Every number shown comes from the core; the panel only formats frames.

# ---- small helpers -----------------------------------------------------------------------------------

# The destination path of an export from its text input: the quotes Windows adds when a path is copied are
# stripped; NULL when empty.
.sr_am_path <- function(x) {
  d <- .sr_ui_text(x)
  if (nchar(d) >= 2L && startsWith(d, "\"") && endsWith(d, "\"")) d <- trimws(substr(d, 2L, nchar(d) - 1L))
  if (!nzchar(d)) return(NULL)
  normalizePath(path.expand(d), winslash = "/", mustWork = FALSE)
}

# The paths an export may create that already exist (normalized): the cleanup after a cancel keeps them.
.sr_am_existing <- function(dir, path, label) {
  cand <- c(path, paste0(path, ".partial"), file.path(dir, "out", paste0(".srproj-", label, ".partial")))
  cand <- cand[file.exists(cand)]
  normalizePath(cand, winslash = "/", mustWork = FALSE)
}

# A byte count as "12.3 Kb" (display only).
.sr_am_size <- function(bytes) {
  format(structure(as.numeric(bytes), class = "object_size"), units = "auto")
}

# The message of a worker value that is not a good result: an error condition, a list(ok = FALSE, error) or
# anything else.
.sr_am_error_text <- function(res) {
  if (inherits(res, "condition")) return(conditionMessage(res))
  if (is.list(res) && identical(res$ok, FALSE) && is.character(res$error)) return(res$error[1L])
  .sr_job_res_message(res)
}

# A data frame as a plain HTML table; NULL or zero rows gives a short note.
.sr_am_table <- function(df) {
  if (is.null(df) || !NROW(df)) return(shiny::p(class = "text-muted small", "none"))
  .sr_wm_table(df)
}

# ---- UI ----------------------------------------------------------------------------------------------

mod_archive_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "sr-archive-stack",
    .sr_ui_card("Freeze",
      shiny::uiOutput(ns("gates")),
      shiny::textInput(ns("label"), "Freeze label", value = ""),
      shiny::actionButton(ns("freeze"), "Freeze...", class = "btn-primary")
    ),
    .sr_ui_card("Freezes and verify",
      DT::DTOutput(ns("freezes"), fill = FALSE),
      shiny::selectInput(ns("freeze_pick"), "Freeze", choices = character()),
      shiny::div(class = "d-flex gap-2 mb-2",
        shiny::actionButton(ns("quick_verify"), "Quick verify", class = "btn-outline-primary"),
        shiny::actionButton(ns("data_verify"), "Verify with data", class = "btn-outline-primary"),
        shiny::actionButton(ns("cancel"), "Cancel", class = "btn-outline-danger")
      ),
      shiny::uiOutput(ns("job_status")),
      shiny::uiOutput(ns("verify_out"))
    ),
    .sr_ui_card("Diff",
      shiny::div(class = "d-flex gap-3 align-items-end",
        shiny::selectInput(ns("diff_from"), "From", choices = character()),
        shiny::selectInput(ns("diff_to"), "To", choices = c("project now" = "now")),
        shiny::actionButton(ns("diff"), "Show diff", class = "btn-outline-primary mb-3")
      ),
      shiny::uiOutput(ns("diff_out"))
    ),
    .sr_ui_card("Export .srproj",
      shiny::selectInput(ns("ex_label"), "Freeze", choices = character()),
      shiny::radioButtons(ns("ex_data"), "Data files", inline = TRUE,
                          choices = c("auto", "full", "lean"), selected = "auto"),
      shiny::textInput(ns("ex_path"), "Destination (a new .srproj file)", value = "", width = "100%"),
      shiny::uiOutput(ns("ex_plan")),
      shiny::div(class = "d-flex gap-2",
        shiny::actionButton(ns("export"), "Export", class = "btn-primary"),
        shiny::actionButton(ns("ex_cancel"), "Cancel", class = "btn-outline-danger")
      )
    ),
    .sr_ui_card("Re-baseline",
      shiny::uiOutput(ns("rb_diff")),
      shiny::textInput(ns("rb_reason"), "Reason", value = "", width = "100%"),
      shiny::actionButton(ns("rebaseline"), "Re-baseline", class = "btn-outline-primary")
    )
  )
}

# ---- server ------------------------------------------------------------------------------------------

mod_archive_server <- function(id, project, busy, settings = function() 0L) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- the live job (transient: nothing here is project state) ---------------------------------------
    job <- new.env(parent = emptyenv())
    active <- shiny::reactiveVal(FALSE)
    running <- shiny::reactive(isTRUE(active()))
    busy_now <- function() isTRUE(tryCatch(shiny::isolate(busy()), error = function(e) FALSE))
    blocked <- function() isTRUE(shiny::isolate(active())) || busy_now()
    act <- .sr_ui_actor(project, blocked)

    # ---- what the project holds (value-compared so a typed field is not wiped) ---------------------------
    labels <- .sr_ui_flag(project, function(p) if (is.null(p)) character() else sr_freezes(p)$label)
    next_label <- .sr_ui_flag(project, function(p) if (is.null(p)) "" else sr_freeze_next_label(p))
    latest <- function() {
      l <- shiny::isolate(labels())
      if (length(l)) l[length(l)] else ""
    }
    # a freeze chosen in a select, or the latest one while the select has no value yet
    pick <- function(x) {
      v <- .sr_ui_text(x)
      if (nzchar(v)) v else latest()
    }

    shiny::observe(shiny::updateTextInput(session, "label", value = next_label()))
    shiny::observe({
      l <- labels()
      last <- if (length(l)) l[length(l)] else NULL
      for (s in c("freeze_pick", "diff_from", "ex_label")) {
        shiny::updateSelectInput(session, s, choices = l, selected = last)
      }
      shiny::updateSelectInput(session, "diff_to", choices = c(stats::setNames(l, l), "project now" = "now"),
                               selected = "now")
    })

    # ---- Freeze --------------------------------------------------------------------------------------
    gates <- shiny::reactive({
      p <- project()
      # a worker may be writing the project: do not verify it meanwhile
      if (is.null(p) || isTRUE(running())) return(NULL)
      .sr_ar_freeze_gates(p)
    })
    output$gates <- shiny::renderUI({
      g <- gates()
      if (is.null(g)) {
        return(shiny::p(class = "text-muted small", if (isTRUE(running())) "A job is running." else "No project."))
      }
      shiny::tags$ul(class = "list-unstyled small", lapply(seq_len(nrow(g)), function(i) {
        shiny::tags$li(class = if (g$ok[i]) "text-success" else "text-danger",
                       if (g$ok[i]) g$gate[i] else paste0(g$gate[i], ": ", g$reason[i]))
      }))
    })

    pending <- shiny::reactiveVal(NULL)   # the label the open dialog will freeze
    shiny::observeEvent(input$freeze, {
      p <- shiny::isolate(project())
      why <- .sr_ui_refusal(p, blocked)
      if (is.null(why)) why <- .sr_ar_label_problem(p, .sr_ui_text(input$label))
      if (!is.null(why)) {
        pending(NULL)
        return(.sr_ui_notify(paste0("Freeze: ", why), "error"))
      }
      lab <- .sr_ui_text(input$label)
      pending(lab)
      try(shiny::showModal(shiny::modalDialog(
        title = paste0("Freeze as '", lab, "'"),
        shiny::p("A freeze records the SAP, the stored results and the library, and cannot be undone.",
                 "Type FREEZE to confirm."),
        shiny::textInput(ns("confirm"), NULL, value = "", placeholder = "FREEZE"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("do_freeze"), "Freeze", class = "btn-primary", disabled = NA)
        ), easyClose = TRUE)), silent = TRUE)
    })
    shiny::observe({
      shiny::updateActionButton(session, "do_freeze", disabled = !identical(input$confirm, "FREEZE"))
    })
    shiny::observeEvent(input$do_freeze, {
      lab <- shiny::isolate(pending())
      if (!identical(shiny::isolate(input$confirm), "FREEZE") || is.null(lab)) {
        return(.sr_ui_notify("Freeze: type FREEZE to confirm", "error"))
      }
      pending(NULL)
      try(shiny::removeModal(), silent = TRUE)
      r <- act("Freeze", function(p) {
        why <- .sr_ar_label_problem(p, lab)
        if (!is.null(why)) stop(why, call. = FALSE)
        sr_freeze(p, lab)
      })
      if (!is.null(r)) .sr_ui_notify(sprintf("Frozen as '%s'", lab), "message")
    })

    # ---- Freezes -------------------------------------------------------------------------------------
    output$freezes <- DT::renderDT({
      p <- project()
      df <- if (is.null(p)) {
        data.frame(label = character(), created = character(), events_seq = integer(), libsnap = character())
      } else {
        .sr_ar_freezes(p)
      }
      .sr_ui_dt(df)
    }, server = FALSE)

    verify_res <- shiny::reactiveVal(NULL)
    show_verify <- function(label, data, v) {
      verify_res(list(label = label, data = data, rows = .sr_ar_verify_rows(v)))
    }
    output$verify_out <- shiny::renderUI({
      s <- verify_res()
      if (is.null(s)) return(NULL)
      r <- s$rows
      shiny::tagList(
        shiny::h6(sprintf("Verification of '%s' (%s)", s$label,
                          if (isTRUE(s$data)) "with data files" else "without data files")),
        if (isTRUE(r$ok)) {
          shiny::p(class = "text-success", shiny::HTML(.sr_ui_badge("ok")), " Every file matches the freeze.")
        } else {
          shiny::tags$ul(class = "text-danger", lapply(r$problems, shiny::tags$li))
        },
        if (length(r$absent)) {
          shiny::div(class = "small text-muted",
                     "Data files not in the project folder (information, not a problem):",
                     shiny::tags$ul(lapply(r$absent, shiny::tags$li)))
        }
      )
    })
    output$job_status <- shiny::renderUI({
      if (!isTRUE(running())) return(NULL)
      shiny::p(class = "text-info small", shiny::tags$span(class = "spinner-border spinner-border-sm me-1"),
               sprintf("Running: %s of '%s'", job$kind %||% "job", job$label %||% ""))
    })

    shiny::observeEvent(input$quick_verify, {
      p <- shiny::isolate(project())
      why <- if (is.null(p)) "open a project first" else if (blocked()) .sr_ui_busy_text else NULL
      if (!is.null(why)) return(.sr_ui_notify(paste0("Verify: ", why), "error"))
      lab <- pick(input$freeze_pick)
      if (!nzchar(lab)) return(.sr_ui_notify("Verify: freeze the project first", "error"))
      v <- .sr_ui_try("Verify", function() sr_freeze_verify(p, lab, data = FALSE))
      if (!is.null(v)) show_verify(lab, FALSE, v)
    })

    # ---- jobs ----------------------------------------------------------------------------------------
    # Take the project folder back after the export worker (only the same folder, only the read-only copy).
    reopen <- function(dir) {
      cur <- shiny::isolate(project())
      if (is.null(cur) || !.sr_data_same_dir(cur$paths$dir, dir) || !isTRUE(cur$readonly)) return(invisible(NULL))
      q <- .sr_ui_try("Reopen project", function() sr_project_open(dir))
      if (!is.null(q)) project(q)
      invisible(NULL)
    }

    # The live job is done: tear it down, put the project back, report. `res` is the worker's value or a
    # condition; `cancelled` is TRUE when the user asked to cancel.
    settle <- function(res, cancelled) {
      kind <- job$kind
      label <- job$label
      dir <- job$dir
      path <- job$path
      n_before <- job$n_before
      keep <- job$keep
      jf <- job$job_file
      # whatever happens below, the job is released: the busy flag (running()) drops and the job env is
      # emptied, so the app is never left blocked
      on.exit({
        for (nm in ls(job, all.names = TRUE)) rm(list = nm, envir = job)
        active(FALSE)
      }, add = TRUE)
      tryCatch({
        if (!is.null(job$obs)) job$obs$destroy()
        job$obs <- job$task <- NULL
        if (.sr_job_path_ok(jf)) try(unlink(c(jf, paste0(jf, ".part"))), silent = TRUE)
        if (identical(kind, "verify")) {
          if (cancelled) {
            .sr_ui_notify("Verify cancelled", "message")
          } else if (is.list(res) && !inherits(res, "condition") && !is.null(res$ok)) {
            show_verify(label, TRUE, res)
          } else {
            .sr_ui_notify(paste0("Verify: ", .sr_am_error_text(res)), "error")
          }
        } else {
          good <- is.list(res) && !inherits(res, "condition") && isTRUE(res$ok)
          state <- if (good) {
            "ok"
          } else {
            tryCatch(.sr_ar_export_cleanup(dir, path, label, n_before, keep), error = function(e) {
              .sr_ui_notify(paste0("Export cleanup: ", conditionMessage(e)), "error")
              "failed"
            })
          }
          reopen(dir)
          if (good || identical(state, "finished")) {
            if (cancelled) {
              .sr_ui_notify(sprintf("Export: finished before it could be cancelled (%s)", path), "message")
            } else {
              .sr_ui_notify(paste0("Exported ", path), "message")
            }
          } else if (identical(state, "failed")) {
            NULL                                  # the cleanup error was reported
          } else if (cancelled) {
            .sr_ui_notify("Export cancelled: nothing was written", "message")
          } else {
            .sr_ui_notify(paste0("Export: ", .sr_am_error_text(res)), "error")
          }
        }
      }, error = function(e) {
        try(.sr_ui_notify(paste0("Archive: ", conditionMessage(e)), "error"), silent = TRUE)
      })
      invisible(NULL)
    }

    # Start the job in a mirai worker (or inline under sr.run_sync) and watch it settle.
    launch <- function(fn, args) {
      if (isTRUE(as.logical(sr_opt("run_sync", FALSE)))) {
        res <- tryCatch(do.call(fn, args), error = function(e) e)
        return(settle(res, FALSE))
      }
      task <- shiny::ExtendedTask$new(function(...) .sr_job_spawn(...))
      job$task <- task
      job$cancelling <- FALSE
      job$obs <- shiny::observeEvent(task$status(), {
        st <- task$status()
        if (!(st %in% c("success", "error"))) return(invisible(NULL))
        res <- tryCatch(task$result(), error = function(e) e)
        settle(res, isTRUE(job$cancelling))
      }, ignoreInit = TRUE)
      started <- .sr_ui_try(if (identical(job$kind, "export")) "Export" else "Verify", function() {
        task$invoke(app_dir = sr_root, fn = fn, args = args, opts = .sr_job_opts())
        TRUE
      })
      if (!isTRUE(started)) settle(simpleError("the worker could not be started"), TRUE)
      invisible(NULL)
    }

    shiny::observeEvent(input$data_verify, {
      p <- shiny::isolate(project())
      why <- .sr_ui_refusal(p, blocked)
      if (!is.null(why)) return(.sr_ui_notify(paste0("Verify: ", why), "error"))
      lab <- pick(input$freeze_pick)
      if (!nzchar(lab)) return(.sr_ui_notify("Verify: freeze the project first", "error"))
      jf <- tempfile("sr-ar-job-")
      job$kind <- "verify"
      job$label <- lab
      job$dir <- p$paths$dir
      job$job_file <- jf
      active(TRUE)
      launch(".sr_ar_verify_worker", list(p$paths$dir, lab, jf))
    })

    # Release the lock for the export worker; project() is a read-only copy until the job is settled.
    shiny::observeEvent(input$export, {
      p <- shiny::isolate(project())
      why <- .sr_ui_refusal(p, blocked)
      if (!is.null(why)) return(.sr_ui_notify(paste0("Export: ", why), "error"))
      fz <- sr_freezes(p)$label
      if (!length(fz)) return(.sr_ui_notify("Export: freeze the project first", "error"))
      lab <- pick(input$ex_label)
      if (!(lab %in% fz)) return(.sr_ui_notify(sprintf("Export: unknown freeze '%s'", lab), "error"))
      path <- .sr_am_path(input$ex_path)
      if (is.null(path)) return(.sr_ui_notify("Export: enter a destination path ending in .srproj", "error"))
      why <- .sr_ar_path_problem(path)
      if (!is.null(why)) return(.sr_ui_notify(paste0("Export: ", why), "error"))
      mode <- .sr_ui_text(input$ex_data)
      if (!(mode %in% c("auto", "full", "lean"))) mode <- "auto"
      dir <- p$paths$dir
      jf <- tempfile("sr-ar-job-")
      job$kind <- "export"
      job$label <- lab
      job$dir <- dir
      job$path <- path
      job$job_file <- jf
      job$keep <- .sr_am_existing(dir, path, lab)       # before anything is created
      job$n_before <- .sr_ar_n_events(dir)
      active(TRUE)
      # the worker (a mirai process, or the inline call under sr.run_sync) opens the project itself
      try(sr_duck_close(p), silent = TRUE)
      try(sr_project_close(p), silent = TRUE)
      ro <- p
      ro$readonly <- TRUE
      project(ro)
      launch(".sr_ar_export_worker", list(dir, path, lab, mode, jf))
    })

    cancel_job <- function(kind, label) {
      if (!isTRUE(shiny::isolate(active())) || !identical(job$kind, kind)) {
        return(.sr_ui_notify(sprintf("%s: nothing is running", label), "message"))
      }
      job$cancelling <- TRUE
      k <- tryCatch(.sr_job_kill(job$job_file), error = function(e) e)
      if (inherits(k, "error")) {
        # the kill failed: a process may still be running, so the job stays (and can be cancelled again)
        return(.sr_ui_notify(paste0(label, ": ", conditionMessage(k)), "error"))
      }
      # TRUE: the worker was killed. FALSE: there is no worker to kill (it never started, its job file is
      # missing, or its process is gone). Either way nothing runs any more: clean up and release.
      settle(NULL, TRUE)
    }
    shiny::observeEvent(input$cancel, cancel_job("verify", "Cancel"))
    shiny::observeEvent(input$ex_cancel, cancel_job("export", "Cancel"))

    # ---- Diff ----------------------------------------------------------------------------------------
    diff_res <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$diff, {
      p <- shiny::isolate(project())
      if (is.null(p)) return(.sr_ui_notify("Diff: open a project first", "error"))
      from <- pick(input$diff_from)
      to <- .sr_ui_text(input$diff_to)
      if (!nzchar(to)) to <- "now"
      if (!nzchar(from)) return(.sr_ui_notify("Diff: freeze the project first", "error"))
      r <- .sr_ui_try("Diff", function() {
        d <- sr_freeze_diff(p, from, if (identical(to, "now")) NULL else to)
        tb <- .sr_ar_diff_tables(d)
        # only what changed is shown: rows the core reports as "same" are left out, and so are empty sections
        for (n in intersect(c("tables", "analyses"), names(tb))) {
          tb[[n]] <- tb[[n]][tb[[n]]$change != "same", , drop = FALSE]
          if (!nrow(tb[[n]])) tb[[n]] <- NULL
        }
        list(from = from, to = to, tables = tb,
             results = if (identical(to, "now")) .sr_ar_results_diff(sr_freeze_results_diff(p, from)))
      })
      if (!is.null(r)) diff_res(r)
    })
    output$diff_out <- shiny::renderUI({
      r <- diff_res()
      if (is.null(r)) return(NULL)
      sec <- c(tables = "Tables", library = "Library", sap = "SAP", analyses = "Analyses")
      shiny::tagList(
        shiny::h6(sprintf("'%s' against %s", r$from, if (identical(r$to, "now")) "the project now" else paste0("'", r$to, "'"))),
        if (!length(r$tables)) shiny::p(class = "text-muted small", "No differences."),
        lapply(names(r$tables), function(n) {
          shiny::tagList(shiny::h6(class = "mt-2", sec[[n]]), .sr_am_table(r$tables[[n]]))
        }),
        if (!is.null(r$results)) {
          shiny::tagList(shiny::h6(class = "mt-3", "Stored results against the project now"), .sr_am_table(r$results))
        }
      )
    })

    # ---- Export plan ---------------------------------------------------------------------------------
    output$ex_plan <- shiny::renderUI({
      settings()
      p <- project()
      l <- labels()
      if (is.null(p) || !length(l)) return(shiny::p(class = "text-muted small", "freeze the project first"))
      lab <- pick(input$ex_label)
      mode <- .sr_ui_text(input$ex_data)
      txt <- if (!(lab %in% l)) {
        sprintf("unknown freeze '%s'", lab)
      } else if (identical(mode, "full") || identical(mode, "lean")) {
        sprintf("%s data files", mode)
      } else {
        pl <- tryCatch(.sr_ar_srproj_plan(p, lab, "auto"), error = function(e) e)
        if (inherits(pl, "error")) {
          conditionMessage(pl)
        } else {
          paste0(sprintf("auto will pick %s (%s)", pl$mode, .sr_am_size(pl$bytes)),
                 if (isTRUE(pl$absent)) "; some data files are missing from the project folder" else "")
        }
      }
      warn <- .sr_ar_actor_warning()
      shiny::tagList(
        shiny::p(class = "small", txt),
        if (!is.null(warn)) {
          shiny::div(class = "alert alert-warning small", warn, " ",
                     shiny::actionLink(ns("go_settings"), "Open Settings"))
        }
      )
    })
    shiny::observeEvent(input$go_settings, {
      .sr_ui_try("Settings", function() bslib::nav_select("main_nav", "Settings", session = session$rootScope()))
    })

    # ---- Re-baseline ---------------------------------------------------------------------------------
    rb_diff <- shiny::reactive({
      settings()
      p <- project()
      if (is.null(p) || !nrow(sr_freezes(p))) return(NULL)
      tryCatch(sr_rebaseline_diff(p), error = function(e) e)
    })
    # why Re-baseline is off, or NULL (read from the core frame)
    rb_off <- function(p, d) {
      if (is.null(p) || !nrow(sr_freezes(p))) return("freeze the project first")
      if (inherits(d, "error")) return(conditionMessage(d))
      if (!NROW(d)) return("the library already matches the baseline")
      NULL
    }
    output$rb_diff <- shiny::renderUI({
      p <- project()
      d <- rb_diff()
      off <- rb_off(p, d)
      shiny::tagList(
        if (!is.null(off)) shiny::p(class = "text-muted small", off),
        if (is.data.frame(d) && nrow(d)) .sr_wm_table(d)
      )
    })
    shiny::observe({
      shiny::updateActionButton(session, "rebaseline", disabled = !is.null(rb_off(project(), rb_diff())))
    })
    shiny::observeEvent(input$rebaseline, {
      p <- shiny::isolate(project())
      why <- .sr_ui_refusal(p, blocked)
      if (is.null(why)) {
        d <- if (nrow(sr_freezes(p))) tryCatch(sr_rebaseline_diff(p), error = function(e) e) else NULL
        why <- rb_off(p, d)
      }
      if (!is.null(why)) return(.sr_ui_notify(paste0("Re-baseline: ", why), "error"))
      reason <- .sr_ui_text(input$rb_reason)
      if (!nzchar(reason)) return(.sr_ui_notify("Re-baseline: give a reason", "error"))
      r <- act("Re-baseline", function(p) sr_rebaseline(p, reason))
      if (!is.null(r)) shiny::updateTextInput(session, "rb_reason", value = "")
    })

    # Closing the session mid-job kills the worker and removes what an unfinished export left behind (never
    # a path that existed before it). It never raises; a worker that could not be killed is left alone.
    session$onSessionEnded(function() {
      try({
        if (isTRUE(shiny::isolate(active())) && !is.null(job$job_file)) {
          k <- tryCatch(.sr_job_kill(job$job_file), error = function(e) e)
          if (!inherits(k, "error") && identical(job$kind, "export")) {
            .sr_ar_export_cleanup(job$dir, job$path, job$label, job$n_before, job$keep)
          }
        }
      }, silent = TRUE)
    })

    list(running = running)
  })
}
