# ui/mod_analyses.R - Shiny module: the Analyses tab. Card 1 (Runs) lists the analyses of the locked SAP
# with their run status and runs them: Run selected, Run all (every analysis not ok, in display order) and
# Cancel. Cards 2-5 (Result, Primary vs sensitivity, Multiplicity, Deviations and run history) are filled
# by later tasks.
#
# mod_analyses_server(id, project, busy) takes the project reactiveVal of mod_project_server() and `busy`, a
# reactive that is TRUE while an import runs. It returns list(running): TRUE while a queue item runs or ids
# are queued (app.R combines it with the import into the app-wide busy flag).
#
# The panel keeps no project state of its own: every card re-derives from the replayed project. The only
# session state is the queue and the live job (`run`): the ids still to run, the current id and job, its
# start time and its three temp files. Each queue item runs in a mirai worker (shiny::ExtendedTask,
# spawner .sr_an_spawn): the session releases the project lock (closing DuckDB first) and holds a
# read-only copy while the worker runs, re-opens the project when the worker is done and only then settles
# the item (.sr_an_settle), which may write run.cancelled. option sr.run_sync = TRUE runs each item inline
# through .sr_an_job instead (tests: their stub routes exist only in the session).
#
# Cancel drops the rest of the queue, creates the cancel file and kills the worker's process tree
# (.sr_an_kill) unless its mirai has resolved already (then the result is settled as it is, and a
# completed run stays: "finished before it could be cancelled"). When the kill raises (a process
# survived), the error is shown and nothing else is done: the project is re-opened only once the worker's
# mirai resolves. Closing the session cancels nothing: the worker finishes and records its events.

# ---- small pure helpers ------------------------------------------------------------------------------

# A duration in seconds as "14 s", "2 m 14 s" or "1 h 5 m".
.sr_an_elapsed <- function(s) {
  s <- suppressWarnings(as.numeric(s))
  if (length(s) != 1L || !is.finite(s) || s < 0) return("0 s")
  s <- floor(s)
  if (s < 60) return(sprintf("%d s", as.integer(s)))
  if (s < 3600) return(sprintf("%d m %d s", as.integer(s %/% 60), as.integer(s %% 60)))
  sprintf("%d h %d m", as.integer(s %/% 3600), as.integer((s %% 3600) %/% 60))
}

# The step the worker wrote to the progress file: "fitting" or "diagnostics", else "starting".
.sr_an_step <- function(file) {
  if (!.sr_an_path_ok(file) || !file.exists(file)) return("starting")
  x <- tryCatch(suppressWarnings(readLines(file, n = 1L, warn = FALSE)), error = function(e) character())
  if (length(x) == 1L && x %in% c("fitting", "diagnostics")) x else "starting"
}

# TRUE when `m` is a mirai that has resolved (its worker is done).
.sr_an_resolved <- function(m) {
  inherits(m, "mirai") && !isTRUE(tryCatch(mirai::unresolved(m), error = function(e) TRUE))
}

# ---- UI ----------------------------------------------------------------------------------------------

mod_analyses_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "sr-analyses-stack",
    .sr_ui_card("Runs",
      shiny::uiOutput(ns("run_btns")),
      shiny::uiOutput(ns("progress")),
      DT::DTOutput(ns("runs"), fill = FALSE)
    ),
    .sr_ui_card("Result"),
    .sr_ui_card("Primary vs sensitivity"),
    .sr_ui_card("Multiplicity"),
    .sr_ui_card("Deviations and run history")
  )
}

# ---- server ------------------------------------------------------------------------------------------

mod_analyses_server <- function(id, project, busy) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # The queue and the live job (transient: nothing here is project state).
    run <- shiny::reactiveValues(queue = character(), total = 0L, index = 0L, id = NULL, job = NULL, t0 = NULL,
                                 dir = NULL, name = NULL, progress = NULL, job_file = NULL, cancel_file = NULL,
                                 cancelling = FALSE)
    # the mirai of the live job, for the "has it resolved already" check of Cancel
    live_m <- new.env(parent = emptyenv())
    task <- shiny::ExtendedTask$new(function(...) {
      m <- .sr_an_spawn(...)
      live_m$m <- m
      m
    })

    running <- shiny::reactive(!is.null(run$id) || length(run$queue) > 0L)
    live <- shiny::reactive({
      if (is.null(run$id) || is.null(run$job)) character() else stats::setNames(run$job, run$id)
    })
    busy_now <- function() isTRUE(shiny::isolate(busy()))

    # ---- selection (held by analysis id) -----------------------------------------------------------
    order_ids <- shiny::reactive({
      p <- project()
      if (is.null(p)) character() else .sr_an_order(p)$id
    })
    sel <- shiny::reactiveVal(NULL)
    sel_id <- shiny::reactive({
      k <- sel()
      if (!is.null(k) && k %in% order_ids()) k else NULL
    })
    shiny::observeEvent(input$runs_rows_selected, {
      ids <- shiny::isolate(order_ids())
      i <- input$runs_rows_selected
      if (length(i) != 1L || is.na(i) || i < 1L || i > length(ids)) return(invisible(NULL))
      sel(ids[i])
    }, ignoreNULL = FALSE, priority = 10)

    # ---- the Runs table (value-compared: it holds the selection) ------------------------------------
    runs_df <- shiny::reactiveVal(NULL)
    shiny::observe({
      p <- project()
      v <- if (is.null(p)) NULL else .sr_an_runs_table(p, live())
      if (!identical(shiny::isolate(runs_df()), v)) runs_df(v)
    })
    output$runs <- DT::renderDT({
      df <- runs_df()
      if (is.null(df)) df <- data.frame(note = "open a project first", stringsAsFactors = FALSE)
      ids <- shiny::isolate(order_ids())
      k <- shiny::isolate(sel_id())
      DT::datatable(df, rownames = FALSE, escape = TRUE,
                    selection = list(mode = "single", selected = if (!is.null(k) && k %in% ids) match(k, ids)),
                    options = list(dom = "tip", pageLength = 25L, scrollX = TRUE, ordering = FALSE),
                    class = "compact stripe")
    }, server = FALSE)

    # ---- buttons and gate (value-compared) ------------------------------------------------------------
    gate <- shiny::reactiveVal(NULL)
    shiny::observe({
      p <- project()
      rn <- isTRUE(running())
      v <- list(reason = .sr_an_can_run(p, isTRUE(busy()) || rn), running = rn,
                readonly = !is.null(p) && isTRUE(p$readonly))
      if (!identical(shiny::isolate(gate()), v)) gate(v)
    })
    output$run_btns <- shiny::renderUI({
      g <- gate()
      off <- is.null(g) || !is.null(g$reason)
      rn <- isTRUE(g$running)
      shiny::tagList(
        if (!rn && isTRUE(g$readonly)) .sr_ui_readonly_banner(shiny::isolate(project())),
        shiny::div(
          class = "d-flex gap-2 mb-2 align-items-center",
          shiny::actionButton(ns("run_sel"), "Run selected", class = "btn-primary", disabled = off),
          shiny::actionButton(ns("run_all"), "Run all", class = "btn-outline-primary", disabled = off),
          shiny::actionButton(ns("cancel"), "Cancel", class = "btn-outline-danger", disabled = !rn),
          if (!rn && !is.null(g$reason)) shiny::span(class = "text-muted small", paste0("Run is off: ", g$reason))
        )
      )
    })

    output$badge <- shiny::renderUI({
      if (isTRUE(running())) shiny::span(class = "badge text-bg-warning", "running")
    })

    output$progress <- shiny::renderUI({
      if (!isTRUE(running())) return(NULL)
      shiny::invalidateLater(1000)
      if (is.null(run$id)) return(NULL)
      el <- if (is.null(run$t0)) 0 else as.numeric(difftime(Sys.time(), run$t0, units = "secs"))
      shiny::div(class = "small mb-2",
                 sprintf("Running %d of %d: %s, %s (%s)", as.integer(run$index), as.integer(run$total), run$id,
                         .sr_an_step(run$progress), .sr_an_elapsed(el)))
    })

    # ---- the queue ----------------------------------------------------------------------------------
    clear_job <- function() {
      f <- c(shiny::isolate(run$progress), shiny::isolate(run$job_file), shiny::isolate(run$cancel_file))
      jf <- shiny::isolate(run$job_file)
      if (length(jf)) f <- c(f, paste0(jf, ".part"))
      if (length(f)) try(unlink(f), silent = TRUE)
      live_m$m <- NULL
      run$id <- NULL
      run$job <- NULL
      run$t0 <- NULL
      run$dir <- NULL
      run$name <- NULL
      run$progress <- NULL
      run$job_file <- NULL
      run$cancel_file <- NULL
      run$cancelling <- FALSE
    }

    stop_queue <- function() {
      run$queue <- character()
    }

    # Take the project folder back after the worker (only the same folder, only the read-only copy, as the
    # Data tab does). Returns the project to settle on: the open one when it is that folder, else NULL.
    reopen <- function(dir) {
      cur <- shiny::isolate(project())
      same <- !is.null(cur) && .sr_data_same_dir(cur$paths$dir, dir)
      if (!same) return(NULL)
      if (isTRUE(cur$readonly)) {
        q <- .sr_ui_try("Reopen project", function() sr_project_open(dir))
        if (!is.null(q)) {
          project(q)
          cur <- q
        }
      }
      cur
    }

    notify_out <- function(out) {
      if (is.list(out) && !is.null(out$notice)) .sr_ui_notify(out$notice, out$type %||% "message")
    }

    # The live job is done (`res` is the worker's value or a condition): re-open, settle, go on.
    settle_job <- function(res, cancelling) {
      id <- shiny::isolate(run$id)
      job <- shiny::isolate(run$job)
      dir <- shiny::isolate(run$dir)
      name <- shiny::isolate(run$name) %||% basename(dir %||% "")
      cur <- reopen(dir)
      if (!is.null(cur)) {
        out <- .sr_ui_try("Run", function() .sr_an_settle(cur, id, job, res, cancelling))
        if (!is.null(out)) project(out$project)
      } else {
        # another project is open now: settle on the run's own folder, leave the open one alone, stop
        out <- .sr_ui_try("Run", function() {
          q <- sr_project_open(dir)
          on.exit({
            try(sr_duck_close(q), silent = TRUE)
            try(sr_project_close(q), silent = TRUE)
          }, add = TRUE)
          o <- .sr_an_settle(q, id, job, res, cancelling)
          if (!is.null(o$notice)) o$notice <- paste0(o$notice, " (in ", name, ")")
          o$stop <- TRUE
          o
        })
      }
      notify_out(out)
      clear_job()
      if (is.null(out) || isTRUE(out$stop)) stop_queue()
      start_next()
    }

    start_next <- function() {
      repeat {
        q <- shiny::isolate(run$queue)
        if (!length(q)) {
          clear_job()
          run$total <- 0L
          run$index <- 0L
          return(invisible(NULL))
        }
        id <- q[1L]
        run$queue <- q[-1L]
        run$index <- shiny::isolate(run$index) + 1L
        p <- shiny::isolate(project())
        why <- .sr_an_can_run(p, busy_now())
        if (!is.null(why)) {
          .sr_ui_notify(sprintf("Run stopped at %s: %s", id, why), "error")
          stop_queue()
          next
        }
        job <- .sr_run_job_id()
        run$id <- id
        run$job <- job
        run$t0 <- Sys.time()
        run$cancelling <- FALSE

        if (isTRUE(as.logical(sr_opt("run_sync", FALSE)))) {
          res <- tryCatch(.sr_an_job(p, id, job),
                          error = function(e) list(status = "refused", message = conditionMessage(e), project = p))
          q2 <- if (is.list(res) && is.list(res$project) && !is.null(res$project$paths)) res$project else p
          project(q2)
          out <- .sr_ui_try("Run", function() .sr_an_settle(q2, id, job, res, FALSE))
          if (!is.null(out)) project(out$project)
          notify_out(out)
          clear_job()
          if (is.null(out) || isTRUE(out$stop)) stop_queue()
          next
        }

        # background: release the lock so the worker can take it; until it is back the session holds a
        # read-only copy, so no write can go through a handle that no longer owns the project
        dir <- p$paths$dir
        prog <- tempfile("sr-an-progress-")
        jf <- tempfile("sr-an-job-")
        cf <- tempfile("sr-an-cancel-")
        try(sr_duck_close(p), silent = TRUE)
        try(sr_project_close(p), silent = TRUE)
        ro <- p
        ro$readonly <- TRUE
        project(ro)
        run$dir <- dir
        run$name <- as.character(p$meta$name %||% basename(dir))[1L]
        run$progress <- prog
        run$job_file <- jf
        run$cancel_file <- cf
        started <- .sr_ui_try("Run", function() {
          task$invoke(app_dir = sr_root, project_dir = dir, id = id, job = job, progress_file = prog,
                      job_file = jf, cancel_file = cf)
          TRUE
        })
        if (!isTRUE(started)) {
          reopen(dir)
          clear_job()
          stop_queue()
          next
        }
        return(invisible(NULL))
      }
    }

    shiny::observeEvent(task$status(), {
      st <- task$status()
      if (!(st %in% c("success", "error"))) return(invisible(NULL))
      if (is.null(shiny::isolate(run$id))) return(invisible(NULL))
      cancelling <- isTRUE(shiny::isolate(run$cancelling))
      res <- tryCatch(task$result(), error = function(e) e)
      # a failed task is a refusal; while cancelling it is the killed worker, settled as a cancel
      if (inherits(res, "error") && !cancelling) res <- list(status = "refused", message = conditionMessage(res))
      settle_job(res, cancelling)
    }, ignoreInit = TRUE)

    start_queue <- function(mode) {
      p <- shiny::isolate(project())
      why <- .sr_an_can_run(p, busy_now() || isTRUE(shiny::isolate(running())))
      if (!is.null(why)) return(.sr_ui_notify(paste0("Run: ", why), "error"))
      q <- tryCatch(.sr_an_queue(p, mode, if (identical(mode, "selected")) shiny::isolate(sel_id())),
                    error = function(e) e)
      if (inherits(q, "error")) return(.sr_ui_notify(paste0("Run: ", conditionMessage(q)), "error"))
      if (!length(q)) return(.sr_ui_notify("Run all: every analysis has run successfully", "message"))
      run$queue <- q
      run$total <- length(q)
      run$index <- 0L
      start_next()
    }

    shiny::observeEvent(input$run_sel, start_queue("selected"))
    shiny::observeEvent(input$run_all, start_queue("all"))

    shiny::observeEvent(input$cancel, {
      if (!isTRUE(running())) return(.sr_ui_notify("Cancel: nothing is running", "message"))
      run$cancelling <- TRUE
      stop_queue()
      jf <- run$job_file
      if (is.null(run$id) || is.null(jf)) return(invisible(NULL))
      try(file.create(run$cancel_file), silent = TRUE)
      # a worker that is done has removed its job file already: nothing to kill, settle its result
      if (.sr_an_resolved(live_m$m)) return(invisible(NULL))
      k <- tryCatch(.sr_an_kill(jf), error = function(e) e)
      # a process survived: say so and wait for the worker's mirai before taking the project back
      if (inherits(k, "error")) .sr_ui_notify(paste0("Cancel: ", conditionMessage(k)), "error")
    })

    # Nothing is cancelled when the session ends: the worker finishes and records (its files it removes
    # itself); the queue goes with the session.
    session$onSessionEnded(function() shiny::isolate(run$queue <- character()))

    list(running = running)
  })
}
