# ui/mod_analyses.R - Shiny module: the Analyses tab. Card 1 (Runs) lists the analyses of the locked SAP
# with their run status and runs them: Run selected, Run all (every analysis not ok, in display order) and
# Cancel. Card 2 (Result) shows the selected analysis with its diagnostics and Apply; cards 3-5 (Primary vs
# sensitivity, Multiplicity, Deviations and run history) are read-only views of the replayed project.
#
# mod_analyses_server(id, project, busy) takes the project reactiveVal of mod_project_server() and `busy`, a
# reactive that is TRUE while an import runs. It returns list(running): TRUE while a queue item runs or ids
# are queued (app.R combines it with the import into the app-wide busy flag).
#
# The panel keeps no project state of its own: every card re-derives from the replayed project. The only
# session state is the queue and the live job (`run`): the ids still to run, the current id and job, its
# start time and its three temp files. Each queue item runs in a mirai worker (shiny::ExtendedTask,
# spawner .sr_an_spawn): the session releases the project lock (its in-memory DuckDB connection is
# closed too, just to be tidy) and holds a
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
    .sr_ui_card("Result",
      shiny::uiOutput(ns("result_head")),
      DT::DTOutput(ns("effects"), fill = FALSE),
      shiny::uiOutput(ns("tables")),
      shiny::uiOutput(ns("figs")),
      shiny::uiOutput(ns("fragments")),
      DT::DTOutput(ns("diag"), fill = FALSE),
      shiny::uiOutput(ns("diag_figs")),
      shiny::uiOutput(ns("apply_preview"))
    ),
    .sr_ui_card("Primary vs sensitivity",
      DT::DTOutput(ns("compare"), fill = FALSE)
    ),
    .sr_ui_card("Multiplicity",
      shiny::uiOutput(ns("families"))
    ),
    .sr_ui_card("Deviations and run history",
      shiny::h6("Deviations"),
      DT::DTOutput(ns("deviations"), fill = FALSE),
      shiny::h6(class = "mt-3", "Run history"),
      DT::DTOutput(ns("history"), fill = FALSE)
    )
  )
}

# ---- server ------------------------------------------------------------------------------------------

# `busy` must be the IMPORT-ONLY flag and must EXCLUDE this module's own `running`: app.R passes the
# import's busy reactive here, and the module combines it with its own running() where it gates. Passing
# the app-wide flag (which includes running) would make every queue item refuse itself.
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
    # the live job, only for the folder it runs in (another open project shows no live row)
    live <- shiny::reactive({
      if (is.null(run$id) || is.null(run$job) || is.null(run$dir)) return(character())
      p <- project()
      if (is.null(p) || !.sr_data_same_dir(p$paths$dir, run$dir)) return(character())
      stats::setNames(run$job, run$id)
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
      if (!length(i)) return(sel(NULL))                     # the row was deselected
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
      v <- list(reason = .sr_an_can_run(p, isTRUE(busy()) || rn), running = rn, busy = isTRUE(busy()),
                readonly = !is.null(p) && isTRUE(p$readonly))
      if (!identical(shiny::isolate(gate()), v)) gate(v)
    })
    output$run_btns <- shiny::renderUI({
      g <- gate()
      off <- is.null(g) || !is.null(g$reason)
      rn <- isTRUE(g$running)
      shiny::tagList(
        if (!rn && !isTRUE(g$busy) && isTRUE(g$readonly)) .sr_ui_readonly_banner(shiny::isolate(project())),
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
      # a streaming job also writes its pass count to <project>/data/tmp/progress-<job>.json
      pt <- if (is.null(run$dir) || is.null(run$job)) "" else {
        .sr_an_progress_text(.sr_progress_path(list(paths = list(dir = run$dir)), run$job))
      }
      shiny::div(class = "small mb-2",
                 sprintf("Running %d of %d: %s, %s%s (%s)", as.integer(run$index), as.integer(run$total), run$id,
                         .sr_an_step(run$progress), if (nzchar(pt)) paste0(", ", pt) else "", .sr_an_elapsed(el)))
    })

    # ---- card 2: the result of the selected analysis ----------------------------------------------------
    # `card` holds only what the card reads of the replayed project (the run record, the diagnostics, the
    # live flag), compared by value, so an unrelated project change does not redraw the plots.
    card <- shiny::reactiveVal(NULL)
    shiny::observe({
      p <- project()
      id <- sel_id()
      v <- if (is.null(p) || is.null(id)) NULL else {
        list(id = id, dir = p$paths$dir, run = p$state$runs[[id]], diag = p$state$diag[[id]],
             live = id %in% names(live()) && (is.null(p$state$runs[[id]]) || !identical(p$state$runs[[id]]$status, "running") ||
                                              .sr_an_is_live(live(), id, p$state$runs[[id]]$job)))
      }
      if (!identical(shiny::isolate(card()), v)) card(v)
    })
    shown <- shiny::reactive({
      k <- card()
      if (is.null(k)) return(NULL)
      ok_key <- k$run$ok_key
      out <- list(id = k$id, run = k$run, live = isTRUE(k$live),
                  has_ok = !is.null(ok_key) && length(ok_key) == 1L && !is.na(ok_key))
      if (out$has_ok) {
        res <- tryCatch(sr_result(shiny::isolate(project()), k$id), error = function(e) e)
        if (inherits(res, "error")) out$err <- conditionMessage(res) else out$pres <- res
      }
      out
    })

    output$result_head <- shiny::renderUI({
      s <- shown()
      if (is.null(s)) return(shiny::div(class = "text-muted", "Select an analysis in the Runs table."))
      r <- s$run
      st <- if (s$live) "running" else .sr_an_s(r$status)
      if (!nzchar(st)) st <- if (is.null(r)) "not run" else if (isTRUE(r$ok)) "ok" else "failed"
      if (identical(st, "running") && !s$live) st <- "interrupted"
      cls <- switch(st, ok = "bg-success", failed = "bg-danger", "bg-warning text-dark")
      eng <- .sr_an_engine_badge(shiny::isolate(project()), s$id)
      head <- if (!is.null(r)) {
        done <- st %in% c("ok", "failed")   # a run that is not finished has no elapsed time or cache flag
        el <- if (done) suppressWarnings(as.numeric(r$elapsed)) else NA_real_
        shiny::div(
          class = "d-flex gap-2 align-items-center mb-2",
          shiny::span(class = paste("badge", cls), `data-status` = st, st),
          if (length(el) == 1L && is.finite(el)) shiny::span(class = "small", .sr_an_elapsed(el)),
          if (done && !is.null(r$cached)) shiny::span(class = "small text-muted", if (isTRUE(r$cached)) "cached" else "fresh"),
          if (nzchar(eng)) shiny::span(class = "badge text-bg-info", `data-engine` = "stream", eng)
        )
      }
      bits <- list(head)
      if (st %in% c("ok", "failed") && isTRUE(r$lib_changed)) {
        bits <- c(bits, list(shiny::div(class = "alert alert-warning py-1",
          "The library snapshot of this run differs from the one recorded at the SAP lock.")))
      }
      if (identical(st, "failed") && nzchar(.sr_an_s(r$error))) {
        bits <- c(bits, list(shiny::tags$pre(.sr_an_s(r$error))))
        # the memory engine refused the analysis: offer the engine switch (a computational deviation)
        if (grepl("sr_engine_switch", .sr_an_s(r$error), fixed = TRUE)) {
          g <- gate()
          bits <- c(bits, list(shiny::actionButton(ns("switch_engine"), "Switch engine...",
                                                   class = "btn-outline-primary mb-2",
                                                   disabled = is.null(g) || !is.null(g$reason))))
        }
      }
      if (!s$has_ok) {
        bits <- c(bits, list(shiny::div(class = "text-muted", "No successful run yet.")))
      } else {
        if (!identical(st, "ok")) {
          h <- Filter(function(e) identical(.sr_an_s(e$outcome), "ok"), r$history %||% list())
          when <- if (length(h)) .sr_an_s(h[[length(h)]]$ended) else ""
          bits <- c(bits, list(shiny::div(class = "small fw-semibold",
            sprintf("Showing the last good run (%s).", when))))
        }
        if (!is.null(s$err)) bits <- c(bits, list(shiny::tags$pre(s$err)))
      }
      do.call(shiny::tagList, bits)
    })

    output$effects <- DT::renderDT({
      s <- shown()
      shiny::req(!is.null(s$pres))
      DT::datatable(.sr_an_effects(s$pres), rownames = FALSE, selection = "none", escape = TRUE,
                    caption = "Effects", options = list(dom = "t", pageLength = 100L, scrollX = TRUE, ordering = FALSE),
                    class = "compact stripe")
    }, server = FALSE)

    output$tables <- shiny::renderUI({
      s <- shown()
      tb <- s$pres$tables
      if (!length(tb)) return(NULL)
      do.call(shiny::tagList, lapply(names(tb), function(nm) {
        shiny::tagList(shiny::h6(class = "mt-3", nm),
                       DT::datatable(as.data.frame(tb[[nm]]), rownames = FALSE, selection = "none", escape = TRUE,
                                     options = list(dom = "t", pageLength = 50L, scrollX = TRUE, ordering = FALSE),
                                     class = "compact stripe"))
      }))
    })

    output$fragments <- shiny::renderUI({
      s <- shown()
      fr <- s$pres$fragments
      if (!length(fr)) return(NULL)
      do.call(shiny::tagList, lapply(names(fr), function(nm) {
        shiny::tagList(shiny::h6(class = "mt-3", nm),
                       shiny::tags$pre(class = "small", paste(as.character(fr[[nm]]), collapse = "\n")))
      }))
    })

    # figures: one plotOutput per figure, drawn by sr_plot (the id is the name made safe)
    fig_id <- function(prefix, nm) paste0(prefix, gsub("[^A-Za-z0-9_]", "_", nm))
    plot_ui <- function(items, prefix) {
      if (!length(items)) return(NULL)
      do.call(shiny::tagList, lapply(names(items), function(nm) {
        shiny::tagList(shiny::h6(class = "mt-3", nm),
                       shiny::plotOutput(ns(fig_id(prefix, nm)), height = "360px"))
      }))
    }
    bind_plots <- function(items, prefix) {
      for (nm in names(items)) local({
        f <- items[[nm]]
        output[[fig_id(prefix, nm)]] <- shiny::renderPlot(sr_plot(f$data, f$kind))
      })
    }
    figs_v <- shiny::reactive({
      s <- shown()
      if (is.null(s$pres)) list() else .sr_an_figures(s$pres)
    })
    dfigs_v <- shiny::reactive({
      s <- shown()
      if (is.null(s) || !s$has_ok) return(list())
      p <- shiny::isolate(project())
      ck <- .sr_an_diag_rows(p, s$id)$check
      out <- lapply(ck, function(k) .sr_an_diag_plot(p, s$id, k))
      names(out) <- ck
      out[!vapply(out, is.null, NA)]
    })
    shiny::observe(bind_plots(figs_v(), "fig_"))
    shiny::observe(bind_plots(dfigs_v(), "dfig_"))
    output$figs <- shiny::renderUI(plot_ui(figs_v(), "fig_"))
    output$diag_figs <- shiny::renderUI(plot_ui(dfigs_v(), "dfig_"))

    output$diag <- DT::renderDT({
      s <- shown()
      shiny::req(!is.null(s), isTRUE(s$has_ok))
      d <- .sr_an_diag_rows(shiny::isolate(project()), s$id)
      for (k in c("value", "warn", "fail")) d[[k]] <- round(d[[k]], 3L)
      DT::datatable(d, rownames = FALSE, selection = "none", escape = TRUE, caption = "Diagnostics",
                    options = list(dom = "t", pageLength = 50L, scrollX = TRUE, ordering = FALSE),
                    class = "compact stripe")
    }, server = FALSE)

    # ---- Apply diagnostics: the preview (value-compared) and the gated action ------------------------
    plan_v <- shiny::reactiveVal(NULL)
    shiny::observe({
      p <- project()
      id <- sel_id()
      rk <- if (is.null(p) || is.null(id)) NULL else p$state$runs[[id]]$ok_key
      v <- NULL
      if (!is.null(rk) && length(rk) == 1L && !is.na(rk)) {
        if (!length(p$state$diag[[id]])) {
          # no diagnostics recorded: either none applies to the route or the run was cut before they ran
          ap <- tryCatch(sr_diag_applicable(p$state$analyses[[id]]), error = function(e) character())
          v <- list(id = id, none = if (length(ap)) "pending" else "not_applicable")
        } else {
          pl <- tryCatch(sr_diag_apply_plan(p, id), error = function(e) e)
          v <- if (inherits(pl, "error")) list(id = id, error = conditionMessage(pl)) else {
            list(id = id, n = nrow(pl), words = .sr_an_plan_words(p, pl))
          }
        }
      }
      if (!identical(shiny::isolate(plan_v()), v)) plan_v(v)
    })
    output$apply_preview <- shiny::renderUI({
      v <- plan_v()
      if (is.null(v)) return(NULL)
      g <- gate()
      off <- is.null(g) || !is.null(g$reason) || !is.null(v$error) || !is.null(v$none) || !isTRUE(v$n > 0L)
      shiny::tagList(
        shiny::h6(class = "mt-3", "Apply diagnostics"),
        if (!is.null(v$none)) {
          shiny::div(class = "text-muted", if (identical(v$none, "pending")) {
            "No diagnostics recorded yet - Run selected runs them."
          } else {
            "No diagnostics apply to this route."
          })
        } else if (!is.null(v$error)) {
          shiny::div(class = "text-danger", v$error)
        } else if (!isTRUE(v$n > 0L)) {
          shiny::div(class = "text-muted", "Nothing to apply.")
        } else {
          shiny::tags$ul(lapply(v$words, shiny::tags$li))
        },
        shiny::div(
          class = "d-flex gap-2 align-items-center",
          shiny::actionButton(ns("apply"), "Apply", class = "btn-outline-primary", disabled = off),
          if (!is.null(g$reason)) shiny::span(class = "text-muted small", paste0("Apply is off: ", g$reason))
        )
      )
    })

    act <- .sr_ui_actor(project, busy = function() busy_now() || isTRUE(shiny::isolate(running())))
    shiny::observeEvent(input$apply, {
      id <- shiny::isolate(sel_id())
      p <- shiny::isolate(project())
      if (is.null(id)) return(.sr_ui_notify("Apply diagnostics: select an analysis first", "error"))
      why <- if (isTRUE(shiny::isolate(running()))) .sr_an_can_run(list(), TRUE) else .sr_an_can_run(p, busy_now())
      if (!is.null(why)) return(.sr_ui_notify(paste0("Apply diagnostics: ", why), "error"))
      pl <- tryCatch(sr_diag_apply_plan(p, id), error = function(e) e)
      if (inherits(pl, "error")) return(.sr_ui_notify(paste0("Apply diagnostics: ", conditionMessage(pl)), "error"))
      if (!nrow(pl)) return(.sr_ui_notify("Apply diagnostics: nothing to apply", "message"))
      act("Apply diagnostics", function(p) sr_diag_apply(p, id))
    })

    # ---- the engine switch dialog ----------------------------------------------------------------------
    # sr_engine_switch records a computational deviation (the estimator and the SAP hash are unchanged); a
    # reason is required, and a core refusal is shown in the dialog with the inputs kept.
    sw_msg <- shiny::reactiveVal("")
    sw_id <- shiny::reactiveVal(NULL)
    output$sw_err <- shiny::renderText(sw_msg())
    shiny::observeEvent(input$switch_engine, {
      id <- shiny::isolate(sel_id())
      p <- shiny::isolate(project())
      if (is.null(id) || is.null(p)) return(.sr_ui_notify("Switch engine: select an analysis first", "error"))
      why <- if (isTRUE(shiny::isolate(running()))) .sr_an_can_run(list(), TRUE) else .sr_an_can_run(p, busy_now())
      if (!is.null(why) && !identical(why, "lock the SAP first")) {
        return(.sr_ui_notify(paste0("Switch engine: ", why), "error"))
      }
      cur <- tryCatch(.sr_engine_effective(p, id), error = function(e) "memory")
      sw_id(id)
      sw_msg("")
      shiny::showModal(shiny::modalDialog(
        title = paste0("Switch the engine of ", id), easyClose = FALSE,
        shiny::p(sprintf("The analysis uses the %s engine now. A switch is recorded as a computational deviation; the estimator is unchanged.", cur)),
        .sr_ui_select(ns("sw_engine"), "Engine", c("stream", "memory"), if (identical(cur, "stream")) "memory" else "stream",
                      texts = c(stream = "Streaming", memory = "In memory")),
        .sr_ui_on_control(shiny::textInput(ns("sw_reason"), "Reason (required)", value = "", width = "100%"),
                          "input", maxlength = 500L),
        shiny::div(class = "text-danger small mt-2", shiny::textOutput(ns("sw_err"))),
        footer = shiny::tagList(shiny::actionButton(ns("sw_cancel"), "Cancel"),
                                shiny::actionButton(ns("sw_ok"), "Switch", class = "btn-primary"))))
    })
    shiny::observeEvent(input$sw_cancel, shiny::removeModal())
    shiny::observeEvent(input$sw_ok, {
      id <- shiny::isolate(sw_id())
      p <- shiny::isolate(project())
      reason <- trimws(.sr_ui_chr(input$sw_reason))
      if (!nzchar(reason)) return(sw_msg("A reason is required for an engine switch."))
      eng <- .sr_ui_chr(input$sw_engine)
      why <- .sr_ui_refusal(p, function() busy_now() || isTRUE(shiny::isolate(running())))
      if (!is.null(why)) return(sw_msg(why))
      q <- tryCatch(sr_engine_switch(p, id, eng, reason), error = function(e) e)
      if (inherits(q, "error")) return(sw_msg(conditionMessage(q)))
      sw_msg("")
      project(q)
      shiny::removeModal()
      .sr_ui_notify(sprintf("%s now uses the %s engine; run it again.", id, eng), "message")
    })

    # ---- cards 3-5: comparison, multiplicity, deviations and run history ---------------------------------
    # Each card re-derives from the replayed project and keeps its value in a reactiveVal that is written
    # only when it changed, so an unrelated project change redraws nothing. Nothing here runs sr_sap_check
    # or scans DuckDB. A derivation that fails is shown as its message (a character string).
    derive <- function(fn) {
      v <- shiny::reactiveVal(NULL)
      shiny::observe({
        p <- project()
        x <- if (is.null(p)) NULL else tryCatch(fn(p), error = function(e) conditionMessage(e))
        if (!identical(shiny::isolate(v()), x)) v(x)
      })
      v
    }
    cmp_v <- derive(function(p) .sr_an_compare(p, live()))
    fam_v <- derive(.sr_an_families)
    dev_v <- derive(sr_deviations)
    hist_v <- derive(function(p) .sr_an_history(p, live()))

    # a one-cell table: what a card shows instead of rows
    note_dt <- function(msg) {
      DT::datatable(data.frame(note = msg, stringsAsFactors = FALSE), rownames = FALSE, selection = "none",
                    escape = TRUE, options = list(dom = "t", ordering = FALSE), class = "compact stripe")
    }
    # a table card: the frame, or the note when it has no rows (or the derivation failed / no project)
    table_dt <- function(v, empty) {
      if (is.null(v)) return(note_dt("open a project first"))
      if (is.character(v)) return(note_dt(v))
      if (!nrow(v)) return(note_dt(empty))
      DT::datatable(v, rownames = FALSE, selection = "none", escape = TRUE,
                    options = list(dom = "tip", pageLength = 25L, scrollX = TRUE, ordering = FALSE),
                    class = "compact stripe")
    }
    output$compare <- DT::renderDT(table_dt(cmp_v(), "No primary or secondary analysis."), server = FALSE)
    output$deviations <- DT::renderDT(table_dt(dev_v(), "No deviation from the locked SAP."), server = FALSE)
    output$history <- DT::renderDT(table_dt(hist_v(), "No cancelled or interrupted run."), server = FALSE)

    output$families <- shiny::renderUI({
      f <- fam_v()
      if (is.null(f)) return(shiny::div(class = "text-muted", "open a project first"))
      if (is.character(f)) return(shiny::div(class = "text-danger", f))
      if (!length(f)) return(shiny::div(class = "text-muted", "No multiplicity family in the SAP."))
      do.call(shiny::tagList, lapply(f, function(fm) {
        body <- if (length(fm$waiting)) {
          shiny::div(class = "text-muted", paste0("Waiting for: ", paste(fm$waiting, collapse = ", ")))
        } else if (!is.data.frame(fm$table)) {
          shiny::div(class = "text-muted", if (!is.null(fm$error) && nzchar(fm$error)) fm$error else "No adjusted table is available for this family.")
        } else {
          tb <- fm$table
          tb$p <- format.pval(tb$p, digits = 3L, eps = 1e-4)
          DT::datatable(tb, rownames = FALSE, selection = "none", escape = TRUE,
                        options = list(dom = "t", pageLength = 100L, scrollX = TRUE, ordering = FALSE),
                        class = "compact stripe")
        }
        shiny::tagList(shiny::h6(class = "mt-3", sprintf("%s (%s)", fm$id, fm$method)), body)
      }))
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
          o$notice <- paste0(o$notice %||% sprintf("%s finished", id), " (in ", name, ")")
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
        try(sr_duck_close(p), silent = TRUE)                # in-memory connection: tidy, holds no file
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
      # while a queue runs the project is the read-only copy: say what is really in the way
      if (isTRUE(shiny::isolate(running()))) {
        return(.sr_ui_notify(paste0("Run: ", .sr_an_can_run(list(), TRUE)), "error"))
      }
      why <- .sr_an_can_run(p, busy_now())
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
      .sr_ui_notify(sprintf("Cancelling %s...", run$id), "message")
      k <- tryCatch(.sr_an_kill(jf), error = function(e) e)
      # a process survived: say so and wait for the worker's mirai before taking the project back
      if (inherits(k, "error")) {
        .sr_ui_notify(paste0("Cancel: ", conditionMessage(k)), "error")
      } else if (isFALSE(k)) {
        # no job file, or its PID is no longer the worker: nothing was killed; the worker's own result
        # is settled (as a cancel) when its mirai resolves
        .sr_ui_notify(sprintf(paste0("Cancel: no running worker was found for %s; it will be settled ",
                                     "when it ends"), run$id), "message")
      }
    })

    # Nothing is cancelled when the session ends: the worker finishes and records (its files it removes
    # itself); the queue goes with the session.
    session$onSessionEnded(function() shiny::isolate(run$queue <- character()))

    list(running = running)
  })
}
