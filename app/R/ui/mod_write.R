# ui/mod_write.R - Shiny module: the Write panel. Cards: Options and gates, Check (the claim guard), Preview,
# Checklist, Export and history.
#
# mod_write_server(id, project, busy, settings = function() 0L) takes the project reactiveVal of
# mod_project_server(), `busy` (a reactive or function, TRUE while any job runs) and `settings`, a function
# whose value changes when the settings change (it is part of the model cache key).
#
# Check, Preview and Export are refused with the first failing gate (.sr_wp_gate_reason) or the options
# error. The panel keeps one cached model (key: options, number of replayed events, digits, settings) that
# Check, Preview and the checklist share; Export always rebuilds inside sr_export and then drops the cache.
# Every number shown comes from the core; the panel only formats frames.

# ---- small helpers -----------------------------------------------------------------------------------

# The number of replayed events of a project (part of the cache key).
.sr_wm_n_events <- function(p) {
  if (is.null(p)) return(0L)
  length(tryCatch(sr_events_read(p$paths$events), error = function(e) list()))
}

# A data frame as a plain HTML table (cells as text); NULL gives NULL.
.sr_wm_table <- function(df) {
  if (is.null(df)) return(NULL)
  cell <- function(x) if (is.na(x)) "" else as.character(x)
  head <- htmltools::tags$tr(lapply(names(df), htmltools::tags$th))
  rows <- lapply(seq_len(nrow(df)), function(i) {
    htmltools::tags$tr(lapply(seq_along(df), function(j) htmltools::tags$td(cell(df[[j]][i]))))
  })
  htmltools::tags$table(class = "table table-sm", htmltools::tags$thead(head), htmltools::tags$tbody(rows))
}

# The input id suffix of an export id.
.sr_wm_nm <- function(id) gsub("-", "_", id, fixed = TRUE)

# ---- UI ----------------------------------------------------------------------------------------------

mod_write_ui <- function(id) {
  ns <- shiny::NS(id)
  pr <- .sr_wp_presets()
  shiny::div(
    class = "sr-write-stack",
    .sr_ui_card("Options and gates",
      shiny::selectInput(ns("preset"), "Journal preset", choices = stats::setNames(pr$name, pr$label),
                         selected = if ("default" %in% pr$name) "default" else pr$name[1L]),
      shiny::numericInput(ns("suppress_k"), "Suppress counts below k", value = SR_SDC_K, min = 1, step = 1),
      shiny::checkboxInput(ns("table2"), "Label small cells of Table 2 (unticked: suppress them)", value = TRUE),
      shiny::numericInput(ns("alpha"), "Alpha", value = 0.05, min = 0, max = 1, step = 0.01),
      shiny::div(class = "text-danger mb-2", shiny::textOutput(ns("opts_error"))),
      shiny::uiOutput(ns("gates"))
    ),
    .sr_ui_card("Check",
      shiny::actionButton(ns("check"), "Check manuscript", class = "btn-outline-primary mb-2"),
      shiny::uiOutput(ns("guard"))
    ),
    .sr_ui_card("Preview",
      shiny::actionButton(ns("preview"), "Preview", class = "btn-outline-primary mb-2"),
      shiny::uiOutput(ns("preview_body"))
    ),
    .sr_ui_card("Checklist", shiny::uiOutput(ns("checklist"))),
    .sr_ui_card("Export and history",
      shiny::actionButton(ns("export"), "Export manuscript", class = "btn-primary mb-2"),
      shiny::uiOutput(ns("history")),
      shiny::uiOutput(ns("verify"))
    )
  )
}

# ---- server ------------------------------------------------------------------------------------------

mod_write_server <- function(id, project, busy, settings = function() 0L) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    busy_now <- function() isTRUE(tryCatch(shiny::isolate(busy()), error = function(e) FALSE))

    # ---- options and gates
    op <- shiny::reactive(.sr_wp_opts(input$preset %||% "default", input$suppress_k %||% SR_SDC_K,
                                      input$table2 %||% TRUE, input$alpha %||% 0.05))
    gates <- shiny::reactive(.sr_wp_gates(project(), isTRUE(busy())))
    output$opts_error <- shiny::renderText(op()$error %||% "")
    output$gates <- shiny::renderUI({
      g <- gates()
      shiny::tags$ul(class = "list-unstyled small mb-0", lapply(seq_len(nrow(g)), function(i) {
        shiny::tags$li(class = if (g$ok[i]) "text-success" else "text-danger",
                       if (g$ok[i]) g$gate[i] else paste0(g$gate[i], ": ", g$reason[i]))
      }))
    })
    why_off <- shiny::reactive(.sr_wp_gate_reason(gates()) %||% op()$error)
    shiny::observe({
      off <- !is.null(why_off())
      for (b in c("check", "preview", "export")) shiny::updateActionButton(session, b, disabled = off)
    })
    # why a button is refused right now, or NULL
    refusal <- function() {
      g <- .sr_wp_gates(shiny::isolate(project()), busy_now())
      .sr_wp_gate_reason(g) %||% shiny::isolate(op())$error
    }

    # ---- the model cache: one model per key, shared by Check, Preview and the checklist
    cache <- new.env(parent = emptyenv())
    ver <- shiny::reactiveVal(0L)   # bumps when the cache is filled or dropped
    bump <- function() ver(shiny::isolate(ver()) + 1L)
    fig_dir <- tempfile("wp-fig-")
    dir.create(fig_dir)
    session$onSessionEnded(function() unlink(fig_dir, recursive = TRUE))
    key_now <- function() {
      list(opts = shiny::isolate(op())$opts, n_events = .sr_wm_n_events(shiny::isolate(project())),
           digits = sr_opt("digits", 2), settings = settings())
    }
    # the model for the current key (built once per key); NULL after a refusal, which is shown
    model <- function() {
      k <- key_now()
      if (!is.null(cache$key) && identical(cache$key, k)) return(cache$model)
      p <- shiny::isolate(project())
      m <- .sr_ui_try("Check manuscript", function() sr_doc_model(p, k$opts))
      cache$key <- cache$model <- cache$guard <- cache$preview <- NULL
      if (!is.null(m)) {
        cache$key <- k
        cache$model <- m
      }
      bump()
      m
    }
    drop_cache <- function() {
      cache$key <- cache$model <- cache$guard <- cache$preview <- NULL
      bump()
    }
    # TRUE when the cached model is still the model of the current inputs and project
    fresh <- function() {
      !is.null(cache$key) && identical(cache$key, tryCatch(key_now(), error = function(e) NULL))
    }
    refuse <- function(label) {
      why <- refusal()
      if (is.null(why)) return(FALSE)
      .sr_ui_notify(paste0(label, ": ", why), "error")
      TRUE
    }
    # the reactive inputs the cached views depend on
    deps <- function() {
      ver(); project(); input$preset; input$suppress_k; input$table2; input$alpha
      invisible(NULL)
    }

    # ---- check
    shiny::observeEvent(input$check, {
      if (refuse("Check manuscript")) return(invisible(NULL))
      m <- model()
      if (is.null(m)) return(invisible(NULL))
      cache$guard <- .sr_ui_try("Check manuscript", function() .sr_wp_guard_rows(sr_claim_guard(m)))
      bump()
    })
    output$guard <- shiny::renderUI({
      deps()
      if (!fresh() || is.null(cache$guard)) {
        return(shiny::p(class = "text-muted small", "Press Check to run the claim guard."))
      }
      g <- cache$guard
      if (!nrow(g)) return(shiny::p(class = "text-success", "The claim guard found nothing."))
      .sr_wm_table(g)
    })

    # ---- preview (rendered once per model, never on a plain invalidation)
    shiny::observeEvent(input$preview, {
      if (refuse("Preview")) return(invisible(NULL))
      m <- model()
      if (is.null(m)) return(invisible(NULL))
      if (is.null(cache$preview)) {
        cache$preview <- .sr_ui_try("Preview", function() .sr_wp_preview(m, fig_dir, sr_journal_preset(m$meta$preset %||% "default")))
      }
      bump()
    })
    output$preview_body <- shiny::renderUI({
      deps()
      if (!fresh() || is.null(cache$preview)) {
        return(shiny::p(class = "text-muted small", "Press Preview to see the manuscript as the document shows it."))
      }
      cache$preview
    })

    # ---- checklist (from the cached model; nothing is built here)
    output$checklist <- shiny::renderUI({
      deps()
      if (!fresh()) return(shiny::p(class = "text-muted small", "Press Check or Preview to build the checklist."))
      cl <- .sr_wp_checklist(cache$model)
      if (!is.null(cl$df)) return(.sr_wm_table(cl$df))
      shiny::p(cl$note %||% "This manuscript has no reporting checklist.")
    })

    # ---- export
    shiny::observeEvent(input$export, {
      if (refuse("Export")) return(invisible(NULL))
      p <- shiny::isolate(project())
      opts <- shiny::isolate(op())$opts
      res <- .sr_ui_try("Export", function() sr_export(p, opts))
      drop_cache()
      if (is.list(res) && !is.null(res$paths)) {
        project(res)
        ex <- res$state$exports
        .sr_ui_notify(paste0("Export written: ", ex[[length(ex)]]$id))
      }
    })

    # ---- history, downloads, open, verify
    hist <- .sr_ui_flag(project, function(p) {
      if (is.null(p)) .sr_wp_history(list(state = list())) else .sr_wp_history(p)
    })
    shown <- shiny::reactiveVal(NULL)   # list(id, v) of the last verification
    reg <- new.env(parent = emptyenv())
    export_of <- function(eid) {
      p <- shiny::isolate(project())
      ex <- Filter(function(e) identical(as.character(e$id), eid), p$state$exports %||% list())
      if (!length(ex)) stop("export '", eid, "' is not in this project", call. = FALSE)
      ex[[1L]]
    }
    register <- function(eid, kind) {
      nm <- .sr_wm_nm(eid)
      if (isTRUE(reg[[nm]])) return(invisible(NULL))
      reg[[nm]] <- TRUE
      output[[paste0("dl_", nm)]] <- shiny::downloadHandler(
        filename = function() {
          if (identical(kind, "manuscript")) paste0(eid, ".zip") else basename(export_of(eid)$files[[1L]]$path)
        },
        content = function(file) {
          p <- shiny::isolate(project())
          if (identical(kind, "manuscript")) return(sr_export_zip(p, eid, file))
          f <- file.path(p$paths$dir, export_of(eid)$files[[1L]]$path)
          if (!file.exists(f)) stop("the file of export '", eid, "' is missing on disk", call. = FALSE)
          file.copy(f, file, overwrite = TRUE)
        })
      shiny::observeEvent(input[[paste0("open_", nm)]], {
        e <- .sr_ui_try("Open folder", function() export_of(eid))
        if (is.null(e)) return(invisible(NULL))
        dir <- dirname(file.path(shiny::isolate(project())$paths$dir, e$files[[1L]]$path))
        .sr_ui_try("Open folder", function() .sr_ui_open_folder(dir))
      }, ignoreInit = TRUE)
      shiny::observeEvent(input[[paste0("verify_", nm)]], {
        v <- .sr_ui_try("Verify", function() sr_export_verify(shiny::isolate(project()), eid))
        if (!is.null(v)) shown(list(id = eid, v = v))
      }, ignoreInit = TRUE)
      invisible(NULL)
    }
    shiny::observe({
      h <- hist()
      for (i in seq_len(nrow(h))) register(h$id[i], h$kind[i])
    })
    output$history <- shiny::renderUI({
      h <- hist()
      if (!nrow(h)) return(shiny::p(class = "text-muted small", "Nothing has been exported yet."))
      row <- function(i) {
        nm <- .sr_wm_nm(h$id[i])
        gone <- identical(h$status[i], "missing on disk")
        dl <- shiny::downloadButton(ns(paste0("dl_", nm)), "Download", class = "btn-sm btn-outline-secondary")
        if (gone) dl <- htmltools::tagAppendAttributes(dl, class = "disabled", `aria-disabled` = "true")
        shiny::tags$tr(
          shiny::tags$td(h$kind[i]), shiny::tags$td(h$id[i]), shiny::tags$td(h$created[i] %||% ""),
          shiny::tags$td(if (is.na(h$preset[i])) "" else h$preset[i]), shiny::tags$td(h$n_files[i]),
          shiny::tags$td(class = if (gone) "text-danger" else "", h$status[i]),
          shiny::tags$td(class = "d-flex gap-1", dl,
                         shiny::actionButton(ns(paste0("open_", nm)), "Open folder",
                                             class = "btn-sm btn-outline-secondary", disabled = if (gone) NA else NULL),
                         shiny::actionButton(ns(paste0("verify_", nm)), "Verify",
                                             class = "btn-sm btn-outline-secondary")))
      }
      shiny::tags$table(class = "table table-sm",
        shiny::tags$thead(shiny::tags$tr(lapply(c("Kind", "Id", "Created", "Preset", "Files", "Status", ""),
                                                shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_len(nrow(h)), row)))
    })
    output$verify <- shiny::renderUI({
      s <- shown()
      if (is.null(s)) return(NULL)
      l <- .sr_wp_verify_lines(s$v)
      shiny::tagList(
        shiny::h6(paste0("Verification of ", s$id)),
        if (length(l$problems)) {
          shiny::tags$ul(class = "text-danger", lapply(l$problems, shiny::tags$li))
        } else {
          shiny::p(class = "text-success", "Every file matches its recorded hash.")
        },
        if (!is.null(l$numbers) && nrow(l$numbers)) {
          shiny::tagList(shiny::p(class = "text-danger", "These numbers no longer re-derive from the project:"),
                         .sr_wm_table(l$numbers))
        } else if (isTRUE(s$v$numbers_ok) && !is.null(s$v$numbers)) {
          shiny::p(class = "text-success", "Every number re-derives from the project.")
        })
    })
    invisible(NULL)
  })
}
