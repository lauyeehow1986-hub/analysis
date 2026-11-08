# ui/mod_design.R - Shiny module: the Design panel. The research question (PICO), the expert decision
# table of the router (run the rules, decide a row, see the model's shadow prediction) and the shadow
# summary with its golden-set export.
#
# mod_design_server(id, project, busy) takes the reactiveVal of mod_project_server() and keeps no state of
# its own beyond the last refusal message. Every change goes through the core (which appends the event); the
# model's prediction is shown as "shadow only, not applied" and never decides anything. After the SAP lock
# the table is the frozen snapshot and every edit control is off.

# ---- small pure helpers ----------------------------------------------------------------------------

.sr_design_pico_labels <- c(population = "Population", exposure = "Exposure / intervention",
                            comparator = "Comparator", outcome = "Outcome", timeframe = "Time frame",
                            setting = "Setting", period = "Study period", aim = "Aim")

# The non-empty PICO fields of the inputs, as a named list of trimmed strings.
.sr_design_pico_fields <- function(input) {
  vals <- lapply(stats::setNames(SR_PICO_FIELDS, SR_PICO_FIELDS),
                 function(f) .sr_ui_text(input[[paste0("pico_", f)]]))
  vals[nzchar(unlist(vals, use.names = FALSE))]
}

# The decision table as a data frame for display: status as a badge, other columns HTML-escaped.
.sr_design_table_view <- function(tb) {
  esc <- function(x) htmltools::htmlEscape(ifelse(is.na(x), "", as.character(x)))
  if (is.null(tb) || !nrow(tb)) {
    return(data.frame(Decision = character(), Table = character(), Column = character(), Status = character(),
                      Choice = character(), Resolver = character(), Suggested = character(),
                      stringsAsFactors = FALSE))
  }
  badge <- function(s) {
    cls <- switch(s, decided = "bg-success", stale = "bg-warning text-dark", override_required = "bg-danger",
                  waiting = "bg-secondary", "bg-info text-dark")
    sprintf("<span class=\"badge %s\" data-status=\"%s\">%s</span>", cls, s, s)
  }
  data.frame(Decision = esc(tb$label), Table = esc(tb$table), Column = esc(tb$column),
             Status = vapply(tb$status, badge, ""), Choice = esc(tb$choice), Resolver = esc(tb$resolver),
             Suggested = esc(tb$suggested), stringsAsFactors = FALSE)
}

# The editor body for one table row (a one-row data frame of sr_router_table) and its editor info.
.sr_design_editor_ui <- function(ns, row, info) {
  line <- function(...) shiny::tags$p(class = "small mb-1", ...)
  where <- if (!is.na(row$table) && nzchar(row$table)) {
    paste0(" (", row$table, if (!is.na(row$column) && nzchar(row$column)) paste0(".", row$column) else "", ")")
  } else {
    ""
  }
  parts <- list(shiny::tags$p(class = "mb-1", shiny::tags$strong(row$label),
                              shiny::tags$span(class = "text-muted", where)))
  if (identical(row$status, "waiting")) {
    parts <- c(parts, list(line(paste0("waiting on: ", paste(info$waiting_on, collapse = ", ")))))
    return(shiny::div(parts))
  }
  if (identical(row$status, "stale")) {
    parts <- c(parts, list(shiny::div(class = "alert alert-warning py-1 small mb-2",
      sprintf("Decided as '%s' under older inputs: decide again.", row$choice))))
  }
  if (!is.na(row$suggested)) parts <- c(parts, list(line(paste0("Suggested: ", row$suggested))))
  if (!is.na(row$model_choice)) {
    parts <- c(parts, list(line(sprintf("Model prediction (%s): %s%s - shadow only, not applied",
      row$backend, row$model_choice,
      if (is.na(row$confidence)) "" else sprintf(", confidence %.2f", row$confidence)))))
  }
  selected <- if (identical(row$status, "decided") && !is.na(row$choice)) {
    row$choice
  } else if (!is.na(row$suggested)) {
    row$suggested
  }
  parts <- c(parts, list(
    shiny::selectInput(ns("dec_choice"), "Choice", choices = info$options, selected = selected, width = "100%"),
    if (identical(row$status, "override_required")) {
      shiny::checkboxInput(ns("dec_override"), "Override the veto (a reason is required)", FALSE)
    }))
  shiny::div(parts)
}

# ---- UI --------------------------------------------------------------------------------------------

mod_design_ui <- function(id) {
  ns <- shiny::NS(id)
  jev <- if (identical(sr_mode(), "connected")) .sr_ui_card("Ask Jev", shiny::uiOutput(ns("jev")))
  shiny::tagList(
    shiny::uiOutput(ns("banner")),
    bslib::layout_columns(
      col_widths = 12,
      .sr_ui_card("Research question", shiny::uiOutput(ns("pico_form"))),
      .sr_ui_card(
        "Decision table",
        shiny::uiOutput(ns("run_rules_btn")),
        DT::DTOutput(ns("table")),
        shiny::hr(),
        shiny::uiOutput(ns("editor")),
        shiny::textAreaInput(ns("dec_reason"), "Reason (required for an override)", rows = 2, width = "100%"),
        shiny::uiOutput(ns("dec_save_btn")),
        shiny::div(class = "text-danger small mt-2", shiny::textOutput(ns("dec_err")))),
      jev,
      .sr_ui_card(
        "Shadow summary",
        DT::DTOutput(ns("shadow")),
        shiny::actionButton(ns("shadow_export"), "Export shadow feed (JSON lines)",
                            class = "btn-outline-primary mt-2"),
        shiny::p(class = "small text-muted mt-2 mb-0", shiny::textOutput(ns("shadow_path"))))
    )
  )
}

# ---- server ----------------------------------------------------------------------------------------

# The Ask Jev card server. A stub here; Task 5 fills it in.
.sr_design_jev_server <- function(input, output, session, project, act) invisible(NULL)

mod_design_server <- function(id, project, busy = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    act <- .sr_ui_actor(project, busy = function() busy())
    err <- shiny::reactiveVal("")
    shadow_path <- shiny::reactiveVal("")

    # The PICO form re-renders only when the project or its editability changes (not on every event), so
    # an unsaved edit is not wiped by another action.
    form_key <- shiny::reactive({
      p <- project()
      list(dir = if (is.null(p)) NULL else p$paths$dir, editable = .sr_ui_editable(p))
    })
    table_df <- shiny::reactive({
      p <- project()
      if (is.null(p)) NULL else sr_router_table(p)
    })
    selected_row <- shiny::reactive({
      tb <- table_df()
      i <- input$table_rows_selected
      if (is.null(tb) || length(i) != 1L || is.na(i) || i < 1L || i > nrow(tb)) return(NULL)
      tb[i, , drop = FALSE]
    })
    shiny::observeEvent(input$table_rows_selected, err(""), ignoreNULL = FALSE)

    output$banner <- shiny::renderUI(.sr_ui_readonly_banner(project()))

    # ---- research question
    output$pico_form <- shiny::renderUI({
      k <- form_key()
      p <- shiny::isolate(project())
      if (is.null(p)) return(shiny::p(class = "text-muted", "Open a project first."))
      q <- sr_pico(p)
      val <- function(f) if (is.null(q) || is.na(q[[f]])) "" else q[[f]]
      if (!k$editable) {
        return(shiny::tags$dl(lapply(SR_PICO_FIELDS, function(f) {
          shiny::tagList(
            shiny::tags$dt(.sr_design_pico_labels[[f]]),
            shiny::tags$dd(if (nzchar(val(f))) val(f) else shiny::span(class = "text-muted", "(not set)")))
        })))
      }
      fields <- lapply(setdiff(SR_PICO_FIELDS, "aim"), function(f) {
        shiny::tagAppendAttributes(
          shiny::textInput(ns(paste0("pico_", f)), .sr_design_pico_labels[[f]], value = val(f), width = "100%"),
          maxlength = SR_PICO_MAX_CHARS)
      })
      shiny::tagList(
        fields,
        shiny::selectInput(ns("pico_aim"), .sr_design_pico_labels[["aim"]], choices = c("", SR_PURPOSES),
                           selected = val("aim"), width = "100%"),
        shiny::actionButton(ns("pico_save"), "Save research question", class = "btn-primary"))
    })
    shiny::observeEvent(input$pico_save, {
      f <- .sr_design_pico_fields(input)
      act("Save research question", function(p) sr_pico_set(p, .list = f))
    })

    # ---- decision table
    btn <- function(bid, label, cls) {
      shiny::actionButton(ns(bid), label, class = cls,
                          disabled = !.sr_ui_editable(project()))
    }
    output$run_rules_btn <- shiny::renderUI(btn("run_rules", "Run rules", "btn-primary mb-2"))
    output$dec_save_btn <- shiny::renderUI(btn("dec_save", "Save decision", "btn-primary"))

    output$table <- DT::renderDT({
      DT::datatable(.sr_design_table_view(table_df()), rownames = FALSE, escape = FALSE,
                    selection = "single",
                    options = list(dom = "tip", pageLength = 15L, scrollX = TRUE, ordering = FALSE),
                    class = "compact stripe")
    })

    shiny::observeEvent(input$run_rules, {
      act("Run rules", function(p) sr_router_run(p))
    })

    output$editor <- shiny::renderUI({
      row <- selected_row()
      if (is.null(row)) return(shiny::p(class = "text-muted", "Select a row of the table to decide it."))
      na_if_empty <- function(x) if (is.na(x) || !nzchar(x)) NA else x
      info <- sr_router_editor(project(), row$node, na_if_empty(row$table), na_if_empty(row$column))
      .sr_design_editor_ui(ns, row, info)
    })

    output$dec_err <- shiny::renderText(err())

    shiny::observeEvent(input$dec_save, {
      row <- selected_row()
      if (is.null(row)) {
        err("Select a row of the table first.")
        return(invisible(NULL))
      }
      err("")
      ok <- FALSE
      res <- act("Decide", function(p) {
        tryCatch({
          out <- sr_router_decide(p, row$node, input$dec_choice, input$dec_reason,
                                  table = row$table, column = row$column,
                                  override = isTRUE(input$dec_override))
          ok <<- TRUE
          out
        }, sr_router_vetoed = function(e) {
          err(conditionMessage(e))
          e$project
        }, error = function(e) {
          err(conditionMessage(e))
          NULL
        })
      })
      if (ok) shiny::updateTextAreaInput(session, "dec_reason", value = "")
      invisible(res)
    })

    # ---- shadow summary
    output$shadow <- DT::renderDT({
      p <- project()
      df <- if (is.null(p)) {
        data.frame(node = character(), backend = character(), n = integer(), decided = integer(),
                   agree_rate = numeric(), would_act_rate = numeric(), act_accuracy = numeric(),
                   error_rate = numeric(), stringsAsFactors = FALSE)
      } else {
        sr_router_shadow_summary(p)
      }
      for (k in intersect(c("agree_rate", "would_act_rate", "act_accuracy", "error_rate"), names(df))) {
        df[[k]] <- round(df[[k]], 3)
      }
      .sr_ui_dt(df)
    })
    output$shadow_path <- shiny::renderText(shadow_path())

    shiny::observeEvent(input$shadow_export, {
      label <- "Export shadow feed"
      p <- shiny::isolate(project())
      refuse <- if (is.null(p)) {
        "open a project first"
      } else if (isTRUE(busy())) {
        "wait for the import to finish"
      } else if (isTRUE(p$readonly)) {
        "the project is read-only (locked by another session)"
      }
      if (!is.null(refuse)) {
        .sr_ui_notify(paste0(label, ": ", refuse), "error")
        return(invisible(NULL))
      }
      path <- .sr_ui_try(label, function() {
        dir <- file.path(p$paths$out, "router")
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        sr_router_shadow_export(
          p, file.path(dir, sprintf("shadow-%s.jsonl", format(Sys.time(), "%Y%m%d-%H%M%S"))))
      })
      if (!is.null(path)) shadow_path(path)
    })

    .sr_design_jev_server(input, output, session, project, act)
    invisible(NULL)
  })
}
