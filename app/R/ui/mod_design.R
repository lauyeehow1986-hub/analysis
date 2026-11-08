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
.sr_design_editor_ui <- function(ns, row, info, editable = TRUE) {
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
    .sr_design_on_control(
      shiny::selectInput(ns("dec_choice"), "Choice", choices = info$options, selected = selected, width = "100%"),
      "select", disabled = if (!editable) NA),
    if (identical(row$status, "override_required")) {
      .sr_design_on_control(
        shiny::checkboxInput(ns("dec_override"), "Override the veto (a reason is required)", FALSE),
        "input", disabled = if (!editable) NA)
    }))
  shiny::div(parts)
}

# Add attributes to the form control(s) inside a shiny input tag (not to its outer div).
.sr_design_on_control <- function(tag, selector, ...) {
  htmltools::tagQuery(tag)$find(selector)$addAttrs(...)$allTags()
}

# ---- UI --------------------------------------------------------------------------------------------

mod_design_ui <- function(id) {
  ns <- shiny::NS(id)
  jev <- if (identical(sr_mode(), "connected")) .sr_ui_card("Ask Jev", shiny::uiOutput(ns("jev")))
  shiny::tagList(
    shiny::uiOutput(ns("banner")),
    do.call(bslib::layout_columns, c(
      list(col_widths = 12),
      list(
        .sr_ui_card("Research question", shiny::uiOutput(ns("pico_form"))),
        .sr_ui_card(
          "Decision table",
          shiny::uiOutput(ns("run_rules_btn")),
          DT::DTOutput(ns("table")),
          shiny::hr(),
          shiny::uiOutput(ns("editor")),
          shiny::uiOutput(ns("dec_reason_ui")),
          shiny::uiOutput(ns("dec_save_btn")),
          shiny::div(class = "text-danger small mt-2", shiny::textOutput(ns("dec_err"))))),
      if (!is.null(jev)) list(jev),
      list(
        .sr_ui_card(
          "Shadow summary",
          DT::DTOutput(ns("shadow")),
          shiny::actionButton(ns("shadow_export"), "Export shadow feed (JSON lines)",
                              class = "btn-outline-primary mt-2"),
          shiny::p(class = "small text-muted mt-2 mb-0", shiny::textOutput(ns("shadow_path")))))
    ))
  )
}

# ---- server ----------------------------------------------------------------------------------------

# A reactive TRUE/FALSE of .sr_ui_editable(project()) that invalidates its readers only when the value
# changes (a plain reactive() invalidates on every project change, which would re-render a card and wipe
# what the user typed or ticked).
.sr_design_editable <- function(project) {
  val <- shiny::reactiveVal(.sr_ui_editable(shiny::isolate(project())))
  shiny::observe({
    v <- .sr_ui_editable(project())
    if (!identical(shiny::isolate(val()), v)) val(v)
  })
  shiny::reactive(val())
}

# ---- Ask Jev (connected mode only) -----------------------------------------------------------------

# The four gates of .sr_jev_available, all checked (not only the first failing one), as
# list(mode, key, ack, consent), each list(ok, text). The key itself is never returned.
.sr_design_gates <- function(p) {
  gate <- function(ok, label, closed) list(ok = ok, text = if (ok) paste0(label, ": ok") else closed)
  key <- sr_opt("jev_key", "")
  key_ok <- is.character(key) && length(key) == 1L && !is.na(key) && nzchar(key)
  list(
    mode = gate(identical(sr_mode(), "connected"), "connected mode", "air-gap mode"),
    key = gate(key_ok, "key set", "no Jev key (set SR_JEV_KEY)"),
    ack = gate(!is.null(p) && !is.null(p$state$router$jev_ack), "DPA / IT acknowledged",
               "no DPA / IT acknowledgement for this project"),
    consent = gate(!is.null(p) && isTRUE(p$state$router$consent[[sr_router_session()]]),
                   "consent given in this session", "no consent in this session"))
}

# The pending payloads: one list(key, sha, body) per pending / override_required row of the decision table
# whose preview is non-NULL under `mask`.
.sr_design_payloads <- function(p, mask) {
  if (is.null(p)) return(list())
  tb <- sr_router_table(p)
  tb <- tb[tb$status %in% c("pending", "override_required"), , drop = FALSE]
  out <- list()
  for (i in seq_len(nrow(tb))) {
    pv <- sr_router_preview(p, tb$node[i], tb$table[i], tb$column[i], mask = mask)
    if (!is.null(pv)) out[[length(out) + 1L]] <- list(key = pv$key, sha = pv$sha, body = pv$body)
  }
  out
}

.sr_design_jev_server <- function(input, output, session, project, act) {
  ns <- session$ns
  editable <- .sr_design_editable(project)
  jerr <- shiny::reactiveVal("")
  asked <- shiny::reactiveVal(FALSE)
  off <- function(x) if (!editable()) shiny::tagAppendAttributes(x, disabled = NA) else x
  btn <- function(id, label, cls = "btn-outline-primary") {
    off(shiny::actionButton(ns(id), label, class = cls))
  }

  # The card. It depends on editable() alone, which changes value only when the project is locked or
  # unlocked, so the mask box and the ticks survive every other project change.
  output$jev <- shiny::renderUI({
    shiny::tagList(
      shiny::p(class = "small text-muted",
               "Jev is an optional cloud model. It only ever sees the payloads you tick below, exactly as shown,",
               "and its answer is a shadow prediction: it never decides anything."),
      shiny::uiOutput(ns("jev_gates")),
      shiny::div(class = "d-flex gap-2 mb-2",
                 btn("jev_ack", "Acknowledge DPA / IT sign-off"), btn("jev_consent", "Consent this session"),
                 btn("jev_withdraw", "Withdraw consent")),
      shiny::div(class = "d-flex gap-2 align-items-center mb-2",
                 btn("jev_ask", "Preview the payloads", "btn-primary"),
                 .sr_design_on_control(shiny::checkboxInput(ns("jev_mask"), "Mask names and values", FALSE),
                                       "input", disabled = if (!editable()) NA)),
      shiny::uiOutput(ns("jev_payloads")),
      .sr_design_on_control(
        shiny::checkboxGroupInput(ns("jev_pick"), "Send these payloads to Jev", choices = character()),
        "input", disabled = if (!editable()) NA),
      btn("jev_send", "Send the ticked payloads", "btn-primary"),
      shiny::div(class = "text-danger small mt-2", shiny::textOutput(ns("jev_err"))))
  })
  output$jev_err <- shiny::renderText(jerr())

  output$jev_gates <- shiny::renderUI({
    g <- .sr_design_gates(project())
    shiny::tags$ul(class = "list-unstyled small mb-2", lapply(g, function(x) {
      shiny::tags$li(shiny::span(class = if (x$ok) "text-success" else "text-danger",
                                 paste0(if (x$ok) "[ok] " else "[closed] ", x$text)))
    }))
  })

  payloads <- shiny::reactive({
    if (!asked()) return(list())
    .sr_design_payloads(project(), isTRUE(input$jev_mask))
  })
  shiny::observe({
    pl <- payloads()
    shas <- vapply(pl, function(x) x$sha, "")
    keep <- intersect(shiny::isolate(input$jev_pick), shas)
    lab <- vapply(pl, function(x) paste0(x$key, " (", substr(x$sha, 1L, 8L), ")"), "")
    shiny::updateCheckboxGroupInput(session, "jev_pick", choices = stats::setNames(shas, lab), selected = keep)
  })
  output$jev_payloads <- shiny::renderUI({
    pl <- payloads()
    if (!length(pl)) {
      return(shiny::p(class = "small text-muted",
                      if (asked()) "Nothing is pending." else "Press Preview to see what would be sent."))
    }
    shiny::tagList(lapply(pl, function(x) {
      body <- tryCatch(as.character(jsonlite::prettify(x$body)), error = function(e) x$body)
      shiny::div(class = "mb-2",
                 shiny::tags$strong(x$key), shiny::span(class = "small text-muted", paste0(" sha256 ", x$sha)),
                 shiny::tags$pre(class = "small border p-2 mb-0", style = "max-height:14em;overflow:auto;", body))
    }))
  })

  shiny::observeEvent(input$jev_ask, {
    jerr("")
    if (!editable()) {
      jerr(.sr_ui_refusal(shiny::isolate(project())) %||% "the plan can no longer be edited")
      return(invisible(NULL))
    }
    asked(TRUE)
  })

  # A core refusal is shown verbatim in jev_err.
  refuse_locked <- function() {
    if (editable()) return(FALSE)
    jerr(.sr_ui_refusal(shiny::isolate(project())) %||%
           "the SAP is locked; the plan can no longer be edited")
    TRUE
  }
  guarded <- function(label, fn) {
    jerr("")
    if (refuse_locked()) return(invisible(NULL))
    act(label, function(p) {
      tryCatch(fn(p), error = function(e) {
        jerr(conditionMessage(e))
        NULL
      })
    })
  }

  shiny::observeEvent(input$jev_ack, {
    shiny::showModal(shiny::modalDialog(
      title = "Acknowledge the DPA / IT sign-off",
      shiny::p("Record that the DPA / PDPA and the institutional IT sign-off for Jev are documented."),
      shiny::textAreaInput(ns("jev_ack_reason"), "Reference (required)", rows = 2, width = "100%"),
      footer = shiny::tagList(shiny::modalButton("Cancel"),
                              shiny::actionButton(ns("jev_ack_ok"), "Acknowledge", class = "btn-primary")),
      easyClose = TRUE))
  })
  shiny::observeEvent(input$jev_ack_ok, {
    r <- input$jev_ack_reason
    guarded("Acknowledge Jev", function(p) sr_router_jev_ack(p, if (is.null(r)) "" else r))
    if (!nzchar(shiny::isolate(jerr()))) shiny::removeModal()
  })
  shiny::observeEvent(input$jev_consent, {
    shiny::showModal(shiny::modalDialog(
      title = "Consent for this session",
      shiny::p("Allow payloads you tick to be sent to Jev (a cloud service) in this session. Consent ends with the session."),
      footer = shiny::tagList(shiny::modalButton("Cancel"),
                              shiny::actionButton(ns("jev_consent_ok"), "I consent", class = "btn-primary")),
      easyClose = TRUE))
  })
  shiny::observeEvent(input$jev_consent_ok, {
    guarded("Consent to Jev", function(p) sr_router_consent(p, TRUE))
    if (!nzchar(shiny::isolate(jerr()))) shiny::removeModal()
  })
  shiny::observeEvent(input$jev_withdraw, {
    guarded("Withdraw consent", function(p) sr_router_consent(p, FALSE))
  })

  shiny::observeEvent(input$jev_send, {
    jerr("")
    pick <- input$jev_pick
    if (!editable()) {
      jerr(.sr_ui_refusal(shiny::isolate(project())) %||% "the plan can no longer be edited")
      return(invisible(NULL))
    }
    if (!length(pick)) {
      jerr("tick at least one payload to send")
      return(invisible(NULL))
    }
    withr::with_options(list(sr.router_mask = isTRUE(input$jev_mask)),
      guarded("Ask Jev", function(p) sr_router_run(p, backends = "jev", approve = pick)))
  })
}

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
    # The selected row is kept by its key, so a table refresh (every decision rewrites the project and
    # DT re-renders with an empty selection) neither loses the row nor clears the message shown for it.
    sel <- shiny::reactiveVal(NULL)
    selected_row <- shiny::reactive({
      tb <- table_df()
      k <- sel()
      if (is.null(tb) || is.null(k) || !k %in% tb$key) return(NULL)
      tb[match(k, tb$key), , drop = FALSE]
    })
    shiny::observeEvent(input$table_rows_selected, {
      tb <- shiny::isolate(table_df())
      i <- input$table_rows_selected
      if (is.null(tb) || length(i) != 1L || is.na(i) || i < 1L || i > nrow(tb)) return(invisible(NULL))
      if (!identical(tb$key[i], shiny::isolate(sel()))) {
        sel(tb$key[i])
        err("")
      }
    }, ignoreNULL = FALSE, priority = 10)
    editable <- .sr_design_editable(project)

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
        .sr_design_on_control(
          shiny::textInput(ns(paste0("pico_", f)), .sr_design_pico_labels[[f]], value = val(f), width = "100%"),
          "input", maxlength = SR_PICO_MAX_CHARS)
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
      k <- shiny::isolate(sel())
      tb <- shiny::isolate(table_df())
      DT::datatable(.sr_design_table_view(table_df()), rownames = FALSE, escape = FALSE,
                    selection = list(mode = "single",
                                     selected = if (!is.null(k) && !is.null(tb) && k %in% tb$key) match(k, tb$key)),
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
      .sr_design_editor_ui(ns, row, info, editable())
    })
    output$dec_reason_ui <- shiny::renderUI({
      ed <- editable()
      .sr_design_on_control(
        shiny::textAreaInput(ns("dec_reason"), "Reason (required for an override)", rows = 2, width = "100%"),
        "textarea", maxlength = SR_PICO_MAX_CHARS, disabled = if (!ed) NA)
    })

    output$dec_err <- shiny::renderText(err())

    shiny::observeEvent(input$dec_save, {
      row <- selected_row()
      err("")
      ok <- FALSE
      res <- act("Decide", function(p) {
        if (is.null(row)) {
          err("Select a row of the table first.")
          return(NULL)
        }
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
      refuse <- .sr_ui_refusal(p, function() busy())
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

    # Air-gap: no Jev observer, preview or transport code is ever created.
    if (identical(sr_mode(), "connected")) .sr_design_jev_server(input, output, session, project, act)
    invisible(NULL)
  })
}
