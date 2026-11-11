# ui/mod_settings.R - Shiny module: the Settings panel (session-only settings, see ui/settings_view.R).
#
# mod_settings_server(id, busy) takes `busy` (a reactive or function, TRUE while any job runs) and returns
# list(version): a counter reactive that goes up by one after every successful set or reset. Write, Archive
# and the top-bar badges watch it.
#
# Edits are options(sr.<name>) for this session only: nothing is written to disk, nothing is a project event,
# and the audit log never records them. The Jev key is a password input; it is cleared right after a set, is
# held only as options(sr.jev_key), and is never rendered back (the table says "set" or "not set"). While a job
# runs every set and reset is refused and the editors are disabled (a worker already holds its snapshot).

mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    .sr_ui_card("Effective settings",
      shiny::p(class = "text-muted small",
               "Edits last for this session only; a restart starts again from the environment and the defaults.",
               "Settings are not project events."),
      shiny::tableOutput(ns("table"))),
    .sr_ui_card("Edit", shiny::uiOutput(ns("editors")))
  )
}

# The editor of one setting, filled with its current value `cur` (NULL = nothing to show; never the Jev key).
.sr_set_editor <- function(ns, name, editor, cur) {
  id <- ns(paste0("val_", name))
  switch(editor,
    choice = shiny::selectInput(id, NULL, selected = cur, width = "100%",
                                choices = switch(name, mode = c("airgap", "connected"),
                                                 profile = c("auto", SR_PROFILES))),
    logical = shiny::selectInput(id, NULL, choices = c("off", "on"), selected = cur, width = "100%"),
    integer = shiny::numericInput(id, NULL, value = cur, min = 0, step = 1, width = "100%"),
    number = shiny::numericInput(id, NULL, value = cur, width = "100%"),
    secret = shiny::passwordInput(id, NULL, value = "", width = "100%", placeholder = "paste the key"),
    date = shiny::textInput(id, NULL, value = cur %||% "", width = "100%", placeholder = "YYYY-MM-DD"),
    shiny::textInput(id, NULL, value = cur %||% "", width = "100%")
  )
}

mod_settings_server <- function(id, busy) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    version <- shiny::reactiveVal(0L)
    edited <- shiny::reactiveVal(character())
    cat <- .sr_set_catalogue()
    # busy_now() is isolated (for handlers); the editors read busy() reactively so they follow a job's start and end
    busy_now <- function() isTRUE(tryCatch(shiny::isolate(busy()), error = function(e) FALSE))
    busy_live <- function() isTRUE(tryCatch(busy(), error = function(e) FALSE))

    # the value an editor starts with: the effective value, except that the key and an unset actor show nothing
    current <- function(nm, editor) {
      if (nm %in% c("jev_key")) return(NULL)
      if (nm == "mode") return(tryCatch(sr_mode(), error = function(e) "airgap"))
      if (nm == "profile") {
        p <- sr_opt("profile", "")
        return(if (is.character(p) && length(p) == 1L && nzchar(p)) p else "auto")
      }
      if (nm %in% c("user", "cran_snapshot") && identical(sr_opt_source(nm), "default")) return(NULL)
      v <- sr_opt(nm, cat$default[[match(nm, cat$name)]])
      if (editor == "logical") return(if (isTRUE(as.logical(v))) "on" else "off")
      if (editor %in% c("integer", "number")) return(suppressWarnings(as.numeric(v)))
      if (length(v) != 1L || is.na(v)) NULL else as.character(v)
    }

    output$table <- shiny::renderTable({
      version()
      edited()
      .sr_set_table(edited())
    }, striped = TRUE, spacing = "s", width = "100%")

    output$editors <- shiny::renderUI({
      version()
      locked <- busy_live()
      rows <- lapply(seq_len(nrow(cat)), function(i) {
        nm <- cat$name[i]
        ed <- cat$editor[i]
        note <- .sr_set_note(nm)
        warn <- if (nm == "user") .sr_ar_actor_warning()
        shiny::div(class = "mb-3",
          shiny::tags$label(class = "form-label fw-semibold", cat$label[i]),
          shiny::div(class = "d-flex gap-2 align-items-start",
            shiny::div(class = "flex-grow-1", .sr_set_editor(ns, nm, ed, current(nm, ed))),
            shiny::actionButton(ns(paste0("set_", nm)), "Set", class = "btn-primary"),
            shiny::actionButton(ns(paste0("reset_", nm)), "Reset", class = "btn-outline-secondary")),
          if (!is.null(note)) shiny::p(class = "text-muted small mb-0", note),
          if (!is.null(warn)) shiny::div(class = "alert alert-warning small mt-1 mb-0", warn))
      })
      if (locked) {
        shiny::tagList(shiny::p(class = "text-info small", .sr_ui_busy_text),
                       shiny::tags$fieldset(disabled = NA, rows))
      } else {
        shiny::tagList(rows)
      }
    })

    refuse <- function() {
      if (!busy_now()) return(FALSE)
      .sr_ui_notify(paste0("Settings: ", .sr_ui_busy_text), "error")
      TRUE
    }
    changed <- function(nm, add = TRUE) {
      edited(if (add) union(edited(), nm) else setdiff(edited(), nm))
      version(version() + 1L)
    }
    fail <- function(nm, e) .sr_ui_notify(paste0("Settings: ", conditionMessage(e)), "error")

    # apply `value` to `nm`, report an error, count a success
    do_apply <- function(nm, value) {
      ok <- tryCatch({
        .sr_set_apply(nm, value)
        TRUE
      }, error = function(e) {
        fail(nm, e)
        FALSE
      })
      if (!ok) return(invisible(FALSE))
      if (identical(nm, "mode") && identical(value, "airgap")) {
        .sr_set_option("jev_key", NULL)
        edited(setdiff(edited(), "jev_key"))
      }
      if (identical(nm, "profile") && identical(value, "auto")) {
        changed(nm, add = FALSE)
      } else {
        changed(nm)
      }
      if (identical(nm, "jev_key")) shiny::updateTextInput(session, "val_jev_key", value = "")
      invisible(TRUE)
    }

    lapply(cat$name, function(nm) {
      shiny::observeEvent(input[[paste0("set_", nm)]], {
        if (refuse()) return()
        value <- input[[paste0("val_", nm)]]
        if (identical(nm, "mode") && identical(.sr_set_str(value), "connected")) {
          shiny::showModal(shiny::modalDialog(
            title = "Switch to connected mode",
            shiny::p("Connected mode allows network calls (Ask Jev, behind its own gates). Type CONNECT to confirm."),
            shiny::textInput(ns("connect_word"), NULL, value = "", placeholder = "CONNECT"),
            footer = shiny::tagList(shiny::modalButton("Cancel"),
                                    shiny::actionButton(ns("connect_ok"), "Connect", class = "btn-danger")),
            easyClose = TRUE))
          return()
        }
        if (identical(nm, "cran_snapshot") && !nzchar(.sr_set_str(value) %||% "")) {
          .sr_set_reset(nm)
          return(changed(nm, add = FALSE))
        }
        do_apply(nm, value)
      })
      shiny::observeEvent(input[[paste0("reset_", nm)]], {
        if (refuse()) return()
        .sr_set_reset(nm)
        if (identical(nm, "mode")) edited(setdiff(edited(), "jev_key"))
        changed(nm, add = FALSE)
        if (identical(nm, "jev_key")) shiny::updateTextInput(session, "val_jev_key", value = "")
      })
    })

    shiny::observeEvent(input$connect_ok, {
      if (refuse()) return()
      if (!identical(input$connect_word, "CONNECT")) {
        return(.sr_ui_notify("Settings: type CONNECT exactly (capital letters) to switch to connected mode",
                             "error"))
      }
      shiny::removeModal()
      do_apply("mode", "connected")
    })

    list(version = version)
  })
}
