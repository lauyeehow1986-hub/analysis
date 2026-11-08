# ui/mod_sap.R - Shiny module: the SAP panel. Task 6: the analyses card and its schema-driven builder dialog.
#
# mod_sap_server(id, project, busy) takes the reactiveVal of mod_project_server(). Every change goes through
# the core (sr_sap_add / sr_sap_remove append the event); the dialog's fields come from sr_spec_form() and
# go back through sr_spec_collect(), and a core refusal is shown verbatim in an_err with the inputs kept.

# ---- pure helpers ------------------------------------------------------------------------------------

.sr_sap_id_msg <- function() paste0("id must match ", SR_ANALYSIS_ID_RE)

# The analyses card rows: id, label, route, table, purpose, feasibility (the report's decision, else its
# tier, else "not assessed").
.sr_sap_table_view <- function(p) {
  empty <- data.frame(id = character(), label = character(), route = character(), table = character(),
                      purpose = character(), feasibility = character(), stringsAsFactors = FALSE)
  if (is.null(p)) return(empty)
  an <- .sr_sap_analyses(p)
  if (!length(an)) return(empty)
  an <- an[order(names(an), method = "radix")]
  rep_ <- tryCatch(sr_feasibility_report(p), error = function(e) NULL)
  feas <- function(id) {
    r <- if (is.null(rep_)) NULL else rep_[rep_$analysis == id, , drop = FALSE]
    if (is.null(r) || !nrow(r)) return("not assessed")
    d <- r$decision[!is.na(r$decision)]
    if (length(d)) return(d[1L])
    paste(unique(r$tier[!is.na(r$tier)]), collapse = ", ")
  }
  s <- function(x) if (is.null(x) || !length(x) || is.na(x[1L])) "" else as.character(x[1L])
  data.frame(id = names(an), label = vapply(an, function(a) s(a$label), ""),
             route = vapply(an, function(a) s(a$route), ""), table = vapply(an, function(a) s(a$table), ""),
             purpose = vapply(an, function(a) s(a$spec$purpose), ""),
             feasibility = vapply(names(an), feas, ""), stringsAsFactors = FALSE, row.names = NULL)
}

# A stored single string, or NA when it is missing.
.sr_sap_na1 <- function(x) if (length(x) != 1L || is.na(x)) NA_character_ else as.character(x)

# A nested value of the spec at `path`, or NULL.
.sr_sap_spec_get <- function(spec, path) {
  x <- spec
  for (k in path) {
    if (!is.list(x)) return(NULL)
    x <- x[[k]]
  }
  x
}

# The stored value of the form entry `e` in a saved spec, or NULL.
.sr_sap_saved_value <- function(e, spec) {
  id <- e$id
  if (startsWith(id, "layers_")) return(spec$layers[[sub("^layers_", "", id)]])
  x <- Filter(function(x) identical(x$id, id), SR_FORM_EXTRAS)
  if (!length(x)) return(spec[[id]])
  x <- x[[1L]]
  if (identical(id, "boot_cluster")) return(if (identical(spec$boot_type, "cluster")) TRUE else NULL)
  .sr_sap_spec_get(spec, x$to)
}

# The form with each value taken from the saved spec (source "saved"); a field the spec does not set is empty.
.sr_sap_apply_saved <- function(form, spec) {
  for (nm in names(form)) {
    v <- .sr_sap_saved_value(form[[nm]], spec)
    form[[nm]]["value"] <- list(v)
    form[[nm]]$source <- if (is.null(v)) NA_character_ else "saved"
  }
  form
}

# The row count input, clipped to 0..SR_FORM_MAX_ROWS (NA and junk give 0).
.sr_sap_n <- function(x) {
  n <- suppressWarnings(as.integer(x)[1L])
  if (is.na(n)) 0L else min(max(n, 0L), SR_FORM_MAX_ROWS)
}

# The layer values of a form description, keyed by layer name.
.sr_sap_form_layers <- function(form) {
  lay <- Filter(function(e) identical(e$kind, "layers"), form)
  stats::setNames(lapply(lay, function(e) e$value), sub("^layers_", "", vapply(lay, `[[`, "", "id")))
}

# The input value of the form entry `e` as the form takes it, or NULL when its input is not there (absent
# from names(input)): a field the user never saw keeps its prefill.
.sr_sap_entry_value <- function(e, input, present) {
  has <- function(k) k %in% present
  rd <- function(k) shiny::isolate(input[[k]])
  switch(e$kind,
    terms = {
      if (!has("f_terms_n")) return(NULL)
      vals <- e$value %||% list()
      pick <- function(k, fb) if (has(k)) rd(k) %||% "" else fb
      lapply(seq_len(.sr_sap_n(rd("f_terms_n"))), function(i) {
        t <- if (i <= length(vals)) vals[[i]] else list()
        list(var = pick(paste0("f_terms_var_", i), .sr_ui_chr(t[["var"]])),
             type = pick(paste0("f_terms_type_", i), if (is.null(t[["type"]])) "linear" else t[["type"]]),
             knots = pick(paste0("f_terms_knots_", i), paste(as.character(t[["knots"]]), collapse = ",")))
      })
    },
    interactions = {
      if (!has("f_int_n")) return(NULL)
      vals <- e$value %||% list()
      pick <- function(k, fb) if (has(k)) rd(k) %||% "" else fb
      lapply(seq_len(.sr_sap_n(rd("f_int_n"))), function(i) {
        ia <- if (i <= length(vals)) as.character(unlist(vals[[i]])) else c("", "")
        c(pick(paste0("f_int_a_", i), .sr_ui_chr(ia[1L])), pick(paste0("f_int_b_", i), .sr_ui_chr(ia[2L])))
      })
    },
    {
      k <- paste0("f_", e$id)
      if (!has(k)) return(NULL)
      v <- rd(k)
      if (identical(e$kind, "layers") && e$id %in% paste0("layers_", SR_LAYER_FLAGS) && !is.null(v)) {
        v <- as.logical(v)
      }
      if (is.null(v)) character() else v
    })
}

#' The input values keyed by entry id, as sr_spec_collect takes them. A field whose input does not exist yet
#' keeps its form value; matching and clustered are turned from "FALSE" / "TRUE" into logicals. `live` is the
#' ids of the inputs that count (the ones that reported in the current dialog); NULL counts every input.
.sr_sap_form_values <- function(input, form, live = NULL) {
  present <- live %||% shiny::isolate(names(input))
  out <- list()
  for (e in form) {
    v <- .sr_sap_entry_value(e, input, present)
    if (is.null(v)) v <- e$value
    if (!is.null(v)) out[[e$id]] <- v
  }
  out
}

# The form with its values replaced by the current inputs (a field is marked unsourced when it changed).
.sr_sap_overlay <- function(form, input, live = NULL) {
  present <- live %||% shiny::isolate(names(input))
  for (nm in names(form)) {
    e <- form[[nm]]
    v <- .sr_sap_entry_value(e, input, present)
    if (is.null(v)) next
    same <- if (e$kind %in% c("terms", "interactions")) {
      TRUE
    } else {
      identical(as.character(unlist(v)), as.character(unlist(e$value)))
    }
    form[[nm]]["value"] <- list(v)
    if (!same) form[[nm]]$source <- NA_character_
  }
  form
}

# ---- UI ----------------------------------------------------------------------------------------------

mod_sap_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("banner")),
    .sr_ui_card("Pre-specified analyses", shiny::uiOutput(ns("btns")), DT::DTOutput(ns("analyses"))))
}

# The builder dialog: the fixed fields, then the schema-driven an_form.
.sr_sap_modal <- function(ns, kind, id, tables, routes, route, label, parent, others, table, disabled) {
  off <- function(x, sel) if (disabled) .sr_ui_on_control(x, sel, disabled = NA) else x
  id_in <- shiny::textInput(ns("an_id"), "Analysis id", value = id, width = "100%")
  id_in <- .sr_ui_on_control(id_in, "input", maxlength = 40L)
  if (identical(kind, "edit")) id_in <- .sr_ui_on_control(id_in, "input", readonly = NA)
  shiny::modalDialog(
    title = if (identical(kind, "edit")) paste0("Edit analysis ", id) else "Add an analysis",
    size = "l", easyClose = FALSE,
    off(id_in, "input"),
    off(shiny::selectInput(ns("an_label"), "Label", choices = c("primary", "secondary", "sensitivity"),
                           selected = label, selectize = FALSE, width = "100%"), "select"),
    off(shiny::selectInput(ns("an_table"), "Table", choices = tables, selected = table, selectize = FALSE,
                           width = "100%"), "select"),
    off(shiny::selectInput(ns("an_route"), "Route", choices = routes, selected = route, selectize = FALSE,
                           width = "100%"), "select"),
    shiny::conditionalPanel(
      condition = "input.an_label == 'sensitivity'", ns = ns,
      off(shiny::selectInput(ns("an_parent"), "Sensitivity analysis of", choices = c("(none)" = "", others),
                             selected = parent, selectize = FALSE, width = "100%"), "select")),
    shiny::hr(),
    shiny::uiOutput(ns("an_form")),
    shiny::div(class = "text-danger small mt-2", shiny::textOutput(ns("an_err"))),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("an_save"), "Save analysis", class = "btn-primary",
                          disabled = disabled)))
}

# ---- server ------------------------------------------------------------------------------------------

# Render counters (tests only).
.sr_sap_renders <- new.env(parent = emptyenv())
.sr_sap_tick <- function(what) {
  .sr_sap_renders[[what]] <- (.sr_sap_renders[[what]] %||% 0L) + 1L
  invisible(NULL)
}

mod_sap_server <- function(id, project, busy = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    act <- .sr_ui_actor(project, busy = function() busy())
    editable <- .sr_design_editable(project)
    err <- shiny::reactiveVal("")
    mode <- shiny::reactiveVal(NULL)      # NULL, or list(kind, id, rec, stamp) while the dialog is open
    stamp <- 0L
    last_key <- NULL
    to_remove <- shiny::reactiveVal(NULL)
    sel <- shiny::reactiveVal(NULL)       # the selected analysis, by id
    # Shiny keeps an input's value after its dialog closes, so an f_* input only counts once it has reported
    # in the CURRENT dialog: a value that differs from what the input held when the dialog opened (or an
    # input that did not exist then). A value still equal to the old one was never re-sent by the browser,
    # and the form's own value stands in for it.
    snap <- list()
    snap_names <- character()
    live <- new.env(parent = emptyenv())
    refresh_live <- function() {
      for (k in grep("^f_", shiny::isolate(names(input)), value = TRUE)) {
        if (exists(k, envir = live, inherits = FALSE)) next
        if (!(k %in% snap_names) || !identical(shiny::isolate(input[[k]]), snap[[k]])) assign(k, TRUE, envir = live)
      }
      invisible(NULL)
    }
    live_ids <- function() {
      refresh_live()
      ls(live, all.names = TRUE)
    }
    shiny::observe({
      for (k in grep("^f_", names(input), value = TRUE)) input[[k]]
      refresh_live()
    }, priority = 100)

    output$banner <- shiny::renderUI(.sr_ui_readonly_banner(project()))

    # Why an edit cannot go ahead (no project, busy, read-only, SAP locked), or NULL.
    refusal <- function() {
      p <- shiny::isolate(project())
      .sr_ui_refusal(p, function() busy()) %||%
        if (!.sr_ui_editable(p)) "the SAP is locked; the plan can no longer be edited"
    }
    refuse <- function(label) {
      why <- refusal()
      if (is.null(why)) return(FALSE)
      err(why)
      .sr_ui_notify(paste0(label, ": ", why), "error")
      TRUE
    }

    # Run a core step; a core error goes to an_err (verbatim). TRUE when it succeeded.
    run <- function(label, fn) {
      err("")
      if (refuse(label)) return(FALSE)
      ok <- FALSE
      act(label, function(p) {
        tryCatch({
          out <- fn(p)
          ok <<- TRUE
          out
        }, error = function(e) {
          err(conditionMessage(e))
          NULL
        })
      })
      ok
    }

    # ---- the card
    output$btns <- shiny::renderUI({
      off <- !editable()
      b <- function(i, l, cls) shiny::actionButton(ns(i), l, class = cls, disabled = off)
      shiny::div(class = "d-flex gap-2 mb-2", b("add", "Add analysis", "btn-primary"),
                 b("edit", "Edit", "btn-outline-primary"), b("remove", "Remove", "btn-outline-danger"))
    })
    table_df <- shiny::reactive(.sr_sap_table_view(project()))
    output$analyses <- DT::renderDT({
      df <- table_df()
      k <- shiny::isolate(sel())
      DT::datatable(df, rownames = FALSE, escape = TRUE,
                    selection = list(mode = "single", selected = if (!is.null(k) && k %in% df$id) match(k, df$id)),
                    options = list(dom = "tip", pageLength = 15L, scrollX = TRUE, ordering = FALSE),
                    class = "compact stripe")
    })
    shiny::observeEvent(input$analyses_rows_selected, {
      df <- shiny::isolate(table_df())
      i <- input$analyses_rows_selected
      if (length(i) != 1L || is.na(i) || i < 1L || i > nrow(df)) return(invisible(NULL))
      sel(df$id[i])
    }, ignoreNULL = FALSE, priority = 10)

    # ---- opening the dialog
    open_dialog <- function(kind, rec = NULL) {
      p <- shiny::isolate(project())
      tables <- sr_store_tables(p)
      routes <- unique(sr_routes()$id)
      an <- names(.sr_sap_analyses(p))
      tb <- if (!is.null(rec)) rec$table else if (length(tables)) tables[1L] else ""
      rt <- if (!is.null(rec)) {
        rec$route
      } else {
        r0 <- if (nzchar(tb)) sr_spec_form_route(p, tb) else NA_character_
        if (!is.na(r0) && r0 %in% routes) r0 else routes[1L]
      }
      stamp <<- stamp + 1L
      last_key <<- NULL
      rm(list = ls(live, all.names = TRUE), envir = live)
      snap_names <<- grep("^f_", shiny::isolate(names(input)), value = TRUE)
      snap <<- stats::setNames(lapply(snap_names, function(k) shiny::isolate(input[[k]])), snap_names)
      err("")
      mode(list(kind = kind, id = if (is.null(rec)) "" else rec$id, rec = rec, stamp = stamp))
      shiny::showModal(.sr_sap_modal(
        ns, kind, if (is.null(rec)) "" else rec$id, tables, routes, rt,
        if (is.null(rec)) "primary" else rec$label, if (is.null(rec) || is.na(.sr_sap_na1(rec$parent))) "" else rec$parent,
        setdiff(an, if (is.null(rec)) character() else rec$id), tb, disabled = FALSE))
    }
    shiny::observeEvent(input$add, {
      if (refuse("Add analysis")) return(invisible(NULL))
      open_dialog("add")
    })
    shiny::observeEvent(input$edit, {
      if (refuse("Edit analysis")) return(invisible(NULL))
      k <- sel()
      an <- .sr_sap_analyses(shiny::isolate(project()))
      if (is.null(k) || !k %in% names(an)) {
        .sr_ui_notify("Edit analysis: select an analysis first", "error")
        return(invisible(NULL))
      }
      rec <- an[[k]]
      rec$id <- k
      open_dialog("edit", rec)
    })

    # In add mode the route follows the router's decision for the chosen table.
    shiny::observeEvent(input$an_table, {
      m <- shiny::isolate(mode())
      if (is.null(m) || !identical(m$kind, "add")) return(invisible(NULL))
      r0 <- sr_spec_form_route(shiny::isolate(project()), input$an_table)
      if (!is.na(r0) && r0 %in% unique(sr_routes()$id)) shiny::updateSelectInput(session, "an_route", selected = r0)
    }, ignoreInit = TRUE)

    # ---- the schema-driven form. It depends on the table, the route, the layer selects, the mode and
    # editability only (never on project()), so typed values survive every other project change; on a
    # rebuild within the same dialog / table / route the current inputs are laid over the new form.
    layers_now <- shiny::reactive({
      mode()   # a new dialog starts again from its own layers
      lay <- lapply(stats::setNames(nm = .SR_FORM_LAYER_ORDER), function(nm) input[[paste0("f_layers_", nm)]])
      lay <- Filter(Negate(is.null), lay)
      ids <- live_ids()
      lay <- lay[paste0("f_layers_", names(lay)) %in% ids]
      if (length(lay)) lay else NULL
    })
    form_r <- shiny::reactive({
      m <- mode()
      if (is.null(m)) return(NULL)
      table <- .sr_ui_text(input$an_table)
      route <- .sr_ui_text(input$an_route)
      lay <- layers_now()
      if (!nzchar(table) || !nzchar(route)) return(list(error = "choose a table and a route"))
      p <- shiny::isolate(project())
      same_saved <- !is.null(m$rec) && identical(m$rec$table, table) && identical(m$rec$route, route)
      key <- paste(m$stamp, table, route, sep = "\r")
      fresh <- !identical(last_key, key)
      last_key <<- key
      ids <- live_ids()
      tryCatch({
        # the layers: the saved (Edit) or prefilled ones, with the ones the user has chosen in this dialog on top
        form <- sr_spec_form(route, table, p)
        if (same_saved) form <- .sr_sap_apply_saved(form, m$rec$spec)
        base <- .sr_sap_form_layers(form)
        if (same_saved) base <- utils::modifyList(base, as.list(m$rec$spec$layers))
        merged <- utils::modifyList(base, as.list(lay))
        form <- sr_spec_form(route, table, p, layers = merged)
        if (same_saved) form <- .sr_sap_apply_saved(form, m$rec$spec)
        if (!fresh) form <- .sr_sap_overlay(form, input, ids)
        list(form = form, route = route, table = table)
      }, error = function(e) list(error = conditionMessage(e)))
    })
    output$an_form <- shiny::renderUI({
      .sr_sap_tick("an_form")
      f <- form_r()
      ed <- editable()
      if (is.null(f)) return(NULL)
      if (!is.null(f$error)) return(shiny::p(class = "text-danger", f$error))
      .sr_ui_spec_inputs(f$form, ns, disabled = !ed)
    })

    # The interaction selects offer the exposure and the linear / factor terms as they are now.
    shiny::observe({
      f <- shiny::isolate(form_r())
      if (is.null(f) || !is.null(f$error) || !"interactions" %in% names(f$form)) return(invisible(NULL))
      n <- .sr_sap_n(input$f_int_n)
      nt <- .sr_sap_n(input$f_terms_n)
      terms <- lapply(seq_len(nt), function(i) {
        list(var = input[[paste0("f_terms_var_", i)]] %||% "",
             type = input[[paste0("f_terms_type_", i)]] %||% "linear")
      })
      ch <- .sr_ui_int_choices(input$f_exposure, terms)
      for (i in seq_len(n)) for (k in c("a", "b")) {
        id <- paste0("f_int_", k, "_", i)
        shiny::updateSelectInput(session, id, choices = c("", ch), selected = shiny::isolate(input[[id]]) %||% "")
      }
    })

    output$an_err <- shiny::renderText(err())

    # ---- save (add, or replace by id)
    shiny::observeEvent(input$an_save, {
      m <- shiny::isolate(mode())
      err("")
      if (is.null(m)) return(invisible(NULL))
      if (refuse("Save analysis")) return(invisible(NULL))
      id <- if (identical(m$kind, "edit")) m$id else .sr_ui_text(input$an_id)
      if (!grepl(SR_ANALYSIS_ID_RE, id)) {
        err(.sr_sap_id_msg())
        return(invisible(NULL))
      }
      f <- shiny::isolate(form_r())
      if (is.null(f) || !is.null(f$error)) {
        err(if (is.null(f)) "no analysis is being edited" else f$error)
        return(invisible(NULL))
      }
      label <- .sr_ui_text(input$an_label)
      parent <- .sr_ui_text(input$an_parent)
      parent <- if (identical(label, "sensitivity") && nzchar(parent)) parent else NA_character_
      rec <- m$rec
      ok <- run("Save analysis", function(p) {
        spec <- sr_spec_collect(f$route, .sr_sap_form_values(input, f$form, live_ids()))
        extra <- if (!is.null(rec)) {
          list(family = .sr_sap_na1(rec$family), addresses = rec$addresses %||% character(),
               version = if (identical(rec$route, f$route)) rec$route_version)
        } else {
          list()
        }
        do.call(sr_sap_add, c(list(p, id, f$route, spec, f$table, label, parent = parent), extra))
      })
      if (ok) {
        shiny::removeModal()
        mode(NULL)
        sel(id)
      }
    })

    # ---- remove (after a confirmation)
    shiny::observeEvent(input$remove, {
      if (refuse("Remove analysis")) return(invisible(NULL))
      k <- sel()
      if (is.null(k) || !k %in% names(.sr_sap_analyses(shiny::isolate(project())))) {
        .sr_ui_notify("Remove analysis: select an analysis first", "error")
        return(invisible(NULL))
      }
      err("")
      to_remove(k)
      shiny::showModal(shiny::modalDialog(
        title = "Remove analysis",
        shiny::p(sprintf("Remove the pre-specified analysis '%s'? This is recorded in the event log.", k)),
        shiny::div(class = "text-danger small", shiny::textOutput(ns("an_err"))),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
                                shiny::actionButton(ns("remove_ok"), "Remove", class = "btn-danger")),
        easyClose = TRUE))
    })
    shiny::observeEvent(input$remove_ok, {
      k <- shiny::isolate(to_remove()) %||% shiny::isolate(sel())
      err("")
      if (refuse("Remove analysis")) return(invisible(NULL))
      if (is.null(k)) {
        err("select an analysis first")
        return(invisible(NULL))
      }
      ok <- run("Remove analysis", function(p) sr_sap_remove(p, k))
      if (ok) {
        shiny::removeModal()
        to_remove(NULL)
        sel(NULL)
      }
    })
    invisible(NULL)
  })
}
