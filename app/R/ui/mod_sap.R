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
  # a row editor is present when any of its inputs is live (an unchanged count is never re-sent)
  any_row <- function(prefix) any(startsWith(present, prefix))
  rd <- function(k) shiny::isolate(input[[k]])
  switch(e$kind,
    terms = {
      if (!any_row("f_terms_")) return(NULL)
      vals <- e$value %||% list()
      pick <- function(k, fb) if (has(k)) rd(k) %||% "" else fb
      n <- if (has("f_terms_n")) .sr_sap_n(rd("f_terms_n")) else min(length(vals), SR_FORM_MAX_ROWS)
      lapply(seq_len(n), function(i) {
        t <- if (i <= length(vals)) vals[[i]] else list()
        list(var = pick(paste0("f_terms_var_", i), .sr_ui_chr(t[["var"]])),
             type = pick(paste0("f_terms_type_", i), if (is.null(t[["type"]])) "linear" else t[["type"]]),
             knots = pick(paste0("f_terms_knots_", i), paste(as.character(t[["knots"]]), collapse = ",")))
      })
    },
    interactions = {
      if (!any_row("f_int_")) return(NULL)
      vals <- e$value %||% list()
      pick <- function(k, fb) if (has(k)) rd(k) %||% "" else fb
      n <- if (has("f_int_n")) .sr_sap_n(rd("f_int_n")) else min(length(vals), SR_FORM_MAX_ROWS)
      lapply(seq_len(n), function(i) {
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
    .sr_ui_card("Pre-specified analyses", shiny::uiOutput(ns("btns")), DT::DTOutput(ns("analyses"))),
    .sr_ui_card("Feasibility", shiny::uiOutput(ns("feas_btns")), DT::DTOutput(ns("feas"))),
    .sr_ui_card("Families and rules",
                shiny::uiOutput(ns("fam_form")),
                shiny::div(class = "text-danger small mt-2", shiny::textOutput(ns("fam_err"))),
                DT::DTOutput(ns("families")),
                shiny::hr(),
                shiny::uiOutput(ns("rule_form")),
                shiny::uiOutput(ns("rule_alt_ui")),
                shiny::div(class = "text-danger small mt-2", shiny::textOutput(ns("rule_err"))),
                DT::DTOutput(ns("rules"))),
    .sr_ui_card("Lock checklist", DT::DTOutput(ns("check"))),
    .sr_ui_card("Lock",
                shiny::uiOutput(ns("lock_summary")),
                shiny::uiOutput(ns("lock_btn")),
                shiny::uiOutput(ns("lock_retry")),
                shiny::div(class = "text-danger small mt-2", shiny::textOutput(ns("lock_err"))),
                shiny::div(class = "text-muted small mt-2", shiny::textOutput(ns("lock_state")))),
    .sr_ui_card("Documents", shiny::uiOutput(ns("docs"))))
}

# The lock confirmation dialog: the typed word, and the refusal in place (the same text as the card's lock_err).
.sr_sap_lock_modal <- function(ns) {
  word <- .sr_ui_on_control(shiny::textInput(ns("lock_word"), "Type LOCK to confirm", value = "", width = "100%"),
                            "input", maxlength = 20L)
  shiny::modalDialog(
    title = "Lock the SAP", easyClose = FALSE,
    shiny::p("Locking is final: the plan can no longer be edited, and the outcome is unblinded."),
    shiny::uiOutput(ns("lock_summary_modal")),
    word,
    shiny::div(class = "text-danger small mt-2", shiny::textOutput(ns("lock_err_modal"))),
    footer = shiny::tagList(shiny::actionButton(ns("lock_cancel"), "Cancel"),
                            shiny::actionButton(ns("lock_ok"), "Lock", class = "btn-danger")))
}

# ---- lock checklist and documents: pure helpers ------------------------------------------------------

# The lock checklist: sr_sap_check, or one block row when it cannot be computed; empty with no project.
.sr_sap_check_view <- function(p) {
  empty <- data.frame(item = character(), status = character(), detail = character(), stringsAsFactors = FALSE)
  if (is.null(p)) return(empty)
  tryCatch(sr_sap_check(p), error = function(e) {
    data.frame(item = "lock check", status = "block", detail = conditionMessage(e), stringsAsFactors = FALSE)
  })
}

# The written documents of the Documents card, per kind: paths relative to the project folder, oldest first.
.sr_sap_docs_view <- function(p) {
  rel <- function(kind) {
    if (is.null(p)) return(character())
    ex <- .sr_ui_exports(p, kind)
    substring(ex$path, nchar(p$paths$dir) + 2L)
  }
  list(sap = rel("sap"), feas = rel("feasibility"),
       locked = isTRUE(p$state$sap$locked), off = is.null(p) || isTRUE(p$readonly))
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

# ---- feasibility, families and rules: pure helpers ---------------------------------------------------

# The feasibility card rows: one row per pre-specified analysis (so `analysis` is a unique key): analysis,
# label, route, tier (the overall tier, "not assessed", or "report error: <message>" when the report cannot be
# built), stale, decision.
.sr_sap_feas_view <- function(p) {
  empty <- data.frame(analysis = character(), label = character(), route = character(), tier = character(),
                      stale = character(), decision = character(), stringsAsFactors = FALSE)
  if (is.null(p)) return(empty)
  an <- .sr_sap_analyses(p)
  if (!length(an)) return(empty)
  failed <- NULL
  rep_ <- tryCatch(sr_feasibility_report(p), error = function(e) {
    failed <<- conditionMessage(e)
    NULL
  })
  rows <- lapply(sort(as.character(names(an)), method = "radix"), function(id) {
    r <- if (is.null(rep_)) NULL else rep_[rep_$analysis == id, , drop = FALSE]
    s <- function(x) if (is.null(x) || !length(x) || is.na(x[1L])) "" else as.character(x[1L])
    if (is.null(r) || !nrow(r)) {
      tier <- if (is.null(failed)) "not assessed" else paste0("report error: ", failed)
      return(data.frame(analysis = id, label = .sr_ui_chr(an[[id]]$label), route = .sr_ui_chr(an[[id]]$route),
                        tier = tier, stale = "", decision = "", stringsAsFactors = FALSE))
    }
    # the report has one row per contrast and measure; the card has one row per analysis, with the
    # overall tier of the assessment (the per-contrast tiers are in the report and the feasibility document)
    overall <- p$state$feasibility[[id]]$assessment$tier
    data.frame(analysis = id, label = s(r$label), route = s(r$route), tier = s(overall),
               stale = if (is.na(r$stale[1L])) "" else paste0("yes (", r$stale[1L], ")"),
               decision = s(r$decision), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# The choices of the families and rules forms: the ids a family may take (primary / secondary), every id, and
# the sensitivity analyses by parent. A plain list so that it can be compared.
.sr_sap_fam_choices <- function(p) {
  an <- if (is.null(p)) list() else .sr_sap_analyses(p)
  ids <- sort(as.character(names(an)), method = "radix")
  lab <- vapply(an[ids], function(a) .sr_ui_chr(a$label), "")
  par <- vapply(an[ids], function(a) if (length(a$parent) != 1L || is.na(a$parent)) "" else as.character(a$parent), "")
  list(all = ids, members = ids[lab != "sensitivity"],
       sens = lapply(stats::setNames(nm = ids), function(id) ids[lab == "sensitivity" & par == id]))
}

.sr_sap_families_view <- function(p) {
  fam <- if (is.null(p)) list() else p$state$sap$families %||% list()
  data.frame(id = vapply(fam, function(f) as.character(f$id), ""),
             members = vapply(fam, function(f) paste(as.character(unlist(f$members)), collapse = ", "), ""),
             method = vapply(fam, function(f) as.character(f$method), ""), stringsAsFactors = FALSE,
             row.names = NULL)
}

.sr_sap_rules_view <- function(p) {
  r <- if (is.null(p)) list() else p$state$sap$rules %||% list()
  g <- function(k) vapply(r, function(x) as.character(x[[k]]), "")
  data.frame(id = g("id"), analysis = g("analysis"), check = g("check"), action = g("action"),
             alternative = g("alternative"), stringsAsFactors = FALSE, row.names = NULL)
}

# A numeric text field: NULL when empty, the number, or NA for text that is not one.
.sr_sap_num <- function(x) {
  x <- if (is.null(x) || !length(x)) "" else trimws(as.character(x[1L]))
  if (is.na(x) || !nzchar(x)) return(NULL)
  suppressWarnings(as.numeric(x))
}

# A reactive that only invalidates when f(project()) changes value (see .sr_ui_flag).
.sr_sap_flag <- function(project, f) .sr_ui_flag(project, f)

# The assessment dialog's fields: the delta, then the assumptions the route / spec call for.
.sr_sap_assess_fields <- function(ns, measures, show_r2, show_icc, disabled) {
  off <- function(x, sel) if (disabled) .sr_ui_on_control(x, sel, disabled = NA) else x
  txt <- function(id, label) {
    .sr_ui_on_control(off(shiny::textInput(ns(id), label, width = "100%"), "input"), "input", maxlength = 200L)
  }
  why <- function(id, label) {
    .sr_ui_on_control(off(shiny::textAreaInput(ns(id), label, rows = 2, width = "100%"), "textarea"),
                      "textarea", maxlength = SR_PICO_MAX_CHARS)
  }
  shiny::tagList(
    shiny::p(class = "text-muted small",
             "The smallest clinically important difference is optional; the assessment never looks at outcomes."),
    txt("fa_value", "Smallest important difference (number)"),
    off(shiny::selectInput(ns("fa_measure"), "Measure", choices = measures, selectize = FALSE, width = "100%"),
        "select"),
    txt("fa_source", "Source of the difference"),
    if (show_r2) shiny::tagList(txt("fa_r2", "R-squared of the covariates on the outcome (0 to 1)"),
                                why("fa_r2_why", "Justification for the R-squared")),
    if (show_icc) shiny::tagList(txt("fa_icc", "Intra-cluster correlation (required)"),
                                 why("fa_icc_why", "Justification for the ICC")))
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
    # `locked_ok` lets a step that must follow the lock (the SAP_v1.docx write) through a locked SAP.
    refusal <- function(locked_ok = FALSE) {
      p <- shiny::isolate(project())
      .sr_ui_refusal(p, function() busy()) %||%
        if (!locked_ok && !.sr_ui_editable(p)) "the SAP is locked; the plan can no longer be edited"
    }
    refuse <- function(label, ev = err, locked_ok = FALSE) {
      why <- refusal(locked_ok)
      if (is.null(why)) return(FALSE)
      ev(why)
      .sr_ui_notify(paste0(label, ": ", why), "error")
      TRUE
    }

    # Run a core step; a core error goes to an_err (verbatim). TRUE when it succeeded.
    run <- function(label, fn, ev = err, locked_ok = FALSE) {
      ev("")
      if (refuse(label, ev, locked_ok)) return(FALSE)
      ok <- FALSE
      act(label, function(p) {
        tryCatch({
          out <- fn(p)
          ok <<- TRUE
          out
        }, error = function(e) {
          ev(conditionMessage(e))
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

    # ---- feasibility card
    fsel <- shiny::reactiveVal(NULL)      # the selected analysis of the feasibility table, by id
    doc_on <- .sr_sap_flag(project, function(p) !is.null(p) && !isTRUE(p$readonly))
    fa_err <- shiny::reactiveVal("")
    fd_err <- shiny::reactiveVal("")
    fa_mode <- shiny::reactiveVal(NULL)   # list(id, measures, show_r2, show_icc) while the assess dialog is open
    fd_pending <- shiny::reactiveVal(NULL)

    output$feas_btns <- shiny::renderUI({
      off <- !editable()
      b <- function(i, l, cls, dis) shiny::actionButton(ns(i), l, class = cls, disabled = dis)
      shiny::div(class = "d-flex gap-2 mb-2", b("feas_assess", "Assess", "btn-primary", off),
                 b("feas_decide", "Decide", "btn-outline-primary", off),
                 b("feas_doc", "Feasibility report", "btn-outline-secondary", !doc_on()))
    })
    feas_df <- shiny::reactive(.sr_sap_feas_view(project()))
    output$feas <- DT::renderDT({
      df <- feas_df()
      k <- shiny::isolate(fsel())
      DT::datatable(df, rownames = FALSE, escape = TRUE,
                    selection = list(mode = "single", selected = if (!is.null(k) && k %in% df$analysis) match(k, df$analysis)),
                    options = list(dom = "tip", pageLength = 15L, scrollX = TRUE, ordering = FALSE),
                    class = "compact stripe")
    })
    shiny::observeEvent(input$feas_rows_selected, {
      df <- shiny::isolate(feas_df())
      i <- input$feas_rows_selected
      if (length(i) != 1L || is.na(i) || i < 1L || i > nrow(df)) return(invisible(NULL))
      fsel(df$analysis[i])
    }, ignoreNULL = FALSE, priority = 10)

    # The selected analysis, or NULL after saying why not (read-only first, then no row).
    feas_target <- function(label, ev) {
      if (refuse(label, ev)) return(NULL)
      k <- fsel()
      if (is.null(k) || !k %in% names(.sr_sap_analyses(shiny::isolate(project())))) {
        .sr_ui_notify(paste0(label, ": select an analysis first"), "error")
        return(NULL)
      }
      k
    }

    # -- assess
    shiny::observeEvent(input$feas_assess, {
      k <- feas_target("Assess feasibility", fa_err)
      if (is.null(k)) return(invisible(NULL))
      p <- shiny::isolate(project())
      a <- .sr_sap_analyses(p)[[k]]
      ms <- tryCatch(sr_feasibility_measures(p, k), error = function(e) {
        .sr_ui_notify(paste0("Assess feasibility: ", conditionMessage(e)), "error")
        NULL
      })
      if (is.null(ms)) return(invisible(NULL))
      fa_err("")
      fa_mode(list(id = k, measures = ms, show_r2 = a$route %in% c("lm", "lm_describe"),
                   show_icc = isTRUE(a$spec$layers$clustered)))
      shiny::showModal(shiny::modalDialog(
        title = paste0("Feasibility of ", k), size = "l", easyClose = FALSE,
        shiny::uiOutput(ns("fa_form")),
        shiny::div(class = "text-danger small mt-2", shiny::textOutput(ns("fa_err"))),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
                                shiny::actionButton(ns("fa_ok"), "Assess", class = "btn-primary",
                                                    disabled = !editable()))))
    })
    output$fa_form <- shiny::renderUI({
      .sr_sap_tick("fa_form")
      m <- fa_mode()
      ed <- editable()
      if (is.null(m)) return(NULL)
      .sr_sap_assess_fields(ns, m$measures, m$show_r2, m$show_icc, disabled = !ed)
    })
    output$fa_err <- shiny::renderText(fa_err())
    shiny::observeEvent(input$fa_ok, {
      m <- shiny::isolate(fa_mode())
      fa_err("")
      if (is.null(m)) return(invisible(NULL))
      if (refuse("Assess feasibility", fa_err)) return(invisible(NULL))
      num <- function(x, what) {
        v <- .sr_sap_num(x)
        if (!is.null(v) && is.na(v)) stop(what, " must be a number", call. = FALSE)
        v
      }
      args <- tryCatch({
        value <- num(input$fa_value, "the difference")
        measure <- .sr_ui_text(input$fa_measure)
        if (!measure %in% m$measures) measure <- m$measures[1L]
        delta <- if (is.null(value)) NULL else list(value = value, measure = measure, source = .sr_ui_text(input$fa_source))
        assm <- list()
        if (m$show_r2) {
          v <- num(input$fa_r2, "the R-squared")
          if (!is.null(v)) assm$r2_y <- list(value = v, justification = .sr_ui_text(input$fa_r2_why))
        }
        if (m$show_icc) {
          v <- num(input$fa_icc, "the ICC")
          if (!is.null(v)) assm$icc <- list(value = v, justification = .sr_ui_text(input$fa_icc_why))
        }
        list(delta = delta, assumptions = assm)
      }, error = function(e) {
        fa_err(conditionMessage(e))
        NULL
      })
      if (is.null(args)) return(invisible(NULL))
      ok <- run("Assess feasibility", function(p) sr_feasibility_assess(p, m$id, args$delta, args$assumptions),
                ev = fa_err)
      if (ok) {
        shiny::removeModal()
        fa_mode(NULL)
      }
    })

    # -- decide (a stop is confirmed a second time)
    decide_modal <- function(id, decision = SR_FEAS_DECISIONS[1L], reason = "") {
      dec <- shiny::selectInput(ns("fd_decision"), "Decision", choices = SR_FEAS_DECISIONS, selected = decision,
                                selectize = FALSE, width = "100%")
      rsn <- .sr_ui_on_control(shiny::textAreaInput(ns("fd_reason"), "Reason", value = reason, rows = 3, width = "100%"),
                               "textarea", maxlength = SR_PICO_MAX_CHARS)
      off <- !editable()
      if (off) {
        dec <- .sr_ui_on_control(dec, "select", disabled = NA)
        rsn <- .sr_ui_on_control(rsn, "textarea", disabled = NA)
      }
      shiny::showModal(shiny::modalDialog(
        title = paste0("Decision on ", id), size = "l", easyClose = FALSE, dec, rsn,
        shiny::div(class = "text-danger small mt-2", shiny::textOutput(ns("fd_err"))),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
                                shiny::actionButton(ns("fd_ok"), "Save decision", class = "btn-primary", disabled = off))))
    }
    fd_id <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$feas_decide, {
      k <- feas_target("Decide feasibility", fd_err)
      if (is.null(k)) return(invisible(NULL))
      fd_err("")
      fd_id(k)
      fd_pending(NULL)
      decide_modal(k)
    })
    output$fd_err <- shiny::renderText(fd_err())
    decide_run <- function(id, decision, reason) {
      run("Decide feasibility", function(p) sr_feasibility_decide(p, id, decision, reason), ev = fd_err)
    }
    shiny::observeEvent(input$fd_ok, {
      id <- shiny::isolate(fd_id())
      fd_err("")
      if (is.null(id)) return(invisible(NULL))
      if (refuse("Decide feasibility", fd_err)) return(invisible(NULL))
      dec <- .sr_ui_text(input$fd_decision)
      rsn <- .sr_ui_text(input$fd_reason)
      if (!dec %in% SR_FEAS_DECISIONS) {
        fd_err(paste0("decision must be one of: ", paste(SR_FEAS_DECISIONS, collapse = ", ")))
        return(invisible(NULL))
      }
      if (identical(dec, "stop")) {
        fd_pending(list(id = id, decision = dec, reason = rsn))
        shiny::showModal(shiny::modalDialog(
          title = paste0("Stop ", id, "?"), easyClose = FALSE,
          shiny::p("stop blocks every run of this analysis"),
          shiny::p(class = "text-muted small", "A stop can be reversed later by a new decision."),
          shiny::div(class = "text-danger small mt-2", shiny::textOutput(ns("fd_err"))),
          footer = shiny::tagList(shiny::modalButton("Cancel"),
                                  shiny::actionButton(ns("fd_stop_ok"), "Stop this analysis", class = "btn-danger"))))
        return(invisible(NULL))
      }
      if (decide_run(id, dec, rsn)) {
        shiny::removeModal()
        fd_id(NULL)
      }
    })
    shiny::observeEvent(input$fd_stop_ok, {
      pend <- shiny::isolate(fd_pending())
      fd_err("")
      if (is.null(pend)) return(invisible(NULL))
      fd_pending(NULL)
      if (decide_run(pend$id, pend$decision, pend$reason)) {
        shiny::removeModal()
        fd_id(NULL)
      } else {
        # a refusal: back to the decision dialog with what was typed and the core message
        decide_modal(pend$id, pend$decision, pend$reason)
      }
    })

    shiny::observeEvent(input$feas_doc, {
      res <- act("Feasibility report", function(p) sr_feasibility_docx(p))
      if (is.list(res) && !is.null(res$paths)) {
        ex <- .sr_ui_exports(res, "feasibility")
        if (nrow(ex)) .sr_ui_notify(paste0("Feasibility report written: ", ex$path[nrow(ex)]))
      }
    })

    # ---- families and rules
    fam_err <- shiny::reactiveVal("")
    rule_err <- shiny::reactiveVal("")
    fam_choices <- .sr_sap_flag(project, .sr_sap_fam_choices)
    off_on <- function(x, sel, ed) if (ed) x else .sr_ui_on_control(x, sel, disabled = NA)
    output$fam_form <- shiny::renderUI({
      .sr_sap_tick("fam_form")
      ch <- fam_choices()
      ed <- editable()
      keep <- shiny::isolate(input$fam_members)
      shiny::div(
        off_on(.sr_ui_on_control(shiny::textInput(ns("fam_id"), "Family id", value = shiny::isolate(input$fam_id) %||% "",
                                                  width = "100%"), "input", maxlength = 40L), "input", ed),
        off_on(shiny::selectInput(ns("fam_members"), "Members (primary or secondary analyses)", choices = ch$members,
                                  selected = intersect(keep, ch$members), multiple = TRUE, selectize = FALSE,
                                  width = "100%"), "select", ed),
        off_on(shiny::selectInput(ns("fam_method"), "Adjustment method",
                                  choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "none"),
                                  selected = shiny::isolate(input$fam_method) %||% "holm", selectize = FALSE,
                                  width = "100%"), "select", ed),
        shiny::actionButton(ns("fam_save"), "Save family", class = "btn-primary", disabled = !ed))
    })
    output$rule_form <- shiny::renderUI({
      ch <- fam_choices()
      ed <- editable()
      a0 <- shiny::isolate(input$rule_analysis)
      shiny::div(
        off_on(.sr_ui_on_control(shiny::textInput(ns("rule_id"), "Rule id", value = shiny::isolate(input$rule_id) %||% "",
                                                  width = "100%"), "input", maxlength = 40L), "input", ed),
        off_on(shiny::selectInput(ns("rule_analysis"), "Analysis", choices = ch$all,
                                  selected = if (!is.null(a0) && a0 %in% ch$all) a0, selectize = FALSE,
                                  width = "100%"), "select", ed),
        off_on(shiny::selectInput(ns("rule_check"), "When this diagnostic fails", choices = SR_DIAG_IDS,
                                  selected = shiny::isolate(input$rule_check), selectize = FALSE, width = "100%"),
               "select", ed),
        off_on(shiny::selectInput(ns("rule_action"), "Action", choices = c("add_sensitivity", "replace_primary"),
                                  selected = shiny::isolate(input$rule_action), selectize = FALSE, width = "100%"),
               "select", ed))
    })
    output$rule_alt_ui <- shiny::renderUI({
      ch <- fam_choices()
      ed <- editable()
      an <- input$rule_analysis
      if (is.null(an) || !an %in% ch$all) an <- if (length(ch$all)) ch$all[1L] else ""
      alts <- ch$sens[[an]] %||% character()
      prev <- shiny::isolate(input$rule_alt)
      shiny::div(
        off_on(shiny::selectInput(ns("rule_alt"), "Alternative (a sensitivity analysis of this analysis)",
                                  choices = alts, selected = if (!is.null(prev) && prev %in% alts) prev,
                                  selectize = FALSE, width = "100%"), "select", ed),
        if (!length(alts)) shiny::p(class = "text-muted small", "add a sensitivity analysis with this parent first"),
        shiny::actionButton(ns("rule_save"), "Save rule", class = "btn-primary", disabled = !ed))
    })
    output$fam_err <- shiny::renderText(fam_err())
    output$rule_err <- shiny::renderText(rule_err())
    output$families <- DT::renderDT(.sr_ui_dt(.sr_sap_families_view(project())))
    output$rules <- DT::renderDT(.sr_ui_dt(.sr_sap_rules_view(project())))

    shiny::observeEvent(input$fam_save, {
      run("Save family", function(p) {
        sr_sap_family(p, .sr_ui_text(input$fam_id), as.character(input$fam_members %||% character()),
                      .sr_ui_text(input$fam_method))
      }, ev = fam_err)
    })
    shiny::observeEvent(input$rule_save, {
      run("Save rule", function(p) {
        alt <- .sr_ui_text(input$rule_alt)
        sr_sap_rule(p, .sr_ui_text(input$rule_id), .sr_ui_text(input$rule_analysis),
                    .sr_ui_text(input$rule_check), .sr_ui_text(input$rule_action), if (nzchar(alt)) alt)
      }, ev = rule_err)
    })

    # ---- lock checklist, lock and documents
    # Lazy on purpose: sr_sap_check is the slow call, so it runs only when an output that is on screen reads
    # it (Shiny suspends the outputs of a hidden tab), not on every project change made in another tab.
    chk <- shiny::reactive(.sr_sap_check_view(project()))
    n_block <- shiny::reactive(sum(chk()$status == "block"))
    lock_err <- shiny::reactiveVal("")
    # SAP_v1.docx is owed when the SAP is locked and no SAP_v1 export is in the event log (derived from the
    # project, so a reopened project or a new session shows it too).
    v1_owed <- .sr_sap_flag(project, function(p) {
      !is.null(p) && isTRUE(p$state$sap$locked) && !"SAP_v1" %in% .sr_ui_exports(p, "sap")$id
    })
    word <- NULL                         # the word typed in the CURRENT lock dialog (see below)
    libsnap_now <- NULL

    output$check <- DT::renderDT({
      ch <- chk()
      esc <- function(x) htmltools::htmlEscape(as.character(x))
      .sr_ui_dt(data.frame(item = esc(ch$item), status = vapply(as.character(ch$status), .sr_ui_badge, ""),
                           detail = esc(ch$detail), stringsAsFactors = FALSE), escape = FALSE)
    }, server = FALSE)

    output$lock_btn <- shiny::renderUI({
      nb <- n_block()
      shiny::actionButton(ns("lock"), sprintf("Lock SAP (%d blocks)", nb), class = "btn-danger",
                          disabled = nb > 0L || !editable())
    })
    lock_facts <- .sr_sap_flag(project, function(p) {
      if (is.null(p)) return(NULL)
      list(ids = sort(as.character(names(.sr_sap_analyses(p))), method = "radix"),
           hash = as.character(p$state$sap$lock$libsnap_hash %||% ""))
    })
    summary_ui <- function() {
      f <- lock_facts()
      if (is.null(f)) return(NULL)
      ch <- chk()
      warns <- ch$detail[ch$status == "warn"]
      hash <- if (nzchar(f$hash)) {
        f$hash
      } else {
        if (is.null(libsnap_now)) libsnap_now <<- tryCatch(sr_libsnap_hash(), error = function(e) "")
        libsnap_now
      }
      shiny::tagList(
        shiny::p(shiny::strong("Analyses: "), if (length(f$ids)) paste(f$ids, collapse = ", ") else "none"),
        shiny::p(shiny::strong(sprintf("Open warnings (%d)", length(warns)))),
        if (length(warns)) shiny::tags$ul(lapply(warns, shiny::tags$li)),
        shiny::p(class = "small", shiny::strong("Library snapshot hash: "), shiny::code(hash)))
    }
    output$lock_summary <- shiny::renderUI(summary_ui())
    output$lock_summary_modal <- shiny::renderUI(summary_ui())
    output$lock_err <-shiny::renderText(lock_err())
    output$lock_err_modal <- shiny::renderText(lock_err())
    output$lock_state <- shiny::renderText({
      p <- project()
      if (!isTRUE(p$state$sap$locked)) return("")
      sprintf("Locked %s, SAP hash %s", p$state$sap$lock$locked_at %||% "",
              substr(p$state$sap$lock$sap_hash %||% "", 1L, 12L))
    })
    output$lock_retry <- shiny::renderUI({
      if (isTRUE(v1_owed())) {
        shiny::actionButton(ns("sap_v1_retry"), "Retry writing SAP_v1.docx", class = "btn-warning mt-2")
      }
    })

    # A project that is swapped for another one carries nothing over.
    pdir <- .sr_sap_flag(project, function(p) if (is.null(p)) NA_character_ else as.character(p$paths$dir))
    shiny::observeEvent(pdir(), {
      lock_err("")
      word <<- NULL
    }, ignoreInit = TRUE)

    # Shiny keeps lock_word after the dialog closes and resends only changed values, so the typed word only
    # counts once it has reported in the CURRENT dialog (the opener resets it first, the word handler runs after).
    shiny::observeEvent(input$lock, {
      lock_err("")
      word <<- NULL
      if (refuse("Lock SAP", lock_err)) return(invisible(NULL))
      shiny::showModal(.sr_sap_lock_modal(ns))
    }, priority = 10)
    shiny::observeEvent(input$lock_cancel, {
      shiny::removeModal()
      lock_err("")
      word <<- NULL
    })
    shiny::observeEvent(input$lock_word, {
      word <<- input$lock_word
    }, priority = 5, ignoreNULL = FALSE)

    # Write SAP_v1.docx after the lock. The lock stands whatever happens here: a failure is shown (card and
    # notification) and the retry button writes it again.
    write_v1 <- function() {
      ok <- run("Write SAP_v1.docx", function(p) sr_sap_docx(p), ev = lock_err, locked_ok = TRUE)
      if (!ok) {
        .sr_ui_notify(paste0("The SAP is locked, but SAP_v1.docx was not written: ", shiny::isolate(lock_err()),
                             " Use Retry in the Lock card."), "error")
      }
      ok
    }
    shiny::observeEvent(input$lock_ok, {
      lock_err("")
      if (refuse("Lock SAP", lock_err)) return(invisible(NULL))
      if (!identical(word, "LOCK")) {
        lock_err("type LOCK to confirm")
        return(invisible(NULL))
      }
      if (!run("Lock SAP", function(p) sr_sap_lock(p), ev = lock_err)) return(invisible(NULL))
      shiny::removeModal()
      word <<- NULL
      write_v1()
    })
    shiny::observeEvent(input$sap_v1_retry, {
      if (!isTRUE(shiny::isolate(v1_owed()))) return(invisible(NULL))
      write_v1()
    })

    # ---- documents
    docs <- .sr_sap_flag(project, .sr_sap_docs_view)
    output$docs <- shiny::renderUI({
      d <- docs()
      row <- function(paths, open, dl) {
        if (!length(paths)) return(NULL)
        n <- length(paths)
        shiny::tagList(
          lapply(paths[-n], function(x) shiny::p(class = "text-muted small mb-1", x)),
          shiny::div(class = "d-flex gap-2 align-items-center mb-2", shiny::code(paths[n]),
                     shiny::actionButton(ns(open), "Open folder", class = "btn-sm btn-outline-secondary"),
                     shiny::downloadButton(ns(dl), "Download", class = "btn-sm btn-outline-secondary")))
      }
      shiny::tagList(
        if (!d$locked) shiny::actionButton(ns("doc_draft"), "Write SAP draft", class = "btn-outline-primary mb-2",
                                           disabled = d$off),
        row(d$sap, "open_sap", "dl_sap"), row(d$feas, "open_feas", "dl_feas"),
        if (!length(d$sap) && !length(d$feas)) shiny::p(class = "text-muted small", "No document has been written yet."))
    })
    shiny::observeEvent(input$doc_draft, {
      if (refuse("SAP draft", function(x) NULL)) return(invisible(NULL))
      res <- act("SAP draft", function(p) sr_sap_docx(p))
      if (is.list(res) && !is.null(res$paths)) {
        ex <- .sr_ui_exports(res, "sap")
        if (nrow(ex)) .sr_ui_notify(paste0("SAP draft written: ", ex$path[nrow(ex)]))
      }
    })
    newest <- function(kind) {
      p <- shiny::isolate(project())
      ex <- if (is.null(p)) NULL else .sr_ui_exports(p, kind)
      if (is.null(ex) || !nrow(ex)) NA_character_ else ex$path[nrow(ex)]
    }
    open_kind <- function(kind, what) {
      f <- newest(kind)
      if (is.na(f)) return(.sr_ui_notify(paste0("Open folder: no ", what, " has been written yet"), "error"))
      .sr_ui_try("Open folder", function() .sr_ui_open_folder(dirname(f)))
      invisible(NULL)
    }
    shiny::observeEvent(input$open_sap, open_kind("sap", "SAP document"))
    shiny::observeEvent(input$open_feas, open_kind("feasibility", "feasibility report"))
    dl_kind <- function(kind, what) {
      shiny::downloadHandler(
        filename = function() {
          f <- newest(kind)
          if (is.na(f)) paste0(kind, ".docx") else basename(f)
        },
        content = function(file) {
          f <- newest(kind)
          if (is.na(f) || !file.exists(f)) stop("no ", what, " has been written yet", call. = FALSE)
          file.copy(f, file, overwrite = TRUE)
        })
    }
    output$dl_sap <- dl_kind("sap", "SAP document")
    output$dl_feas <- dl_kind("feasibility", "feasibility report")
    invisible(NULL)
  })
}
