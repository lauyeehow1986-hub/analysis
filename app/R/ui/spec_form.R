# ui/spec_form.R - the pure spec-form builder (no Shiny server code).
#
# A route's spec_schema is shallow (chr / num / lgl / list, optional values and default): it does not
# say that a string is a column or what shape `terms` has. SR_FORM_KINDS pairs every field of every
# registered P1 route with a form kind; SR_FORM_EXTRAS adds the form fields that live outside the
# schema (the exposure reference level, the MI and bootstrap settings, the MI x bootstrap choice).
#
#   sr_spec_form(route_id, table, project, layers)  the form description: one entry per field, in schema
#                                                   order, then the extras whose condition holds
#   sr_spec_collect(route_id, values)               form values (keyed by entry id) back to a spec list,
#                                                   without validation (sr_spec_validate / sr_sap_add do it)
#   sr_spec_form_route(project, table)              the decided primary_route of the table, or NA
#
# A schema field with no kind refuses the whole route: the form never builds a route without a field
# it does not know.

#' Field name -> form kind, for every schema field of the registered P1 routes.
SR_FORM_KINDS <- c(
  outcome = "column", exposure = "column", cluster = "column", time = "column", status = "column",
  strata = "column", by = "column",
  vars = "columns", nonnormal = "columns",
  terms = "terms",
  interactions = "interactions",
  layers = "layers",
  se = "enum", standardize_over = "enum", ties = "enum", penalty = "enum",
  marginal = "logical",
  seed = "number", tau = "number",
  event_value = "text",
  status_event = "event",
  exclude_rows = "hidden",
  engine = "engine",
  stream_chunk = "hidden"                      # set by the engine, never typed (sr_opt stream_chunk_rows)
)

# The engine choices of the form (label = value). Streaming is offered only to a route whose schema carries
# stream_chunk (the five routes of P1e.1); Cox and RMST run in memory until P1e.2.
.SR_FORM_ENGINES <- c("Auto" = "auto", "In memory" = "memory", "Streaming" = "stream")
.sr_form_engine_choices <- function(schema) {
  if ("stream_chunk" %in% names(schema)) .SR_FORM_ENGINES else .SR_FORM_ENGINES[c("Auto", "In memory")]
}

# Layer defaults of the form (docs/analysis.md section 2) and the values P1 cannot run.
.SR_FORM_LAYER_DEFAULTS <- list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                                framework = "frequentist", clustered = FALSE)
.SR_FORM_LAYER_ORDER <- c("missing", "weights", "matching", "resampling", "framework", "clustered")
.SR_FORM_NOT_P1 <- "not runnable in P1"
.SR_FORM_LAYER_DISABLED <- list(weights = c("ipw", "overlap", "survey"), matching = "TRUE",
                                framework = c("bayesian", "both"))

# Form defaults of fields whose schema gives none.
.SR_FORM_DEFAULTS <- list(seed = 42, engine = "auto")

.SR_FORM_LABELS <- c(
  outcome = "Outcome column", exposure = "Exposure column", cluster = "Cluster column",
  time = "Follow-up time column", status = "Event status column", strata = "Strata column",
  by = "Group by column", vars = "Variables", nonnormal = "Non-normal variables",
  terms = "Adjustment terms", interactions = "Interactions", se = "Standard errors",
  standardize_over = "Standardise over", ties = "Tie handling", penalty = "Penalty", engine = "Engine",
  marginal = "Report marginal effects", seed = "Random seed", tau = "Restriction time (tau)",
  event_value = "Outcome value counted as the event", status_event = "Status value counted as the event",
  layers_missing = "Missing data", layers_weights = "Weighting", layers_matching = "Matching",
  layers_resampling = "Resampling", layers_framework = "Framework", layers_clustered = "Clustered data"
)

# TRUE when a layer (current values, possibly NULL) is on.
.sr_form_layer_is <- function(layers, name, value) identical(as.character(layers[[name]]), value)

#' The form fields outside the schema. Each is list(id, kind, label, when, to, choices, default);
#' `when(route_id, layers)` says whether the field applies, `to` is its path in the spec.
SR_FORM_EXTRAS <- list(
  list(id = "exposure_ref", kind = "text", label = "Exposure reference level",
       when = function(route_id, layers) "exposure" %in% names(sr_route_get(route_id)$spec_schema),
       to = "exposure_ref", choices = NULL, default = NULL),
  list(id = "mi_m", kind = "text", label = "Number of imputations (a whole number, or auto)",
       when = function(route_id, layers) .sr_form_layer_is(layers, "missing", "mi"),
       to = c("mi", "m"), choices = NULL, default = NULL),
  list(id = "mi_maxit", kind = "number", label = "Imputation iterations",
       when = function(route_id, layers) .sr_form_layer_is(layers, "missing", "mi"),
       to = c("mi", "maxit"), choices = NULL, default = 10),
  list(id = "mi_aux", kind = "columns", label = "Auxiliary imputation columns",
       when = function(route_id, layers) .sr_form_layer_is(layers, "missing", "mi"),
       to = c("mi", "aux"), choices = NULL, default = NULL),
  list(id = "boot_B", kind = "number", label = "Bootstrap replicates",
       when = function(route_id, layers) .sr_form_layer_is(layers, "resampling", "bootstrap"),
       to = c("boot", "B"), choices = NULL, default = 2000),
  list(id = "boot_type", kind = "enum", label = "Bootstrap interval",
       when = function(route_id, layers) .sr_form_layer_is(layers, "resampling", "bootstrap"),
       to = c("boot", "type"), choices = c("percentile", "bca", "normal"), default = "percentile"),
  list(id = "boot_cluster", kind = "logical", label = "Resample whole clusters",
       when = function(route_id, layers) .sr_form_layer_is(layers, "resampling", "bootstrap"),
       to = "boot_type", choices = NULL, default = FALSE),
  list(id = "mi_x_boot", kind = "enum", label = "Multiple imputation with the bootstrap",
       when = function(route_id, layers) {
         .sr_form_layer_is(layers, "missing", "mi") && .sr_form_layer_is(layers, "resampling", "bootstrap")
       },
       to = c("choices", "mi_x_boot"), choices = c("boot_then_impute", "impute_then_boot"), default = NULL)
)

# The route and its schema; stops when a schema field has no form kind (purpose is the route's own).
.sr_form_route <- function(route_id) {
  r <- sr_route_get(route_id)
  for (f in names(r$spec_schema)) {
    if (!identical(f, "purpose") && !(f %in% names(SR_FORM_KINDS))) {
      stop("the form cannot build field '", f, "' of route '", route_id, "' yet", call. = FALSE)
    }
  }
  r
}

# The column names of `table` (empty when the table has no manifest).
.sr_form_columns <- function(project, table) {
  m <- tryCatch(sr_store_manifest(project, table), error = function(e) NULL)
  if (is.null(m)) return(character())
  vapply(m$schema, function(s) s$name, "")
}

# A decided, current router choice (not stale, not "none"), or NULL.
.sr_form_router <- function(project, node, table = NA) {
  ch <- tryCatch(sr_router_choice(project, node, table), error = function(e) NA_character_)
  if (length(ch) != 1L || is.na(ch) || identical(ch, "none")) NULL else ch
}

# The columns of `table` with role `role`, in the order given.
.sr_form_role_cols <- function(project, table, role) {
  r <- tryCatch(sr_roles(project, table), error = function(e) character())
  unname(names(r)[r == role])
}

# The prefill of a field: list(value, source), router first, then roles, then the default.
.sr_form_prefill <- function(f, route_id, project, table, default) {
  hit <- function(value, source) list(value = value, source = source)
  if (f %in% c("outcome", "exposure")) {
    ch <- .sr_form_router(project, paste0(f, "_col"), table)
    if (!is.null(ch)) return(hit(ch, "router"))
  }
  role <- switch(f, outcome = "outcome", exposure = "exposure", cluster = "cluster", time = "time",
                 status = "event", by = "exposure", NULL)
  if (!is.null(role)) {
    cols <- .sr_form_role_cols(project, table, role)
    if (length(cols)) return(hit(cols[1L], "roles"))
  }
  if (f %in% c("terms", "vars")) {
    cols <- .sr_form_role_cols(project, table, "covariate")
    if (length(cols)) {
      v <- if (identical(f, "terms")) lapply(cols, function(cl) list(var = cl, type = "linear", knots = NULL)) else cols
      return(hit(v, "roles"))
    }
  }
  if (identical(f, "layers_clustered") && length(.sr_form_role_cols(project, table, "cluster"))) {
    return(hit(TRUE, "roles"))
  }
  if (!is.null(default)) return(hit(default, "default"))
  hit(NULL, NA_character_)
}

.sr_form_entry <- function(id, kind, label, required, choices, disabled, default, pf) {
  list(id = id, kind = kind, label = label, required = required, choices = choices, disabled = disabled,
       default = default, value = pf$value, source = pf$source)
}

.sr_form_label <- function(id) if (id %in% names(.SR_FORM_LABELS)) .SR_FORM_LABELS[[id]] else id

.sr_form_no_disabled <- function() stats::setNames(character(), character())

# The six layer entries (ids layers_<name>).
.sr_form_layer_entries <- function(route_id, project, table) {
  lapply(.SR_FORM_LAYER_ORDER, function(nm) {
    id <- paste0("layers_", nm)
    choices <- if (nm %in% SR_LAYER_FLAGS) c("FALSE", "TRUE") else SR_LAYER_SETS[[nm]]
    off <- .SR_FORM_LAYER_DISABLED[[nm]]
    disabled <- if (is.null(off)) .sr_form_no_disabled() else stats::setNames(rep(.SR_FORM_NOT_P1, length(off)), off)
    default <- .SR_FORM_LAYER_DEFAULTS[[nm]]
    .sr_form_entry(id, "layers", .sr_form_label(id), TRUE, choices, disabled, default,
                   .sr_form_prefill(id, route_id, project, table, default))
  })
}

#' The form description of route `route_id` for `table` of `project`: a list (named by entry id) of entries
#' list(id, kind, label, required, choices, disabled, default, value, source), in schema order, then the
#' extras of SR_FORM_EXTRAS whose condition holds for `layers` (the current layer values, as the
#' layers_* selects give them; NULL uses the prefilled layers). Rebuild the form when a layer changes.
#' `layers` is one entry per layer (layers_<name>); `exclude_rows` (hidden) and `purpose` (the route's
#' own) are never entries. Every route gets the layer entries, also one whose schema does not name
#' `layers` (table1): sr_compose_check() needs spec$layers on every spec. Stops with "the form cannot build
#' field '<f>' of route '<r>' yet" when a schema field has no kind.
sr_spec_form <- function(route_id, table, project, layers = NULL) {
  r <- .sr_form_route(route_id)
  schema <- r$spec_schema
  cols <- .sr_form_columns(project, table)
  out <- list()
  add <- function(e) out[[length(out) + 1L]] <<- e
  has_layers <- FALSE
  for (f in names(schema)) {
    if (identical(f, "purpose")) next
    kind <- SR_FORM_KINDS[[f]]
    d <- schema[[f]]
    if (identical(kind, "hidden")) next
    if (identical(kind, "layers")) {
      for (e in .sr_form_layer_entries(route_id, project, table)) add(e)
      has_layers <- TRUE
      next
    }
    choices <- switch(kind, column = , columns = , terms = , interactions = cols, enum = d[["values"]],
                      engine = .sr_form_engine_choices(schema), NULL)
    default <- if (!is.null(d[["default"]])) d[["default"]] else .SR_FORM_DEFAULTS[[f]]
    add(.sr_form_entry(f, kind, .sr_form_label(f), isTRUE(d[["required"]]), choices, .sr_form_no_disabled(),
                       default, .sr_form_prefill(f, route_id, project, table, default)))
  }
  if (!has_layers) for (e in .sr_form_layer_entries(route_id, project, table)) add(e)
  if (is.null(layers)) {
    lay <- Filter(function(e) identical(e$kind, "layers"), out)
    layers <- stats::setNames(lapply(lay, function(e) e$value), sub("^layers_", "", vapply(lay, `[[`, "", "id")))
  }
  for (x in SR_FORM_EXTRAS) {
    if (!isTRUE(x$when(route_id, layers))) next
    choices <- if (identical(x$kind, "columns")) cols else x$choices
    pf <- if (is.null(x$default)) list(value = NULL, source = NA_character_) else list(value = x$default, source = "default")
    add(.sr_form_entry(x$id, x$kind, x$label, FALSE, choices, .sr_form_no_disabled(), x$default, pf))
  }
  names(out) <- vapply(out, `[[`, "", "id")
  out
}

# TRUE for a value the form treats as empty: NULL, zero length, all NA, or all blank strings.
.sr_form_empty <- function(x) {
  if (is.null(x) || length(x) == 0L) return(TRUE)
  if (is.list(x)) return(FALSE)
  if (all(is.na(x))) return(TRUE)
  is.character(x) && all(!nzchar(trimws(x)))
}

# RCS knots from the form: empty gives NULL, one number stays a number, a comma list gives locations.
.sr_form_knots <- function(k) {
  if (.sr_form_empty(k)) return(NULL)
  if (is.numeric(k)) return(k)
  parts <- trimws(unlist(strsplit(paste(as.character(k), collapse = ","), ",", fixed = TRUE)))
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(NULL)
  num <- suppressWarnings(as.numeric(parts))
  if (anyNA(num)) return(paste(parts, collapse = ","))  # left for the core to refuse, never dropped
  num
}

.sr_form_terms <- function(x) {
  out <- list()
  for (t in x) {
    v <- t[["var"]]
    if (.sr_form_empty(v)) next
    ty <- t[["type"]]
    term <- list(var = as.character(v), type = if (.sr_form_empty(ty)) "linear" else as.character(ty))
    kn <- .sr_form_knots(t[["knots"]])
    if (!is.null(kn)) term$knots <- kn
    out[[length(out) + 1L]] <- term
  }
  out
}

.sr_form_interactions <- function(x) {
  out <- list()
  for (ia in x) {
    ia <- as.character(unlist(ia))
    if (length(ia) == 2L && !anyNA(ia) && all(nzchar(trimws(ia)))) out[[length(out) + 1L]] <- ia
  }
  out
}

.sr_form_event <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  if (!is.na(n)) n else as.character(x)
}

# A form value cast by kind (NULL when it is empty after the cast).
.sr_form_cast <- function(kind, x) {
  v <- switch(kind,
    column = , text = , enum = , engine = as.character(x)[1L],
    columns = as.character(unlist(x)),
    terms = .sr_form_terms(x),
    interactions = .sr_form_interactions(x),
    logical = as.logical(x)[1L],
    number = as.numeric(x)[1L],
    event = .sr_form_event(as.character(x)[1L]),
    stop("the form has no cast for kind '", kind, "'", call. = FALSE))
  if (.sr_form_empty(v)) NULL else v
}

# The number of imputations: a whole number when numeric, else the text as given ("auto").
.sr_form_mi_m <- function(x) {
  x <- trimws(as.character(x)[1L])
  n <- suppressWarnings(as.numeric(x))
  if (!is.na(n)) n else x
}

.sr_form_set_path <- function(spec, path, value) {
  if (length(path) == 1L) {
    spec[[path]] <- value
  } else {
    sub <- spec[[path[1L]]]
    if (!is.list(sub)) sub <- list()
    sub[[path[2L]]] <- value
    spec[[path[1L]]] <- sub
  }
  spec
}

#' Form values (a named list keyed by entry id) back to a spec list for route `route_id`. Empty values
#' (NULL, "", zero length, NA) are dropped; `layers` is built from the layers_* ids (matching and
#' clustered as logical); the extras are mapped through their `to` path only when their condition holds;
#' `purpose` is the route's own. No validation: sr_spec_validate() and sr_sap_add() do that.
sr_spec_collect <- function(route_id, values) {
  r <- .sr_form_route(route_id)
  values <- as.list(values)
  spec <- list(purpose = r$purpose)
  for (f in names(r$spec_schema)) {
    if (identical(f, "purpose")) next
    kind <- SR_FORM_KINDS[[f]]
    if (kind %in% c("hidden", "layers")) next
    x <- values[[f]]
    if (.sr_form_empty(x)) next
    v <- .sr_form_cast(kind, x)
    if (identical(kind, "engine") && identical(v, "auto")) next   # the default: not stored (spec hash unchanged)
    if (!is.null(v)) spec[[f]] <- v
  }
  layers <- list()
  for (nm in .SR_FORM_LAYER_ORDER) {
    x <- values[[paste0("layers_", nm)]]
    if (.sr_form_empty(x)) next
    layers[[nm]] <- if (nm %in% SR_LAYER_FLAGS) as.logical(x)[1L] else as.character(x)[1L]
  }
  if (length(layers)) spec$layers <- layers
  for (x in SR_FORM_EXTRAS) {
    if (!isTRUE(x$when(route_id, layers))) next
    v <- values[[x$id]]
    if (.sr_form_empty(v)) next
    if (identical(x$id, "boot_cluster")) {
      if (isTRUE(as.logical(v)[1L])) spec$boot_type <- "cluster"
      next
    }
    v <- if (identical(x$id, "mi_m")) .sr_form_mi_m(v) else .sr_form_cast(x$kind, v)
    if (!is.null(v)) spec <- .sr_form_set_path(spec, x$to, v)
  }
  spec
}

#' The decided, current primary_route choice of `table`, or NA when it is undecided, stale or "none".
sr_spec_form_route <- function(project, table) {
  ch <- .sr_form_router(project, "primary_route", table)
  if (is.null(ch)) NA_character_ else ch
}

# ---- the renderer (Shiny input tags for a form description) -------------------------------------------

SR_FORM_MAX_ROWS <- 30L
.SR_FORM_TEXT_MAX <- 200L

.sr_ui_source_text <- function(source) {
  if (is.null(source) || length(source) != 1L || is.na(source)) return(NULL)
  switch(as.character(source), router = "from router", roles = "from roles", default = "default",
         saved = "saved", NULL)
}

# The label of a field: its text, a required mark, and the small source badge.
.sr_ui_field_label <- function(e) {
  src <- .sr_ui_source_text(e$source)
  shiny::tagList(e$label, if (isTRUE(e$required)) " *",
                 if (!is.null(src)) shiny::span(class = "badge text-bg-light border ms-1 fw-normal", src))
}

.sr_ui_off <- function(tag, selector, disabled) {
  if (isTRUE(disabled)) .sr_ui_on_control(tag, selector, disabled = NA) else tag
}

# A plain <select> whose options may be disabled (a disabled option carries its reason in its text).
.sr_ui_select <- function(id, label, choices, selected, disabled_choices = character(), disabled = FALSE,
                          texts = NULL) {
  texts <- texts %||% stats::setNames(choices, choices)
  opts <- lapply(choices, function(ch) {
    off <- ch %in% names(disabled_choices)
    txt <- if (nzchar(ch) && ch %in% names(texts)) texts[[ch]] else ch
    shiny::tags$option(value = ch, if (off) paste0(txt, " (", disabled_choices[[ch]], ")") else txt,
                       disabled = if (off) NA, selected = if (identical(ch, selected)) NA)
  })
  shiny::div(class = "form-group shiny-input-container mb-2", style = "width:100%;",
             shiny::tags$label(class = "control-label", `for` = id, label),
             shiny::tags$select(id = id, class = "form-select", disabled = if (isTRUE(disabled)) NA, opts))
}

.sr_ui_chr <- function(x) if (is.null(x) || !length(x) || all(is.na(x))) "" else as.character(x)[1L]

.sr_ui_terms_inputs <- function(e, ns, disabled) {
  vals <- e$value %||% list()
  n0 <- min(length(vals), SR_FORM_MAX_ROWS)
  cols <- c("", e$choices)
  rows <- lapply(seq_len(SR_FORM_MAX_ROWS), function(i) {
    t <- if (i <= length(vals)) vals[[i]] else list()
    kn <- t[["knots"]]
    shiny::conditionalPanel(
      condition = sprintf("input.f_terms_n >= %d", i), ns = ns,
      shiny::div(class = "d-flex gap-2 align-items-end",
        .sr_ui_select(ns(paste0("f_terms_var_", i)), paste("Term", i), cols, .sr_ui_chr(t[["var"]]),
                      disabled = disabled),
        .sr_ui_select(ns(paste0("f_terms_type_", i)), "Type", .sr_term_types,
                      if (is.null(t[["type"]])) "linear" else as.character(t[["type"]]), disabled = disabled),
        .sr_ui_off(shiny::textInput(ns(paste0("f_terms_knots_", i)), "Knots (rcs)",
                                    value = paste(as.character(kn), collapse = ","), width = "100%"),
                   "input", disabled)))
  })
  shiny::tagList(
    .sr_ui_off(shiny::numericInput(ns("f_terms_n"), .sr_ui_field_label(e), value = n0, min = 0L,
                                   max = SR_FORM_MAX_ROWS, step = 1L, width = "100%"), "input", disabled),
    rows)
}

# The variables an interaction may name: the exposure plus the linear and factor terms.
.sr_ui_int_choices <- function(exposure, terms) {
  tv <- vapply(terms, function(t) {
    ty <- t[["type"]]
    if (is.null(ty) || ty %in% c("linear", "factor")) .sr_ui_chr(t[["var"]]) else ""
  }, "")
  ch <- unique(c(.sr_ui_chr(exposure), tv))
  ch[nzchar(ch)]
}

.sr_ui_interaction_inputs <- function(e, ns, disabled, choices) {
  vals <- e$value %||% list()
  n0 <- min(length(vals), SR_FORM_MAX_ROWS)
  rows <- lapply(seq_len(SR_FORM_MAX_ROWS), function(i) {
    ia <- if (i <= length(vals)) as.character(unlist(vals[[i]])) else c("", "")
    shiny::conditionalPanel(
      condition = sprintf("input.f_int_n >= %d", i), ns = ns,
      shiny::div(class = "d-flex gap-2",
        .sr_ui_select(ns(paste0("f_int_a_", i)), paste("Interaction", i, "- first"), c("", choices),
                      .sr_ui_chr(ia[1L]), disabled = disabled),
        .sr_ui_select(ns(paste0("f_int_b_", i)), "second", c("", choices), .sr_ui_chr(ia[2L]),
                      disabled = disabled)))
  })
  shiny::tagList(
    .sr_ui_off(shiny::numericInput(ns("f_int_n"), .sr_ui_field_label(e), value = n0, min = 0L,
                                   max = SR_FORM_MAX_ROWS, step = 1L, width = "100%"), "input", disabled),
    rows)
}

#' The input tags of a form description (sr_spec_form): one control per entry, its id `ns("f_<id>")`, a
#' source badge next to the label, every control disabled when `disabled`. The terms editor and the
#' interactions editor use the fixed ids f_terms_n / f_terms_var_<i> / f_terms_type_<i> / f_terms_knots_<i>
#' and f_int_n / f_int_a_<i> / f_int_b_<i>. Layers are plain selects; a layer value that P1 cannot run is
#' a disabled option that says why. matching and clustered take "FALSE" / "TRUE" choices.
.sr_ui_spec_inputs <- function(form, ns, disabled = FALSE) {
  exposure <- form[["exposure"]]$value
  terms_val <- form[["terms"]]$value %||% list()
  one <- function(e) {
    id <- ns(paste0("f_", e$id))
    lab <- .sr_ui_field_label(e)
    v <- e$value
    switch(e$kind,
      column = .sr_ui_select(id, lab, c("", e$choices), .sr_ui_chr(v), disabled = disabled),
      columns = .sr_ui_off(shiny::selectInput(id, lab, choices = e$choices, selected = as.character(unlist(v)),
                                              multiple = TRUE, width = "100%"), "select", disabled),
      enum = .sr_ui_select(id, lab, e$choices, .sr_ui_chr(if (is.null(v)) e$default else v), disabled = disabled),
      engine = .sr_ui_select(id, lab, unname(e$choices), .sr_ui_chr(if (is.null(v)) e$default else v),
                             disabled = disabled,
                             texts = stats::setNames(names(e$choices), unname(e$choices))),
      logical = .sr_ui_off(shiny::checkboxInput(id, lab, value = isTRUE(as.logical(v)[1L])), "input", disabled),
      number = .sr_ui_off(shiny::numericInput(id, lab, value = if (is.null(v)) NA_real_ else as.numeric(v)[1L],
                                              width = "100%"), "input", disabled),
      text = , event = .sr_ui_off(
        .sr_ui_on_control(shiny::textInput(id, lab, value = .sr_ui_chr(v), width = "100%"), "input",
                          maxlength = .SR_FORM_TEXT_MAX), "input", disabled),
      layers = {
        flag <- e$id %in% paste0("layers_", SR_LAYER_FLAGS)
        sel <- if (is.null(v)) .sr_ui_chr(e$default) else as.character(v)[1L]
        texts <- if (flag) c("FALSE" = "no", "TRUE" = "yes") else stats::setNames(e$choices, e$choices)
        .sr_ui_select(id, lab, e$choices, sel, e$disabled, disabled, texts)
      },
      terms = .sr_ui_terms_inputs(e, ns, disabled),
      interactions = .sr_ui_interaction_inputs(e, ns, disabled, .sr_ui_int_choices(exposure, terms_val)),
      stop("the form cannot render kind '", e$kind, "' (field '", e$id, "')", call. = FALSE))
  }
  shiny::tagList(lapply(unname(form), one))
}
