# ui/mod_data.R - Shiny module: the Data tab. Import a file (shinyFiles pick, sniff preview with
# overrides, pre-flight, background import with progress and cancel), attach a dictionary, confirm
# variable roles, run the DQ checks, decide the suggested actions, edit the recipe, draw the flow
# diagram and show the readiness panel.
#
# mod_data_server(id, project) takes the reactiveVal that mod_project_server() returns and keeps no state
# of its own beyond the project: every action calls the core, which appends the event, and the new
# project is written back with project(new). The few other values (the running import's temp files) are
# transient. Outcome-blind: while sr_blind(project) the viewer drops the columns with the outcome role and
# the tables list hides the row count of a table whose flow has a hidden row; the core guards do the rest.
#
# Import runs in a mirai worker (shiny::ExtendedTask). The worker sources the app, opens the project
# itself, ingests and closes it again; for that the UI session releases its lock before the worker starts
# and reopens the project when the worker is done (a second process on this host would otherwise only get
# a read-only project). The app dir and the project folder reach the worker as arguments computed at
# run time. option sr.ingest_sync = TRUE runs the import inline instead (tests).

# ---- small pure helpers ------------------------------------------------------------------------------

# A single trimmed string from an input value, "" when it is not one.
.sr_data_text <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x)) trimws(x) else ""
}

# The path as typed or pasted: trimmed, one pair of surrounding double quotes removed (Windows "Copy as
# path" adds them), made absolute. "" when empty.
.sr_data_path <- function(x) {
  d <- .sr_data_text(x)
  if (nchar(d) >= 2L && startsWith(d, "\"") && endsWith(d, "\"")) d <- trimws(substr(d, 2L, nchar(d) - 1L))
  if (!nzchar(d)) return("")
  normalizePath(path.expand(d), winslash = "/", mustWork = FALSE)
}

#' Run fn() for the user: an error becomes an error notification and NULL is returned; a warning becomes a
#' warning notification and the result is kept. Never lets a condition escape into the session.
.sr_data_try <- function(label, fn) {
  say <- function(msg, type) {
    try(shiny::showNotification(paste0(label, ": ", msg), type = type, duration = 10), silent = TRUE)
  }
  tryCatch(
    withCallingHandlers(fn(), warning = function(w) {
      say(conditionMessage(w), "warning")
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      say(conditionMessage(e), "error")
      NULL
    })
}

# ---- sniff, preview, pre-flight --------------------------------------------------------------------

# The sniff with the user's overrides applied; `ov` is a named list (delim, encoding, decimal, header,
# date_order), each "auto"/NULL for no override.
.sr_data_apply_overrides <- function(sniff, ov) {
  pick <- function(k) {
    v <- ov[[k]]
    if (is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v) && !identical(v, "auto")) v else NULL
  }
  if (!is.null(v <- pick("delim"))) sniff$delim <- if (identical(v, "\\t")) "\t" else v
  if (!is.null(v <- pick("encoding"))) sniff$encoding <- v
  if (!is.null(v <- pick("decimal"))) sniff$decimal <- v
  if (!is.null(v <- pick("header"))) sniff$header <- identical(v, "TRUE")
  if (!is.null(v <- pick("date_order"))) {
    nm <- names(sniff$date_order)
    sniff$date_order <- if (length(nm)) stats::setNames(rep(v, length(nm)), nm) else sniff$date_order
  }
  sniff
}

# The sniff as "name: value" lines.
.sr_data_sniff_text <- function(sniff) {
  show <- c("format", "compression", "encoding", "bom", "delim", "quote", "decimal", "header",
            "quoted_newlines", "size_bytes")
  one <- function(k) {
    v <- sniff[[k]]
    if (is.null(v) || !length(v)) return(NULL)
    v <- if (identical(v, "\t")) "\\t" else paste(format(v), collapse = ", ")
    paste0(k, ": ", v)
  }
  lines <- unlist(lapply(show, one), use.names = FALSE)
  d <- sniff$date_order
  if (length(d)) {
    lines <- c(lines, paste0("date order: ", paste0(names(d), " = ", d, collapse = "; ")))
  }
  w <- sniff$warnings
  if (length(w)) lines <- c(lines, paste0("warning: ", w))
  paste(lines, collapse = "\n")
}

# The first n rows of a delimited text file as a character data frame, read the way the sniff says (so an
# override shows at once). A file this cannot show (not delimited text) gives a one-column note.
.sr_data_preview <- function(path, sniff, n = 20L) {
  note <- function(msg) data.frame(note = msg, stringsAsFactors = FALSE)
  if (!is.character(sniff$delim) || length(sniff$delim) != 1L || is.na(sniff$delim)) {
    return(note("no preview for this format"))
  }
  lines <- tryCatch({
    con <- .sr_open_text(path, sniff)
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    readLines(con, n = n + 1L, warn = FALSE)
  }, error = function(e) NULL)
  if (is.null(lines)) return(note("no preview for this format"))
  if (!length(lines)) return(note("the file is empty"))
  quote <- if (is.character(sniff$quote) && length(sniff$quote) == 1L && !is.na(sniff$quote)) sniff$quote else "\""
  dec <- if (is.character(sniff$decimal) && length(sniff$decimal) == 1L && !is.na(sniff$decimal)) sniff$decimal else "."
  df <- tryCatch(
    utils::read.table(text = lines, sep = sniff$delim, quote = quote, dec = dec,
                      header = !identical(sniff$header, FALSE), fill = TRUE, comment.char = "",
                      colClasses = "character", check.names = FALSE, stringsAsFactors = FALSE,
                      na.strings = character(), nrows = n),
    error = function(e) NULL)
  if (is.null(df)) data.frame(line = utils::head(lines, n), stringsAsFactors = FALSE) else df
}

# GB with one decimal.
.sr_data_gb <- function(b) sprintf("%.1f GB", as.numeric(b) / 1024^3)

# A duration in seconds as "3 s", "4 min" or "2.5 h".
.sr_data_dur <- function(s) {
  s <- as.numeric(s)
  if (!is.finite(s)) return("unknown")
  if (s < 90) sprintf("%.0f s", max(1, s)) else if (s < 5400) sprintf("%.0f min", s / 60) else sprintf("%.1f h", s / 3600)
}

#' The pre-flight card: numbers, warnings and, when the pre-flight refused, the reason.
.sr_data_preflight_ui <- function(pf) {
  if (!is.list(pf)) return(NULL)
  row <- function(k, v) shiny::tags$tr(shiny::tags$th(k), shiny::tags$td(v))
  num <- function(x) if (is.null(x) || !length(x) || !is.finite(x)) "unknown" else .sr_data_gb(x)
  warns <- as.character(pf$warnings %||% character())
  shiny::tagList(
    shiny::tags$table(
      class = "table table-sm w-auto mb-2",
      shiny::tags$tbody(
        row("Source size", num(pf$size_bytes)),
        row("Space needed", num(pf$need_bytes)),
        row("Free space", num(pf$free_bytes)),
        row("Estimated time", .sr_data_dur(pf$est_seconds %||% NA_real_)),
        row("Network share", if (isTRUE(pf$network_share)) "yes" else "no")
      )
    ),
    if (isTRUE(pf$external_suggested)) {
      shiny::div(class = "alert alert-info py-1 small",
                 "Large file: external mode (read in place, not copied into the project) is suggested.")
    },
    lapply(warns, function(w) shiny::div(class = "alert alert-warning py-1 small mb-1", w)),
    if (!isTRUE(pf$ok)) {
      shiny::div(class = "alert alert-danger py-1 small mb-0",
                 paste0("Import refused: ", pf$refuse_reason %||% "pre-flight failed"))
    } else {
      shiny::div(class = "text-success small", "Pre-flight passed.")
    }
  )
}

# TRUE when the pre-flight allows the import.
.sr_data_can_import <- function(pf) is.list(pf) && isTRUE(pf$ok)

# ---- the import job ----------------------------------------------------------------------------------

#' What the progress file holds, "<done> <total>" in bytes: list(done, total, fraction), or NULL when the
#' file is missing, empty or torn (the worker may be writing it).
.sr_data_progress_read <- function(file) {
  if (!is.character(file) || length(file) != 1L || !file.exists(file)) return(NULL)
  x <- tryCatch(suppressWarnings(readLines(file, n = 1L, warn = FALSE)), error = function(e) character())
  if (length(x) != 1L) return(NULL)
  v <- suppressWarnings(as.numeric(strsplit(trimws(x), "[[:space:]]+")[[1L]]))
  if (length(v) != 2L || anyNA(v) || v[2L] <= 0) return(NULL)
  list(done = v[1L], total = v[2L], fraction = min(1, v[1L] / v[2L]))
}

#' Ingest `path` into the open `project`, writing progress to `progress_file` and stopping when `cancel_file`
#' exists. Runs in the worker and, with sr.ingest_sync, in the session. Never raises: returns
#' list(ok, cancelled, error, project) where `project` is the updated project when ok, the one given
#' otherwise. A cancelled ingest keeps its checkpoint, so importing again resumes.
.sr_data_ingest_job <- function(project, path, table, sniff = NULL, external = NULL,
                                progress_file = NULL, cancel_file = NULL) {
  out <- list(ok = FALSE, cancelled = FALSE, error = NA_character_, project = project)
  res <- tryCatch({
    sniff <- sniff %||% sr_sniff(path)
    write_progress <- function(done, total) {
      if (is.character(progress_file)) {
        try(writeLines(sprintf("%.0f %.0f", done, max(total, 1)), progress_file), silent = TRUE)
      }
      invisible(NULL)
    }
    cancel <- function() is.character(cancel_file) && file.exists(cancel_file)
    p <- withCallingHandlers(
      sr_ingest(project, path, table = table, sniff = sniff, external = external,
                progress = write_progress, cancel = cancel),
      warning = function(w) {
        out$warnings <<- c(out$warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    write_progress(1, 1)
    p
  }, sr_ingest_cancelled = function(e) e, error = function(e) e)
  if (inherits(res, "sr_ingest_cancelled")) {
    out$cancelled <- TRUE
    out$error <- conditionMessage(res)
  } else if (inherits(res, "error")) {
    out$error <- conditionMessage(res)
  } else {
    out$ok <- TRUE
    out$project <- res
  }
  out
}

#' Start the import in a mirai worker and return the mirai. The worker sources the app at `app_dir`, opens
#' the project folder `project_dir` (the caller has released its lock), runs .sr_data_ingest_job() and closes
#' the project again. Nothing is read from the caller's session: every path is an argument.
.sr_data_spawn <- function(app_dir, project_dir, path, table, sniff = NULL, external = NULL,
                           progress_file = NULL, cancel_file = NULL) {
  mirai::mirai({
    source(file.path(app_dir, "global.R"), local = FALSE, encoding = "UTF-8")
    p <- sr_project_open(project_dir)
    out <- if (isTRUE(p$readonly)) {
      list(ok = FALSE, cancelled = FALSE, warnings = NULL,
           error = "the project is locked by another session")
    } else {
      res <- tryCatch(
        .sr_data_ingest_job(p, path, table, sniff, external, progress_file, cancel_file),
        error = function(e) list(ok = FALSE, cancelled = FALSE, warnings = NULL, error = conditionMessage(e)))
      try(sr_project_close(p), silent = TRUE)
      res$project <- NULL
      res
    }
    # the session may be gone (it cannot clean up after a worker still running): the worker removes the
    # cancel and progress files it was given
    try(unlink(c(progress_file, cancel_file)), silent = TRUE)
    out
  }, .args = list(app_dir = app_dir, project_dir = project_dir, path = path, table = table, sniff = sniff,
                  external = external, progress_file = progress_file, cancel_file = cancel_file))
}

# TRUE when two project folder paths are the same folder (case and slash style do not matter).
.sr_data_same_dir <- function(a, b) {
  ok <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!ok(a) || !ok(b)) return(FALSE)
  norm <- function(x) tolower(normalizePath(x, winslash = "/", mustWork = FALSE))
  identical(norm(a), norm(b))
}

# Suggestions most severe first (block, warn, info, anything else last), the finding order kept within a
# severity.
.sr_data_sort_suggestions <- function(s) {
  if (!is.data.frame(s) || !nrow(s) || is.null(s$severity)) return(s)
  rank <- match(as.character(s$severity), c("block", "warn", "info"), nomatch = 4L)
  s[order(rank, seq_len(nrow(s))), , drop = FALSE]
}

# How many suggestions one page of the Suggested actions card shows.
.SR_DATA_SUGGEST_PAGE <- 50L

# ---- outcome-blind helpers ----------------------------------------------------------------------------

# The marker .sr_data_hidden_cols() and .sr_data_outcome_names() return when they cannot tell which columns
# are outcome columns: every column is then hidden (fail closed).
.SR_DATA_ALL <- "<all columns>"

# TRUE when the row count of `table` must not be shown: its flow (or the flow of the table it was derived
# from) has a hidden count. Never errors, and fails closed: while blind, a table whose flow cannot be
# computed (or that is nested too deep to follow) has its count hidden.
.sr_data_rows_hidden <- function(project, table, depth = 0L) {
  if (!sr_blind(project)) return(FALSE)
  if (depth > 5L) return(TRUE)
  fl <- tryCatch(sr_flow(project, table), error = function(e) NULL)
  if (!is.data.frame(fl)) return(TRUE)
  if (nrow(fl) &&
      (anyNA(fl$n_before) || anyNA(fl$n_after) || any(endsWith(as.character(fl$label), SR_FLOW_HIDDEN)))) {
    return(TRUE)
  }
  src <- as.character(unlist(project$state$tables[[table]]$derived_from))
  any(vapply(src, function(s) !identical(s, table) && .sr_data_rows_hidden(project, s, depth + 1L), NA))
}

# The columns of `table` the viewer must not show: the outcome-role columns while blind; .SR_DATA_ALL when
# they cannot be determined.
.sr_data_hidden_cols <- function(project, table) {
  if (!sr_blind(project)) return(character())
  tryCatch(as.character(sr_role_cols(project, table, "outcome")), error = function(e) .SR_DATA_ALL)
}

# TRUE when `hidden` (from .sr_data_hidden_cols() or .sr_data_outcome_names()) hides anything.
.sr_data_any_hidden <- function(hidden) length(hidden) > 0L

# `cols` without the hidden ones; nothing at all when everything is hidden.
.sr_data_visible <- function(cols, hidden) {
  if (.SR_DATA_ALL %in% hidden) character() else setdiff(cols, hidden)
}

# The names of the outcome columns of every table of the project (a file about to be imported may be
# another file of any of them, or a re-import of one): while blind, nothing from the raw file with such a
# name may be shown. Character(0) when not blind or nothing is an outcome; .SR_DATA_ALL when it cannot be
# determined.
.sr_data_outcome_names <- function(project) {
  if (is.null(project) || !sr_blind(project)) return(character())
  tryCatch({
    tabs <- union(sr_store_tables(project), names(project$state$tables))
    unique(unlist(lapply(tabs, function(t) as.character(sr_role_cols(project, t, "outcome")))))
  }, error = function(e) .SR_DATA_ALL)
}

# A column name reduced for comparison: case, spaces and punctuation do not matter, so a raw header such as
# "Mortality 30d" is still taken for the outcome column mortality_30d.
.sr_data_norm_name <- function(x) gsub("[^a-z0-9]", "", tolower(as.character(x)))

# The preview data frame without the columns whose header is an outcome name. A file with no header row
# cannot be matched by name, and neither can raw lines the preview could not split, so those are not shown
# at all while outcome columns exist.
.sr_data_preview_hide <- function(df, hidden, header = TRUE) {
  note <- function() data.frame(note = "columns hidden until SAP lock", stringsAsFactors = FALSE)
  if (!length(hidden)) return(df)
  if (.SR_DATA_ALL %in% hidden || identical(header, FALSE) || identical(names(df), "line")) return(note())
  keep <- !(.sr_data_norm_name(names(df)) %in% .sr_data_norm_name(hidden))
  if (!any(keep)) return(note())
  df[, keep, drop = FALSE]
}

# ---- rendering helpers --------------------------------------------------------------------------------

# A severity (block, warn, info) or readiness status (block, warn, ok) as a badge. `data-status` carries
# the plain status for tests and scripts.
.sr_data_badge <- function(status) {
  cls <- switch(status, block = "bg-danger", warn = "bg-warning text-dark", ok = "bg-success", "bg-secondary")
  sprintf("<span class=\"badge %s\" data-status=\"%s\">%s</span>", cls, status, status)
}

# A data frame as a compact DT (server mode, no search box, no paging: the callers page themselves).
.sr_data_dt <- function(df, escape = TRUE, page_length = 15L) {
  DT::datatable(df, rownames = FALSE, selection = "none", escape = escape,
                options = list(dom = "tip", pageLength = page_length, scrollX = TRUE, ordering = FALSE),
                class = "compact stripe")
}

# The empty frame the findings table shows before a DQ run.
.sr_data_no_findings <- function() {
  data.frame(severity = character(), check = character(), domain = character(), column = character(),
             n_affected = numeric(), message = character(), stringsAsFactors = FALSE)
}

# ---- UI ----------------------------------------------------------------------------------------------

# One card of the Data tab. The tab is a normal scrolling page: a card is not a fill item (it would be
# squeezed into the viewport height) and its body is not a fill container, so it sizes to its content.
.sr_data_card <- function(header, ...) {
  bslib::card(fill = FALSE, bslib::card_header(header), bslib::card_body(fillable = FALSE, fill = FALSE, ...))
}

mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  ov_choices <- function(...) c(auto = "auto", ...)
  shiny::div(
    class = "sr-data-stack",
    .sr_data_card("1. Import a data file",
      shiny::div(
        class = "d-flex gap-2 align-items-end flex-wrap",
        shinyFiles::shinyFilesButton(ns("pick"), "Choose file", "Pick a data file", multiple = FALSE,
                                     class = "btn-outline-primary"),
        shiny::div(class = "flex-grow-1",
                   shiny::textInput(ns("path"), "File path (chosen above or pasted)", width = "100%"))
      ),
      bslib::layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2), fill = FALSE,
        shiny::selectInput(ns("ov_delim"), "Delimiter", ov_choices(comma = ",", semicolon = ";", tab = "\\t", pipe = "|")),
        shiny::selectInput(ns("ov_encoding"), "Encoding", ov_choices("UTF-8", "windows-1252", "ISO-8859-1", "UTF-16LE")),
        shiny::selectInput(ns("ov_decimal"), "Decimal mark", ov_choices(point = ".", comma = ",")),
        shiny::selectInput(ns("ov_header"), "Header row", ov_choices(yes = "TRUE", no = "FALSE")),
        shiny::selectInput(ns("ov_date_order"), "Date order", ov_choices("ymd", "dmy", "mdy")),
        shiny::textInput(ns("tname"), "Table name (optional)")
      ),
      bslib::layout_columns(
        col_widths = c(5, 7), fill = FALSE,
        shiny::div(shiny::h6("What the file looks like"), shiny::verbatimTextOutput(ns("sniff"), placeholder = TRUE)),
        shiny::div(shiny::h6("First 20 rows"), shiny::uiOutput(ns("preview_note")), DT::DTOutput(ns("preview"), fill = FALSE))
      ),
      shiny::h6("Pre-flight"),
      shiny::uiOutput(ns("preflight")),
      shiny::checkboxInput(ns("external"), "External mode: read the file in place, do not copy it into the project"),
      shiny::div(
        class = "d-flex gap-2 align-items-center",
        shiny::uiOutput(ns("import_ui")),
        shiny::actionButton(ns("cancel"), "Cancel import", class = "btn-outline-danger"),
        shiny::uiOutput(ns("progress"), class = "flex-grow-1")
      )
    ),
    .sr_data_card("2. Tables",
      shiny::uiOutput(ns("tables_list")),
      shiny::selectInput(ns("table"), "Working table", choices = character(0))
    ),
    .sr_data_card("3. View the data",
      shiny::div(
        class = "d-flex gap-2 align-items-end flex-wrap",
        shiny::uiOutput(ns("view_nav"), inline = TRUE),
        shiny::numericInput(ns("view_page"), "Page", value = 1, min = 1, step = 1, width = "90px"),
        shiny::selectInput(ns("view_size"), "Rows per page", c(25, 50, 100, 250), selected = 25, width = "130px"),
        shiny::uiOutput(ns("view_order_ui")),
        shiny::checkboxInput(ns("view_desc"), "Descending")
      ),
      shiny::uiOutput(ns("view_note")),
      DT::DTOutput(ns("viewer"), fill = FALSE)
    ),
    .sr_data_card("4. Data dictionary",
      shiny::div(
        class = "d-flex gap-2 align-items-end flex-wrap",
        shiny::radioButtons(ns("dict_kind"), "Format", c("CSV" = "csv", "REDCap data dictionary" = "redcap",
                                                         "ODM XML" = "odm"), inline = TRUE),
        shinyFiles::shinyFilesButton(ns("dict_pick"), "Choose file", "Pick a dictionary file", multiple = FALSE,
                                     class = "btn-outline-primary"),
        shiny::div(class = "flex-grow-1", shiny::textInput(ns("dict_path"), "Dictionary path", width = "100%")),
        shiny::actionButton(ns("dict_attach"), "Attach", class = "btn-primary")
      ),
      DT::DTOutput(ns("dict_table"), fill = FALSE)
    ),
    .sr_data_card("5. Variable roles",
      shiny::uiOutput(ns("roles_ui")),
      shiny::actionButton(ns("roles_confirm"), "Confirm roles", class = "btn-primary")
    ),
    .sr_data_card("6. Data-quality checks",
      shiny::actionButton(ns("dq_run"), "Run DQ checks", class = "btn-primary"),
      DT::DTOutput(ns("findings"), fill = FALSE)
    ),
    .sr_data_card("7. Suggested actions",
      shiny::numericInput(ns("sug_page"), "Findings page", value = 1, min = 1, step = 1, width = "120px"),
      shiny::uiOutput(ns("suggestions_ui")),
      shiny::actionButton(ns("export_query"), "Export query sheet", class = "btn-outline-primary")
    ),
    .sr_data_card("8. Recipe",
      DT::DTOutput(ns("recipe_steps"), fill = FALSE),
      shiny::div(
        class = "d-flex gap-2 align-items-end flex-wrap mt-2",
        shiny::actionButton(ns("recipe_undo"), "Undo last step"),
        shiny::textInput(ns("branch_name"), "New branch name"),
        shiny::actionButton(ns("recipe_branch"), "Branch"),
        shiny::selectInput(ns("branch_pick"), "Branch", choices = "main"),
        shiny::actionButton(ns("recipe_checkout"), "Checkout"),
        shiny::actionButton(ns("recipe_apply"), "Apply recipe", class = "btn-primary")
      )
    ),
    .sr_data_card("9. Flow of records",
      shiny::imageOutput(ns("flow_png"), height = "auto")
    ),
    .sr_data_card("10. Readiness",
      shiny::uiOutput(ns("readiness"))
    )
  )
}

# ---- server -------------------------------------------------------------------------------------------

mod_data_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    roots <- tryCatch(shinyFiles::getVolumes()(), error = function(e) c(Home = path.expand("~")))
    shinyFiles::shinyFileChoose(input, "pick", roots = roots, session = session)
    shinyFiles::shinyFileChoose(input, "dict_pick", roots = roots, session = session)

    # The import that is running, if any (transient: temp files and the folder to reopen).
    job <- shiny::reactiveValues(busy = FALSE, dir = NULL, name = NULL, table = NULL, progress = NULL,
                                 cancel = NULL)
    task <- shiny::ExtendedTask$new(function(...) .sr_data_spawn(...))

    notify <- function(msg, type = "message") {
      try(shiny::showNotification(msg, type = type, duration = 8), silent = TRUE)
    }

    tbl <- shiny::reactive({
      t <- .sr_data_text(input$table)
      p <- project()
      if (is.null(p) || !nzchar(t)) return(NULL)
      t
    })

    # Run a project-changing step: refuses while an import runs or the project is read-only, reports core
    # errors and warnings as notifications, and writes the new project back.
    act <- function(label, fn) {
      p <- shiny::isolate(project())
      if (is.null(p)) {
        notify(paste0(label, ": open a project first"), "error")
        return(invisible(NULL))
      }
      if (isTRUE(shiny::isolate(job$busy))) {
        notify(paste0(label, ": wait for the import to finish"), "error")
        return(invisible(NULL))
      }
      if (isTRUE(p$readonly)) {
        notify(paste0(label, ": the project is read-only (locked by another session)"), "error")
        return(invisible(NULL))
      }
      res <- .sr_data_try(label, function() fn(p))
      if (is.list(res) && !is.null(res$paths)) project(res)
      invisible(res)
    }

    need_table <- function(label) {
      t <- shiny::isolate(tbl())
      if (is.null(t)) notify(paste0(label, ": choose a table first"), "error")
      t
    }

    # ---- file pick -----------------------------------------------------------------------------------
    shiny::observeEvent(input$pick, {
      f <- tryCatch(shinyFiles::parseFilePaths(roots, input$pick), error = function(e) NULL)
      if (is.data.frame(f) && nrow(f)) shiny::updateTextInput(session, "path", value = as.character(f$datapath[1L]))
    })
    shiny::observeEvent(input$dict_pick, {
      f <- tryCatch(shinyFiles::parseFilePaths(roots, input$dict_pick), error = function(e) NULL)
      if (is.data.frame(f) && nrow(f)) shiny::updateTextInput(session, "dict_path", value = as.character(f$datapath[1L]))
    })

    # ---- sniff, preview, pre-flight --------------------------------------------------------------------
    path_in <- shiny::reactive(.sr_data_path(input$path))

    sniff_raw <- shiny::reactive({
      p <- path_in()
      if (!nzchar(p)) return(NULL)
      tryCatch(sr_sniff(p), error = function(e) structure(list(), error = conditionMessage(e)))
    })

    sniff_eff <- shiny::reactive({
      s <- sniff_raw()
      if (is.null(s) || !is.null(attr(s, "error"))) return(s)
      .sr_data_apply_overrides(s, list(delim = input$ov_delim, encoding = input$ov_encoding,
                                       decimal = input$ov_decimal, header = input$ov_header,
                                       date_order = input$ov_date_order))
    })

    external_in <- function() if (is.null(input$external)) NULL else isTRUE(input$external)

    pf <- shiny::reactive({
      s <- sniff_eff()
      p <- shiny::isolate(project())
      if (is.null(s) || !is.null(attr(s, "error")) || is.null(p)) return(NULL)
      tryCatch(sr_preflight(p, path_in(), s, external = isTRUE(input$external)),
               error = function(e) list(ok = FALSE, warnings = character(), refuse_reason = conditionMessage(e)))
    })

    output$sniff <- shiny::renderText({
      s <- sniff_eff()
      if (is.null(s)) return("Choose a file to see what it looks like.")
      if (!is.null(attr(s, "error"))) return(paste0("Cannot read this file: ", attr(s, "error")))
      .sr_data_sniff_text(s)
    })

    # While blind the raw file may be another copy of a table with outcome columns: nothing under an outcome
    # column name of any table is shown (the same rule as the viewer).
    preview_hidden <- shiny::reactive(.sr_data_outcome_names(project()))

    output$preview_note <- shiny::renderUI({
      if (.sr_data_any_hidden(preview_hidden())) {
        shiny::p(class = "text-muted small", "outcome columns hidden until SAP lock")
      }
    })

    output$preview <- DT::renderDT({
      s <- sniff_eff()
      shiny::req(s, is.null(attr(s, "error")))
      df <- .sr_data_preview_hide(.sr_data_preview(path_in(), s, 20L), preview_hidden(), s$header)
      .sr_data_dt(df, page_length = 20L)
    }, server = FALSE)

    output$preflight <- shiny::renderUI({
      x <- pf()
      if (is.null(x)) return(shiny::p(class = "text-muted small", "No file chosen yet."))
      .sr_data_preflight_ui(x)
    })

    shiny::observeEvent(pf(), {
      x <- pf()
      if (is.list(x) && isTRUE(x$external_suggested) && !isTRUE(input$external)) {
        shiny::updateCheckboxInput(session, "external", value = TRUE)
      }
    })

    output$import_ui <- shiny::renderUI({
      shiny::actionButton(ns("import"), "Import", class = "btn-primary",
                          disabled = !.sr_data_can_import(pf()) || isTRUE(job$busy))
    })

    # ---- import ----------------------------------------------------------------------------------------
    # `where` is "" when the import ran in the project that is open, else " in <project name>".
    finish_import <- function(res, dir, table, where = "") {
      if (is.list(res$warnings) || is.character(res$warnings)) {
        for (w in unlist(res$warnings)) notify(paste0("Import: ", w), "warning")
      }
      if (isTRUE(res$ok)) {
        notify(if (nzchar(where)) sprintf("Import finished%s: imported '%s'.", where, table)
               else sprintf("Imported '%s'.", table), "message")
      } else if (isTRUE(res$cancelled)) {
        notify(sprintf("Import cancelled%s; importing the same file again resumes it.", where), "warning")
      } else {
        notify(sprintf("Import failed%s: %s", where, res$error %||% "unknown error"), "error")
      }
    }

    shiny::observeEvent(input$import, {
      label <- "Import"
      p <- project()
      if (is.null(p)) return(notify("Import: open a project first", "error"))
      if (isTRUE(job$busy)) return(notify("Import: one import is already running", "error"))
      if (isTRUE(p$readonly)) return(notify("Import: the project is read-only (locked by another session)", "error"))
      path <- path_in()
      if (!nzchar(path)) return(notify("Import: choose a file first", "error"))
      s <- sniff_eff()
      if (!is.null(attr(s, "error"))) return(notify(paste0("Import: ", attr(s, "error")), "error"))
      x <- pf()
      if (!.sr_data_can_import(x)) {
        return(notify(paste0("Import refused: ", x$refuse_reason %||% "pre-flight failed"), "error"))
      }
      name <- .sr_data_text(input$tname)
      if (!nzchar(name)) name <- sr_table_name(path, sr_store_tables(p))
      prog <- tempfile("sr-progress-")
      canc <- tempfile("sr-cancel-")
      ext <- external_in()
      if (isTRUE(sr_opt("ingest_sync", FALSE))) {
        res <- .sr_data_try(label, function() .sr_data_ingest_job(p, path, name, s, ext, prog, canc))
        if (is.null(res)) return(invisible(NULL))
        if (isTRUE(res$ok)) project(res$project)
        return(finish_import(res, p$paths$dir, name))
      }
      # background: release our lock so the worker can take it, start the worker, reopen when it is done
      dir <- p$paths$dir
      try(sr_project_close(p), silent = TRUE)
      # the lock is released for the worker: until it is back the session holds a read-only copy, so no
      # write can go through a handle that no longer owns the project
      ro <- p
      ro$readonly <- TRUE
      project(ro)
      job$busy <- TRUE
      job$dir <- dir
      job$name <- as.character(p$meta$name %||% basename(dir))[1L]
      job$table <- name
      job$progress <- prog
      job$cancel <- canc
      started <- .sr_data_try(label, function() {
        task$invoke(app_dir = sr_root, project_dir = dir, path = path, table = name, sniff = s, external = ext,
                    progress_file = prog, cancel_file = canc)
        TRUE
      })
      if (!isTRUE(started)) reopen(dir)
    })

    # The session is gone while the worker still runs: ask it to stop (it keeps its checkpoint, removes the
    # cancel and progress files itself and releases the project lock). The progress file is not needed any more.
    session$onSessionEnded(function() {
      if (isTRUE(shiny::isolate(job$busy))) {
        try(file.create(shiny::isolate(job$cancel)), silent = TRUE)
        try(unlink(shiny::isolate(job$progress)), silent = TRUE)
      }
    })

    # Take the project folder back after a worker has used it (the worker released the lock). Only when the
    # open project is still the one the import started on, and still the read-only copy: another project
    # that was opened meanwhile is left alone (nothing it holds is released), and so is the same project
    # when it was opened afresh. Returns "" when the import's project is the open one, else " in <name>".
    reopen <- function(dir) {
      job$busy <- FALSE
      cur <- shiny::isolate(project())
      same <- !is.null(cur) && .sr_data_same_dir(cur$paths$dir, dir)
      if (same && isTRUE(cur$readonly)) {
        p <- .sr_data_try("Reopen project", function() sr_project_open(dir))
        if (!is.null(p)) project(p)
      }
      unlink(c(job$progress, job$cancel))
      if (same) "" else paste0(" in ", shiny::isolate(job$name) %||% basename(dir))
    }

    shiny::observeEvent(task$status(), {
      st <- task$status()
      if (!(st %in% c("success", "error"))) return()
      dir <- shiny::isolate(job$dir)
      table <- shiny::isolate(job$table)
      res <- tryCatch(task$result(), error = function(e) list(ok = FALSE, error = conditionMessage(e)))
      if (!is.list(res)) res <- list(ok = FALSE, error = paste(format(res), collapse = " "))
      where <- reopen(dir)
      finish_import(res, dir, table, where)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cancel, {
      if (!isTRUE(job$busy)) return(notify("No import is running.", "message"))
      try(file.create(job$cancel), silent = TRUE)
      notify("Cancelling after the current chunk...", "message")
    })

    output$progress <- shiny::renderUI({
      if (!isTRUE(job$busy)) return(NULL)
      shiny::invalidateLater(1000)
      pr <- .sr_data_progress_read(job$progress)
      pct <- if (is.null(pr)) 0 else round(100 * pr$fraction)
      shiny::div(
        class = "progress", style = "height: 1.4rem;",
        shiny::div(class = "progress-bar progress-bar-striped progress-bar-animated", role = "progressbar",
                   style = sprintf("width: %d%%;", pct), `aria-valuenow` = pct,
                   if (is.null(pr)) "starting..." else sprintf("%d%%", pct))
      )
    })

    # ---- tables ----------------------------------------------------------------------------------------
    store_tables <- shiny::reactive({
      p <- project()
      if (is.null(p)) character() else tryCatch(sr_store_tables(p), error = function(e) character())
    })

    shiny::observe({
      tabs <- store_tables()
      cur <- shiny::isolate(input$table)
      shiny::updateSelectInput(session, "table", choices = tabs,
                               selected = if (!is.null(cur) && cur %in% tabs) cur else utils::head(tabs, 1L))
    })

    output$tables_list <- shiny::renderUI({
      p <- project()
      shiny::req(p)
      tabs <- store_tables()
      if (!length(tabs)) return(shiny::p(class = "text-muted small", "No tables yet. Import a file above."))
      rows <- lapply(tabs, function(t) {
        m <- tryCatch(sr_store_manifest(p, t), error = function(e) NULL)
        hidden <- .sr_data_rows_hidden(p, t)
        n <- if (is.null(m)) "" else format(m$rows, big.mark = ",", scientific = FALSE, trim = TRUE)
        shiny::tags$tr(
          shiny::tags$td(t),
          shiny::tags$td(class = "text-end", if (hidden) shiny::span(class = "text-muted", "hidden until SAP lock") else n),
          shiny::tags$td(class = "text-end", if (is.null(m)) "" else length(m$schema)),
          shiny::tags$td(as.character(p$state$tables[[t]]$format %||% ""))
        )
      })
      shiny::tags$table(
        class = "table table-sm w-auto",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Table"), shiny::tags$th("Rows"), shiny::tags$th("Columns"),
                                         shiny::tags$th("Format"))),
        shiny::tags$tbody(rows)
      )
    })

    # ---- viewer ----------------------------------------------------------------------------------------
    view_cols <- shiny::reactive({
      p <- project()
      t <- tbl()
      shiny::req(p, t)
      m <- tryCatch(sr_store_manifest(p, t), error = function(e) NULL)
      if (is.null(m)) return(character())
      .sr_data_visible(vapply(m$schema, function(s) s$name, ""), .sr_data_hidden_cols(p, t))
    })

    output$view_order_ui <- shiny::renderUI({
      cols <- view_cols()
      shiny::selectInput(ns("view_order"), "Sort by", cols, selected = utils::head(cols, 1L), width = "200px")
    })

    # While the row count of the table is hidden the viewer is one page: any further page would show where
    # the rows end. Previous and Next are disabled and refuse.
    single_page <- shiny::reactive({
      p <- project()
      t <- tbl()
      !is.null(p) && !is.null(t) && .sr_data_rows_hidden(p, t)
    })

    output$view_nav <- shiny::renderUI({
      off <- isTRUE(single_page())
      shiny::tagList(
        shiny::actionButton(ns("view_prev"), "Previous", class = "btn-sm", disabled = off),
        shiny::actionButton(ns("view_next"), "Next", class = "btn-sm", disabled = off),
        if (off) shiny::span(class = "text-muted small", "one page only until SAP lock")
      )
    })

    refuse_paging <- function() {
      notify("View: paging is off until SAP lock (the row count is hidden)", "warning")
      shiny::updateNumericInput(session, "view_page", value = 1)
    }
    shiny::observeEvent(input$view_prev, {
      if (isTRUE(single_page())) return(refuse_paging())
      shiny::updateNumericInput(session, "view_page", value = max(1, (input$view_page %||% 1) - 1))
    })
    shiny::observeEvent(input$view_next, {
      if (isTRUE(single_page())) return(refuse_paging())
      shiny::updateNumericInput(session, "view_page", value = (input$view_page %||% 1) + 1)
    })

    output$view_note <- shiny::renderUI({
      p <- project()
      t <- tbl()
      shiny::req(p, t)
      if (.sr_data_any_hidden(.sr_data_hidden_cols(p, t))) {
        shiny::p(class = "text-muted small", "outcome columns hidden until SAP lock")
      }
    })

    output$viewer <- DT::renderDT({
      p <- project()
      t <- tbl()
      shiny::req(p, t)
      hidden <- .sr_data_hidden_cols(p, t)
      if (.SR_DATA_ALL %in% hidden) {      # cannot tell the outcome columns: show none of the data
        return(.sr_data_dt(data.frame(note = "columns hidden until SAP lock", stringsAsFactors = FALSE)))
      }
      cols <- view_cols()
      size <- suppressWarnings(as.integer(input$view_size %||% 25L))
      if (is.na(size) || size < 1L) size <- 25L
      page <- suppressWarnings(as.integer(input$view_page %||% 1L))
      if (is.na(page) || page < 1L || isTRUE(single_page())) page <- 1L
      ord <- .sr_data_text(input$view_order)
      if (!nzchar(ord) || !(ord %in% cols)) ord <- utils::head(cols, 1L)    # never an outcome column while blind
      df <- .sr_data_try("View", function() {
        sr_table_page(p, t, offset = (page - 1L) * size, limit = size, order_by = ord, desc = isTRUE(input$view_desc))
      })
      shiny::req(df)
      df <- df[, setdiff(names(df), hidden), drop = FALSE]
      DT::datatable(df, rownames = FALSE, selection = "none",
                    options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE),
                    class = "compact stripe")
    }, server = TRUE)

    # ---- dictionary ------------------------------------------------------------------------------------
    shiny::observeEvent(input$dict_attach, {
      t <- need_table("Dictionary")
      if (is.null(t)) return()
      kind <- .sr_data_text(input$dict_kind)
      path <- .sr_data_path(input$dict_path)
      if (!nzchar(path)) return(notify("Dictionary: choose a dictionary file first", "error"))
      act("Dictionary", function(p) {
        d <- switch(kind, csv = sr_dict_read_csv(path), redcap = sr_dict_read_redcap(path),
                    odm = sr_dict_read_odm(path), stop("choose a dictionary format", call. = FALSE))
        sr_dict_set(p, t, d, source = kind)
      })
    })

    output$dict_table <- DT::renderDT({
      p <- project()
      t <- tbl()
      shiny::req(p, t)
      d <- tryCatch(sr_dict_get(p, t), error = function(e) NULL)
      if (is.null(d)) d <- data.frame(note = "no dictionary attached", stringsAsFactors = FALSE)
      d[] <- lapply(d, function(x) if (is.list(x)) vapply(x, function(v) paste(format(v), collapse = "; "), "") else x)
      .sr_data_dt(d)
    }, server = FALSE)

    # ---- roles -----------------------------------------------------------------------------------------
    table_cols <- shiny::reactive({
      p <- project()
      t <- tbl()
      shiny::req(p, t)
      m <- tryCatch(sr_store_manifest(p, t), error = function(e) NULL)
      if (is.null(m)) character() else vapply(m$schema, function(s) s$name, "")
    })

    output$roles_ui <- shiny::renderUI({
      p <- project()
      t <- tbl()
      shiny::req(p, t)
      cols <- table_cols()
      sug <- tryCatch(sr_roles_suggest(p, t), error = function(e) NULL)
      conf <- tryCatch(sr_roles(p, t), error = function(e) character())
      cells <- lapply(seq_along(cols), function(i) {
        cur <- if (cols[i] %in% names(conf)) unname(conf[cols[i]]) else NULL
        s <- if (is.data.frame(sug)) sug[sug$column == cols[i], , drop = FALSE] else NULL
        sel <- cur %||% if (!is.null(s) && nrow(s)) s$role[1L] else "covariate"
        why <- if (!is.null(cur)) "confirmed" else if (!is.null(s) && nrow(s)) paste0("suggested: ", s$reason[1L]) else ""
        shiny::div(
          shiny::selectInput(ns(paste0("role_", i)), cols[i], choices = SR_ROLES, selected = sel, width = "100%"),
          shiny::div(class = "text-muted small mb-2", why)
        )
      })
      do.call(bslib::layout_columns, c(list(col_widths = c(4, 4, 4), fill = FALSE), cells))
    })

    shiny::observeEvent(input$roles_confirm, {
      t <- need_table("Roles")
      if (is.null(t)) return()
      cols <- shiny::isolate(table_cols())
      act("Roles", function(p) {
        sug <- sr_roles_suggest(p, t)
        r <- vapply(seq_along(cols), function(i) {
          v <- input[[paste0("role_", i)]]
          if (is.character(v) && length(v) == 1L && !is.na(v)) v else sug$role[match(cols[i], sug$column)]
        }, "")
        sr_roles_set(p, t, stats::setNames(r, cols))
      })
    })

    # ---- DQ and suggestions ----------------------------------------------------------------------------
    findings <- shiny::reactive({
      p <- project()
      t <- tbl()
      shiny::req(p, t)
      tryCatch(sr_dq_findings(p, t), error = function(e) NULL)
    })

    shiny::observeEvent(input$dq_run, {
      t <- need_table("DQ")
      if (is.null(t)) return()
      act("DQ", function(p) sr_dq_run(p, t))
    })

    output$findings <- DT::renderDT({
      f <- findings()
      if (is.null(f) || !nrow(f)) return(.sr_data_dt(.sr_data_no_findings()))
      esc <- function(x) htmltools::htmlEscape(as.character(x))
      d <- data.frame(severity = vapply(as.character(f$severity), .sr_data_badge, ""), check = esc(f$check),
                      domain = esc(f$domain), column = esc(f$column), n_affected = f$n_affected,
                      message = esc(f$message), stringsAsFactors = FALSE)
      .sr_data_dt(d, escape = FALSE)
    }, server = FALSE)

    suggestions <- shiny::reactive({
      p <- project()
      t <- tbl()
      shiny::req(p, t)
      s <- tryCatch(sr_suggest(p, t), error = function(e) NULL)
      f <- findings()
      if (is.data.frame(s) && is.data.frame(f)) s$severity <- f$severity[match(s$finding_id, f$id)]
      .sr_data_sort_suggestions(s)
    })

    shiny::observeEvent(tbl(), shiny::updateNumericInput(session, "sug_page", value = 1), ignoreInit = TRUE)

    output$suggestions_ui <- shiny::renderUI({
      p <- project()
      s <- suggestions()
      if (is.null(p) || is.null(s) || !nrow(s)) {
        return(shiny::p(class = "text-muted small", "No findings to decide yet. Run the DQ checks first."))
      }
      per <- .SR_DATA_SUGGEST_PAGE
      n <- nrow(s)
      npages <- max(1L, as.integer(ceiling(n / per)))
      pg <- suppressWarnings(as.integer(input$sug_page %||% 1L))
      if (is.na(pg) || pg < 1L) pg <- 1L
      pg <- min(pg, npages)
      from <- (pg - 1L) * per + 1L
      to <- min(n, pg * per)
      shown <- s[from:to, , drop = FALSE]
      rows <- lapply(seq_len(nrow(shown)), function(i) {
        id <- shown$finding_id[i]
        done <- p$state$actions[[id]]
        sev <- as.character(shown$severity[i] %||% "")
        click <- sprintf("Shiny.setInputValue(%s, {id: %s, nonce: Math.random()}, {priority: 'event'})",
                         jsonlite::toJSON(ns("decide"), auto_unbox = TRUE), jsonlite::toJSON(id, auto_unbox = TRUE))
        shiny::tags$tr(
          shiny::tags$td(shiny::HTML(.sr_data_badge(if (nzchar(sev) && !is.na(sev)) sev else "info"))),
          shiny::tags$td(shiny::tags$code(shown$check[i]), " ", shown$column[i]),
          shiny::tags$td(class = "small", paste0("suggested: ", shown$action[i], " - ", shown$reason[i]),
                         if (!is.null(done)) shiny::tags$div(class = "text-success", paste0("decided: ", done$action))),
          shiny::tags$td(shiny::selectInput(ns(paste0("act_", id)), NULL, choices = SR_ACTIONS,
                                            selected = done$action %||% shown$action[i], width = "150px")),
          shiny::tags$td(shiny::textInput(ns(paste0("why_", id)), NULL, value = done$reason %||% "",
                                          placeholder = "reason (needed to override)", width = "260px")),
          shiny::tags$td(shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-primary",
                                            onclick = click, "Decide"))
        )
      })
      shiny::tagList(
        shiny::p(class = "small text-muted",
                 sprintf("showing %d-%d of %d findings, most severe first (page %d of %d)", from, to, n, pg, npages)),
        shiny::tags$table(
          class = "table table-sm align-middle",
          shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Severity"), shiny::tags$th("Finding"),
                                           shiny::tags$th("Suggestion"), shiny::tags$th("Action"),
                                           shiny::tags$th("Reason"), shiny::tags$th())),
          shiny::tags$tbody(rows)
        )
      )
    })

    shiny::observeEvent(input$decide, {
      t <- need_table("Decide")
      if (is.null(t)) return()
      id <- .sr_data_text(input$decide$id)
      if (!nzchar(id)) return()
      action <- .sr_data_text(input[[paste0("act_", id)]])
      why <- .sr_data_text(input[[paste0("why_", id)]])
      act("Decide", function(p) sr_action_decide(p, id, action, reason = if (nzchar(why)) why else NULL, table = t))
    })

    shiny::observeEvent(input$export_query, {
      t <- need_table("Query sheet")
      if (is.null(t)) return()
      res <- act("Query sheet", function(p) sr_query_sheet(p, t))
      if (is.list(res) && !is.null(res$paths)) notify("Query sheet written under out/queries.", "message")
    })

    # ---- recipe ----------------------------------------------------------------------------------------
    output$recipe_steps <- DT::renderDT({
      p <- project()
      shiny::req(p)
      st <- tryCatch(sr_recipe_steps(p), error = function(e) NULL)
      d <- if (is.null(st) || !length(st)) {
        data.frame(note = "no steps on this branch", stringsAsFactors = FALSE)
      } else if (is.data.frame(st)) {
        st
      } else {
        data.frame(id = vapply(st, function(s) as.character(s$id), ""),
                   op = vapply(st, function(s) as.character(s$op), ""),
                   reason = vapply(st, function(s) paste(as.character(s$reason %||% ""), collapse = " "), ""),
                   stringsAsFactors = FALSE)
      }
      d[] <- lapply(d, function(x) if (is.list(x)) vapply(x, function(v) paste(format(v), collapse = " "), "") else x)
      .sr_data_dt(d)
    }, server = FALSE)

    shiny::observe({
      p <- project()
      shiny::req(p)
      br <- names(p$state$recipe$branches %||% list())
      if (!length(br)) br <- "main"
      shiny::updateSelectInput(session, "branch_pick", choices = br, selected = p$state$recipe$current %||% br[1L])
    })

    shiny::observeEvent(input$recipe_undo, act("Undo", function(p) sr_recipe_undo(p)))
    shiny::observeEvent(input$recipe_branch, {
      nm <- .sr_data_text(input$branch_name)
      act("Branch", function(p) sr_recipe_branch(p, nm))
    })
    shiny::observeEvent(input$recipe_checkout, {
      nm <- .sr_data_text(input$branch_pick)
      act("Checkout", function(p) sr_recipe_checkout(p, nm))
    })
    shiny::observeEvent(input$recipe_apply, {
      t <- need_table("Apply recipe")
      if (is.null(t)) return()
      act("Apply recipe", function(p) sr_recipe_apply(p, t))
    })

    # ---- flow ------------------------------------------------------------------------------------------
    output$flow_png <- shiny::renderImage({
      p <- project()
      t <- tbl()
      shiny::req(p, t)
      f <- tempfile(fileext = ".png")
      ok <- .sr_data_try("Flow", function() {
        sr_flow_plot(sr_flow(p, t), f)
        TRUE
      })
      shiny::req(isTRUE(ok))
      # the file is 300 dpi (2100 px wide): scale it down to the card, keep the aspect ratio
      list(src = f, contentType = "image/png", alt = paste0("Flow of records for ", t),
           style = "max-width:100%; height:auto;")
    }, deleteFile = TRUE)

    # ---- readiness -------------------------------------------------------------------------------------
    output$readiness <- shiny::renderUI({
      p <- project()
      t <- tbl()
      shiny::req(p, t)
      r <- tryCatch(sr_readiness(p, t), error = function(e) NULL)
      if (is.null(r)) return(shiny::p(class = "text-muted small", "Readiness is not available for this table."))
      rows <- lapply(seq_len(nrow(r)), function(i) {
        shiny::tags$tr(shiny::tags$td(shiny::HTML(.sr_data_badge(as.character(r$status[i])))),
                       shiny::tags$td(r$item[i]), shiny::tags$td(class = "small text-muted", r$detail[i]))
      })
      shiny::tags$table(class = "table table-sm", shiny::tags$tbody(rows))
    })

    invisible(NULL)
  })
}
