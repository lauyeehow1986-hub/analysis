# ui/mod_project.R - Shiny module: create or open a portable project folder and list its events.
#
# mod_project_server() returns a reactive holding the open project (list(paths, meta, state, readonly))
# or NULL. The module owns the project it opened: it closes the previous one when another is created or
# opened, and releases the lock when the session ends. A project opened read-only holds no lock of ours,
# and sr_project_close() only removes a lock that belongs to this process, so closing is always safe.

#' The host named in a project's lock, or "unknown host" when there is no readable lock.
.sr_lock_host_label <- function(paths) {
  l <- .sr_lock_read(paths$lock)
  if (is.list(l) && is.null(l$unreadable) && is.character(l$host) && length(l$host) == 1L &&
      !is.na(l$host) && nzchar(l$host)) {
    l$host
  } else {
    "unknown host"
  }
}

# The events table of a project (seq, ts, type, actor), empty when the log cannot be read.
.sr_events_table <- function(project) {
  empty <- data.frame(seq = integer(), ts = character(), type = character(), actor = character(),
                      stringsAsFactors = FALSE)
  if (is.null(project)) return(empty)
  evs <- tryCatch(suppressWarnings(sr_events_read(project$paths$events)), error = function(e) NULL)
  if (!length(evs)) return(empty)
  one <- function(e, k) {
    v <- e[[k]]
    if (is.null(v) || length(v) != 1L) NA else v
  }
  data.frame(
    seq = vapply(evs, function(e) as.integer(one(e, "seq")), integer(1)),
    ts = vapply(evs, function(e) as.character(one(e, "ts")), character(1)),
    type = vapply(evs, function(e) as.character(one(e, "type")), character(1)),
    actor = vapply(evs, function(e) as.character(one(e, "actor")), character(1)),
    stringsAsFactors = FALSE
  )
}

mod_project_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(5, 7),
    bslib::card(
      bslib::card_header("Project folder"),
      shiny::textInput(ns("dir"), "Folder path", width = "100%",
                       placeholder = "A new, empty folder to create, or an existing project folder"),
      shiny::textInput(ns("name"), "Project name (needed to create)", width = "100%"),
      shiny::div(
        class = "d-flex gap-2",
        shiny::actionButton(ns("create"), "Create new", class = "btn-primary"),
        shiny::actionButton(ns("open"), "Open existing", class = "btn-outline-primary")
      ),
      shiny::p(class = "text-muted small mt-3 mb-0", shiny::textOutput(ns("profile")))
    ),
    bslib::card(
      bslib::card_header("Status"),
      shiny::verbatimTextOutput(ns("status"), placeholder = TRUE),
      bslib::card_header("Events"),
      shiny::tableOutput(ns("events"))
    )
  )
}

mod_project_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    current <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal("No project open.")

    # Release the project this session holds, if any. Never errors.
    release <- function() {
      p <- shiny::isolate(current())
      if (!is.null(p) && !isTRUE(p$readonly)) try(sr_project_close(p), silent = TRUE)
      current(NULL)
      invisible(NULL)
    }

    # Run `fn()` (create or open): close the previous project first, then show the result or the error.
    load_project <- function(fn, describe) {
      release()
      res <- tryCatch(fn(), error = function(e) e)
      if (inherits(res, "error")) {
        status(paste0("Error: ", conditionMessage(res)))
        return(invisible(NULL))
      }
      current(res)
      status(describe(res))
      invisible(NULL)
    }

    text_in <- function(k) {
      v <- input[[k]]
      if (is.character(v) && length(v) == 1L && !is.na(v)) trimws(v) else ""
    }

    # The folder path as typed or pasted: trimmed, one pair of surrounding double quotes removed
    # (Windows "Copy as path" adds them), then made absolute (relative paths resolve against the
    # process working directory, which is app/ under Shiny) so the status shows where it really is.
    dir_in <- function() {
      d <- text_in("dir")
      if (nchar(d) >= 2L && startsWith(d, "\"") && endsWith(d, "\"")) d <- trimws(substr(d, 2L, nchar(d) - 1L))
      if (!nzchar(d)) stop("enter a project folder path", call. = FALSE)
      normalizePath(path.expand(d), winslash = "/", mustWork = FALSE)
    }

    shiny::observeEvent(input$create, {
      load_project(function() {
        d <- dir_in()
        nm <- text_in("name")
        if (!nzchar(nm)) stop("enter a project name", call. = FALSE)
        sr_project_create(d, nm)
      }, function(p) sprintf("Created project '%s' at %s", p$meta$name, p$paths$dir))
    })

    shiny::observeEvent(input$open, {
      load_project(function() sr_project_open(dir_in()), function(p) {
        msg <- if (isTRUE(p$readonly)) {
          sprintf("Opened project '%s' - read-only (locked by %s)", p$meta$name,
                  .sr_lock_host_label(p$paths))
        } else {
          sprintf("Opened project '%s' at %s", p$meta$name, p$paths$dir)
        }
        # A failed integrity check is reported, not refused: the project still opens for inspection.
        if (is.list(p$verify) && !isTRUE(p$verify$ok)) {
          msg <- paste0(msg, "\nIntegrity check failed: ", paste(p$verify$problems, collapse = "; "))
        }
        msg
      })
    })

    session$onSessionEnded(function() release())

    output$status <- shiny::renderText(status())
    output$events <- shiny::renderTable(.sr_events_table(current()), striped = TRUE, spacing = "s",
                                        na = "")
    output$profile <- shiny::renderText({
      p <- tryCatch(sr_profile(), error = function(e) NULL)
      if (is.null(p)) return("")
      caps <- sr_profile_caps(p)
      sprintf("New projects use the '%s' profile (%d workers, %g GB RAM budget).", p,
              caps$workers, caps$ram_budget_gb)
    })

    current
  })
}
