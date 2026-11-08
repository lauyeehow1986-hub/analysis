# Shared helpers of the UI modules (Data, Design, SAP): notifications, the act() factory, cards, tables, the
# read-only banner and the export listing. Pure where it can be; the Shiny calls sit behind small wrappers.

# A single trimmed string from an input value, "" when it is not one.
.sr_ui_text <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x)) trimws(x) else ""
}

#' Run fn() for the user: an error becomes an error notification and NULL is returned; a warning becomes a
#' warning notification and the result is kept. Never lets a condition escape into the session.
.sr_ui_try <- function(label, fn) {
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

.sr_ui_notify <- function(msg, type = "message") {
  try(shiny::showNotification(msg, type = type, duration = 8), silent = TRUE)
}

# A severity (block, warn, info) or readiness status (block, warn, ok) as a badge. `data-status` carries
# the plain status for tests and scripts.
.sr_ui_badge <- function(status) {
  cls <- switch(status, block = "bg-danger", warn = "bg-warning text-dark", ok = "bg-success", "bg-secondary")
  sprintf("<span class=\"badge %s\" data-status=\"%s\">%s</span>", cls, status, status)
}

# A data frame as a compact DT (server mode, no search box, no paging: the callers page themselves).
.sr_ui_dt <- function(df, escape = TRUE, page_length = 15L) {
  DT::datatable(df, rownames = FALSE, selection = "none", escape = escape,
                options = list(dom = "tip", pageLength = page_length, scrollX = TRUE, ordering = FALSE),
                class = "compact stripe")
}

# One card of a tab. The tab is a normal scrolling page: a card is not a fill item (it would be
# squeezed into the viewport height) and its body is not a fill container, so it sizes to its content.
.sr_ui_card <- function(header, ...) {
  bslib::card(fill = FALSE, bslib::card_header(header), bslib::card_body(fillable = FALSE, fill = FALSE, ...))
}

# Why an edit is refused (no project, busy, read-only), or NULL when it may go ahead. Shared by act() and
# the actions that do not return a project.
.sr_ui_refusal <- function(p, busy = function() FALSE) {
  if (is.null(p)) return("open a project first")
  if (isTRUE(busy())) return("wait for the import to finish")
  if (isTRUE(p$readonly)) return("the project is read-only (locked by another session)")
  NULL
}

#' The act(label, fn) closure of a module: runs a project-changing step for the user. Refuses with no project,
#' while `busy()` is TRUE and on a read-only project; reports core errors and warnings as notifications and
#' writes back any result that carries `$paths` (a project). `project` is the reactiveVal holding the project.
.sr_ui_actor <- function(project, busy = function() FALSE) {
  function(label, fn) {
    p <- shiny::isolate(project())
    why <- .sr_ui_refusal(p, busy)
    if (!is.null(why)) {
      .sr_ui_notify(paste0(label, ": ", why), "error")
      return(invisible(NULL))
    }
    res <- .sr_ui_try(label, function() fn(p))
    if (is.list(res) && !is.null(res$paths)) project(res)
    invisible(res)
  }
}

# TRUE iff the project is open, not read-only and its SAP is not locked.
.sr_ui_editable <- function(p) {
  !is.null(p) && !isTRUE(p$readonly) && !isTRUE(p$state$sap$locked)
}

# The banner that says why the editing controls are off; NULL when they are on or there is no project.
.sr_ui_readonly_banner <- function(p) {
  if (is.null(p) || .sr_ui_editable(p)) return(NULL)
  msg <- if (isTRUE(p$readonly)) {
    "This project is read-only: another session holds its lock."
  } else {
    sprintf("The SAP is locked (%s); the plan can no longer be edited.", p$state$sap$lock$locked_at %||% "")
  }
  shiny::div(class = "alert alert-warning", msg)
}

# The exports of one kind as data.frame(id, path), in event order; path is absolute.
.sr_ui_exports <- function(p, kind) {
  ex <- p$state$exports %||% list()
  ex <- Filter(function(e) identical(e$kind, kind), ex)
  if (!length(ex)) return(data.frame(id = character(), path = character(), stringsAsFactors = FALSE))
  data.frame(id = vapply(ex, function(e) as.character(e$id), ""),
             path = vapply(ex, function(e) file.path(p$paths$dir, e$files[[1L]]$path), ""),
             stringsAsFactors = FALSE)
}

# Open a folder in the OS file browser (tests mock this).
.sr_ui_open_folder <- function(dir) {
  if (.Platform$OS.type == "windows") shell.exec(dir) else utils::browseURL(dir)
}
