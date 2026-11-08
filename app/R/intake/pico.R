# intake/pico.R - the research question (PICO + setting, period, aim), as one event-sourced record.
#
# `pico.set` carries the whole question, not a patch: setting it again replaces it. Every field is a
# single trimmed string or NA. `aim` is the user's stated aim (one of SR_PURPOSES); the router's purpose
# node decides the analysis purpose, this is only an input to it. The question is frozen by the SAP lock.

SR_PICO_FIELDS <- c("population", "exposure", "comparator", "outcome", "timeframe", "setting",
                    "period", "aim")
SR_PICO_REQUIRED <- c("population", "outcome")
SR_PICO_MAX_CHARS <- 2000L

# One field value as character(1): trimmed, "" -> NA. NULL and NA are NA.
.sr_pico_value <- function(x, field) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) return(NA_character_)
  if (!is.character(x) || length(x) != 1L) {
    stop("PICO field '", field, "' must be a single string", call. = FALSE)
  }
  x <- trimws(x)
  if (nchar(x) > SR_PICO_MAX_CHARS) {
    stop("PICO field '", field, "' is longer than ", SR_PICO_MAX_CHARS, " characters", call. = FALSE)
  }
  if (!nzchar(x)) NA_character_ else x
}

#' Record the research question. Fields not given are NA; population and outcome are required.
#' `...` are named fields; `.list` is a named list of fields (merged with `...`).
sr_pico_set <- function(project, ..., .list = NULL) {
  .sr_sap_open(project, "the SAP is locked: the research question is frozen")
  args <- c(list(...), as.list(.list))
  nms <- names(args) %||% rep("", length(args))
  if (length(args) && (anyNA(nms) || any(!nzchar(nms)))) {
    stop("every PICO field must be named; fields: ", paste(SR_PICO_FIELDS, collapse = ", "), call. = FALSE)
  }
  bad <- setdiff(nms, SR_PICO_FIELDS)
  if (length(bad)) {
    stop("unknown PICO field(s): ", paste(bad, collapse = ", "), "; fields: ",
         paste(SR_PICO_FIELDS, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(nms)) {
    stop("PICO field(s) given more than once: ", paste(unique(nms[duplicated(nms)]), collapse = ", "),
         call. = FALSE)
  }
  vals <- stats::setNames(lapply(SR_PICO_FIELDS, function(f) .sr_pico_value(args[[f]], f)), SR_PICO_FIELDS)
  miss <- SR_PICO_REQUIRED[vapply(vals[SR_PICO_REQUIRED], is.na, logical(1))]
  if (length(miss)) stop("the research question needs: ", paste(miss, collapse = ", "), call. = FALSE)
  if (!is.na(vals$aim) && !(vals$aim %in% SR_PURPOSES)) {
    stop("aim must be one of: ", paste(SR_PURPOSES, collapse = ", "), call. = FALSE)
  }
  sr_project_append(project, "pico.set", vals)
}

sr_reducer_register("pico.set", function(state, event) {
  p <- event$payload
  q <- stats::setNames(lapply(SR_PICO_FIELDS, function(f) {
    v <- p[[f]]
    if (is.null(v) || (length(v) == 1L && is.na(v))) NA_character_ else as.character(v)
  }), SR_PICO_FIELDS)
  q$at <- event$ts
  state$pico <- q
  state
})

#' The recorded question (the eight fields plus `at`), or NULL before any pico.set.
sr_pico <- function(project) project$state$pico

#' `key: value` lines of the non-NA fields, in SR_PICO_FIELDS order (character(0) when none).
.sr_pico_text <- function(project) {
  q <- sr_pico(project)
  if (is.null(q)) return(character(0))
  v <- vapply(SR_PICO_FIELDS, function(f) q[[f]], character(1))
  keep <- !is.na(v)
  if (!any(keep)) return(character(0))
  paste0(SR_PICO_FIELDS[keep], ": ", v[keep])
}
