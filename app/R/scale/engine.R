# scale/engine.R - the engine field (auto | memory | stream), its refusals and its resolution (P1e.1).
#
# `engine` is an optional spec field of lm, logistic, logistic_describe, modpois and table1 (cox and rmst
# take `engine` only, and refuse "stream" until P1e.2). "auto" (the default) runs in memory when the analysis
# fits the profile's RAM budget and streams when it does not; "memory" never switches (a table that does not
# fit is a block); "stream" always streams. The engine is resolved from the sizes, never from the data
# values, and is frozen at the SAP lock.

SR_ENGINES <- c("auto", "memory", "stream")

#' The reason a spec cannot run on the streaming engine yet, or NULL. Stateless: it looks at the route id and
#' the spec only. A route's cross-field validator calls it when engine = "stream"; sr_engine_resolve calls it
#' when "auto" resolves to stream.
.sr_engine_refusal <- function(route_id, spec) {
  id <- as.character(route_id)[1L]
  if (!is.na(id) && (startsWith(id, "cox") || startsWith(id, "rmst"))) {
    return("the streaming engine for Cox / RMST arrives in P1e.2")
  }
  L <- spec[["layers"]]
  if (identical(L[["missing"]], "mi")) return("multiple imputation at scale arrives in P1e.3")
  if (identical(L[["resampling"]], "bootstrap")) return("the bag of little bootstraps arrives in P1e.3")
  if (identical(spec[["penalty"]], "firth")) return("the Firth penalty at scale arrives in P1e.3")
  NULL
}

#' Cross-field validation of the engine fields, shared by the routes (stops with the reason): `engine` is one
#' of SR_ENGINES, `stream_chunk` a whole number >= 1, and a stream request has no refusal.
.sr_engine_check <- function(spec, route_id) {
  eng <- spec[["engine"]]
  if (!is.null(eng) && !(is.character(eng) && length(eng) == 1L && !is.na(eng) && eng %in% SR_ENGINES)) {
    stop("engine must be one of: ", paste(SR_ENGINES, collapse = ", "), call. = FALSE)
  }
  ch <- spec[["stream_chunk"]]
  if (!is.null(ch) && !(is.numeric(ch) && length(ch) == 1L && is.finite(ch) && ch >= 1 && ch == floor(ch))) {
    stop("stream_chunk must be a whole number >= 1", call. = FALSE)
  }
  if (identical(eng, "stream")) {
    why <- .sr_engine_refusal(route_id, spec)
    if (!is.null(why)) stop(why, call. = FALSE)
  }
  invisible(TRUE)
}

#' Resolve the engine of an analysis from its requested engine and the memory estimate.
#' @param spec NULL (the state spec of `analysis_id`) or a spec to resolve instead.
#' @return list(engine, requested, reason, estimate, block). `engine` is "memory" or "stream", NA when blocked;
#'   `block` is NULL or the text of the block.
sr_engine_resolve <- function(project, analysis_id, spec = NULL) {
  a <- project$state$analyses[[analysis_id]]
  if (is.null(a)) stop("unknown analysis '", analysis_id, "'", call. = FALSE)
  if (is.null(spec)) spec <- .sr_spec_from_state(a$spec)
  requested <- spec[["engine"]]
  if (is.null(requested)) requested <- "auto"
  if (!(is.character(requested) && length(requested) == 1L && requested %in% SR_ENGINES)) {
    stop("engine must be one of: ", paste(SR_ENGINES, collapse = ", "), call. = FALSE)
  }
  cols <- sr_spec_cols(spec)
  estimate <- sr_mem_estimate(project, a$table, cols, .sr_mem_method(a$route))
  engine <- switch(requested,
    memory = if (isTRUE(estimate$fits)) "memory" else NA_character_,
    stream = "stream",
    if (isTRUE(estimate$fits)) "memory" else "stream")
  block <- NULL
  if (is.na(engine)) {
    block <- paste0("memory engine requested but the analysis does not fit in memory: ", estimate$reason)
  } else if (identical(engine, "stream")) {
    block <- .sr_engine_refusal(a$route, spec)
    if (!is.null(block)) engine <- NA_character_
  }
  reason <- if (is.null(block)) paste0(requested, " -> ", engine, ": ", estimate$reason) else block
  list(engine = engine, requested = requested, reason = reason, estimate = estimate, block = block)
}
