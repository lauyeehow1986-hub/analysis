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

#' Resolve the engine of one pre-specified analysis for the lock: the sr_engine_resolve result, or the error
#' object when the analysis cannot be sized. NULL when the analysis names no data columns (nothing to size).
.sr_sap_engine_one <- function(project, id) {
  a <- project$state$analyses[[id]]
  if (!length(sr_spec_cols(a$spec))) return(NULL)
  tryCatch(sr_engine_resolve(project, id), error = function(e) e)
}

#' The engines of the pre-specified analyses: a named list `id -> list(engine, requested, reason)`. After the
#' lock it is the frozen `lock$engines` (empty for a SAP locked before P1e). Before the lock the engines are
#' resolved now; an analysis that cannot be sized or whose resolution is blocked has `engine = NA`.
.sr_sap_engines <- function(project) {
  if (isTRUE(project$state$sap$locked)) return(project$state$sap$lock$engines %||% list())
  out <- list()
  for (id in names(.sr_sap_analyses(project))) {
    r <- .sr_sap_engine_one(project, id)
    if (is.null(r)) next
    out[[id]] <- if (inherits(r, "error")) {
      list(engine = NA_character_, requested = NA_character_, reason = conditionMessage(r))
    } else {
      list(engine = r$engine, requested = r$requested, reason = r$reason)
    }
  }
  out
}

# ---- engine switch (computational deviation) ---------------------------------------------------------

#' The engine an analysis runs on now: the latest switch, else the one frozen at the lock, else the one
#' resolved when the analysis was added, else "memory".
.sr_engine_effective <- function(project, id) {
  st <- project$state
  pick <- function(x) if (is.character(x) && length(x) == 1L && !is.na(x) && x %in% c("memory", "stream")) x
  pick(st$engines[[id]]$to) %||% pick(st$sap$lock$engines[[id]]$engine) %||%
    pick(st$analyses[[id]]$engine_resolved$engine) %||% "memory"
}

sr_reducer_register("engine.switched", function(state, event) {
  p <- event$payload
  id <- .sr_sap_chr1(p$analysis)
  if (is.na(id)) stop("engine.switched needs an analysis id", call. = FALSE)
  eg <- state$engines %||% list()
  one <- list(from = .sr_sap_chr1(p$from), to = .sr_sap_chr1(p$to), reason = .sr_sap_chr1(p$reason))
  # from/to/reason stay the latest switch (what .sr_engine_effective reads); history keeps every switch
  eg[[id]] <- c(one, list(history = c(eg[[id]]$history %||% list(), list(one))))
  state$engines <- eg
  state
})

#' Switch an analysis between the in-memory and the streaming engine. The estimator is unchanged, so the SAP
#' hash is too; the switch is listed by sr_deviations as a computational deviation. Refused for a read-only
#' project, a SAP that is not locked (before the lock the engine is part of the analysis spec), an unknown
#' analysis, an engine other than "memory"/"stream", no reason, the engine it already uses, "memory" for an
#' analysis that does not fit, and "stream" for one the streaming engine cannot run yet.
#' @return the project
sr_engine_switch <- function(project, analysis_id, engine, reason) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  if (sr_blind(project)) {
    stop("the SAP is not locked; an engine switch is recorded only after the lock (before it, edit the ",
         "analysis engine)", call. = FALSE)
  }
  a <- project$state$analyses[[analysis_id]]
  if (is.null(a)) stop("unknown analysis '", analysis_id, "'", call. = FALSE)
  if (!(is.character(engine) && length(engine) == 1L && !is.na(engine) && engine %in% c("memory", "stream"))) {
    stop("engine must be \"memory\" or \"stream\"", call. = FALSE)
  }
  reason <- .sr_sap_chr1(reason)
  if (is.na(reason) || !nzchar(trimws(reason))) stop("a reason is required", call. = FALSE)
  from <- .sr_engine_effective(project, analysis_id)
  if (identical(from, engine)) stop("analysis '", analysis_id, "' already uses the ", engine, " engine", call. = FALSE)
  spec <- .sr_spec_from_state(a$spec)
  if (identical(engine, "memory")) {
    est <- sr_mem_estimate(project, a$table, sr_spec_cols(spec), .sr_mem_method(a$route))
    if (!isTRUE(est$fits)) stop("the analysis does not fit in memory: ", est$reason, call. = FALSE)
  } else {
    why <- .sr_engine_refusal(a$route, spec)
    if (!is.null(why)) stop(why, call. = FALSE)
  }
  sr_project_append(project, "engine.switched",
                    list(analysis = analysis_id, from = from, to = engine, reason = trimws(reason)))
}

# ---- the run spec, progress and the Methods fragment (rulings R6, R7, R15, R17) ------------------------

#' `spec` as it runs on `engine` (ruling R6). For "memory" the fields `engine` and `stream_chunk` are removed,
#' so a spec written before P1e compiles and hashes byte-for-byte as it did. For "stream" `engine` is set to
#' "stream" and `stream_chunk` (an integer) to `chunk`, else the spec's own value, else
#' sr_opt("stream_chunk_rows", 250000) (ruling R7).
.sr_spec_for_engine <- function(spec, engine, chunk = NULL) {
  if (!identical(engine, "stream")) {
    spec[["engine"]] <- NULL
    spec[["stream_chunk"]] <- NULL
    return(spec)
  }
  ch <- chunk %||% spec[["stream_chunk"]] %||% sr_opt("stream_chunk_rows", 250000)
  ch <- suppressWarnings(as.numeric(ch))
  if (!(length(ch) == 1L && is.finite(ch) && ch >= 1 && ch == floor(ch) && ch <= .Machine$integer.max)) {
    stop("stream_chunk must be a whole number >= 1", call. = FALSE)
  }
  spec[["engine"]] <- "stream"
  spec[["stream_chunk"]] <- as.integer(ch)
  spec
}

#' The spec an analysis runs with: its state spec on `engine` (NULL: the effective engine,
#' .sr_engine_effective) with `chunk` (NULL: the spec's, else the option's). Used by sr_run, sr_result,
#' .sr_script_block and sr_diag_run.
.sr_run_spec <- function(project, id, engine = NULL, chunk = NULL) {
  a <- project$state$analyses[[id]]
  if (is.null(a)) stop("unknown analysis '", id, "'", call. = FALSE)
  if (is.null(engine)) engine <- .sr_engine_effective(project, id)
  .sr_spec_for_engine(.sr_spec_from_state(a$spec), engine, chunk)
}

#' The progress file of a streaming job (ruling R15): <project>/data/tmp/progress-<job>.json. The worker
#' gets it as the input sr_progress_file, outside the cache key, and writes {"pass": k, "max": m} to it.
.sr_progress_path <- function(project, job) {
  file.path(project$paths$dir, "data", "tmp", paste0("progress-", job, ".json"))
}

#' The DuckDB thread count the stream prologue of `route_id` emits as a literal, or NULL for the input
#' sr_duck_threads. Table 1 runs on one thread (ruling R14): one thread fixes the summation order of the SQL
#' aggregates, and the count is then set by the lockdown with no SET after it.
.sr_stream_threads <- function(route_id) {
  if (identical(as.character(route_id), "table1")) 1L else NULL
}

#' The kind of engine Methods sentence a route gets ("lm", "glm" or "table1"), or NULL for a route that
#' has no streaming engine (the six routes whose schema carries `engine` and `stream_chunk`).
.sr_engine_kind <- function(route_id) {
  kinds <- c(lm = "lm", lm_describe = "lm", logistic = "glm", logistic_describe = "glm", modpois = "glm",
             table1 = "table1")
  id <- as.character(route_id)[1L]
  if (is.na(id) || !(id %in% names(kinds))) return(NULL)
  kinds[[id]]
}

#' The engine requested for an analysis: the one frozen at the lock, else the one recorded when it was
#' added, else "auto". An engine.switched does not change it.
.sr_engine_requested <- function(project, id) {
  st <- project$state
  pick <- function(x) if (is.character(x) && length(x) == 1L && !is.na(x) && x %in% SR_ENGINES) x
  pick(st$sap$lock$engines[[id]]$requested) %||% pick(st$analyses[[id]]$engine_resolved$requested) %||% "auto"
}

#' The engine Methods sentence of a streamed result (spec section 7). Numbers are plain digits with no
#' thousands mark, because sr_ph_lift reads "1,003" as two numbers; the writer lifts them into
#' placeholders. `kind` is "glm" (iteratively reweighted least squares), "lm" (least squares) or "table1"
#' (over all rows: the overall n plus the rows excluded for a missing group). The spec sentence (the data
#' exceeded the in-memory budget) is used when the engine was resolved from "auto". When the analysis asked
#' for "stream" explicitly (`requested`), the sentence says so instead of claiming the budget was exceeded.
.sr_engine_fragment <- function(value, kind, requested = "auto") {
  num <- function(x) format(x, scientific = FALSE, trim = TRUE, big.mark = "")
  if (identical(kind, "table1")) {
    n <- value$n
    tot <- as.numeric(n[length(n)]) + as.numeric(value$n_excluded %||% 0)
    return(paste0("Descriptive statistics were computed exactly by SQL aggregation over all ", num(tot),
                  " rows."))
  }
  how <- if (identical(kind, "lm")) "least squares" else "iteratively reweighted least squares"
  tail <- paste0("This gives the same maximum-likelihood estimates and standard errors as standard software ",
                 "within numerical precision.")
  if (identical(requested, "stream")) {
    return(paste0("The analysis was run with the streaming engine, as requested, reading the data in chunks of ",
                  num(value$chunk), " rows, so that the models were fitted by streaming ", how, " over all ",
                  num(value$n), " rows. ", tail))
  }
  paste0("The analysis dataset (", num(value$n), " rows) exceeded the in-memory budget, so the models were ",
         "fitted by streaming ", how, " over fixed chunks of ", num(value$chunk), " rows. ", tail)
}
