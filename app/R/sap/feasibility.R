# sap/feasibility.R - the workflow of the study feasibility verdict (spec catalogue O): assess, decide,
# stop, report. The engine (sap/feas_engine.R) is pure; this file stores what it returns as events and
# folds them into state:
#   state$feasibility[[analysis_id]] = list(assessment, decision)
#   assessment = list(analysis, data_hash, spec_hash, delta, assumptions, inputs, contrasts, blockers,
#                     warnings, tier, advice)   (inputs without the outcome pairs; contrasts a data.frame)
#   inputs of an rmst route also carry rmst = list(sims, seed): the simulation settings used (R18)
#   decision   = list(decision, reason)         (NULL until sr_feasibility_decide)
# feasibility.assessed replaces the assessment and clears the decision; feasibility.decided sets it.
#
# An assessment is tied to the data and the spec it was made on: data_hash is the store hash of the
# analysed table, spec_hash the hash of (route, route version, table, spec). .sr_feas_stale() recomputes
# only those two hashes; the verdict is never recomputed. A stale assessment cannot be decided (a stop
# can still be replaced), and the lock gate (.sr_feas_gate) blocks it.
#
# A stop is sticky: while an analysis carries one it cannot be re-assessed, and only
# sr_feasibility_decide(..., reason) replaces it. sr_project_stopped() names it for the run and removal
# guards.
#
# Reducers see the event as a re-read parses it (NA -> NULL, vectors -> lists, data.frames as named
# lists of columns), so each normalises its payload to fixed shapes and column types (NULL -> NA);
# live folding and replay then give identical state.

SR_FEAS_DECISIONS <- c("proceed", "exploratory", "revise", "stop")

# ---- normalising event payloads ----------------------------------------------------------------------

# One value of `type` ("chr", "num", "int", "lgl"), NA when absent; a non-finite number is NA.
.sr_feas_one <- function(x, type) {
  na <- switch(type, chr = NA_character_, num = NA_real_, int = NA_integer_, lgl = NA)
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) return(na)
  v <- x[[1L]]
  switch(type, chr = as.character(v), lgl = as.logical(v),
         num = if (is.finite(v)) as.numeric(v) else na,
         int = if (is.finite(v)) as.integer(v) else na)
}

# A column of `type`: x is NULL, an atomic vector (live) or a list (a re-read; null elements -> NA).
.sr_feas_col <- function(x, type) {
  ctor <- switch(type, chr = character, num = numeric, int = integer, lgl = logical)
  if (is.null(x) || length(x) == 0L) return(ctor(0L))
  if (!is.list(x)) x <- as.list(x)
  vapply(x, function(e) .sr_feas_one(e, type), ctor(1L), USE.NAMES = FALSE)
}

# A data.frame of fixed columns from a named list of columns (or a data.frame); NULL stays NULL. A
# column that is NULL (a one-row column holding NA is unboxed to null by the JSON round trip) is NA.
.sr_feas_frame <- function(x, types) {
  if (is.null(x)) return(NULL)
  lens <- vapply(names(types), function(nm) length(x[[nm]]), 1L)
  n <- max(lens)
  cols <- lapply(names(types), function(nm) {
    v <- x[[nm]]
    .sr_feas_col(if (is.null(v)) as.list(rep(NA, n)) else v, types[[nm]])
  })
  names(cols) <- names(types)
  as.data.frame(cols, stringsAsFactors = FALSE)
}

# A data.frame as the named list of its columns: the payload form (an array per column).
.sr_feas_cols <- function(d) if (is.null(d)) NULL else as.list(d)

.sr_feas_levels_types <- c(level = "chr", n_all = "int", n_cc = "int")
.sr_feas_cin_types <- c(level = "chr", n_k = "int", n_ref = "int", r2x = "num", ps_outside = "num",
                        separated = "lgl")
.sr_feas_contrast_types <- c(contrast = "chr", level = "chr", measure = "chr", scale = "chr", se = "num",
                             h = "num", mde = "num", se_upper = "num", n_k = "int", n_ref = "int",
                             r2x = "num", deff = "num", tier = "chr")

# The fixed-shape list of named scalars: fields <- c(name = type).
.sr_feas_fields <- function(x, fields) {
  out <- lapply(names(fields), function(nm) .sr_feas_one(x[[nm]], fields[[nm]]))
  names(out) <- names(fields)
  out
}

# An assessment payload in its stored shape. NULL parts (an outcome-less kind, no exposure, no
# clusters, no delta) stay NULL (delta and the assumptions become all-NA lists).
.sr_feas_norm_assessment <- function(p) {
  inp <- p$inputs
  ex <- inp$exposure
  assm <- function(nm) .sr_feas_fields(p$assumptions[[nm]], c(value = "num", justification = "chr"))
  inputs <- list(
    kind = .sr_feas_one(inp$kind, "chr"), n_rows = .sr_feas_one(inp$n_rows, "int"),
    n_cc = .sr_feas_one(inp$n_cc, "int"), n_obs = .sr_feas_one(inp$n_obs, "int"),
    outcome = if (is.null(inp$outcome)) NULL else {
      .sr_feas_fields(inp$outcome, c(sd = "num", risk = "num", events_total = "num", events_cc = "num",
                                     followup_total = "num", max_time = "num"))
    },
    exposure = if (is.null(ex)) NULL else {
      list(var = .sr_feas_one(ex$var, "chr"), type = .sr_feas_one(ex$type, "chr"),
           ref = .sr_feas_one(ex$ref, "chr"), levels = .sr_feas_frame(ex$levels, .sr_feas_levels_types),
           variance = .sr_feas_one(ex$variance, "num"))
    },
    contrasts_in = .sr_feas_frame(inp$contrasts_in, .sr_feas_cin_types),
    clusters = if (is.null(inp$clusters)) NULL else {
      .sr_feas_fields(inp$clusters, c(G = "int", m_bar = "num", cv = "num"))
    },
    params = .sr_feas_one(inp$params, "int"), blockers = .sr_sap_chrv(inp$blockers))
  # rmst routes only: the simulation settings (R18); absent otherwise, so other records keep their shape
  if (!is.null(inp$rmst)) inputs$rmst <- .sr_feas_fields(inp$rmst, c(sims = "int", seed = "int"))
  list(
    analysis = .sr_sap_chr1(p$analysis), data_hash = .sr_sap_chr1(p$data_hash),
    spec_hash = .sr_sap_chr1(p$spec_hash),
    delta = .sr_feas_fields(p$delta, c(value = "num", measure = "chr", source = "chr")),
    assumptions = list(r2_y = assm("r2_y"), icc = assm("icc")),
    inputs = inputs,
    contrasts = .sr_feas_frame(p$contrasts, .sr_feas_contrast_types),
    blockers = .sr_sap_chrv(p$blockers), warnings = .sr_sap_chrv(p$warnings),
    tier = .sr_sap_chr1(p$tier), advice = .sr_sap_chrv(p$advice))
}

# ---- reducers ----------------------------------------------------------------------------------------

.sr_feas_id_of <- function(p, type) {
  id <- p$analysis
  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    stop(type, " needs an analysis id", call. = FALSE)
  }
  id
}

sr_reducer_register("feasibility.assessed", function(state, event) {
  p <- event$payload
  id <- .sr_feas_id_of(p, "feasibility.assessed")
  fe <- state$feasibility %||% list()
  fe[[id]] <- list(assessment = .sr_feas_norm_assessment(p), decision = NULL)
  state$feasibility <- fe
  state
})

sr_reducer_register("feasibility.decided", function(state, event) {
  p <- event$payload
  id <- .sr_feas_id_of(p, "feasibility.decided")
  fe <- state$feasibility %||% list()
  if (is.null(fe[[id]])) stop("feasibility.decided: analysis ", id, " has no assessment", call. = FALSE)
  fe[[id]]$decision <- list(decision = .sr_sap_chr1(p$decision), reason = .sr_sap_chr1(p$reason))
  state$feasibility <- fe
  state
})

# ---- staleness ---------------------------------------------------------------------------------------

# SHA-256 of the canonical JSON of (route, route version, table, spec) of an analysis record.
.sr_feas_spec_hash <- function(a) {
  .sr_sha256(sr_canonical_json(list(route = a$route, route_version = as.integer(a$route_version),
                                    table = a$table, spec = a$spec)))
}

# The store hash of `table`, NA when the table cannot be read.
.sr_feas_data_hash <- function(project, table) {
  tryCatch(sr_store_hash(sr_store_manifest(project, table)), error = function(e) NA_character_)
}

#' Whether the stored assessment of `id` still matches the project: NA when it is current, "data" when
#' the analysed table changed (a re-import), "spec" when the analysis (spec, route, route version or
#' table) changed or is gone. NA when there is no assessment. Recomputes the two hashes only, never
#' the verdict.
.sr_feas_stale <- function(project, id) {
  as <- project$state$feasibility[[id]]$assessment
  if (is.null(as)) return(NA_character_)
  a <- project$state$analyses[[id]]
  if (is.null(a) || !isTRUE(a$prespecified)) return("spec")
  if (!identical(.sr_feas_data_hash(project, a$table), as$data_hash)) return("data")
  if (!identical(.sr_feas_spec_hash(a), as$spec_hash)) return("spec")
  NA_character_
}

# ---- assess ------------------------------------------------------------------------------------------

# The text of a reason, NA when absent or blank; a non-text reason is refused.
.sr_feas_reason <- function(reason) {
  if (is.null(reason) || (length(reason) == 1L && is.na(reason))) return(NA_character_)
  if (!is.character(reason) || length(reason) != 1L) stop("reason must be a single string", call. = FALSE)
  if (nzchar(trimws(reason))) reason else NA_character_
}

#' Assess the feasibility of a pre-specified analysis, outcome-blind (sap/feas_engine.R), and record it
#' (`feasibility.assessed`). The stored assessment is tied to the table's store hash and the analysis'
#' spec hash; it replaces an earlier assessment and clears its decision. Refused when the project is
#' read-only, the SAP is locked, the analysis is unknown or not pre-specified, its decision is a stop
#' (reverse it with sr_feasibility_decide first), or `delta` / `assumptions` are invalid (a clustered
#' analysis needs assumptions$icc).
#' @param delta NULL or list(value, measure, source): the smallest clinically important difference.
#' @param assumptions list of r2_y and icc, each list(value, justification).
#' @return the project (`feasibility.assessed`).
sr_feasibility_assess <- function(project, analysis_id, delta = NULL, assumptions = list()) {
  .sr_sap_open(project, "the SAP is locked: the feasibility verdict is frozen")
  .sr_sap_id(analysis_id, "analysis_id")
  a <- .sr_sap_get(project, analysis_id)
  if (identical(project$state$feasibility[[analysis_id]]$decision$decision, "stop")) {
    stop("analysis ", .sr_sap_q(analysis_id), " is stopped: reverse the stop with sr_feasibility_decide first",
         call. = FALSE)
  }
  # an assumption given as NULL is an absent one: never stored, and the verdict warns which value it used
  if (is.list(assumptions)) {
    assumptions <- Filter(Negate(is.null), assumptions)
    if (!length(assumptions)) assumptions <- list()
  }
  route <- sr_route_get(a$route, a$route_version)
  spec <- .sr_spec_from_state(a$spec)
  cols <- sr_spec_cols(spec)
  df <- if (length(cols)) .sr_extract_frame(project, a$table, cols) else data.frame()
  inputs <- .sr_feas_inputs(df, spec, route)
  verdict <- .sr_feas_compute(inputs, spec, route, delta = delta, assumptions = assumptions)
  if (grepl("^rmst", route$id)) inputs$rmst <- .sr_feas_sim_settings(spec, route)   # R18
  .sr_feas_blind_frame(inputs, spec)
  .sr_feas_blind_frame(verdict, spec)
  if (!is.null(inputs$outcome)) inputs$outcome$pairs <- NULL
  if (!is.null(inputs$exposure)) inputs$exposure$levels <- .sr_feas_cols(inputs$exposure$levels)
  if (!is.null(inputs$contrasts_in)) inputs$contrasts_in <- .sr_feas_cols(inputs$contrasts_in)
  sr_project_append(project, "feasibility.assessed", list(
    analysis = analysis_id, data_hash = sr_store_hash(sr_store_manifest(project, a$table)),
    spec_hash = .sr_feas_spec_hash(a),
    delta = if (is.null(delta)) NA else list(value = delta$value, measure = delta$measure, source = delta$source),
    assumptions = lapply(assumptions, function(x) list(value = x$value, justification = x$justification)),
    inputs = inputs,
    contrasts = .sr_feas_cols(verdict$contrasts), blockers = verdict$blockers, warnings = verdict$warnings,
    tier = verdict$tier, advice = verdict$advice))
}

# ---- decide ------------------------------------------------------------------------------------------

#' Record the decision on the assessment of a pre-specified analysis (`feasibility.decided`): proceed,
#' exploratory, revise or stop. Refused when the SAP is locked, the analysis has no assessment, the
#' assessment is stale (except to replace a stop: a stop can always be reversed), proceed / exploratory
#' is chosen on a cannot_run verdict, or the reason is missing where it is required: for exploratory,
#' revise, stop, any decision replacing a stop, and proceed on an inadequate or precision_only verdict.
#' @return the project (`feasibility.decided`).
sr_feasibility_decide <- function(project, analysis_id, decision, reason = NA) {
  .sr_sap_open(project, "the SAP is locked: the feasibility verdict is frozen")
  .sr_sap_id(analysis_id, "analysis_id")
  .sr_sap_get(project, analysis_id)
  if (!is.character(decision) || length(decision) != 1L || is.na(decision) || !(decision %in% SR_FEAS_DECISIONS)) {
    stop("decision must be one of: ", paste(SR_FEAS_DECISIONS, collapse = ", "), call. = FALSE)
  }
  reason <- .sr_feas_reason(reason)
  rec <- project$state$feasibility[[analysis_id]]
  q <- .sr_sap_q(analysis_id)
  if (is.null(rec$assessment)) {
    stop("analysis ", q, " has no feasibility assessment: run sr_feasibility_assess first", call. = FALSE)
  }
  replacing_stop <- identical(rec$decision$decision, "stop")
  st <- .sr_feas_stale(project, analysis_id)
  if (!is.na(st) && !replacing_stop) {
    stop("analysis ", q, ": the feasibility assessment is stale (the ", st, " changed): assess again",
         call. = FALSE)
  }
  tier <- rec$assessment$tier
  if (identical(tier, "cannot_run") && decision %in% c("proceed", "exploratory")) {
    stop("analysis ", q, " cannot run (", paste(rec$assessment$blockers, collapse = "; "),
         "): decide revise or stop, not ", decision, call. = FALSE)
  }
  # when a reason is required: NA no, "" yes, text yes with the text as the explanation
  need <- if (replacing_stop) {
    "replacing a stop"
  } else if (decision %in% c("exploratory", "revise", "stop")) {
    ""
  } else if (tier %in% c("inadequate", "precision_only")) {
    paste0("tier ", tier)
  } else {
    NA_character_
  }
  if (!is.na(need) && is.na(reason)) {
    stop("analysis ", q, ": a reason is required to decide ", decision,
         if (nzchar(need)) paste0(" (", need, ")") else "", call. = FALSE)
  }
  sr_project_append(project, "feasibility.decided", list(analysis = analysis_id, decision = decision, reason = reason))
}

# ---- reading -----------------------------------------------------------------------------------------

#' The stored feasibility record of an analysis, as recorded: list(assessment, decision, stale).
#' assessment and decision are NULL until made; stale is NA, "data" or "spec" (.sr_feas_stale).
sr_feasibility <- function(project, analysis_id) {
  .sr_sap_id(analysis_id, "analysis_id")
  rec <- project$state$feasibility[[analysis_id]]
  list(assessment = rec$assessment, decision = rec$decision, stale = .sr_feas_stale(project, analysis_id))
}

#' The first pre-specified analysis, by id, whose current decision is stop: list(analysis, reason), or
#' NULL when none is stopped.
sr_project_stopped <- function(project) {
  an <- .sr_sap_analyses(project)
  for (id in sort(as.character(names(an)), method = "radix")) {
    d <- project$state$feasibility[[id]]$decision
    if (identical(d$decision, "stop")) return(list(analysis = id, reason = d$reason))
  }
  NULL
}

# Exposure prevalence of a stored assessment (R12), from the stored counts only: the share of the
# complete cases at each exposure level (inputs$exposure$levels), as a named list. NA_real_ for a
# continuous exposure, kind none, or no complete cases.
.sr_feas_exposure_prevalence <- function(inputs) {
  ex <- inputs$exposure
  lv <- ex$levels
  n <- inputs$n_cc
  if (is.null(ex) || is.null(lv) || !nrow(lv) || !(ex$type %in% c("factor", "binary")) ||
      is.na(n) || n < 1L) {
    return(NA_real_)
  }
  stats::setNames(as.list(lv$n_cc / n), lv$level)
}

# The exposure prevalence of each contrast row (n_k / n_cc, the share of complete cases at the row's
# contrast level, from contrasts_in); NA for a continuous exposure, no complete cases or no row.
.sr_feas_row_prevalence <- function(inputs, level) {
  ci <- inputs$contrasts_in
  n <- inputs$n_cc
  if (is.null(ci) || is.na(n) || n < 1L) return(rep(NA_real_, length(level)))
  nk <- ci$n_k[match(level, ci$level)]
  ifelse(is.na(level), NA_real_, nk / n)
}

#' The stored feasibility verdicts of the pre-specified analyses as one table (stored records only; the
#' stale column compares the two hashes): data.frame(analysis, label, route, contrast, measure, n_cc,
#' events, prevalence, se, h, mde, se_upper, delta, delta_measure, delta_source, tier, decision, reason,
#' stale, blockers, warnings). One row per (contrast x measure), or one row with NA contrast columns
#' when there is none (a cannot_run or not_applicable verdict). `events` is the expected number of
#' events among the complete cases, `prevalence` the exposure prevalence of the row's contrast level
#' (n_k / n_cc; NA for a continuous exposure), `tier` the tier of the row (the overall tier when there is no row),
#' blockers / warnings the stored texts joined by "; " ("" when none). Analyses without an assessment
#' are not listed.
sr_feasibility_report <- function(project) {
  an <- .sr_sap_analyses(project)
  rows <- list()
  for (id in sort(as.character(names(an)), method = "radix")) {
    rec <- project$state$feasibility[[id]]
    as <- rec$assessment
    if (is.null(as)) next
    ct <- as$contrasts
    has <- nrow(ct) > 0L
    pick <- function(col, na) if (has) ct[[col]] else na
    d <- as$delta
    rows[[length(rows) + 1L]] <- data.frame(
      analysis = id, label = an[[id]]$label, route = an[[id]]$route,
      contrast = pick("contrast", NA_character_), measure = pick("measure", NA_character_),
      n_cc = as$inputs$n_cc, events = as$inputs$outcome$events_cc %||% NA_real_,
      prevalence = if (has) .sr_feas_row_prevalence(as$inputs, ct$level) else NA_real_,
      se = pick("se", NA_real_), h = pick("h", NA_real_), mde = pick("mde", NA_real_),
      se_upper = pick("se_upper", NA_real_), delta = d$value, delta_measure = d$measure,
      delta_source = d$source, tier = if (has) ct$tier else as$tier,
      decision = rec$decision$decision %||% NA_character_, reason = rec$decision$reason %||% NA_character_,
      stale = .sr_feas_stale(project, id), blockers = paste(as$blockers, collapse = "; "),
      warnings = paste(as$warnings, collapse = "; "), stringsAsFactors = FALSE)
  }
  if (!length(rows)) {
    return(data.frame(analysis = character(), label = character(), route = character(),
                      contrast = character(), measure = character(), n_cc = integer(), events = numeric(),
                      prevalence = numeric(), se = numeric(), h = numeric(), mde = numeric(),
                      se_upper = numeric(), delta = numeric(), delta_measure = character(),
                      delta_source = character(), tier = character(), decision = character(),
                      reason = character(), stale = character(), blockers = character(),
                      warnings = character(), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' The Methods fragment inputs of an assessed analysis (inputs, not prose: the P1c convention):
#' list(template = "feasibility_methods", values = list(n, events, prevalence, h, mde, delta,
#' delta_source, tier)). n is the complete-case n, events as in the report, prevalence the named list of every exposure level's
#' share of the complete cases (NA for a continuous exposure, kind none, no complete case), h and mde
#' those of the least precise contrast (largest mde) of the delta measure, or of the first measure when
#' there is no delta (NA when none is computed), tier the overall tier. Refused without an assessment.
sr_feasibility_fragment <- function(project, analysis_id) {
  .sr_sap_id(analysis_id, "analysis_id")
  .sr_sap_get(project, analysis_id)
  as <- project$state$feasibility[[analysis_id]]$assessment
  if (is.null(as)) {
    stop("analysis ", .sr_sap_q(analysis_id), " has no feasibility assessment: run sr_feasibility_assess first",
         call. = FALSE)
  }
  ct <- as$contrasts
  h <- mde <- NA_real_
  if (nrow(ct)) {
    m <- if (!is.na(as$delta$measure)) as$delta$measure else ct$measure[1L]
    sub <- ct[ct$measure == m & is.finite(ct$mde), , drop = FALSE]
    if (nrow(sub)) {
      i <- which.max(sub$mde)
      h <- sub$h[i]
      mde <- sub$mde[i]
    }
  }
  list(template = "feasibility_methods",
       values = list(n = as$inputs$n_cc, events = as$inputs$outcome$events_cc %||% NA_real_,
                     prevalence = .sr_feas_exposure_prevalence(as$inputs), h = h, mde = mde, delta = as$delta$value,
                     delta_source = as$delta$source, tier = as$tier))
}

# ---- the lock gate -----------------------------------------------------------------------------------

#' One detail per feasibility problem that blocks the SAP lock (sr_sap_check, later):
#'  - of every pre-specified analysis that carries a stop (any label): "<id>: decision is stop (<reason>)";
#'  - of the primary analyses: "<id>: no feasibility assessment"; "<id>: the feasibility assessment is
#'    stale (the data changed)" or "(the spec changed)"; "<id>: no feasibility decision" (not stale);
#'    "<id>: decision is revise: change the design, assess and decide again" (not stale).
#' In order of analysis id. character(0) when nothing blocks.
.sr_feas_gate <- function(project) {
  an <- .sr_sap_analyses(project)
  out <- character()
  for (id in sort(as.character(names(an)), method = "radix")) {
    primary <- identical(an[[id]]$label, "primary")
    rec <- project$state$feasibility[[id]]
    dec <- rec$decision$decision
    stopped <- identical(dec, "stop")
    if (primary && is.null(rec$assessment)) {
      out <- c(out, paste0(id, ": no feasibility assessment"))
      next
    }
    st <- if (primary) .sr_feas_stale(project, id) else NA_character_
    if (!is.na(st)) out <- c(out, paste0(id, ": the feasibility assessment is stale (the ", st, " changed)"))
    if (stopped) {
      out <- c(out, paste0(id, ": decision is stop (", rec$decision$reason %||% "no reason", ")"))
    } else if (primary && is.na(st)) {
      if (is.null(dec)) {
        out <- c(out, paste0(id, ": no feasibility decision"))
      } else if (identical(dec, "revise")) {
        out <- c(out, paste0(id, ": decision is revise: change the design, assess and decide again"))
      }
    }
  }
  out
}
