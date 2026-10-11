# profile/suggest.R - the Data Action Suggestor and the readiness panel.
#
#   sr_suggest(project, table)      one routed action per warn+ finding of the latest DQ run of `table`
#   sr_action_decide(project, finding_id, action, reason, step)   record the decision (`action.decided`)
#   sr_readiness(project, table)    what still blocks analysis, one row per item
#
# Routing (see .sr_sg_default): every check has a default action, the protected roles (SR_PROTECTED_ROLES)
# turn drop_var into keep_sensitivity (nzv, collinear, high_card) or recheck (leakage), and a check the
# table does not know is a recheck. A suggestion proposes a recipe step only where the step follows from
# the finding alone: unit_mix (convert_unit with the inverse of the template factor, restricted by the
# `where` of the finding) and missing_codes (set_missing with the `where` of the finding).
#
# A decision is the action the user picked. It is an OVERRIDE when it differs from the suggested action,
# and an override needs a reason. When the action takes a step (fix, drop_cases, drop_var,
# set_missing_impute) the step is added through sr_recipe_add() with the finding_id set: the step the
# user passed, else the one the finding implies (the proposed step; a cast for a coercion fix; a drop_var
# of the column). A decision that has no step (dup_rows, missing_col: no recipe op can express them) is
# still recorded, with step_id NA and a detail that says the step is pending, and it counts as decided.
# state$actions holds the latest decision per table and finding, keyed "<table>:<finding_id>" (read it with
# sr_action()); the event log keeps them all. A finding id is a hash of the check, the column and the
# filter, so the same id occurs on a table and its re-imported version: a decision is always about one
# table and never reads or changes another table's decision or step.
# A later decision for a finding on the same table supersedes the earlier one: if the step the earlier
# decision added is still active on the current branch, that branch is bound to the same table, and the
# new decision does not reuse it (same op), it is undone through `recipe.undo` (by step id) and its id is
# recorded as undone_step_id in the new `action.decided`, so the decision record and the recipe never
# disagree. While blind, the typed reasons pass the outcome-blind text screen.
#
# Outcome-blind rule: a reason is built from the check, the column and the counts of the finding, never
# from an example value, a level, an AUC or any statistic that involves the outcome. While
# sr_blind(project) the suggestion frame passes .sr_assert_blind() with the outcome family
# (sr_outcome_family_cols) as the outcome columns before it is returned. A finding whose counts are
# withheld (an outcome-dependent table while blind) reads "some rows". The readiness panel hides the row
# count of an outcome-dependent table while blind.

SR_ACTIONS <- c("recheck", "fix", "drop_cases", "drop_var", "set_missing_impute", "keep_sensitivity")

# The key of a decision in state$actions: "<table>:<finding_id>" (the finding id alone for an event that
# names no table).
.sr_action_key <- function(table, finding_id) {
  if (is.character(table) && length(table) == 1L && !is.na(table) && nzchar(table)) paste0(table, ":", finding_id) else finding_id
}

#' The latest decision for `finding_id` on `table` (the action.decided payload), or NULL when there is
#' none. With table NULL, the one decision for that finding on any table; an error when it was decided on
#' more than one table.
sr_action <- function(project, finding_id, table = NULL) {
  act <- project$state$actions %||% list()
  if (!is.null(table)) return(act[[.sr_action_key(table, finding_id)]] %||% act[[finding_id]] %||% NULL)
  hits <- Filter(function(a) identical(as.character(a$finding_id), finding_id), act)
  if (!length(hits)) return(NULL)
  tabs <- unique(vapply(hits, function(a) as.character(a$table %||% ""), ""))
  if (length(tabs) > 1L) {
    stop("finding_id '", finding_id, "' was decided on more than one table (", paste(tabs, collapse = ", "), "); pass table",
         call. = FALSE)
  }
  hits[[1L]]
}

# The active steps of the current recipe branch when it is bound to `table`, else none: the only steps a
# decision on `table` may reuse or undo.
.sr_sg_table_steps <- function(project, table) {
  rec <- .sr_rc_state(project$state)
  br <- rec$branches[[rec$current]]
  if (is.null(br) || !identical(br$table, table)) return(list())
  .sr_rc_active(br)
}

# The actions that take a recipe step.
.sr_sg_step_actions <- c("fix", "drop_cases", "drop_var", "set_missing_impute")

# ---- routing ----------------------------------------------------------------------------------------

.sr_sg_default <- c(
  unit_mix = "fix", coercion = "recheck", missing_codes = "set_missing_impute", range = "recheck",
  future_date = "recheck", date_order = "recheck", dup_rows = "drop_cases", dup_id = "recheck",
  nzv = "drop_var", collinear = "drop_var", high_card = "drop_var", leakage = "drop_var",
  separation = "keep_sensitivity", outlier = "keep_sensitivity", immortal_time = "keep_sensitivity",
  heaping = "keep_sensitivity", missing_col = "set_missing_impute", dict_levels = "recheck",
  dict_range = "recheck", check_error = "recheck",
  # not in the table of the plan: a data problem is rechecked, a missingness finding is a sensitivity analysis
  dict_type = "recheck", old_date = "recheck", missing_row = "recheck", missing_pattern = "keep_sensitivity",
  missing_dependence = "keep_sensitivity", post_outcome_date = "recheck", cluster_detect = "recheck",
  offset_detect = "recheck")

# The suggested action of a check; `protected` is TRUE when the column has a protected role.
.sr_sg_route <- function(check, protected = FALSE) {
  a <- unname(.sr_sg_default[check])
  if (length(a) != 1L || is.na(a)) return("recheck")
  if (isTRUE(protected) && a == "drop_var") return(if (check == "leakage") "recheck" else "keep_sensitivity")
  a
}

# ---- reasons ----------------------------------------------------------------------------------------

# "<n> row(s) (<p>)" for a finding; NA counts (a blind leakage flag) read "some rows".
.sr_sg_rows <- function(f) {
  n <- f$n_affected
  if (is.na(n)) "some rows" else sprintf("%s row(s) (%s)", .sr_dq_big(n), .sr_dq_pct(f$p_affected))
}

# One sentence naming the evidence of finding row `f` (a one-row findings frame) and what the action does.
# `protected` is TRUE for a protected-role column, `is_outcome` for the outcome column (its values stay out).
.sr_sg_reason <- function(f, protected = FALSE, is_outcome = FALSE) {
  check <- f$check
  col <- f$column
  what <- if (identical(col, "*")) "the table" else col
  d <- f$detail[[1L]]
  rows <- .sr_sg_rows(f)
  sentence <- switch(check,
    unit_mix = {
      u <- d$units
      if (is.null(u) || !nrow(u) || is.na(f$n_affected)) {
        sprintf("%s holds %s that fit another unit; convert them with the factor of the plausibility template", what, rows)
      } else {
        sprintf("%s holds %s value(s) that fit %s rather than %s, so convert them with the %s factor of the plausibility template",
                what, .sr_dq_big(f$n_affected), u$alt_unit[1L], u$primary_unit[1L], u$analyte[1L])
      }
    },
    coercion = sprintf("%s is stored as text and %s do not parse as numbers or dates; recheck them against the source before casting",
                       what, rows),
    missing_codes = {
      code <- d$codes$code[1L]
      code_txt <- if (is_outcome || is.null(code) || is.na(code)) "a placeholder code" else sprintf("the placeholder code %s", format(code))
      sprintf("%s holds %s in %s; set it to missing", what, code_txt, rows)
    },
    range = sprintf("%s has %s outside the hard plausible range; recheck them against the source", what, rows),
    future_date = sprintf("%s has %s dated in the future; recheck them against the source", what, rows),
    date_order = sprintf("%s is out of order against the date it must follow in %s; recheck the dates against the source", what, rows),
    dup_rows = {
      dp <- d$duplicates
      if (is.null(dp) || !nrow(dp) || anyNA(c(dp$extra_rows[1L], dp$groups[1L]))) {
        sprintf("%s has %s that repeat an earlier row; keep the first occurrence of each", what, rows)
      } else {
        sprintf("%s has %s extra copy(ies) of rows in %s group(s); keep the first occurrence of each",
                what, .sr_dq_big(dp$extra_rows[1L]), .sr_dq_big(dp$groups[1L]))
      }
    },
    dup_id = sprintf("%s repeats in %s; recheck whether this is a repeated-measures table or a duplicated record", what, rows),
    nzv = if (protected) {
      sprintf("%s has almost no variation (%s) but a protected role; keep it and run a sensitivity analysis", what, rows)
    } else {
      sprintf("%s has almost no variation (%s), so it cannot inform a model; drop it", what, rows)
    },
    collinear = if (protected) {
      sprintf("%s is almost a copy of another column but has a protected role; keep it and run a sensitivity analysis", what)
    } else {
      sprintf("%s is almost a linear copy of another column (%s); drop it", what, rows)
    },
    high_card = if (protected) {
      sprintf("%s has nearly as many distinct values as rows but a protected role; keep it and run a sensitivity analysis", what)
    } else {
      sprintf("%s has nearly as many distinct values as rows, so a model cannot use it; drop it", what)
    },
    leakage = if (protected) {
      sprintf("%s is flagged as near-deterministic for the outcome but has a protected role; recheck its role before analysis", what)
    } else {
      sprintf("%s is near-deterministic for the outcome, so it is probably recorded from or after it; drop it", what)
    },
    separation = sprintf(paste0("%s has a level in which the outcome never varies; keep it and use a penalised-likelihood ",
                                "fit as the sensitivity analysis"), what),
    outlier = sprintf("%s has %s that are extreme; keep them and run a sensitivity analysis without them", what, rows),
    immortal_time = sprintf(paste0("%s falls after time zero in %s, so the exposed are immortal until then; keep it and ",
                                   "handle it in the design (a landmark or a time-varying exposure)"), what, rows),
    heaping = sprintf("%s has values heaped on round numbers in %s; keep it and run a sensitivity analysis on the rounding", what, rows),
    missing_col = sprintf("%s is missing in %s; impute under a stated missing-data assumption", what, rows),
    dict_levels = sprintf("%s has %s with levels the dictionary does not list; recheck them against the source or the dictionary",
                          what, rows),
    dict_range = sprintf("%s has %s outside the range of the dictionary; recheck them against the source or the dictionary",
                         what, rows),
    dict_type = sprintf("the dictionary type of %s disagrees with the stored type; recheck the dictionary or the import", what),
    old_date = sprintf("%s has %s dated implausibly long ago; recheck them against the source", what, rows),
    missing_row = sprintf("%s has %s that are mostly empty; recheck whether they are real records", what, rows),
    missing_pattern = sprintf("%s shows a pattern in its missing values; keep it and run a missing-data sensitivity analysis", what),
    missing_dependence = sprintf(paste0("the missingness of %s depends on other columns; keep it and run a sensitivity ",
                                        "analysis of the missing-data assumption"), what),
    post_outcome_date = sprintf("%s is recorded after the event date in %s; recheck whether it can be a baseline predictor", what, rows),
    cluster_detect = sprintf("%s looks like a clustering variable; recheck its role", what),
    offset_detect = sprintf("%s looks like an exposure-time offset; recheck its role", what),
    check_error = "a check failed to run; recheck the data and run it again",
    sprintf("the %s check flagged %s on %s; recheck it against the source", check, rows, what))
  sentence
}

# ---- steps ------------------------------------------------------------------------------------------

# The alternative unit of an analyte column as list(factor, offset): from the unit_mix detail, else from
# the plausibility template (the single alternative unit, or the one the detail names).
.sr_sg_unit <- function(d, col) {
  u <- d$units
  if (!is.null(u) && nrow(u) && is.numeric(u$factor) && is.finite(u$factor[1L]) && u$factor[1L] != 0) {
    off <- if (is.numeric(u$offset) && is.finite(u$offset[1L])) u$offset[1L] else 0
    return(list(factor = u$factor[1L], offset = off))
  }
  a <- tryCatch(.sr_pl_analyte(col), error = function(e) NULL)
  if (is.null(a) || !nrow(a$alt)) return(NULL)
  alt <- a$alt
  if (!is.null(u) && nrow(u) && "alt_unit" %in% names(u)) alt <- alt[alt$unit %in% u$alt_unit[1L], , drop = FALSE]
  if (nrow(alt) != 1L || !is.finite(alt$factor) || alt$factor == 0) return(NULL)
  list(factor = alt$factor, offset = alt$offset)
}

# TRUE when column `col` of the store of `table` has a numeric type (convert_unit needs one).
.sr_sg_is_num <- function(project, table, col) {
  m <- tryCatch(sr_store_manifest(project, table), error = function(e) NULL)
  if (is.null(m)) return(FALSE)
  for (s in m$schema) if (identical(s$name, col)) return(identical(.sr_rc_family(s$type), "num"))
  FALSE
}

# The recipe step that finding row `f` implies for `action`, or NULL. alt = primary * factor + offset in
# the template, so converting back is x / factor - offset / factor. With derive = FALSE only the steps a
# suggestion proposes (unit_mix fix, missing_codes set_missing_impute); with derive = TRUE also the cast of
# a coercion fix and the drop_var of the column of a drop_var decision.
.sr_sg_step <- function(project, table, f, action, reason, derive = FALSE) {
  check <- f$check
  col <- f$column
  w <- f$where[[1L]]
  mk <- function(op, args) list(op = op, args = args, reason = reason, finding_id = f$id)
  if (check == "unit_mix" && action == "fix") {
    u <- .sr_sg_unit(f$detail[[1L]], col)
    if (is.null(u) || is.null(w) || !.sr_sg_is_num(project, table, col)) return(NULL)
    off <- -u$offset / u$factor
    if (off == 0) off <- 0
    return(mk("convert_unit", list(col = col, where = w, factor = signif(1 / u$factor, 12), offset = signif(off, 12))))
  }
  if (check == "missing_codes" && action == "set_missing_impute") {
    if (is.null(w)) return(NULL)
    return(mk("set_missing", list(col = col, where = w)))
  }
  if (!derive) return(NULL)
  if (check == "coercion" && action == "fix") {
    pc <- tryCatch(sr_profile_table(project, table)$columns, error = function(e) NULL)
    i <- if (is.null(pc)) NA_integer_ else match(col, pc$column)
    if (is.na(i)) return(NULL)
    type <- if (isTRUE(pc$p_numeric_parsable[i] >= SR_DQ_COERCE_MIN)) "DOUBLE" else
      if (isTRUE(pc$p_date_parsable[i] >= SR_DQ_COERCE_MIN)) "DATE" else NA_character_
    if (is.na(type)) return(NULL)
    return(mk("cast", list(col = col, type = type)))
  }
  if (action == "drop_var" && !identical(col, "*")) return(mk("drop_var", list(col = col)))
  NULL
}

# ---- one finding ------------------------------------------------------------------------------------

# The suggestion for finding row `f` of `table`: list(action, reason, step, protected).
.sr_sg_one <- function(project, table, f, roles, outcome_cols) {
  col <- f$column
  protected <- col %in% names(roles) && roles[[col]] %in% SR_PROTECTED_ROLES
  action <- .sr_sg_route(f$check, protected)
  reason <- .sr_sg_reason(f, protected = protected, is_outcome = col %in% outcome_cols)
  step <- .sr_sg_step(project, table, f, action, reason, derive = FALSE)
  if (is.null(step) && f$check == "unit_mix" && action == "fix" && !.sr_sg_is_num(project, table, col)) {
    reason <- paste0(reason, "; the column is stored as text, so cast it to a number before converting")
  }
  list(action = action, reason = reason, protected = protected, step = step)
}

#' The Data Action Suggestor: one routed action per finding of severity warn or block of the latest DQ run
#' of `table` (an info finding gets none). `reason` is one sentence naming the evidence; `proposed_step`
#' is a recipe step (list(op, args, reason, finding_id)) or NULL. drop_var is never proposed for a column
#' with a protected role. While sr_blind(project) the frame passes .sr_assert_blind().
#' @return data.frame(finding_id, check, column, action, reason, proposed_step (list)), in finding order.
sr_suggest <- function(project, table) {
  .sr_store_check_table(table)
  f <- sr_dq_findings(project, table)
  f <- f[f$severity != "info", , drop = FALSE]
  roles <- sr_roles(project, table)
  outcome_cols <- sr_outcome_family_cols(project, table)
  n <- nrow(f)
  one <- lapply(seq_len(n), function(i) .sr_sg_one(project, table, f[i, , drop = FALSE], roles, outcome_cols))
  out <- data.frame(finding_id = as.character(f$id), check = as.character(f$check), column = as.character(f$column),
                    action = vapply(one, function(o) o$action, ""), reason = vapply(one, function(o) o$reason, ""),
                    stringsAsFactors = FALSE)
  steps <- vector("list", n)
  for (i in seq_len(n)) steps[i] <- list(one[[i]]$step)
  out$proposed_step <- steps
  if (sr_blind(project)) .sr_assert_blind(out, outcome_cols, setdiff(names(roles), outcome_cols))
  out
}

# ---- deciding ---------------------------------------------------------------------------------------

# The finding `finding_id` in the latest DQ run of `table` (every table with a run when NULL):
# list(table, finding) with the finding as a one-row frame. Unknown, or in more than one table: an error.
.sr_sg_locate <- function(project, finding_id, table = NULL) {
  tabs <- if (is.null(table)) names(project$state$dq) else table
  hits <- list()
  for (t in tabs) {
    f <- sr_dq_findings(project, t)
    i <- which(f$id == finding_id)
    if (length(i)) hits[[length(hits) + 1L]] <- list(table = t, finding = f[i[1L], , drop = FALSE])
  }
  if (!length(hits)) {
    stop("unknown finding_id '", finding_id, "': it is not in the latest DQ run of ",
         if (is.null(table)) "any table" else paste0("table '", table, "'"), call. = FALSE)
  }
  if (length(hits) > 1L) {
    stop("finding_id '", finding_id, "' is in the latest DQ run of more than one table (",
         paste(vapply(hits, function(h) h$table, ""), collapse = ", "), "); pass table", call. = FALSE)
  }
  hits[[1L]]
}

#' Record the decision for a finding (`action.decided`) and, for an action that takes a step, add the step
#' to the recipe (sr_recipe_add) with `finding_id` set. See the header for the rules.
#' @param finding_id the id of a finding in the latest DQ run of a table.
#' @param action one of SR_ACTIONS. Differing from the suggested action makes it an override.
#' @param reason why; required (non-empty) for an override.
#' @param step optional recipe step list(op, args, reason) used instead of the one the finding implies;
#'   only for fix, drop_cases, drop_var and set_missing_impute.
#' @param table the table of the finding; only needed when the id is in the latest run of several tables.
#' @return the project with the event(s) applied.
sr_action_decide <- function(project, finding_id, action, reason = NULL, step = NULL, table = NULL) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  if (!is.character(finding_id) || length(finding_id) != 1L || is.na(finding_id) || !nzchar(finding_id)) {
    stop("finding_id must be a single string", call. = FALSE)
  }
  if (!is.character(action) || length(action) != 1L || is.na(action) || !(action %in% SR_ACTIONS)) {
    stop("action must be one of: ", paste(SR_ACTIONS, collapse = ", "), " (got: ", paste(format(action), collapse = ","), ")",
         call. = FALSE)
  }
  if (!is.null(reason) && (!is.character(reason) || length(reason) != 1L)) stop("reason must be a single string", call. = FALSE)
  reason <- if (is.null(reason) || is.na(reason)) "" else trimws(reason)
  if (!is.null(table) && (!is.character(table) || length(table) != 1L || is.na(table))) {
    stop("table must be a single string", call. = FALSE)
  }
  hit <- .sr_sg_locate(project, finding_id, table)
  table <- hit$table
  f <- hit$finding
  sug <- .sr_sg_one(project, table, f, sr_roles(project, table), sr_outcome_family_cols(project, table))
  override <- !identical(action, sug$action)
  if (override && !nzchar(reason)) {
    stop("overriding the suggested action '", sug$action, "' with '", action, "' needs a reason", call. = FALSE)
  }
  takes_step <- action %in% .sr_sg_step_actions
  if (!is.null(step) && !takes_step) {
    stop("a step goes with ", paste(.sr_sg_step_actions, collapse = ", "), ", not with '", action, "'", call. = FALSE)
  }
  .sr_sg_assert_text(project, table, reason)
  if (is.list(step)) .sr_sg_assert_text(project, table, step$reason)
  step_reason <- if (nzchar(reason)) reason else sug$reason
  step_id <- NA_character_
  undone <- NA_character_
  detail <- "no recipe step is needed"
  # the step the earlier decision for this finding on this table added, if it is still active on the
  # current branch of this table
  prev <- sr_action(project, finding_id, table)
  tsteps <- .sr_sg_table_steps(project, table)
  stale <- NULL
  if (!is.null(prev) && is.character(prev$step_id) && length(prev$step_id) == 1L && !is.na(prev$step_id) &&
        prev$step_id %in% vapply(tsteps, function(s) as.character(s$id), "")) {
    stale <- prev$step_id
  }
  adding <- FALSE
  supplied <- !is.null(step)
  if (takes_step) {
    if (supplied) {
      if (!is.list(step) || is.null(names(step))) stop("step must be list(op, args, reason)", call. = FALSE)
      step$reason <- if (is.character(step$reason) && length(step$reason) == 1L && !is.na(step$reason) &&
                         nzchar(trimws(step$reason))) step$reason else step_reason
    } else {
      step <- .sr_sg_step(project, table, f, action, step_reason, derive = TRUE)
    }
    if (is.null(step)) {
      detail <- sprintf("step pending: no recipe step expresses '%s' for a %s finding; add one with sr_recipe_add", action, f$check)
    } else {
      step$finding_id <- finding_id
      old <- Filter(function(s) identical(s$finding_id, finding_id) && identical(s$op, step$op), tsteps)
      if (!supplied && length(old)) {
        step_id <- as.character(old[[length(old)]]$id)
        detail <- sprintf("recipe step %s (%s) is already in the recipe", step_id, step$op)
      } else {
        adding <- TRUE
      }
    }
  }
  # the step an earlier decision for this finding added is out of date unless this decision reuses it
  drop <- if (!is.null(stale) && !identical(stale, step_id)) stale else NULL
  # every refusal comes before the first write: the end state (the recipe without the superseded step
  # and with the new one) must compile, so a refused decision records nothing
  if (!is.null(drop) || adding) .sr_sg_validate_end(project, table, drop, if (adding) step)
  if (!is.null(drop)) {
    project <- sr_recipe_undo(project, drop)
    undone <- drop
  }
  if (adding) {
    project <- sr_recipe_add(project, table, step)
    step_id <- as.character(sr_recipe_steps(project)[[length(sr_recipe_steps(project))]]$id)
    detail <- sprintf("recipe step %s (%s) added", step_id, step$op)
    if (step$op == "cast") {
      detail <- paste0(detail, "; TRY_CAST turns the cells that do not parse into NULL (sr_recipe_counts reports them as new_nulls)")
    }
  }
  if (!is.null(drop)) {
    detail <- sprintf("%s; the recipe step %s of the earlier decision was undone", detail, drop)
  } else if (!is.null(prev) && is.character(prev$step_id) && length(prev$step_id) == 1L && !is.na(prev$step_id) &&
              is.null(stale) && !identical(prev$step_id, step_id)) {
    br <- .sr_sg_other_branch(project, prev$step_id)
    if (!is.null(br)) {
      detail <- sprintf("%s; the recipe step %s of the earlier decision is on branch '%s', not the current branch, so it was not undone",
                        detail, prev$step_id, br)
    }
  }
  sr_project_append(project, "action.decided", list(
    finding_id = finding_id, table = table, check = f$check, column = f$column, severity = f$severity,
    action = action, suggested_action = sug$action, override = override, reason = reason,
    step_id = step_id, undone_step_id = undone, detail = detail))
}

# The name of another branch on which the step `id` is still active, or NULL.
.sr_sg_other_branch <- function(project, id) {
  rec <- .sr_rc_state(project$state)
  for (b in setdiff(names(rec$branches), rec$current)) {
    if (id %in% vapply(.sr_rc_active(rec$branches[[b]]), function(s) as.character(s$id), "")) return(b)
  }
  NULL
}

# Errors unless the end state of a decision works: the active steps of the current branch without the
# step `drop` (NULL: none) and with the new `step` (NULL: none) must compile, and the branch must take
# the table. Reads and compiles only; nothing is recorded.
.sr_sg_validate_end <- function(project, table, drop, step) {
  rec <- .sr_rc_state(project$state)
  br <- rec$branches[[rec$current]]
  act <- .sr_rc_active(br)
  ids <- vapply(act, function(s) as.character(s$id), "")
  keep <- if (is.null(drop)) act else act[ids != drop]
  if (!is.null(step) && !is.null(br$table) && !identical(br$table, table) && length(keep)) {
    stop("the current branch '", rec$current, "' is bound to table '", br$table, "', not '", table,
         "'; check out or create another branch for it", call. = FALSE)
  }
  if (!is.null(drop) && length(keep) && !identical(drop, ids[length(ids)])) {
    tryCatch(.sr_rc_compile(project, br$table, keep), error = function(e) {
      stop("cannot replace step ", drop, " of the earlier decision: the steps after it no longer work without it (",
           conditionMessage(e), ")", call. = FALSE)
    })
  }
  if (!is.null(step)) {
    st <- .sr_rc_norm_step(step, rec)
    tryCatch(.sr_rc_compile(project, table, c(keep, list(st))), error = function(e) {
      stop(if (is.null(drop)) "" else paste0("cannot replace step ", drop, " of the earlier decision: "),
           conditionMessage(e), call. = FALSE)
    })
  }
  invisible()
}

# While blind, the text of a decision (a typed reason, the reason of a supplied step) must not carry an
# outcome statistic or cross the outcome column with another column, as a generated reason never does.
# Errors with the gate message; free prose is fine, so only a statistic phrase or the words of the text
# naming the outcome column together with another column trip it (the same crossing rule as the gate).
.sr_sg_assert_text <- function(project, table, text) {
  if (is.null(text) || length(text) != 1L || is.na(text) || !nzchar(text) || !sr_blind(project)) return(invisible())
  stat <- regmatches(text, regexpr("(?i)\\b(auc|auroc|c-?statistic|c-?index|area under|(odds|hazard|risk) ratio|relative risk|p[- ]?values?|crosstab)\\b",
                                   text, perl = TRUE))
  if (length(stat)) {
    stop(sprintf("outcome-blind gate: the text mentions an outcome statistic ('%s') and may not be recorded before the SAP is locked",
                 stat), call. = FALSE)
  }
  tok <- unique(strsplit(text, "[^A-Za-z0-9_.]+")[[1L]])
  tok <- tok[nzchar(tok) & !(tolower(tok) %in% .sr_blind_names)]   # plain words such as "or" and "effect" are prose, the phrases above are the statistics
  if (!length(tok)) return(invisible())
  cols <- .sr_roles_schema(project, table)$name
  tok <- ifelse(tolower(tok) %in% tolower(cols), cols[match(tolower(tok), tolower(cols))], tok)   # column names match in any case
  oc <- sr_outcome_family_cols(project, table)
  x <- stats::setNames(as.data.frame(matrix(NA, 1L, length(tok))), tok)
  .sr_assert_blind(x, oc, setdiff(cols, oc))
  invisible()
}

# `action.decided` keeps the latest decision of each finding; the event log keeps them all.
sr_reducer_register("action.decided", function(state, event) {
  p <- event$payload
  fid <- p$finding_id
  if (!is.character(fid) || length(fid) != 1L || is.na(fid) || !nzchar(fid)) stop("action.decided needs a finding_id", call. = FALSE)
  p$step_id <- if (is.null(p$step_id) || is.na(p$step_id)) NA_character_ else as.character(p$step_id)
  p$undone_step_id <- if (is.null(p$undone_step_id) || is.na(p$undone_step_id)) NA_character_ else as.character(p$undone_step_id)
  act <- state$actions %||% list()
  act[[.sr_action_key(p$table, fid)]] <- p
  state$actions <- act
  state
})

# ---- readiness --------------------------------------------------------------------------------------

# The branch whose steps sr_recipe_apply would apply to `table`: the current branch when it is bound to it,
# else the first branch bound to it, else NULL.
.sr_sg_branch <- function(rec, table) {
  bound <- names(rec$branches)[vapply(rec$branches, function(b) identical(b$table, table), NA)]
  if (!length(bound)) return(NULL)
  if (rec$current %in% bound) rec$current else bound[1L]
}

# The "recipe applied" row: ok with no active step on the branch bound to `table`; warn when the clean
# table was never made from it, was made from another branch, or a step, undo or branch event on the branch
# is newer (event seq) than the latest `recipe.applied` into <table>_clean.
.sr_sg_recipe_row <- function(project, table) {
  rec <- .sr_rc_state(project$state)
  b <- .sr_sg_branch(rec, table)
  active <- if (is.null(b)) list() else .sr_rc_active(rec$branches[[b]])
  clean <- paste0(table, "_clean")
  if (!length(active)) return(list(status = "ok", detail = "no recipe steps"))
  ev <- sr_events_read(project$paths$events)
  seq_of <- function(e) as.numeric(e$seq)
  applied <- Filter(function(e) identical(e$type, "recipe.applied") && identical(e$payload$into, clean) &&
                      identical(e$payload$table, table), ev)
  if (!length(applied)) {
    return(list(status = "warn", detail = sprintf("%d recipe step(s) on branch '%s' have not been applied (sr_recipe_apply makes %s)",
                                                  length(active), b, clean)))
  }
  last <- applied[[length(applied)]]
  changed <- Filter(function(e) {
    (e$type %in% c("recipe.step", "recipe.undo") && identical(e$payload$branch, b)) ||
      (identical(e$type, "recipe.branch") && identical(e$payload$name, b))
  }, ev)
  newest <- if (length(changed)) max(vapply(changed, seq_of, 0)) else 0
  if (!identical(last$payload$branch, b) || newest > seq_of(last)) {
    return(list(status = "warn", detail = sprintf("the recipe of branch '%s' changed after %s was made; apply it again", b, clean)))
  }
  list(status = "ok", detail = sprintf("%d recipe step(s) applied to %s", length(active), clean))
}

# The "block / warn findings decided" row for severity `sev` from the findings `f` (NULL: no usable run).
.sr_sg_decided_row <- function(project, table, f, sev, why) {
  fail <- if (sev == "block") "block" else "warn"
  if (is.null(f)) return(list(status = fail, detail = why))
  ids <- f$id[f$severity == sev]
  decided <- vapply(ids, function(i) {
    a <- sr_action(project, i, table)
    !is.null(a) && identical(a$table, table)
  }, NA)
  un <- sum(!decided)
  list(status = if (un > 0L) fail else "ok",
       detail = sprintf("%d of %d %s finding(s) undecided", un, length(ids), sev))
}

#' What still blocks analysis on `table`, one row per item, in this order:
#' ingest complete, dictionary attached (warn if absent), roles confirmed (block if no roles.set), outcome
#' role set (block), DQ run on current store hash (block if absent, stale, or made under another outcome
#' family: see sr_dq_run), block findings decided (block
#' while any is undecided), warn findings decided (warn), recipe applied (warn while steps are newer than
#' <table>_clean), source full SHA (warn while pending; required at freeze). A finding is decided when
#' state$actions holds a decision for it on this table, whatever the action.
#' @return data.frame(item, status in ok|warn|block, detail).
sr_readiness <- function(project, table) {
  .sr_store_check_table(table)
  st <- project$state
  e <- st$tables[[table]]
  rows <- list()
  add <- function(item, status, detail) rows[[length(rows) + 1L]] <<- list(item = item, status = status, detail = detail)

  if (is.null(e) || is.null(e$store)) {
    add("ingest complete", "block", sprintf("table '%s' has not been imported", table))
  } else if (sr_blind(project) && sr_outcome_dependent(project, table)) {
    add("ingest complete", "ok", sprintf("%d columns in the store (the row count is hidden until the SAP is locked)",
                                         as.integer(e$store$ncol)))
  } else {
    add("ingest complete", "ok", sprintf("%s rows, %d columns in the store", format(e$store$rows, big.mark = ",", scientific = FALSE),
                                         as.integer(e$store$ncol)))
  }
  if (is.null(st$dict[[table]])) {
    add("dictionary attached", "warn", "no data dictionary: types, levels and ranges cannot be checked against one")
  } else {
    add("dictionary attached", "ok", "a data dictionary is attached")
  }
  roles <- st$roles[[table]]
  if (is.null(roles)) {
    add("roles confirmed", "block", "no roles have been confirmed for this table")
  } else {
    add("roles confirmed", "ok", sprintf("%d column(s) have a confirmed role", length(roles)))
  }
  oc <- unname(names(roles)[roles == "outcome"])
  if (length(oc)) add("outcome role set", "ok", "the outcome column is set") else {
    add("outcome role set", "block", "no column has the outcome role")
  }

  run <- st$dq[[table]]
  cur <- if (is.null(e)) NULL else e$store_hash
  f <- NULL
  why <- "no DQ run recorded for this table"
  if (is.null(run)) {
    add("DQ run on current store hash", "block", why)
  } else if (is.null(cur) || !identical(as.character(run$store_hash), as.character(cur))) {
    add("DQ run on current store hash", "block", "the store changed since the latest DQ run; run the checks again")
  } else if (!.sr_dq_family_current(run, tryCatch(sr_outcome_family_cols(project, table), error = function(err) NULL))) {
    add("DQ run on current store hash", "block",
        "the variable roles changed since the latest DQ run (its outcome-family columns differ); run the checks again")
  } else {
    add("DQ run on current store hash", "ok", sprintf("run %s matches the current store", run$run_id))
  }
  if (!is.null(run)) {
    f <- tryCatch(sr_dq_findings(project, table), error = function(err) {
      why <<- paste0("the DQ findings cannot be read: ", conditionMessage(err))
      NULL
    })
  }
  for (sev in c("block", "warn")) {
    r <- .sr_sg_decided_row(project, table, f, sev, why)
    add(paste(sev, "findings decided"), r$status, r$detail)
  }

  r <- .sr_sg_recipe_row(project, table)
  add("recipe applied", r$status, r$detail)

  if (.sr_is_derived(e)) {
    add("source full SHA", "ok", "a derived table has no source file")
  } else if (table %in% sr_tables_needing_hash(project)) {
    add("source full SHA", "warn", "pending: the full SHA-256 of the source is not computed yet (required before freeze)")
  } else {
    add("source full SHA", "ok", "the full SHA-256 of the source is recorded")
  }
  data.frame(item = vapply(rows, function(r) r$item, ""), status = vapply(rows, function(r) r$status, ""),
             detail = vapply(rows, function(r) r$detail, ""), stringsAsFactors = FALSE)
}
