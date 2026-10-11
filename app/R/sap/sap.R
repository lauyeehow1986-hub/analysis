# sap/sap.R - the statistical analysis plan: drafting events, the SAP hash and the lock.
#
# A SAP is a set of pre-specified analyses (primary / secondary / sensitivity), multiplicity families
# of the inferential analyses, and rules that name a pre-specified response to a diagnostic. Every edit
# is an event (sap.analysis_set, sap.analysis_removed, sap.family_set, sap.rule_set); the lock
# (sap.locked) freezes them. State:
#   state$analyses[[id]]      list(id, route, route_version, table, label, family, spec, parent, trigger,
#                                  addresses, prespecified, reason)
#   state$sap$families[[id]]  list(id, members, method)
#   state$sap$rules[[id]]     list(id, analysis, check, action, alternative)
#   state$sap$locked / $lock  TRUE / list(sap_hash, tables = list(<t> = list(store_hash)), libsnap_hash,
#                                         locked_at)
# The outcome stays blind until the lock (sr_blind reads state$sap$locked), and the lock is refused
# while a table is not ready, a kept-for-sensitivity finding has no sensitivity analysis, or an analysis
# cannot be fitted in memory. Analyses added after the lock (sr_analysis_add, later) carry
# prespecified = FALSE and are not part of the SAP hash.
#
# Reducers see the event as a re-read parses it (NA -> NULL, vectors -> lists), so each one normalises
# its payload; live folding and replay then give identical state.

#' The diagnostic ids a SAP rule can react to. A literal: this file loads before the diagnostics code.
SR_DIAG_IDS <- c("ph", "influence", "separation", "few_clusters", "inf_censoring", "linearity",
                 "heteroscedasticity")

SR_SAP_LABELS <- c("primary", "secondary", "sensitivity")
SR_SAP_METHODS <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "none")
SR_SAP_ACTIONS <- c("add_sensitivity", "replace_primary")

#' The `method` argument of sr_mem_estimate for a route id: lm* -> "lm", logistic* / modpois* -> "glm",
#' cox* / rmst* -> "cox", anything else (table1, stub routes) -> "lm". Vectorised over `route_id`.
.sr_mem_method <- function(route_id) {
  id <- as.character(route_id)
  starts <- function(p) !is.na(id) & startsWith(id, p)
  out <- rep("lm", length(id))
  out[starts("logistic") | starts("modpois")] <- "glm"
  out[starts("cox") | starts("rmst")] <- "cox"
  out[starts("lm")] <- "lm"
  out
}

# ---- normalising event payloads -------------------------------------------------------------------

# A single string, NA_character_ when absent.
.sr_sap_chr1 <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) NA_character_ else as.character(x[[1L]])
}

# A character vector (character(0) when absent).
.sr_sap_chrv <- function(x) {
  if (is.null(x)) return(character())
  out <- as.character(unlist(x, use.names = FALSE))
  out[!is.na(out)]
}

# ---- reducers --------------------------------------------------------------------------------------

sr_reducer_register("sap.analysis_set", function(state, event) {
  p <- event$payload
  id <- p$id
  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    stop("sap.analysis_set needs an id", call. = FALSE)
  }
  rec <- list(id = id, route = as.character(p$route), route_version = as.integer(p$route_version),
              table = as.character(p$table), label = as.character(p$label),
              family = .sr_sap_chr1(p$family), spec = p$spec, parent = .sr_sap_chr1(p$parent),
              trigger = NA_character_, addresses = .sr_sap_chrv(p$addresses), prespecified = TRUE,
              reason = NA_character_)
  an <- state$analyses %||% list()
  an[[id]] <- rec
  state$analyses <- an
  state
})

sr_reducer_register("sap.analysis_removed", function(state, event) {
  id <- event$payload$id
  an <- state$analyses %||% list()
  an[[id]] <- NULL
  state$analyses <- an
  fams <- state$sap$families %||% list()
  for (f in names(fams)) {
    keep <- setdiff(fams[[f]]$members, id)
    if (length(keep)) fams[[f]]$members <- keep else fams[[f]] <- NULL
  }
  state$sap$families <- fams
  rules <- state$sap$rules %||% list()
  for (r in names(rules)) {
    if (identical(rules[[r]]$analysis, id) || identical(rules[[r]]$alternative, id)) rules[[r]] <- NULL
  }
  state$sap$rules <- rules
  state
})

sr_reducer_register("sap.family_set", function(state, event) {
  p <- event$payload
  id <- p$id
  members <- .sr_sap_chrv(p$members)
  fams <- state$sap$families %||% list()
  fams[[id]] <- list(id = id, members = members, method = as.character(p$method))
  state$sap$families <- fams
  an <- state$analyses %||% list()
  for (a in names(an)) {
    if (a %in% members) {
      an[[a]]$family <- id
    } else if (identical(an[[a]]$family, id)) {
      an[[a]]$family <- NA_character_
    }
  }
  state$analyses <- an
  state
})

sr_reducer_register("sap.rule_set", function(state, event) {
  p <- event$payload
  rules <- state$sap$rules %||% list()
  rules[[p$id]] <- list(id = p$id, analysis = as.character(p$analysis), check = as.character(p$check),
                        action = as.character(p$action), alternative = as.character(p$alternative))
  state$sap$rules <- rules
  state
})

sr_reducer_register("sap.locked", function(state, event) {
  state$sap$locked <- TRUE
  state$sap$lock <- event$payload
  state
})

# ---- guards ----------------------------------------------------------------------------------------

# Drafting needs a writable project whose SAP is still open.
.sr_sap_open <- function(project, locked_msg = "the SAP is locked") {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  if (isTRUE(project$state$sap$locked)) stop(locked_msg, call. = FALSE)
  invisible(project)
}

# Single-quoted ids for messages.
.sr_sap_q <- function(x) paste0("\047", x, "\047")

.sr_sap_id <- function(x, what) {
  if (!.sr_is_text1(x)) stop(what, " must be a single non-empty string", call. = FALSE)
  invisible(x)
}

# The pre-specified analyses of a project, keyed by id.
.sr_sap_analyses <- function(project) {
  Filter(function(a) isTRUE(a$prespecified), project$state$analyses %||% list())
}

.sr_sap_get <- function(project, id, what = "analysis") {
  a <- project$state$analyses[[id]]
  if (is.null(a) || !isTRUE(a$prespecified)) stop("unknown ", what, " ", .sr_sap_q(id), call. = FALSE)
  a
}

# The id of the family that lists `id` as a member, NA when none.
.sr_sap_member_of <- function(project, id) {
  for (f in project$state$sap$families %||% list()) if (id %in% f$members) return(f$id)
  NA_character_
}

# ---- drafting --------------------------------------------------------------------------------------

#' Add (or replace, by id) a pre-specified analysis. Refused when the SAP is locked, the route is
#' unknown, the spec does not validate for the route, the layer stack is refused or has an unresolved
#' choice or requirement, the table is not in the store, or `parent` is not a pre-specified analysis.
#' @param label primary, secondary or sensitivity; only the first two can be in a multiplicity family.
#' @param family the family the analysis is meant to join (sr_sap_family makes the family itself).
#' @param parent the analysis this one is a sensitivity analysis of.
#' @param addresses ids of the DQ findings (kept for a sensitivity analysis) this analysis answers.
#' @param version route version; NULL pins the latest registered one.
#' @return the project (`sap.analysis_set`).
sr_sap_add <- function(project, id, route, spec, table, label = c("primary", "secondary", "sensitivity"),
                       family = NA, parent = NA, addresses = character(), version = NULL) {
  .sr_sap_open(project, "the SAP is locked; use sr_analysis_add")
  .sr_sap_id(id, "id")
  label <- match.arg(label)
  .sr_sap_id(route, "route")
  if (!is.list(spec)) stop("spec must be a list", call. = FALSE)
  .sr_store_check_table(table)
  if (!(table %in% sr_store_tables(project))) {
    stop("table ", .sr_sap_q(table), " is not in the project store", call. = FALSE)
  }
  r <- if (is.null(version)) sr_route_get(route) else sr_route_get(route, version)
  v <- sr_spec_validate(spec, r)
  if (!v$ok) {
    stop("the spec is not valid for route ", .sr_sap_q(route), ": ", paste(v$errors, collapse = "; "),
         call. = FALSE)
  }
  cc <- sr_compose_check(spec)
  if (!cc$ok) {
    stop("the layer combination is not allowed: ",
         paste(c(cc$refused, cc$unmet, names(cc$choices)), collapse = ", "),
         " (", paste(cc$notes, collapse = "; "), ")", call. = FALSE)
  }
  family <- if (length(family) == 1L && is.na(family)) NA_character_ else {
    .sr_sap_id(family, "family")
  }
  parent <- if (length(parent) == 1L && is.na(parent)) NA_character_ else .sr_sap_id(parent, "parent")
  if (!is.na(parent)) {
    if (identical(parent, id)) stop("an analysis cannot be the parent of itself", call. = FALSE)
    .sr_sap_get(project, parent, "parent analysis")
  }
  if (!is.character(addresses) || anyNA(addresses) || any(!nzchar(addresses))) {
    stop("addresses must be a character vector of finding ids", call. = FALSE)
  }
  addresses <- unique(addresses)
  if (length(addresses) && label != "sensitivity") {
    stop("only a sensitivity analysis can address a finding", call. = FALSE)
  }
  cur <- .sr_sap_member_of(project, id)
  if (!is.na(cur)) {
    if (label == "sensitivity") {
      stop("analysis ", .sr_sap_q(id), " is in family ", .sr_sap_q(cur),
           "; sensitivity analyses are not an inferential family", call. = FALSE)
    }
    if (!is.na(family) && !identical(family, cur)) {
      stop("analysis ", .sr_sap_q(id), " is already in family ", .sr_sap_q(cur),
           "; change it with sr_sap_family", call. = FALSE)
    }
    family <- cur
  }
  if (!is.na(family) && label == "sensitivity") {
    stop("sensitivity analyses are not an inferential family", call. = FALSE)
  }
  sr_project_append(project, "sap.analysis_set", list(
    id = id, route = route, route_version = as.integer(r$version), spec = spec, table = table, label = label,
    family = family, parent = parent, addresses = addresses))
}

#' Remove a pre-specified analysis before the lock. Refused while another analysis names it as its
#' parent. It also leaves its family and drops the rules that name it.
#' @return the project (`sap.analysis_removed`).
sr_sap_remove <- function(project, id) {
  .sr_sap_open(project)
  .sr_sap_id(id, "id")
  .sr_sap_get(project, id)
  kids <- names(Filter(function(a) identical(a$parent, id), .sr_sap_analyses(project)))
  if (length(kids)) {
    stop("cannot remove ", .sr_sap_q(id), ": ", paste(.sr_sap_q(kids), collapse = ", "),
         " name it as the parent; remove those first", call. = FALSE)
  }
  sr_project_append(project, "sap.analysis_removed", list(id = id))
}

#' Set (or replace, by id) a multiplicity family: the members share one adjustment `method`. Members
#' must be pre-specified primary or secondary analyses (a sensitivity analysis is not an inferential
#' family, spec M1.4), and an analysis is in at most one family.
#' @return the project (`sap.family_set`).
sr_sap_family <- function(project, id, members,
                          method = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "none")) {
  .sr_sap_open(project)
  .sr_sap_id(id, "family id")
  method <- match.arg(method)
  if (!is.character(members) || !length(members) || anyNA(members) || any(!nzchar(members))) {
    stop("members must be a non-empty character vector of analysis ids", call. = FALSE)
  }
  if (anyDuplicated(members)) {
    stop("members names an analysis twice: ", paste(unique(members[duplicated(members)]), collapse = ", "),
         call. = FALSE)
  }
  for (m in members) {
    a <- .sr_sap_get(project, m)
    if (identical(a$label, "sensitivity")) {
      stop(.sr_sap_q(m), " is a sensitivity analysis; sensitivity analyses are not an inferential family",
           call. = FALSE)
    }
    other <- .sr_sap_member_of(project, m)
    if (!is.na(other) && !identical(other, id)) {
      stop("analysis ", .sr_sap_q(m), " is already in family ", .sr_sap_q(other),
           "; an analysis is in at most one family", call. = FALSE)
    }
  }
  sr_project_append(project, "sap.family_set", list(id = id, members = members, method = method))
}

#' Pre-specify the response to a diagnostic: when `check` fails for `analysis`, add the pre-specified
#' sensitivity analysis `alternative` to the report (add_sensitivity) or make it the primary result
#' (replace_primary). `alternative` must be a sensitivity analysis whose parent is `analysis`.
#' @return the project (`sap.rule_set`).
sr_sap_rule <- function(project, id, analysis, check, action = c("add_sensitivity", "replace_primary"),
                        alternative) {
  .sr_sap_open(project)
  .sr_sap_id(id, "rule id")
  .sr_sap_id(analysis, "analysis")
  if (!is.character(check) || length(check) != 1L || is.na(check) || !(check %in% SR_DIAG_IDS)) {
    stop("check must be one of: ", paste(SR_DIAG_IDS, collapse = ", "), call. = FALSE)
  }
  action <- match.arg(action)
  if (missing(alternative) || is.null(alternative)) {
    stop("alternative (a pre-specified sensitivity analysis) is required", call. = FALSE)
  }
  .sr_sap_id(alternative, "alternative")
  .sr_sap_get(project, analysis)
  alt <- .sr_sap_get(project, alternative, "alternative analysis")
  if (!identical(alt$label, "sensitivity")) {
    stop("alternative ", .sr_sap_q(alternative), " must be a pre-specified sensitivity analysis", call. = FALSE)
  }
  if (!identical(alt$parent, analysis)) {
    stop("alternative ", .sr_sap_q(alternative), " must have ", .sr_sap_q(analysis),
         " as its parent (its parent is ", if (is.na(alt$parent)) "unset" else .sr_sap_q(alt$parent), ")",
         call. = FALSE)
  }
  sr_project_append(project, "sap.rule_set", list(id = id, analysis = analysis, check = check,
                                                   action = action, alternative = alternative))
}

#' SHA-256 of the canonical JSON of the pre-specified analyses, the families and the rules, each
#' ordered by id: independent adds in any order give the same hash. Analyses added after the lock
#' (prespecified = FALSE) and the lock itself are not part of it.
sr_sap_hash <- function(project) {
  by_id <- function(x) if (length(x)) unname(x[order(names(x), method = "radix")]) else list()
  .sr_sha256(sr_canonical_json(list(
    analyses = by_id(.sr_sap_analyses(project)),
    families = by_id(project$state$sap$families %||% list()),
    rules = by_id(project$state$sap$rules %||% list()))))
}

# ---- the check and the lock ------------------------------------------------------------------------

# The finding ids on `table` that were decided keep_sensitivity: decisions whose finding is still in the
# latest DQ run (every decision when the findings cannot be read; readiness reports that).
.sr_sap_kept <- function(project, table) {
  acts <- Filter(function(a) identical(a$table, table) && identical(a$action, "keep_sensitivity"),
                 project$state$actions %||% list())
  ids <- unique(vapply(acts, function(a) as.character(a$finding_id), ""))
  cur <- tryCatch(sr_dq_findings(project, table)$id, error = function(e) NULL)
  if (is.null(cur)) ids else ids[ids %in% cur]
}

#' What the lock would refuse, one row per item: data.frame(item, status in ok|warn|block, detail).
#' block: no primary analysis; a block row of sr_readiness for an analysed table (detail prefixed with
#' the table); a finding kept for a sensitivity analysis that no sensitivity analysis addresses; an
#' analysis that does not fit in memory (or whose columns cannot be sized); a rule or family that no
#' longer fits the analyses. warn: a readiness warn row; an analysis naming a family it is not in.
sr_sap_check <- function(project) {
  an <- .sr_sap_analyses(project)
  if (length(an)) an <- an[order(names(an), method = "radix")]
  rows <- list()
  add <- function(item, status, detail) rows[[length(rows) + 1L]] <<- list(item = item, status = status, detail = detail)

  has_primary <- any(vapply(an, function(a) identical(a$label, "primary"), NA))
  add("primary analysis", if (has_primary) "ok" else "block",
      if (has_primary) "the SAP has a primary analysis" else "the SAP has no primary analysis")

  tabs <- sort(unique(vapply(an, function(a) as.character(a$table), "")), method = "radix")
  for (t in tabs) {
    rd <- tryCatch(sr_readiness(project, t), error = function(e) {
      data.frame(item = "readiness", status = "block", detail = conditionMessage(e), stringsAsFactors = FALSE)
    })
    for (i in which(rd$status != "ok")) add(rd$item[i], rd$status[i], paste0(t, ": ", rd$detail[i]))
  }

  addressed <- function(t) {
    unique(unlist(lapply(an, function(a) {
      if (identical(a$label, "sensitivity") && identical(a$table, t)) a$addresses else NULL
    }), use.names = FALSE))
  }
  for (t in tabs) {
    for (fid in setdiff(.sr_sap_kept(project, t), addressed(t))) {
      add("kept finding addressed", "block",
          sprintf("%s: finding %s was kept for a sensitivity analysis, but none addresses it", t, fid))
    }
  }

  big <- FALSE
  for (id in names(an)) {
    a <- an[[id]]
    cols <- sr_spec_cols(a$spec)
    if (!length(cols)) next
    est <- tryCatch(sr_mem_estimate(project, a$table, cols, .sr_mem_method(a$route)), error = function(e) e)
    if (inherits(est, "error")) {
      big <- TRUE
      add("fits in memory", "block", sprintf("%s: cannot size the analysis: %s", id, conditionMessage(est)))
    } else if (!isTRUE(est$fits)) {
      big <- TRUE
      add("fits in memory", "block",
          sprintf("%s: too large for an in-memory fit; big-data engines arrive in P1e (%s)", id, est$reason))
    }
  }
  if (!big) add("fits in memory", "ok", "every analysis fits in memory")

  for (r in project$state$sap$rules %||% list()) {
    alt <- an[[r$alternative]]
    if (is.null(an[[r$analysis]]) || is.null(alt) || !identical(alt$label, "sensitivity") ||
          !identical(alt$parent, r$analysis)) {
      add("rules fit the analyses", "block",
          sprintf("rule %s: %s is not a pre-specified sensitivity analysis of %s", r$id,
                  .sr_sap_q(r$alternative), .sr_sap_q(r$analysis)))
    }
  }
  for (f in project$state$sap$families %||% list()) {
    for (m in f$members) {
      if (is.null(an[[m]]) || identical(an[[m]]$label, "sensitivity")) {
        add("families fit the analyses", "block",
            sprintf("family %s: %s is not a pre-specified primary or secondary analysis", f$id, .sr_sap_q(m)))
      }
    }
  }
  for (id in names(an)) {
    fam <- an[[id]]$family
    if (!is.na(fam) && !(id %in% (project$state$sap$families[[fam]]$members %||% character()))) {
      add("family membership", "warn",
          sprintf("%s names family %s but is not a member of one with that id", id, .sr_sap_q(fam)))
    }
  }
  data.frame(item = vapply(rows, function(r) r$item, ""), status = vapply(rows, function(r) r$status, ""),
             detail = vapply(rows, function(r) r$detail, ""), stringsAsFactors = FALSE)
}

#' Lock the SAP (`sap.locked`): refused, listing every block row of sr_sap_check, while anything blocks.
#' The event records the SAP hash, the store hash of every analysed table, the library snapshot hash and
#' the time; once it is applied sr_blind() is FALSE and the SAP can no longer be edited.
#' @return the project.
sr_sap_lock <- function(project) {
  .sr_sap_open(project, "the SAP is already locked")
  chk <- sr_sap_check(project)
  blk <- chk[chk$status == "block", , drop = FALSE]
  if (nrow(blk)) {
    stop("the SAP cannot be locked yet:\n", paste0("  - ", blk$item, ": ", blk$detail, collapse = "\n"),
         call. = FALSE)
  }
  an <- .sr_sap_analyses(project)
  tabs <- sort(unique(vapply(an, function(a) as.character(a$table), "")), method = "radix")
  tables <- list()
  for (t in tabs) tables[[t]] <- list(store_hash = sr_store_hash(sr_store_manifest(project, t)))
  sr_project_append(project, "sap.locked", list(
    sap_hash = sr_sap_hash(project), tables = tables, libsnap_hash = sr_libsnap_hash(),
    locked_at = .sr_now()))
}
