# intake/roles.R - variable roles, rule-based role suggestions, and the outcome-blind gate.
#
# A role map is a named character vector (column -> role) per store table. It may be partial: a column
# with no entry has no confirmed role yet. `roles.set` replaces one table's whole map in
# state$roles[[table]]; other tables are untouched. The user confirms roles; sr_roles_suggest() only
# proposes (rules, no model) and never proposes `outcome` or `exposure`.
#
# Outcome-blind rule: until the SAP is locked (sr_blind()), no pre-lock output may carry a statistic that
# involves the outcome column and any other column. .sr_assert_blind() is the structural check that the
# tests of every pre-lock output run.
#
# The outcome FAMILY (SR_OUTCOME_FAMILY_ROLES) is what the gate protects: the outcome itself and every
# column that encodes it or its timing (event, event_date, censor_date, time, and offset, the follow-up
# length, which on real cohorts is nearly as informative as the outcome itself). time_zero, exposure_date,
# weight, cluster, id and covariate are not in it. The family of a table is the union over its lineage:
# its own family-role columns, the family columns of every table it was derived from (state$tables[[t]]
# $derived_from, followed recursively, matched by column name) and the columns its name map (a rename in a
# recipe, a right-side column a join renamed, a cohort feature) derives from a family column. So a role set
# on a source after a recipe, join or cohort was made from it reaches the derived table at once.
# A table is outcome-dependent (its row counts are an outcome statistic while blind) when its event set
# the persisted flag `outcome_dependent`, when its persisted recipe steps or cohort spec touch the family
# of the tables they read (re-checked against the current roles), or when any table it was derived from is.

SR_ROLES <- c("outcome", "exposure", "id", "cluster", "time_zero", "exposure_date", "event_date",
              "censor_date", "time", "event", "offset", "weight", "covariate", "ignore")
SR_PROTECTED_ROLES <- c("outcome", "exposure", "id", "cluster", "time_zero", "time", "event")
SR_OUTCOME_FAMILY_ROLES <- c("outcome", "event", "event_date", "censor_date", "time", "offset")
.SR_LINEAGE_MAX <- 20L

# Column-name patterns (matched case-insensitively on the name or the dictionary label) that suggest a role;
# sr_roles_suggest() and the DQ checks cluster_detect / offset_detect share them.
SR_RE_CLUSTER <- "site|hospital|centre|center|surgeon|cluster"
SR_RE_OFFSET <- "follow_?up|person_?(years|days|time)|exposure_time"

# ---- helpers ----------------------------------------------------------------------------------------

# The stored schema (data.frame(name, type), schema order) of a store table, or an error naming it.
.sr_roles_schema <- function(project, table) {
  .sr_store_check_table(table)
  m <- tryCatch(sr_store_manifest(project, table), error = function(e) {
    stop("table '", table, "' is not in the store (", conditionMessage(e), ")", call. = FALSE)
  })
  data.frame(name = vapply(m$schema, function(s) s$name, ""), type = vapply(m$schema, function(s) s$type, ""),
             stringsAsFactors = FALSE)
}

# The dictionary label of each column ("" when none). A dictionary that cannot be read (changed outside
# the app, missing file) is not fatal here: warn, naming the table, and use column names only.
.sr_roles_labels <- function(project, table, cols) {
  out <- rep("", length(cols))
  d <- tryCatch(sr_dict_get(project, table), error = function(e) {
    warning("the dictionary of table '", table, "' could not be read (", conditionMessage(e),
            "); suggesting roles from column names only", call. = FALSE)
    NULL
  })
  if (is.null(d) || !nrow(d)) return(out)
  i <- match(cols, d$variable)
  ok <- !is.na(i)
  lab <- as.character(d$label[i[ok]])
  lab[is.na(lab)] <- ""
  out[ok] <- lab
  out
}

# Number of distinct non-NULL values of each of `cols`, in ONE aggregate over the table's view.
.sr_roles_distinct <- function(project, table, cols) {
  if (!length(cols)) return(numeric())
  view <- sr_store_view(project, table)
  con <- sr_duck(project)
  agg <- sprintf("count(DISTINCT %s) AS %s", DBI::dbQuoteIdentifier(con, cols),
                 DBI::dbQuoteIdentifier(con, paste0("c", seq_along(cols))))
  r <- sr_duck_query(con, sprintf("SELECT %s FROM %s", paste(agg, collapse = ", "),
                                  DBI::dbQuoteIdentifier(con, view)))
  vapply(seq_along(cols), function(i) as.numeric(r[[i]][1L]), numeric(1))
}

# ---- suggestions ------------------------------------------------------------------------------------

#' Suggested roles for every column of a store table, from rules only (no outcome or exposure: the user
#' sets those). The first matching rule wins; the match is case-insensitive on the column name OR the
#' dictionary label:
#'   id        ^(patient_?)?id$ | mrn | nric | record_id | study_id
#'   cluster   site | hospital | centre | center | surgeon | cluster, with 2 to 200 distinct values
#'             (a DATE / TIMESTAMP column is never a cluster)
#'   date      _date$ | ^date_ | _dt$ or a DATE / TIMESTAMP type: event_date when death, event or
#'             outcome is in the name, otherwise covariate
#'   offset    follow-up, person-years / -days / -time, exposure_time
#'   weight    weight$ (a body weight such as weight_kg or wt_kg is not a sampling weight)
#'   covariate everything else
#' Returns one row per schema column, in schema order: data.frame(column, role, reason).
sr_roles_suggest <- function(project, table) {
  sc <- .sr_roles_schema(project, table)
  cols <- sc$name
  n <- length(cols)
  out <- data.frame(column = cols, role = rep(NA_character_, n), reason = rep(NA_character_, n),
                    stringsAsFactors = FALSE)
  if (!n) return(out)
  nm <- tolower(cols)
  lb <- tolower(.sr_roles_labels(project, table, cols))
  hit <- function(re) grepl(re, nm, perl = TRUE) | grepl(re, lb, perl = TRUE)
  via <- function(re) ifelse(grepl(re, nm, perl = TRUE), "name", "label")
  assign_rule <- function(idx, role, reason) {
    idx <- idx[is.na(out$role[idx])]
    out$role[idx] <<- role
    out$reason[idx] <<- reason[match(idx, seq_len(n))]
  }
  free <- function() is.na(out$role)

  re_id <- "^(patient_?)?id$|mrn|nric|record_id|study_id"
  i <- which(free() & hit(re_id))
  assign_rule(i, "id", paste(via(re_id), "matches id pattern"))

  re_dt <- "_date$|^date_|_dt$"
  is_dt <- hit(re_dt) | grepl("^(DATE|TIMESTAMP)", toupper(sc$type))

  # a date column is never a cluster, however its name reads
  re_cl <- SR_RE_CLUSTER
  cand <- which(free() & hit(re_cl) & !is_dt)
  if (length(cand)) {
    k <- .sr_roles_distinct(project, table, cols[cand])
    ok <- k >= 2 & k <= 200
    rs <- character(n)
    rs[cand] <- sprintf("%s matches cluster pattern; %d distinct values (2-200)", via(re_cl)[cand], as.integer(k))
    assign_rule(cand[ok], "cluster", rs)
  }

  re_ev <- "death|event|outcome"
  i <- which(free() & is_dt)
  ev <- hit(re_ev)
  assign_rule(i[ev[i]], "event_date",
              paste("date column with death, event or outcome in its", via(re_ev)))
  assign_rule(i[!ev[i]], "covariate", rep("date column (no event keyword in its name or label)", n))

  re_off <- SR_RE_OFFSET
  i <- which(free() & hit(re_off))
  assign_rule(i, "offset", paste(via(re_off), "matches follow-up or exposure-time pattern"))

  re_wt <- "weight$"
  re_body <- "body|birth"
  i <- which(free() & hit(re_wt) & !hit(re_body))
  assign_rule(i, "weight", paste(via(re_wt), "matches weight pattern"))

  i <- which(free())
  assign_rule(i, "covariate", rep("no rule matched; default covariate", n))
  out
}

# ---- setting and reading ----------------------------------------------------------------------------

#' Confirm variable roles for a store table. `roles` is a named character vector (column -> role) and
#' may be partial; it replaces the table's whole map (other tables are untouched). Errors, naming the
#' offending column or role, when the table is not in the store, a name is empty, NA, duplicated or not
#' a column of the stored schema, a value is not in SR_ROLES, or more than one column is the outcome
#' (several outcomes belong to the SAP multiplicity registry, P1d). Appends `roles.set`.
sr_roles_set <- function(project, table, roles) {
  sc <- .sr_roles_schema(project, table)
  if (!is.character(roles) || (length(roles) > 0L && is.null(names(roles)))) {
    stop("roles must be a named character vector (column -> role)", call. = FALSE)
  }
  cols <- names(roles) %||% character(0)
  vals <- unname(roles)
  if (anyNA(cols) || any(!nzchar(cols))) stop("roles has an empty or NA column name", call. = FALSE)
  if (anyDuplicated(cols)) {
    stop("roles names a column more than once: ", paste(unique(cols[duplicated(cols)]), collapse = ", "), call. = FALSE)
  }
  unknown <- setdiff(cols, sc$name)
  if (length(unknown)) {
    stop("unknown column(s) for table '", table, "': ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  na <- is.na(vals)
  if (any(na)) stop("no role given for column(s): ", paste(cols[na], collapse = ", "), call. = FALSE)
  badr <- !(vals %in% SR_ROLES)
  if (any(badr)) {
    stop("unknown role(s): ", paste(unique(vals[badr]), collapse = ", "), " (column(s): ",
         paste(cols[badr], collapse = ", "), "; allowed roles: ", paste(SR_ROLES, collapse = ", "), ")", call. = FALSE)
  }
  oc <- cols[vals == "outcome"]
  if (length(oc) > 1L) {
    stop("at most one outcome per table, but roles gives ", length(oc), ": ", paste(oc, collapse = ", "), call. = FALSE)
  }
  sr_project_append(project, "roles.set", list(table = table, columns = cols, roles = vals))
}

#' The confirmed roles of a table as a named character vector (column -> role); a zero-length named
#' vector when nothing is set.
sr_roles <- function(project, table) {
  .sr_store_check_table(table)
  r <- project$state$roles[[table]]
  if (is.null(r)) stats::setNames(character(0), character(0)) else r
}

#' The columns of `table` that have role `role` (one of SR_ROLES), in the order they were given.
sr_role_cols <- function(project, table, role) {
  if (!is.character(role) || length(role) != 1L || is.na(role) || !(role %in% SR_ROLES)) {
    stop("role must be one of: ", paste(SR_ROLES, collapse = ", "), " (got: ",
         paste(format(role), collapse = ", "), ")", call. = FALSE)
  }
  r <- sr_roles(project, table)
  unname(names(r)[r == role])
}

# `roles.set` replaces the table's whole map and nothing else.
sr_reducer_register("roles.set", function(state, event) {
  p <- event$payload
  if (!is.character(p$table) || length(p$table) != 1L || is.na(p$table)) stop("roles.set needs a table", call. = FALSE)
  cols <- as.character(unlist(p$columns, use.names = FALSE))
  vals <- as.character(unlist(p$roles, use.names = FALSE))
  if (length(cols) != length(vals)) stop("roles.set needs one role per column", call. = FALSE)
  roles <- state$roles %||% list()
  roles[[p$table]] <- stats::setNames(vals, cols)
  state$roles <- roles
  state
})

# ---- the outcome family and lineage -----------------------------------------------------------------

# The name map of a table record (list(list(from, col, to), ...): column `to` of the table is derived from
# column `col` of table `from`) as a data.frame(from, col, to).
.sr_name_map <- function(t) {
  nm <- t$name_map
  if (!length(nm)) return(data.frame(from = character(), col = character(), to = character(), stringsAsFactors = FALSE))
  g <- function(k) vapply(nm, function(e) as.character(unlist(e[[k]]))[1L], "")
  data.frame(from = g("from"), col = g("col"), to = g("to"), stringsAsFactors = FALSE)
}

# The tables `table` was derived from (itself excluded).
.sr_parents <- function(state, table) {
  setdiff(unique(as.character(unlist(state$tables[[table]]$derived_from, use.names = FALSE))), table)
}

# The outcome-family column names of `table` in `state`, over its lineage (see the header). Errors when
# the lineage is deeper than .SR_LINEAGE_MAX (a cycle).
.sr_family_names <- function(state, table, depth = 0L) {
  if (depth > .SR_LINEAGE_MAX) stop("the lineage of table '", table, "' is too deep (a cycle?)", call. = FALSE)
  r <- state$roles[[table]]
  out <- if (is.null(r)) character() else names(r)[r %in% SR_OUTCOME_FAMILY_ROLES]
  nm <- .sr_name_map(state$tables[[table]])
  for (pa in .sr_parents(state, table)) {
    pf <- .sr_family_names(state, pa, depth + 1L)
    out <- c(out, pf, nm$to[nm$from == pa & nm$col %in% pf])
  }
  unique(as.character(out))
}

#' The outcome-family columns of `table`: the columns with a role in SR_OUTCOME_FAMILY_ROLES, in the table
#' itself or (by name, or through its name map) in any table it was derived from. While blind, these are
#' hidden and never crossed with another column. Errors when the lineage cannot be followed.
sr_outcome_family_cols <- function(project, table) {
  .sr_store_check_table(table)
  .sr_family_names(project$state, table)
}

# TRUE when the persisted recipe steps of a recipe table, or the cohort spec of a cohort table, touch the
# current outcome family of the tables they read; or the table's event flagged it; or a parent is
# dependent. Fails closed (TRUE) when the lineage is too deep.
.sr_table_dependent <- function(state, table, depth = 0L) {
  if (depth > .SR_LINEAGE_MAX) return(TRUE)
  t <- state$tables[[table]]
  if (is.null(t)) return(FALSE)
  if (isTRUE(t$outcome_dependent)) return(TRUE)
  par <- .sr_parents(state, table)
  if (identical(t$format, "recipe") && length(t$recipe_steps) && length(par)) {
    fam <- tryCatch(.sr_family_names(state, par[1L]), error = function(e) NULL)
    if (is.null(fam) || any(.sr_rc_steps_touch(t$recipe_steps, fam))) return(TRUE)
  }
  if (identical(t$format, "cohort") && !is.null(state$cohorts[[table]]$spec)) {
    dep <- tryCatch(.sr_cohort_spec_dep(state, state$cohorts[[table]]$spec), error = function(e) TRUE)
    if (any(dep)) return(TRUE)
  }
  for (pa in par) if (.sr_table_dependent(state, pa, depth + 1L)) return(TRUE)
  FALSE
}

#' TRUE when the rows of `table` depend on the outcome (see the header): while blind, its row counts are
#' then an outcome statistic and are not shown.
sr_outcome_dependent <- function(project, table) {
  .sr_store_check_table(table)
  .sr_table_dependent(project$state, table)
}

# ---- the outcome-blind gate -------------------------------------------------------------------------

#' TRUE until the SAP is locked: while blind, no function may return a statistic that involves both the
#' outcome-role column and any other column. A project is locked when `state$sap$locked` is TRUE.
#' NOTE for P1d: the real `sap.locked` reducer MUST set `state$sap$locked <- TRUE`; this gate reads
#' nothing else.
sr_blind <- function(project) !isTRUE(project$state$sap$locked)

# Element names that carry an outcome statistic (whole name, case-insensitive).
.sr_blind_names <- c("auc", "or", "rr", "hr", "estimate", "effect", "crosstab", "by_outcome",
                     "smd_by_outcome")

#' Structural check that a pre-lock output carries no outcome-crossing statistic. Walks `x` through
#' lists (and data.frame columns) and errors, naming the path of the offender (e.g. "a$auc"), when
#'  - any element is named, as a whole name and case-insensitively, auc / or / rr / hr / estimate /
#'    effect / crosstab / by_outcome / smd_by_outcome ("order" and "n_or_more" are fine), or
#'  - a data.frame's column names, or a matrix / table's dimnames (names and values), contain an
#'    outcome column AND one of `other_cols`.
#' Cell values are never inspected: a per-column profile whose `column` field lists the outcome is
#' legal. Returns `x` invisibly when it passes.
#' Limits: arrays (matrix / table) are checked by dimnames only; environments, R6 objects and POSIXlt are
#' leaves and are not inspected. Names of named atomic vectors are checked too. Because names are
#' matched exactly, pre-lock outputs must be data.frames with a `column` field, NOT lists or vectors keyed
#' by column or level name: clinical names such as HR (heart rate), RR (respiratory rate) or OR
#' (operating room) would trip the gate.
.sr_assert_blind <- function(x, outcome_cols, other_cols = character()) {
  outcome_cols <- as.character(outcome_cols)
  other_cols <- as.character(other_cols)
  cross <- function(labels, where) {
    labels <- labels[!is.na(labels)]
    o <- intersect(outcome_cols, labels)
    r <- setdiff(intersect(other_cols, labels), o)
    if (length(o) && length(r)) {
      stop("outcome-blind gate: ", where, " crosses outcome column '", o[1L], "' with '", r[1L],
           "' before the SAP is locked", call. = FALSE)
    }
  }
  walk <- function(el, path) {
    where <- if (nzchar(path)) path else "<top level>"
    # leaves: environments / R6 objects, and POSIXlt (a list whose [[ and length are overridden, so
    # walking it never ends)
    if (is.environment(el) || inherits(el, "POSIXlt")) return(invisible())
    if (is.array(el)) {
      dn <- dimnames(el)
      if (!is.null(dn)) cross(c(names(dn), unlist(dn, use.names = FALSE)), where)
      return(invisible())
    }
    nms <- names(el)
    if (!is.null(nms)) {
      bad <- which(!is.na(nms) & tolower(nms) %in% .sr_blind_names)
      if (length(bad)) {
        at <- if (nzchar(path)) paste0(path, "$", nms[bad[1L]]) else nms[bad[1L]]
        stop("outcome-blind gate: ", at, " is an outcome statistic ('", nms[bad[1L]],
             "') and may not be shown before the SAP is locked", call. = FALSE)
      }
      if (is.data.frame(el)) cross(nms, where)
    }
    if (is.list(el)) {
      for (i in seq_along(el)) {
        nm <- if (!is.null(nms) && !is.na(nms[i]) && nzchar(nms[i])) nms[i] else NA_character_
        child <- if (!is.na(nm)) {
          if (nzchar(path)) paste0(path, "$", nm) else nm
        } else {
          paste0(path, "[[", i, "]]")
        }
        walk(el[[i]], child)
      }
    }
    invisible()
  }
  walk(x, "")
  invisible(x)
}
