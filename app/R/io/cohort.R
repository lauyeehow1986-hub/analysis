# io/cohort.R - the cohort builder: long EHR-style tables -> one row per patient, with a participant
# flow, features and a time-to-event outcome, all as DuckDB SQL over the store views.
#
#   sr_cohort_spec(base, id, index, criteria, features, outcome, name)   the specification (a list)
#   sr_cohort_sql(project, spec)     list(steps, extra, final): the SQL; nothing is run or written
#   sr_cohort_build(project, spec)   materialise store table `name`, append `cohort.built`
#
# Rules (the spec is data, the SQL is derived from it and recorded in the event):
#  - Every table is joined on the SAME id column name, spec$id. The cohort is the distinct non-NULL ids
#    of the base table that have an index date and pass every criterion, in order.
#  - Dates: a DATE or TIMESTAMP column, cut to the day. A window c(lo, hi) is in inclusive whole days,
#    date_diff('day', index_date, row_date) in [lo, hi], so a row on the index day has day 0. A row whose
#    date is NULL is outside every window. A window NULL means "any time" (the date column is then not
#    needed, except for first / last).
#  - Index: the first (pick = "first", the earliest) or last date per id over the rows of index$table
#    that match index$where.
#  - Criterion: "include" keeps the ids with at least one matching row (the where, inside the window);
#    "exclude" drops them. A row is matching when its where is TRUE (NULL is not a match).
#  - Feature (one column per feature): the rows of `table` matching `where` inside `window`, with a
#    NON-NULL `col` (every aggregate ignores a NULL col), aggregated per id:
#      first / last       the value on the earliest / latest date (needs date_col; a row with a NULL date
#                         is not a candidate). Rows on the same date are broken by the value: first takes
#                         the smallest, last the greatest.
#      count              the number of rows;  any  TRUE when there is at least one row
#      min / max / mean   over the values (mean needs a numeric col)
#    An empty window gives NULL, except count (0) and any (FALSE).
#  - Outcome (time to first event): the first event is the earliest event_date_col of the rows of
#    event_table matching event_where with 0 <= days <= horizon_days from the index date. Events before
#    the index, or after the horizon, are ignored. With such an event: event = 1, time = days. Without:
#    event = 0, time = min(censor days, horizon). The censor date is the latest censor_date_col per id
#    in censor_table when it is given, else in the base table when only censor_date_col is given, else
#    there is none and time = horizon; an id with no censor date is censored at the horizon. A censor
#    date before the index gives a negative time (it is not clamped); the number of such ids is recorded
#    as n_negative_time in the payload and the state, and sr_cohort_build() warns.
#  - Output columns: <id>, index_date, the features (in order), then time and event. One row per id,
#    ORDER BY id.
#
# Outcome-blind: a criterion (or the index filter) whose where uses an outcome-family column of its table
# (sr_outcome_family_cols: outcome, event, event_date, censor_date, time, offset, by lineage), or that
# reads the outcome event table or the censor table (a count of "died in the first 30 days" is an outcome
# count whatever the roles), is flagged outcome_dependent in the flow, and sr_flow() hides the counts from
# it on until the SAP is locked. The payload keeps the true counts. `cohort.built` also persists
# outcome_dependent (any step is) on the table, so its rows stay hidden, and a name map (each feature is
# derived from its col and the columns of its where), so a feature made from a family column is a family
# column of the cohort. The new table gets the roles id (spec$id), outcome (event) and time (time), so the
# outcome-blind guard applies to it.
#
# The CTEs and aliases of the SQL start with sr_x_; a table with such a name cannot be used.

SR_COHORT_AGGS <- c("first", "last", "count", "min", "max", "mean", "any")
SR_COHORT_PICKS <- c("first", "last")
SR_COHORT_TYPES <- c("include", "exclude")
.sr_cohort_reserved <- c("index_date", "time", "event")
.sr_cohort_name_re <- "^[A-Za-z_][A-Za-z0-9_]{0,62}$"

# ---- the specification ------------------------------------------------------------------------------

.sr_ch_str <- function(x, what, null_ok = FALSE) {
  if (is.null(x) && null_ok) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(what, " must be a single non-empty string", call. = FALSE)
  }
  x
}

# A window: NULL (any time) or c(lo, hi) of whole days with lo <= hi.
.sr_ch_window <- function(w, what) {
  if (is.null(w)) return(NULL)
  w <- suppressWarnings(as.numeric(unlist(w, use.names = FALSE)))
  if (length(w) != 2L || anyNA(w) || any(!is.finite(w)) || any(w != floor(w))) {
    stop(what, " window must be c(lo_days, hi_days), two whole numbers", call. = FALSE)
  }
  if (w[1L] > w[2L]) {
    stop(what, " window is the wrong way round: lo (", w[1L], ") is after hi (", w[2L], ")", call. = FALSE)
  }
  w
}

.sr_ch_list <- function(x, what) {
  if (!is.list(x) || is.null(names(x))) stop(what, " must be a named list", call. = FALSE)
  x
}

# Check the shape of a spec and fill in the defaults; the result is what is recorded in the event.
# Idempotent. Table and column names are checked against the store later (.sr_cohort_check).
.sr_cohort_norm <- function(spec) {
  spec <- .sr_ch_list(spec, "the cohort spec")
  base <- .sr_ch_str(spec$base, "base")
  id <- .sr_ch_str(spec$id, "id")
  name <- .sr_ch_str(spec$name, "name")
  .sr_store_check_table(name)
  if (tolower(id) %in% .sr_cohort_reserved) {
    stop("id may not be named ", paste(.sr_cohort_reserved, collapse = ", "), call. = FALSE)
  }

  ix <- .sr_ch_list(spec$index, "index")
  pick <- ix$pick %||% "first"
  if (!is.character(pick) || length(pick) != 1L || is.na(pick) || !(pick %in% SR_COHORT_PICKS)) {
    stop("index pick must be one of: ", paste(SR_COHORT_PICKS, collapse = ", "), call. = FALSE)
  }
  index <- list(table = .sr_ch_str(ix$table, "index table"), date_col = .sr_ch_str(ix$date_col, "index date_col"),
                where = NULL, pick = pick)
  index["where"] <- list(ix$where)

  crit <- lapply(seq_along(spec$criteria), function(i) {
    what <- paste0("criterion ", i)
    cr <- .sr_ch_list(spec$criteria[[i]], what)
    type <- cr$type %||% "include"
    if (!is.character(type) || length(type) != 1L || is.na(type) || !(type %in% SR_COHORT_TYPES)) {
      stop(what, " type must be one of: ", paste(SR_COHORT_TYPES, collapse = ", "), call. = FALSE)
    }
    out <- list(label = .sr_ch_str(cr$label, paste(what, "label")), type = type,
                table = .sr_ch_str(cr$table, paste(what, "table")), where = NULL, window = NULL, date_col = NULL)
    out["where"] <- list(cr$where)
    out["window"] <- list(.sr_ch_window(cr$window, what))
    out["date_col"] <- list(.sr_ch_str(cr$date_col, paste(what, "date_col"), null_ok = TRUE))
    if (!is.null(out$window) && is.null(out$date_col)) stop(what, " has a window, so it needs a date_col", call. = FALSE)
    out
  })

  feat <- lapply(seq_along(spec$features), function(i) {
    what <- paste0("feature ", i)
    f <- .sr_ch_list(spec$features[[i]], what)
    fname <- .sr_ch_str(f$name, paste(what, "name"))
    if (!grepl(.sr_cohort_name_re, fname)) {
      stop(what, " name '", fname, "' is not a valid column name (letters, digits and _, not starting with a digit)",
           call. = FALSE)
    }
    agg <- f$agg
    if (!is.character(agg) || length(agg) != 1L || is.na(agg) || !(agg %in% SR_COHORT_AGGS)) {
      stop(what, " agg must be one of: ", paste(SR_COHORT_AGGS, collapse = ", "), call. = FALSE)
    }
    out <- list(name = fname, table = .sr_ch_str(f$table, paste(what, "table")), col = .sr_ch_str(f$col, paste(what, "col")),
                agg = agg, window = NULL, where = NULL, date_col = NULL)
    out["window"] <- list(.sr_ch_window(f$window, what))
    out["where"] <- list(f$where)
    out["date_col"] <- list(.sr_ch_str(f$date_col, paste(what, "date_col"), null_ok = TRUE))
    if ((!is.null(out$window) || agg %in% c("first", "last")) && is.null(out$date_col)) {
      stop(what, " (", agg, if (is.null(out$window)) "" else " in a window", ") needs a date_col", call. = FALSE)
    }
    out
  })
  fn <- vapply(feat, function(f) f$name, "")
  if (anyDuplicated(tolower(fn))) stop("feature names must be unique (case-insensitively)", call. = FALSE)
  clash <- fn[tolower(fn) %in% tolower(c(id, .sr_cohort_reserved))]
  if (length(clash)) stop("feature name(s) clash with an output column: ", paste(clash, collapse = ", "), call. = FALSE)

  outcome <- NULL
  if (!is.null(spec$outcome)) {
    o <- .sr_ch_list(spec$outcome, "outcome")
    h <- o$horizon_days
    if (!is.numeric(h) || length(h) != 1L || !is.finite(h) || h != floor(h) || h < 0) {
      stop("outcome horizon_days must be a single whole number of days, 0 or more", call. = FALSE)
    }
    outcome <- list(event_table = .sr_ch_str(o$event_table, "outcome event_table"),
                    event_date_col = .sr_ch_str(o$event_date_col, "outcome event_date_col"),
                    event_where = NULL, censor_date_col = NULL, censor_table = NULL, horizon_days = h)
    outcome["event_where"] <- list(o$event_where)
    outcome["censor_date_col"] <- list(.sr_ch_str(o$censor_date_col, "outcome censor_date_col", null_ok = TRUE))
    outcome["censor_table"] <- list(.sr_ch_str(o$censor_table, "outcome censor_table", null_ok = TRUE))
    if (!is.null(outcome$censor_table) && is.null(outcome$censor_date_col)) {
      stop("outcome censor_table needs censor_date_col (the column of that table)", call. = FALSE)
    }
  }
  res <- list(base = base, id = id, index = index, criteria = crit, features = feat, outcome = NULL, name = name)
  res["outcome"] <- list(outcome)
  res
}

#' The specification of a cohort.
#'
#' @param base the table whose distinct ids are the starting population.
#' @param id the id column (the same name in every table used).
#' @param index list(table, date_col, where = NULL, pick = "first" | "last"): the index date per id.
#' @param criteria a list of list(label, type = "include" | "exclude", table, where, window = c(lo, hi) or
#'   NULL, date_col): an include keeps the ids with at least one matching row in the window, an exclude
#'   drops them. `date_col` (the date column of `table`) is needed when there is a window.
#' @param features a list of list(name, table, col, agg, window, where, date_col), agg one of
#'   first, last, count, min, max, mean, any. See the header of io/cohort.R for the exact rules: every
#'   aggregate ignores NULL values of `col`; a first / last tie on the same date goes to the smaller /
#'   greater value; an empty window gives NULL, count 0, any FALSE.
#' @param outcome NULL, or list(event_table, event_date_col, event_where, censor_date_col, censor_table,
#'   horizon_days): the first event with 0 <= days <= horizon gives event 1 and time = days, otherwise
#'   event 0 and time = min(censor days, horizon); events before the index are ignored. The censor date is
#'   the latest censor_date_col per id in censor_table, else in the base table, else the horizon.
#' @param name the new store table.
#' @return the normalised spec (a list); the shape is checked here, the tables and columns when the SQL is made.
sr_cohort_spec <- function(base, id, index, criteria = list(), features = list(), outcome = NULL, name) {
  if (missing(base) || missing(id) || missing(index) || missing(name)) {
    stop("base, id, index and name are required", call. = FALSE)
  }
  s <- list(base = base, id = id, index = index, criteria = criteria, features = features, name = name)
  s["outcome"] <- list(outcome)
  .sr_cohort_norm(s)
}

# ---- checks against the store ----------------------------------------------------------------------

# The schemas (named chr: column -> type) of the tables a spec uses.
.sr_cohort_schemas <- function(project, tables) {
  out <- list()
  for (t in unique(tables)) {
    if (startsWith(t, "sr_x_")) {
      stop("table names starting with 'sr_x_' are reserved by the cohort builder: '", t, "'", call. = FALSE)
    }
    if (!(t %in% sr_store_tables(project))) stop("table '", t, "' is not in the store", call. = FALSE)
    m <- sr_store_manifest(project, t)
    out[[t]] <- stats::setNames(vapply(m$schema, function(s) s$type, ""), vapply(m$schema, function(s) s$name, ""))
  }
  out
}

# Every table and column the spec names exists (also those inside a where), the id columns have one
# type family, date columns are dates, mean is over numbers. Pure R over the manifests: no SQL runs.
# Returns the schemas.
.sr_cohort_check <- function(project, spec) {
  tabs <- c(spec$base, spec$index$table, vapply(spec$criteria, function(c) c$table, ""),
            vapply(spec$features, function(f) f$table, ""))
  o <- spec$outcome
  if (!is.null(o)) tabs <- c(tabs, o$event_table, o$censor_table)
  sch <- .sr_cohort_schemas(project, tabs)
  what_col <- function(tbl, col, why) {
    if (!(col %in% names(sch[[tbl]]))) {
      stop(why, ": '", col, "' is not a column of table '", tbl, "'", call. = FALSE)
    }
    invisible(sch[[tbl]][[col]])
  }
  is_date <- function(tbl, col, why) {
    ty <- what_col(tbl, col, why)
    if (!grepl("^(DATE|TIMESTAMP)", toupper(ty))) {
      stop(why, ": column '", col, "' of table '", tbl, "' is ", ty, ", not a date", call. = FALSE)
    }
  }
  where_cols <- function(tbl, w, why) for (cc in unique(.sr_rc_where_cols(w))) what_col(tbl, cc, paste(why, "(filter)"))

  fam <- NULL
  for (t in unique(tabs)) {
    f <- .sr_join_family(what_col(t, spec$id, "id column"))
    if (is.null(fam)) {
      fam <- f
    } else if (!identical(fam, f)) {
      stop("the id column '", spec$id, "' has different types in the tables (", fam, " and ", f, " in '", t, "')",
           call. = FALSE)
    }
  }
  is_date(spec$index$table, spec$index$date_col, "index")
  where_cols(spec$index$table, spec$index$where, "index")
  for (i in seq_along(spec$criteria)) {
    cr <- spec$criteria[[i]]
    why <- paste0("criterion ", i, " ('", cr$label, "')")
    where_cols(cr$table, cr$where, why)
    if (!is.null(cr$date_col)) is_date(cr$table, cr$date_col, why)
  }
  for (f in spec$features) {
    why <- paste0("feature '", f$name, "'")
    ty <- what_col(f$table, f$col, why)
    where_cols(f$table, f$where, why)
    if (!is.null(f$date_col)) is_date(f$table, f$date_col, why)
    if (identical(f$agg, "mean") && !identical(.sr_join_family(ty), "NUMBER")) {
      stop(why, ": mean needs a numeric column, but '", f$col, "' is ", ty, call. = FALSE)
    }
  }
  if (!is.null(o)) {
    is_date(o$event_table, o$event_date_col, "outcome event")
    where_cols(o$event_table, o$event_where, "outcome event")
    if (!is.null(o$censor_date_col)) is_date(o$censor_table %||% spec$base, o$censor_date_col, "outcome censor")
  }
  sch
}

# ---- the SQL ----------------------------------------------------------------------------------------

# The CTEs as one WITH clause.
.sr_cohort_with <- function(ctes) paste0("WITH ", paste(sprintf("%s AS (%s)", names(ctes), ctes), collapse = ", "))

# Compile a checked spec. Returns list(steps = named chr (the flow CTEs: base, index, one per
# criterion), extra = named chr (the feature and outcome CTEs), final = the full SELECT, last = the name
# of the CTE with the final ids, cols = the output columns).
.sr_cohort_compile <- function(con, spec, sch) {
  Q <- function(x) .sr_join_q(con, x)
  id <- Q(spec$id)
  idx <- Q("index_date")
  # the rows of a table that match a where (the table itself without one)
  src <- function(tbl, where) {
    if (is.null(where)) return(Q(tbl))
    sprintf("(SELECT * FROM %s WHERE %s)", Q(tbl), sr_where_sql(con, where, names(sch[[tbl]]), sch[[tbl]]))
  }
  # the window predicate on the rows of sr_x_t against the index date of sr_x_c
  win <- function(w, date_col) {
    if (is.null(w)) return(NULL)
    sprintf("date_diff('day', sr_x_c.%s, CAST(sr_x_t.%s AS DATE)) BETWEEN %.0f AND %.0f", idx, Q(date_col), w[1L], w[2L])
  }
  all_of <- function(...) {
    p <- c(...)
    if (!length(p)) "TRUE" else paste(p, collapse = " AND ")
  }

  steps <- character()
  steps[["sr_x_base"]] <- sprintf("SELECT DISTINCT %s FROM %s WHERE %s IS NOT NULL", id, Q(spec$base), id)
  ix <- spec$index
  steps[["sr_x_index"]] <- sprintf(
    paste0("SELECT sr_x_b.%s, sr_x_i.sr_x_d AS %s FROM sr_x_base AS sr_x_b JOIN (SELECT %s, %s(CAST(%s AS DATE)) AS sr_x_d ",
           "FROM %s WHERE %s IS NOT NULL AND %s IS NOT NULL GROUP BY %s) AS sr_x_i ON sr_x_b.%s = sr_x_i.%s"),
    id, idx, id, if (identical(ix$pick, "first")) "min" else "max", Q(ix$date_col),
    src(ix$table, ix$where), id, Q(ix$date_col), id, id, id)
  prev <- "sr_x_index"
  for (i in seq_along(spec$criteria)) {
    cr <- spec$criteria[[i]]
    nm <- paste0("sr_x_c", i)
    cond <- all_of(sprintf("sr_x_t.%s = sr_x_c.%s", id, id), win(cr$window, cr$date_col))
    steps[[nm]] <- sprintf("SELECT sr_x_c.* FROM %s AS sr_x_c WHERE %sEXISTS (SELECT 1 FROM %s AS sr_x_t WHERE %s)",
                           prev, if (identical(cr$type, "exclude")) "NOT " else "", src(cr$table, cr$where), cond)
    prev <- nm
  }

  extra <- character()
  sel <- c(sprintf("sr_x_c.%s", id), sprintf("sr_x_c.%s", idx))
  joins <- character()
  for (i in seq_along(spec$features)) {
    f <- spec$features[[i]]
    nm <- paste0("sr_x_f", i)
    col <- sprintf("sr_x_t.%s", Q(f$col))
    dt <- if (is.null(f$date_col)) NULL else sprintf("CAST(sr_x_t.%s AS DATE)", Q(f$date_col))
    expr <- switch(f$agg,
      first = sprintf("arg_min(%s, struct_pack(d := %s, v := %s))", col, dt, col),
      last = sprintf("arg_max(%s, struct_pack(d := %s, v := %s))", col, dt, col),
      count = "count(*)", any = "count(*) > 0",
      min = sprintf("min(%s)", col), max = sprintf("max(%s)", col), mean = sprintf("avg(%s)", col))
    # first / last rank by date: a row with no date is not a candidate (arg_max would rank NULL greatest)
    undated <- if (f$agg %in% c("first", "last")) sprintf("sr_x_t.%s IS NOT NULL", Q(f$date_col)) else NULL
    cond <- all_of(sprintf("sr_x_t.%s = sr_x_c.%s", id, id), sprintf("%s IS NOT NULL", col), undated, win(f$window, f$date_col))
    extra[[nm]] <- sprintf("SELECT sr_x_c.%s, %s AS v FROM %s AS sr_x_c JOIN %s AS sr_x_t ON %s GROUP BY sr_x_c.%s",
                           id, expr, prev, src(f$table, f$where), cond, id)
    joins <- c(joins, sprintf("LEFT JOIN %s ON %s.%s = sr_x_c.%s", nm, nm, id, id))
    v <- switch(f$agg, count = sprintf("coalesce(%s.v, 0)", nm), any = sprintf("coalesce(%s.v, FALSE)", nm),
                sprintf("%s.v", nm))
    sel <- c(sel, sprintf("%s AS %s", v, Q(f$name)))
  }
  o <- spec$outcome
  if (!is.null(o)) {
    h <- sprintf("%.0f", o$horizon_days)
    evday <- sprintf("date_diff('day', sr_x_c.%s, CAST(sr_x_t.%s AS DATE))", idx, Q(o$event_date_col))
    extra[["sr_x_ev"]] <- sprintf(
      paste0("SELECT sr_x_c.%s, min(%s) AS d FROM %s AS sr_x_c JOIN %s AS sr_x_t ON sr_x_t.%s = sr_x_c.%s ",
             "WHERE %s BETWEEN 0 AND %s GROUP BY sr_x_c.%s"),
      id, evday, prev, src(o$event_table, o$event_where), id, id, evday, h, id)
    joins <- c(joins, sprintf("LEFT JOIN sr_x_ev ON sr_x_ev.%s = sr_x_c.%s", id, id))
    cens <- "CAST(NULL AS BIGINT)"
    if (!is.null(o$censor_date_col)) {
      extra[["sr_x_cens"]] <- sprintf(
        paste0("SELECT sr_x_c.%s, max(CAST(sr_x_t.%s AS DATE)) AS d FROM %s AS sr_x_c JOIN %s AS sr_x_t ",
               "ON sr_x_t.%s = sr_x_c.%s GROUP BY sr_x_c.%s"),
        id, Q(o$censor_date_col), prev, Q(o$censor_table %||% spec$base), id, id, id)
      joins <- c(joins, sprintf("LEFT JOIN sr_x_cens ON sr_x_cens.%s = sr_x_c.%s", id, id))
      cens <- sprintf("date_diff('day', sr_x_c.%s, sr_x_cens.d)", idx)
    }
    sel <- c(sel,
      sprintf("CAST(CASE WHEN sr_x_ev.d IS NOT NULL THEN sr_x_ev.d WHEN %s IS NULL THEN %s ELSE least(%s, %s) END AS BIGINT) AS %s",
              cens, h, cens, h, Q("time")),
      sprintf("CAST(CASE WHEN sr_x_ev.d IS NOT NULL THEN 1 ELSE 0 END AS INTEGER) AS %s", Q("event")))
  }
  final <- paste0(.sr_cohort_with(c(steps, extra)), " SELECT ", paste(sel, collapse = ", "), " FROM ", prev,
                  " AS sr_x_c ", paste(joins, collapse = " "), " ORDER BY sr_x_c.", id)
  list(steps = steps, extra = extra, final = final, last = prev,
       cols = c(spec$id, "index_date", vapply(spec$features, function(f) f$name, ""),
                if (is.null(o)) character() else c("time", "event")))
}

# Check the spec against the store (no SQL), make the views of the tables used, compile.
.sr_cohort_prepare <- function(project, spec) {
  spec <- .sr_cohort_norm(spec)
  sch <- .sr_cohort_check(project, spec)
  for (t in names(sch)) sr_store_view(project, t)
  list(spec = spec, sql = .sr_cohort_compile(sr_duck(project), spec, sch))
}

#' The SQL of a cohort spec (nothing is run or written). The tables and columns are checked first.
#' @return list(steps = named chr, one CTE per flow step (sr_x_base, sr_x_index, sr_x_c1, ...) so that each
#'   step's distinct-id count can be queried; extra = the feature and outcome CTEs; final = the SELECT, with
#'   its WITH clause, that returns one row per id, ORDER BY id).
sr_cohort_sql <- function(project, spec) {
  p <- .sr_cohort_prepare(project, spec)
  list(steps = p$sql$steps, extra = p$sql$extra, final = p$sql$final)
}

# ---- build ------------------------------------------------------------------------------------------

# TRUE when a filter reads the outcome: its table is the outcome event table or the censor table, or it
# uses an outcome-family column of its table (in `state`, by lineage).
.sr_cohort_dep_one <- function(state, spec, table, where) {
  if (table %in% c(spec$outcome$event_table, spec$outcome$censor_table)) return(TRUE)
  length(intersect(.sr_rc_where_cols(where), .sr_family_names(state, table))) > 0L
}

# The outcome dependence of each flow step of a spec (the index, then each criterion), against `state`.
.sr_cohort_spec_dep <- function(state, spec) {
  c(.sr_cohort_dep_one(state, spec, spec$index$table, spec$index$where),
    vapply(spec$criteria %||% list(), function(cr) .sr_cohort_dep_one(state, spec, cr$table, cr$where), logical(1)))
}

# The name map of a cohort: each feature column is derived from its col and the columns of its where.
.sr_cohort_name_map <- function(spec) {
  out <- list()
  for (f in spec$features) {
    for (cc in unique(c(f$col, .sr_rc_where_cols(f$where)))) {
      out[[length(out) + 1L]] <- list(from = f$table, col = cc, to = f$name)
    }
  }
  out
}

#' Build a cohort: count every step, materialise the final table as store table `spec$name`, append
#' `cohort.built` and set the roles of the new table (spec$id = id, event = outcome, time = time).
#' The table name must be new. Everything is checked before any SQL runs; on an error nothing is written.
#' The flow (data.frame(step, label, n_before, n_after, outcome_dependent)) has a row "index" (n_before =
#' distinct ids of the base table, n_after = those with an index date) and one row per criterion; its counts
#' are true counts (sr_flow() hides the outcome-dependent ones while the SAP is not locked).
#' A negative time (censor date before the index) is kept; the count is recorded as n_negative_time and a
#' warning names it.
#' @return the project with the events applied.
sr_cohort_build <- function(project, spec) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  spec <- .sr_cohort_norm(spec)
  .sr_derived_check_name(project, spec$name)
  sql <- .sr_cohort_prepare(project, spec)$sql
  con <- sr_duck(project)

  # the distinct-id count of each step (base, index, criteria), in one query
  nms <- names(sql$steps)
  cnt <- sr_duck_query(con, paste0(.sr_cohort_with(sql$steps), " SELECT ",
                                   paste(sprintf("(SELECT count(*) FROM %s) AS n%d", nms, seq_along(nms)), collapse = ", ")))
  n <- vapply(seq_along(nms), function(i) as.numeric(cnt[[i]][1L]), numeric(1))
  nc <- length(spec$criteria)
  dep <- .sr_cohort_spec_dep(project$state, spec)
  flow <- data.frame(
    step = c("index", if (nc) paste0("c", seq_len(nc))),
    label = c("Patients with an index date", vapply(spec$criteria, function(cr) cr$label, "")),
    n_before = n[seq_len(nc + 1L)], n_after = n[seq_len(nc + 1L) + 1L],
    outcome_dependent = dep, stringsAsFactors = FALSE)

  w <- .sr_derived_write(project, spec$name, sql$final, sql$cols, "the cohort")
  n_neg <- 0
  if (!is.null(spec$outcome)) {
    sr_store_view(project, spec$name)
    n_neg <- as.numeric(sr_duck_query(con, sprintf("SELECT count(*) AS n FROM %s WHERE %s < 0",
                                                   .sr_join_q(con, spec$name), .sr_join_q(con, "time")))$n[1L])
  }
  payload <- list(
    name = spec$name, spec = spec,
    sql = list(steps = as.list(sql$steps), extra = as.list(sql$extra), final = sql$final),
    flow = flow, n_negative_time = n_neg, store_hash = w$store_hash, store = w$store,
    outcome_dependent = any(dep), name_map = .sr_cohort_name_map(spec))
  project <- .sr_derived_append(project, spec$name, "cohort.built", payload)

  roles <- stats::setNames("id", spec$id)
  if (!is.null(spec$outcome)) roles <- c(roles, event = "outcome", time = "time")
  project <- sr_roles_set(project, spec$name, roles)
  if (n_neg > 0) {
    warning(sprintf("%.0f id(s) of cohort '%s' have a negative time: censor date before index (kept as is; a survival model cannot use them)",
                    n_neg, spec$name), call. = FALSE)
  }
  project
}

# The flow as a data.frame: the event payload carries it as a list of rows once it went through the log.
.sr_cohort_flow_df <- function(x) {
  if (is.data.frame(x) || is.null(x)) return(x)
  do.call(rbind, lapply(x, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
}

# `cohort.built` records the cohort and adds the result as a table (format "cohort", no source file: it
# is derived from the tables the spec reads).
sr_reducer_register("cohort.built", function(state, event) {
  p <- event$payload
  name <- p$name
  if (!is.character(name) || length(name) != 1L || is.na(name)) stop("cohort.built needs a name", call. = FALSE)
  co <- state$cohorts %||% list()
  c1 <- list()
  for (k in c("name", "spec", "sql", "n_negative_time", "store_hash")) c1[k] <- list(p[[k]])
  c1["flow"] <- list(.sr_cohort_flow_df(p$flow))
  co[[name]] <- c1
  state$cohorts <- co
  s <- p$spec
  from <- unique(c(s$base, s$index$table, vapply(s$criteria %||% list(), function(c) c$table, ""),
                   vapply(s$features %||% list(), function(f) f$table, ""), s$outcome$event_table,
                   s$outcome$censor_table))
  tabs <- state$tables %||% list()
  t <- tabs[[name]] %||% list()
  t$table <- name
  t$format <- "cohort"
  t["source"] <- list(NULL)
  t$store <- p$store
  t$store_hash <- p$store_hash
  t$derived_from <- as.list(from)
  # older logs have no flag: any outcome-dependent flow step makes the table dependent
  t$outcome_dependent <- if (is.null(p$outcome_dependent)) {
    any(as.logical(c1$flow$outcome_dependent) %in% TRUE)
  } else isTRUE(p$outcome_dependent)
  t$name_map <- p$name_map
  tabs[[name]] <- t
  state$tables <- tabs
  state
})
