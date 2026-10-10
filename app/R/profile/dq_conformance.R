# profile/dq_conformance.R - conformance checks: does the stored data look like what it is declared to be?
#   coercion     a VARCHAR column that is >= 95% numeric (or date) text with values that do not parse
#   dict_type    the dictionary type disagrees with the store type
#   dict_levels  values outside the dictionary levels
#   dict_range   values outside the dictionary min / max
# All four are severity warn. The three dict_* checks return nothing when the table has no dictionary.
# Counts come from SQL over the FULL table; the checks never pull the data into R.
#
# Outcome-blind: these are single-column checks (type, levels, range of one column); they never cross a
# column with another. coercion needs the parse shares, which the profile redacts for a blind outcome
# column, so it skips it.

SR_DQ_COERCE_MIN <- 0.95
SR_DQ_WHERE_IN_MAX <- 50L

# ---- helpers ----------------------------------------------------------------------------------------

.sr_dq_is_date_type <- function(type) grepl("^(DATE|TIMESTAMP)", toupper(type))

# The profile row of each column, by name.
.sr_dq_prof_row <- function(ctx, col) {
  pc <- ctx$profile$columns
  pc[match(col, pc$column), , drop = FALSE]
}

# Dictionary rows for columns that exist in the store (a dictionary-only variable is not a data problem).
.sr_dq_dict_rows <- function(ctx) {
  d <- ctx$dict
  if (is.null(d) || !nrow(d)) return(NULL)
  d <- d[d$variable %in% ctx$profile$columns$column, , drop = FALSE]
  if (nrow(d)) d else NULL
}

# The distinct offending values of `expr` where `pred`, as text, and a `where` IN filter over them when
# there are at most SR_DQ_WHERE_IN_MAX of them (NULL otherwise).
.sr_dq_in_where <- function(ctx, col, expr, pred) {
  v <- .sr_dq_values(ctx, expr, pred, limit = SR_DQ_WHERE_IN_MAX + 1L)
  where <- if (nrow(v) <= SR_DQ_WHERE_IN_MAX && nrow(v) > 0L) .sr_where_leaf(col, "in", v$value) else NULL
  list(examples = utils::head(v$value, SR_DQ_EXAMPLES_N), where = where, values = v)
}

# ---- coercion ---------------------------------------------------------------------------------------

.sr_dq_check_coercion <- function(ctx) {
  pc <- ctx$profile$columns
  n <- ctx$profile$rows
  out <- list()
  for (i in seq_len(nrow(pc))) {
    col <- pc$column[i]
    if (!.sr_prof_is_varchar(pc$type[i])) next
    pn <- pc$p_numeric_parsable[i]
    pd <- pc$p_date_parsable[i]
    kind <- if (!is.na(pn) && pn >= SR_DQ_COERCE_MIN) "numeric" else if (!is.na(pd) && pd >= SR_DQ_COERCE_MIN) "date" else next
    q <- DBI::dbQuoteIdentifier(ctx$con, col)
    cast <- sprintf("TRY_CAST(%s AS %s)", q, if (kind == "numeric") "DOUBLE" else "DATE")
    pred <- sprintf("%s IS NOT NULL AND %s IS NULL", q, cast)
    k <- .sr_dq_count(ctx, pred)
    if (k < 1) next
    ev <- .sr_dq_in_where(ctx, col, q, pred)
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "coercion", "conformance", col, "warn", k, k / n,
      sprintf("%s is stored as text but %s of its values are %s; %s value(s) do not parse (e.g. %s)", col,
              sprintf("%.1f%%", 100 * if (kind == "numeric") pn else pd), if (kind == "numeric") "numbers" else "dates",
              format(k, big.mark = ",", scientific = FALSE), paste(sprintf("'%s'", ev$examples), collapse = ", ")),
      where = ev$where, examples = ev$examples,
      detail = list(values = data.frame(column = col, value = ev$values$value, n = ev$values$n,
                                        stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

# ---- dict_type --------------------------------------------------------------------------------------

# Store types a dictionary type accepts.
.sr_dq_type_ok <- function(dict_type, store_type) {
  num <- .sr_prof_is_numeric(store_type)
  chr <- .sr_prof_is_varchar(store_type)
  lgl <- identical(toupper(store_type), "BOOLEAN")
  dt <- .sr_dq_is_date_type(store_type)
  switch(dict_type,
         numeric = , integer = num,
         categorical = num || chr || lgl,
         text = chr,
         date = , datetime = dt,
         logical = lgl || num,
         TRUE)
}

.sr_dq_check_dict_type <- function(ctx) {
  d <- .sr_dq_dict_rows(ctx)
  if (is.null(d)) return(sr_findings_empty())
  pc <- ctx$profile$columns
  n <- ctx$profile$rows
  out <- list()
  for (i in seq_len(nrow(d))) {
    col <- d$variable[i]
    pr <- pc[match(col, pc$column), , drop = FALSE]
    if (.sr_dq_type_ok(d$type[i], pr$type)) next
    nn <- pr$n - pr$n_missing
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "dict_type", "conformance", col, "warn", nn, if (n > 0) nn / n else 0,
      sprintf("the dictionary types %s as %s but the store holds %s", col, d$type[i], pr$type),
      detail = list(types = data.frame(column = col, dictionary_type = d$type[i], store_type = pr$type,
                                       stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

# ---- dict_levels ------------------------------------------------------------------------------------

.sr_dq_check_dict_levels <- function(ctx) {
  d <- .sr_dq_dict_rows(ctx)
  if (is.null(d)) return(sr_findings_empty())
  pc <- ctx$profile$columns
  n <- ctx$profile$rows
  con <- ctx$con
  todo <- list()
  for (i in seq_len(nrow(d))) {
    lv <- d$levels[[i]]
    if (!(d$type[i] %in% c("categorical", "logical")) || is.null(lv) || !nrow(lv)) next
    col <- d$variable[i]
    type <- pc$type[match(col, pc$column)]
    # a level may be written as its code or its label
    vals <- unique(c(lv$code, lv$label))
    q <- DBI::dbQuoteIdentifier(con, col)
    if (.sr_prof_is_numeric(type)) {
      num <- suppressWarnings(as.numeric(vals))
      num <- unique(num[!is.na(num)])
      if (!length(num)) next
      expr <- sprintf("CAST(%s AS DOUBLE)", q)
      lits <- paste(vapply(num, .sr_dq_num, ""), collapse = ", ")
      vals <- num
    } else if (.sr_prof_is_varchar(type)) {
      expr <- q
      lits <- paste(DBI::dbQuoteString(con, vals), collapse = ", ")
    } else if (identical(toupper(type), "BOOLEAN")) {
      expr <- sprintf("lower(CAST(%s AS VARCHAR))", q)
      vals <- tolower(vals)
      lits <- paste(DBI::dbQuoteString(con, vals), collapse = ", ")
    } else next
    todo[[length(todo) + 1L]] <- list(col = col, q = q, expr = expr, vals = vals,
                                      pred = sprintf("%s IS NOT NULL AND %s NOT IN (%s)", q, expr, lits))
  }
  if (!length(todo)) return(sr_findings_empty())
  counts <- .sr_dq_count(ctx, vapply(todo, function(t) t$pred, ""))
  out <- list()
  for (j in seq_along(todo)) {
    if (counts[j] < 1) next
    t <- todo[[j]]
    ev <- .sr_dq_values(ctx, t$q, t$pred, limit = SR_DQ_WHERE_IN_MAX + 1L)
    where <- .sr_where_all(.sr_where_leaf(t$col, "not_na"), .sr_where_leaf(t$col, "not_in", t$vals))
    ex <- utils::head(ev$value, SR_DQ_EXAMPLES_N)
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "dict_levels", "conformance", t$col, "warn", counts[j], counts[j] / n,
      sprintf("%s has %s value(s) outside the dictionary levels (e.g. %s)", t$col,
              format(counts[j], big.mark = ",", scientific = FALSE), paste(sprintf("'%s'", ex), collapse = ", ")),
      where = where, examples = ex,
      detail = list(values = data.frame(column = t$col, value = ev$value, n = ev$n, stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

# ---- dict_range -------------------------------------------------------------------------------------

# A dictionary bound as list(sql, value) for the store type, or NULL when it is empty or unusable.
.sr_dq_bound <- function(ctx, kind, bound) {
  bound <- trimws(bound)
  if (!nzchar(bound)) return(NULL)
  if (kind == "numeric") {
    v <- suppressWarnings(as.numeric(bound))
    if (is.na(v) || !is.finite(v)) return(NULL)
    return(list(sql = .sr_dq_num(v), value = v))
  }
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", bound) || is.na(suppressWarnings(as.Date(bound, optional = TRUE)))) return(NULL)
  list(sql = sprintf("CAST(%s AS DATE)", DBI::dbQuoteString(ctx$con, bound)), value = bound)
}

.sr_dq_check_dict_range <- function(ctx) {
  d <- .sr_dq_dict_rows(ctx)
  if (is.null(d)) return(sr_findings_empty())
  pc <- ctx$profile$columns
  n <- ctx$profile$rows
  todo <- list()
  for (i in seq_len(nrow(d))) {
    kind <- if (d$type[i] %in% c("numeric", "integer")) "numeric" else if (d$type[i] %in% c("date", "datetime")) "date" else next
    col <- d$variable[i]
    type <- pc$type[match(col, pc$column)]
    q <- DBI::dbQuoteIdentifier(ctx$con, col)
    expr <- if (kind == "numeric") {
      if (.sr_prof_is_numeric(type)) sprintf("CAST(%s AS DOUBLE)", q)
      else if (.sr_prof_is_varchar(type)) sprintf("TRY_CAST(%s AS DOUBLE)", q)
      else next
    } else {
      if (.sr_dq_is_date_type(type)) sprintf("CAST(%s AS DATE)", q)
      else if (.sr_prof_is_varchar(type)) sprintf("TRY_CAST(%s AS DATE)", q)
      else next
    }
    lo <- .sr_dq_bound(ctx, kind, d$min[i])
    hi <- .sr_dq_bound(ctx, kind, d$max[i])
    if (is.null(lo) && is.null(hi)) next
    conds <- c(if (!is.null(lo)) sprintf("%s < %s", expr, lo$sql), if (!is.null(hi)) sprintf("%s > %s", expr, hi$sql))
    todo[[length(todo) + 1L]] <- list(col = col, q = q, expr = expr, lo = lo, hi = hi,
                                      pred = paste0("(", paste(conds, collapse = " OR "), ")"))
  }
  if (!length(todo)) return(sr_findings_empty())
  counts <- .sr_dq_count(ctx, vapply(todo, function(t) t$pred, ""))
  out <- list()
  for (j in seq_along(todo)) {
    if (counts[j] < 1) next
    t <- todo[[j]]
    ev <- .sr_dq_values(ctx, t$q, t$pred, limit = SR_DQ_EXAMPLES_N)
    leaves <- c(if (!is.null(t$lo)) list(.sr_where_leaf(t$col, "<", t$lo$value)),
                if (!is.null(t$hi)) list(.sr_where_leaf(t$col, ">", t$hi$value)))
    where <- if (length(leaves) == 1L) leaves[[1L]] else list(any = leaves)
    rng <- paste0("[", if (is.null(t$lo)) "" else t$lo$value, ", ", if (is.null(t$hi)) "" else t$hi$value, "]")
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "dict_range", "conformance", t$col, "warn", counts[j], counts[j] / n,
      sprintf("%s has %s value(s) outside the dictionary range %s (e.g. %s)", t$col,
              format(counts[j], big.mark = ",", scientific = FALSE), rng, paste(ev$value, collapse = ", ")),
      where = where, examples = ev$value,
      detail = list(values = data.frame(column = t$col, value = ev$value, n = ev$n, stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

.sr_dq_register("coercion", "conformance", .sr_dq_check_coercion)
.sr_dq_register("dict_type", "conformance", .sr_dq_check_dict_type)
.sr_dq_register("dict_levels", "conformance", .sr_dq_check_dict_levels)
.sr_dq_register("dict_range", "conformance", .sr_dq_check_dict_range)
