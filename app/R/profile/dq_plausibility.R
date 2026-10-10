# profile/dq_plausibility.R - plausibility checks: are the values and the dates believable?
#   range          a known analyte (age, weight, ...) outside its hard range (block) or its soft range (info)
#   unit_mix       values that fit the alternative unit of an analyte (lb for kg, mg/dL for umol/L, ...)
#   heaping        integer values that pile up on terminal digits 0 and 5 (rounding, digit preference)
#   outlier        values far from the median in robust-SD terms AND outside the plausible range (info)
#   future_date    a date after the run date plus one day (block)
#   old_date       a date before 1900-01-01 (warn)
#   date_order     later < earlier for admit < surgery < discharge, birth < every date, time_zero < event / censor
#   immortal_time  exposure dates after time zero: the exposed rows were immortal until they were exposed
# The analytes, name patterns, units and ranges come from inst/templates/plausibility.json. A column is
# an analyte when its lower-cased name matches a name_regex (the first analyte in file order wins). Alt
# units are affine: alt = primary * factor + offset. A dictionary min / max replaces BOTH the hard and the
# soft limit on its side (so a dictionary column has no info tier), for range and for outlier; unit_mix
# always reads the template's soft range, because a unit is told apart by what is physiologically
# plausible, not by what a study chose to record.
#
# Every threshold is a share, a count or an effect size, never a p-value, so a large table does not make
# a check fire. Counts come from SQL aggregates over the FULL table; the sample is used only for the
# median and MAD of outlier. p_affected is a share of ALL rows. A column that holds numbers as text is
# read through TRY_CAST when >= 95% of its values parse (coercion reports the rest).
#
# Outcome-blind: a blind outcome-family column (outcome, event, event_date, censor_date, time, offset; see
# sr_outcome_family_cols) gets its marginals only (range, unit_mix, heaping, future_date, old_date).
# outlier needs its sample values, which the blind sample does not have, so it is skipped; immortal_time
# never pairs a family column. date_order still checks a pair with a family date (time zero before the
# event date is the outcome's own plausibility), but while blind such a finding is a flag: it names the
# later column only, with no count, share, example or detail. Every `detail` is a data.frame with a
# `column` field.

SR_DQ_UNIT_MIX_MIN_SHARE <- 0.00155
SR_DQ_UNIT_MIX_MIN_N <- 3L
SR_DQ_HEAP_MIN_N <- 200L
SR_DQ_HEAP_MIN_DISTINCT <- 20L
SR_DQ_HEAP_EXPECTED <- 0.2
SR_DQ_HEAP_SHARE <- 0.40
SR_DQ_OUTLIER_Z <- 6
SR_DQ_OLD_DATE <- "1900-01-01"
SR_DQ_IMMORTAL_SHARE <- 0.01
SR_DQ_PL_DATE_ROLES <- c("time_zero", "exposure_date", "event_date", "censor_date")
SR_DQ_PL_SKIP_ROLES <- c("id", "cluster", "weight", "ignore", SR_DQ_PL_DATE_ROLES)

# ---- the template -----------------------------------------------------------------------------------

.sr_pl_cache <- new.env(parent = emptyenv())

# inst/templates/plausibility.json: next to app/ in the repo and in the portable bundle.
.sr_pl_template_path <- function() {
  root <- get0("sr_root", envir = globalenv(), inherits = FALSE, ifnotfound = "app")
  cand <- file.path(c(dirname(root), root), "inst", "templates", "plausibility.json")
  hit <- cand[file.exists(cand)]
  if (!length(hit)) stop("the plausibility template (inst/templates/plausibility.json) was not found", call. = FALSE)
  hit[1L]
}

# The analytes as a list of list(analyte, name_regex, unit, hard, soft, alt) in file order; `alt` is a
# data.frame(unit, factor, offset) with one row per alternative unit.
.sr_pl_template <- function() {
  if (!is.null(.sr_pl_cache$template)) return(.sr_pl_cache$template)
  path <- .sr_pl_template_path()
  j <- .sr_parse_json(readChar(path, file.size(path), useBytes = TRUE))
  num2 <- function(x) as.numeric(unlist(x, use.names = FALSE))
  out <- lapply(j, function(a) {
    u <- a$alt_units
    list(analyte = as.character(a$analyte), name_regex = as.character(a$name_regex), unit = as.character(a$unit),
         hard = num2(a$hard), soft = num2(a$soft),
         alt = data.frame(unit = vapply(u, function(x) as.character(x$unit), ""),
                          factor = vapply(u, function(x) as.numeric(x$factor), numeric(1)),
                          offset = vapply(u, function(x) as.numeric(x$offset %||% 0), numeric(1)),
                          stringsAsFactors = FALSE))
  })
  .sr_pl_cache$template <- out
  out
}

# The analyte a column name belongs to (the first whose name_regex matches), or NULL.
.sr_pl_analyte <- function(col) {
  nm <- tolower(col)
  for (a in .sr_pl_template()) {
    if (grepl(a$name_regex, nm, perl = TRUE)) return(a)
  }
  NULL
}

# ---- columns ----------------------------------------------------------------------------------------

# The run date: options(sr.dq_as_of = "YYYY-MM-DD") overrides the clock (tests, reproducible reports).
.sr_pl_today <- function() {
  o <- getOption("sr.dq_as_of")
  if (is.null(o)) Sys.Date() else as.Date(o)
}

# The roles of the columns, skipping the ones in `skip`: a character vector of column names that may be
# read as measurements.
.sr_pl_role_of <- function(ctx, col) unname(ctx$roles[col])

# The numeric columns a check can read, as data.frame(column, q, expr, nn, n_distinct): `expr` is the SQL
# that yields the number (CAST for a numeric type, TRY_CAST for text that is >= 95% numeric), `nn` the
# count of non-missing numbers. Columns whose role is in `skip_roles` are left out.
.sr_pl_num_cols <- function(ctx, skip_roles = SR_DQ_PL_SKIP_ROLES) {
  pc <- ctx$profile$columns
  keep <- list()
  for (i in seq_len(nrow(pc))) {
    col <- pc$column[i]
    role <- .sr_pl_role_of(ctx, col)
    if (!is.na(role) && role %in% skip_roles) next
    q <- as.character(DBI::dbQuoteIdentifier(ctx$con, col))
    nn <- pc$n[i] - pc$n_missing[i]
    pn <- pc$p_numeric_parsable[i]
    if (.sr_prof_is_numeric(pc$type[i])) {
      expr <- sprintf("CAST(%s AS DOUBLE)", q)
    } else if (.sr_prof_is_varchar(pc$type[i]) && !is.na(pn) && pn >= SR_DQ_COERCE_MIN) {
      expr <- sprintf("TRY_CAST(%s AS DOUBLE)", q)
      nn <- round(nn * pn)
    } else next
    keep[[length(keep) + 1L]] <- data.frame(column = col, q = q, expr = expr, nn = nn,
                                            n_distinct = pc$n_distinct[i], stringsAsFactors = FALSE)
  }
  if (!length(keep)) {
    return(data.frame(column = character(), q = character(), expr = character(), nn = numeric(),
                      n_distinct = numeric(), stringsAsFactors = FALSE))
  }
  do.call(rbind, keep)
}

# The date columns, as data.frame(column, q, expr): DATE / TIMESTAMP types, and text that is >= 95% dates
# (read through TRY_CAST). `expr` yields a DATE.
.sr_pl_date_cols <- function(ctx) {
  pc <- ctx$profile$columns
  keep <- list()
  for (i in seq_len(nrow(pc))) {
    col <- pc$column[i]
    q <- as.character(DBI::dbQuoteIdentifier(ctx$con, col))
    pd <- pc$p_date_parsable[i]
    if (.sr_dq_is_date_type(pc$type[i])) {
      expr <- sprintf("CAST(%s AS DATE)", q)
    } else if (.sr_prof_is_varchar(pc$type[i]) && !is.na(pd) && pd >= SR_DQ_COERCE_MIN) {
      expr <- sprintf("TRY_CAST(%s AS DATE)", q)
    } else next
    keep[[length(keep) + 1L]] <- data.frame(column = col, q = q, expr = expr, stringsAsFactors = FALSE)
  }
  if (!length(keep)) return(data.frame(column = character(), q = character(), expr = character(), stringsAsFactors = FALSE))
  do.call(rbind, keep)
}

# The dictionary's numeric bounds of a column as c(lo, hi), NA where it gives none. Only a variable the
# dictionary types numeric / integer counts.
.sr_pl_dict_bounds <- function(ctx, col) {
  none <- c(NA_real_, NA_real_)
  d <- ctx$dict
  if (is.null(d) || !nrow(d)) return(none)
  i <- match(col, d$variable)
  if (is.na(i) || !(d$type[i] %in% c("numeric", "integer"))) return(none)
  b <- function(x) {
    v <- .sr_dq_bound(ctx, "numeric", x)
    if (is.null(v)) NA_real_ else v$value
  }
  c(b(d$min[i]), b(d$max[i]))
}

# The limits of a column: list(analyte, hard, soft, source) with c(lo, hi) vectors (NA = unbounded), or
# NULL when the column is neither an analyte nor bounded by the dictionary. A dictionary bound replaces
# both the hard and the soft limit on its side.
.sr_pl_limits <- function(ctx, col) {
  a <- .sr_pl_analyte(col)
  db <- .sr_pl_dict_bounds(ctx, col)
  if (is.null(a) && all(is.na(db))) return(NULL)
  hard <- if (is.null(a)) c(NA_real_, NA_real_) else a$hard
  soft <- if (is.null(a)) c(NA_real_, NA_real_) else a$soft
  for (k in 1:2) {
    if (!is.na(db[k])) {
      hard[k] <- db[k]
      soft[k] <- db[k]
    }
  }
  src <- if (all(is.na(db))) "template" else if (is.null(a) || all(!is.na(db))) "dictionary" else "dictionary and template"
  list(analyte = a, hard = signif(hard, 12), soft = signif(soft, 12), source = src)
}

# ---- intervals: one description for the SQL predicate and the recipe filter -------------------------

# An interval of a number line; lo / hi NA = unbounded, *_incl = the end belongs to it.
.sr_pl_piece <- function(lo = NA_real_, hi = NA_real_, lo_incl = FALSE, hi_incl = FALSE) {
  list(lo = lo, hi = hi, lo_incl = lo_incl, hi_incl = hi_incl)
}

.sr_pl_piece_sql <- function(p, expr) {
  conds <- c(if (!is.na(p$lo)) sprintf("%s %s %s", expr, if (p$lo_incl) ">=" else ">", .sr_dq_num(p$lo)),
             if (!is.na(p$hi)) sprintf("%s %s %s", expr, if (p$hi_incl) "<=" else "<", .sr_dq_num(p$hi)))
  paste0("(", paste(conds, collapse = " AND "), ")")
}

# The SQL predicate of a union of intervals.
.sr_pl_pieces_sql <- function(pieces, expr) {
  paste0("(", paste(vapply(pieces, .sr_pl_piece_sql, "", expr = expr), collapse = " OR "), ")")
}

.sr_pl_piece_where <- function(p, col) {
  leaves <- c(if (!is.na(p$lo)) list(.sr_where_leaf(col, if (p$lo_incl) ">=" else ">", p$lo)),
              if (!is.na(p$hi)) list(.sr_where_leaf(col, if (p$hi_incl) "<=" else "<", p$hi)))
  if (length(leaves) == 1L) leaves[[1L]] else .sr_where_all(leaves[[1L]], leaves[[2L]])
}

# The recipe filter of a union of intervals.
.sr_pl_pieces_where <- function(pieces, col) {
  w <- lapply(pieces, .sr_pl_piece_where, col = col)
  if (length(w) == 1L) w[[1L]] else list(any = w)
}

.sr_pl_fmt <- function(x) {
  vapply(x, function(v) if (is.na(v)) "" else format(signif(v, 4), scientific = FALSE, trim = TRUE), "")
}

.sr_pl_range_txt <- function(r) sprintf("[%s, %s]", .sr_pl_fmt(r[1L]), .sr_pl_fmt(r[2L]))

# ---- range ------------------------------------------------------------------------------------------

.sr_dq_check_range <- function(ctx) {
  nc <- .sr_pl_num_cols(ctx)
  n <- ctx$profile$rows
  todo <- list()
  for (i in seq_len(nrow(nc))) {
    col <- nc$column[i]
    lim <- .sr_pl_limits(ctx, col)
    if (is.null(lim) || is.null(lim$analyte)) next   # a dictionary-only column is dict_range's business
    hard <- lim$hard
    soft <- lim$soft
    block <- Filter(Negate(is.null), list(
      if (!is.na(hard[1L])) .sr_pl_piece(hi = hard[1L]),
      if (!is.na(hard[2L])) .sr_pl_piece(lo = hard[2L])))
    # between the soft and the hard limit; a dictionary makes them equal, so it has no such band
    info <- Filter(Negate(is.null), list(
      if (!is.na(soft[1L]) && (is.na(hard[1L]) || soft[1L] > hard[1L])) {
        .sr_pl_piece(lo = hard[1L], lo_incl = TRUE, hi = soft[1L])
      },
      if (!is.na(soft[2L]) && (is.na(hard[2L]) || soft[2L] < hard[2L])) {
        .sr_pl_piece(lo = soft[2L], hi = hard[2L], hi_incl = TRUE)
      }))
    for (tier in c("block", "info")) {
      pieces <- if (tier == "block") block else info
      if (!length(pieces)) next
      todo[[length(todo) + 1L]] <- list(col = col, q = nc$q[i], tier = tier, pieces = pieces, lim = lim,
                                        pred = .sr_pl_pieces_sql(pieces, nc$expr[i]))
    }
  }
  if (!length(todo)) return(sr_findings_empty())
  counts <- .sr_dq_count(ctx, vapply(todo, function(t) t$pred, ""))
  out <- list()
  for (j in seq_along(todo)) {
    k <- counts[j]
    if (k < 1) next
    t <- todo[[j]]
    a <- t$lim$analyte
    ev <- .sr_dq_values(ctx, t$q, t$pred, limit = SR_DQ_EXAMPLES_N)
    hard_txt <- .sr_pl_range_txt(t$lim$hard)
    msg <- if (t$tier == "block") {
      sprintf("%s has %s value(s) outside the possible range %s %s (e.g. %s)", t$col, .sr_dq_big(k), hard_txt, a$unit,
              paste(ev$value, collapse = ", "))
    } else {
      sprintf("%s has %s value(s) outside the usual range %s %s but inside the possible range %s (e.g. %s)", t$col,
              .sr_dq_big(k), .sr_pl_range_txt(t$lim$soft), a$unit, hard_txt, paste(ev$value, collapse = ", "))
    }
    lim_df <- data.frame(column = t$col, analyte = a$analyte, unit = a$unit, tier = t$tier,
                         hard_lo = t$lim$hard[1L], hard_hi = t$lim$hard[2L], soft_lo = t$lim$soft[1L],
                         soft_hi = t$lim$soft[2L], source = t$lim$source, stringsAsFactors = FALSE)
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "range", "plausibility", t$col, if (t$tier == "block") "block" else "info", k, k / n, msg,
      where = .sr_pl_pieces_where(t$pieces, t$col), examples = ev$value, detail = list(limits = lim_df))
  }
  .sr_dq_bind(out)
}

# ---- unit_mix ---------------------------------------------------------------------------------------

.sr_dq_check_unit_mix <- function(ctx) {
  nc <- .sr_pl_num_cols(ctx)
  n <- ctx$profile$rows
  todo <- list()
  for (i in seq_len(nrow(nc))) {
    col <- nc$column[i]
    a <- .sr_pl_analyte(col)
    if (is.null(a) || !nrow(a$alt)) next
    plo <- a$soft[1L]
    phi <- a$soft[2L]
    for (j in seq_len(nrow(a$alt))) {
      alt <- a$alt[j, ]
      # the primary soft range expressed in the alt unit (factor > 0, so the order is kept)
      alo <- signif(plo * alt$factor + alt$offset, 12)
      ahi <- signif(phi * alt$factor + alt$offset, 12)
      e <- nc$expr[i]
      todo[[length(todo) + 1L]] <- list(
        col = col, q = nc$q[i], nn = nc$nn[i], a = a, alt = alt, alo = alo, ahi = ahi,
        pred = sprintf("(%s BETWEEN %s AND %s) AND (%s < %s OR %s > %s)", e, .sr_dq_num(alo), .sr_dq_num(ahi), e,
                       .sr_dq_num(plo), e, .sr_dq_num(phi)))
    }
  }
  if (!length(todo)) return(sr_findings_empty())
  counts <- .sr_dq_count(ctx, vapply(todo, function(t) t$pred, ""))
  out <- list()
  for (j in seq_along(todo)) {
    t <- todo[[j]]
    k <- counts[j]
    if (k < SR_DQ_UNIT_MIX_MIN_N || t$nn < 1 || k / t$nn < SR_DQ_UNIT_MIX_MIN_SHARE) next
    a <- t$a
    plo <- a$soft[1L]
    phi <- a$soft[2L]
    ev <- .sr_dq_values(ctx, t$q, t$pred, limit = SR_DQ_EXAMPLES_N)
    where <- .sr_where_all(.sr_where_leaf(t$col, "between", c(t$alo, t$ahi)),
                           .sr_where_any(.sr_where_leaf(t$col, "<", plo), .sr_where_leaf(t$col, ">", phi)))
    msg <- sprintf(paste0("%s holds %s value(s) (%s of its values) that look like %s rather than %s: they fit %s ",
                          "but not %s (e.g. %s); a convert_unit step can convert them"),
                   t$col, .sr_dq_big(k), .sr_dq_pct(k / t$nn), t$alt$unit, a$unit,
                   sprintf("%s %s", .sr_pl_range_txt(c(t$alo, t$ahi)), t$alt$unit),
                   sprintf("%s %s", .sr_pl_range_txt(c(plo, phi)), a$unit), paste(ev$value, collapse = ", "))
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "unit_mix", "plausibility", t$col, "warn", k, k / n, msg, where = where, examples = ev$value,
      detail = list(units = data.frame(column = t$col, analyte = a$analyte, primary_unit = a$unit,
                                       alt_unit = t$alt$unit, factor = t$alt$factor, offset = t$alt$offset, n = k,
                                       p_values = k / t$nn, alt_lo = t$alo, alt_hi = t$ahi, primary_lo = plo,
                                       primary_hi = phi, stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

# ---- heaping ----------------------------------------------------------------------------------------

# Candidate columns of heaping and outlier: numeric, >= 20 distinct values (so no flag and no short scale).
.sr_pl_measure_cols <- function(ctx) {
  nc <- .sr_pl_num_cols(ctx)
  nc[!is.na(nc$n_distinct) & nc$n_distinct >= SR_DQ_HEAP_MIN_DISTINCT, , drop = FALSE]
}

.sr_dq_check_heaping <- function(ctx) {
  nc <- .sr_pl_measure_cols(ctx)
  nc <- nc[nc$nn >= SR_DQ_HEAP_MIN_N, , drop = FALSE]
  if (!nrow(nc)) return(sr_findings_empty())
  n <- ctx$profile$rows
  e <- nc$expr
  # per column: integer-valued check, and the values ending in 0 or 5
  preds <- as.vector(rbind(sprintf("%s <> round(%s)", e, e),
                           sprintf("%s = round(%s) AND (TRY_CAST(abs(%s) AS BIGINT) %% 10) IN (0, 5)", e, e, e)))
  counts <- .sr_dq_count(ctx, preds)
  out <- list()
  for (i in seq_len(nrow(nc))) {
    non_int <- counts[2L * i - 1L]
    n05 <- counts[2L * i]
    nn <- nc$nn[i]
    if (non_int > 0 || n05 / nn < SR_DQ_HEAP_SHARE) next
    col <- nc$column[i]
    obs <- n05 / nn
    excess <- max(1, round(n05 - SR_DQ_HEAP_EXPECTED * nn))
    ev <- .sr_dq_values(ctx, nc$q[i], preds[2L * i], limit = SR_DQ_EXAMPLES_N)
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "heaping", "plausibility", col, "warn", excess, excess / n,
      sprintf(paste0("%s: %s of its %s integer values end in 0 or 5 (about %s expected), roughly %s more than chance; ",
                     "the values look rounded"), col, .sr_dq_pct(obs), .sr_dq_big(nn), .sr_dq_pct(SR_DQ_HEAP_EXPECTED),
              .sr_dq_big(excess)),
      examples = ev$value,
      detail = list(heaping = data.frame(column = col, n = nn, n_heaped = n05, observed = obs,
                                         expected = SR_DQ_HEAP_EXPECTED, threshold = SR_DQ_HEAP_SHARE,
                                         stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

# ---- outlier ----------------------------------------------------------------------------------------

.sr_dq_check_outlier <- function(ctx) {
  nc <- .sr_pl_measure_cols(ctx)
  n <- ctx$profile$rows
  todo <- list()
  for (i in seq_len(nrow(nc))) {
    col <- nc$column[i]
    lim <- .sr_pl_limits(ctx, col)
    if (is.null(lim) || !(col %in% names(ctx$sample))) next
    x <- suppressWarnings(as.numeric(ctx$sample[[col]]))
    x <- x[is.finite(x)]
    if (length(x) < SR_DQ_HEAP_MIN_DISTINCT) next
    med <- stats::median(x)
    s <- stats::mad(x)
    if (!is.finite(s) || s <= 0) next
    lc <- signif(med - SR_DQ_OUTLIER_Z * s, 12)
    hc <- signif(med + SR_DQ_OUTLIER_Z * s, 12)
    slo <- if (is.na(lim$soft[1L])) -Inf else lim$soft[1L]
    shi <- if (is.na(lim$soft[2L])) Inf else lim$soft[2L]
    # (below lc or above hc) AND (below slo or above shi), as a union of open intervals
    pieces <- list()
    if (is.finite(min(lc, slo))) pieces[[length(pieces) + 1L]] <- .sr_pl_piece(hi = min(lc, slo))
    if (is.finite(max(hc, shi))) pieces[[length(pieces) + 1L]] <- .sr_pl_piece(lo = max(hc, shi))
    if (is.finite(shi) && shi < lc) pieces[[length(pieces) + 1L]] <- .sr_pl_piece(lo = shi, hi = lc)
    if (is.finite(slo) && hc < slo) pieces[[length(pieces) + 1L]] <- .sr_pl_piece(lo = hc, hi = slo)
    if (!length(pieces)) next
    todo[[length(todo) + 1L]] <- list(col = col, q = nc$q[i], pieces = pieces, med = med, s = s, lc = lc, hc = hc,
                                      lim = lim, pred = .sr_pl_pieces_sql(pieces, nc$expr[i]))
  }
  if (!length(todo)) return(sr_findings_empty())
  counts <- .sr_dq_count(ctx, vapply(todo, function(t) t$pred, ""))
  out <- list()
  for (j in seq_along(todo)) {
    k <- counts[j]
    if (k < 1) next
    t <- todo[[j]]
    ev <- .sr_dq_values(ctx, t$q, t$pred, limit = SR_DQ_EXAMPLES_N)
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "outlier", "plausibility", t$col, "info", k, k / n,
      sprintf(paste0("%s has %s value(s) more than %g robust SDs from its median %s (robust SD %s) and outside the ",
                     "plausible range %s (e.g. %s); they are flagged, never deleted"), t$col, .sr_dq_big(k),
              SR_DQ_OUTLIER_Z, .sr_pl_fmt(t$med), .sr_pl_fmt(t$s), .sr_pl_range_txt(t$lim$soft),
              paste(ev$value, collapse = ", ")),
      where = .sr_pl_pieces_where(t$pieces, t$col), examples = ev$value,
      detail = list(robust = data.frame(column = t$col, median = t$med, robust_sd = t$s, z_cut = SR_DQ_OUTLIER_Z,
                                        lower = t$lc, upper = t$hc, n = k, stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

# ---- future_date and old_date -----------------------------------------------------------------------

# One finding per date column that has values where `pred_of(expr)` holds.
.sr_pl_date_findings <- function(ctx, check, severity, pred_of, where_of, msg_of, detail_of,
                                 id_where_of = where_of) {
  dc <- .sr_pl_date_cols(ctx)
  if (!nrow(dc)) return(sr_findings_empty())
  n <- ctx$profile$rows
  preds <- vapply(dc$expr, pred_of, "", USE.NAMES = FALSE)
  counts <- .sr_dq_count(ctx, preds)
  out <- list()
  for (i in seq_len(nrow(dc))) {
    k <- counts[i]
    if (k < 1) next
    col <- dc$column[i]
    ev <- .sr_dq_values(ctx, dc$q[i], preds[i], limit = SR_DQ_EXAMPLES_N)
    out[[length(out) + 1L]] <- .sr_dq_finding(
      check, "plausibility", col, severity, k, k / n, msg_of(col, k, ev$value), where = where_of(col),
      examples = ev$value, detail = detail_of(col),
      id_where = id_where_of(col))
  }
  .sr_dq_bind(out)
}

.sr_dq_check_future_date <- function(ctx) {
  as_of <- .sr_pl_today()
  cutoff <- format(as_of + 1L)
  # a date column holds a date and a timestamp column a time of day: ">= as_of + 2" selects exactly the rows
  # the count selects (CAST(x AS DATE) > as_of + 1), for both
  from <- format(as_of + 2L)
  .sr_pl_date_findings(
    ctx, "future_date", "block",
    pred_of = function(expr) sprintf("%s > CAST('%s' AS DATE)", expr, cutoff),
    where_of = function(col) .sr_where_leaf(col, ">=", from),
    id_where_of = function(col) .sr_where_leaf(col, ">=", "run_date+2"),   # the id must not change with the day
    msg_of = function(col, k, ex) {
      sprintf("%s has %s date(s) after %s, the day after the run date (e.g. %s)", col, .sr_dq_big(k), cutoff,
              paste(ex, collapse = ", "))
    },
    detail_of = function(col) {
      list(as_of = data.frame(column = col, as_of = format(as_of), cutoff = cutoff, stringsAsFactors = FALSE))
    })
}

.sr_dq_check_old_date <- function(ctx) {
  .sr_pl_date_findings(
    ctx, "old_date", "warn",
    pred_of = function(expr) sprintf("%s < CAST('%s' AS DATE)", expr, SR_DQ_OLD_DATE),
    where_of = function(col) .sr_where_leaf(col, "<", SR_DQ_OLD_DATE),
    msg_of = function(col, k, ex) {
      sprintf("%s has %s date(s) before %s (e.g. %s)", col, .sr_dq_big(k), SR_DQ_OLD_DATE, paste(ex, collapse = ", "))
    },
    detail_of = function(col) {
      list(limit = data.frame(column = col, earliest = SR_DQ_OLD_DATE, stringsAsFactors = FALSE))
    })
}

# ---- date_order -------------------------------------------------------------------------------------

# Date columns that may be paired with others: a blind outcome-family column is never paired.
.sr_pl_pairable <- function(ctx) {
  dc <- .sr_pl_date_cols(ctx)
  dc[!(dc$column %in% .sr_dq_blind_outcome(ctx)), , drop = FALSE]
}

.sr_pl_by_role <- function(ctx, dc, role) {
  intersect(names(ctx$roles)[ctx$roles == role], dc$column)
}

.sr_pl_by_name <- function(dc, re) dc$column[grepl(re, tolower(dc$column), perl = TRUE)]

.sr_dq_check_date_order <- function(ctx) {
  dc <- .sr_pl_date_cols(ctx)
  fam <- .sr_dq_blind_outcome(ctx)
  if (nrow(dc) < 2L) return(sr_findings_empty())
  admit <- .sr_pl_by_name(dc, "admi(t|ss)")
  surgery <- .sr_pl_by_name(dc, "surg|operation|procedure")
  discharge <- .sr_pl_by_name(dc, "disch")
  birth <- .sr_pl_by_name(dc, "birth|(^|_)dob($|_)")
  # roles first, then names; time_zero is a design choice, so it is never guessed from a name
  tz <- .sr_pl_by_role(ctx, dc, "time_zero")
  event <- .sr_pl_by_role(ctx, dc, "event_date")
  if (!length(event)) event <- .sr_pl_by_name(dc, "death|died|event|outcome")
  censor <- .sr_pl_by_role(ctx, dc, "censor_date")
  if (!length(censor)) censor <- .sr_pl_by_name(dc, "censor")
  pairs <- list()
  add <- function(earlier, later) {
    for (a in earlier) for (b in later) if (!identical(a, b)) pairs[[length(pairs) + 1L]] <<- c(a, b)
  }
  add(admit, surgery)
  add(surgery, discharge)
  add(admit, discharge)
  add(birth, setdiff(dc$column, birth))
  if (length(tz)) {
    add(tz, event)
    add(tz, censor)
  }
  if (!length(pairs)) return(sr_findings_empty())
  pairs <- pairs[!duplicated(vapply(pairs, paste, "", collapse = "\r"))]
  ex <- function(col) dc$expr[match(col, dc$column)]
  preds <- vapply(pairs, function(p) sprintf("%s < %s", ex(p[2L]), ex(p[1L])), "")
  counts <- .sr_dq_count(ctx, preds)
  n <- ctx$profile$rows
  out <- list()
  for (i in seq_along(pairs)) {
    k <- counts[i]
    if (k < 1) next
    earlier <- pairs[[i]][1L]
    later <- pairs[[i]][2L]
    if (any(c(earlier, later) %in% fam)) {
      out[[length(out) + 1L]] <- .sr_dq_finding(
        "date_order", "plausibility", later, "warn", NA_integer_, NA_real_,
        sprintf("%s is out of order with another date column in some rows (details withheld until the SAP is locked)", later),
        salt = earlier)
      next
    }
    val <- sprintf("concat(CAST(%s AS VARCHAR), ' < ', CAST(%s AS VARCHAR))", ex(later), ex(earlier))
    evd <- .sr_dq_values(ctx, val, preds[i], limit = SR_DQ_EXAMPLES_N)
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "date_order", "plausibility", later, "warn", k, k / n,
      sprintf("%s is before %s in %s row(s) (%s of rows; e.g. %s)", later, earlier, .sr_dq_big(k), .sr_dq_pct(k / n),
              paste(evd$value, collapse = ", ")),
      examples = evd$value, salt = earlier,
      detail = list(pair = data.frame(column = later, earlier = earlier, later = later, n = k, p = k / n,
                                      stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

# ---- immortal_time ----------------------------------------------------------------------------------

.sr_dq_check_immortal_time <- function(ctx) {
  dc <- .sr_pl_pairable(ctx)
  tz <- .sr_pl_by_role(ctx, dc, "time_zero")
  ex <- .sr_pl_by_role(ctx, dc, "exposure_date")
  if (!length(tz) || !length(ex)) return(sr_findings_empty())
  expr <- function(col) dc$expr[match(col, dc$column)]
  pairs <- list()
  for (a in ex) for (b in tz) if (!identical(a, b)) pairs[[length(pairs) + 1L]] <- c(a, b)
  if (!length(pairs)) return(sr_findings_empty())
  preds <- unlist(lapply(pairs, function(p) {
    c(sprintf("%s IS NOT NULL AND %s IS NOT NULL", expr(p[1L]), expr(p[2L])),
      sprintf("%s > %s", expr(p[1L]), expr(p[2L])))
  }))
  counts <- .sr_dq_count(ctx, preds)
  n <- ctx$profile$rows
  out <- list()
  for (i in seq_along(pairs)) {
    n_exp <- counts[2L * i - 1L]
    k <- counts[2L * i]
    if (n_exp < 1 || k / n_exp <= SR_DQ_IMMORTAL_SHARE) next
    e <- pairs[[i]][1L]
    z <- pairs[[i]][2L]
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "immortal_time", "plausibility", e, "warn", k, k / n,
      sprintf(paste0("%s is after %s in %s of %s exposed rows (%s): treating the exposure as fixed from time zero ",
                     "would give those rows immortal time. Use a time-varying exposure or a landmark / ",
                     "clone-censor-weight design"), e, z, .sr_dq_big(k), .sr_dq_big(n_exp), .sr_dq_pct(k / n_exp)),
      salt = z,
      detail = list(immortal = data.frame(column = e, time_zero = z, n_exposed = n_exp, n_after = k,
                                          p_after = k / n_exp, stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

.sr_dq_register("range", "plausibility", .sr_dq_check_range)
.sr_dq_register("unit_mix", "plausibility", .sr_dq_check_unit_mix)
.sr_dq_register("heaping", "plausibility", .sr_dq_check_heaping)
.sr_dq_register("outlier", "plausibility", .sr_dq_check_outlier)
.sr_dq_register("future_date", "plausibility", .sr_dq_check_future_date)
.sr_dq_register("old_date", "plausibility", .sr_dq_check_old_date)
.sr_dq_register("date_order", "plausibility", .sr_dq_check_date_order)
.sr_dq_register("immortal_time", "plausibility", .sr_dq_check_immortal_time)
