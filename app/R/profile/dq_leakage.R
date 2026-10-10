# profile/dq_leakage.R - outcome-blind leakage checks: columns that give the outcome away.
#   leakage            a column whose single-variable AUC for a binary outcome is >= 0.99 either way (block)
#   separation         a level of a categorical column in which the outcome never varies, in >= 10 rows (warn)
#   post_outcome_date  a date column that is mostly AFTER the event date (warn)
# leakage and separation need the outcome VALUES, which sr_sample() drops while blind. They read the
# outcome of the SAME sample rows through .sr_dq_sample_col() (dq_completeness.R), inside the check
# function: the values never leave it. While blind the findings name the column and nothing else: n_affected
# and p_affected are NA, `where` is NULL, examples and detail are empty, and the message carries no AUC, no
# level name, no direction and no count. Once the SAP is locked, leakage carries its AUC in a `detail` frame
# and separation lists the levels. post_outcome_date compares two date columns and never reads the outcome,
# so it is the same blind or not.
#
# Candidates: every column except the outcome and the columns that are deterministic for it by
# construction (id, cluster, time_zero, event_date, censor_date, time, event) and `ignore`, and, while blind,
# the exposure roles (exposure, exposure_date: "no deaths in the treated arm" is an outcome-by-exposure fact). AUC is the
# rank-based Mann-Whitney statistic; a categorical column is scored by the outcome rate of its level (the
# levels ordered by outcome rate), over the levels with at least 10 rows and only for 2-50 levels, so a
# free-text column cannot "leak" through singleton levels. Both are computed on the reproducible sample.

SR_DQ_LEAK_AUC <- 0.99
SR_DQ_LEAK_MIN_N <- 10L
SR_DQ_LEAK_MIN_LEVEL <- 10L
SR_DQ_LEAK_MAX_LEVELS <- 50L
SR_DQ_LEAK_SKIP_ROLES <- c("outcome", "id", "cluster", "time_zero", "event_date", "censor_date", "time", "event",
                           "ignore")
# The exposure roles are not candidates while blind: "the treated arm has no deaths" is an outcome-by-exposure
# statistic. Once the SAP is locked they are checked like any other column.
SR_DQ_LEAK_BLIND_SKIP_ROLES <- c("exposure", "exposure_date")
SR_DQ_POD_SHARE <- 0.5
SR_DQ_POD_MIN_N <- 10L

# ---- statistics -------------------------------------------------------------------------------------

# Rank-based AUC (Mann-Whitney U / (n1 * n0), ties count half) of `x` for the 0/1 outcome `y`, over the rows
# where both are present. NA when there are fewer than `min_n` events or non-events, or x is constant.
.sr_dq_auc <- function(x, y, min_n = SR_DQ_LEAK_MIN_N) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]
  y <- y[ok]
  n1 <- sum(y == 1L)
  n0 <- sum(y == 0L)
  if (n1 < min_n || n0 < min_n || length(unique(x)) < 2L) return(NA_real_)
  r <- rank(x)
  (sum(r[y == 1L]) - n1 * (n1 + 1) / 2) / (as.numeric(n1) * as.numeric(n0))
}

# AUC of a categorical column: each row scored by the outcome rate of its level, over the levels with at
# least `min_level` rows. NA when fewer than two levels remain or there are too many levels.
.sr_dq_cat_auc <- function(x, y, min_level = SR_DQ_LEAK_MIN_LEVEL, max_levels = SR_DQ_LEAK_MAX_LEVELS) {
  ok <- !is.na(x) & !is.na(y)
  x <- as.character(x[ok])
  y <- y[ok]
  if (length(unique(x)) > max_levels) return(NA_real_)
  cnt <- table(x)
  keep <- names(cnt)[cnt >= min_level]
  if (length(keep) < 2L) return(NA_real_)
  k <- x %in% keep
  rate <- tapply(y[k], x[k], mean)
  .sr_dq_auc(unname(rate[x[k]]), y[k])
}

# ---- the outcome and the candidates -----------------------------------------------------------------

# The binary outcome over the sample rows: list(col, y (0/1, NA where missing), values (the two original
# values, y = 0 then y = 1)), or NULL when there is not exactly one outcome column with exactly two values.
.sr_dq_outcome_binary <- function(ctx) {
  oc <- sr_role_cols(ctx$project, ctx$table, "outcome")
  if (length(oc) != 1L || !(oc %in% ctx$profile$columns$column)) return(NULL)
  v <- .sr_dq_sample_col(ctx, oc)
  u <- sort(unique(v[!is.na(v)]))
  if (length(u) != 2L) return(NULL)
  list(col = oc, y = ifelse(is.na(v), NA_integer_, as.integer(v == u[2L])), values = as.character(u))
}

# The columns of the sample that may be tested against the outcome.
.sr_dq_leak_cands <- function(ctx, oc) {
  skip_roles <- c(SR_DQ_LEAK_SKIP_ROLES, if (isTRUE(ctx$blind)) SR_DQ_LEAK_BLIND_SKIP_ROLES)
  skip <- names(ctx$roles)[ctx$roles %in% skip_roles]
  setdiff(names(ctx$sample), c(oc, skip))
}

# One candidate as a number and as a category (each NULL when it does not apply): dates and logicals and
# numbers are numeric; text that is >= 95% numbers is numeric; other text and logicals are categorical.
.sr_dq_leak_view <- function(ctx, col) {
  x <- ctx$sample[[col]]
  if (inherits(x, c("Date", "POSIXct"))) return(list(num = as.numeric(x), cat = NULL))
  if (is.logical(x)) return(list(num = as.integer(x), cat = as.character(x)))
  if (is.numeric(x)) return(list(num = as.numeric(x), cat = NULL))
  if (is.character(x) || is.factor(x)) {
    pn <- ctx$profile$columns$p_numeric_parsable[match(col, ctx$profile$columns$column)]
    if (isTRUE(pn >= SR_DQ_COERCE_MIN)) return(list(num = suppressWarnings(as.numeric(as.character(x))), cat = NULL))
    return(list(num = NULL, cat = as.character(x)))
  }
  list(num = NULL, cat = NULL)
}

.sr_dq_nonmissing <- function(ctx, col) {
  pc <- ctx$profile$columns
  j <- match(col, pc$column)
  pc$n[j] - pc$n_missing[j]
}

# ---- leakage ----------------------------------------------------------------------------------------

.sr_dq_check_leakage <- function(ctx) {
  oc <- .sr_dq_outcome_binary(ctx)
  if (is.null(oc)) return(sr_findings_empty())
  n <- ctx$profile$rows
  out <- list()
  for (col in .sr_dq_leak_cands(ctx, oc$col)) {
    v <- .sr_dq_leak_view(ctx, col)
    auc <- if (!is.null(v$num)) .sr_dq_auc(v$num, oc$y) else if (!is.null(v$cat)) .sr_dq_cat_auc(v$cat, oc$y) else NA_real_
    if (is.na(auc) || max(auc, 1 - auc) < SR_DQ_LEAK_AUC) next
    a <- max(auc, 1 - auc)
    out[[length(out) + 1L]] <- if (ctx$blind) {
      .sr_dq_finding("leakage", "leakage", col, "block", NA_integer_, NA_real_,
                     sprintf("%s is near-deterministic for the outcome", col))
    } else {
      nn <- .sr_dq_nonmissing(ctx, col)
      .sr_dq_finding("leakage", "leakage", col, "block", nn, nn / n,
                     sprintf("%s is near-deterministic for the outcome (AUC %.3f)", col, a),
                     detail = list(auc = data.frame(column = col, auc = a, stringsAsFactors = FALSE)))
    }
  }
  .sr_dq_bind(out)
}

# ---- separation -------------------------------------------------------------------------------------

.sr_dq_check_separation <- function(ctx) {
  oc <- .sr_dq_outcome_binary(ctx)
  if (is.null(oc)) return(sr_findings_empty())
  n <- ctx$profile$rows
  out <- list()
  for (col in .sr_dq_leak_cands(ctx, oc$col)) {
    x <- .sr_dq_leak_view(ctx, col)$cat
    if (is.null(x)) next
    ok <- !is.na(x) & !is.na(oc$y)
    if (length(unique(x[ok])) < 2L || length(unique(x[ok])) > SR_DQ_LEAK_MAX_LEVELS) next
    cnt <- c(table(x[ok]))
    ones <- tapply(oc$y[ok], x[ok], sum)
    ones <- stats::setNames(as.numeric(ones), names(ones))
    big <- names(cnt)[cnt >= SR_DQ_LEAK_MIN_LEVEL]
    sep <- big[ones[big] == 0 | ones[big] == cnt[big]]
    if (!length(sep)) next
    if (ctx$blind) {
      out[[length(out) + 1L]] <- .sr_dq_finding(
        "separation", "leakage", col, "warn", NA_integer_, NA_real_,
        sprintf("%s has a level in which the outcome never varies (perfect separation)", col))
      next
    }
    lv <- data.frame(column = col, level = sep, n = as.numeric(cnt[sep]),
                     outcome = oc$values[1L + as.integer(ones[sep] > 0)], stringsAsFactors = FALSE)
    lv <- lv[order(-lv$n, lv$level), , drop = FALSE]
    k <- sum(lv$n)
    where <- if (nrow(lv) <= SR_DQ_WHERE_IN_MAX) .sr_where_leaf(col, "in", lv$level) else NULL
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "separation", "leakage", col, "warn", k, k / n,
      sprintf("%s has %d level(s) with at least %d rows in which the outcome never varies (%s): a regression on it separates",
              col, nrow(lv), SR_DQ_LEAK_MIN_LEVEL, paste(utils::head(lv$level, 3L), collapse = ", ")),
      where = where, examples = lv$level, detail = list(levels = lv), id_where = NULL)   # same id blind and unblinded
  }
  .sr_dq_bind(out)
}

# ---- post_outcome_date ------------------------------------------------------------------------------

.sr_dq_check_post_outcome_date <- function(ctx) {
  dc <- .sr_pl_pairable(ctx)
  if (nrow(dc) < 2L) return(sr_findings_empty())
  ev <- .sr_pl_by_role(ctx, dc, "event_date")
  if (!length(ev)) ev <- .sr_pl_by_name(dc, "death|died|event|outcome")
  if (!length(ev)) return(sr_findings_empty())
  skip <- c(ev, .sr_pl_by_role(ctx, dc, "censor_date"))
  cand <- setdiff(dc$column, skip)
  if (!length(cand)) return(sr_findings_empty())
  expr <- function(col) dc$expr[match(col, dc$column)]
  pairs <- list()
  for (d in cand) for (e in ev) pairs[[length(pairs) + 1L]] <- c(d, e)
  preds <- unlist(lapply(pairs, function(p) {
    c(sprintf("%s IS NOT NULL AND %s IS NOT NULL", expr(p[1L]), expr(p[2L])), sprintf("%s > %s", expr(p[1L]), expr(p[2L])))
  }))
  counts <- .sr_dq_count(ctx, preds)
  n <- ctx$profile$rows
  out <- list()
  for (i in seq_along(pairs)) {
    both <- counts[2L * i - 1L]
    k <- counts[2L * i]
    if (both < SR_DQ_POD_MIN_N || k / both <= SR_DQ_POD_SHARE) next
    d <- pairs[[i]][1L]
    e <- pairs[[i]][2L]
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "post_outcome_date", "leakage", d, "warn", k, k / n,
      sprintf(paste0("%s is after %s in %s of %s rows with both (%s): it is recorded after the outcome, so it cannot be ",
                     "a baseline predictor of it"), d, e, .sr_dq_big(k), .sr_dq_big(both), .sr_dq_pct(k / both)),
      salt = e,
      detail = list(after = data.frame(column = d, event_date = e, n_both = both, n_after = k, p_after = k / both,
                                       stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

.sr_dq_register("leakage", "leakage", .sr_dq_check_leakage)
.sr_dq_register("separation", "leakage", .sr_dq_check_separation)
.sr_dq_register("post_outcome_date", "leakage", .sr_dq_check_post_outcome_date)
