# profile/dq_completeness.R - completeness checks: what is missing, in which rows, and why.
#   missing_col         a column with >= 5% missing (info), >= 30% (warn), block when the role is protected
#   missing_row         rows with more than half of their fields missing
#   missing_codes       sentinel values (-99, 999, "N/A", ...) that stand in for "missing"
#   missing_pattern     the ten most common joint missingness patterns, from the sample (info)
#   missing_dependence  what the missingness of a column depends on: the largest standardised mean
#                       difference of the other columns between its missing and observed rows
# Every threshold is a share or an effect size, never a p-value, so a large table does not make a
# check fire. p_affected is a share of ALL rows. Counts come from SQL over the full table; only the
# robust range of missing_codes, the patterns and the SMDs use the reproducible sample.
#
# Outcome-blind: while sr_blind(), the sample has no outcome column, so no pattern or SMD can involve
# one. The two uses of an outcome column are marginal: missing_col (its own p_missing) and
# missing_dependence for an outcome with >= 5% missing, which reads ONLY is.na(outcome) from the sample
# file and never a value. missing_row leaves a blind outcome out of the field count; missing_codes
# skips it (its robust range would need the outcome's values).

SR_DQ_MISSING_COL_INFO <- 0.05
SR_DQ_MISSING_COL_WARN <- 0.30
SR_DQ_MISSING_ROW_FRAC <- 0.5
SR_DQ_CODES_MIN_SHARE <- 0.005
SR_DQ_CODES_NUM <- c(-99, -999, -9999, 999, 9999, -1, -9)
SR_DQ_CODES_TXT_MIN_SHARE <- 0.001
SR_DQ_CODES_TXT <- c("NA", "N/A", ".", "-", "?", "unknown", "missing", "")
SR_DQ_PATTERN_TOP <- 10L
SR_DQ_DEP_MIN_P <- 0.05
SR_DQ_DEP_MIN_GROUP <- 30L
SR_DQ_DEP_MAX_LEVELS <- 20L
SR_DQ_SMD_INFO <- 0.2
SR_DQ_SMD_WARN <- 0.5
SR_DQ_SMD_CAP <- 10

# Outcome columns that may not be looked into beyond their own marginals (empty once the SAP is locked).
.sr_dq_blind_outcome <- function(ctx) {
  if (!isTRUE(ctx$blind)) return(character())
  sr_role_cols(ctx$project, ctx$table, "outcome")
}

.sr_dq_pct <- function(x) sprintf("%.1f%%", 100 * x)
.sr_dq_big <- function(x) format(x, big.mark = ",", scientific = FALSE)

# ---- missing_col ------------------------------------------------------------------------------------

.sr_dq_check_missing_col <- function(ctx) {
  pc <- ctx$profile$columns
  out <- list()
  for (i in seq_len(nrow(pc))) {
    p <- pc$p_missing[i]
    if (is.na(p) || p < SR_DQ_MISSING_COL_INFO) next
    col <- pc$column[i]
    role <- unname(ctx$roles[col])
    sev <- "info"
    if (p >= SR_DQ_MISSING_COL_WARN) sev <- if (!is.na(role) && role %in% SR_PROTECTED_ROLES) "block" else "warn"
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "missing_col", "completeness", col, sev, pc$n_missing[i], p,
      sprintf("%s is missing in %s of rows (%s)%s", col, .sr_dq_pct(p), .sr_dq_big(pc$n_missing[i]),
              if (identical(sev, "block")) sprintf("; its role (%s) cannot be analysed with this much missing", role) else ""),
      where = .sr_where_leaf(col, "is_na"))
  }
  .sr_dq_bind(out)
}

# ---- missing_row ------------------------------------------------------------------------------------

.sr_dq_check_missing_row <- function(ctx) {
  cols <- setdiff(ctx$profile$columns$column, .sr_dq_blind_outcome(ctx))
  k <- length(cols)
  n <- ctx$profile$rows
  if (k < 1L || n < 1) return(sr_findings_empty())
  q <- DBI::dbQuoteIdentifier(ctx$con, cols)
  miss <- paste(sprintf("CASE WHEN %s IS NULL THEN 1 ELSE 0 END", q), collapse = " + ")
  m <- .sr_dq_count(ctx, sprintf("(%s) > %s", miss, .sr_dq_num(SR_DQ_MISSING_ROW_FRAC * k)))
  if (m < 1) return(sr_findings_empty())
  p <- m / n
  .sr_dq_finding("missing_row", "completeness", "*", if (p < 0.01) "info" else "warn", m, p,
                 sprintf("%s row(s) (%s) have more than half of their %d fields missing", .sr_dq_big(m), .sr_dq_pct(p), k))
}

# ---- missing_codes ----------------------------------------------------------------------------------

# The dictionary's missing codes of one column (character, possibly empty).
.sr_dq_dict_codes <- function(ctx, col) {
  d <- ctx$dict
  if (is.null(d) || !nrow(d)) return(character())
  i <- match(col, d$variable)
  if (is.na(i)) return(character())
  x <- as.character(unlist(d$missing_codes[[i]], use.names = FALSE))
  x[!is.na(x)]
}

# Is each candidate code outside the robust range (median +- 10 MAD) of the column's sample values
# (the candidate codes themselves left out)? With MAD == 0 any value other than the median is outside.
.sr_dq_outside <- function(x, codes) {
  x <- as.numeric(x)
  x <- x[!is.na(x) & !(x %in% codes)]
  if (!length(x)) return(rep(TRUE, length(codes)))
  med <- stats::median(x)
  s <- stats::mad(x)
  if (s == 0) codes != med else abs(codes - med) > 10 * s
}

.sr_dq_check_missing_codes_num <- function(ctx, skip) {
  pc <- ctx$profile$columns
  n <- ctx$profile$rows
  cols <- pc$column[vapply(pc$type, .sr_prof_is_numeric, logical(1)) & !(pc$column %in% skip)]
  cand <- list()
  preds <- character()
  for (col in cols) {
    dc <- suppressWarnings(as.numeric(.sr_dq_dict_codes(ctx, col)))
    codes <- unique(c(SR_DQ_CODES_NUM, dc[is.finite(dc)]))
    cand[[col]] <- codes
    q <- DBI::dbQuoteIdentifier(ctx$con, col)
    preds <- c(preds, sprintf("CAST(%s AS DOUBLE) = %s", q, vapply(codes, .sr_dq_num, "")))
  }
  if (!length(preds)) return(sr_findings_empty())
  counts <- .sr_dq_count(ctx, preds)
  pos <- 0L
  out <- list()
  for (col in cols) {
    codes <- cand[[col]]
    cnt <- counts[pos + seq_along(codes)]
    pos <- pos + length(codes)
    i <- match(col, pc$column)
    nm <- pc$n[i] - pc$n_missing[i]
    if (nm < 1 || !(col %in% names(ctx$sample))) next
    hit <- cnt > 0 & cnt / nm >= SR_DQ_CODES_MIN_SHARE
    if (!any(hit)) next
    hit[hit] <- .sr_dq_outside(ctx$sample[[col]], codes)[hit]
    if (!any(hit)) next
    # the robust range ignores every candidate code, so each hit is judged against the same range
    hit_codes <- codes[hit]
    hit_n <- cnt[hit]
    ord <- order(-hit_n, hit_codes)
    hit_codes <- hit_codes[ord]
    hit_n <- hit_n[ord]
    where <- if (length(hit_codes) == 1L) .sr_where_leaf(col, "==", hit_codes) else .sr_where_leaf(col, "in", hit_codes)
    k <- sum(hit_n)
    lab <- vapply(hit_codes, function(x) format(x, scientific = FALSE, trim = TRUE), "")
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "missing_codes", "completeness", col, "warn", k, k / n,
      sprintf("%s holds %s value(s) (%s of its observed values) that look like missing-value codes: %s", col,
              .sr_dq_big(k), .sr_dq_pct(k / nm), paste(lab, collapse = ", ")),
      where = where, examples = lab,
      detail = list(codes = data.frame(column = col, code = hit_codes, n = hit_n, stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

.sr_dq_check_missing_codes_txt <- function(ctx, skip) {
  pc <- ctx$profile$columns
  n <- ctx$profile$rows
  con <- ctx$con
  cols <- pc$column[vapply(pc$type, .sr_prof_is_varchar, logical(1)) & !(pc$column %in% skip)]
  if (!length(cols)) return(sr_findings_empty())
  lits <- lapply(cols, function(col) {
    codes <- unique(tolower(trimws(c(SR_DQ_CODES_TXT, .sr_dq_dict_codes(ctx, col)))))
    paste(DBI::dbQuoteString(con, codes), collapse = ", ")
  })
  expr <- function(col) sprintf("lower(trim(%s))", DBI::dbQuoteIdentifier(con, col))
  preds <- vapply(seq_along(cols), function(i) sprintf("%s IN (%s)", expr(cols[i]), lits[[i]]), "")
  counts <- .sr_dq_count(ctx, preds)
  out <- list()
  for (i in seq_along(cols)) {
    col <- cols[i]
    j <- match(col, pc$column)
    nm <- pc$n[j] - pc$n_missing[j]
    if (nm < 1 || counts[i] < 1 || counts[i] / nm < SR_DQ_CODES_TXT_MIN_SHARE) next
    q <- DBI::dbQuoteIdentifier(con, col)
    ev <- .sr_dq_values(ctx, q, preds[i], limit = SR_DQ_WHERE_IN_MAX + 1L)
    where <- if (nrow(ev) <= SR_DQ_WHERE_IN_MAX) .sr_where_leaf(col, "in", ev$value) else NULL
    ex <- utils::head(ev$value, SR_DQ_EXAMPLES_N)
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "missing_codes", "completeness", col, "warn", counts[i], counts[i] / n,
      sprintf("%s holds %s text value(s) (%s of its observed values) that look like missing-value codes: %s", col,
              .sr_dq_big(counts[i]), .sr_dq_pct(counts[i] / nm), paste(sprintf("'%s'", ex), collapse = ", ")),
      where = where, examples = ex,
      detail = list(codes = data.frame(column = col, code = ev$value, n = ev$n, stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

.sr_dq_check_missing_codes <- function(ctx) {
  skip <- .sr_dq_blind_outcome(ctx)
  .sr_dq_bind(list(.sr_dq_check_missing_codes_num(ctx, skip), .sr_dq_check_missing_codes_txt(ctx, skip)))
}

# ---- missing_pattern --------------------------------------------------------------------------------

.sr_dq_check_missing_pattern <- function(ctx) {
  s <- ctx$sample
  if (!nrow(s)) return(sr_findings_empty())
  has <- names(s)[vapply(s, function(v) anyNA(v), logical(1))]
  if (!length(has)) return(sr_findings_empty())
  flags <- lapply(has, function(col) ifelse(is.na(s[[col]]), "1", "0"))
  key <- do.call(paste0, flags)
  tab <- table(key)
  tab <- tab[order(-as.integer(tab), names(tab))]
  top <- utils::head(tab, SR_DQ_PATTERN_TOP)
  lab <- vapply(names(top), function(k) {
    on <- has[strsplit(k, "", fixed = TRUE)[[1L]] == "1"]
    if (length(on)) paste(on, collapse = ", ") else "(complete)"
  }, "")
  n_s <- nrow(s)
  incomplete <- sum(rowSums(is.na(s[, has, drop = FALSE])) > 0L)
  .sr_dq_finding(
    "missing_pattern", "completeness", "*", "info", incomplete, incomplete / n_s,
    sprintf("%d joint missingness pattern(s) in the sample of %s rows; the commonest: %s", length(tab), .sr_dq_big(n_s),
            paste(sprintf("%s (%s)", lab[seq_len(min(3L, length(lab)))], .sr_dq_pct(as.integer(top)[seq_len(min(3L, length(lab)))] / n_s)),
                  collapse = "; ")),
    detail = list(patterns = data.frame(column = "*", rank = seq_along(top), n = as.integer(top),
                                        p = as.integer(top) / n_s, missing_columns = unname(lab),
                                        stringsAsFactors = FALSE)))
}

# ---- missing_dependence -----------------------------------------------------------------------------

# The values of one column over the sample rows, in the order of ctx$sample. This is the ONE internal
# reader of a column the blind sample dropped (an outcome): it is read from the sample file with the same
# ORDER BY as sr_sample() (the kept columns), so row i of the result belongs to row i of ctx$sample. Rows
# tied on every kept column are identical in ctx$sample, so any order among them gives the same
# (row, value) pairs. The values never leave the check function that asked for them; a check may turn them
# into a marginal (is.na, a flag) or, for leakage and separation, into a statistic it reports under the
# blind rules of those checks.
.sr_dq_sample_col <- function(ctx, col) {
  s <- ctx$sample
  if (col %in% names(s)) return(s[[col]])
  con <- ctx$con
  keep <- DBI::dbQuoteIdentifier(con, names(s))
  r <- sr_duck_query(con, sprintf("SELECT %s AS sr_dq_v FROM read_parquet(%s) ORDER BY %s",
                                  DBI::dbQuoteIdentifier(con, col), DBI::dbQuoteString(con, ctx$profile$sample_file),
                                  paste(keep, collapse = ", ")))
  if (nrow(r) != nrow(s)) stop("the sample file and the sample do not have the same rows", call. = FALSE)
  r$sr_dq_v
}

# The missingness indicator of `col` over the sample rows, and the sample's other columns. A column the
# blind sample dropped (an outcome) contributes ONLY its is.na() flag.
.sr_dq_miss_frame <- function(ctx, col) {
  s <- ctx$sample
  list(m = is.na(.sr_dq_sample_col(ctx, col)), cov = s[, setdiff(names(s), col), drop = FALSE])
}

# |standardised difference| of one covariate between the rows where `m` is TRUE and FALSE: for a numeric
# covariate (dates and mostly-numeric text count as numeric) the difference of means over the pooled SD,
# for a categorical one with <= 20 levels the largest per-level difference of proportions over the pooled
# SD of a proportion. NA when it cannot be computed. Capped at SR_DQ_SMD_CAP.
.sr_dq_smd <- function(x, m) {
  if (inherits(x, c("Date", "POSIXct"))) x <- as.numeric(x)
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    num <- suppressWarnings(as.numeric(x))
    ok <- !is.na(x)
    if (any(ok) && all(!is.na(num[ok]))) x <- num
  }
  if (is.logical(x)) x <- as.integer(x)
  pooled <- function(d, v) {
    if (!is.finite(v) || v <= 0) return(if (isTRUE(abs(d) > 1e-12)) SR_DQ_SMD_CAP else 0)
    min(abs(d) / sqrt(v), SR_DQ_SMD_CAP)
  }
  if (is.numeric(x)) {
    a <- x[m & is.finite(x)]
    b <- x[!m & is.finite(x)]
    if (length(a) < 2L || length(b) < 2L) return(NA_real_)
    return(pooled(mean(a) - mean(b), (stats::var(a) + stats::var(b)) / 2))
  }
  levs <- unique(x[!is.na(x)])
  if (length(levs) < 2L || length(levs) > SR_DQ_DEP_MAX_LEVELS) return(NA_real_)
  a <- x[m & !is.na(x)]
  b <- x[!m & !is.na(x)]
  if (length(a) < 2L || length(b) < 2L) return(NA_real_)
  max(vapply(levs, function(l) {
    pa <- mean(a == l)
    pb <- mean(b == l)
    pooled(pa - pb, (pa * (1 - pa) + pb * (1 - pb)) / 2)
  }, numeric(1)))
}

.sr_dq_check_missing_dependence <- function(ctx) {
  pc <- ctx$profile$columns
  out <- list()
  skip_cov <- names(ctx$roles)[ctx$roles %in% c("id", "ignore")]
  for (i in seq_len(nrow(pc))) {
    p <- pc$p_missing[i]
    if (is.na(p) || p < SR_DQ_DEP_MIN_P) next
    col <- pc$column[i]
    fr <- .sr_dq_miss_frame(ctx, col)
    if (min(sum(fr$m), sum(!fr$m)) < SR_DQ_DEP_MIN_GROUP) next
    cov <- fr$cov[, setdiff(names(fr$cov), skip_cov), drop = FALSE]
    if (!ncol(cov)) next
    smd <- vapply(cov, .sr_dq_smd, numeric(1), m = fr$m)
    smd <- smd[!is.na(smd)]
    if (!length(smd) || max(smd) < SR_DQ_SMD_INFO) next
    ord <- order(-smd, names(smd))
    top <- utils::head(smd[ord], SR_DQ_EXAMPLES_N)
    sev <- if (max(smd) >= SR_DQ_SMD_WARN) "warn" else "info"
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "missing_dependence", "completeness", col, sev, pc$n_missing[i], p,
      sprintf("whether %s is missing depends on %s (largest standardised difference %.2f between its missing and observed rows)",
              col, paste(utils::head(names(top), 3L), collapse = ", "), max(smd)),
      where = .sr_where_leaf(col, "is_na"),
      detail = list(dependence = data.frame(column = col, covariate = names(top), smd = unname(top),
                                            stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

.sr_dq_register("missing_col", "completeness", .sr_dq_check_missing_col)
.sr_dq_register("missing_row", "completeness", .sr_dq_check_missing_row)
.sr_dq_register("missing_codes", "completeness", .sr_dq_check_missing_codes)
.sr_dq_register("missing_pattern", "completeness", .sr_dq_check_missing_pattern)
.sr_dq_register("missing_dependence", "completeness", .sr_dq_check_missing_dependence)
