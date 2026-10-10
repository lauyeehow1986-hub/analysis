# profile/dq_structure.R - uniqueness and structure checks: duplicated rows and ids, columns that carry no
# information, redundant columns, and roles the data suggests.
#   dup_rows        exact duplicate rows (warn)
#   dup_id          an id-role value that repeats on rows that are NOT exact duplicates (block)
#   nzv             near-zero variance, the caret rule: most common / second most common > 95/5 AND fewer
#                   than 10% distinct values (info for a protected role, warn otherwise)
#   high_card       a text column with more than 50 distinct values, more than half of the rows (info)
#   collinear       numeric pairs with |Pearson r| >= 0.98 on the sample (warn)
#   cluster_detect  an id that repeats, or a cluster-named column of 2-200 groups of >= 5 rows (info)
#   offset_detect   a positive follow-up / person-time column (info)
# Counts (dup_rows, dup_id, nzv, high_card) are SQL aggregates over the FULL table, so n_affected is exact
# and p_affected is a share of all rows. collinear works on the reproducible sample (at most 200 columns).
# n_affected of a column-level finding is the column's non-missing rows. dup_rows counts the EXTRA copies
# (rows beyond the first of each duplicated row), not the rows that have a twin: a row that appears three
# times adds two. dup_id counts the same way for ids, over rows that are distinct as a whole.
#
# Outcome-blind: the sample has no outcome column while blind, so collinear never pairs the outcome with
# anything; nzv reads only the outcome's own marginals (the profile's top values), as missing_col does.
# dup_rows groups on every column, the outcome included, which is an exact-duplicate count and not a
# statistic that relates the outcome to another column. Details are data.frames with a `column` field.

SR_DQ_NZV_RATIO <- 95 / 5
SR_DQ_NZV_PCT_UNIQUE <- 0.10
SR_DQ_NZV_MIN_N <- 20L
SR_DQ_HIGH_CARD_MIN <- 50L
SR_DQ_HIGH_CARD_SHARE <- 0.5
SR_DQ_COLLINEAR_R <- 0.98
SR_DQ_COLLINEAR_MAX_COLS <- 200L
SR_DQ_COLLINEAR_MAX_PAIRS <- 50L
SR_DQ_CLUSTER_LEVELS <- c(2L, 200L)
SR_DQ_CLUSTER_MIN_ROWS <- 5L
SR_DQ_ID_REPEAT_SHARE <- 0.01

# ---- helpers ----------------------------------------------------------------------------------------

# The confirmed role of each column (NA when it has none).
.sr_st_role <- function(ctx, cols) unname(ctx$roles[cols])

# Does the column name, or its dictionary label, match the pattern (case-insensitive)? Same rule as
# sr_roles_suggest().
.sr_st_name_hit <- function(ctx, cols, re) {
  lb <- rep("", length(cols))
  d <- ctx$dict
  if (!is.null(d) && nrow(d)) {
    i <- match(cols, d$variable)
    ok <- !is.na(i)
    l <- tolower(as.character(d$label[i[ok]]))
    l[is.na(l)] <- ""
    lb[ok] <- l
  }
  grepl(re, tolower(cols), perl = TRUE) | grepl(re, lb, perl = TRUE)
}

# A filter on the values of an id column: numeric for a numeric column, text otherwise.
.sr_st_in_where <- function(ctx, col, values) {
  type <- ctx$profile$columns$type[match(col, ctx$profile$columns$column)]
  if (.sr_prof_is_numeric(type)) values <- suppressWarnings(as.numeric(values))
  .sr_where_leaf(col, "in", values)
}

# ---- dup_rows ---------------------------------------------------------------------------------------

.sr_dq_check_dup_rows <- function(ctx) {
  cols <- ctx$profile$columns$column
  n <- ctx$profile$rows
  if (!length(cols) || n < 2) return(sr_findings_empty())
  con <- ctx$con
  grp <- paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", ")
  r <- sr_duck_query(con, sprintf(paste0(
    "SELECT count(*) AS g, coalesce(sum(c), 0) AS tot, coalesce(max(c), 0) AS mx ",
    "FROM (SELECT count(*) AS c FROM %s GROUP BY %s HAVING count(*) > 1)"), .sr_dq_qv(ctx), grp))
  groups <- as.numeric(r$g[1L])
  if (groups < 1) return(sr_findings_empty())
  extra <- as.numeric(r$tot[1L]) - groups
  ex <- character()
  idc <- names(ctx$roles)[ctx$roles == "id"]
  if (length(idc)) {
    q <- DBI::dbQuoteIdentifier(con, idc[1L])
    e <- sr_duck_query(con, sprintf(paste0(
      "SELECT v FROM (SELECT CAST(min(%s) AS VARCHAR) AS v FROM %s GROUP BY %s HAVING count(*) > 1) ",
      "WHERE v IS NOT NULL ORDER BY v LIMIT %d"), q, .sr_dq_qv(ctx), grp, SR_DQ_EXAMPLES_N))
    ex <- as.character(e$v)
  }
  .sr_dq_finding(
    "dup_rows", "uniqueness", "*", "warn", extra, extra / n,
    sprintf("%s exact duplicate row(s) (%s of rows): %s distinct row(s) appear more than once, the most often %s times%s",
            .sr_dq_big(extra), .sr_dq_pct(extra / n), .sr_dq_big(groups), .sr_dq_big(as.numeric(r$mx[1L])),
            if (length(ex)) sprintf("; ids %s", paste(ex, collapse = ", ")) else ""),
    examples = ex,
    detail = list(duplicates = data.frame(column = "*", groups = groups, extra_rows = extra,
                                          max_copies = as.numeric(r$mx[1L]), stringsAsFactors = FALSE)))
}

# ---- dup_id -----------------------------------------------------------------------------------------

.sr_dq_check_dup_id <- function(ctx) {
  idc <- names(ctx$roles)[ctx$roles == "id"]
  idc <- idc[idc %in% ctx$profile$columns$column]
  if (!length(idc)) return(sr_findings_empty())
  con <- ctx$con
  n <- ctx$profile$rows
  # the rows as a whole, once each: an id that repeats on identical rows is dup_rows' business
  uniq <- sprintf("(SELECT DISTINCT * FROM %s)", .sr_dq_qv(ctx))
  out <- list()
  for (col in idc) {
    q <- DBI::dbQuoteIdentifier(con, col)
    r <- sr_duck_query(con, sprintf(paste0(
      "SELECT count(*) AS g, coalesce(sum(c - 1), 0) AS extra FROM ",
      "(SELECT count(*) AS c FROM %s WHERE %s IS NOT NULL GROUP BY %s HAVING count(*) > 1)"), uniq, q, q))
    groups <- as.numeric(r$g[1L])
    if (groups < 1) next
    extra <- as.numeric(r$extra[1L])
    v <- sr_duck_query(con, sprintf(paste0(
      "SELECT CAST(k AS VARCHAR) AS value, c AS n FROM (SELECT %s AS k, count(*) AS c FROM %s WHERE %s IS NOT NULL ",
      "GROUP BY 1 HAVING count(*) > 1) ORDER BY c DESC, value LIMIT %d"), q, uniq, q, SR_DQ_WHERE_IN_MAX + 1L))
    vals <- as.character(v$value)
    where <- if (length(vals) <= SR_DQ_WHERE_IN_MAX) .sr_st_in_where(ctx, col, vals) else NULL
    ex <- utils::head(vals, SR_DQ_EXAMPLES_N)
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "dup_id", "uniqueness", col, "block", extra, extra / n,
      sprintf(paste0("%s is an id but %s value(s) repeat on rows that differ (%s surplus row(s), %s of rows; e.g. %s). ",
                     "An id must identify one row: fix the data, or give %s the cluster role if the rows are repeated ",
                     "measures"), col, .sr_dq_big(groups), .sr_dq_big(extra), .sr_dq_pct(extra / n),
              paste(ex, collapse = ", "), col),
      where = where, examples = ex,
      detail = list(ids = data.frame(column = col, n_ids = groups, extra_rows = extra, stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

# ---- nzv --------------------------------------------------------------------------------------------

.sr_dq_check_nzv <- function(ctx) {
  pc <- ctx$profile$columns
  n <- ctx$profile$rows
  out <- list()
  for (i in seq_len(nrow(pc))) {
    nn <- pc$n[i] - pc$n_missing[i]
    tv <- pc$top_values[[i]]
    if (nn < SR_DQ_NZV_MIN_N || is.na(pc$n_distinct[i]) || !nrow(tv)) next
    top1 <- tv$n[1L]
    top2 <- if (nrow(tv) >= 2L) tv$n[2L] else 0
    ratio <- if (top2 > 0) top1 / top2 else Inf
    pct <- pc$n_distinct[i] / nn
    if (!(ratio > SR_DQ_NZV_RATIO && pct < SR_DQ_NZV_PCT_UNIQUE)) next
    col <- pc$column[i]
    role <- .sr_st_role(ctx, col)
    sev <- if (!is.na(role) && role %in% SR_PROTECTED_ROLES) "info" else "warn"
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "nzv", "structure", col, sev, nn, nn / n,
      if (is.infinite(ratio)) {
        sprintf("%s is constant: every non-missing value is '%s'", col, tv$value[1L])
      } else {
        sprintf("%s is almost constant: '%s' is %s of its non-missing values (most common / second most common = %.0f, %s distinct)",
                col, tv$value[1L], .sr_dq_pct(top1 / nn), ratio, .sr_dq_big(pc$n_distinct[i]))
      },
      examples = utils::head(tv$value, 2L),
      detail = list(variance = data.frame(column = col, freq_ratio = if (is.finite(ratio)) ratio else NA_real_,
                                         p_unique = pct, stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

# ---- high_card --------------------------------------------------------------------------------------

.sr_dq_check_high_card <- function(ctx) {
  pc <- ctx$profile$columns
  n <- ctx$profile$rows
  out <- list()
  for (i in seq_len(nrow(pc))) {
    col <- pc$column[i]
    if (!.sr_prof_is_varchar(pc$type[i])) next
    role <- .sr_st_role(ctx, col)
    if (!is.na(role) && role %in% c("id", "ignore")) next
    # numbers and dates stored as text are conformance's business (coercion)
    if (isTRUE(pc$p_numeric_parsable[i] >= SR_DQ_COERCE_MIN) || isTRUE(pc$p_date_parsable[i] >= SR_DQ_COERCE_MIN)) next
    k <- pc$n_distinct[i]
    if (is.na(k) || n < 1 || !(k > SR_DQ_HIGH_CARD_MIN && k / n > SR_DQ_HIGH_CARD_SHARE)) next
    nn <- pc$n[i] - pc$n_missing[i]
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "high_card", "structure", col, "info", nn, nn / n,
      sprintf(paste0("%s is text with %s distinct values in %s rows (%s): it cannot be a category. Treat it as ",
                     "free text, an identifier or a column to ignore"), col, .sr_dq_big(k), .sr_dq_big(n), .sr_dq_pct(k / n)),
      detail = list(cardinality = data.frame(column = col, n_distinct = k, p_unique = k / n, stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

# ---- collinear --------------------------------------------------------------------------------------

.sr_dq_check_collinear <- function(ctx) {
  s <- ctx$sample
  if (nrow(s) < 3L) return(sr_findings_empty())
  skip <- names(ctx$roles)[ctx$roles %in% c("id", "cluster", "ignore")]
  is_num <- vapply(s, function(v) (is.numeric(v) || is.logical(v)) && !inherits(v, c("Date", "POSIXct")), NA)
  cols <- setdiff(names(s)[is_num], skip)
  cols <- utils::head(cols, SR_DQ_COLLINEAR_MAX_COLS)
  m <- lapply(s[cols], as.numeric)
  varies <- vapply(m, function(v) sum(!is.na(v)) >= 3L && isTRUE(stats::sd(v, na.rm = TRUE) > 0), NA)
  cols <- cols[varies]
  if (length(cols) < 2L) return(sr_findings_empty())
  r <- suppressWarnings(stats::cor(do.call(cbind, m[cols]), use = "pairwise.complete.obs"))
  hit <- which(upper.tri(r) & !is.na(r) & abs(r) >= SR_DQ_COLLINEAR_R, arr.ind = TRUE)
  if (!nrow(hit)) return(sr_findings_empty())
  hr <- abs(r[hit])
  ord <- order(-hr, hit[, 1L], hit[, 2L])
  hit <- utils::head(hit[ord, , drop = FALSE], SR_DQ_COLLINEAR_MAX_PAIRS)
  pc <- ctx$profile$columns
  n <- ctx$profile$rows
  out <- list()
  for (k in seq_len(nrow(hit))) {
    a <- cols[hit[k, 1L]]
    b <- cols[hit[k, 2L]]
    rv <- r[hit[k, 1L], hit[k, 2L]]
    j <- match(b, pc$column)
    nn <- pc$n[j] - pc$n_missing[j]
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "collinear", "structure", b, "warn", nn, nn / n,
      sprintf("%s and %s are almost collinear (r = %.3f in the sample); keep one of them in a model", b, a, rv),
      salt = a,
      detail = list(pair = data.frame(column = b, other = a, r = rv, stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

# ---- cluster_detect ---------------------------------------------------------------------------------

.sr_dq_check_cluster_detect <- function(ctx) {
  if (any(ctx$roles == "cluster")) return(sr_findings_empty())
  pc <- ctx$profile$columns
  n <- ctx$profile$rows
  out <- list()
  # an id that repeats: repeated measures of one unit. A few repeats are dup_id's business.
  for (col in names(ctx$roles)[ctx$roles == "id"]) {
    i <- match(col, pc$column)
    if (is.na(i)) next
    nn <- pc$n[i] - pc$n_missing[i]
    if (nn < 1 || is.na(pc$n_distinct[i])) next
    extra <- nn - pc$n_distinct[i]
    if (extra / nn < SR_DQ_ID_REPEAT_SHARE) next
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "cluster_detect", "structure", col, "info", nn, nn / n,
      sprintf(paste0("%s repeats: %s distinct values over %s rows (about %.1f rows each). If the rows are repeated ",
                     "measures of one unit, give %s the cluster role so the standard errors allow for it"), col,
              .sr_dq_big(pc$n_distinct[i]), .sr_dq_big(nn), nn / pc$n_distinct[i], col),
      detail = list(cluster = data.frame(column = col, n_groups = pc$n_distinct[i], mean_rows = nn / pc$n_distinct[i],
                                         stringsAsFactors = FALSE)))
  }
  # a cluster-named column with 2-200 groups of at least 5 rows
  role <- .sr_st_role(ctx, pc$column)
  simple <- vapply(pc$type, function(t) .sr_prof_is_numeric(t) || .sr_prof_is_varchar(t), NA)
  ok <- simple & (is.na(role) | role == "covariate") & !is.na(pc$n_distinct) &
    pc$n_distinct >= SR_DQ_CLUSTER_LEVELS[1L] & pc$n_distinct <= SR_DQ_CLUSTER_LEVELS[2L] &
    .sr_st_name_hit(ctx, pc$column, SR_RE_CLUSTER)
  for (i in which(ok)) {
    col <- pc$column[i]
    q <- DBI::dbQuoteIdentifier(ctx$con, col)
    r <- sr_duck_query(ctx$con, sprintf(
      "SELECT min(c) AS m FROM (SELECT count(*) AS c FROM %s WHERE %s IS NOT NULL GROUP BY %s)", .sr_dq_qv(ctx), q, q))
    if (is.na(r$m[1L]) || r$m[1L] < SR_DQ_CLUSTER_MIN_ROWS) next
    nn <- pc$n[i] - pc$n_missing[i]
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "cluster_detect", "structure", col, "info", nn, nn / n,
      sprintf(paste0("%s looks like a clustering variable: %s groups of at least %s rows. Give it the cluster role if ",
                     "rows within a group are not independent"), col, .sr_dq_big(pc$n_distinct[i]),
              .sr_dq_big(as.numeric(r$m[1L]))),
      detail = list(cluster = data.frame(column = col, n_groups = pc$n_distinct[i], min_rows = as.numeric(r$m[1L]),
                                         stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

# ---- offset_detect ----------------------------------------------------------------------------------

.sr_dq_check_offset_detect <- function(ctx) {
  pc <- ctx$profile$columns
  n <- ctx$profile$rows
  role <- .sr_st_role(ctx, pc$column)
  mn <- suppressWarnings(as.numeric(pc$min))
  ok <- vapply(pc$type, .sr_prof_is_numeric, NA) & (is.na(role) | role == "covariate") & !is.na(mn) & mn > 0 &
    .sr_st_name_hit(ctx, pc$column, SR_RE_OFFSET)
  out <- list()
  for (i in which(ok)) {
    col <- pc$column[i]
    nn <- pc$n[i] - pc$n_missing[i]
    out[[length(out) + 1L]] <- .sr_dq_finding(
      "offset_detect", "structure", col, "info", nn, nn / n,
      sprintf(paste0("%s is positive and named like follow-up or person-time: give it the offset role for a rate ",
                     "model (its log is the offset)"), col),
      detail = list(offset = data.frame(column = col, min = mn[i], stringsAsFactors = FALSE)))
  }
  .sr_dq_bind(out)
}

.sr_dq_register("dup_rows", "uniqueness", .sr_dq_check_dup_rows)
.sr_dq_register("dup_id", "uniqueness", .sr_dq_check_dup_id)
.sr_dq_register("nzv", "structure", .sr_dq_check_nzv)
.sr_dq_register("high_card", "structure", .sr_dq_check_high_card)
.sr_dq_register("collinear", "structure", .sr_dq_check_collinear)
.sr_dq_register("cluster_detect", "structure", .sr_dq_check_cluster_detect)
.sr_dq_register("offset_detect", "structure", .sr_dq_check_offset_detect)
