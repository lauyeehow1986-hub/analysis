# profile/profile.R - out-of-core column profiles, a reproducible sample, and the memory estimator.
#
# sr_profile_table() profiles a store table with SQL aggregates over the FULL table: every scalar
# statistic of every column comes from ONE aggregate query, plus one small GROUP BY per column for its
# ten most frequent values. Nothing is pulled into R but the answers. The result is cached as
# <data>/profile/<table>-<store hash, 12 chars>.json and reused only when the store hash, the
# exact-vs-approximate decision and the set of redacted outcome columns all match.
#
# Outcome-blind rule: while sr_blind(project) is TRUE the columns with role `outcome` get type, n,
# n_missing, p_missing, min, max, n_distinct and top_values only (completeness and plausibility
# marginals). Their mean, sd, quantiles and string / parse statistics are not even computed; they stay
# NA. The cache records which columns were redacted (`redacted_cols`), so a cache made before the roles
# were set, or before the SAP was locked, is never reused across that change.
#
# The reproducible sample is DuckDB `USING SAMPLE reservoir(n ROWS) REPEATABLE (seed)`. REPEATABLE is
# deterministic only on a single thread, and the application connection is configuration-locked (its
# thread count cannot be changed), so the sample runs on its own short-lived single-threaded connection
# with the same lockdown. The rows are written ORDER BY every column, so the Parquet file is byte-stable
# for one seed.

SR_PROFILE_QUANTILES <- c(q01 = 0.01, q25 = 0.25, q50 = 0.50, q75 = 0.75, q99 = 0.99)
SR_PROFILE_SAMPLE_N <- 100000L
SR_PROFILE_TOP_N <- 10L

# ---- types ------------------------------------------------------------------------------------------

.sr_prof_is_numeric <- function(type) {
  grepl("^(U?(TINYINT|SMALLINT|INTEGER|BIGINT|HUGEINT)|FLOAT|DOUBLE|DECIMAL)([(]|$)", toupper(type))
}

# Types that min / max / n_distinct / top_values are computed for (everything a store holds from a
# tabular source); anything else (LIST, STRUCT, BLOB, ...) gets only n and n_missing.
.sr_prof_is_simple <- function(type) {
  .sr_prof_is_numeric(type) || grepl("^(VARCHAR|BOOLEAN|DATE|TIME|TIMESTAMP)", toupper(type))
}

.sr_prof_is_varchar <- function(type) identical(toupper(type), "VARCHAR")

# ---- paths ------------------------------------------------------------------------------------------

.sr_prof_dir <- function(project, what) {
  d <- file.path(sr_project_paths(project$paths$dir)$data, what)
  if (!dir.exists(d) && !isTRUE(dir.create(d, recursive = TRUE, showWarnings = FALSE))) {
    stop("cannot create ", what, " directory: ", d, call. = FALSE)
  }
  d
}

.sr_prof_cache_path <- function(project, table, hash) {
  file.path(.sr_prof_dir(project, "profile"), sprintf("%s-%s.json", table, substr(hash, 1L, 12L)))
}

.sr_prof_sample_path <- function(project, table, hash, seed) {
  file.path(.sr_prof_dir(project, "sample"), sprintf("%s-%s-%d.parquet", table, substr(hash, 1L, 12L), seed))
}

# The table's part files (checked to exist), as normalised absolute paths.
.sr_prof_parts <- function(project, table, manifest) {
  files <- file.path(sr_store_dir(project, table), vapply(manifest$parts, function(p) p$file, ""))
  if (!length(files)) stop("table '", table, "' has no parts", call. = FALSE)
  gone <- !file.exists(files)
  if (any(gone)) {
    stop("missing part file(s) for table '", table, "': ", paste(basename(files[gone]), collapse = ", "),
         call. = FALSE)
  }
  normalizePath(files, winslash = "/", mustWork = TRUE)
}

.sr_prof_check_seed <- function(seed) {
  if (!.sr_whole_number(seed, 0, .Machine$integer.max)) {
    stop("seed must be a whole number between 0 and ", .Machine$integer.max, call. = FALSE)
  }
  as.integer(seed)
}

# ---- the reproducible sample ------------------------------------------------------------------------

# A fresh in-memory connection with the same lockdown as sr_duck(), ONE thread, its own spill directory
# (<data>/tmp/sample) and half the application connection's memory limit, so the two never share spill
# files or double the budget. Closed by the caller.
.sr_prof_single_con <- function(project) {
  sr_duck(project)   # makes sure the project-owned directories exist
  caps <- sr_profile_caps(project$meta$profile)
  caps$duckdb_threads <- 1L
  caps$duckdb_memory_gb <- caps$duckdb_memory_gb / 2
  dirs <- .sr_duck_dirs(project)
  dirs$tmp <- file.path(dirs$tmp, "sample")
  if (!dir.exists(dirs$tmp) && !isTRUE(dir.create(dirs$tmp, recursive = TRUE, showWarnings = FALSE))) {
    stop("cannot create DuckDB directory: ", dirs$tmp, call. = FALSE)
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  ok <- FALSE
  on.exit(if (!ok) try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  .sr_duck_setup(con, dirs, caps)
  ok <- TRUE
  con
}

# Number of rows of a Parquet file (NA when it cannot be read).
.sr_prof_file_rows <- function(con, file) {
  tryCatch(as.numeric(sr_duck_query(con, sprintf(
    "SELECT count(*) AS n FROM read_parquet(%s)", DBI::dbQuoteString(con, file)))$n[1L]),
    error = function(e) NA_real_)
}

# Make sure the sample file exists with min(sample_n, rows) rows; returns its absolute path. An existing
# file with the right row count is reused; anything else is regenerated (atomically).
.sr_prof_ensure_sample <- function(project, table, manifest, hash, sample_n, seed) {
  file <- .sr_prof_sample_path(project, table, hash, seed)
  want <- min(sample_n, manifest$rows)
  con <- .sr_prof_single_con(project)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  if (file.exists(file) && identical(.sr_prof_file_rows(con, file), as.numeric(want))) {
    return(normalizePath(file, winslash = "/", mustWork = TRUE))
  }
  parts <- .sr_prof_parts(project, table, manifest)
  src <- sprintf("SELECT * FROM read_parquet([%s])", paste(DBI::dbQuoteString(con, parts), collapse = ", "))
  if (sample_n < manifest$rows) {
    src <- sprintf("%s USING SAMPLE reservoir(%.0f ROWS) REPEATABLE (%d)", src, sample_n, seed)
  }
  cols <- vapply(manifest$schema, function(s) s$name, "")
  order_by <- paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", ")
  tmp <- paste0(file, ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  sr_duck_exec(con, sprintf("COPY (SELECT * FROM (%s) ORDER BY %s) TO %s (FORMAT parquet)",
                            src, order_by, DBI::dbQuoteString(con, normalizePath(tmp, winslash = "/", mustWork = FALSE))))
  if (!isTRUE(file.rename(tmp, file))) {
    unlink(file)
    if (!isTRUE(file.rename(tmp, file))) stop("cannot write the sample file", call. = FALSE)
  }
  normalizePath(file, winslash = "/", mustWork = TRUE)
}

#' The reproducible sample of a store table as a data.frame (rows ordered by every column). Reads the
#' sample file for this seed made by sr_profile_table(); when there is none, makes one with the default
#' sample size (100000 rows, or the whole table when it is smaller).
#' While sr_blind(project) the outcome-role columns are dropped from the returned frame (there is no
#' override). The sample parquet on disk keeps every column: it is an internal artefact and must not be
#' shown or exported to anyone before the SAP is locked; always go through sr_sample().
sr_sample <- function(project, table, seed = 1L) {
  seed <- .sr_prof_check_seed(seed)
  m <- sr_store_manifest(project, table)
  hash <- sr_store_hash(m)
  file <- .sr_prof_sample_path(project, table, hash, seed)
  if (!file.exists(file)) file <- .sr_prof_ensure_sample(project, table, m, hash, SR_PROFILE_SAMPLE_N, seed)
  con <- sr_duck(project)
  cols <- vapply(m$schema, function(s) s$name, "")
  keep <- cols
  if (sr_blind(project)) keep <- setdiff(cols, sr_role_cols(project, table, "outcome"))
  if (!length(keep)) stop("table '", table, "' has no column that may be shown before the SAP is locked", call. = FALSE)
  # ORDER BY every stored column (the dropped ones too) so the row order does not depend on the filter
  sr_duck_query(con, sprintf("SELECT %s FROM read_parquet(%s) ORDER BY %s",
                             paste(DBI::dbQuoteIdentifier(con, keep), collapse = ", "), DBI::dbQuoteString(con, file),
                             paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", ")))
}

# ---- the profile ------------------------------------------------------------------------------------

.sr_prof_num <- function(x) {
  x <- as.numeric(x)
  x[is.nan(x)] <- NA_real_
  x
}

# One aggregate query for every column of the view, then one top-values query per column.
.sr_prof_compute <- function(project, view, schema, redacted, exact) {
  con <- sr_duck(project)
  qv <- DBI::dbQuoteIdentifier(con, view)
  cols <- schema$name
  types <- schema$type
  k <- length(cols)
  sel <- "count(*) AS n_rows"
  add <- function(expr, alias) sel <<- c(sel, sprintf("%s AS %s", expr, DBI::dbQuoteIdentifier(con, alias)))
  for (i in seq_len(k)) {
    q <- DBI::dbQuoteIdentifier(con, cols[i])
    a <- function(s) paste0("c", i, "_", s)
    add(sprintf("count(%s)", q), a("nn"))
    if (!.sr_prof_is_simple(types[i])) next
    add(if (exact) sprintf("count(DISTINCT %s)", q) else sprintf("approx_count_distinct(%s)", q), a("nd"))
    add(sprintf("CAST(min(%s) AS VARCHAR)", q), a("min"))
    add(sprintf("CAST(max(%s) AS VARCHAR)", q), a("max"))
    if (cols[i] %in% redacted) next
    if (.sr_prof_is_numeric(types[i])) {
      d <- sprintf("CAST(%s AS DOUBLE)", q)
      add(sprintf("avg(%s)", d), a("mean"))
      add(sprintf("stddev_samp(%s)", d), a("sd"))
      for (nm in names(SR_PROFILE_QUANTILES)) {
        fn <- if (exact) "quantile_cont" else "approx_quantile"
        add(sprintf("%s(%s, %s)", fn, d, sprintf("%.2f", SR_PROFILE_QUANTILES[[nm]])), a(nm))
      }
    } else if (.sr_prof_is_varchar(types[i])) {
      add(sprintf("avg(length(%s))", q), a("nchar"))
      add(sprintf("count(TRY_CAST(%s AS DOUBLE))", q), a("pn"))
      add(sprintf("count(TRY_CAST(%s AS DATE))", q), a("pd"))
    }
  }
  r <- sr_duck_query(con, sprintf("SELECT %s FROM %s", paste(sel, collapse = ", "), qv))
  n <- as.numeric(r$n_rows[1L])
  get <- function(i, s) {
    nm <- paste0("c", i, "_", s)
    if (nm %in% names(r)) r[[nm]][1L] else NULL
  }
  num <- function(i, s) {
    v <- get(i, s)
    if (is.null(v)) NA_real_ else .sr_prof_num(v)
  }
  chr <- function(i, s) {
    v <- get(i, s)
    if (is.null(v)) NA_character_ else as.character(v)
  }
  nn <- vapply(seq_len(k), function(i) num(i, "nn"), numeric(1))
  ratio <- function(x, d) ifelse(!is.na(d) & d > 0, x / d, NA_real_)
  out <- data.frame(
    column = cols, type = types, n = rep(n, k), n_missing = n - nn, p_missing = ratio(n - nn, rep(n, k)),
    n_distinct = vapply(seq_len(k), function(i) num(i, "nd"), numeric(1)),
    min = vapply(seq_len(k), function(i) chr(i, "min"), ""),
    max = vapply(seq_len(k), function(i) chr(i, "max"), ""),
    mean = vapply(seq_len(k), function(i) num(i, "mean"), numeric(1)),
    sd = vapply(seq_len(k), function(i) num(i, "sd"), numeric(1)),
    stringsAsFactors = FALSE)
  for (nm in names(SR_PROFILE_QUANTILES)) out[[nm]] <- vapply(seq_len(k), function(i) num(i, nm), numeric(1))
  out$top_values <- lapply(seq_len(k), function(i) {
    empty <- data.frame(value = character(), n = numeric(), stringsAsFactors = FALSE)
    if (!.sr_prof_is_simple(types[i])) return(empty)
    q <- DBI::dbQuoteIdentifier(con, cols[i])
    tv <- sr_duck_query(con, sprintf(paste0(
      "SELECT v AS value, c AS n FROM (SELECT CAST(%s AS VARCHAR) AS v, count(*) AS c FROM %s ",
      "WHERE %s IS NOT NULL GROUP BY 1) ORDER BY c DESC, v LIMIT %d"), q, qv, q, SR_PROFILE_TOP_N))
    data.frame(value = as.character(tv$value), n = as.numeric(tv$n), stringsAsFactors = FALSE)
  })
  out$mean_nchar <- vapply(seq_len(k), function(i) num(i, "nchar"), numeric(1))
  out$p_numeric_parsable <- ratio(vapply(seq_len(k), function(i) num(i, "pn"), numeric(1)), nn)
  out$p_date_parsable <- ratio(vapply(seq_len(k), function(i) num(i, "pd"), numeric(1)), nn)
  out
}

# ---- cache (JSON) -----------------------------------------------------------------------------------

.sr_prof_to_json <- function(cols, hash, redacted, exact, rows) {
  recs <- lapply(seq_len(nrow(cols)), function(i) {
    r <- as.list(cols[i, setdiff(names(cols), "top_values"), drop = FALSE])
    r$top_values <- cols$top_values[[i]]
    r
  })
  list(store_hash = hash, redacted_cols = as.list(redacted), exact = exact, rows = rows, columns = recs)
}

.sr_prof_from_json <- function(j) {
  pick <- function(rec, f, type) {
    v <- rec[[f]]
    if (is.null(v)) return(type)
    if (is.character(type)) as.character(v) else as.numeric(v)
  }
  cols <- j$columns
  num <- function(f) vapply(cols, function(r) pick(r, f, NA_real_), numeric(1))
  chr <- function(f) vapply(cols, function(r) pick(r, f, NA_character_), "")
  out <- data.frame(column = chr("column"), type = chr("type"), n = num("n"), n_missing = num("n_missing"),
                    p_missing = num("p_missing"), n_distinct = num("n_distinct"), min = chr("min"),
                    max = chr("max"), mean = num("mean"), sd = num("sd"), stringsAsFactors = FALSE)
  for (nm in names(SR_PROFILE_QUANTILES)) out[[nm]] <- num(nm)
  out$top_values <- lapply(cols, function(r) {
    tv <- r$top_values
    data.frame(value = vapply(tv, function(t) as.character(t$value), ""),
               n = vapply(tv, function(t) as.numeric(t$n), numeric(1)), stringsAsFactors = FALSE)
  })
  out$mean_nchar <- num("mean_nchar")
  out$p_numeric_parsable <- num("p_numeric_parsable")
  out$p_date_parsable <- num("p_date_parsable")
  out
}

# The cached columns when the cache file is valid for this store hash, schema, exact decision and
# redaction set; NULL otherwise (a damaged file is simply recomputed).
.sr_prof_cache_read <- function(path, hash, schema_names, redacted, exact) {
  if (!file.exists(path)) return(NULL)
  tryCatch({
    j <- .sr_read_json_file(path)
    got <- sort(as.character(unlist(j$redacted_cols, use.names = FALSE)), method = "radix")
    if (!identical(j$store_hash, hash) || !identical(got, redacted) || !identical(j$exact, exact)) return(NULL)
    cols <- .sr_prof_from_json(j)
    if (!identical(cols$column, schema_names)) return(NULL)
    cols
  }, error = function(e) NULL)
}

#' Profile a store table out of core. Returns list(columns, sample_file, store_hash, exact, rows):
#'  - `columns` has one row per column: column, type, n, n_missing, p_missing, n_distinct, min, max (character,
#'    ISO for dates), mean, sd, q01 q25 q50 q75 q99 (numeric types only, else NA), top_values (list column of
#'    data.frame(value, n), the ten most frequent, ties broken by value), mean_nchar (VARCHAR only) and
#'    p_numeric_parsable / p_date_parsable (VARCHAR only; the share of NON-missing values that parse, NA
#'    when there are none).
#'  - n_distinct and the quantiles are exact when the table has at most getOption("sr.profile_exact_max", 1e6)
#'    rows, otherwise approx_count_distinct / approx_quantile (`exact` says which).
#'  - while sr_blind(project), the outcome-role columns keep only type, n, n_missing, p_missing, min, max,
#'    n_distinct and top_values; the other fields are NA.
#'  - `sample_file` is the reproducible reservoir sample (min(sample_n, rows) rows) for `seed`.
sr_profile_table <- function(project, table, sample_n = SR_PROFILE_SAMPLE_N, seed = 1L) {
  if (!.sr_whole_number(sample_n, 1)) stop("sample_n must be a whole number >= 1", call. = FALSE)
  seed <- .sr_prof_check_seed(seed)
  m <- sr_store_manifest(project, table)
  hash <- sr_store_hash(m)
  pc <- .sr_prof_columns(project, table, m, hash)
  sample_file <- .sr_prof_ensure_sample(project, table, m, hash, sample_n, seed)
  list(columns = pc$columns, sample_file = sample_file, store_hash = hash, exact = pc$exact, rows = m$rows)
}

# The column frame of a table, from the cache when it is valid and computed (and cached) otherwise; no
# sample is touched. Returns list(columns, exact). `m` is the table's manifest and `hash` its store hash.
.sr_prof_columns <- function(project, table, m, hash) {
  schema <- data.frame(name = vapply(m$schema, function(s) s$name, ""),
                       type = vapply(m$schema, function(s) s$type, ""), stringsAsFactors = FALSE)
  redacted <- character()
  if (sr_blind(project)) {
    redacted <- intersect(sr_role_cols(project, table, "outcome"), schema$name)
  }
  redacted <- sort(redacted, method = "radix")
  exact <- m$rows <= getOption("sr.profile_exact_max", 1e6)
  path <- .sr_prof_cache_path(project, table, hash)
  cols <- .sr_prof_cache_read(path, hash, schema$name, redacted, exact)
  if (is.null(cols)) {
    view <- sr_store_view(project, table)
    cols <- .sr_prof_compute(project, view, schema, redacted, exact)
    doc <- .sr_prof_to_json(cols, hash, redacted, exact, m$rows)
    .sr_write_json_file(path, doc)
    # hand back what a cache hit would: the JSON text keeps 15 significant digits, so a first call and
    # a cached one return identical values
    cols <- .sr_prof_from_json(.sr_parse_json(.sr_json(doc)))
  }
  list(columns = cols, exact = exact)
}

# ---- memory estimate --------------------------------------------------------------------------------

# Bytes one value of a column takes in memory: 8 for 64-bit and decimal types, 4 for 32-bit ones, the
# mean string length + 8 for VARCHAR. Unknown types count as 8 (conservative).
.sr_mem_bytes <- function(type, mean_nchar) {
  t <- toupper(type)
  if (identical(t, "VARCHAR")) return(mean_nchar + 8)
  if (grepl("^(INTEGER|SMALLINT|TINYINT|BOOLEAN|DATE|UINTEGER|USMALLINT|UTINYINT)$", t)) return(4)
  8
}

#' Estimate whether an analysis of `cols` fits in memory and choose a route if it does not.
#' data bytes = rows * sum(bytes per column); model matrix = rows * p * 8 where `p` defaults to the number of
#' columns in `cols` (the outcome included, when it is listed) plus (distinct levels - 1) for each VARCHAR
#' column; pass `p` to override that. GB means 1024^3 bytes throughout. need = data + 3 * model matrix. It fits when
#' need <= 0.8 * the profile's RAM budget. When it does not: glm / lm -> "bigglm", cox -> "bam", ml ->
#' "subsample". `override_rows` replaces the stored row count (tests). Returns list(need_gb, budget_gb,
#' fits, route, reason, rows, p); the caller logs the decision.
sr_mem_estimate <- function(project, table, cols, method = c("glm", "lm", "cox", "ml"), p = NULL,
                            profile = project$meta$profile, override_rows = NULL) {
  method <- match.arg(method)
  caps <- sr_profile_caps(profile)
  if (!is.character(cols) || !length(cols) || anyNA(cols)) {
    stop("cols must be a character vector naming at least one column", call. = FALSE)
  }
  cols <- unique(cols)
  if (!is.null(p) && !(is.numeric(p) && length(p) == 1L && is.finite(p) && p >= 1)) {
    stop("p must be a single number >= 1", call. = FALSE)
  }
  if (!is.null(override_rows) &&
      !(is.numeric(override_rows) && length(override_rows) == 1L && is.finite(override_rows) && override_rows >= 0)) {
    stop("override_rows must be a single non-negative number", call. = FALSE)
  }
  m <- sr_store_manifest(project, table)
  cp <- .sr_prof_columns(project, table, m, sr_store_hash(m))$columns   # no sample is written
  unknown <- setdiff(cols, cp$column)
  if (length(unknown)) {
    stop("unknown column(s) for table '", table, "': ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  cp <- cp[match(cols, cp$column), , drop = FALSE]
  rows <- if (is.null(override_rows)) m$rows else override_rows
  nchar <- ifelse(is.na(cp$mean_nchar), 16, cp$mean_nchar)   # a redacted outcome has no mean_nchar
  bytes <- vapply(seq_len(nrow(cp)), function(i) .sr_mem_bytes(cp$type[i], nchar[i]), numeric(1))
  cat_levels <- ifelse(vapply(cp$type, .sr_prof_is_varchar, logical(1)), pmax(cp$n_distinct, 1, na.rm = TRUE) - 1, 0)
  p_use <- if (is.null(p)) length(cols) + sum(cat_levels, na.rm = TRUE) else p
  gb <- 1024^3
  data_gb <- rows * sum(bytes) / gb
  mm_gb <- rows * p_use * 8 / gb
  need <- data_gb + 3 * mm_gb
  budget <- caps$ram_budget_gb
  fits <- need <= 0.8 * budget
  route <- if (fits) "in_memory" else switch(method, glm = , lm = "bigglm", cox = "bam", ml = "subsample")
  reason <- sprintf(
    "%s: about %.2f GB needed (data %.2f GB + 3 x model matrix %.2f GB) for %s rows and %s model columns, against %.2f GB usable (80%% of the %.0f GB budget on the %s profile)",
    if (fits) "fits in memory" else sprintf("does not fit in memory, route to %s", route),
    need, data_gb, mm_gb, format(rows, big.mark = ",", scientific = FALSE), format(round(p_use)), 0.8 * budget,
    budget, profile)
  list(need_gb = need, budget_gb = budget, fits = fits, route = route, reason = reason, rows = rows, p = p_use)
}
