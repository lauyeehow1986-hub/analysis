# scale/stream_io.R - the inlined IO helpers of the streaming engine (P1e.1, spec section 6).
#
# Every .h_st_* function here is emitted into the analysis code with .sr_inline(), so it is
# self-contained: it calls only base, stats, utils, DBI, duckdb and the other .h_ helpers, never a
# StatRouter function, and its text is ASCII. The prologue (.sr_run_prologue_stream, routes/run.R)
# opens the connection; the terms setup (routes/terms.R) and the fitting code read through the rest.

#' Open the locked-down in-memory DuckDB connection of a streaming run. `path` is the extract (it is
#' only checked to exist); `mem` is a memory_limit text such as "4GB" (a number is read as GB);
#' `threads` an integer; `temp` the spill directory. The statements and their order mirror
#' .sr_duck_setup() in io/duck.R: the extension directory is an empty folder and nothing can be
#' installed or loaded, the limits follow, and lock_configuration comes last.
.h_st_open <- function(path, mem, threads, temp) {
  if (!file.exists(path)) stop("the extract is missing")
  if (is.numeric(mem)) mem <- paste0(format(mem), "GB")
  ext <- file.path(temp, "ext-empty")
  dir.create(ext, showWarnings = FALSE, recursive = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  ok <- FALSE
  on.exit(if (!ok) try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  run <- function(sql) DBI::dbExecute(con, sql)
  loc <- function(x) DBI::dbQuoteString(con, normalizePath(x, winslash = "/", mustWork = FALSE))
  run(paste0("SET extension_directory = ", loc(ext)))
  run(paste0("SET custom_extension_repository = ", loc(ext)))
  run(paste0("SET autoinstall_extension_repository = ", loc(ext)))
  run("SET autoinstall_known_extensions = false")
  run("SET autoload_known_extensions = false")
  run("SET allow_community_extensions = false")
  run(paste0("SET memory_limit = ", DBI::dbQuoteString(con, as.character(mem))))
  run(sprintf("SET threads = %d", as.integer(threads)))
  run(paste0("SET temp_directory = ", loc(temp)))
  run("SET preserve_insertion_order = true")
  run("SET lock_configuration = true")
  got <- DBI::dbGetQuery(con, paste(
    "SELECT current_setting('autoload_known_extensions') AS a,",
    "current_setting('autoinstall_known_extensions') AS b,",
    "current_setting('allow_community_extensions') AS c,",
    "current_setting('lock_configuration') AS l"))
  if (!identical(c(as.logical(got$a), as.logical(got$b), as.logical(got$c), as.logical(got$l)),
                 c(FALSE, FALSE, FALSE, TRUE))) {
    stop("DuckDB lockdown did not take effect")
  }
  ok <- TRUE
  con
}

#' The SQL predicate that drops the excluded rows (ruling R13). With no rows it is "TRUE".
#' Up to `lit_max` rows it is a NOT IN over the row numbers as literals. A literal list is pushed into
#' the Parquet scan as a filter, so the scan keeps the order of .sr_row, but it costs O(n x k).
#' Above `lit_max` the rows are registered as the table sr_excl and the predicate is the hash
#' anti-join NOT IN (SELECT r FROM sr_excl). DuckDB plans that as a parallel join whose output is not
#' in .sr_row order once the extract has several row groups (Task 15), so that predicate carries
#' attr(, "sort") = TRUE and a read that needs file order adds .h_st_order_by(where).
.h_st_excl <- function(con, rows, lit_max = 1000L) {
  if (!length(rows)) return("TRUE")
  rows <- sort(unique(as.numeric(rows)))
  if (length(rows) <= lit_max) {
    return(paste0("\".sr_row\" NOT IN (", paste(sprintf("%.0f", rows), collapse = ", "), ")"))
  }
  duckdb::duckdb_register(con, "sr_excl", data.frame(r = rows), overwrite = TRUE)
  structure("\".sr_row\" NOT IN (SELECT r FROM sr_excl)", sort = TRUE)
}

#' The ORDER BY clause a file-order read over `where` needs: "" when the predicate keeps the scan
#' order, else ' ORDER BY ".sr_row"' (the hash anti-join of .h_st_excl).
.h_st_order_by <- function(where) if (isTRUE(attr(where, "sort"))) " ORDER BY \".sr_row\"" else ""

#' Fold step(state, df) over the result of `sql` in chunks of `chunk` rows. With order = TRUE the
#' column .sr_row must be strictly increasing within and across chunks, else it stops with "extract
#' read out of order". Returns list(state, rows).
.h_st_pass <- function(con, sql, chunk, init, step, order = TRUE) {
  res <- DBI::dbSendQuery(con, sql)
  on.exit(try(DBI::dbClearResult(res), silent = TRUE), add = TRUE)
  state <- init
  rows <- 0
  last <- -Inf
  repeat {
    df <- DBI::dbFetch(res, n = chunk)
    if (!nrow(df)) break
    if (isTRUE(order)) {
      r <- df[[".sr_row"]]
      if (is.null(r) || anyNA(r) || r[1L] <= last || (length(r) > 1L && any(diff(r) <= 0))) {
        stop("extract read out of order")
      }
      last <- r[length(r)]
    }
    state <- step(state, df)
    rows <- rows + nrow(df)
  }
  list(state = state, rows = rows)
}

#' Write {"pass": k, "max": m} to `file` for the Analyses panel. A failed write is ignored.
.h_st_progress <- function(file, k, m) {
  # written beside the file and renamed over it, so a reader never sees a half-written file
  tryCatch({
    txt <- sprintf("{\"pass\": %d, \"max\": %d}\n", as.integer(k), as.integer(m))
    tmp <- paste0(file, ".part")
    suppressWarnings(cat(txt, file = tmp))
    if (!isTRUE(suppressWarnings(file.rename(tmp, file)))) suppressWarnings(cat(txt, file = file))
  }, error = function(e) NULL)
  invisible(NULL)
}

#' The distinct non-NA values of column `col` of `src` among the rows `where` keeps. Pipe them to
#' .h_levels(), which sorts, so the levels are those of the in-memory setup.
.h_st_distinct <- function(con, src, col, where) {
  id <- DBI::dbQuoteIdentifier(con, col)
  DBI::dbGetQuery(con, paste0("SELECT DISTINCT ", id, " AS v FROM ", src, " WHERE (", where, ") AND ",
                              id, " IS NOT NULL"))$v
}

#' nk RCS knots of numeric column `col`, equal to .h_knots(x, nk) on the full column (spec 6.3).
#' The quantile probabilities are those of Hmisc::rcspline.eval for nk 3 to 7, as literals. The SQL
#' quantiles (type 7, as R's quantile) are used when Hmisc's default rule applies unchanged: at least
#' 100 values, more than nk + 2 distinct values, distinct quantiles, and no end value holding 5% of the
#' values (Hmisc's fractied rule). Otherwise the (value, count) table is fetched and .h_knots() runs on
#' rep(value, count), refused above `max_rows` values. Fewer than 100 values are fetched whole.
.h_st_knots <- function(con, src, col, nk, where, max_rows) {
  id <- DBI::dbQuoteIdentifier(con, col)
  ty <- DBI::dbGetQuery(con, paste0("SELECT typeof(", id, ") AS t FROM ", src, " LIMIT 1"))$t
  if (length(ty) && !grepl("^(TINYINT|SMALLINT|INTEGER|BIGINT|HUGEINT|UTINYINT|USMALLINT|UINTEGER|UBIGINT|UHUGEINT|FLOAT|DOUBLE|REAL|DECIMAL|BOOLEAN)",
                           toupper(ty))) {
    stop("column '", col, "' is not numeric (set a reference level to treat it as a factor)")
  }
  prob <- list(
    c(0.10000000000000001, 0.5, 0.90000000000000002),
    c(0.050000000000000003, 0.34999999999999998, 0.65000000000000002, 0.94999999999999996),
    c(0.050000000000000003, 0.27499999999999997, 0.49999999999999994, 0.72499999999999998, 0.94999999999999996),
    c(0.050000000000000003, 0.22999999999999998, 0.40999999999999998, 0.59000000000000008, 0.77000000000000002, 0.94999999999999996),
    c(0.025000000000000001, 0.18333333333333332, 0.34166666666666667, 0.5, 0.65833333333333333, 0.81666666666666665, 0.97499999999999998)
  )
  nk <- as.integer(nk)
  if (nk < 3L || nk > 7L) stop("knots for '", col, "' must be a number of knots (3 to 7)")
  p <- prob[[nk - 2L]]
  # a NaN is missing, as in memory (.h_knots drops is.na, which holds for NaN); DuckDB would sort it last
  base <- paste0("WITH d AS (SELECT CAST(", id, " AS DOUBLE) AS x FROM ", src, " WHERE (", where,
                 ") AND ", id, " IS NOT NULL AND NOT isnan(CAST(", id, " AS DOUBLE)))")
  qs <- paste0("quantile_cont(x, ", sprintf("%.17g", p), ") AS q", seq_along(p), collapse = ", ")
  st <- DBI::dbGetQuery(con, paste0(
    base, ", s AS (SELECT min(x) AS lo, max(x) AS hi FROM d) ",
    "SELECT count(*) AS m, count(DISTINCT x) AS nu, ",
    "count(*) FILTER (WHERE x = (SELECT lo FROM s)) AS flo, ",
    "count(*) FILTER (WHERE x = (SELECT hi FROM s)) AS fhi, ", qs, " FROM d"))
  m <- as.numeric(st$m)
  if (m < 100) {
    x <- DBI::dbGetQuery(con, paste0(base, " SELECT x FROM d"))$x
    return(.h_knots(x, nk))
  }
  qv <- as.numeric(unlist(st[paste0("q", seq_along(p))]))
  plain <- as.numeric(st$nu) - 2 > nk && st$flo / m < 0.05 && st$fhi / m < 0.05 &&
    !anyNA(qv) && length(unique(qv)) == nk
  if (plain) return(sort(unique(qv)))
  if (m > max_rows) {
    stop("too many tied values to place knots for ", col, " at scale; set explicit knots in the SAP")
  }
  tb <- DBI::dbGetQuery(con, paste0(base, " SELECT x, count(*) AS n FROM d GROUP BY x"))
  .h_knots(rep(tb$x, as.numeric(tb$n)), nk)
}
