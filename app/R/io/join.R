# io/join.R - join two ingested store tables, with a key-check report, into a new store table.
#
#   sr_join_check(project, left, right, by)  the key report (read-only; no event)
#   sr_join(project, left, right, by, type, into, allow_many_to_many)  materialise `into` and append
#                                                                       `join.created`
#
# `by` is a named character vector, names = columns of `left`, values = columns of `right`
# (e.g. c(patient_id = "pid")); an unnamed element is the same name on both sides.
#
# All counts are DuckDB aggregates over the tables' views: no row is pulled into R. NA keys never
# match (SQL equality), also in a full join, where they stay as separate rows. The join is
# materialised the way ingest does it: COPY (SELECT ...) TO a Parquet part under data/ingest/<into>,
# then .sr_store_install() swaps the verified store into data/store/<into> atomically.
#
# The event carries the SELECT over the store VIEW names (never file paths), so the project stays
# relocatable: re-running it after sr_store_view() of both inputs reproduces the table.

.sr_join_types <- c("left", "inner", "full")

# Type family of a DuckDB column type: all numbers compare with each other, everything else only with
# its own kind (an INTEGER key against a VARCHAR key is refused, not silently cast).
.sr_join_family <- function(type) {
  t <- toupper(as.character(type))
  if (grepl("^(U?(TINY|SMALL|BIG|HUGE)?INT(EGER)?|FLOAT|DOUBLE|DECIMAL)([(].*[)])?$", t)) return("NUMBER")
  t
}

# Validate the arguments of a join and return the resolved spec: the two tables, the key columns of
# each side and the schemas. Creates the views of both tables on the project's connection.
.sr_join_spec <- function(project, left, right, by) {
  one <- function(x, what) {
    if (!is.character(x) || length(x) != 1L || is.na(x)) stop(what, " must be a single table name", call. = FALSE)
    .sr_store_check_table(x)
    if (!(x %in% sr_store_tables(project))) stop("unknown table: ", x, call. = FALSE)
    x
  }
  left <- one(left, "left")
  right <- one(right, "right")
  if (!is.character(by) || !length(by) || anyNA(by) || any(!nzchar(by))) {
    stop("by must be a non-empty character vector of column names (names = left columns, values = right columns)",
         call. = FALSE)
  }
  nm <- names(by)
  if (is.null(nm)) nm <- rep("", length(by))
  nm[is.na(nm) | !nzchar(nm)] <- unname(by)[is.na(nm) | !nzchar(nm)]
  lk <- unname(nm)
  rk <- unname(as.character(by))
  if (anyDuplicated(lk) || anyDuplicated(rk)) stop("by names the same column twice", call. = FALSE)
  ml <- sr_store_manifest(project, left)
  mr <- sr_store_manifest(project, right)
  sl <- stats::setNames(vapply(ml$schema, function(s) s$type, ""), vapply(ml$schema, function(s) s$name, ""))
  sr <- stats::setNames(vapply(mr$schema, function(s) s$type, ""), vapply(mr$schema, function(s) s$name, ""))
  for (k in lk) if (!(k %in% names(sl))) stop("'", k, "' is not a column of table '", left, "'", call. = FALSE)
  for (k in rk) if (!(k %in% names(sr))) stop("'", k, "' is not a column of table '", right, "'", call. = FALSE)
  for (i in seq_along(lk)) {
    if (!identical(.sr_join_family(sl[[lk[i]]]), .sr_join_family(sr[[rk[i]]]))) {
      stop("the key columns have different types: '", lk[i], "' (", sl[[lk[i]]], ") in '", left, "' and '",
           rk[i], "' (", sr[[rk[i]]], ") in '", right, "'; make them the same type first", call. = FALSE)
    }
  }
  sr_store_view(project, left)
  sr_store_view(project, right)
  list(left = left, right = right, lk = lk, rk = rk, ls = sl, rs = sr)
}

.sr_join_q <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))

# The key report of a resolved spec: one SELECT of scalar aggregates.
.sr_join_report <- function(project, spec) {
  con <- sr_duck(project)
  q <- function(x) .sr_join_q(con, x)
  L <- q(spec$left)
  R <- q(spec$right)
  lcols <- q(spec$lk)
  rcols <- q(spec$rk)
  na_any <- function(cols) paste(paste(cols, "IS NULL"), collapse = " OR ")
  na_none <- function(cols) paste(paste(cols, "IS NOT NULL"), collapse = " AND ")
  on <- paste(sprintf("sr_l.%s = sr_r.%s", lcols, rcols), collapse = " AND ")
  dups <- function(tbl, cols) {
    sprintf("(SELECT count(*) FROM (SELECT %s FROM %s WHERE %s GROUP BY %s HAVING count(*) > 1) AS d)",
            paste(cols, collapse = ", "), tbl, na_none(cols), paste(cols, collapse = ", "))
  }
  sql <- paste0(
    "SELECT ",
    "(SELECT count(*) FROM ", L, ") AS left_rows, ",
    "(SELECT count(*) FROM ", R, ") AS right_rows, ",
    "(SELECT count(*) FROM ", L, " WHERE ", na_any(lcols), ") AS left_key_na, ",
    "(SELECT count(*) FROM ", R, " WHERE ", na_any(rcols), ") AS right_key_na, ",
    dups(L, lcols), " AS left_dup_keys, ",
    dups(R, rcols), " AS right_dup_keys, ",
    "(SELECT count(*) FROM ", L, " AS sr_l WHERE EXISTS (SELECT 1 FROM ", R, " AS sr_r WHERE ", on, ")) AS matched_left, ",
    "(SELECT count(*) FROM ", R, " AS sr_r WHERE EXISTS (SELECT 1 FROM ", L, " AS sr_l WHERE ", on, ")) AS matched_right")
  d <- sr_duck_query(con, sql)
  n <- function(k) as.numeric(d[[k]][1L])
  lu <- n("left_dup_keys") == 0
  ru <- n("right_dup_keys") == 0
  list(left_rows = n("left_rows"), right_rows = n("right_rows"),
       left_key_unique = lu, right_key_unique = ru,
       left_key_na = n("left_key_na"), right_key_na = n("right_key_na"),
       matched_left = n("matched_left"), unmatched_left = n("left_rows") - n("matched_left"),
       matched_right = n("matched_right"), unmatched_right = n("right_rows") - n("matched_right"),
       relationship = if (lu && ru) "one_to_one" else if (lu) "one_to_many" else if (ru) "many_to_one" else "many_to_many")
}

#' The key report of a join of two ingested tables (nothing is recorded or written).
#'
#' Counts are LEFT rows for `matched_left` / `unmatched_left` and RIGHT rows for `matched_right` /
#' `unmatched_right`; a row with an NA in any key column counts as unmatched. `*_key_unique` and
#' `relationship` are over the non-NA keys. Everything is computed by DuckDB.
#' @return list(left_rows, right_rows, left_key_unique, right_key_unique, left_key_na, right_key_na,
#'   matched_left, unmatched_left, matched_right, unmatched_right,
#'   relationship = "one_to_one" | "one_to_many" | "many_to_one" | "many_to_many")
sr_join_check <- function(project, left, right, by) {
  .sr_join_report(project, .sr_join_spec(project, left, right, by))
}

# The output columns of the join and the SELECT that makes them, over the store view names. Left
# columns keep their names (the left key names are the key names; in a full join the key is the
# COALESCE of both sides so right-only rows keep theirs); right key columns are dropped; a right
# column whose name is already taken (case-insensitively, as DuckDB compares names) becomes
# <name>_<right table>, then <name>_<right table>_2, ...
.sr_join_select <- function(con, spec, type) {
  q <- function(x) .sr_join_q(con, x)
  used <- tolower(names(spec$ls))
  exprs <- character()
  for (nm in names(spec$ls)) {
    i <- match(nm, spec$lk)
    e <- if (!is.na(i) && type == "full") sprintf("COALESCE(sr_l.%s, sr_r.%s)", q(nm), q(spec$rk[i])) else sprintf("sr_l.%s", q(nm))
    exprs <- c(exprs, paste(e, "AS", q(nm)))
  }
  for (nm in setdiff(names(spec$rs), spec$rk)) {
    out <- nm
    if (tolower(out) %in% used) {
      out <- paste0(nm, "_", spec$right)
      k <- 2L
      while (tolower(out) %in% used) {
        out <- paste0(nm, "_", spec$right, "_", k)
        k <- k + 1L
      }
    }
    used <- c(used, tolower(out))
    exprs <- c(exprs, paste(sprintf("sr_r.%s", q(nm)), "AS", q(out)))
  }
  on <- paste(sprintf("sr_l.%s = sr_r.%s", q(spec$lk), q(spec$rk)), collapse = " AND ")
  kw <- switch(type, left = "LEFT JOIN", inner = "INNER JOIN", full = "FULL OUTER JOIN")
  paste0("SELECT ", paste(exprs, collapse = ", "), " FROM ", q(spec$left), " AS sr_l ", kw, " ",
         q(spec$right), " AS sr_r ON ", on)
}

#' Join two ingested tables into a new store table `into` and append `join.created`.
#'
#' `into` must be a new, valid table name (checked first, before any work). A many-to-many join is
#' refused (before anything is written) unless `allow_many_to_many = TRUE`. The result has the left
#' table's columns, then the right table's non-key columns; see .sr_join_select for the naming. Row
#' order is not guaranteed.
#' @return the project with the event applied.
sr_join <- function(project, left, right, by, type = c("left", "inner", "full"), into,
                    allow_many_to_many = FALSE) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  type <- match.arg(type)
  if (missing(into)) stop("into must name the new table", call. = FALSE)
  .sr_store_check_table(into)
  if (!is.logical(allow_many_to_many) || length(allow_many_to_many) != 1L || is.na(allow_many_to_many)) {
    stop("allow_many_to_many must be TRUE or FALSE", call. = FALSE)
  }
  if (into %in% sr_store_tables(project) || dir.exists(sr_store_dir(project, into)) ||
      !is.null(project$state$tables[[into]]) || !is.null(project$state$joins[[into]])) {
    stop("table '", into, "' already exists", call. = FALSE)
  }
  spec <- .sr_join_spec(project, left, right, by)
  report <- .sr_join_report(project, spec)
  if (identical(report$relationship, "many_to_many") && !allow_many_to_many) {
    stop("refusing a many-to-many join of '", spec$left, "' and '", spec$right,
         "': the key repeats on both sides, so rows multiply (", report$left_rows, " x ", report$right_rows,
         " rows); set allow_many_to_many = TRUE to join anyway", call. = FALSE)
  }

  con <- sr_duck(project)
  sql <- .sr_join_select(con, spec, type)
  ck <- .sr_ck_new(project, into, NULL, NULL, 0)
  if (dir.exists(ck$ingest_dir)) unlink(ck$ingest_dir, recursive = TRUE)
  if (!isTRUE(dir.create(ck$store_dir, recursive = TRUE, showWarnings = FALSE))) {
    stop("cannot create directory: ", ck$store_dir, call. = FALSE)
  }
  on.exit(unlink(ck$ingest_dir, recursive = TRUE), add = TRUE)

  part <- .sr_part_path(ck)
  rows <- .sr_col_copy(con, from = paste0("(", sql, ") AS sr_j"), to = part, what = "the join")
  .sr_part_commit(ck, part, rows)
  schema <- .sr_col_schema(con, ck)
  parts <- lapply(ck$parts, function(p) list(file = p$file, rows = p$rows, sha256 = p$sha256))
  manifest <- .sr_store_install(ck, list(table = into, parts = parts, schema = schema, rows = rows))
  store_hash <- sr_store_hash(manifest)

  payload <- list(
    into = into, left = spec$left, right = spec$right,
    by = as.list(stats::setNames(spec$rk, spec$lk)), type = type, report = report, sql = sql,
    store_hash = store_hash,
    store = list(dir = paste0("data/store/", into), rows = rows, ncol = length(schema), schema = schema,
                 parts = parts))
  tryCatch(sr_project_append(project, "join.created", payload), error = function(e) {
    unlink(sr_store_dir(project, into), recursive = TRUE)     # no event: do not leave the table behind
    stop(e)
  })
}

# `join.created` records the join and adds the result as a table (format "join", no source file: it
# is derived from the two inputs). Merges into the state; nothing else is touched.
sr_reducer_register("join.created", function(state, event) {
  p <- event$payload
  into <- p$into
  if (!is.character(into) || length(into) != 1L || is.na(into)) stop("join.created needs a target table", call. = FALSE)
  joins <- state$joins %||% list()
  j <- joins[[into]] %||% list()
  for (k in c("into", "left", "right", "by", "type", "report", "sql", "store_hash")) j[k] <- list(p[[k]])
  joins[[into]] <- j
  state$joins <- joins
  tabs <- state$tables %||% list()
  t <- tabs[[into]] %||% list()
  t$table <- into
  t$format <- "join"
  t["source"] <- list(NULL)
  t$store <- p$store
  t$store_hash <- p$store_hash
  t$derived_from <- list(p$left, p$right)
  tabs[[into]] <- t
  state$tables <- tabs
  state
})
