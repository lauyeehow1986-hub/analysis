# profile/dq.R - the data-quality (DQ) framework: the findings schema, the check registry, the runner
# and the stored runs. The checks themselves live in dq_*.R and register through .sr_dq_register().
#
# A finding is one row of a data.frame (sr_findings_empty() has the schema):
#   id (chr) check (chr) domain (chr) column (chr) severity (chr) n_affected (int) p_affected (dbl)
#   message (chr) where (list) examples (list) detail (list)
# - id is substr(sha256(check | column | where JSON), 1, 12): stable across runs of the same data.
# - domain is one of conformance, completeness, plausibility, uniqueness, structure, leakage.
# - severity is info < warn < block; a block finding blocks analysis until an action is decided for it.
# - p_affected is n_affected over ALL rows of the table (not over its non-missing values).
# - `where` is a recipe filter that selects the affected rows, or NULL (all rows, or not expressible):
#     leaf        list(col = , op = , value = )   op in == != < <= > >= in not_in is_na not_na between
#                 (is_na / not_na have no value; in / not_in take a vector; between takes c(lo, hi))
#     combinator  list(all = list(<filters>)) or list(any = list(<filters>))
#   Numbers are doubles, dates are ISO strings. sr_where_sql() (the recipe, Task 17) turns it into SQL.
# - examples is a list of character vectors of up to five offending values.
# - detail is a named list of data.frames, each with a `column` field. Never a list keyed by a column
#   or level name: the outcome-blind gate (.sr_assert_blind) matches names exactly.
#
# A run is stored as data/dq/<table>-<run_id>.json (written atomically) and recorded by the `dq.run`
# event {table, run_id, file, sha256, n_findings, by_severity, store_hash, checks}; state$dq[[table]]
# holds the latest run of each table. sr_dq_findings() reads the file back (checking its SHA-256) and
# returns exactly what sr_dq_run() saw: the findings are normalised through their own JSON form, so
# the two are identical().
#
# Outcome-blind rule: a check sees ctx$sample, which sr_sample() already strips of the outcome columns
# while blind. The one exception is missing_dependence for an outcome column with >= 5% missing: it may
# read is.na(outcome) from the sample file (never the values). Every finding set passes
# .sr_assert_blind() before it is stored.

SR_DQ_DOMAINS <- c("conformance", "completeness", "plausibility", "uniqueness", "structure", "leakage")
SR_DQ_SEVERITIES <- c("info", "warn", "block")
SR_DQ_EXAMPLES_N <- 5L

# ---- the schema -------------------------------------------------------------------------------------

#' A findings frame with no rows.
sr_findings_empty <- function() {
  out <- data.frame(id = character(), check = character(), domain = character(), column = character(),
                    severity = character(), n_affected = integer(), p_affected = numeric(),
                    message = character(), stringsAsFactors = FALSE)
  out$where <- list()
  out$examples <- list()
  out$detail <- list()
  out
}

# ---- recipe filters ---------------------------------------------------------------------------------

.sr_where_ops <- c("==", "!=", "<", "<=", ">", ">=", "in", "not_in", "is_na", "not_na", "between")

# A leaf filter. Numbers become doubles; is_na / not_na carry no value.
.sr_where_leaf <- function(col, op, value = NULL) {
  if (!(op %in% .sr_where_ops)) stop("unknown filter op: ", op, call. = FALSE)
  if (is.numeric(value)) value <- as.numeric(value)
  out <- list(col = col, op = op)
  if (!(op %in% c("is_na", "not_na"))) out$value <- value
  out
}

.sr_where_all <- function(...) list(all = list(...))
.sr_where_any <- function(...) list(any = list(...))

# Decode a parsed-JSON filter: numbers are doubles, `value` is an atomic vector, combinator children
# stay a list. NULL stays NULL.
.sr_where_decode <- function(x, key = "") {
  if (is.null(x)) return(NULL)
  if (identical(key, "value")) {
    v <- unlist(x, use.names = FALSE)
    return(if (is.numeric(v)) as.numeric(v) else v)
  }
  if (is.list(x)) {
    nms <- names(x)
    out <- lapply(seq_along(x), function(i) .sr_where_decode(x[[i]], if (is.null(nms)) "" else nms[i]))
    names(out) <- nms
    return(out)
  }
  if (is.numeric(x)) as.numeric(x) else x
}

# ---- one finding ------------------------------------------------------------------------------------

# The id of a finding: the first 12 hex characters of sha256(check | column | where JSON). `salt` is
# mixed into the id only when it is given (check_error, so two failing checks do not collide).
.sr_dq_id <- function(check, column, where, salt = NULL) {
  wj <- as.character(jsonlite::toJSON(where, auto_unbox = TRUE, digits = NA, null = "null"))
  substr(.sr_sha256(paste(c(check, column, wj, salt), collapse = "|")), 1L, 12L)
}

# One finding as a one-row frame (see sr_findings_empty). `examples` is coerced to character and cut to
# five; `where` is a recipe filter or NULL; `detail` is a named list of data.frames.
.sr_dq_finding <- function(check, domain, column, severity, n_affected, p_affected, message, where = NULL,
                           examples = character(), detail = list(), salt = NULL) {
  ex <- as.character(examples)
  ex <- utils::head(ex[!is.na(ex)], SR_DQ_EXAMPLES_N)
  out <- data.frame(id = .sr_dq_id(check, column, where, salt), check = check, domain = domain, column = column,
                    severity = severity, n_affected = as.integer(n_affected), p_affected = as.numeric(p_affected),
                    message = message, stringsAsFactors = FALSE)
  out$where <- list(where)
  out$examples <- list(ex)
  out$detail <- list(detail)
  out
}

# Row-bind findings frames (list columns kept as lists). NULL and zero-row parts are dropped.
.sr_dq_bind <- function(parts) {
  parts <- Filter(function(f) !is.null(f) && nrow(f) > 0L, parts)
  if (!length(parts)) return(sr_findings_empty())
  pick <- function(col) unlist(lapply(parts, function(f) f[[col]]), use.names = FALSE)
  lst <- function(col) unlist(lapply(parts, function(f) f[[col]]), recursive = FALSE, use.names = FALSE)
  out <- data.frame(id = pick("id"), check = pick("check"), domain = pick("domain"), column = pick("column"),
                    severity = pick("severity"), n_affected = as.integer(pick("n_affected")),
                    p_affected = as.numeric(pick("p_affected")), message = pick("message"),
                    stringsAsFactors = FALSE)
  out$where <- lst("where")
  out$examples <- lst("examples")
  out$detail <- lst("detail")
  out
}

# Validate what a check returned: a findings frame with the right columns and vocabularies.
.sr_dq_validate <- function(x, check) {
  want <- names(sr_findings_empty())
  if (!is.data.frame(x) || !all(want %in% names(x))) {
    stop("check '", check, "' must return a findings data.frame (see sr_findings_empty())", call. = FALSE)
  }
  if (!nrow(x)) return(sr_findings_empty())
  if (!all(x$domain %in% SR_DQ_DOMAINS)) {
    stop("check '", check, "' returned an unknown domain: ", paste(setdiff(x$domain, SR_DQ_DOMAINS), collapse = ", "),
         call. = FALSE)
  }
  if (!all(x$severity %in% SR_DQ_SEVERITIES)) {
    stop("check '", check, "' returned an unknown severity: ",
         paste(setdiff(x$severity, SR_DQ_SEVERITIES), collapse = ", "), call. = FALSE)
  }
  bad <- !vapply(x$detail, function(d) {
    is.list(d) && !is.data.frame(d) && all(vapply(d, is.data.frame, logical(1))) &&
      (!length(d) || !is.null(names(d)))
  }, logical(1))
  if (any(bad)) {
    stop("check '", check, "' returned a detail that is not a named list of data.frames", call. = FALSE)
  }
  .sr_dq_bind(list(x[, want, drop = FALSE]))
}

# ---- findings <-> JSON ------------------------------------------------------------------------------

.sr_dq_df_to_json <- function(df) {
  types <- vapply(df, function(v) {
    if (is.integer(v)) "integer" else if (is.numeric(v)) "numeric" else if (is.logical(v)) "logical" else "character"
  }, "")
  cols <- lapply(seq_along(df), function(i) {
    v <- df[[i]]
    v <- switch(types[i], integer = as.integer(v), numeric = as.numeric(v), logical = as.logical(v), as.character(v))
    I(unname(v))
  })
  list(columns = as.list(names(df)), types = as.list(unname(types)), data = cols)
}

.sr_dq_df_from_json <- function(j) {
  nms <- vapply(j$columns, as.character, "")
  types <- vapply(j$types, as.character, "")
  cols <- lapply(seq_along(nms), function(i) {
    raw <- j$data[[i]]
    conv <- switch(types[i], integer = function(v) if (is.null(v)) NA_integer_ else as.integer(v),
                   numeric = function(v) if (is.null(v)) NA_real_ else as.numeric(v),
                   logical = function(v) if (is.null(v)) NA else as.logical(v),
                   function(v) if (is.null(v)) NA_character_ else as.character(v))
    proto <- switch(types[i], integer = integer(1), numeric = numeric(1), logical = logical(1), character(1))
    vapply(raw, conv, proto)
  })
  names(cols) <- nms
  as.data.frame(cols, stringsAsFactors = FALSE, optional = TRUE)
}

.sr_dq_to_json <- function(f) {
  lapply(seq_len(nrow(f)), function(i) {
    rec <- list(id = f$id[i], check = f$check[i], domain = f$domain[i], column = f$column[i],
                severity = f$severity[i], n_affected = f$n_affected[i], p_affected = f$p_affected[i],
                message = f$message[i])
    rec["where"] <- list(f$where[[i]])
    rec$examples <- I(unname(as.character(f$examples[[i]])))
    d <- f$detail[[i]]
    rec$detail <- lapply(d, .sr_dq_df_to_json)
    rec
  })
}

.sr_dq_from_json <- function(recs) {
  if (!length(recs)) return(sr_findings_empty())
  s <- function(f) vapply(recs, function(r) if (is.null(r[[f]])) NA_character_ else as.character(r[[f]]), "")
  out <- data.frame(id = s("id"), check = s("check"), domain = s("domain"), column = s("column"),
                    severity = s("severity"),
                    n_affected = vapply(recs, function(r) if (is.null(r$n_affected)) NA_integer_ else as.integer(r$n_affected),
                                        integer(1)),
                    p_affected = vapply(recs, function(r) if (is.null(r$p_affected)) NA_real_ else as.numeric(r$p_affected),
                                        numeric(1)),
                    message = s("message"), stringsAsFactors = FALSE)
  out$where <- lapply(recs, function(r) .sr_where_decode(r$where))
  out$examples <- lapply(recs, function(r) as.character(unlist(r$examples, use.names = FALSE)))
  out$detail <- lapply(recs, function(r) {
    d <- lapply(r$detail, .sr_dq_df_from_json)
    if (!length(d)) list() else d
  })
  out
}

# Findings as the JSON round trip returns them (what a stored run reads back as).
.sr_dq_normalise <- function(f) {
  .sr_dq_from_json(.sr_parse_json(.sr_json(.sr_dq_to_json(f))))
}

# ---- the registry -----------------------------------------------------------------------------------

.sr_dq_checks <- new.env(parent = emptyenv())
.sr_dq_counter <- new.env(parent = emptyenv())
.sr_dq_counter$n <- 0L

# Register (or replace) a DQ check. `fn(ctx) -> findings` where ctx is list(project, con, table, view,
# profile, dict, roles, blind, sample). Checks run in registration order; replacing a check keeps its
# place.
.sr_dq_register <- function(check, domain, fn) {
  if (!is.character(check) || length(check) != 1L || is.na(check) || !grepl("^[a-z][a-z0-9_]*$", check)) {
    stop("check must be a single snake_case name", call. = FALSE)
  }
  if (length(domain) != 1L || !(domain %in% SR_DQ_DOMAINS)) {
    stop("domain must be one of: ", paste(SR_DQ_DOMAINS, collapse = ", "), call. = FALSE)
  }
  if (!is.function(fn)) stop("fn must be a function(ctx)", call. = FALSE)
  old <- get0(check, envir = .sr_dq_checks, inherits = FALSE)
  seq <- if (is.null(old)) {
    .sr_dq_counter$n <- .sr_dq_counter$n + 1L
    .sr_dq_counter$n
  } else old$seq
  assign(check, list(check = check, domain = domain, fn = fn, seq = seq), envir = .sr_dq_checks)
  invisible(check)
}

# Remove a check (tests that register a throwaway one).
.sr_dq_unregister <- function(check) {
  if (exists(check, envir = .sr_dq_checks, inherits = FALSE)) rm(list = check, envir = .sr_dq_checks)
  invisible(check)
}

#' The registered check names, in registration order.
sr_dq_checks <- function() {
  nms <- ls(.sr_dq_checks)
  if (!length(nms)) return(character())
  seqs <- vapply(nms, function(n) get(n, envir = .sr_dq_checks, inherits = FALSE)$seq, integer(1))
  nms[order(seqs)]
}

# ---- the context ------------------------------------------------------------------------------------

.sr_dq_context <- function(project, table) {
  profile <- sr_profile_table(project, table)
  view <- sr_store_view(project, table)
  list(project = project, con = sr_duck(project), table = table, view = view, profile = profile,
       dict = sr_dict_get(project, table), roles = sr_roles(project, table), blind = sr_blind(project),
       sample = sr_sample(project, table))
}

# The SQL-quoted store view, for checks that build queries.
.sr_dq_qv <- function(ctx) DBI::dbQuoteIdentifier(ctx$con, ctx$view)

# ---- SQL helpers for the checks ---------------------------------------------------------------------

# One aggregate for many predicates: counts of rows where each SQL predicate holds. `preds` is a
# character vector of SQL boolean expressions; the result is a numeric vector in the same order.
.sr_dq_count <- function(ctx, preds) {
  if (!length(preds)) return(numeric())
  sel <- sprintf("count(*) FILTER (WHERE %s) AS %s", preds, DBI::dbQuoteIdentifier(ctx$con, paste0("p", seq_along(preds))))
  r <- sr_duck_query(ctx$con, sprintf("SELECT %s FROM %s", paste(sel, collapse = ", "), .sr_dq_qv(ctx)))
  vapply(seq_along(preds), function(i) as.numeric(r[[i]][1L]), numeric(1))
}

# The distinct values (as text) of an SQL expression among rows where `pred` holds, most frequent first
# (ties by value), at most `limit`. Returns data.frame(value, n).
.sr_dq_values <- function(ctx, expr, pred, limit = SR_DQ_EXAMPLES_N) {
  r <- sr_duck_query(ctx$con, sprintf(paste0(
    "SELECT v AS value, c AS n FROM (SELECT CAST(%s AS VARCHAR) AS v, count(*) AS c FROM %s WHERE %s GROUP BY 1) ",
    "ORDER BY c DESC, v LIMIT %d"), expr, .sr_dq_qv(ctx), pred, as.integer(limit)))
  data.frame(value = as.character(r$value), n = as.numeric(r$n), stringsAsFactors = FALSE)
}

# A number as an exact-enough SQL literal.
.sr_dq_num <- function(x) sprintf("%.15g", x)

# ---- the runner -------------------------------------------------------------------------------------

# UTC timestamp to the millisecond with only digits and the T (20261004T120000123), then 4 random hex.
.sr_dq_run_id <- function() {
  ts <- format(Sys.time(), "%Y%m%dT%H%M%OS3", tz = "UTC")
  paste0(sub(".", "", ts, fixed = TRUE), "-", substr(.sr_uuid4(), 1L, 4L))
}

.sr_dq_rel <- function(table, run_id) paste0("data/dq/", table, "-", run_id, ".json")

# One check, run safely: an error (or an output that fails validation or the blind gate) becomes one
# `check_error` finding with the message.
.sr_dq_run_one <- function(entry, ctx, outcome_cols, other_cols) {
  tryCatch({
    f <- .sr_dq_validate(entry$fn(ctx), entry$check)
    if (ctx$blind) .sr_assert_blind(f, outcome_cols, other_cols)
    f
  }, error = function(e) {
    .sr_dq_finding("check_error", entry$domain, "*", "info", 0L, 0,
                   sprintf("check '%s' failed: %s", entry$check, conditionMessage(e)),
                   detail = list(error = data.frame(column = "*", check = entry$check, stringsAsFactors = FALSE)),
                   salt = entry$check)
  })
}

#' Run the registered DQ checks (all of them, or the ones named in `checks`, in registration order) on
#' a store table and record the run. Each check is wrapped in tryCatch: a failing check becomes one
#' `info` finding `check_error` and the run continues. While sr_blind(project) the findings go through
#' .sr_assert_blind(). Writes data/dq/<table>-<run_id>.json atomically and appends `dq.run`. Returns the
#' project. An unknown check name is an error.
sr_dq_run <- function(project, table, checks = NULL) {
  .sr_store_check_table(table)
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  known <- sr_dq_checks()
  if (is.null(checks)) {
    checks <- known
  } else {
    if (!is.character(checks) || anyNA(checks)) stop("checks must be NULL or a character vector of names", call. = FALSE)
    unknown <- setdiff(checks, known)
    if (length(unknown)) {
      stop("unknown DQ check(s): ", paste(unknown, collapse = ", "), " (registered: ", paste(known, collapse = ", "),
           ")", call. = FALSE)
    }
    checks <- known[known %in% checks]
  }
  ctx <- .sr_dq_context(project, table)
  role_cols <- names(ctx$roles)
  outcome_cols <- sr_role_cols(project, table, "outcome")
  other_cols <- setdiff(role_cols, outcome_cols)
  parts <- lapply(checks, function(ch) {
    .sr_dq_run_one(get(ch, envir = .sr_dq_checks, inherits = FALSE), ctx, outcome_cols, other_cols)
  })
  findings <- .sr_dq_normalise(.sr_dq_bind(parts))
  if (ctx$blind) .sr_assert_blind(findings, outcome_cols, other_cols)

  run_id <- .sr_dq_run_id()
  rel <- .sr_dq_rel(table, run_id)
  file <- file.path(sr_project_paths(project$paths$dir)$dir, rel)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  doc <- list(table = table, run_id = run_id, store_hash = ctx$profile$store_hash, checks = as.list(checks),
              findings = .sr_dq_to_json(findings))
  .sr_write_json_file(file, doc)
  sha <- .sr_sha256(.sr_read_bytes(file))
  by_sev <- lapply(stats::setNames(SR_DQ_SEVERITIES, SR_DQ_SEVERITIES), function(s) sum(findings$severity == s))
  tryCatch(
    sr_project_append(project, "dq.run", list(table = table, run_id = run_id, file = rel, sha256 = sha,
                                              n_findings = nrow(findings), by_severity = by_sev,
                                              store_hash = ctx$profile$store_hash, checks = as.list(checks))),
    error = function(e) {
      unlink(file)   # the event is the commit: no event, no file
      stop(e)
    })
}

#' The findings of the latest DQ run of `table`, as sr_dq_run() saw them. The file is checked against
#' the SHA-256 recorded by the `dq.run` event; a missing or altered file is an error.
sr_dq_findings <- function(project, table) {
  .sr_store_check_table(table)
  e <- project$state$dq[[table]]
  if (is.null(e)) stop("no DQ run recorded for table '", table, "'", call. = FALSE)
  file <- file.path(sr_project_paths(project$paths$dir)$dir, e$file)
  if (!file.exists(file)) stop("the DQ findings file of table '", table, "' is missing", call. = FALSE)
  bytes <- .sr_read_bytes(file)
  if (!identical(.sr_sha256(bytes), as.character(e$sha256))) {
    stop("the DQ findings file of table '", table, "' does not match its recorded SHA-256 (it was changed ",
         "outside the app)", call. = FALSE)
  }
  txt <- rawToChar(bytes)
  Encoding(txt) <- "UTF-8"
  .sr_dq_from_json(.sr_parse_json(txt)$findings)
}

# `dq.run` keeps the latest run of each table; other tables are untouched.
sr_reducer_register("dq.run", function(state, event) {
  p <- event$payload
  if (!is.character(p$table) || length(p$table) != 1L || is.na(p$table)) stop("dq.run needs a table", call. = FALSE)
  dq <- state$dq %||% list()
  dq[[p$table]] <- p
  state$dq <- dq
  state
})
