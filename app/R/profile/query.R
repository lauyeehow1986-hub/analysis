# profile/query.R - the "recheck at source" loop: a data-query sheet for the findings decided as recheck, and
# the corrected source brought back as a new version of the table, with a diff against the old one.
#
#   sr_query_sheet(project, table, finding_ids, path)   write the sheet (.xlsx) and append `query.exported`
#   sr_reimport(project, table, path, key, ...)         ingest the corrected source as <table>_r<k>, diff it
#                                                       against the active version, carry roles and the
#                                                       dictionary, append `data.reimported`
#   sr_table_diff(project, old, new, key)               the differences between two store tables, by key
#
# The sheet. One row per affected record of each finding: the rows are the ones the `where` of the finding
# selects (the same filter the recipe uses, through sr_where_sql), in order of record_id. A finding without
# a `where` (a whole-column or whole-table finding) gives one row with record_id and current_value NA.
# record_id is the id-role column, else the row number of the table sorted by every column (the store does
# not keep the row order of the source, so no other number is stable). current_value is the cell as text.
# The question is one templated sentence per check; it names the column, never a value. At most
# SR_QUERY_MAX_ROWS rows are written; if there are more, a `summary` sheet states the total. The file is
# written to <path>.tmp and renamed, so a re-export on the same day replaces it atomically. The path must be
# inside the project folder; the event holds it relative to the project root, with the SHA-256 of the file.
#
# Outcome-blind rule: while sr_blind(project), a value of an outcome-role column is never written, unless the
# finding is on the outcome column itself and its domain is completeness or plausibility. A filter that reads
# the outcome column (it would tell which records have which outcome) collapses the finding to one row, unless
# that same exception holds. The whole sheet passes .sr_assert_blind(), and each question passes the same
# text screen as a decision reason (.sr_sg_assert_text).
#
# The re-import. <table>_r<k> is the next free version number (1 + the highest existing); the original and
# earlier versions stay readable by their own names, and `state$tables[[table]]$active` records the latest.
# The corrected source is compared with the version that was active, on the key (the id-role columns unless
# `key` is given; no key is an error raised before anything is ingested). Roles (those whose column still
# exists) and the dictionary of the old version are given to the new one through roles.set / dict.set, as
# sr_recipe_apply does, because the outcome-blind guard depends on roles.
#
# The diff. Rows are compared on the columns both tables have, with EXCEPT ALL in both directions (so a
# repeated key is compared row by row, and an exact repeat is not a difference); the surviving rows are
# paired on the key and their order within it and joined with a FULL OUTER JOIN, and a cell differs when
# IS DISTINCT FROM says so. A column whose type changed is compared as text. added / removed count distinct
# keys present in only one table. Every count is a DuckDB aggregate; no row is pulled into R.

SR_QUERY_MAX_ROWS <- 50000L
SR_QUERY_COLS <- c("query_id", "record_id", "column", "current_value", "check", "question", "corrected_value", "comment")

# ---- questions --------------------------------------------------------------------------------------

# One sentence per check; %s is "the value of <column>" or "this record". No value, no number, and no word
# that could be the name of a column of the outcome (outcome, death, event, status, time).
.sr_qy_templates <- c(
  coercion = "Please check %s against the source: it could not be read as a number or a date. Give it as it is recorded.",
  range = "Please check %s against the source: it is outside the plausible range. Give the correct value.",
  unit_mix = "Please check %s against the source: it may be recorded in a different unit. Give it in the usual unit.",
  missing_codes = "Please check %s against the source: it holds a placeholder code. Give the real value, or leave it blank if it was not measured.",
  future_date = "Please check %s against the source: it is dated in the future. Give the correct date.",
  date_order = "Please check %s against the source: its dates are out of order. Give the correct dates.",
  old_date = "Please check %s against the source: it is dated implausibly long ago. Give the correct date.",
  post_outcome_date = "Please check %s against the source: it is dated later than it should be. Give the correct date.",
  dup_rows = "Please check %s against the source: it repeats an earlier record. Say whether it is a true repeat.",
  dup_id = "Please check %s against the source: this identifier occurs on more than one record. Say which record is correct.",
  dict_levels = "Please check %s against the source: it holds a code the dictionary does not list. Give the correct code.",
  dict_range = "Please check %s against the source: it is outside the range of the dictionary. Give the correct value.",
  dict_type = "Please check %s against the source: its stored type disagrees with the dictionary. Say how it is recorded.",
  missing_col = "Please check %s against the source: it is empty. Give it if it was recorded.",
  missing_row = "Please check %s against the source: most of it is empty. Say whether it is a real record.",
  cluster_detect = "Please check %s against the source: it looks like a grouping of the records. Say what it records.",
  offset_detect = "Please check %s against the source: it looks like a length of follow-up. Say what it records.",
  leakage = "Please check %s against the source: say when it was recorded, and by whom.")

.sr_qy_default_template <- "Please check %s against the source: a data check flagged it. Give the correct value."

# The question for `check` on `column` ("*" is the whole record).
.sr_qy_question <- function(check, column) {
  what <- if (identical(column, "*")) "this record" else paste0("the value of ", column)
  t <- unname(.sr_qy_templates[as.character(check)])
  if (length(t) != 1L || is.na(t)) t <- .sr_qy_default_template
  sprintf(t, what)
}

# ---- the outcome-blind rules ------------------------------------------------------------------------

# TRUE when the value of the finding's column may be written: always, except a blind outcome column outside
# the finding domains that are about the outcome column itself.
.sr_qy_show_value <- function(blind, is_outcome, domain) {
  !(isTRUE(blind) && isTRUE(is_outcome)) || domain %in% c("completeness", "plausibility")
}

# TRUE when, while blind, the filter reads an outcome column and the finding is not the allowed one on the
# outcome itself: the records it selects would tell their outcome, so the finding gets one row.
.sr_qy_hides_rows <- function(where, outcome_cols, blind, allowed_self) {
  if (!isTRUE(blind) || isTRUE(allowed_self) || is.null(where)) return(FALSE)
  any(.sr_rc_where_cols(where) %in% outcome_cols)
}

# ---- the rows of one finding ------------------------------------------------------------------------

# The connection, the view and the SQL pieces shared by every finding of `table`.
.sr_qy_context <- function(project, table) {
  sr_store_view(project, table)
  con <- sr_duck(project)
  m <- sr_store_manifest(project, table)
  cols <- vapply(m$schema, function(s) s$name, "")
  types <- stats::setNames(vapply(m$schema, function(s) s$type, ""), cols)
  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  id <- sr_role_cols(project, table, "id")
  if (length(id)) {
    base <- sprintf("SELECT * FROM %s", q(table))
    rid <- q(id[1L])
  } else {
    base <- sprintf("SELECT sr_t.*, row_number() OVER (ORDER BY %s) AS sr_qy_rid FROM %s AS sr_t",
                    paste(q(cols), collapse = ", "), q(table))
    rid <- "sr_qy_rid"
  }
  list(con = con, cols = cols, types = types, q = q, base = base, rid = rid)
}

# The rows of finding row `f` (a one-row findings frame): list(total, record_id, current_value), the vectors
# holding at most `limit` rows in order of record_id.
.sr_qy_finding_rows <- function(ctx, f, blind, outcome_cols, limit) {
  col <- f$column
  w <- f$where[[1L]]
  is_out <- col %in% outcome_cols
  allowed_self <- is_out && f$domain %in% c("completeness", "plausibility")
  show <- .sr_qy_show_value(blind, is_out, f$domain) && col %in% ctx$cols
  if (is.null(w) || .sr_qy_hides_rows(w, outcome_cols, blind, allowed_self)) {
    return(list(total = 1, record_id = NA_character_, current_value = NA_character_))
  }
  pred <- sr_where_sql(ctx$con, w, ctx$cols, ctx$types)
  from <- sprintf("(%s) AS sr_q WHERE %s", ctx$base, pred)
  total <- as.numeric(sr_duck_query(ctx$con, paste("SELECT count(*) AS n FROM", from))$n[1L])
  if (limit <= 0 || total <= 0) return(list(total = total, record_id = character(), current_value = character()))
  cur <- if (show) sprintf("CAST(%s AS VARCHAR)", ctx$q(col)) else "CAST(NULL AS VARCHAR)"
  d <- sr_duck_query(ctx$con, sprintf(
    "SELECT CAST(%s AS VARCHAR) AS sr_qy_id, %s AS sr_qy_cur FROM %s ORDER BY %s NULLS LAST, sr_qy_cur NULLS LAST LIMIT %.0f",
    ctx$rid, cur, from, ctx$rid, limit))
  list(total = total, record_id = as.character(d$sr_qy_id), current_value = as.character(d$sr_qy_cur))
}

# ---- paths ------------------------------------------------------------------------------------------

# `path` relative to the project folder, or NULL when it is not inside it (or climbs out with "..").
.sr_qy_rel <- function(project, path) {
  root <- sub("/+$", "", normalizePath(project$paths$dir, winslash = "/", mustWork = FALSE))
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  pre <- paste0(root, "/")
  if (!startsWith(tolower(p), tolower(pre)) || grepl("(^|/)[.][.](/|$)", p)) return(NULL)
  substring(p, nchar(pre) + 1L)
}

# ---- the sheet --------------------------------------------------------------------------------------

#' Write the data-query sheet of `table` (an .xlsx) and append `query.exported`.
#'
#' @param finding_ids ids of findings of the latest DQ run of `table`; the default is every finding decided
#'   as recheck (sr_action_decide) that is still in that run. No such finding is an error.
#' @param path an .xlsx file inside the project folder; the default is out/queries/<table>-<date>.xlsx.
#' @return the project with the event applied. The event holds the relative path, the SHA-256 of the file,
#'   the number of rows written (`rows`), the number there are (`total`) and whether the sheet was cut.
sr_query_sheet <- function(project, table, finding_ids = NULL,
                           path = file.path(sr_project_paths(project$paths$dir)$out, "queries",
                                            sprintf("%s-%s.xlsx", table, format(Sys.Date(), "%Y-%m-%d")))) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  .sr_store_check_table(table)
  if (!is.character(path) || length(path) != 1L || is.na(path) || !grepl("[.]xlsx$", path, ignore.case = TRUE)) {
    stop("path must be a single .xlsx file name", call. = FALSE)
  }
  rel <- .sr_qy_rel(project, path)
  if (is.null(rel)) stop("path must be inside the project folder (", project$paths$dir, ")", call. = FALSE)
  f <- sr_dq_findings(project, table)
  if (is.null(finding_ids)) {
    act <- project$state$actions %||% list()
    rc <- vapply(f$id, function(i) {
      a <- act[[i]]
      !is.null(a) && identical(a$table, table) && identical(a$action, "recheck")
    }, NA)
    finding_ids <- f$id[rc]
    if (!length(finding_ids)) {
      stop("no finding of table '", table, "' is decided as recheck; decide one with sr_action_decide() or pass finding_ids",
           call. = FALSE)
    }
  } else {
    if (!is.character(finding_ids) || !length(finding_ids) || anyNA(finding_ids) || anyDuplicated(finding_ids)) {
      stop("finding_ids must be distinct finding ids", call. = FALSE)
    }
    bad <- setdiff(finding_ids, f$id)
    if (length(bad)) {
      stop("unknown finding_id '", bad[1L], "': it is not in the latest DQ run of table '", table, "'", call. = FALSE)
    }
  }
  blind <- sr_blind(project)
  outcome_cols <- sr_role_cols(project, table, "outcome")
  ctx <- .sr_qy_context(project, table)

  parts <- list()
  total <- 0
  written <- 0
  for (id in finding_ids) {
    fr <- f[f$id == id, , drop = FALSE]
    r <- .sr_qy_finding_rows(ctx, fr, blind, outcome_cols, max(0, SR_QUERY_MAX_ROWS - written))
    total <- total + r$total
    n <- length(r$record_id)
    written <- written + n
    if (n) {
      parts[[length(parts) + 1L]] <- data.frame(record_id = r$record_id, column = fr$column, current_value = r$current_value,
                                                check = fr$check, stringsAsFactors = FALSE)
    }
  }
  if (!length(parts)) stop("the selected finding(s) match no rows", call. = FALSE)
  d <- do.call(rbind, parts)
  n <- nrow(d)
  q <- data.frame(query_id = sprintf("Q%05d", seq_len(n)), record_id = d$record_id, column = d$column,
                  current_value = d$current_value, check = d$check,
                  question = vapply(seq_len(n), function(i) .sr_qy_question(d$check[i], d$column[i]), ""),
                  corrected_value = rep(NA_character_, n), comment = rep(NA_character_, n), stringsAsFactors = FALSE)
  if (blind) {
    .sr_assert_blind(q, outcome_cols, setdiff(ctx$cols, outcome_cols))
    for (txt in unique(q$question)) .sr_sg_assert_text(project, table, txt)
  }
  truncated <- total > n
  sheets <- list(queries = q)
  if (truncated) {
    sheets$summary <- data.frame(
      item = c("table", "findings", "records in total", "rows in the queries sheet", "note"),
      value = c(table, as.character(length(finding_ids)), sprintf("%.0f", total), as.character(n),
                sprintf("the sheet holds the first %d rows; re-export after the first rows are answered", SR_QUERY_MAX_ROWS)),
      stringsAsFactors = FALSE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  existed <- file.exists(path)
  tmp <- paste0(path, ".tmp")
  ok <- tryCatch({
    writexl::write_xlsx(sheets, tmp)
    TRUE
  }, error = function(e) {
    unlink(tmp)
    stop("cannot write the query sheet: ", conditionMessage(e), call. = FALSE)
  })
  tryCatch(.sr_rename_retry(tmp, path), error = function(e) {
    unlink(tmp)
    stop(e)
  })
  rel <- .sr_qy_rel(project, normalizePath(path, winslash = "/", mustWork = TRUE))
  tryCatch(
    sr_project_append(project, "query.exported", list(
      table = table, path = rel, sha256 = sr_sha256_file(path), rows = n, total = total, truncated = truncated,
      finding_ids = as.list(finding_ids))),
    error = function(e) {
      if (!existed) unlink(path)      # no event: do not leave a new sheet the log does not know
      stop(e)
    })
}

# `query.exported` keeps every export, in order.
sr_reducer_register("query.exported", function(state, event) {
  p <- event$payload
  if (!is.character(p$table) || length(p$table) != 1L || is.na(p$table)) stop("query.exported needs a table", call. = FALSE)
  p$finding_ids <- as.list(as.character(unlist(p$finding_ids, use.names = FALSE)))
  state$queries <- c(state$queries %||% list(), list(p))
  state
})

# ---- the diff ---------------------------------------------------------------------------------------

#' The differences between two store tables, matched on `key` (default: the id-role columns of `old`; none
#' is an error). See the header for how rows and cells are compared.
#' @return list(added, removed, changed_rows, changed_cells = data.frame(column, n) for the columns with at
#'   least one changed cell, most first; schema_changes = list(added, removed, type_changed =
#'   data.frame(column, old_type, new_type)); key; old_rows, new_rows; duplicate_keys = list(old, new), the
#'   number of key values on more than one row).
sr_table_diff <- function(project, old, new, key = NULL) {
  .sr_store_check_table(old)
  .sr_store_check_table(new)
  if (is.null(key)) {
    key <- sr_role_cols(project, old, "id")
    if (!length(key)) {
      stop("table '", old, "' has no id role to match rows on; set one with sr_roles_set() or pass key", call. = FALSE)
    }
  }
  if (!is.character(key) || !length(key) || anyNA(key) || any(!nzchar(key)) || anyDuplicated(key)) {
    stop("key must be distinct column names", call. = FALSE)
  }
  spec <- .sr_join_spec(project, old, new, stats::setNames(key, key))
  con <- sr_duck(project)
  q <- function(x) .sr_join_q(con, x)
  O <- q(old)
  N <- q(new)
  common <- intersect(names(spec$ls), names(spec$rs))
  tchg <- common[toupper(spec$ls[common]) != toupper(spec$rs[common])]
  nonkey <- setdiff(common, key)
  sel <- paste(ifelse(common %in% tchg, sprintf("CAST(%s AS VARCHAR) AS %s", q(common), q(common)), q(common)),
               collapse = ", ")
  ord <- paste(q(common), collapse = ", ")
  kc <- q(key)
  keys <- paste(kc, collapse = ", ")
  not_na <- paste(paste(kc, "IS NOT NULL"), collapse = " AND ")
  eq <- function(a, b) paste(sprintf("%s.%s = %s.%s", a, kc, b, kc), collapse = " AND ")
  only_in <- function(x, y) {
    sprintf("(SELECT count(*) FROM (SELECT DISTINCT %s FROM %s WHERE %s) AS sr_a WHERE NOT EXISTS (SELECT 1 FROM %s AS sr_b WHERE %s))",
            keys, x, not_na, y, eq("sr_b", "sr_a"))
  }
  dups <- function(x) {
    sprintf("(SELECT count(*) FROM (SELECT %s FROM %s WHERE %s GROUP BY %s HAVING count(*) > 1) AS sr_d)", keys, x, not_na, keys)
  }
  r <- sr_duck_query(con, paste0(
    "SELECT (SELECT count(*) FROM ", O, ") AS old_rows, (SELECT count(*) FROM ", N, ") AS new_rows, ",
    only_in(N, O), " AS added, ", only_in(O, N), " AS removed, ", dups(O), " AS old_dups, ", dups(N), " AS new_dups"))
  num <- function(d, k) as.numeric(d[[k]][1L])

  both <- "sr_od.sr_rn IS NOT NULL AND sr_nd.sr_rn IS NOT NULL"
  side <- function(a, b) {
    sprintf("(SELECT sr_x.*, row_number() OVER (PARTITION BY %s ORDER BY %s) AS sr_rn FROM (SELECT %s FROM %s EXCEPT ALL SELECT %s FROM %s) AS sr_x)",
            keys, ord, sel, a, sel, b)
  }
  cells <- if (length(nonkey)) {
    paste0(", ", paste(sprintf("count(*) FILTER (WHERE %s AND sr_od.%s IS DISTINCT FROM sr_nd.%s) AS sr_c%d",
                               both, q(nonkey), q(nonkey), seq_along(nonkey)), collapse = ", "))
  } else ""
  c2 <- sr_duck_query(con, paste0(
    "SELECT count(*) FILTER (WHERE ", both, ") AS changed_rows", cells,
    " FROM ", side(O, N), " AS sr_od FULL OUTER JOIN ", side(N, O), " AS sr_nd ON ", eq("sr_od", "sr_nd"),
    " AND sr_od.sr_rn = sr_nd.sr_rn"))
  n <- vapply(seq_along(nonkey), function(i) num(c2, paste0("sr_c", i)), 0)
  hit <- n > 0
  cc <- data.frame(column = nonkey[hit], n = n[hit], stringsAsFactors = FALSE)
  cc <- cc[order(-cc$n, cc$column, method = "radix"), , drop = FALSE]
  rownames(cc) <- NULL
  tc <- data.frame(column = tchg, old_type = unname(spec$ls[tchg]), new_type = unname(spec$rs[tchg]), stringsAsFactors = FALSE)
  list(added = num(r, "added"), removed = num(r, "removed"), changed_rows = num(c2, "changed_rows"),
       changed_cells = cc,
       schema_changes = list(added = setdiff(names(spec$rs), names(spec$ls)), removed = setdiff(names(spec$ls), names(spec$rs)),
                             type_changed = tc),
       key = key, old_rows = num(r, "old_rows"), new_rows = num(r, "new_rows"),
       duplicate_keys = list(old = num(r, "old_dups"), new = num(r, "new_dups")))
}

# ---- the re-import ----------------------------------------------------------------------------------

# The diff as the event keeps it: arrays stay arrays, an empty one is [].
.sr_qy_diff_payload <- function(d) {
  list(added = d$added, removed = d$removed, changed_rows = d$changed_rows, changed_cells = d$changed_cells,
       schema_changes = list(added = as.list(d$schema_changes$added), removed = as.list(d$schema_changes$removed),
                             type_changed = d$schema_changes$type_changed),
       old_rows = d$old_rows, new_rows = d$new_rows, duplicate_keys = d$duplicate_keys)
}

#' Ingest a corrected source of `table` as the next version `<table>_r<k>`, diff it against the version that
#' was active, give it the roles and the dictionary, and make it the active version (`data.reimported`).
#'
#' @param path the corrected source file.
#' @param key columns to match rows on; the default is the id-role columns of the active version. With neither,
#'   an error is raised before anything is ingested.
#' @param ... passed to sr_ingest() (sheet, options, ...).
#' @return the project with the events applied. The diff is in `project$state$reimports[["<table>_r<k>"]]$diff`
#'   (and, with the full detail, from sr_table_diff()).
sr_reimport <- function(project, table, path, key = NULL, ...) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  .sr_store_check_table(table)
  if (!(table %in% sr_store_tables(project))) stop("unknown table: ", table, call. = FALSE)
  active <- project$state$tables[[table]]$active
  old <- if (is.character(active) && length(active) == 1L && active %in% sr_store_tables(project)) active else table
  if (is.null(key)) {
    key <- sr_role_cols(project, old, "id")
    if (!length(key)) {
      stop("table '", old, "' has no id role to match the corrected rows on; set one with sr_roles_set() or pass key",
           call. = FALSE)
    }
  }
  re <- paste0("^", table, "_r([0-9]+)$")
  existing <- union(names(project$state$tables %||% list()), sr_store_tables(project))
  k <- max(0L, as.integer(sub(re, "\\1", existing[grepl(re, existing)]))) + 1L
  new <- sprintf("%s_r%d", table, k)
  .sr_store_check_table(new)

  project <- sr_ingest(project, path, table = new, ...)
  diff <- sr_table_diff(project, old, new, key)

  roles <- sr_roles(project, old)
  keep <- names(roles) %in% .sr_roles_schema(project, new)$name
  if (any(keep)) project <- sr_roles_set(project, new, roles[keep])
  d <- sr_dict_get(project, old)
  if (!is.null(d)) project <- sr_dict_set(project, new, d, "reimport")

  sr_project_append(project, "data.reimported", c(
    list(table = table, old = old, new = new, version = k, key = as.list(key),
         roles_dropped = as.list(names(roles)[!keep]), store_hash = project$state$tables[[new]]$store_hash),
    list(diff = .sr_qy_diff_payload(diff))))
}

# `data.reimported` makes the new table the active version of `table` and keeps the diff by version.
sr_reducer_register("data.reimported", function(state, event) {
  p <- event$payload
  if (!is.character(p$table) || length(p$table) != 1L || is.na(p$table) ||
      !is.character(p$new) || length(p$new) != 1L || is.na(p$new)) {
    stop("data.reimported needs a table and its new version", call. = FALSE)
  }
  tabs <- state$tables %||% list()
  t <- tabs[[p$table]] %||% list(table = p$table)
  t$active <- p$new
  tabs[[p$table]] <- t
  state$tables <- tabs
  re <- state$reimports %||% list()
  re[[p$new]] <- p
  state$reimports <- re
  state
})
