# profile/recipe.R - the reversible cleaning recipe: a filter DSL compiled to SQL, recipe steps recorded
# as events (with undo and branching), and the compile / apply to a clean store table.
#
#   sr_where_sql(con, where, cols, types)   the filter DSL (the `where` of a DQ finding) -> a SQL predicate
#   sr_recipe_add(project, table, step)     append `recipe.step` (the step is validated first)
#   sr_recipe_undo(project, step_id)        append `recipe.undo`: the last active step (or step_id) of the branch is marked
#   sr_recipe_branch(project, name, from_step)   append `recipe.branch`: copy the active steps (no switch)
#   sr_recipe_checkout(project, name)       append `recipe.checkout`
#   sr_recipe_steps(project, branch)        the active steps of a branch
#   sr_recipe_sql(project, table)           the recipe of the current branch as ONE SQL query (code-first)
#   sr_recipe_apply(project, table, into)   materialise it as store table `into`, append `recipe.applied`
#   sr_recipe_counts(project, table)        rows before / after and cells changed per step
#
# The filter DSL (see dq.R): a leaf is list(col, op, value) with op in == != < <= > >= in not_in is_na
# not_na between; list(all = <filters>) and list(any = <filters>) combine; NULL is every row.
#
# Steps: list(id, op, args, reason, finding_id). op and args:
#   set_value     col, where, value            recode      col, map (old value -> new value; NA blanks)
#   convert_unit  col, where, factor, offset   set_missing col, where
#   drop_cases    where, confirm_outcome_dependent   (needs a reason and a filter)
#   drop_var      col                          (needs a reason; protected roles are refused)
#   rename        col, to                      (a column with any role is refused)
#   cast          col, type                    (TRY_CAST: a value that does not convert becomes NULL)
#
# The compiled query is a sequential chain: one SELECT layer per active step over the previous layer
# (WITH sr_l1 AS (...), sr_l2 AS (... FROM sr_l1) ...), so a later step sees the columns and values the
# earlier ones made. A value op is SELECT * REPLACE (CASE WHEN <where> THEN <new> ELSE <col> END AS <col>);
# drop_cases is WHERE NOT coalesce(<where>, FALSE), so a row whose filter is NULL (a missing value in
# the filtered column) is KEPT: a row is only dropped when the filter is TRUE. drop_var is EXCLUDE,
# rename is RENAME, cast is TRY_CAST. Every identifier is quoted and every literal quoted, never pasted.
#
# State: state$recipe = list(current, branches = list(<name> = list(table, steps = list(step + undone)))).
# A branch is bound to the table of its first active step; undoing every step unbinds it. Step ids are
# s<n>, unique across the project (a step copied into another branch keeps its id). The checks that
# need the roles (protected columns, outcome-dependent exclusion) run when the recipe is compiled, so a
# role set after a step was recorded is still honoured.
#
# Outcome-blind (one taint rule, .sr_rc_taint): while blind, a step is tainted when its col, or any column
# of its where, is in the outcome family of the table (sr_outcome_family_cols: by lineage, so a family
# column of the table a derived table came from counts), and every later step is tainted too, because its
# counts descend from the tainted one; on an outcome-dependent table every step is tainted. The counts of a
# tainted step are NA in sr_recipe_counts(), and sr_flow() and the Data tab hide them through the same
# rule. sr_recipe_apply() persists `outcome_dependent` (any step touches the family, or the source is
# dependent) on `recipe.applied`, so the clean table stays hidden whatever branch is current later.

SR_RECIPE_OPS <- c("set_value", "recode", "convert_unit", "set_missing", "drop_cases", "drop_var", "rename", "cast")
SR_RECIPE_CAST_TYPES <- c("INTEGER", "BIGINT", "DOUBLE", "VARCHAR", "DATE", "TIMESTAMP", "BOOLEAN")

.sr_rc_args <- list(
  set_value = c("col", "where", "value"), recode = c("col", "map"),
  convert_unit = c("col", "where", "factor", "offset"), set_missing = c("col", "where"),
  drop_cases = c("where", "confirm_outcome_dependent"), drop_var = "col", rename = c("col", "to"),
  cast = c("col", "type"))

# dbQuoteIdentifier, but always quoted (DuckDB leaves a plain name bare): the SQL reads the same everywhere.
.sr_rc_q <- function(con, x) {
  q <- as.character(DBI::dbQuoteIdentifier(con, x))
  ifelse(startsWith(q, "\""), q, paste0("\"", q, "\""))
}

# ---- literals ---------------------------------------------------------------------------------------

# Type family of a DuckDB column type: "num", "date", "bool" or "chr" (NA when the type is unknown).
.sr_rc_family <- function(type) {
  if (is.null(type) || length(type) != 1L || is.na(type)) return(NA_character_)
  t <- toupper(type)
  if (grepl("^(U?(TINY|SMALL|BIG|HUGE)?INT(EGER)?|FLOAT|REAL|DOUBLE|DECIMAL)([(].*[)])?$", t)) return("num")
  if (grepl("^(DATE|TIMESTAMP)", t)) return("date")
  if (identical(t, "BOOLEAN")) return("bool")
  "chr"
}

# A finite number as an exact-enough SQL literal (15 digits, 17 when 15 do not round-trip).
.sr_rc_num <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) stop("a number in a filter or step must be finite", call. = FALSE)
  s <- sprintf("%.15g", x)
  if (as.numeric(s) != x) s <- sprintf("%.17g", x)
  s
}

.sr_rc_date_re <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
.sr_rc_ts_re <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}(:[0-9]{2}([.][0-9]+)?)?$"

.sr_rc_is_date <- function(v) grepl(.sr_rc_date_re, v) && !is.na(as.Date(v, format = "%Y-%m-%d"))

# One value as a SQL literal for a column of family `fam`. NA fam (type unknown) goes by the value:
# an ISO date string is a DATE literal. `what` names the value in an error message.
.sr_rc_lit <- function(con, v, fam = NA_character_, what = "value") {
  if (inherits(v, "Date")) v <- format(v)
  if (is.null(v) || length(v) != 1L) stop("a ", what, " must be a single value", call. = FALSE)
  if (is.na(v)) stop("a missing value cannot be used as a ", what, " (use is_na / not_na, or set_missing)", call. = FALSE)
  if (is.logical(v)) {
    if (identical(fam, "chr")) return(as.character(DBI::dbQuoteString(con, if (v) "TRUE" else "FALSE")))
    if (identical(fam, "num")) return(if (v) "1" else "0")
    return(if (v) "TRUE" else "FALSE")
  }
  if (is.numeric(v)) {
    if (identical(fam, "date")) stop("a number cannot be compared with a date column", call. = FALSE)
    s <- .sr_rc_num(v)
    return(if (identical(fam, "chr")) as.character(DBI::dbQuoteString(con, s)) else s)
  }
  v <- as.character(v)
  quote <- function(x) as.character(DBI::dbQuoteString(con, x))
  if (identical(fam, "num")) {
    n <- suppressWarnings(as.numeric(v))
    if (is.na(n)) stop("'", v, "' is not a number, but the column is numeric", call. = FALSE)
    return(.sr_rc_num(n))
  }
  if (identical(fam, "bool")) {
    b <- tolower(v)
    if (!(b %in% c("true", "false"))) stop("'", v, "' is not TRUE or FALSE, but the column is logical", call. = FALSE)
    return(toupper(b))
  }
  if (identical(fam, "chr")) return(quote(v))
  is_ts <- grepl(.sr_rc_ts_re, v)
  if (identical(fam, "date")) {
    if (is_ts) return(paste0("TIMESTAMP ", quote(sub("T", " ", v, fixed = TRUE))))
    if (!.sr_rc_is_date(v)) stop("'", v, "' is not a date (yyyy-mm-dd), but the column is a date", call. = FALSE)
    return(paste0("DATE ", quote(v)))
  }
  if (.sr_rc_is_date(v)) return(paste0("DATE ", quote(v)))
  quote(v)
}

# ---- the filter DSL ---------------------------------------------------------------------------------

# The columns a filter refers to, at any depth (a lenient walk: validation is sr_where_sql's job).
.sr_rc_where_cols <- function(w) {
  if (!is.list(w)) return(character())
  nms <- names(w)
  if (!is.null(nms) && "col" %in% nms) return(as.character(w[["col"]]))
  unlist(lapply(w, .sr_rc_where_cols), use.names = FALSE) %||% character()
}

.sr_rc_value <- function(w) {
  v <- w[["value"]]
  if (is.list(v)) v <- unlist(v, use.names = FALSE)
  v
}

.sr_rc_where_node <- function(con, w, cols, types) {
  bad <- function(why) stop("not a valid filter: ", why, call. = FALSE)
  if (!is.list(w) || is.data.frame(w) || is.null(names(w)) || anyNA(names(w)) || any(!nzchar(names(w)))) {
    bad("a filter is list(col, op, value), list(all = <filters>) or list(any = <filters>)")
  }
  nms <- names(w)
  if (any(c("all", "any") %in% nms)) {
    if (length(nms) != 1L) bad("a combinator has exactly one of 'all' or 'any' and nothing else")
    kids <- w[[1L]]
    if (!is.list(kids) || !length(kids)) stop("the '", nms, "' combinator of a filter is empty", call. = FALSE)
    parts <- vapply(unname(kids), function(k) .sr_rc_where_node(con, k, cols, types), "")
    return(paste0("(", paste(parts, collapse = if (identical(nms, "all")) " AND " else " OR "), ")"))
  }
  if (!all(c("col", "op") %in% nms)) bad("a leaf needs 'col' and 'op'")
  extra <- setdiff(nms, c("col", "op", "value"))
  if (length(extra)) stop("unexpected field(s) in a filter leaf: ", paste(extra, collapse = ", "), call. = FALSE)
  col <- w[["col"]]
  op <- w[["op"]]
  if (!is.character(col) || length(col) != 1L || is.na(col) || !(col %in% cols)) {
    stop("unknown column in a filter: ", paste(format(col), collapse = ","), call. = FALSE)
  }
  if (!is.character(op) || length(op) != 1L || is.na(op) || !(op %in% .sr_where_ops)) {
    stop("unknown filter op: ", paste(format(op), collapse = ","), call. = FALSE)
  }
  q <- .sr_rc_q(con, col)
  if (op == "is_na") return(paste0("(", q, " IS NULL)"))
  if (op == "not_na") return(paste0("(", q, " IS NOT NULL)"))
  fam <- if (is.null(types)) NA_character_ else .sr_rc_family(types[col])
  v <- .sr_rc_value(w)
  # a number against a text column compares the number the text holds (TRY_CAST, NULL when it is not one),
  # as the data-quality checks read such a column
  if (identical(fam, "chr") && length(v) && is.numeric(v)) {
    q <- paste0("TRY_CAST(", q, " AS DOUBLE)")
    fam <- "num"
  }
  lit <- function(x) .sr_rc_lit(con, x, fam, "filter value")
  if (op %in% c("in", "not_in")) {
    if (!length(v)) return(if (op == "in") "(FALSE)" else "(TRUE)")
    return(paste0("(", q, if (op == "in") " IN (" else " NOT IN (", paste(vapply(v, lit, ""), collapse = ", "), "))"))
  }
  if (op == "between") {
    if (length(v) != 2L) stop("op 'between' takes two values (low and high)", call. = FALSE)
    return(paste0("(", q, " BETWEEN ", lit(v[[1L]]), " AND ", lit(v[[2L]]), ")"))
  }
  if (!length(v)) stop("op '", op, "' needs a value", call. = FALSE)
  if (length(v) != 1L) stop("op '", op, "' takes one value", call. = FALSE)
  sym <- switch(op, "==" = "=", "!=" = "<>", op)
  paste0("(", q, " ", sym, " ", lit(v[[1L]]), ")")
}

#' Compile a recipe filter (the `where` of a DQ finding) to a SQL predicate. `cols` are the valid column
#' names (an unknown column is an error, so a hostile name never reaches the SQL); `types` is an optional
#' named character vector (column -> DuckDB type): with it a text column always gets a text literal and
#' a date column a DATE literal, without it an ISO date string is a DATE literal. Identifiers are quoted
#' with dbQuoteIdentifier and literals with dbQuoteString. NULL is every row ("TRUE"). `in` / `not_in`
#' with an empty set are the constants (FALSE) / (TRUE). A comparison with NULL is NULL, as in SQL.
#' @return the predicate, a single string.
sr_where_sql <- function(con, where, cols, types = NULL) {
  if (!is.character(cols) || anyNA(cols)) stop("cols must be a character vector of column names", call. = FALSE)
  if (is.null(where)) return("TRUE")
  .sr_rc_where_node(con, where, cols, types)
}

# ---- state ------------------------------------------------------------------------------------------

.sr_rc_state <- function(state) {
  state$recipe %||% list(current = "main", branches = list(main = list(table = NULL, steps = list())))
}

.sr_rc_branch <- function(project, name = NULL) {
  rec <- .sr_rc_state(project$state)
  name <- name %||% rec$current
  if (!is.character(name) || length(name) != 1L || is.na(name) || is.null(rec$branches[[name]])) {
    stop("no such branch: ", paste(format(name), collapse = ","), call. = FALSE)
  }
  rec$branches[[name]]
}

# The active steps of a branch, without the `undone` flag.
.sr_rc_active <- function(br) {
  act <- Filter(function(s) !isTRUE(s$undone), br$steps)
  lapply(act, function(s) {
    s$undone <- NULL
    s
  })
}

#' The active steps (not undone) of a branch, in order; the current branch by default.
sr_recipe_steps <- function(project, branch = NULL) {
  .sr_rc_active(.sr_rc_branch(project, branch))
}

# The highest s<n> id used by any step of any branch.
.sr_rc_max_id <- function(rec) {
  ids <- unlist(lapply(rec$branches, function(b) vapply(b$steps, function(s) as.character(s$id), "")), use.names = FALSE)
  n <- suppressWarnings(as.integer(sub("^s", "", ids[grepl("^s[0-9]+$", ids)])))
  if (length(n)) max(n) else 0L
}

# ---- one step ---------------------------------------------------------------------------------------

# Check the shape of a step and fill in its id; returns the step as it is recorded.
.sr_rc_norm_step <- function(step, rec) {
  if (!is.list(step) || is.null(names(step))) stop("a recipe step is list(op, args, reason, finding_id)", call. = FALSE)
  op <- step$op
  if (!is.character(op) || length(op) != 1L || is.na(op) || !(op %in% SR_RECIPE_OPS)) {
    stop("unknown recipe op: ", paste(format(op), collapse = ","), " (known: ", paste(SR_RECIPE_OPS, collapse = ", "), ")",
         call. = FALSE)
  }
  args <- step$args %||% list()
  if (!is.list(args) || (length(args) && is.null(names(args)))) stop("args must be a named list", call. = FALSE)
  extra <- setdiff(names(args), .sr_rc_args[[op]])
  if (length(extra)) {
    stop("unexpected argument(s) for ", op, ": ", paste(extra, collapse = ", "), " (takes: ",
         paste(.sr_rc_args[[op]], collapse = ", "), ")", call. = FALSE)
  }
  if (op == "recode" && is.atomic(args$map) && !is.null(args$map)) args$map <- as.list(args$map)
  reason <- step$reason %||% ""
  if (!is.character(reason) || length(reason) != 1L || is.na(reason)) stop("reason must be a single string", call. = FALSE)
  fid <- step$finding_id
  if (is.null(fid) || length(fid) != 1L || is.na(fid)) fid <- NA_character_
  if (!is.na(fid) && !is.character(fid)) stop("finding_id must be a string or NA", call. = FALSE)
  id <- step$id
  if (is.null(id)) {
    id <- paste0("s", .sr_rc_max_id(rec) + 1L)
  } else {
    all_ids <- unlist(lapply(rec$branches, function(b) vapply(b$steps, function(s) as.character(s$id), "")), use.names = FALSE)
    if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]{0,39}$", id)) {
      stop("invalid step id", call. = FALSE)
    }
    if (id %in% all_ids) stop("step id '", id, "' is already used", call. = FALSE)
  }
  list(id = id, op = op, args = args, reason = trimws(reason), finding_id = fid)
}

.sr_rc_col_arg <- function(args, cols, op) {
  col <- args[["col"]]
  if (!is.character(col) || length(col) != 1L || is.na(col) || !(col %in% cols)) {
    stop("unknown column for ", op, ": ", paste(format(col), collapse = ","), call. = FALSE)
  }
  col
}

.sr_rc_need_reason <- function(step) {
  if (!nzchar(step$reason)) stop(step$op, " needs a reason", call. = FALSE)
}

# The compiled layer of one step over the columns `cols` / `types` that the previous layer produced.
# Returns list(select, filter, changed, nulls, cols, types): the SELECT list part (without FROM), the WHERE
# filter (drop_cases), a predicate over the PREVIOUS layer that is TRUE for each cell the step changes,
# one that is TRUE for each cell a cast turned into NULL, and the new columns and types.
.sr_rc_layer <- function(con, step, cols, types, roles) {
  op <- step$op
  args <- step$args
  q <- function(x) .sr_rc_q(con, x)
  wsql <- function() sr_where_sql(con, args[["where"]], cols, types)
  out <- list(select = "SELECT *", filter = NULL, changed = NULL, nulls = NULL, cols = cols, types = types)
  value_op <- function(col, new, changed) {
    out$select <<- sprintf("SELECT * REPLACE (%s AS %s)", new, q(col))
    out$changed <<- changed
  }

  if (op == "drop_cases") {
    .sr_rc_need_reason(step)
    if (is.null(args[["where"]])) stop("drop_cases needs a where filter (it would drop every row)", call. = FALSE)
    w <- wsql()
    outcome <- names(roles)[roles == "outcome"]
    hit <- intersect(.sr_rc_where_cols(args[["where"]]), outcome)
    if (length(hit) && !isTRUE(args[["confirm_outcome_dependent"]])) {
      stop("outcome-dependent exclusion: this filter uses the outcome column '", hit[1L], "'; excluding cases by ",
           "their outcome biases the analysis. Set confirm_outcome_dependent = TRUE to record it deliberately",
           call. = FALSE)
    }
    out$filter <- paste0("NOT coalesce(", w, ", FALSE)")
    return(out)
  }

  if (op == "drop_var") {
    .sr_rc_need_reason(step)
    col <- .sr_rc_col_arg(args, cols, op)
    role <- roles[col]
    if (!is.na(role) && role %in% SR_PROTECTED_ROLES) {
      stop("column '", col, "' has the protected role '", role, "' and cannot be dropped", call. = FALSE)
    }
    if (length(cols) < 2L) stop("drop_var would drop every column", call. = FALSE)
    out$select <- sprintf("SELECT * EXCLUDE (%s)", q(col))
    out$cols <- setdiff(cols, col)
    out$types <- types[out$cols]
    return(out)
  }

  col <- .sr_rc_col_arg(args, cols, op)
  qc <- q(col)
  fam <- .sr_rc_family(types[col])

  if (op == "rename") {
    to <- args[["to"]]
    if (!is.character(to) || length(to) != 1L || is.na(to) || !grepl("^[A-Za-z_][A-Za-z0-9_]{0,62}$", to)) {
      stop("invalid column name for rename: ", paste(format(to), collapse = ","),
           " (letters, digits and _, not starting with a digit)", call. = FALSE)
    }
    if (tolower(to) %in% tolower(cols)) stop("a column named '", to, "' already exists", call. = FALSE)
    if (col %in% names(roles)) {
      stop("column '", col, "' has the role '", roles[[col]], "'; roles refer to column names, so it cannot be renamed",
           call. = FALSE)
    }
    out$select <- sprintf("SELECT * RENAME (%s AS %s)", qc, q(to))
    out$cols <- ifelse(cols == col, to, cols)
    out$types <- stats::setNames(unname(types[cols]), out$cols)
    return(out)
  }

  if (op == "cast") {
    type <- args[["type"]]
    if (!is.character(type) || length(type) != 1L || is.na(type) || !(toupper(type) %in% SR_RECIPE_CAST_TYPES)) {
      stop("invalid cast type: ", paste(format(type), collapse = ","), " (one of ", paste(SR_RECIPE_CAST_TYPES, collapse = ", "),
           ")", call. = FALSE)
    }
    type <- toupper(type)
    tc <- sprintf("TRY_CAST(%s AS %s)", qc, type)
    out$select <- sprintf("SELECT * REPLACE (%s AS %s)", tc, qc)
    out$changed <- sprintf("(%s IS NOT NULL) AND (%s IS NOT NULL)", qc, tc)
    out$nulls <- sprintf("(%s IS NOT NULL) AND (%s IS NULL)", qc, tc)
    out$types[col] <- type
    return(out)
  }

  if (op == "set_missing") {
    w <- wsql()
    value_op(col, sprintf("CASE WHEN %s THEN NULL ELSE %s END", w, qc), sprintf("(%s) AND (%s IS NOT NULL)", w, qc))
    return(out)
  }

  if (op == "set_value") {
    w <- wsql()
    v <- args[["value"]]
    if (is.list(v)) v <- unlist(v, use.names = FALSE)
    if (is.null(v) || (length(v) == 1L && is.na(v))) stop("set_value needs a value (use set_missing to blank cells)", call. = FALSE)
    lit <- .sr_rc_lit(con, v, fam, "value")
    value_op(col, sprintf("CASE WHEN %s THEN %s ELSE %s END", w, lit, qc),
             sprintf("(%s) AND (%s IS DISTINCT FROM %s)", w, lit, qc))
    return(out)
  }

  if (op == "convert_unit") {
    if (!identical(fam, "num")) stop("convert_unit needs a numeric column, but '", col, "' is ", types[[col]], call. = FALSE)
    num <- function(x, what) {
      if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) stop(what, " must be a single finite number", call. = FALSE)
      x
    }
    f <- num(args[["factor"]], "factor")
    if (f == 0) stop("factor must not be zero", call. = FALSE)
    o <- num(args[["offset"]] %||% 0, "offset")
    w <- wsql()
    expr <- sprintf("%s * CAST(%s AS DOUBLE) + CAST(%s AS DOUBLE)", qc, .sr_rc_num(f), .sr_rc_num(o))
    value_op(col, sprintf("CASE WHEN %s THEN %s ELSE %s END", w, expr, qc),
             sprintf("(%s) AND ((%s) IS DISTINCT FROM %s)", w, expr, qc))
    return(out)
  }

  # recode
  map <- args[["map"]]
  keys <- names(map)
  if (!is.list(map) || !length(map) || is.null(keys) || anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys)) {
    stop("recode needs a map: a named list of old value -> new value (names non-empty and unique)", call. = FALSE)
  }
  whens <- vapply(seq_along(map), function(i) {
    v <- map[[i]]
    if (is.list(v)) v <- unlist(v, use.names = FALSE)
    new <- if (is.null(v) || (length(v) == 1L && is.na(v))) "NULL" else .sr_rc_lit(con, v, fam, "recode value")
    paste0("WHEN ", .sr_rc_lit(con, keys[i], fam, "recode key"), " THEN ", new)
  }, "")
  new <- sprintf("CASE %s %s ELSE %s END", qc, paste(whens, collapse = " "), qc)
  value_op(col, new, sprintf("(%s) IS DISTINCT FROM %s", new, qc))
  out
}

# ---- the chain --------------------------------------------------------------------------------------

# Compile `steps` over store table `table`: list(sql, chain, layers, cols, src); `chain` is the WITH clause
# alone ("" for no step). Each layer is
# list(id, op, prev, name, changed, nulls) with the quoted names of the layer before and of itself.
.sr_rc_compile <- function(project, table, steps) {
  con <- sr_duck(project)
  m <- sr_store_manifest(project, table)
  cols <- vapply(m$schema, function(s) s$name, "")
  types <- stats::setNames(vapply(m$schema, function(s) s$type, ""), cols)
  roles <- sr_roles(project, table)
  src <- .sr_rc_q(con, table)
  prev <- src
  ctes <- character()
  layers <- list()
  for (i in seq_along(steps)) {
    st <- steps[[i]]
    L <- tryCatch(.sr_rc_layer(con, st, cols, types, roles), error = function(e) {
      stop("recipe step ", st$id, " (", st$op, "): ", conditionMessage(e), call. = FALSE)
    })
    name <- .sr_rc_q(con, sprintf("sr_l%d", i))
    ctes <- c(ctes, sprintf("%s AS (%s FROM %s%s)", name, L$select, prev,
                            if (is.null(L$filter)) "" else paste0(" WHERE ", L$filter)))
    layers[[i]] <- list(id = st$id, op = st$op, prev = prev, name = name, changed = L$changed, nulls = L$nulls)
    prev <- name
    cols <- L$cols
    types <- L$types
  }
  chain <- if (length(ctes)) paste0("WITH ", paste(ctes, collapse = ",\n")) else ""
  sql <- if (length(ctes)) paste0(chain, "\nSELECT * FROM ", prev) else paste0("SELECT * FROM ", src)
  list(sql = sql, chain = chain, layers = layers, cols = cols, src = src)
}

# The compile of the current branch's active steps over `table`, after checking the table and the branch.
.sr_rc_current <- function(project, table) {
  .sr_store_check_table(table)
  if (!(table %in% sr_store_tables(project))) stop("table '", table, "' is not in the store", call. = FALSE)
  br <- .sr_rc_branch(project)
  steps <- .sr_rc_active(br)
  if (length(steps) && !identical(br$table, table)) {
    stop("the current branch is bound to table '", br$table, "', not '", table, "'", call. = FALSE)
  }
  c(.sr_rc_compile(project, table, steps), list(branch = .sr_rc_state(project$state)$current, steps = steps))
}

#' The recipe of the current branch over `table` as ONE SQL query over the store view: a WITH chain with
#' one layer per active step (see the header). With no active step it is SELECT * FROM <table>. Pure: it
#' reads the manifest and the roles, runs nothing and records nothing. Errors if the branch is bound to
#' another table, or if a recorded step no longer passes its checks (a protected role, an outcome-dependent
#' exclusion that was not confirmed).
sr_recipe_sql <- function(project, table) {
  .sr_rc_current(project, table)$sql
}

# ---- recording --------------------------------------------------------------------------------------

#' Validate a step against the current branch and record it (`recipe.step`). The whole recipe including the
#' new step is compiled first, so a bad column, filter, role or type errors here and nothing is recorded.
#' A branch is bound to the table of its first step: a step for another table errors.
#' @param step list(op, args, reason = "", finding_id = NA, id = <generated>)
#' @return the project with the event applied.
sr_recipe_add <- function(project, table, step) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  .sr_store_check_table(table)
  if (!(table %in% sr_store_tables(project))) stop("table '", table, "' is not in the store", call. = FALSE)
  rec <- .sr_rc_state(project$state)
  br <- rec$branches[[rec$current]]
  if (!is.null(br$table) && !identical(br$table, table)) {
    stop("the current branch '", rec$current, "' is bound to table '", br$table, "', not '", table,
         "'; check out or create another branch for it", call. = FALSE)
  }
  st <- .sr_rc_norm_step(step, rec)
  .sr_rc_compile(project, table, c(.sr_rc_active(br), list(st)))
  sr_project_append(project, "recipe.step", list(table = table, branch = rec$current, step = st))
}

#' Undo the last active step of the current branch (`recipe.undo`): it is marked undone, not deleted.
#' With `step_id`, undo that active step instead (the event names it, so a replay undoes the same step);
#' the steps after it must still compile without it, else nothing is recorded.
sr_recipe_undo <- function(project, step_id = NULL) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  rec <- .sr_rc_state(project$state)
  br <- rec$branches[[rec$current]]
  act <- .sr_rc_active(br)
  if (!length(act)) stop("nothing to undo on branch '", rec$current, "'", call. = FALSE)
  ids <- vapply(act, function(s) as.character(s$id), "")
  target <- ids[length(ids)]
  if (!is.null(step_id)) {
    if (!is.character(step_id) || length(step_id) != 1L || is.na(step_id) || !(step_id %in% ids)) {
      stop("'", paste(format(step_id), collapse = ","), "' is not an active step of branch '", rec$current, "'", call. = FALSE)
    }
    target <- step_id
    if (target != ids[length(ids)]) {
      tryCatch(.sr_rc_compile(project, br$table, act[ids != target]), error = function(e) {
        stop("cannot undo step ", target, ": the steps after it no longer work without it (", conditionMessage(e), ")",
             call. = FALSE)
      })
    }
  }
  sr_project_append(project, "recipe.undo", list(branch = rec$current, step_id = target))
}

.sr_rc_branch_re <- "^[A-Za-z][A-Za-z0-9_-]{0,39}$"

#' Create branch `name` (`recipe.branch`) holding a copy of the current branch's active steps up to and
#' including `from_step` (all of them when NULL). It does NOT switch to it: use sr_recipe_checkout().
sr_recipe_branch <- function(project, name, from_step = NULL) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  if (!is.character(name) || length(name) != 1L || is.na(name) || !grepl(.sr_rc_branch_re, name)) {
    stop("invalid branch name (letters, digits, _ and -, starting with a letter): ", paste(format(name), collapse = ","),
         call. = FALSE)
  }
  rec <- .sr_rc_state(project$state)
  if (!is.null(rec$branches[[name]])) stop("branch '", name, "' already exists", call. = FALSE)
  if (!is.null(from_step)) {
    ids <- vapply(.sr_rc_active(rec$branches[[rec$current]]), function(s) as.character(s$id), "")
    if (!is.character(from_step) || length(from_step) != 1L || is.na(from_step) || !(from_step %in% ids)) {
      stop("from_step ", paste(format(from_step), collapse = ","), " is not an active step of branch '", rec$current, "'",
           call. = FALSE)
    }
  }
  sr_project_append(project, "recipe.branch", list(name = name, from = rec$current, from_step = from_step))
}

#' Make branch `name` the current one (`recipe.checkout`).
sr_recipe_checkout <- function(project, name) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  .sr_rc_branch(project, name)
  sr_project_append(project, "recipe.checkout", list(name = name))
}

sr_reducer_register("recipe.step", function(state, event) {
  p <- event$payload
  rec <- .sr_rc_state(state)
  br <- rec$branches[[rec$current]]
  st <- p$step
  st$reason <- st$reason %||% ""
  st$finding_id <- if (is.null(st$finding_id)) NA_character_ else as.character(st$finding_id)
  st$undone <- FALSE
  if (is.null(br$table)) br$table <- p$table
  br$steps[[length(br$steps) + 1L]] <- st
  rec$branches[[rec$current]] <- br
  state$recipe <- rec
  state
})

sr_reducer_register("recipe.undo", function(state, event) {
  rec <- .sr_rc_state(state)
  br <- rec$branches[[rec$current]]
  open <- which(!vapply(br$steps, function(s) isTRUE(s$undone), NA))
  if (length(open)) {
    # the event names the step it undid (older events always name the last active one)
    sid <- event$payload$step_id
    k <- length(open)
    if (is.character(sid) && length(sid) == 1L && !is.na(sid)) {
      m <- match(sid, vapply(br$steps[open], function(s) as.character(s$id), ""))
      if (!is.na(m)) k <- m
    }
    br$steps[[open[k]]]$undone <- TRUE
    if (length(open) == 1L) br["table"] <- list(NULL)       # no active step left: the branch is unbound
  }
  rec$branches[[rec$current]] <- br
  state$recipe <- rec
  state
})

sr_reducer_register("recipe.branch", function(state, event) {
  p <- event$payload
  rec <- .sr_rc_state(state)
  from <- rec$branches[[rec$current]]
  act <- Filter(function(s) !isTRUE(s$undone), from$steps)
  if (!is.null(p$from_step)) {
    k <- match(p$from_step, vapply(act, function(s) as.character(s$id), ""))
    if (!is.na(k)) act <- act[seq_len(k)]
  }
  act <- lapply(act, function(s) {
    s$undone <- FALSE
    s
  })
  nb <- list(table = if (length(act)) from$table else NULL, steps = unname(act))
  rec$branches[[p$name]] <- nb
  state$recipe <- rec
  state
})

sr_reducer_register("recipe.checkout", function(state, event) {
  rec <- .sr_rc_state(state)
  rec$current <- event$payload$name
  state$recipe <- rec
  state
})

# ---- apply ------------------------------------------------------------------------------------------

#' Materialise the current branch's recipe over `table` as the store table `into` (default `<table>_clean`)
#' and append `recipe.applied`. The table gets format "recipe", `recipe_sql` (the query of sr_recipe_sql) and
#' `recipe_hash` (its SHA-256). `into` may be a table this function made from the same source before: it is
#' replaced atomically (apply after an undo, or from another branch). Any other existing name is refused.
#' The rows are written in a total order (ORDER BY every column), as a join's are, so the same recipe over
#' the same data gives the same store hash. The roles and the dictionary of `table` reach `into` with the
#' recipe's renames and drops applied (see .sr_rc_carry), recorded in the same `recipe.applied` event.
#' @return the project with the event applied.
sr_recipe_apply <- function(project, table, into = paste0(table, "_clean")) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  .sr_store_check_table(table)
  .sr_store_check_table(into)
  if (identical(into, table)) stop("into must differ from the source table", call. = FALSE)
  cur <- .sr_rc_current(project, table)
  carry <- .sr_rc_carry(project, table, into, cur$steps)
  old <- project$state$tables[[into]]
  replace <- identical(old$format, "recipe") && identical(as.character(unlist(old$derived_from)), table)
  if (!replace && (into %in% sr_store_tables(project) || dir.exists(sr_store_dir(project, into)) ||
                   !is.null(old) || !is.null(project$state$joins[[into]]))) {
    stop("table '", into, "' already exists", call. = FALSE)
  }
  stage <- file.path(sr_project_paths(project$paths$dir)$data, "ingest", into)
  if (dir.exists(stage) || file.exists(stage)) {
    stop("table '", into, "' has a staging directory in data/ingest (an unfinished ingest?); resume or ",
         "remove it, or choose another name", call. = FALSE)
  }
  sr_store_view(project, table)
  con <- sr_duck(project)
  if (replace) sr_duck_exec(con, paste("DROP VIEW IF EXISTS", .sr_rc_q(con, into)))   # a stale view over the old parquet
  ck <- .sr_ck_new(project, into, NULL, NULL, 0)
  if (!isTRUE(dir.create(ck$store_dir, recursive = TRUE, showWarnings = FALSE))) {
    stop("cannot create directory: ", ck$store_dir, call. = FALSE)
  }
  on.exit(unlink(ck$ingest_dir, recursive = TRUE), add = TRUE)
  part <- .sr_part_path(ck)
  rows <- .sr_col_copy(con, from = paste0("(", cur$sql, ") AS sr_rc"), to = part, what = "the recipe",
                       order = paste(.sr_rc_q(con, cur$cols), collapse = ", "))
  .sr_part_commit(ck, part, rows)
  schema <- .sr_col_schema(con, ck)
  parts <- lapply(ck$parts, function(p) list(file = p$file, rows = p$rows, sha256 = p$sha256))
  manifest <- .sr_store_install(ck, list(table = into, parts = parts, schema = schema, rows = rows))
  store_hash <- sr_store_hash(manifest)
  payload <- list(
    into = into, table = table, branch = cur$branch, sql = cur$sql, recipe_hash = .sr_sha256(cur$sql),
    steps = cur$steps, store_hash = store_hash, outcome_dependent = any(.sr_rc_dep(project, table, cur$steps)),
    name_map = .sr_rc_name_map(table, cur$steps),
    store = list(dir = paste0("data/store/", into), rows = rows, ncol = length(schema), schema = schema,
                 parts = parts))
  if (!is.null(carry$roles)) {
    payload$roles <- list(columns = as.list(names(carry$roles)), roles = as.list(unname(carry$roles)))
  }
  dw <- NULL
  tryCatch({
    if (!is.null(carry$dict)) {
      dw <- .sr_dict_write(project, into, carry$dict, "recipe")
      payload$dict <- dw$record
    }
    sr_project_append(project, "recipe.applied", payload)
  }, error = function(e) {
    if (!is.null(dw)) .sr_dict_restore(dw)
    if (!replace) unlink(sr_store_dir(project, into), recursive = TRUE)   # no event: do not leave the table behind
    stop(e)
  })
}

# Per step: TRUE when the step reads or writes an outcome-family column (its col, or a column of its
# where). A rename of a family column makes the new name a family column for the later steps.
.sr_rc_steps_touch <- function(steps, fam) {
  fam <- as.character(unlist(fam, use.names = FALSE))
  out <- logical(length(steps))
  for (i in seq_along(steps)) {
    st <- steps[[i]]
    a <- st$args
    col <- as.character(unlist(a[["col"]], use.names = FALSE))
    used <- c(col, .sr_rc_where_cols(a[["where"]]))
    out[i] <- any(used %in% fam)
    if (identical(as.character(unlist(st$op)), "rename") && length(col) && col[1L] %in% fam) {
      fam <- c(fam, as.character(unlist(a[["to"]], use.names = FALSE)))
    }
  }
  out
}

# Per step of `steps` over `table`: TRUE when its rows or cells depend on the outcome (the step or an
# earlier one touches the family, or the table itself is outcome-dependent). Fails closed: every step is
# dependent when the lineage cannot be followed.
.sr_rc_dep <- function(project, table, steps) {
  n <- length(steps)
  if (!n) return(logical())
  fam <- tryCatch(sr_outcome_family_cols(project, table), error = function(e) NULL)
  base <- tryCatch(sr_outcome_dependent(project, table), error = function(e) TRUE)
  if (is.null(fam) || isTRUE(base)) return(rep(TRUE, n))
  cumsum(.sr_rc_steps_touch(steps, fam)) > 0L
}

#' The outcome-blind taint of the recipe steps `steps` over `table` (see the header): while blind, TRUE for
#' a step whose counts are an outcome statistic. sr_recipe_counts(), sr_flow() and the Data tab use it.
.sr_rc_taint <- function(project, table, steps) {
  sr_blind(project) & .sr_rc_dep(project, table, steps)
}

# The renames of `steps` as name-map entries list(from = table, col = <original>, to = <final name>).
.sr_rc_name_map <- function(table, steps) {
  cur <- character()
  for (st in steps) {
    if (!identical(st$op, "rename")) next
    col <- st$args[["col"]]
    to <- st$args[["to"]]
    k <- match(col, cur)
    if (is.na(k)) cur[[col]] <- to else cur[k] <- to
  }
  lapply(names(cur), function(o) list(from = table, col = o, to = unname(cur[[o]])))
}

# Apply the renames and drops of `steps`, in order, to a vector named by columns (roles) or by variables:
# the names after the recipe, a dropped column removed.
.sr_rc_carry_names <- function(x, steps) {
  for (st in steps) {
    col <- st$args[["col"]]
    if (st$op == "drop_var") {
      x <- x[names(x) != col]
    } else if (st$op == "rename") {
      names(x)[names(x) == col] <- st$args[["to"]]
    }
  }
  x
}

# What `into` gets from `table`: list(roles, dict), the roles and the dictionary with the recipe's renames and
# drops applied (the checks at add time mean a role-assigned column is never renamed and a protected one never
# dropped, so in practice only unprotected columns vanish). sr_recipe_apply records both in `recipe.applied`
# (no separate roles.set / dict.set event). `roles` is NULL when neither table has roles (otherwise it
# replaces the whole map, empty included); `dict` is NULL when `table` has no dictionary. Applying again over
# the same source leaves the same end state. A cast step does not touch the `type` of a dictionary entry.
.sr_rc_carry <- function(project, table, into, steps) {
  roles <- .sr_rc_carry_names(sr_roles(project, table), steps)
  if (!length(roles) && !length(sr_roles(project, into))) roles <- NULL
  d <- sr_dict_get(project, table)
  if (!is.null(d)) {
    kept <- .sr_rc_carry_names(stats::setNames(d$variable, d$variable), steps)   # names = new, values = old
    d <- d[d$variable %in% unname(kept), , drop = FALSE]
    d$variable <- names(kept)[match(d$variable, unname(kept))]
    rownames(d) <- NULL
    d <- sr_dict_validate(d)
  }
  list(roles = roles, dict = d)
}

# `recipe.applied` adds (or replaces) the clean table: format "recipe", no source file, derived from `table`.
sr_reducer_register("recipe.applied", function(state, event) {
  p <- event$payload
  into <- p$into
  if (!is.character(into) || length(into) != 1L || is.na(into)) stop("recipe.applied needs a target table", call. = FALSE)
  tabs <- state$tables %||% list()
  t <- list(table = into, format = "recipe")
  t["source"] <- list(NULL)
  t$store <- p$store
  t$store_hash <- p$store_hash
  t$derived_from <- list(p$table)
  t$recipe_sql <- p$sql
  t$recipe_hash <- p$recipe_hash
  t$recipe_branch <- p$branch
  t$recipe_steps <- p$steps
  t$outcome_dependent <- isTRUE(p$outcome_dependent)
  t$name_map <- p$name_map
  tabs[[into]] <- t
  state$tables <- tabs
  # older logs have neither: their roles.set / dict.set events follow and set the same state
  if (!is.null(p$roles)) state <- .sr_roles_state_set(state, into, p$roles)
  if (!is.null(p$dict)) state <- .sr_dict_state_set(state, into, p$dict)
  state
})

# ---- counts -----------------------------------------------------------------------------------------

#' Per active step of the current branch over `table`: rows before and after, the cells it changed
#' (value ops: cells whose value differs afterwards; cast: cells that converted; drop_cases, drop_var and
#' rename: 0) and, for cast only, the non-NULL cells that became NULL (`new_nulls`; NA for other ops).
#' Computed by DuckDB over the store view; feeds the flow diagram. While blind, a tainted step (see
#' .sr_rc_taint: it, or an earlier step, touches the outcome family) has NA for every count.
#' @return data.frame(step_id, rows_before, rows_after, cells_changed, new_nulls), one row per step.
sr_recipe_counts <- function(project, table) {
  cur <- .sr_rc_current(project, table)
  n <- length(cur$layers)
  out <- data.frame(step_id = character(), rows_before = numeric(), rows_after = numeric(),
                    cells_changed = numeric(), new_nulls = numeric(), stringsAsFactors = FALSE)
  if (!n) return(out)
  sr_store_view(project, table)
  con <- sr_duck(project)
  cnt <- function(from, where = NULL) {
    paste0("(SELECT count(*) FROM ", from, if (is.null(where)) "" else paste0(" WHERE ", where), ")")
  }
  sel <- c(sprintf("%s AS r0", cnt(cur$src)))
  for (i in seq_len(n)) {
    L <- cur$layers[[i]]
    sel <- c(sel, sprintf("%s AS r%d", cnt(L$name), i),
             sprintf("%s AS c%d", if (is.null(L$changed)) "0" else cnt(L$prev, L$changed), i),
             sprintf("%s AS n%d", if (is.null(L$nulls)) "NULL" else cnt(L$prev, L$nulls), i))
  }
  d <- sr_duck_query(con, paste0(cur$chain, "\nSELECT ", paste(sel, collapse = ", ")))
  g <- function(k) as.numeric(d[[k]][1L])
  out <- data.frame(step_id = vapply(cur$layers, function(L) L$id, ""),
                    rows_before = vapply(seq_len(n), function(i) g(sprintf("r%d", i - 1L)), numeric(1)),
                    rows_after = vapply(seq_len(n), function(i) g(sprintf("r%d", i)), numeric(1)),
                    cells_changed = vapply(seq_len(n), function(i) g(sprintf("c%d", i)), numeric(1)),
                    new_nulls = vapply(seq_len(n), function(i) g(sprintf("n%d", i)), numeric(1)),
                    stringsAsFactors = FALSE)
  taint <- .sr_rc_taint(project, table, cur$steps)
  for (k in c("rows_before", "rows_after", "cells_changed", "new_nulls")) out[[k]][taint] <- NA_real_
  out
}
