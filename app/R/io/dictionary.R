# io/dictionary.R - data dictionaries: the shape, the generic CSV reader, the labels a source file
# carried (haven), storage as data/dict/<table>.json, and matching against a table's columns.
#
# A dictionary is a data frame, one row per variable:
#   variable, label, type, units, min, max, levels, ordered, missing_codes, source
#   type          numeric | integer | categorical | date | datetime | text | logical
#   min, max      character, interpreted by `type` (a date bound is ISO text, "" is "no bound")
#   levels        list of data.frame(code, label), in the order the levels were given
#   ordered       logical: the levels are ordinal (default FALSE; absent in input means FALSE)
#   missing_codes list of character vectors: codes that mean "missing" (e.g. -9)
#   source        where the row came from (csv, redcap, odm, source, ...)
# Later data-quality checks read the levels, the bounds and the missing codes from here.
#
# Storage: sr_dict_set() writes data/dict/<table>.json atomically and appends `dict.set` with the
# relative file, its SHA-256, the variable count and the source; state$dict[[table]] holds the same.
# sr_dict_get() re-reads the file and checks it against that SHA-256. REDCap readers: redcap_dict.R.

.sr_dict_types <- c("numeric", "integer", "categorical", "date", "datetime", "text", "logical")
.sr_dict_cols <- c("variable", "label", "type", "units", "min", "max", "levels", "ordered",
                   "missing_codes", "source")
.sr_dict_required <- setdiff(.sr_dict_cols, "ordered")

# ---- shape ----------------------------------------------------------------------------------------

.sr_dict_levels0 <- function() data.frame(code = character(), label = character(), stringsAsFactors = FALSE)

# A dictionary data frame from parallel vectors (levels and missing_codes are lists). No checks.
.sr_dict_build <- function(variable, label, type, units, min, max, levels, ordered, missing_codes, source) {
  d <- data.frame(variable = as.character(variable), label = as.character(label), type = as.character(type),
                  units = as.character(units), min = as.character(min), max = as.character(max),
                  stringsAsFactors = FALSE)
  d$levels <- unname(as.list(levels))
  d$ordered <- as.logical(ordered)
  d$missing_codes <- unname(as.list(missing_codes))
  d$source <- as.character(source)
  d
}

#' A dictionary with no rows.
sr_dict_empty <- function() {
  .sr_dict_build(character(), character(), character(), character(), character(), character(),
                 list(), logical(), list(), character())
}

# NA -> "", anything else as character.
.sr_dict_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

# One variable's levels as a clean data.frame(code, label), or an error naming the variable.
.sr_dict_norm_levels <- function(lv, variable) {
  if (is.null(lv)) return(.sr_dict_levels0())
  if (!is.data.frame(lv) || !all(c("code", "label") %in% names(lv))) {
    stop("levels of '", variable, "' must be a data frame with columns code and label", call. = FALSE)
  }
  code <- as.character(lv$code)
  label <- as.character(lv$label)
  if (anyNA(code) || any(!nzchar(code))) stop("levels of '", variable, "' has an empty code", call. = FALSE)
  if (anyDuplicated(code)) stop("levels of '", variable, "' has a duplicate code", call. = FALSE)
  label[is.na(label)] <- code[is.na(label)]
  data.frame(code = code, label = label, stringsAsFactors = FALSE)
}

#' Check a dictionary and return it in canonical form (the `ordered` column filled in when absent,
#' min / max as character, NA text as ""). Errors on a missing column, an empty or duplicate variable
#' name, a type outside numeric / integer / categorical / date / datetime / text / logical, or
#' malformed levels.
sr_dict_validate <- function(dict) {
  if (!is.data.frame(dict)) stop("a dictionary must be a data frame", call. = FALSE)
  miss <- setdiff(.sr_dict_required, names(dict))
  if (length(miss)) stop("dictionary is missing column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  n <- nrow(dict)
  variable <- as.character(dict$variable)
  if (anyNA(variable) || any(!nzchar(trimws(variable)))) stop("dictionary has an empty variable name", call. = FALSE)
  if (anyDuplicated(variable)) {
    stop("dictionary has a duplicate variable: ", paste(unique(variable[duplicated(variable)]), collapse = ", "),
         call. = FALSE)
  }
  type <- as.character(dict$type)
  bad <- unique(type[is.na(type) | !(type %in% .sr_dict_types)])
  if (length(bad)) {
    stop("unknown type: ", paste(format(bad), collapse = ", "), " (allowed: ", paste(.sr_dict_types, collapse = ", "),
         ")", call. = FALSE)
  }
  ordered <- if ("ordered" %in% names(dict)) dict$ordered else rep(FALSE, n)
  if (is.numeric(ordered)) ordered <- ordered != 0
  if (!is.logical(ordered)) stop("dictionary column 'ordered' must be logical", call. = FALSE)
  ordered[is.na(ordered)] <- FALSE
  levs <- if (n) lapply(seq_len(n), function(i) .sr_dict_norm_levels(dict$levels[[i]], variable[i])) else list()
  miss_codes <- if (n) {
    lapply(seq_len(n), function(i) {
      x <- as.character(unlist(dict$missing_codes[[i]], use.names = FALSE))
      x[!is.na(x)]
    })
  } else list()
  .sr_dict_build(variable, .sr_dict_chr(dict$label), type, .sr_dict_chr(dict$units), .sr_dict_chr(dict$min),
                 .sr_dict_chr(dict$max), levs, ordered, miss_codes, .sr_dict_chr(dict$source))
}

# Row-bind dictionaries (list columns kept as lists).
.sr_dict_bind <- function(parts) {
  parts <- Filter(function(d) !is.null(d) && nrow(d) > 0L, parts)
  if (!length(parts)) return(sr_dict_empty())
  pick <- function(col) unlist(lapply(parts, function(d) d[[col]]), use.names = FALSE)
  lst <- function(col) do.call(c, lapply(parts, function(d) unname(as.list(d[[col]]))))
  .sr_dict_build(pick("variable"), pick("label"), pick("type"), pick("units"), pick("min"), pick("max"),
                 lst("levels"), pick("ordered"), lst("missing_codes"), pick("source"))
}

# ---- reading text tables --------------------------------------------------------------------------

# A path is never a URL (read.csv / file() / read_xml would fetch one).
.sr_dict_check_path <- function(path, what = "the file") {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop(what, " must be a single path", call. = FALSE)
  }
  if (grepl("^[A-Za-z][A-Za-z0-9+.-]*://", path)) stop(what, " must be a local path, not a URL", call. = FALSE)
  if (!file.exists(path) || dir.exists(path)) stop(what, " not found: ", basename(path), call. = FALSE)
  invisible(path)
}

# A CSV as an all-character data frame (UTF-8, BOM tolerated, "#" is not a comment, empty cells are "").
.sr_dict_read_table <- function(path) {
  .sr_dict_check_path(path, "the dictionary file")
  bytes <- .sr_read_bytes(path)
  if (length(bytes) >= 3L && identical(bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) bytes <- bytes[-(1:3)]
  if (!length(bytes)) stop("the dictionary file is empty: ", basename(path), call. = FALSE)
  txt <- rawToChar(bytes)
  Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) stop("the dictionary file is not valid UTF-8: ", basename(path), call. = FALSE)
  tryCatch(
    utils::read.csv(text = txt, colClasses = "character", na.strings = character(), check.names = FALSE,
                    stringsAsFactors = FALSE, comment.char = "", strip.white = FALSE, encoding = "UTF-8"),
    error = function(e) stop("cannot read the dictionary file ", basename(path), ": ", conditionMessage(e),
                             call. = FALSE))
}

# "1=Male|2=Female" -> data.frame(code, label); "F|M" -> code = label; "" -> no levels.
.sr_dict_parse_levels <- function(s) {
  s <- if (is.na(s)) "" else s
  parts <- trimws(strsplit(s, "|", fixed = TRUE)[[1L]])
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(.sr_dict_levels0())
  eq <- regexpr("=", parts, fixed = TRUE)
  has <- eq > 0L
  code <- ifelse(has, trimws(substr(parts, 1L, eq - 1L)), parts)
  label <- ifelse(has, trimws(substring(parts, eq + 1L)), parts)
  data.frame(code = code, label = label, stringsAsFactors = FALSE)
}

# Case- and space-insensitive type names, and the three vocabulary aliases: id -> text, binary ->
# categorical, ordinal -> categorical + ordered.
.sr_dict_type_alias <- function(type) {
  t <- tolower(trimws(type))
  ordered <- t == "ordinal"
  t[t == "id"] <- "text"
  t[t %in% c("binary", "ordinal")] <- "categorical"
  list(type = t, ordered = ordered)
}

.sr_dict_truthy <- function(x) tolower(trimws(x)) %in% c("true", "t", "yes", "y", "1")

#' Read a generic dictionary CSV: columns variable (required), label, type, units, min, max, levels
#' ("1=Male|2=Female" or "F|M"), and optionally ordered and missing_codes ("-9|-8"). A type of `id`
#' reads as text, `binary` as categorical and `ordinal` as categorical with ordered = TRUE.
sr_dict_read_csv <- function(path) {
  x <- .sr_dict_read_table(path)
  names(x) <- tolower(trimws(names(x)))
  if (!("variable" %in% names(x))) stop("the dictionary CSV has no 'variable' column", call. = FALSE)
  n <- nrow(x)
  col <- function(nm, default = "") if (nm %in% names(x)) x[[nm]] else rep(default, n)
  variable <- trimws(col("variable"))
  al <- .sr_dict_type_alias(col("type", "text"))
  type <- al$type
  type[!nzchar(type)] <- "text"
  bad <- unique(type[!(type %in% .sr_dict_types)])
  if (length(bad)) stop("unknown type in the dictionary CSV: ", paste(bad, collapse = ", "), call. = FALSE)
  label <- col("label")
  label[!nzchar(label)] <- variable[!nzchar(label)]
  ordered <- al$ordered | (if ("ordered" %in% names(x)) .sr_dict_truthy(x$ordered) else FALSE)
  src <- col("source")
  src[!nzchar(src)] <- "csv"
  mc <- col("missing_codes")
  d <- .sr_dict_build(variable, label, type, col("units"), col("min"), col("max"),
                      lapply(col("levels"), .sr_dict_parse_levels), ordered,
                      lapply(mc, function(s) .sr_dict_parse_levels(s)$code), src)
  sr_dict_validate(d)
}

# ---- labels carried by the source file ------------------------------------------------------------

#' A dictionary from the labels a source file carried (the `dict_labels` of ingest, per column
#' list(label, values = data.frame(code, label, order), missing_codes, kind)): a column with value
#' labels is categorical, otherwise the type follows the column's kind; user-missing values become
#' missing_codes. A user-missing range (SPSS) is not carried over.
sr_dict_from_labels <- function(dict_labels) {
  nm <- names(dict_labels)
  if (!length(dict_labels)) return(sr_dict_empty())
  kind_type <- c(integer = "integer", double = "numeric", character = "text", logical = "logical",
                 date = "date", posixct = "datetime", time = "text", labelled = "categorical")
  rows <- lapply(seq_along(dict_labels), function(i) {
    e <- dict_labels[[i]]
    v <- nm[i]
    vals <- e$values
    has_vals <- is.data.frame(vals) && nrow(vals) > 0L
    lv <- .sr_dict_levels0()
    if (has_vals) {
      o <- if ("order" %in% names(vals)) order(vals$order) else seq_len(nrow(vals))
      lv <- data.frame(code = as.character(vals$code[o]), label = as.character(vals$label[o]),
                       stringsAsFactors = FALSE)
    }
    kind <- as.character(e$kind %||% "")[1L]
    type <- if (has_vals) "categorical" else if (!is.na(kind) && kind %in% names(kind_type)) kind_type[[kind]] else "text"
    lab <- e$label
    if (is.null(lab) || length(lab) != 1L || is.na(lab) || !nzchar(lab)) lab <- v
    mc <- as.character(unlist(e$missing_codes, use.names = FALSE))
    list(variable = v, label = lab, type = type, levels = lv, missing = mc[!is.na(mc)])
  })
  g <- function(f) vapply(rows, function(r) r[[f]], character(1))
  d <- .sr_dict_build(g("variable"), g("label"), g("type"), "", "", "", lapply(rows, function(r) r$levels),
                      FALSE, lapply(rows, function(r) r$missing), "source")
  sr_dict_validate(d)
}

# ---- storage --------------------------------------------------------------------------------------

.sr_dict_rel <- function(table) paste0("data/dict/", table, ".json")
.sr_dict_file <- function(project, table) {
  file.path(sr_project_paths(project$paths$dir)$data, "dict", paste0(table, ".json"))
}

# The JSON form of a validated dictionary (every array stays an array, however short).
.sr_dict_to_json <- function(dict) {
  n <- nrow(dict)
  vars <- lapply(seq_len(n), function(i) {
    lv <- dict$levels[[i]]
    list(variable = dict$variable[i], label = dict$label[i], type = dict$type[i], units = dict$units[i],
         min = dict$min[i], max = dict$max[i], ordered = dict$ordered[i], source = dict$source[i],
         levels = lapply(seq_len(nrow(lv)), function(j) list(code = lv$code[j], label = lv$label[j])),
         missing_codes = as.list(dict$missing_codes[[i]]))
  })
  .sr_json(list(format = "statrouter-dictionary", version = 1L, variables = vars))
}

.sr_dict_from_json <- function(x, table) {
  bad <- function(why) stop("the stored dictionary of table '", table, "' is invalid: ", why, call. = FALSE)
  if (!is.list(x) || !identical(x$format, "statrouter-dictionary") || !is.list(x$variables)) bad("unexpected format")
  v <- x$variables
  s <- function(f) vapply(v, function(r) {
    z <- r[[f]]
    if (is.null(z)) "" else as.character(z)
  }, character(1))
  lev <- lapply(v, function(r) {
    l <- r$levels
    if (!length(l)) return(.sr_dict_levels0())
    data.frame(code = vapply(l, function(z) as.character(z$code), character(1)),
               label = vapply(l, function(z) as.character(z$label), character(1)), stringsAsFactors = FALSE)
  })
  mc <- lapply(v, function(r) vapply(r$missing_codes %||% list(), as.character, character(1)))
  ord <- vapply(v, function(r) isTRUE(r$ordered), logical(1))
  d <- .sr_dict_build(s("variable"), s("label"), s("type"), s("units"), s("min"), s("max"), lev, ord, mc, s("source"))
  tryCatch(sr_dict_validate(d), error = function(e) bad(conditionMessage(e)))
}

#' Store `dict` as the dictionary of `table` (replacing any earlier one): validate, write
#' data/dict/<table>.json atomically, append `dict.set` {table, file, sha256, n, source}. `source` says
#' where it came from (csv, redcap, odm, source, manual). Returns the project.
sr_dict_set <- function(project, table, dict, source) {
  .sr_store_check_table(table)
  if (!is.character(source) || length(source) != 1L || is.na(source) || !nzchar(source)) {
    stop("source must be a single non-empty string", call. = FALSE)
  }
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  dict <- sr_dict_validate(dict)
  file <- .sr_dict_file(project, table)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  old <- if (file.exists(file)) .sr_read_bytes(file) else NULL
  bytes <- c(charToRaw(enc2utf8(.sr_dict_to_json(dict))), as.raw(0x0A))
  .sr_write_atomic(file, bytes)
  out <- tryCatch(
    sr_project_append(project, "dict.set", list(table = table, file = .sr_dict_rel(table), sha256 = .sr_sha256(bytes),
                                                n = nrow(dict), source = source)),
    error = function(e) {
      # the event is the commit: without it the file must not change what state says
      if (is.null(old)) unlink(file) else try(.sr_write_atomic(file, old), silent = TRUE)
      stop(e)
    })
  out
}

#' The stored dictionary of `table`, or NULL when it has none. The file is checked against the SHA-256
#' recorded in state; a missing, altered or unreadable file is an error naming the table.
sr_dict_get <- function(project, table) {
  .sr_store_check_table(table)
  e <- project$state$dict[[table]]
  if (is.null(e)) return(NULL)
  if (!identical(e$file, .sr_dict_rel(table))) {
    stop("the dictionary record of table '", table, "' points at an unexpected file", call. = FALSE)
  }
  file <- .sr_dict_file(project, table)
  if (!file.exists(file)) stop("the dictionary file of table '", table, "' is missing", call. = FALSE)
  bytes <- .sr_read_bytes(file)
  if (!identical(.sr_sha256(bytes), as.character(e$sha256))) {
    stop("the dictionary file of table '", table, "' does not match its recorded SHA-256 (it was changed ",
         "outside the app)", call. = FALSE)
  }
  txt <- rawToChar(bytes)
  Encoding(txt) <- "UTF-8"
  parsed <- tryCatch(.sr_parse_json(txt), error = function(err) {
    stop("the dictionary file of table '", table, "' is not valid JSON", call. = FALSE)
  })
  .sr_dict_from_json(parsed, table)
}

sr_reducer_register("dict.set", function(state, event) {
  p <- event$payload
  if (!is.character(p$table) || length(p$table) != 1L || is.na(p$table)) stop("dict.set needs a table", call. = FALSE)
  d <- state$dict %||% list()
  old <- d[[p$table]] %||% list()
  for (k in setdiff(names(p), "table")) old[k] <- list(p[[k]])
  d[[p$table]] <- old
  state$dict <- d
  state
})

# ---- matching -------------------------------------------------------------------------------------

#' Which names are in the table (from its stored schema), which in its dictionary: a data frame
#' (variable, in_data, in_dict) with the table's columns first, then the dictionary-only variables. A
#' table with no dictionary has in_dict FALSE throughout. Names match exactly.
sr_dict_match <- function(project, table) {
  m <- sr_store_manifest(project, table)
  data_names <- vapply(m$schema, function(s) s$name, character(1))
  d <- sr_dict_get(project, table)
  dict_names <- if (is.null(d)) character() else d$variable
  vars <- c(data_names, setdiff(dict_names, data_names))
  data.frame(variable = vars, in_data = vars %in% data_names, in_dict = vars %in% dict_names,
             stringsAsFactors = FALSE)
}
