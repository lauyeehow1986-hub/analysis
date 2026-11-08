# router/state.R - the metadata-only state a router question sees, column-name masking, the token
# budget and the payload preview.
#
# The state is plain text: the node, the research question (PICO), the table and its row count, the
# node's facts and, for a question that needs them, one line per column (name, label, type, role and
# a bucketed distinct count). It never holds a cell value of any column. Person counts below 5 are
# suppressed; distinct counts are not person counts, so they are bucketed instead.

SR_ROUTER_MAX_TOKENS <- 8000L

#' Person count as text: "<5" for 1-4, otherwise the number.
.sr_router_count <- function(n) {
  txt <- format(n, scientific = FALSE, trim = TRUE)
  txt[is.na(n)] <- "NA"
  ifelse(!is.na(n) & n >= 1 & n <= 4, "<5", txt)
}

#' Distinct-value count as a bucket: "1", "2", "3-9" or "10+" (an empty column is "0").
.sr_router_bucket <- function(k) {
  k[is.na(k)] <- 0
  ifelse(k < 1, "0", ifelse(k < 2, "1", ifelse(k < 3, "2", ifelse(k < 10, "3-9", "10+"))))
}

# One line of text: every newline becomes a space; NA is "".
.sr_router_oneline <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  gsub("\r\n|\r|\n", " ", x)
}

# A name or label inside a `name=...; label=...` line: single-line, with ; and = escaped.
.sr_router_esc <- function(x) {
  x <- .sr_router_oneline(x)
  x <- gsub(";", "\\;", x, fixed = TRUE)
  gsub("=", "\\=", x, fixed = TRUE)
}

# The columns with their real names and labels, in schema order, keyed c001, c002, ...
.sr_router_columns_raw <- function(project, table) {
  sc <- .sr_roles_schema(project, table)
  n <- nrow(sc)
  roles <- project$state$roles[[table]] %||% stats::setNames(character(), character())
  role <- if (n) unname(roles[sc$name]) else character()
  role[is.na(role)] <- "unassigned"
  data.frame(key = sprintf("c%03d", seq_len(n)), name = sc$name,
             label = .sr_router_oneline(.sr_roles_labels(project, table, sc$name)), type = sc$type,
             role = as.character(role), distinct = .sr_roles_distinct(project, table, sc$name),
             stringsAsFactors = FALSE)
}

#' The columns of a store table as data.frame(key, name, label, type, role, distinct), in schema order.
#' `key` is c001, c002, ...; when `mask` is TRUE the name is the key and the label is "".
sr_router_columns <- function(project, table, mask = sr_opt("router_mask", FALSE)) {
  cols <- .sr_router_columns_raw(project, table)
  if (isTRUE(as.logical(mask))) {
    cols$name <- cols$key
    cols$label <- rep("", nrow(cols))
  }
  cols
}

# Replace whole-token occurrences of the real column names in `x` by their keys: case-sensitive, longest
# name first, a token boundary being any character outside [A-Za-z0-9_.] (or the string's ends). One pass,
# so a key that equals another column's name is never replaced twice.
.sr_router_mask_text <- function(x, raw) {
  x <- as.character(x)
  if (!length(x) || !nrow(raw)) return(x)
  nm <- raw$name[nzchar(raw$name)]
  nm <- nm[order(-nchar(nm), seq_along(nm))]
  nm <- nm[!duplicated(nm)]
  if (!length(nm)) return(x)
  esc <- gsub("([][{}()*+?.^$|\\\\])", "\\\\\\1", nm)
  re <- paste0("(?<![A-Za-z0-9_.])(?:", paste(esc, collapse = "|"), ")(?![A-Za-z0-9_.])")
  vapply(x, function(s) {
    if (is.na(s)) return(s)
    m <- gregexpr(re, s, perl = TRUE)
    hit <- regmatches(s, m)
    if (length(hit[[1L]])) {
      regmatches(s, m) <- list(raw$key[match(hit[[1L]], raw$name)])
    }
    s
  }, "", USE.NAMES = FALSE)
}

# The real columns of the context's table when masking applies, else NULL.
.sr_router_mask_cols <- function(ctx, mask) {
  if (!mask || is.na(ctx$table)) return(NULL)
  .sr_router_columns_raw(ctx$project, ctx$table)
}

# One `fact.<name>: <value>` line per fact. With `mask`, the facts column_name and column_label of a
# column-scope node show the column key and "" (the input hash still hashes the real values).
.sr_router_fact_lines <- function(ctx, mask, raw = NULL) {
  nd <- ctx$node
  facts <- if (is.null(nd$facts)) list() else nd$facts(ctx)
  if (!length(facts)) return(character())
  if (mask) {
    key <- NA_character_
    if (!is.null(raw) && !is.na(ctx$column)) key <- raw$key[match(ctx$column, raw$name)]
    if ("column_name" %in% names(facts)) facts$column_name <- if (is.na(key)) "" else key
    if ("column_label" %in% names(facts)) facts$column_label <- ""
  }
  vals <- vapply(facts, function(v) {
    v <- as.character(unlist(v, use.names = FALSE))
    if (!is.null(raw)) v <- .sr_router_mask_text(v, raw)
    paste(v, collapse = ", ")
  }, "")
  paste0("fact.", .sr_router_oneline(names(facts)), ": ", .sr_router_oneline(vals))
}

# One `column.<key>: ...` line per column.
.sr_router_column_lines <- function(cols) {
  if (!nrow(cols)) return(character())
  sprintf("column.%s: name=%s; label=%s; type=%s; role=%s; distinct=%s", cols$key,
          .sr_router_esc(cols$name), .sr_router_esc(cols$label), .sr_router_esc(cols$type),
          .sr_router_esc(cols$role), .sr_router_bucket(cols$distinct))
}

#' The state of a node instance: list(text, lines, tokens, omitted). Lines, in order: the node, the PICO
#' lines, table and rows (table / column scope), the facts, then the column lines when the node needs
#' them (`facts_columns`). Tokens are estimated as ceiling(nchar / 4); above SR_ROUTER_MAX_TOKENS the
#' column lines are dropped from the end and `omitted` counts them.
sr_router_state <- function(ctx, mask = sr_opt("router_mask", FALSE)) {
  mask <- isTRUE(as.logical(mask))
  nd <- ctx$node
  has_table <- !is.na(ctx$table)
  raw <- .sr_router_mask_cols(ctx, mask)
  pico <- .sr_pico_text(ctx$project)
  if (!is.null(raw)) pico <- .sr_router_mask_text(pico, raw)
  head <- c(paste0("node: ", .sr_router_oneline(nd$id)), .sr_router_oneline(pico))
  if (has_table) {
    rows <- sr_store_manifest(ctx$project, ctx$table)$rows
    head <- c(head, paste0("table: ", .sr_router_oneline(ctx$table)),
              paste0("rows: ", .sr_router_count(as.numeric(rows))))
  }
  head <- c(head, .sr_router_fact_lines(ctx, mask, raw))
  col_lines <- if (isTRUE(nd$facts_columns) && has_table) {
    .sr_router_column_lines(sr_router_columns(ctx$project, ctx$table, mask))
  } else {
    character()
  }
  n <- length(col_lines)
  keep <- n
  if (n) {
    chars <- nchar(paste(head, collapse = "\n")) + cumsum(nchar(col_lines) + 1L)
    fits <- which(ceiling(chars / 4) <= SR_ROUTER_MAX_TOKENS)
    keep <- if (length(fits)) max(fits) else 0L
  }
  lines <- c(head, col_lines[seq_len(keep)])
  text <- paste(lines, collapse = "\n")
  list(text = text, lines = lines, tokens = as.integer(ceiling(nchar(text) / 4)),
       omitted = as.integer(n - keep))
}

#' The request for a node instance: list(request, sha, keymap), or NULL when the node has no question,
#' the question needs more than SR_SYSONE_MAX_CRITERIA criteria, or a column line was omitted. `keymap`
#' maps each criteria key to its option: itself for a fixed option, the column name for a c001 key (an error
#' unless the node has facts_columns). With `mask`, real column names are replaced by their keys in every
#' string. The token cap also stops a request. The single question is named "choice".
sr_router_request <- function(ctx, mask = sr_opt("router_mask", FALSE)) {
  mask <- isTRUE(as.logical(mask))
  nd <- ctx$node
  if (is.null(nd$question)) return(NULL)
  q <- nd$question(ctx)
  if (is.null(q)) return(NULL)
  if (length(q$criteria) > SR_SYSONE_MAX_CRITERIA) return(NULL)
  st <- sr_router_state(ctx, mask)
  if (st$omitted > 0L || st$tokens > SR_ROUTER_MAX_TOKENS) return(NULL)
  keys <- names(q$criteria)
  keymap <- stats::setNames(keys, keys)
  raw <- NULL
  if (!is.na(ctx$table)) {
    raw <- .sr_router_columns_raw(ctx$project, ctx$table)
    fixed <- if (is.function(nd$options)) nd$options(ctx) else nd$options
    hit <- keys %in% raw$key & !(keys %in% fixed)
    if (any(hit) && !isTRUE(nd$facts_columns)) {
      stop("sr_router_request: node '", nd$id, "' has column keys in its criteria (",
           paste(utils::head(keys[hit], 3L), collapse = ", "),
           ") but facts_columns is FALSE, so the model would see keys without names", call. = FALSE)
    }
    keymap[hit] <- raw$name[match(keys[hit], raw$key)]
  }
  if (mask && !is.null(raw)) {
    q$instructions <- .sr_router_mask_text(q$instructions, raw)
    q$criteria <- lapply(q$criteria, function(d) if (is.null(d)) NULL else .sr_router_mask_text(d, raw))
  }
  request <- sr_sysone_request(st$text, list(choice = sr_sysone_choice(q$instructions, q$criteria)))
  list(request = request, sha = sr_sysone_sha(request), keymap = keymap)
}

#' The exact bytes that would be sent for one node instance, and their SHA-256, for the user to approve:
#' list(key, body, sha, text), or NULL when there is nothing to ask.
sr_router_preview <- function(project, node, table = NA, column = NA,
                              mask = sr_opt("router_mask", FALSE)) {
  if (is.character(node)) node <- sr_node_get(node)
  ctx <- .sr_router_ctx(project, node, table, column)
  r <- sr_router_request(ctx, mask)
  if (is.null(r)) return(NULL)
  list(key = sr_router_key(node$id, table, column), body = sr_sysone_body(r$request), sha = r$sha,
       text = r$request$state)
}

#' The question of a column choice: list(instructions, criteria). One criterion per column of the table,
#' keyed by its c001 key with the text "the column <name>" (the request builder masks the names), and one
#' per `extra` option, keyed by itself. The node needs facts_columns = TRUE and `options(ctx)` = the column
#' names plus the extras.
.sr_router_column_question <- function(ctx, instructions, extra = "none") {
  cols <- sr_router_columns(ctx$project, ctx$table, mask = FALSE)
  criteria <- as.list(stats::setNames(paste0("the column ", cols$name), cols$key))
  criteria[extra] <- as.list(paste0("the option ", extra))
  list(instructions = instructions, criteria = criteria)
}
