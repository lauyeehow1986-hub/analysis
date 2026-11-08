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

# One `fact.<name>: <value>` line per fact. With `mask`, the facts column_name and column_label of a
# column-scope node show the column key and "" (the input hash still hashes the real values).
.sr_router_fact_lines <- function(ctx, mask) {
  nd <- ctx$node
  facts <- if (is.null(nd$facts)) list() else nd$facts(ctx)
  if (!length(facts)) return(character())
  if (mask) {
    key <- NA_character_
    if (!is.na(ctx$table) && !is.na(ctx$column)) {
      raw <- .sr_router_columns_raw(ctx$project, ctx$table)
      key <- raw$key[match(ctx$column, raw$name)]
    }
    if ("column_name" %in% names(facts)) facts$column_name <- if (is.na(key)) "" else key
    if ("column_label" %in% names(facts)) facts$column_label <- ""
  }
  vals <- vapply(facts, function(v) paste(as.character(unlist(v, use.names = FALSE)), collapse = ", "), "")
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
  head <- c(paste0("node: ", .sr_router_oneline(nd$id)), .sr_router_oneline(.sr_pico_text(ctx$project)))
  if (has_table) {
    rows <- sr_store_manifest(ctx$project, ctx$table)$rows
    head <- c(head, paste0("table: ", .sr_router_oneline(ctx$table)),
              paste0("rows: ", .sr_router_count(as.numeric(rows))))
  }
  head <- c(head, .sr_router_fact_lines(ctx, mask))
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
#' maps each criteria key to its option: itself for a fixed option, the column name for a c001 key of a
#' node that needs the columns. The single question is named "choice".
sr_router_request <- function(ctx, mask = sr_opt("router_mask", FALSE)) {
  nd <- ctx$node
  if (is.null(nd$question)) return(NULL)
  q <- nd$question(ctx)
  if (is.null(q)) return(NULL)
  if (length(q$criteria) > SR_SYSONE_MAX_CRITERIA) return(NULL)
  st <- sr_router_state(ctx, mask)
  if (st$omitted > 0L) return(NULL)
  keys <- names(q$criteria)
  keymap <- stats::setNames(keys, keys)
  if (isTRUE(nd$facts_columns) && !is.na(ctx$table)) {
    raw <- .sr_router_columns_raw(ctx$project, ctx$table)
    hit <- keys %in% raw$key
    keymap[hit] <- raw$name[match(keys[hit], raw$key)]
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
