# router/graph.R - the decision-node contract, the DAG registry, per-scope instances and the input hash.
#
# A node is one routing decision (an analysis choice with a closed set of options). Nodes depend on
# earlier nodes, so the registry order is a topological order and the graph is a DAG by construction.
# A node has a scope: one instance for the project, one per table with confirmed roles, or one per
# column of such a table. `facts(ctx)` are the data-derived inputs; with the options and the choices
# of the dependencies they make the input hash, which says when a decision has gone stale.

SR_NODE_SCOPES <- c("project", "table", "column")

.sr_node_env <- new.env(parent = emptyenv())
.sr_node_env$nodes <- list()
.sr_node_env$order <- character()

#' Build a decision node. Hooks are `function(ctx)`, except `veto(ctx, choice)`.
#' `options`: chr or function(ctx) -> chr. `applies`: lgl, NULL = always. `rule`: NULL | list(choice, reason).
#' `question`: NULL | list(instructions, criteria), NULL = rule / user only. `facts`: named list.
#' `veto`: NULL | chr reason. `columns`: chr, the instance columns of a column-scope node (required).
#' `facts_columns`: TRUE when the question needs the table's column metadata in its state (router/state.R).
#' `pico`: TRUE when the node's input hash includes the PICO text (its decision goes stale on a PICO edit).
sr_node <- function(id, label, options, scope = c("project", "table", "column"), depends = character(),
                    applies = NULL, rule = NULL, question = NULL, facts = NULL, veto = NULL,
                    danger = list(), columns = NULL, facts_columns = FALSE, pico = FALSE,
                    cite = NA_character_, version = 1L) {
  scope <- match.arg(scope)
  if (!is.character(id) || length(id) != 1L || !grepl("^[a-z][a-z0-9_]*$", id)) {
    stop("sr_node: id must match ^[a-z][a-z0-9_]*$", call. = FALSE)
  }
  if (!is.character(label) || length(label) != 1L || is.na(label)) {
    stop("sr_node '", id, "': label must be a single string", call. = FALSE)
  }
  good_opts <- is.function(options) ||
    (is.character(options) && length(options) >= 2L && !anyNA(options) && !anyDuplicated(options))
  if (!good_opts) {
    stop("sr_node '", id, "': options must be at least two distinct strings or a function(ctx)",
         call. = FALSE)
  }
  if (!is.character(depends)) stop("sr_node '", id, "': depends must be a character vector", call. = FALSE)
  hooks <- list(applies = applies, rule = rule, question = question, facts = facts, veto = veto,
                columns = columns)
  for (h in names(hooks)) {
    if (!is.null(hooks[[h]]) && !is.function(hooks[[h]])) {
      stop("sr_node '", id, "': ", h, " must be a function", call. = FALSE)
    }
  }
  if (identical(scope, "column") && is.null(columns)) {
    stop("sr_node '", id, "': a column-scope node needs `columns`", call. = FALSE)
  }
  if (!is.logical(facts_columns) || length(facts_columns) != 1L || is.na(facts_columns)) {
    stop("sr_node '", id, "': facts_columns must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.logical(pico) || length(pico) != 1L || is.na(pico)) {
    stop("sr_node '", id, "': pico must be TRUE or FALSE", call. = FALSE)
  }
  structure(list(id = id, label = label, options = options, scope = scope, depends = depends,
                 applies = applies, rule = rule, question = question, facts = facts, veto = veto,
                 danger = danger, columns = columns, facts_columns = facts_columns, pico = pico,
                 cite = cite, version = as.integer(version)),
            class = "sr_node")
}

#' Register a node. Every dependency must already be registered; re-registering an id replaces it
#' (keeping its place) only when `depends` is unchanged.
sr_node_register <- function(node) {
  if (!inherits(node, "sr_node")) stop("sr_node_register: not an sr_node", call. = FALSE)
  missing <- setdiff(node$depends, .sr_node_env$order)
  if (length(missing)) {
    stop("sr_node_register: '", node$id, "' depends on unregistered node(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  old <- .sr_node_env$nodes[[node$id]]
  if (!is.null(old) && !identical(old$depends, node$depends)) {
    stop("sr_node_register: '", node$id, "' is registered with different depends", call. = FALSE)
  }
  .sr_node_env$nodes[[node$id]] <- node
  if (is.null(old)) .sr_node_env$order <- c(.sr_node_env$order, node$id)
  invisible(node$id)
}

#' Node ids in registration order (a topological order).
sr_nodes <- function() .sr_node_env$order

sr_node_get <- function(id) {
  n <- .sr_node_env$nodes[[id]]
  if (is.null(n)) stop("unknown router node '", id, "'", call. = FALSE)
  n
}

# Tests only.
.sr_node_unregister <- function(id) {
  .sr_node_env$nodes[[id]] <- NULL
  .sr_node_env$order <- setdiff(.sr_node_env$order, id)
  invisible(NULL)
}

#' `node`, `node@table` or `node@table:column`.
sr_router_key <- function(node, table = NA, column = NA) {
  key <- node
  if (!is.na(table)) key <- paste0(key, "@", table)
  if (!is.na(table) && !is.na(column)) key <- paste0(key, ":", column)
  key
}

# Memo for one top-level .sr_router_current call: key -> current choice (NA when undecided or stale).
.sr_router_memo <- new.env(parent = emptyenv())

#' The router decisions frozen by the SAP lock: a named list key -> list(choice, resolver, override,
#' reason, input_hash), or NULL when the SAP is open or its lock carries no snapshot (a lock made before
#' P1d keeps the live view). Each record is normalised as the decision reducer stores it, so a re-read
#' of the event (NA -> NULL) gives the same values.
.sr_router_frozen <- function(project) {
  if (!isTRUE(project$state$sap$locked)) return(NULL)
  snap <- project$state$sap$lock$router
  if (is.null(snap) || !is.list(snap)) return(NULL)
  recs <- snap$decisions
  if (!length(recs)) return(stats::setNames(list(), character()))
  lapply(recs, function(r) {
    list(choice = .sr_rt_chr(r$choice), resolver = .sr_rt_chr(r$resolver), override = isTRUE(r$override),
         reason = .sr_rt_chr(r$reason), input_hash = .sr_rt_chr(r$input_hash))
  })
}

# TRUE when (node, table, column) is one of .sr_router_instances(project): a project node always is; a
# table node needs a table with confirmed roles; a column node also needs the column among its
# `columns(ctx)`. One instance at a time, so it recurses only into the node's dependencies.
.sr_router_has_instance <- function(project, node, table, column) {
  nd <- sr_node_get(node)
  if (nd$scope == "project") return(is.na(table) && is.na(column))
  if (is.na(table) || !table %in% names(project$state$roles)) return(FALSE)
  if (nd$scope == "table") return(is.na(column))
  !is.na(column) && column %in% as.character(nd$columns(.sr_router_ctx(project, nd, table, NA)))
}

#' The current choice stored for a key, or NA when undecided or stale. Once the SAP is locked the frozen
#' snapshot answers (.sr_router_frozen): a snapshot key is current with its stored choice and every other
#' key is not, whatever changed since. Otherwise a decision is current while the input hash it was made
#' under equals the hash of its instance now. The hash of an instance depends on its dependencies'
#' current choices, so the check recurses in topological order; one top-level call memoises each key. A
#' decision whose node is no longer registered, or whose instance is gone (a column that left its role,
#' a table without roles), is not current: the instance is checked before the hash, so it never errors.
.sr_router_current <- function(project, key) {
  fz <- .sr_router_frozen(project)
  if (!is.null(fz)) {
    r <- fz[[key]]
    return(if (is.null(r)) NA_character_ else r$choice)
  }
  dec <- project$state$router$decisions[[key]]
  if (is.null(dec)) return(NA_character_)
  top <- is.null(.sr_router_memo$env)
  if (top) {
    .sr_router_memo$env <- new.env(parent = emptyenv())
    on.exit(.sr_router_memo$env <- NULL, add = TRUE)
  }
  memo <- .sr_router_memo$env
  if (!is.null(memo[[key]])) return(memo[[key]])
  ch <- dec$choice
  out <- NA_character_
  if (!is.null(ch) && length(ch) == 1L && !is.na(ch) && !is.null(dec$input_hash) &&
      !is.null(dec$node) && dec$node %in% sr_nodes()) {
    tb <- if (is.null(dec$table)) NA_character_ else dec$table
    cl <- if (is.null(dec$column)) NA_character_ else dec$column
    if (.sr_router_has_instance(project, dec$node, tb, cl)) {
      now <- .sr_router_ctx(project, dec$node, tb, cl)
      if (identical(sr_router_input_hash(now), as.character(dec$input_hash))) out <- as.character(ch)
    }
  }
  memo[[key]] <- out
  out
}

# Instance key of dependency `dn` as seen from a node at (table, column): the widest scope that fits,
# or NA when the dependency is narrower than the node (or the table / column is not known).
.sr_dep_key <- function(dn, node, table, column) {
  rank <- function(s) match(s, SR_NODE_SCOPES)
  if (rank(dn$scope) > rank(node$scope)) return(NA_character_)
  switch(dn$scope,
         project = sr_router_key(dn$id),
         table = if (is.na(table)) NA_character_ else sr_router_key(dn$id, table),
         column = if (is.na(table) || is.na(column)) NA_character_ else sr_router_key(dn$id, table, column))
}

#' Routing context for one node instance: project, node, table, column, pico and `dep`, the named
#' list of the current choices of the node's dependencies (NA when undecided or stale).
.sr_router_ctx <- function(project, node, table = NA, column = NA) {
  if (is.character(node)) node <- sr_node_get(node)
  dep <- lapply(stats::setNames(node$depends, node$depends), function(d) {
    key <- .sr_dep_key(sr_node_get(d), node, table, column)
    if (is.na(key)) NA_character_ else .sr_router_current(project, key)
  })
  list(project = project, node = node, table = table, column = column, pico = sr_pico(project),
       dep = dep)
}

#' Every instance of every node, in `sr_nodes()` order: project nodes once, table nodes once per
#' table with confirmed roles (sorted), column nodes once per `columns(ctx)` of each such table.
.sr_router_instances <- function(project) {
  tables <- sort(names(project$state$roles))
  rows <- list()
  add <- function(node, table, column) {
    rows[[length(rows) + 1L]] <<- data.frame(key = sr_router_key(node, table, column), node = node,
                                              table = table, column = column, stringsAsFactors = FALSE)
  }
  for (id in sr_nodes()) {
    nd <- sr_node_get(id)
    if (nd$scope == "project") {
      add(id, NA_character_, NA_character_)
      next
    }
    for (tb in tables) {
      if (nd$scope == "table") {
        add(id, tb, NA_character_)
      } else {
        for (cl in as.character(nd$columns(.sr_router_ctx(project, nd, tb, NA)))) add(id, tb, cl)
      }
    }
  }
  if (!length(rows)) {
    return(data.frame(key = character(), node = character(), table = character(), column = character(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

#' SHA-256 of the node id and version, its options, its facts and its dependencies' choices, plus the
#' PICO text (.sr_pico_text) for a node with `pico = TRUE`. A node without the flag hashes as it did
#' before the flag existed.
sr_router_input_hash <- function(ctx) {
  nd <- ctx$node
  opts <- if (is.function(nd$options)) nd$options(ctx) else nd$options
  facts <- if (is.null(nd$facts)) list() else nd$facts(ctx)
  parts <- list(node = nd$id, version = nd$version, options = opts, facts = facts, dep = ctx$dep)
  if (isTRUE(nd$pico)) parts$pico <- .sr_pico_text(ctx$project)
  .sr_sha256(sr_canonical_json(parts))
}
