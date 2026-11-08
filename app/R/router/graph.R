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
sr_node <- function(id, label, options, scope = c("project", "table", "column"), depends = character(),
                    applies = NULL, rule = NULL, question = NULL, facts = NULL, veto = NULL,
                    danger = list(), columns = NULL, cite = NA_character_, version = 1L) {
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
  structure(list(id = id, label = label, options = options, scope = scope, depends = depends,
                 applies = applies, rule = rule, question = question, facts = facts, veto = veto,
                 danger = danger, columns = columns, cite = cite, version = as.integer(version)),
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

#' The current choice stored for a key, or NA. A decision is "current" while it exists; a later task
#' compares `ctx_hash` with the decision's input hash.
.sr_router_current <- function(project, key, ctx_hash = NULL) {
  ch <- project$state$router$decisions[[key]]$choice
  if (is.null(ch) || length(ch) != 1L || is.na(ch)) NA_character_ else as.character(ch)
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

#' SHA-256 of the node id and version, its options, its facts and its dependencies' choices.
sr_router_input_hash <- function(ctx) {
  nd <- ctx$node
  opts <- if (is.function(nd$options)) nd$options(ctx) else nd$options
  facts <- if (is.null(nd$facts)) list() else nd$facts(ctx)
  .sr_sha256(sr_canonical_json(list(node = nd$id, version = nd$version, options = opts, facts = facts,
                                    dep = ctx$dep)))
}
