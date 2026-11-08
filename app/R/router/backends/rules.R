# router/backends/rules.R - the rules backend: a node's deterministic rule, run first and always.
#
# A rule is code, not a model: it either decides (a choice and a reason) or says nothing (NULL). A rule
# that names a choice outside the node's options is a bug in the node, so it is an error that names
# the node, never a silent fallback.

#' The options of a node for one context: the node's fixed options or `options(ctx)`.
.sr_node_options <- function(node, ctx) {
  opts <- if (is.function(node$options)) node$options(ctx) else node$options
  as.character(opts)
}

#' Run the node's rule for a context: list(choice, reason), or NULL when the node has no rule or the
#' rule does not decide. A choice outside `.sr_node_options` is an error naming the node.
.sr_router_rule <- function(ctx) {
  nd <- ctx$node
  if (is.null(nd$rule)) return(NULL)
  r <- nd$rule(ctx)
  if (is.null(r)) return(NULL)
  ch <- r$choice
  if (!is.character(ch) || length(ch) != 1L || is.na(ch)) {
    stop("router node '", nd$id, "': the rule must return a single choice", call. = FALSE)
  }
  opts <- .sr_node_options(nd, ctx)
  if (!ch %in% opts) {
    stop("router node '", nd$id, "': the rule chose '", ch, "', which is not one of the options (",
         paste(opts, collapse = ", "), ")", call. = FALSE)
  }
  reason <- r$reason
  if (is.null(reason) || length(reason) != 1L || is.na(reason)) reason <- NA_character_
  list(choice = ch, reason = as.character(reason))
}
