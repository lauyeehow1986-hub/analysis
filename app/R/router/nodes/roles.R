# router/nodes/roles.R - the table-scope column choices of the P1 nodes: the outcome and the exposure
# column. A confirmed role decides; else the PICO text must match exactly one column name or label.

#' Lower case, every run of non-alphanumerics one space, trimmed. NA stays NA.
.sr_router_norm <- function(x) {
  x <- tolower(as.character(x))
  trimws(gsub("[^a-z0-9]+", " ", x))
}

# The column names of the context's table, then "none".
.sr_node_col_options <- function(ctx) {
  unique(c(.sr_roles_schema(ctx$project, ctx$table)$name, "none"))
}

# The rule shared by both column nodes: the single column with `role`, else a unique exact match of
# `text` (a PICO field) with a normalised column name or label; NULL when neither settles it.
.sr_node_col_rule <- function(ctx, role, text) {
  hit <- sr_role_cols(ctx$project, ctx$table, role)
  if (length(hit) == 1L) return(list(choice = hit, reason = "confirmed role"))
  if (length(hit) > 1L || is.null(text) || length(text) != 1L || is.na(text)) return(NULL)
  target <- .sr_router_norm(text)
  if (!nzchar(target)) return(NULL)
  sc <- .sr_roles_schema(ctx$project, ctx$table)
  lab <- .sr_roles_labels(ctx$project, ctx$table, sc$name)
  m <- sc$name[.sr_router_norm(sc$name) == target | .sr_router_norm(lab) == target]
  if (length(m) != 1L) return(NULL)
  list(choice = m, reason = paste0("the PICO ", role, " matches column '", m, "'"))
}

# The facts that make a column decision stale: the confirmed role columns and the PICO text it reads.
.sr_node_col_facts <- function(ctx, role, text) {
  out <- list(roles = sr_role_cols(ctx$project, ctx$table, role),
              pico = if (is.null(text)) NA_character_ else text)
  stats::setNames(out, c(paste0(role, "_role"), paste0("pico_", role)))
}

# The veto of a column node: `choice` equals the other node's current choice (and is not "none").
.sr_node_col_veto <- function(ctx, choice, other, what) {
  if (identical(choice, "none")) return(NULL)
  o <- sr_router_choice(ctx$project, other, ctx$table)
  if (!is.na(o) && identical(o, choice)) {
    return(paste0("'", choice, "' is already the ", what, " column"))
  }
  NULL
}

sr_node_register(sr_node(
  "outcome_col", "Outcome column", .sr_node_col_options, scope = "table", depends = "purpose",
  facts_columns = TRUE,
  rule = function(ctx) .sr_node_col_rule(ctx, "outcome", ctx$pico$outcome),
  facts = function(ctx) .sr_node_col_facts(ctx, "outcome", ctx$pico$outcome),
  question = function(ctx) .sr_router_column_question(
    ctx, "Choose the column of the table that holds the outcome named in the PICO lines."),
  veto = function(ctx, choice) .sr_node_col_veto(ctx, choice, "exposure_col", "exposure")))

sr_node_register(sr_node(
  "exposure_col", "Exposure column", .sr_node_col_options, scope = "table", depends = "purpose",
  facts_columns = TRUE,
  applies = function(ctx) !identical(.sr_node_purpose(ctx), "describe"),
  rule = function(ctx) .sr_node_col_rule(ctx, "exposure", ctx$pico$exposure),
  facts = function(ctx) .sr_node_col_facts(ctx, "exposure", ctx$pico$exposure),
  question = function(ctx) .sr_router_column_question(
    ctx, "Choose the column of the table that holds the exposure named in the PICO lines."),
  veto = function(ctx, choice) .sr_node_col_veto(ctx, choice, "outcome_col", "outcome")))
