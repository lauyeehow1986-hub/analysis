# router/nodes/analysis.R - the P1 nodes that shape the analysis: the outcome type, the primary route and
# the timing of each covariate. A rule reads only the outcome column on its own (the outcome-blind gate
# allows that) and the confirmed roles; a model, when asked, sees metadata only.

SR_OUTCOME_TYPES <- c("binary", "continuous", "time_to_event", "count", "other")
SR_TIMING_CHECKS <- c("leakage", "post_outcome_date")

# What the outcome_type node knows about its table: whether time and event (or an event date) roles are
# confirmed, and the distinct count and type of the chosen outcome column (NA / "" when there is none).
.sr_ot_info <- function(ctx) {
  roles <- ctx$project$state$roles[[ctx$table]]
  has <- function(r) any(roles == r)
  tte <- (has("time") && has("event")) || has("event_date")
  oc <- ctx$dep$outcome_col
  k <- NA_real_
  type <- ""
  if (!is.null(oc) && !is.na(oc) && !identical(oc, "none")) {
    sc <- .sr_roles_schema(ctx$project, ctx$table)
    if (oc %in% sc$name) {
      k <- .sr_roles_distinct(ctx$project, ctx$table, oc)
      type <- sc$type[match(oc, sc$name)]
    }
  }
  list(tte = tte, distinct = k, type = type)
}

sr_node_register(sr_node(
  "outcome_type", "Outcome type", SR_OUTCOME_TYPES, scope = "table", depends = "outcome_col",
  rule = function(ctx) {
    i <- .sr_ot_info(ctx)
    if (i$tte) {
      return(list(choice = "time_to_event", reason = "time and event roles are confirmed"))
    }
    if (!is.na(i$distinct) && i$distinct == 2) {
      return(list(choice = "binary", reason = "the outcome has two distinct values"))
    }
    if (nzchar(i$type) && grepl("^(DOUBLE|FLOAT|REAL|DECIMAL|NUMERIC)", toupper(i$type))) {
      return(list(choice = "continuous", reason = "the outcome is a non-integer number"))
    }
    NULL
  },
  facts = function(ctx) {
    i <- .sr_ot_info(ctx)
    list(time_event_roles = if (i$tte) "confirmed" else "none", outcome_type = i$type,
         outcome_distinct = .sr_router_bucket(i$distinct))
  },
  veto = function(ctx, choice) {
    i <- .sr_ot_info(ctx)
    if (identical(choice, "time_to_event") && !i$tte) return("no time and event roles are confirmed")
    if (identical(choice, "binary") && !is.na(i$distinct) && i$distinct != 2) {
      return(paste0("the outcome has ", .sr_router_bucket(i$distinct), " distinct values"))
    }
    NULL
  }))

# The P1 route that a purpose and an outcome type give, or NULL.
.sr_node_route_for <- function(purpose, type) {
  if (identical(purpose, "describe")) return("table1")
  if (!identical(purpose, "explain-causal") || is.null(type) || is.na(type)) return(NULL)
  switch(type, continuous = "lm", binary = "logistic", time_to_event = "cox", NULL)
}

sr_node_register(sr_node(
  "primary_route", "Primary analysis route",
  function(ctx) c(sort(unique(sr_routes()$id)), "none"), scope = "table",
  depends = "purpose",
  rule = function(ctx) {
    type <- sr_router_choice(ctx$project, "outcome_type", ctx$table)
    route <- .sr_node_route_for(ctx$dep$purpose, type)
    if (is.null(route) || !route %in% sr_routes()$id) return(NULL)
    list(choice = route,
         reason = paste0("route '", route, "' is the P1 route for the ", ctx$dep$purpose, " purpose",
                         if (identical(ctx$dep$purpose, "describe")) "" else paste0(" and a ", type, " outcome")))
  },
  facts = function(ctx) list(outcome_type = sr_router_choice(ctx$project, "outcome_type", ctx$table)),
  veto = function(ctx, choice) {
    if (identical(choice, "none")) return(NULL)
    rt <- sr_routes()
    purpose <- rt$purpose[match(choice, rt$id)]
    want <- ctx$dep$purpose
    if (is.na(purpose) || is.na(want) || identical(purpose, want)) return(NULL)
    paste0("route '", choice, "' serves the ", purpose, " purpose, not ", want)
  }))

# The name, label and type of one column.
.sr_ct_meta <- function(ctx) {
  sc <- .sr_roles_schema(ctx$project, ctx$table)
  list(label = .sr_router_oneline(.sr_roles_labels(ctx$project, ctx$table, ctx$column)),
       type = sc$type[match(ctx$column, sc$name)])
}

# The sorted ids of the DQ findings that say a column is measured after the outcome. No DQ run
# recorded for the table means no findings.
.sr_ct_findings <- function(ctx) {
  if (is.null(ctx$project$state$dq[[ctx$table]])) return(character())
  f <- sr_dq_findings(ctx$project, ctx$table)
  sort(unique(as.character(f$id[f$column == ctx$column & f$check %in% SR_TIMING_CHECKS])))
}

sr_node_register(sr_node(
  "covariate_timing", "Covariate timing", c("baseline", "mediator", "post_outcome", "unclear"),
  scope = "column", depends = c("purpose", "outcome_col"),
  columns = function(ctx) sr_role_cols(ctx$project, ctx$table, "covariate"),
  applies = function(ctx) {
    p <- .sr_node_purpose(ctx)
    is.na(p) || identical(p, "explain-causal")
  },
  rule = function(ctx) {
    ids <- .sr_ct_findings(ctx)
    if (!length(ids)) return(NULL)
    list(choice = "post_outcome",
         reason = paste0("DQ finding ", paste(ids, collapse = ", "), " flags the column as measured after the outcome"))
  },
  facts = function(ctx) {
    m <- .sr_ct_meta(ctx)
    role <- ctx$project$state$roles[[ctx$table]][[ctx$column]]
    list(column_name = ctx$column, column_label = m$label, column_type = m$type,
         column_role = if (is.null(role)) "unassigned" else role, finding_ids = .sr_ct_findings(ctx))
  },
  question = function(ctx) list(
    instructions = "Decide when the covariate column described in the facts is measured relative to the outcome.",
    criteria = list(
      baseline = "measured before the exposure or at the start of follow-up",
      mediator = "measured after the exposure and on the causal path to the outcome",
      post_outcome = "measured after the outcome occurred",
      unclear = "the timing cannot be told from the metadata")),
  danger = list(c("post_outcome", "baseline"), c("mediator", "baseline"))))
