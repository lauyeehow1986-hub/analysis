# router/nodes/intake.R - the project-scope P1 nodes: the study design, the analysis purpose and the
# reporting guideline. Rules decide where the research question or an earlier choice settles them; the
# questions carry the PICO lines and, for the design, the role counts of the tables (no cell values).
# The design and the purpose hash the PICO text too (pico = TRUE), so a PICO edit makes both stale.

SR_DESIGNS <- c("cohort_prospective", "cohort_retrospective", "case_control", "cross_sectional", "rct",
                "other")

# The decided purpose of the context's dependency: NA while undecided or stale.
.sr_node_purpose <- function(ctx) {
  p <- ctx$dep$purpose
  if (is.null(p)) NA_character_ else p
}

# "outcome=1; exposure=1; covariate=5": the confirmed roles of a table, in SR_ROLES order.
.sr_role_counts_text <- function(roles) {
  n <- vapply(SR_ROLES, function(r) sum(roles == r), numeric(1))
  paste0(SR_ROLES[n > 0], "=", n[n > 0], collapse = "; ")
}

sr_node_register(sr_node(
  "design", "Study design", SR_DESIGNS, scope = "project", pico = TRUE, cite = "vonelm2007",
  facts = function(ctx) {
    roles <- ctx$project$state$roles
    tb <- sort(names(roles)[vapply(roles, length, 1L) > 0L])
    list(tables = vapply(tb, function(t) paste0(t, ": ", .sr_role_counts_text(roles[[t]])), "",
                         USE.NAMES = FALSE))
  },
  question = function(ctx) list(
    instructions = "Decide the study design that the research question in the PICO lines describes.",
    criteria = list(
      cohort_prospective = "a cohort followed forward in time from enrolment",
      cohort_retrospective = "a cohort assembled from existing records and followed backward in time",
      case_control = "participants sampled by outcome status, cases against controls",
      cross_sectional = "one snapshot in time with exposure and outcome measured together",
      rct = "a randomised controlled trial with assigned treatment",
      other = "none of the above")),
  danger = list(c("case_control", "cohort_retrospective"), c("case_control", "cohort_prospective"),
                c("rct", "cohort_prospective"))))

sr_node_register(sr_node(
  "purpose", "Analysis purpose", SR_PURPOSES, scope = "project", pico = TRUE,
  rule = function(ctx) {
    aim <- ctx$pico$aim
    if (is.null(aim) || length(aim) != 1L || is.na(aim) || !aim %in% SR_PURPOSES) return(NULL)
    list(choice = aim, reason = "the stated aim")
  },
  facts = function(ctx) {
    aim <- ctx$pico$aim
    list(aim = if (is.null(aim)) NA_character_ else aim)
  },
  question = function(ctx) list(
    instructions = "Decide the purpose of the analysis that the research question in the PICO lines asks for.",
    criteria = list(
      describe = "summarise the data without estimating an effect",
      predict = "build a model that predicts the outcome for new patients",
      `explain-causal` = "estimate the causal effect of the exposure on the outcome")),
  danger = list(c("explain-causal", "predict"), c("predict", "explain-causal"))))

sr_node_register(sr_node(
  "guideline", "Reporting guideline", c("strobe", "consort", "none"), scope = "project",
  depends = "design", cite = "vonelm2007",
  rule = function(ctx) {
    d <- ctx$dep$design
    if (is.null(d) || is.na(d)) return(NULL)
    g <- if (identical(d, "rct")) "consort" else if (identical(d, "other")) "none" else "strobe"
    list(choice = g, reason = paste0("the guideline for a ", gsub("_", " ", d), " design"))
  }))
