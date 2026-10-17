# routes/cox.R - the Cox proportional hazards route cox@1 (explain-causal) with its describe twin
# cox_describe@1, which share their compile and present functions, and the Kaplan-Meier data that both
# survival routes (this one and routes/rmst.R) put into their figures. The model is
#   Surv(time, status == status_event) ~ exposure + terms (+ interactions) [+ strata(..)] [+ cluster(..)]
# fitted by survival::coxph on the complete cases (rows with a missing time, status, term, stratum or
# cluster value are dropped and counted). Ties are Efron (default) or Breslow.
#
# Standard errors: model-based, or the robust (sandwich) ones of cluster(), used when and only when
# layers$clustered = TRUE; that needs spec$cluster, and a cluster column without layers$clustered = TRUE
# is refused. There is no other robust option (ruling R23a: no se field). Intervals are normal (z) on
# the log hazard ratio.
#
# The compiled code is data-free (see routes/terms.R): every model column has a derived name (t1, t2f,
# ...) and present() maps the coefficients back to the user variables with sr_coef_source(). The strata
# and cluster columns are copied to the derived columns .s1, .s2, ... and .cl, so no user column name
# appears as a symbol in the formula.
#
# Effects. One HR row per exposure coefficient (a factor exposure gives one per non-reference level),
# role "primary", scale "ratio"; every other coefficient is role "covariate" (the Table 2 flag is
# added by sr_result_check). The rows carry the analysed n and the events. A hazard ratio is not
# collapsible, so every exposure row is marked conditional when the model has any term beyond the
# exposure (or a stratum), or when the exposure takes part in an interaction.
#
# Kaplan-Meier data (figures$km, figures$km_risk): survfit(Surv(time, event) ~ exposure) over the rows
# of the fit, with the exposure level as the stratum: the levels of a factor exposure, the values of a
# linear exposure with at most 8 distinct values, and one overall curve ("All") otherwise. km holds
# strata, time, surv, lower, upper, n.risk, n.event, n.censor; km_risk is the number at risk at the
# pretty() times of the time range that do not exceed the longest follow-up (strata, time, n.risk).
#
# Multiple imputation (layers$missing = "mi" with spec$mi; impute/impute.R): rows with a missing time or
# status are dropped (never imputed) and counted; the event indicator and the Nelson-Aalen cumulative
# hazard enter the imputation model (the time itself does not), the strata are imputed as factors, the
# Cox model is fitted in every completed dataset and the log hazard ratios are pooled by Rubin's rules
# with normal intervals. The Kaplan-Meier data use the rows with an observed exposure. MI with
# clustered data is refused.
#
# The bootstrap (layers$resampling = "bootstrap", spec$boot) is resample/boot.R: the primary HR takes
# a bootstrap interval of the log HR.
#
# Not implemented (refused at compile, never silently ignored): weights, matching and the bayesian
# framework.

.sr_cox_ties_of <- function(spec) if (is.null(spec[["ties"]])) "efron" else spec[["ties"]]

# Whether the fit uses cluster(): layers$clustered = TRUE (and then spec$cluster is required).
.sr_cox_clustered <- function(spec) isTRUE(spec[["layers"]][["clustered"]])

# The checks the survival routes share: time, status and exposure are three different columns and
# status_event is one value. Stops with the reason.
.sr_surv_check <- function(spec) {
  one <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!one(spec[["time"]])) stop("time must be one column name", call. = FALSE)
  if (!one(spec[["status"]])) stop("status must be one column name", call. = FALSE)
  if (!one(spec[["exposure"]])) stop("exposure must be one column name", call. = FALSE)
  cols <- c(spec[["time"]], spec[["status"]], spec[["exposure"]])
  if (anyDuplicated(cols)) {
    stop("time, status and exposure must be three different columns", call. = FALSE)
  }
  ev <- spec[["status_event"]]
  if (!is.null(ev) && !(is.atomic(ev) && length(ev) == 1L && !is.na(ev))) {
    stop("status_event must be one non-missing value (the status value that counts as the event)", call. = FALSE)
  }
  invisible(TRUE)
}

# Cross-field validation of a cox spec (the schema has been checked already); stops with the reason.
.sr_cox_check <- function(spec) {
  .sr_surv_check(spec)
  .sr_exclude_check(spec)
  base <- c(spec[["time"]], spec[["status"]], spec[["exposure"]])
  cl <- spec[["cluster"]]
  if (.sr_cox_clustered(spec)) {
    if (!(is.character(cl) && length(cl) == 1L && !is.na(cl) && nzchar(cl))) {
      stop("layers$clustered = TRUE needs a cluster column (spec$cluster)", call. = FALSE)
    }
    if (cl %in% base) stop("the cluster column must differ from time, status and the exposure", call. = FALSE)
  } else if (length(.sr_spec_chr(cl))) {
    stop("a cluster column needs layers$clustered = TRUE (the robust SE of cluster())", call. = FALSE)
  }
  st <- .sr_spec_chr(spec[["strata"]])
  if (length(st) && any(st %in% base)) {
    stop("a strata column must differ from time, status and the exposure", call. = FALSE)
  }
  invisible(TRUE)
}

.sr_cox_estimand <- function(spec) {
  .sr_cox_check(spec)
  st <- .sr_spec_chr(spec[["strata"]])
  mi <- identical(spec[["layers"]][["missing"]], "mi")
  summary <- if (length(spec[["terms"]]) > 0L) {
    "adjusted hazard ratio"
  } else if (length(st)) {
    paste0("hazard ratio stratified by ", paste(st, collapse = ", "), " (no other model terms)")
  } else {
    "hazard ratio"
  }
  sr_estimand(population = if (mi) "all analysed (MI)" else "complete cases",
              variables = c(spec[["exposure"]], spec[["time"]], spec[["status"]]),
              intercurrent = "other reasons for censoring are treated as non-informative censoring",
              summary = summary, level = "conditional")
}

# ---- Kaplan-Meier -------------------------------------------------------------------------------

# Kaplan-Meier data of `time` / `event` (0/1) by `group` (NULL gives one overall curve, "All").
# Returns list(km, risk): km = data.frame(strata, time, surv, lower, upper, n.risk, n.event, n.censor)
# as survfit gives them; risk = data.frame(strata, time, n.risk) at the pretty() times of the time
# range that do not exceed the longest time. Inlined into the emitted code as .h_km.
.sr_h_km <- function(time, event, group = NULL) {
  g <- if (is.null(group)) factor(rep("All", length(time))) else droplevels(as.factor(group))
  d <- data.frame(.t = time, .e = event, .g = g)
  fit <- survival::survfit(survival::Surv(.t, .e) ~ .g, data = d)
  strip <- function(x) sub(".g=", "", as.character(x), fixed = TRUE)
  lab <- if (is.null(fit$strata)) {
    rep(levels(g)[1L], length(fit$time))
  } else {
    rep(strip(names(fit$strata)), as.integer(fit$strata))
  }
  km <- data.frame(strata = lab, time = fit$time, surv = fit$surv, lower = fit$lower, upper = fit$upper,
                   n.risk = fit$n.risk, n.event = fit$n.event, n.censor = fit$n.censor,
                   stringsAsFactors = FALSE)
  tt <- pretty(range(time), n = 6)
  tt <- tt[tt <= max(time)]
  sm <- summary(fit, times = tt, extend = TRUE)
  rl <- if (is.null(sm$strata)) rep(levels(g)[1L], length(sm$time)) else strip(sm$strata)
  risk <- data.frame(strata = rl, time = as.numeric(sm$time), n.risk = as.numeric(sm$n.risk),
                     stringsAsFactors = FALSE)
  list(km = km, risk = risk)
}

# ---- compile ------------------------------------------------------------------------------------

.sr_cox_compile <- function(spec) {
  .sr_cox_check(spec)
  .sr_glm_layers_check(spec, "cox")   # the layer stack is the one of the other model routes
  if (.sr_boot_on(spec)) return(.sr_boot_compile(spec, .sr_cox_compile, "cox", "cox"))
  if (.sr_mi_on(spec)) return(.sr_cox_mi_compile(spec))
  tc <- sr_terms_code(spec)
  plan <- .sr_term_plan(spec)
  ties <- .sr_cox_ties_of(spec)
  cl <- if (.sr_cox_clustered(spec)) spec[["cluster"]] else NULL
  st <- .sr_spec_chr(spec[["strata"]])
  st_names <- if (length(st)) paste0(".s", seq_along(st)) else character()
  rhs_vars <- all.vars(stats::as.formula(paste("~", tc$rhs)))
  used <- unique(c(".time", ".event", rhs_vars, st_names, if (!is.null(cl)) ".cl"))
  rhs <- paste(c(
    tc$rhs,
    if (length(st)) sprintf("strata(%s)", paste(st_names, collapse = ", ")),
    if (!is.null(cl)) "cluster(.cl)"), collapse = " + ")
  km_group <- if (identical(plan[[1L]]$type, "factor")) {
    "des[[\"t1f\"]]"
  } else {
    "if (length(unique(des[[\"t1\"]])) <= 8L) des[[\"t1\"]] else NULL"
  }
  copy <- c(
    vapply(seq_along(st), function(i) sprintf("des[[%s]] <- des[[%s]]", .sr_lit(st_names[i]), .sr_lit(st[i])),
           character(1)),
    if (!is.null(cl)) sprintf("des[[\".cl\"]] <- des[[%s]]", .sr_lit(cl)))
  code <- c(
    tc$setup,
    .sr_inline(.sr_h_coef_table, ".h_coef_table"),
    .sr_inline(.sr_h_km, ".h_km"),
    tc$design,
    "des <- .h_design(sr_data)",
    sr_outcome_code(spec, "surv"),
    copy,
    sprintf(".keep <- stats::complete.cases(des[%s])", .sr_lit(used)),
    "if (!any(.keep)) stop(\"no complete rows: every row has a missing value in the model\")",
    "des <- des[.keep, , drop = FALSE]",
    "if (sum(des[[\".event\"]]) == 0L) {",
    "  stop(\"the outcome has no events among the \", nrow(des), \" rows analysed\")",
    "}",
    ".ncl <- NA_real_",
    if (!is.null(cl)) c(
      ".ncl <- length(unique(des[[\".cl\"]]))",
      "if (.ncl < 2L) stop(\"layers$clustered needs at least two clusters (found \", .ncl, \")\")"
    ),
    ".wrn <- character()",
    sprintf("fit <- withCallingHandlers(survival::coxph(survival::Surv(.time, .event) ~ %s, data = des, ties = %s),",
            rhs, .sr_lit(ties)),
    "  warning = function(w) {",
    "    .wrn <<- c(.wrn, conditionMessage(w))",
    "    invokeRestart(\"muffleWarning\")",
    "  })",
    ".cf <- stats::coef(fit)",
    ".ok <- !is.na(.cf)",
    ".nm <- names(.cf)[.ok]",
    ".V <- stats::vcov(fit)[.nm, .nm, drop = FALSE]",
    "tab <- .h_coef_table(.cf[.ok], .V, df = Inf)",
    sprintf(".kmg <- %s", km_group),
    ".kmo <- .h_km(des[[\".time\"]], des[[\".event\"]], .kmg)",
    paste0("list(coef = tab, aliased = names(.cf)[!.ok], n = nrow(des), n_total = nrow(sr_data), ",
           "events = sum(des[[\".event\"]]), level = 0.95, se_type = ",
           .sr_lit(if (is.null(cl)) "model" else "cluster"), ", n_clusters = .ncl, ties = ", .sr_lit(ties),
           ", strata = ", .sr_lit(st), ", levels = .sr_fixed$levels, warnings = .wrn, ",
           "km_by = if (is.null(.kmg)) \"overall\" else \"exposure\", km = .kmo$km, km_risk = .kmo$risk)")
  )
  paste(code, collapse = "\n")
}

# The compile under multiple imputation: one coxph per completed dataset inside sr_mi_code() (kind
# "surv": the time and the event are never imputed; the strata columns .s1, ... come from the imputed
# frame).
.sr_cox_mi_compile <- function(spec) {
  .sr_mi_check(spec, "cox")
  tc <- sr_terms_code(spec)
  plan <- .sr_term_plan(spec)
  ties <- .sr_cox_ties_of(spec)
  st <- .sr_spec_chr(spec[["strata"]])
  st_names <- if (length(st)) paste0(".s", seq_along(st)) else character()
  rhs <- paste(c(tc$rhs, if (length(st)) sprintf("strata(%s)", paste(st_names, collapse = ", "))),
               collapse = " + ")
  fac <- identical(plan[[1L]]$type, "factor")
  pre <- c(
    "if (sum(des[[\".event\"]]) == 0L) {",
    "  stop(\"the outcome has no events among the \", nrow(des), \" rows with a time and a status\")",
    "}",
    sprintf(".kmd <- des[!is.na(des[[%s]]), , drop = FALSE]", .sr_lit(if (fac) "t1f" else "t1")),
    if (fac) {
      ".kmg <- .kmd[[\"t1f\"]]"
    } else {
      ".kmg <- if (length(unique(.kmd[[\"t1\"]])) <= 8L) .kmd[[\"t1\"]] else NULL"
    },
    ".kmo <- .h_km(.kmd[[\".time\"]], .kmd[[\".event\"]], .kmg)")
  fit <- c(
    ".wrn <- character()",
    sprintf("fit <- withCallingHandlers(survival::coxph(survival::Surv(.time, .event) ~ %s, data = des, ties = %s),",
            rhs, .sr_lit(ties)),
    "  warning = function(w) {",
    "    .wrn <<- c(.wrn, conditionMessage(w))",
    "    invokeRestart(\"muffleWarning\")",
    "  })",
    ".cf <- stats::coef(fit)",
    ".ok <- !is.na(.cf)",
    ".nm <- names(.cf)[.ok]",
    ".V <- stats::vcov(fit)[.nm, .nm, drop = FALSE]",
    paste0("list(est = .cf[.ok], vcov = .V, dfcom = Inf, aliased = names(.cf)[!.ok], scal = NULL, ",
           "num = NULL, flags = NULL, notes = .wrn)"))
  code <- c(
    sr_mi_code(spec, fit, "surv", helpers = .sr_inline(.sr_h_km, ".h_km"), pre = pre),
    paste0("list(coef = .mi$coef, aliased = .mi$aliased, n = nrow(des), n_total = nrow(sr_data), ",
           "events = sum(des[[\".event\"]]), level = 0.95, se_type = \"model\", n_clusters = NA_real_, ",
           "ties = ", .sr_lit(ties), ", strata = ", .sr_lit(st), ", levels = .sr_fixed$levels, warnings = .mi$notes, ",
           "km_by = if (is.null(.kmg)) \"overall\" else \"exposure\", km = .kmo$km, km_risk = .kmo$risk, ",
           "mi = .mi_info)"))
  paste(code, collapse = "\n")
}

# ---- present ------------------------------------------------------------------------------------

.sr_cox_se_text <- function(value) {
  if (identical(value$se_type, "cluster")) {
    sprintf("cluster-robust (sandwich) standard errors, %d clusters, normal (z) intervals",
            as.integer(round(value$n_clusters)))
  } else {
    "model-based standard errors, normal (z) intervals"
  }
}

.sr_cox_present <- function(value, spec) {
  tc <- sr_terms_code(spec)
  tab <- value$coef
  exposure <- spec[["exposure"]]
  ia <- spec[["interactions"]]
  has_ia <- any(vapply(ia, function(x) exposure %in% x, logical(1)))
  strata <- value$strata
  ref_of <- function(coef) {
    id <- sub("f.*$", "", coef)
    lv <- value$levels[[id]]
    if (is.null(lv)) NA_character_ else as.character(lv[1L])
  }
  asrc <- NULL
  if (length(value$aliased)) {
    asrc <- sr_coef_source(value$aliased, tc$coef_map)
    if (any(asrc$source_var == exposure & asrc$kind %in% c("linear", "factor"))) {
      stop("the exposure \"", exposure, "\" is collinear with other terms of the model; its effect is not estimable",
           call. = FALSE)
    }
  }
  src <- if (nrow(tab)) sr_coef_source(tab$term, tc$coef_map) else NULL
  ci_base <- if (identical(value$se_type, "cluster")) "z_cluster_robust" else "z_model"
  mi <- !is.null(value$mi)
  if (nrow(tab)) {
    lab <- .sr_lm_labels(tab$term, src, ref_of)
    prim <- src$source_var == exposure & src$kind %in% c("linear", "factor")
    adjusted <- sum(!prim) > 0L || length(strata) > 0L
    effects <- data.frame(
      term = lab, measure = "HR", est = exp(tab$est), lo = exp(tab$lo), hi = exp(tab$hi), p = tab$p,
      scale = "ratio", level = ifelse(is.na(src$level), NA_character_, src$level),
      mcse = if (mi) tab$mcse else NA_real_, role = ifelse(prim, "primary", "covariate"),
      conditional = adjusted | has_ia, ci_method = if (mi) "rubin" else ci_base,
      n = value$n, events = value$events, stringsAsFactors = FALSE)
  } else {
    effects <- sr_effects_empty()
    lab <- character()
  }
  rownames(effects) <- NULL
  prim_rows <- effects[effects$role == "primary", , drop = FALSE]
  if (!nrow(prim_rows) || !all(is.finite(c(prim_rows$est, prim_rows$lo, prim_rows$hi)))) {
    stop("The estimate or interval for the exposure \"", exposure, "\" is not finite: the partial likelihood has no ",
         "finite maximum for it (for example because it perfectly separates the events), so the hazard ratio cannot be reported.",
         call. = FALSE)
  }

  coefs <- data.frame(
    Term = lab,
    `HR (95% CI)` = sr_fmt_ci(exp(tab$est), exp(tab$lo), exp(tab$hi)),
    `SE (log scale)` = .sr_lm_num(tab$se),
    P = ifelse(is.na(tab$p), "NA", sr_fmt_p(pmin(pmax(tab$p, 0), 1))),
    check.names = FALSE, stringsAsFactors = FALSE)
  fit_tab <- data.frame(
    Statistic = c("Rows analysed", "Rows excluded (missing)", "Events", "Censored"),
    Value = c(sprintf("%d", as.integer(round(value$n))),
              sprintf("%d", as.integer(round(value$n_total - value$n))),
              sprintf("%d", as.integer(round(value$events))),
              sprintf("%d", as.integer(round(value$n - value$events)))),
    stringsAsFactors = FALSE)
  tables <- list(coefficients = coefs, model_fit = fit_tab)

  n_other <- if (nrow(tab)) sum(!prim) else 0L
  st_text <- paste(strata, collapse = ", ")
  # strata with no other term: the stratification is the whole adjustment, said once
  adj_text <- if (n_other > 0L) {
    "adjusted for the other terms of the model"
  } else if (length(strata)) {
    paste0("stratified by ", st_text, " (no other model terms)")
  } else {
    "unadjusted (no model terms besides the exposure)"
  }
  st_suffix <- if (length(strata) && n_other > 0L) paste0("; baseline hazard stratified by ", st_text) else ""
  frag <- list(cox_note = sprintf(
    paste0("Cox proportional hazards regression of time to %s on %s (%s ties), %s; ",
           "%d row(s) analysed (%d events), %d excluded for a missing value%s; %s."),
    spec[["status"]], exposure, value$ties, adj_text, as.integer(round(value$n)), as.integer(round(value$events)),
    as.integer(round(value$n_total - value$n)),
    st_suffix,
    .sr_cox_se_text(value)))
  frag$exclude_rows <- .sr_exclude_fragment(spec)
  if (mi) {
    tables$model_fit <- data.frame(
      Statistic = c("Rows analysed", "Rows excluded (missing time or status)", "Events", "Censored",
                    "Imputed datasets (m)"),
      Value = c(sprintf("%d", as.integer(round(value$n))),
                sprintf("%d", as.integer(round(value$n_total - value$n))),
                sprintf("%d", as.integer(round(value$events))),
                sprintf("%d", as.integer(round(value$n - value$events))),
                sprintf("%d", as.integer(round(value$mi$m)))),
      stringsAsFactors = FALSE)
    frag$cox_note <- sprintf(
      paste0("Cox proportional hazards regression of time to %s on %s (%s ties), %s, in each of %d imputed datasets; ",
             "%d row(s) analysed (%d events), %d excluded for a missing time or status%s; %s; pooled by Rubin's rules on the log scale."),
      spec[["status"]], exposure, value$ties, adj_text, as.integer(round(value$mi$m)), as.integer(round(value$n)),
      as.integer(round(value$events)), as.integer(round(value$n_total - value$n)),
      st_suffix,
      .sr_cox_se_text(value))
  }
  if (length(value$aliased)) {
    alab <- .sr_lm_labels(value$aliased, asrc, ref_of)
    frag$cox_aliased <- sprintf("Dropped as perfectly collinear with the other terms: %s.", paste(alab, collapse = ", "))
  }
  if (has_ia) {
    frag$cox_interaction <- paste("The exposure takes part in an interaction: its main-effect row is the hazard ratio",
                                  "where the interacting variable is 0 (or at its reference level), not an average effect.")
  }
  if (length(value$warnings)) {
    frag$cox_warning <- paste0("The fitting routine warned: ", paste(unique(value$warnings), collapse = "; "),
                               ". The estimates may be unreliable.")
  }
  if (identical(value$km_by, "overall")) {
    frag$cox_km_overall <- sprintf(
      "The exposure \"%s\" is continuous (or has more than 8 distinct values), so the Kaplan-Meier figure shows one overall curve.",
      exposure)
  }
  if (mi) {
    mp <- .sr_mi_present(value, "cox")
    tables$imputation <- mp$table
    frag <- c(frag, mp$fragments)
  }
  list(tables = tables,
       figures = list(km = value$km, km_risk = value$km_risk),
       fragments = frag, effects = effects)
}

# ---- the routes ---------------------------------------------------------------------------------

.sr_cox_schema <- function() {
  list(
    time = list(type = "chr", required = TRUE),
    status = list(type = "chr", required = TRUE),
    status_event = list(type = "num|chr", required = FALSE),
    exposure = list(type = "chr", required = TRUE),
    terms = list(type = "list", required = FALSE),
    interactions = list(type = "list", required = FALSE),
    strata = list(type = "chr", required = FALSE),
    cluster = list(type = "chr", required = FALSE),
    ties = list(type = "chr", required = FALSE, values = c("efron", "breslow")),
    exclude_rows = list(type = "num", required = FALSE),
    layers = list(type = "list", required = TRUE),
    seed = list(type = "num", required = TRUE)
  )
}

# The present step of both cox routes: the bootstrap bounds when the value has them (resample/boot.R).
.sr_cox_present_boot <- function(value, spec) .sr_boot_present(value, spec, .sr_cox_present, "cox")

.sr_cox_route <- function() {
  sr_route(id = "cox", version = 1L, family = "survival", purpose = "explain-causal",
           estimand = .sr_cox_estimand, spec_schema = .sr_cox_schema(),
           compile = .sr_cox_compile, present = .sr_cox_present_boot)
}

.sr_cox_describe_route <- function() {
  sr_route(id = "cox_describe", version = 1L, family = "survival", purpose = "describe",
           estimand = .sr_cox_estimand, spec_schema = .sr_cox_schema(),
           compile = .sr_cox_compile, present = .sr_cox_present_boot)
}

if (!(.sr_route_key("cox", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
  sr_route_register(.sr_cox_route())
}
if (!(.sr_route_key("cox_describe", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
  sr_route_register(.sr_cox_describe_route())
}
