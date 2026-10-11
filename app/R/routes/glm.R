# routes/glm.R - the binary-outcome regression routes: logistic@1 (explain-causal) with its describe
# twin logistic_describe@1, and modpois@1 (modified Poisson, Zou 2004). Each pair shares its compile and
# present functions. The model is
#   outcome ~ exposure + terms (+ interactions)
# fitted on the complete cases (rows with a missing outcome, term or cluster value are dropped and
# counted): stats::glm with a binomial (logit) family for logistic, a Poisson log-link family for
# modpois, or logistf::logistf (Firth penalised likelihood) for logistic with penalty = "firth".
# The standard errors are those of .h_vcov: model-based, robust (HC0, the form defined for a glm) or
# cluster (HC1). Modified Poisson is only valid with a robust or cluster SE, so se defaults to robust
# there and "model" is refused. Intervals are normal (z); Firth uses profile-likelihood intervals.
#
# The compiled code is data-free (see routes/terms.R): every model column has a derived name (t1,
# t2f, ...), and present() maps the coefficients back to the user variables with sr_coef_source().
# Outcome coding: a logical or 0/1 outcome is used as is; any other column needs event_value, the one
# value that counts as the event. That cannot be checked at validation (there is no data), so a
# missing event_value on a non-0/1 outcome is refused at run time, by the emitted code.
#
# Effects. The exposure coefficient(s) are conditional ratios (OR for logistic, RR for modpois), role
# "primary", scale "ratio", one row per non-reference level for a factor exposure; every other
# coefficient is role "covariate" (the Table 2 flag is added by sr_result_check). A conditional OR is
# not collapsible, so sr_result_check flags it and the marginal measures sit next to it.
# Marginal effects (marginal, default on for a binary or categorical exposure, off with Firth): the
# g-computation risk difference and risk ratio with delta-method intervals, and the NNT from the RD
# (routes/marginal.R), standardised over the fitted rows ("analysis") or also the rows lost only to a
# missing outcome ("all"). They are skipped with a note for a continuous exposure unless marginal = TRUE
# was asked for explicitly, which is then an error. The observed risk by exposure level, with Wilson
# intervals, is a table.
#
# Not implemented (refused at compile, never silently ignored): multiple imputation, weights, matching,
# the bootstrap and the bayesian framework.

.sr_glm_ses <- c("model", "robust", "cluster")

# "logistic" or "modpois", the kind of a route function; the spec has no knowledge of it.
.sr_glm_default_se <- function(kind) if (identical(kind, "modpois")) "robust" else "model"

# The effective SE type of a spec.
.sr_glm_se <- function(spec, kind) {
  se <- spec[["se"]]
  if (is.null(se)) .sr_glm_default_se(kind) else se
}

.sr_glm_firth <- function(spec) identical(spec[["penalty"]], "firth")

# Whether the marginal block runs: on unless marginal = FALSE or the model is Firth penalised.
.sr_glm_marg_on <- function(spec) !isFALSE(spec[["marginal"]]) && !.sr_glm_firth(spec)

# Cross-field validation of a spec (the schema has been checked already); stops with the reason.
.sr_glm_check <- function(spec, kind) {
  one <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!one(spec[["outcome"]])) stop("outcome must be one column name", call. = FALSE)
  if (!one(spec[["exposure"]])) stop("exposure must be one column name", call. = FALSE)
  if (identical(spec[["outcome"]], spec[["exposure"]])) {
    stop("the outcome and the exposure must be different columns", call. = FALSE)
  }
  if (!is.null(spec[["event_value"]]) && !one(spec[["event_value"]])) {
    stop("event_value must be one non-empty value (the outcome value that counts as the event)", call. = FALSE)
  }
  se <- .sr_glm_se(spec, kind)
  if (identical(kind, "modpois")) {
    if (identical(se, "model")) {
      stop("modified Poisson needs a robust or cluster SE (the model-based SE of a Poisson fit is not valid for a binary outcome)",
           call. = FALSE)
    }
    if (!is.null(spec[["penalty"]])) stop("modified Poisson has no penalty option", call. = FALSE)
  }
  if (identical(se, "cluster")) {
    if (!one(spec[["cluster"]])) {
      stop("se = \"cluster\" needs a cluster column (spec$cluster)", call. = FALSE)
    }
    if (!isTRUE(spec[["layers"]][["clustered"]])) {
      stop("se = \"cluster\" needs layers$clustered = TRUE", call. = FALSE)
    }
  }
  if (.sr_glm_firth(spec)) {
    if (isTRUE(spec[["marginal"]])) {
      stop("Firth penalised logistic regression has no marginal effects: set marginal = FALSE or penalty = \"none\"",
           call. = FALSE)
    }
    if (se != "model") {
      stop("Firth penalised logistic regression has model-based (profile likelihood) intervals only: se must be \"model\"",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

# The estimand, a function of the spec so that it follows the missing-data layer and the marginal
# switch. Ruling R18: the route cross-field validation lives in this function-form estimand
# (sr_spec_validate() evaluates it once the schema holds), through .sr_glm_check().
.sr_glm_estimand_fn <- function(kind) {
  force(kind)
  function(spec) {
    .sr_glm_check(spec, kind)
    mi <- identical(spec[["layers"]][["missing"]], "mi")
    marg <- .sr_glm_marg_on(spec)
    sr_estimand(population = if (mi) "all analysed (MI)" else "complete cases",
                variables = c(spec[["exposure"]], spec[["outcome"]]),
                intercurrent = "not applicable",
                summary = if (marg) "marginal (standardised) risk difference and ratio" else
                  if (identical(kind, "modpois")) "adjusted risk ratio" else "adjusted odds ratio",
                level = if (marg) "marginal" else "conditional")
  }
}

# Layers the route does not implement yet: stop rather than analyse as if they were absent.
.sr_glm_layers_check <- function(spec, id) {
  L <- spec[["layers"]]
  todo <- c(
    if (!is.null(L[["missing"]]) && !identical(L[["missing"]], "cc")) "missing = mi (multiple imputation)",
    if (!is.null(L[["weights"]]) && !identical(L[["weights"]], "none")) paste0("weights = ", L[["weights"]]),
    if (isTRUE(L[["matching"]])) "matching",
    if (!is.null(L[["resampling"]]) && !identical(L[["resampling"]], "none")) paste0("resampling = ", L[["resampling"]]),
    if (!is.null(L[["framework"]]) && !identical(L[["framework"]], "frequentist")) paste0("framework = ", L[["framework"]])
  )
  if (length(todo)) {
    stop(id, "@1 does not implement the layer(s): ", paste(todo, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# ---- compile ------------------------------------------------------------------------------------

# The lines that count events and Wilson-interval the observed risk at each exposure level (a factor
# exposure, or a 0/1 one; a continuous exposure gives an empty table).
.sr_glm_obs_code <- function(spec) {
  fac <- identical(.sr_term_plan(spec)[[1L]]$type, "factor")
  c(".obs <- NULL",
    if (fac) {
      c(".exl <- .sr_fixed$levels[[\"t1\"]]", ".exc <- \"t1f\"")
    } else {
      c(".exl <- if (all(des[[\"t1\"]] %in% c(0, 1))) c(0, 1)", ".exc <- \"t1\"")
    },
    "for (.l in .exl) {",
    "  .i <- des[[.exc]] == .l",
    "  .e <- sum(des[[\".y\"]][.i])",
    "  .w <- .h_wilson(.e, sum(.i))",
    paste0("  .obs <- rbind(.obs, data.frame(level = as.character(.l), n = sum(.i), events = .e, ",
           "est = .w[[\"est\"]], lo = .w[[\"lo\"]], hi = .w[[\"hi\"]], stringsAsFactors = FALSE))"),
    "}")
}

.sr_glm_compile_fn <- function(kind) {
  force(kind)
  id <- kind
  function(spec) {
    .sr_glm_check(spec, kind)
    .sr_glm_layers_check(spec, id)
    firth <- .sr_glm_firth(spec)
    se <- .sr_glm_se(spec, kind)
    tc <- sr_terms_code(spec)
    cl <- if (identical(se, "cluster")) spec[["cluster"]] else NULL
    rhs_vars <- all.vars(stats::as.formula(paste("~", tc$rhs)))
    marg_on <- .sr_glm_marg_on(spec)
    mg <- .sr_marg_code(spec, rhs_vars, on = marg_on, strict = isTRUE(spec[["marginal"]]))
    used <- c(".y", rhs_vars, cl)
    cluster_arg <- if (is.null(cl)) "NULL" else sprintf("des[[%s]]", .sr_lit(cl))
    family <- if (identical(kind, "modpois")) "stats::poisson(link = \"log\")" else "stats::binomial()"
    fit_code <- if (firth) {
      c(sprintf("fit <- logistf::logistf(.y ~ %s, data = des)", tc$rhs),
        ".cf <- fit$coefficients",
        ".ok <- rep(TRUE, length(.cf))",
        ".ncl <- NA_real_",
        paste0("tab <- data.frame(term = names(.cf), est = unname(.cf), se = sqrt(diag(fit$var)), ",
               "lo = unname(fit$ci.lower), hi = unname(fit$ci.upper), p = unname(fit$prob), ",
               "stringsAsFactors = FALSE)"),
        ".conv <- TRUE",
        ".sep <- FALSE",
        ".rab <- FALSE",
        ".aliased <- character()")
    } else {
      c(sprintf("fit <- suppressWarnings(stats::glm(.y ~ %s, data = des, family = %s))", tc$rhs, family),
        ".cf <- stats::coef(fit)",
        ".ok <- !is.na(.cf)",
        ".nm <- names(.cf)[.ok]",
        ".ncl <- NA_real_",
        if (!is.null(cl)) c(
          sprintf(".ncl <- length(unique(des[[%s]]))", .sr_lit(cl)),
          "if (.ncl < 2L) stop(\"se = cluster needs at least two clusters (found \", .ncl, \")\")"
        ),
        sprintf(".V <- .h_vcov(fit, %s, %s)[.nm, .nm, drop = FALSE]", .sr_lit(se), cluster_arg),
        "tab <- .h_coef_table(.cf[.ok], .V, df = Inf)",
        ".conv <- isTRUE(fit$converged)",
        if (identical(kind, "modpois")) {
          # separation is a binomial notion; a log-link fit can instead predict risks above 1
          c(".sep <- FALSE", ".rab <- any(fit$fitted.values > 1)")
        } else {
          c(".sep <- any(fit$fitted.values < 1e-08 | fit$fitted.values > 1 - 1e-08)", ".rab <- FALSE")
        },
        ".aliased <- names(.cf)[!.ok]")
    }
    code <- c(
      tc$setup,
      if (!firth) c(.sr_inline(.sr_h_vcov, ".h_vcov"), .sr_inline(.sr_h_coef_table, ".h_coef_table")),
      if (marg_on) .sr_inline(.sr_h_gcomp, ".h_gcomp"),
      .sr_inline(.sr_h_wilson, ".h_wilson"),
      tc$design,
      "des <- .h_design(sr_data)",
      sr_outcome_code(spec, "binary"),
      mg$pre,
      sprintf(".keep <- stats::complete.cases(des[%s])", .sr_lit(used)),
      "if (!any(.keep)) stop(\"no complete rows: every row has a missing value in the model\")",
      "des <- des[.keep, , drop = FALSE]",
      "if (sum(des[[\".y\"]]) == 0L || all(des[[\".y\"]] == 1L)) {",
      "  stop(\"the outcome has no events or no non-events among the \", nrow(des), \" rows analysed\")",
      "}",
      fit_code,
      mg$post,
      .sr_glm_obs_code(spec),
      paste0("list(coef = tab, aliased = .aliased, n = nrow(des), n_total = nrow(sr_data), ",
             "events = sum(des[[\".y\"]]), df = Inf, se_type = ", .sr_lit(se), ", n_clusters = .ncl, level = 0.95, ",
             "levels = .sr_fixed$levels, penalty = ", .sr_lit(if (firth) "firth" else "none"),
             ", family = ", .sr_lit(if (identical(kind, "modpois")) "poisson" else "binomial"),
             ", converged = .conv, near_separation = .sep, risk_above_one = .rab, obs = if (is.null(.obs)) data.frame() else .obs, ",
             "marginal = ", mg$value, ")")
    )
    paste(code, collapse = "\n")
  }
}

# ---- present ------------------------------------------------------------------------------------

.sr_glm_se_text <- function(value) {
  if (identical(value$penalty, "firth")) return("profile penalised-likelihood intervals")
  switch(value$se_type,
    model = "model-based standard errors, normal (z) intervals",
    robust = "robust (HC0) standard errors, normal (z) intervals",
    cluster = sprintf("cluster-robust (HC1) standard errors, %d clusters, normal (z) intervals",
                      as.integer(round(value$n_clusters))),
    value$se_type)
}

.sr_glm_present <- function(value, spec) {
  tc <- sr_terms_code(spec)
  tab <- value$coef
  exposure <- spec[["exposure"]]
  ia <- spec[["interactions"]]
  has_ia <- any(vapply(ia, function(x) exposure %in% x, logical(1)))
  odds <- identical(value$family, "binomial")
  measure <- if (odds) "OR" else "RR"
  firth <- identical(value$penalty, "firth")
  ref_of <- function(coef) {
    id <- sub("f.*$", "", coef)
    lv <- value$levels[[id]]
    if (is.null(lv)) NA_character_ else as.character(lv[1L])
  }
  if (length(value$aliased)) {
    asrc <- sr_coef_source(value$aliased, tc$coef_map)
    if (any(asrc$source_var == exposure & asrc$kind %in% c("linear", "factor"))) {
      stop("the exposure \"", exposure, "\" is collinear with other terms of the model; its effect is not estimable",
           call. = FALSE)
    }
  }
  is_int <- tab$term == "(Intercept)"
  b <- tab[!is_int, , drop = FALSE]
  src <- if (nrow(b)) sr_coef_source(b$term, tc$coef_map) else NULL
  ci_base <- if (firth) "profile_firth" else c(model = "z_model", robust = "z_robust_hc0",
                                                cluster = "z_cluster_hc1")[[value$se_type]]
  if (nrow(b)) {
    lab <- .sr_lm_labels(b$term, src, ref_of)
    prim <- src$source_var == exposure & src$kind %in% c("linear", "factor")
    adjusted <- sum(!prim) > 0L
    effects <- data.frame(
      term = lab, measure = measure, est = exp(b$est), lo = exp(b$lo), hi = exp(b$hi), p = b$p,
      scale = "ratio", level = ifelse(is.na(src$level), NA_character_, src$level),
      mcse = NA_real_, role = ifelse(prim, "primary", "covariate"),
      conditional = prim & (has_ia | (odds & adjusted)), ci_method = ci_base,
      n = value$n, events = value$events, stringsAsFactors = FALSE)
  } else {
    effects <- sr_effects_empty()
    lab <- character()
  }
  rownames(effects) <- NULL

  all_lab <- c(if (any(is_int)) "(Intercept)", lab)
  all_tab <- rbind(tab[is_int, , drop = FALSE], b)
  est_name <- paste0(measure, " (95% CI)")
  coefs <- data.frame(
    Term = all_lab,
    Est = sr_fmt_ci(exp(all_tab$est), exp(ifelse(is.nan(all_tab$lo), NA_real_, all_tab$lo)),
                    exp(ifelse(is.nan(all_tab$hi), NA_real_, all_tab$hi))),
    SE = .sr_lm_num(all_tab$se),
    P = ifelse(is.na(all_tab$p), "NA", sr_fmt_p(pmin(pmax(all_tab$p, 0), 1))),
    stringsAsFactors = FALSE)
  names(coefs)[2L] <- est_name
  names(coefs)[3L] <- "SE (log scale)"
  rownames(coefs) <- NULL
  fit_tab <- data.frame(
    Statistic = c("Rows analysed", "Rows excluded (missing)", "Events", "Non-events"),
    Value = c(sprintf("%d", as.integer(round(value$n))),
              sprintf("%d", as.integer(round(value$n_total - value$n))),
              sprintf("%d", as.integer(round(value$events))),
              sprintf("%d", as.integer(round(value$n - value$events)))),
    stringsAsFactors = FALSE)
  tables <- list(coefficients = coefs, model_fit = fit_tab)

  what <- if (firth) "Firth penalised logistic regression" else
    if (odds) "Logistic regression" else "Modified Poisson regression (log link, robust variance)"
  frag <- list(glm_note = sprintf(
    "%s of %s on %s, adjusted for the other terms of the model; %d row(s) analysed (%d events), %d excluded for a missing value; %s.",
    what, spec[["outcome"]], exposure, as.integer(round(value$n)), as.integer(round(value$events)),
    as.integer(round(value$n_total - value$n)), .sr_glm_se_text(value)))
  if (length(value$aliased)) {
    alab <- .sr_lm_labels(value$aliased, asrc, ref_of)
    frag$glm_aliased <- sprintf("Dropped as perfectly collinear with the other terms: %s.",
                                paste(alab, collapse = ", "))
  }
  if (has_ia) {
    frag$glm_interaction <- paste("The exposure takes part in an interaction: its main-effect row is the effect",
                                  "where the interacting variable is 0 (or at its reference level), not an average effect.")
  }
  if (isFALSE(value$converged)) {
    frag$glm_convergence <- "The fitting algorithm did not converge: the estimates are unreliable."
  }
  if (isTRUE(value$near_separation)) {
    frag$glm_separation <- paste("Fitted probabilities are numerically 0 or 1 for some rows (near-separation):",
                                 "maximum-likelihood estimates and intervals are unreliable; consider the Firth penalised model.")
  }
  mgv <- value$marginal
  mg_above <- !is.null(mgv) && !is.null(mgv$rows) && any(c(mgv$rows$p1, mgv$rows$p0) > 1, na.rm = TRUE)
  if (!odds && (isTRUE(value$risk_above_one) || mg_above)) {
    frag$glm_risk_above_one <- paste("The log-link model predicts risks above 1 for some rows or for a standardised risk:",
                                     "the risk ratios are extrapolated beyond the possible range for those covariate values,",
                                     "so interpret them with caution.")
  }
  if (!is.null(value$obs) && nrow(value$obs)) {
    ob <- value$obs
    tables$observed_risk <- data.frame(
      `Exposure level` = ob$level, N = sprintf("%d", as.integer(round(ob$n))),
      Events = sprintf("%d", as.integer(round(ob$events))),
      `Risk (95% Wilson CI)` = sr_fmt_ci(ob$est, ob$lo, ob$hi), check.names = FALSE, stringsAsFactors = FALSE)
    # the crude risk per exposure level, descriptive only (the Wilson interval is not a model interval)
    effects <- rbind(effects, data.frame(
      term = paste0("Observed risk (", exposure, " = ", ob$level, ")"), measure = "risk",
      est = ob$est, lo = ob$lo, hi = ob$hi, p = NA_real_, scale = NA_character_, level = ob$level,
      mcse = NA_real_, role = "descriptive", conditional = FALSE, ci_method = "wilson",
      n = ob$n, events = ob$events, stringsAsFactors = FALSE))
    rownames(effects) <- NULL
  }
  if (!is.null(mgv) && !is.null(mgv$rows)) {
    mp <- .sr_marg_present(value, exposure, "risk", ci_base)
    effects <- rbind(effects, mp$effects)
    rownames(effects) <- NULL
    tables$marginal <- mp$table
    frag$glm_marginal <- sprintf(
      "Marginal risk difference and ratio by g-computation over %d row(s) (standardize_over = %s); delta-method intervals; NNT from the risk difference (Altman).",
      as.integer(round(mgv$n_over)), mgv$over)
  }
  if (!is.null(mgv) && is.null(mgv$rows) && !is.null(mgv$note)) frag$glm_marginal_skipped <- mgv$note
  list(tables = tables, figures = list(), fragments = frag, effects = effects)
}

# ---- the routes ---------------------------------------------------------------------------------

.sr_glm_schema <- function(penalty) {
  s <- list(
    outcome = list(type = "chr", required = TRUE),
    event_value = list(type = "chr", required = FALSE),
    exposure = list(type = "chr", required = TRUE),
    terms = list(type = "list", required = FALSE),
    interactions = list(type = "list", required = FALSE),
    se = list(type = "chr", required = FALSE, values = .sr_glm_ses),
    cluster = list(type = "chr", required = FALSE),
    marginal = list(type = "lgl", required = FALSE, default = TRUE),
    standardize_over = list(type = "chr", required = FALSE, values = c("analysis", "all")),
    layers = list(type = "list", required = TRUE),
    seed = list(type = "num", required = TRUE)
  )
  if (penalty) s$penalty <- list(type = "chr", required = FALSE, values = c("none", "firth"))
  s
}

# One estimand / compile function per kind, shared by a route and its describe twin.
.sr_glm_fns <- list(logistic = list(estimand = .sr_glm_estimand_fn("logistic"), compile = .sr_glm_compile_fn("logistic")),
                    modpois = list(estimand = .sr_glm_estimand_fn("modpois"), compile = .sr_glm_compile_fn("modpois")))

.sr_logistic_route <- function() {
  sr_route(id = "logistic", version = 1L, family = "regression", purpose = "explain-causal",
           estimand = .sr_glm_fns$logistic$estimand, spec_schema = .sr_glm_schema(TRUE),
           compile = .sr_glm_fns$logistic$compile, present = .sr_glm_present)
}

.sr_logistic_describe_route <- function() {
  sr_route(id = "logistic_describe", version = 1L, family = "regression", purpose = "describe",
           estimand = .sr_glm_fns$logistic$estimand, spec_schema = .sr_glm_schema(TRUE),
           compile = .sr_glm_fns$logistic$compile, present = .sr_glm_present)
}

.sr_modpois_route <- function() {
  sr_route(id = "modpois", version = 1L, family = "regression", purpose = "explain-causal",
           estimand = .sr_glm_fns$modpois$estimand, spec_schema = .sr_glm_schema(FALSE),
           compile = .sr_glm_fns$modpois$compile, present = .sr_glm_present)
}

if (!(.sr_route_key("logistic", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
  sr_route_register(.sr_logistic_route())
}
if (!(.sr_route_key("logistic_describe", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
  sr_route_register(.sr_logistic_describe_route())
}
if (!(.sr_route_key("modpois", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
  sr_route_register(.sr_modpois_route())
}
