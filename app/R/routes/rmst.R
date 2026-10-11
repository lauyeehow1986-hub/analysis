# routes/rmst.R - rmst@1 (explain-causal): the restricted mean survival time up to a horizon tau, by exposure
# level. Each exposure level is a Kaplan-Meier curve of its own (survival::survfit(Surv(time, event) ~ 1)
# on the complete cases of time, status and exposure); its RMST and the SE of it are the "rmean" and
# "se(rmean)" of summary(fit, rmean = tau). The effect is the RMST difference of each level against the
# reference level (the first level: the smallest value, or spec$exposure_ref), treating the groups as
# independent: difference = RMST_j - RMST_ref, SE = sqrt(se_j^2 + se_ref^2), normal (z) interval and
# two-sided p. There is no covariate adjustment; clustered data are refused (ruling R23b).
#
# The horizon check is part of the emitted code, a plain data-driven `if (...) stop(...)`: when tau is
# larger than the longest observed time of any group the code stops with a message naming tau, the
# group and its maximum (the run then ends with that message as its error; ruling R23g). A group with
# fewer than 10 subjects still at risk at tau gets a note (fragment rmst_few_at_risk), because the
# RMST then rests on the thin tail of the curve.
#
# Effects. One RMST_diff row per non-reference level: role "primary", scale "difference", level the
# compared level. One RMST row per level: role "descriptive". All rows carry n and events of their own
# group (the difference rows those of the analysis). figures$km and figures$km_risk are the Kaplan-Meier
# data by level, as for the Cox route.
#
# Not implemented (refused at compile, never silently ignored): multiple imputation, weights, matching,
# the bootstrap and the bayesian framework.

# tau: one finite number above zero. Stops with the reason.
.sr_rmst_tau_check <- function(tau) {
  if (!(is.numeric(tau) && length(tau) == 1L && !is.na(tau) && is.finite(tau) && tau > 0)) {
    stop("tau must be one finite number above zero (the horizon of the restricted mean, in the unit of the time column)",
         call. = FALSE)
  }
  invisible(TRUE)
}

.sr_rmst_check <- function(spec) {
  .sr_rmst_tau_check(spec[["tau"]])
  .sr_surv_check(spec)
  if (isTRUE(spec[["layers"]][["clustered"]])) {
    stop("clustered RMST is not supported in P1c", call. = FALSE)
  }
  invisible(TRUE)
}

.sr_rmst_estimand <- function(spec) {
  .sr_rmst_check(spec)
  sr_estimand(population = "complete cases",
              variables = c(spec[["exposure"]], spec[["time"]], spec[["status"]]),
              intercurrent = "other reasons for censoring are treated as non-informative censoring",
              summary = sprintf("restricted mean survival time difference up to tau = %s", format(spec[["tau"]])),
              level = "marginal")
}

# RMST and its SE up to tau of one group: c(rmst, se). Inlined into the emitted code as .h_rmst.
.sr_h_rmst <- function(time, event, tau) {
  fit <- survival::survfit(survival::Surv(.t, .e) ~ 1, data = data.frame(.t = time, .e = event))
  tab <- summary(fit, rmean = tau)$table
  c(rmst = unname(tab[["rmean"]]), se = unname(tab[["se(rmean)"]]))
}

# ---- compile ------------------------------------------------------------------------------------

.sr_rmst_compile <- function(spec) {
  .sr_rmst_check(spec)
  .sr_glm_layers_check(spec, "rmst")
  cols <- c(spec[["time"]], spec[["status"]], spec[["exposure"]])
  ref <- spec[["exposure_ref"]]
  has_ref <- length(ref) == 1L && !is.na(ref) && nzchar(as.character(ref))
  code <- c(
    sprintf(".miss <- setdiff(%s, names(sr_data))", .sr_lit(cols)),
    "if (length(.miss)) stop(\"column(s) not in the data: \", paste(.miss, collapse = \", \"))",
    .sr_inline(.h_levels, ".h_levels"),
    .sr_inline(.sr_h_km, ".h_km"),
    .sr_inline(.sr_h_rmst, ".h_rmst"),
    sprintf(".tau <- %s", .sr_lit(spec[["tau"]])),
    sprintf("des <- as.data.frame(sr_data)[%s]", .sr_lit(cols)),
    sr_outcome_code(spec, "surv"),
    sprintf("des[[\".grp\"]] <- as.character(des[[%s]])", .sr_lit(spec[["exposure"]])),
    ".keep <- stats::complete.cases(des[c(\".time\", \".event\", \".grp\")])",
    "if (!any(.keep)) stop(\"no complete rows: every row has a missing value in the model\")",
    "des <- des[.keep, , drop = FALSE]",
    "if (sum(des[[\".event\"]]) == 0L) {",
    "  stop(\"the outcome has no events among the \", nrow(des), \" rows analysed\")",
    "}",
    sprintf(".lv <- .h_levels(des[[\".grp\"]], %s)", if (has_ref) .sr_lit(as.character(ref)) else "NULL"),
    ".mx <- vapply(.lv, function(l) max(des[[\".time\"]][des[[\".grp\"]] == l]), numeric(1))",
    ".far <- which(.mx < .tau)",
    "if (length(.far)) {",
    paste0("  stop(\"tau = \", .tau, \" exceeds the follow-up of group '\", .lv[.far[1L]], \"' (max time \", ",
           ".mx[.far[1L]], \")\")"),
    "}",
    ".rows <- lapply(.lv, function(l) {",
    "  i <- des[[\".grp\"]] == l",
    "  r <- .h_rmst(des[[\".time\"]][i], des[[\".event\"]][i], .tau)",
    paste0("  data.frame(level = l, rmst = r[[\"rmst\"]], se = r[[\"se\"]], n = sum(i), ",
           "events = sum(des[[\".event\"]][i]), at_risk = sum(des[[\".time\"]][i] >= .tau), ",
           "max_time = max(des[[\".time\"]][i]), stringsAsFactors = FALSE)"),
    "})",
    "groups <- do.call(rbind, .rows)",
    ".kmo <- .h_km(des[[\".time\"]], des[[\".event\"]], factor(des[[\".grp\"]], levels = .lv))",
    paste0("list(groups = groups, tau = .tau, n = nrow(des), n_total = nrow(sr_data), ",
           "events = sum(des[[\".event\"]]), level = 0.95, km = .kmo$km, km_risk = .kmo$risk)")
  )
  paste(code, collapse = "\n")
}

# ---- present ------------------------------------------------------------------------------------

.sr_rmst_present <- function(value, spec) {
  g <- value$groups
  exposure <- spec[["exposure"]]
  tau <- value$tau
  z <- stats::qnorm(0.975)
  ref <- g$level[1L]
  fmt_tau <- format(tau)
  k <- nrow(g)
  gl <- g$level

  per_group <- data.frame(
    term = sprintf("RMST up to %s (%s = %s)", fmt_tau, exposure, gl), measure = "RMST",
    est = g$rmst, lo = g$rmst - z * g$se, hi = g$rmst + z * g$se, p = NA_real_,
    scale = NA_character_, level = gl, mcse = NA_real_, role = "descriptive", conditional = FALSE,
    ci_method = "z_km_variance", n = g$n, events = g$events, stringsAsFactors = FALSE)

  js <- seq_len(k)[-1L]
  dse <- sqrt(g$se[js]^2 + g$se[1L]^2)
  dest <- g$rmst[js] - g$rmst[1L]
  diffs <- data.frame(
    term = sprintf("%s: %s vs %s", exposure, gl[js], ref), measure = "RMST_diff",
    est = dest, lo = dest - z * dse, hi = dest + z * dse, p = 2 * stats::pnorm(-abs(dest / dse)),
    scale = "difference", level = gl[js], mcse = NA_real_, role = "primary", conditional = FALSE,
    ci_method = "z_independent_groups", n = value$n, events = value$events, stringsAsFactors = FALSE)
  effects <- rbind(diffs, per_group)
  rownames(effects) <- NULL

  tab <- data.frame(
    Group = gl, N = sprintf("%d", as.integer(g$n)), Events = sprintf("%d", as.integer(g$events)),
    `At risk at tau` = sprintf("%d", as.integer(g$at_risk)),
    `RMST (95% CI)` = sr_fmt_ci(g$rmst, g$rmst - z * g$se, g$rmst + z * g$se),
    `SE` = .sr_lm_num(g$se), check.names = FALSE, stringsAsFactors = FALSE)
  dtab <- data.frame(
    Comparison = diffs$term,
    `RMST difference (95% CI)` = sr_fmt_ci(diffs$est, diffs$lo, diffs$hi),
    SE = .sr_lm_num(dse),
    P = sr_fmt_p(pmin(pmax(diffs$p, 0), 1)),
    check.names = FALSE, stringsAsFactors = FALSE)
  tables <- list(rmst = tab, rmst_difference = dtab)

  frag <- list(rmst_note = sprintf(
    paste0("Restricted mean survival time up to tau = %s by %s (reference %s); each group is its own Kaplan-Meier ",
           "curve and the differences treat the groups as independent; %d row(s) analysed (%d events), %d excluded ",
           "for a missing value; normal (z) intervals."),
    fmt_tau, exposure, ref, as.integer(round(value$n)), as.integer(round(value$events)),
    as.integer(round(value$n_total - value$n))))
  thin <- g$at_risk < 10
  if (any(thin)) {
    frag$rmst_few_at_risk <- sprintf(
      "Fewer than 10 subjects are still at risk at tau = %s in group(s) %s (%s): the RMST there rests on the thin tail of the curve and is imprecise.",
      fmt_tau, paste(gl[thin], collapse = ", "), paste(g$at_risk[thin], collapse = ", "))
  }
  list(tables = tables, figures = list(km = value$km, km_risk = value$km_risk), fragments = frag,
       effects = effects)
}

# ---- the route ----------------------------------------------------------------------------------

.sr_rmst_schema <- function() {
  list(
    time = list(type = "chr", required = TRUE),
    status = list(type = "chr", required = TRUE),
    status_event = list(type = "num", required = FALSE),
    exposure = list(type = "chr", required = TRUE),
    tau = list(type = "num", required = TRUE),
    layers = list(type = "list", required = TRUE),
    seed = list(type = "num", required = TRUE)
  )
}

.sr_rmst_route <- function() {
  sr_route(id = "rmst", version = 1L, family = "survival", purpose = "explain-causal",
           estimand = .sr_rmst_estimand, spec_schema = .sr_rmst_schema(),
           compile = .sr_rmst_compile, present = .sr_rmst_present)
}

if (!(.sr_route_key("rmst", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
  sr_route_register(.sr_rmst_route())
}
