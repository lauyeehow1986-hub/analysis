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
# The groups are the distinct values of the exposure, ordered as the original type (numbers as numbers),
# the reference first. The code stops when the exposure has more than 8 distinct values among the rows
# analysed ("categorise it first", ruling R24a): a continuous exposure would give one curve per value.
# present() stops when a primary difference is not finite (ruling R24b).
#
# Effects. One RMST_diff row per non-reference level: role "primary", scale "difference", level the
# compared level. One RMST row per level: role "descriptive". All rows carry n and events of their own
# group (the difference rows those of the analysis). figures$km and figures$km_risk are the Kaplan-Meier
# data by level, as for the Cox route.
#
# Multiple imputation (layers$missing = "mi" with spec$mi; impute/impute.R): rows with a missing time or
# status are dropped (never imputed) and counted; a missing exposure is imputed from the event indicator
# and the Nelson-Aalen cumulative hazard. In every completed dataset each group's RMST and SE and the
# differences against the reference (variance se_j^2 + se_ref^2) are computed; the differences and
# the per-group RMSTs are pooled by Rubin's rules on the identity scale, normal intervals. The group
# sizes, events and numbers at risk are means over the imputations. The horizon check and the
# Kaplan-Meier data use the rows with an observed exposure.
#
# The bootstrap (layers$resampling = "bootstrap", spec$boot) is resample/boot.R: the RMST differences
# take bootstrap intervals (clustered RMST stays refused).
#
# Not implemented (refused at compile, never silently ignored): weights, matching and the bayesian
# framework.

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
  .sr_exclude_refuse(spec, "rmst")
  .sr_surv_check(spec)
  if (isTRUE(spec[["layers"]][["clustered"]])) {
    stop("clustered RMST is not supported in P1c", call. = FALSE)
  }
  invisible(TRUE)
}

.sr_rmst_estimand <- function(spec) {
  .sr_rmst_check(spec)
  sr_estimand(population = if (identical(spec[["layers"]][["missing"]], "mi")) "all analysed (MI)" else "complete cases",
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
  if (.sr_boot_on(spec)) return(.sr_boot_compile(spec, .sr_rmst_compile, "rmst", "rmst"))
  if (.sr_mi_on(spec)) return(.sr_rmst_mi_compile(spec))
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
    sprintf("des[[\".ex\"]] <- des[[%s]]", .sr_lit(spec[["exposure"]])),
    ".keep <- stats::complete.cases(des[c(\".time\", \".event\", \".ex\")])",
    "if (!any(.keep)) stop(\"no complete rows: every row has a missing value in the model\")",
    "des <- des[.keep, , drop = FALSE]",
    "if (sum(des[[\".event\"]]) == 0L) {",
    "  stop(\"the outcome has no events among the \", nrow(des), \" rows analysed\")",
    "}",
    sprintf(".exn <- %s", .sr_lit(spec[["exposure"]])),
    ".nlv <- length(unique(des[[\".ex\"]]))",
    "if (.nlv > 8L) {",
    "  stop(\"RMST compares groups: exposure '\", .exn, \"' has \", .nlv, \" distinct values; categorise it first\")",
    "}",
    sprintf(".lv <- .h_levels(des[[\".ex\"]], %s)", if (has_ref) .sr_lit(as.character(ref)) else "NULL"),
    "des[[\".grp\"]] <- as.character(des[[\".ex\"]])",
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

# The compile under multiple imputation: the exposure is the one (factor) term of an internal spec for
# sr_mi_code(); in every completed dataset the group RMSTs and the differences are computed and pooled.
# The internal spec of the MI code: the exposure as the one factor term (and the bootstrap-within marker
# of impute_then_boot, resample/boot.R).
.sr_rmst_isp <- function(spec) {
  isp <- list(time = spec[["time"]], status = spec[["status"]], status_event = spec[["status_event"]],
              terms = list(list(var = spec[["exposure"]], type = "factor", ref = spec[["exposure_ref"]])),
              layers = spec[["layers"]], mi = spec[["mi"]])
  if (!is.null(spec[[".sr_boot_within"]])) isp[[".sr_boot_within"]] <- spec[[".sr_boot_within"]]
  isp
}

.sr_rmst_mi_compile <- function(spec) {
  .sr_mi_check(spec, "rmst")
  isp <- .sr_rmst_isp(spec)
  pre <- c(
    sprintf(".tau <- %s", .sr_lit(spec[["tau"]])),
    ".lv <- .sr_fixed$levels[[\"t1\"]]",
    sprintf(".exn <- %s", .sr_lit(spec[["exposure"]])),
    "if (length(.lv) > 8L) {",
    "  stop(\"RMST compares groups: exposure '\", .exn, \"' has \", length(.lv), \" distinct values; categorise it first\")",
    "}",
    "if (sum(des[[\".event\"]]) == 0L) {",
    "  stop(\"the outcome has no events among the \", nrow(des), \" rows with a time and a status\")",
    "}",
    ".kmd <- des[!is.na(des[[\"t1f\"]]), , drop = FALSE]",
    ".kgrp <- as.character(.kmd[[\"t1f\"]])",
    ".mx <- vapply(.lv, function(l) if (any(.kgrp == l)) max(.kmd[[\".time\"]][.kgrp == l]) else -Inf, numeric(1))",
    ".far <- which(.mx < .tau)",
    "if (length(.far)) {",
    paste0("  stop(\"tau = \", .tau, \" exceeds the follow-up of group '\", .lv[.far[1L]], \"' (max time \", ",
           ".mx[.far[1L]], \")\")"),
    "}",
    ".kmo <- .h_km(.kmd[[\".time\"]], .kmd[[\".event\"]], factor(.kgrp, levels = .lv))")
  fit <- c(
    ".g <- as.character(des[[\"t1f\"]])",
    ".r <- lapply(.lv, function(l) {",
    "  i <- .g == l",
    paste0("  c(.h_rmst(des[[\".time\"]][i], des[[\".event\"]][i], .tau), n = sum(i), ",
           "e = sum(des[[\".event\"]][i]), r = sum(des[[\".time\"]][i] >= .tau))"),
    "})",
    ".k <- length(.lv)",
    ".est <- vapply(.r, function(x) x[[\"rmst\"]], numeric(1))",
    ".se <- vapply(.r, function(x) x[[\"se\"]], numeric(1))",
    ".d <- stats::setNames(.est[-1L] - .est[1L], paste0(\"d\", seq_len(.k)[-1L]))",
    ".dv <- diag(.se[-1L]^2 + .se[1L]^2, nrow = .k - 1L)",
    "dimnames(.dv) <- list(names(.d), names(.d))",
    ".sc <- data.frame(key = paste0(\"g\", seq_len(.k)), est = .est, se = .se, prim = FALSE, stringsAsFactors = FALSE)",
    paste0(".num <- stats::setNames(unlist(lapply(.r, function(x) x[c(\"n\", \"e\", \"r\")])), ",
           "paste0(rep(c(\"n\", \"e\", \"r\"), .k), rep(seq_len(.k), each = 3L)))"),
    paste0("list(est = .d, vcov = .dv, dfcom = Inf, aliased = character(), scal = .sc, num = .num, ",
           "flags = NULL, notes = character())"))
  helpers <- c(.sr_inline(.sr_h_km, ".h_km"), .sr_inline(.sr_h_rmst, ".h_rmst"))
  code <- c(
    sr_mi_code(isp, fit, "surv", helpers = helpers, pre = pre, group = TRUE),
    ".j <- seq_along(.lv)",
    paste0(".groups <- data.frame(level = .lv, rmst = .mi$scal$est, se = .mi$scal$se, lo = .mi$scal$lo, ",
           "hi = .mi$scal$hi, mcse = .mi$scal$mcse, n = unname(.mi$num[paste0(\"n\", .j)]), ",
           "events = unname(.mi$num[paste0(\"e\", .j)]), at_risk = unname(.mi$num[paste0(\"r\", .j)]), ",
           "stringsAsFactors = FALSE)"),
    paste0(".diffs <- data.frame(level = .lv[-1L], est = .mi$coef$est, se = .mi$coef$se, lo = .mi$coef$lo, ",
           "hi = .mi$coef$hi, p = .mi$coef$p, mcse = .mi$coef$mcse, stringsAsFactors = FALSE)"),
    paste0("list(groups = .groups, diffs = .diffs, tau = .tau, n = nrow(des), n_total = nrow(sr_data), ",
           "events = sum(des[[\".event\"]]), level = 0.95, km = .kmo$km, km_risk = .kmo$risk, ",
           "coef = .mi$coef, mi = .mi_info)"))
  paste(code, collapse = "\n")
}

# The present step under multiple imputation: the pooled RMSTs and differences (Rubin, identity scale).
.sr_rmst_mi_present <- function(value, spec) {
  g <- value$groups
  d <- value$diffs
  exposure <- spec[["exposure"]]
  fmt_tau <- format(value$tau)
  gl <- g$level
  ref <- gl[1L]
  per_group <- data.frame(
    term = sprintf("RMST up to %s (%s = %s)", fmt_tau, exposure, gl), measure = "RMST",
    est = g$rmst, lo = g$lo, hi = g$hi, p = NA_real_,
    scale = NA_character_, level = gl, mcse = g$mcse, role = "descriptive", conditional = FALSE,
    ci_method = "rubin", n = g$n, events = g$events, stringsAsFactors = FALSE)
  diffs <- data.frame(
    term = sprintf("%s: %s vs %s", exposure, d$level, ref), measure = "RMST_diff",
    est = d$est, lo = d$lo, hi = d$hi, p = d$p,
    scale = "difference", level = d$level, mcse = d$mcse, role = "primary", conditional = FALSE,
    ci_method = "rubin", n = value$n, events = value$events, stringsAsFactors = FALSE)
  bad <- diffs[!is.finite(diffs$est) | !is.finite(diffs$lo) | !is.finite(diffs$hi), , drop = FALSE]
  if (nrow(bad)) {
    stop("The RMST difference for \"", bad$term[1L], "\" is not finite: the standard error of a group's restricted ",
         "mean could not be computed (a group with too few subjects or events), so the comparison cannot be reported.",
         call. = FALSE)
  }
  effects <- rbind(diffs, per_group)
  rownames(effects) <- NULL
  tab <- data.frame(
    Group = gl, `N (mean)` = sr_fmt_num(g$n, digits = 1L), `Events (mean)` = sr_fmt_num(g$events, digits = 1L),
    `At risk at tau (mean)` = sr_fmt_num(g$at_risk, digits = 1L),
    `RMST (95% CI)` = sr_fmt_ci(g$rmst, g$lo, g$hi),
    `SE` = .sr_lm_num(g$se), check.names = FALSE, stringsAsFactors = FALSE)
  dtab <- data.frame(
    Comparison = diffs$term,
    `RMST difference (95% CI)` = sr_fmt_ci(diffs$est, diffs$lo, diffs$hi),
    SE = .sr_lm_num(d$se),
    P = sr_fmt_p(pmin(pmax(diffs$p, 0), 1)),
    check.names = FALSE, stringsAsFactors = FALSE)
  tables <- list(rmst = tab, rmst_difference = dtab)
  frag <- list(rmst_note = sprintf(
    paste0("Restricted mean survival time up to tau = %s by %s (reference %s), in each of %d imputed datasets; each ",
           "group is its own Kaplan-Meier curve and the differences treat the groups as independent; %d row(s) ",
           "analysed (%d events), %d excluded for a missing time or status; pooled by Rubin's rules, normal intervals."),
    fmt_tau, exposure, ref, as.integer(round(value$mi$m)), as.integer(round(value$n)),
    as.integer(round(value$events)), as.integer(round(value$n_total - value$n))))
  thin <- g$at_risk < 10
  if (any(thin)) {
    frag$rmst_few_at_risk <- sprintf(
      "Fewer than 10 subjects (mean over the imputations) are still at risk at tau = %s in group(s) %s (%s): the RMST there rests on the thin tail of the curve and is imprecise.",
      fmt_tau, paste(gl[thin], collapse = ", "), paste(sr_fmt_num(g$at_risk[thin], digits = 1L), collapse = ", "))
  }
  mp <- .sr_mi_present(value, "rmst")
  tables$imputation <- mp$table
  frag <- c(frag, mp$fragments)
  list(tables = tables, figures = list(km = value$km, km_risk = value$km_risk), fragments = frag,
       effects = effects)
}

# ---- present ------------------------------------------------------------------------------------

.sr_rmst_present <- function(value, spec) {
  if (!is.null(value$mi)) return(.sr_rmst_mi_present(value, spec))
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
  bad <- diffs[!is.finite(diffs$est) | !is.finite(diffs$lo) | !is.finite(diffs$hi), , drop = FALSE]
  if (nrow(bad)) {
    stop("The RMST difference for \"", bad$term[1L], "\" is not finite: the standard error of a group's restricted ",
         "mean could not be computed (a group with too few subjects or events), so the comparison cannot be reported.",
         call. = FALSE)
  }
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
    status_event = list(type = "num|chr", required = FALSE),
    exposure = list(type = "chr", required = TRUE),
    tau = list(type = "num", required = TRUE),
    layers = list(type = "list", required = TRUE),
    seed = list(type = "num", required = TRUE)
  )
}

# The present step of the rmst route: the bootstrap bounds when the value has them (resample/boot.R).
.sr_rmst_present_boot <- function(value, spec) .sr_boot_present(value, spec, .sr_rmst_present, "rmst")

.sr_rmst_route <- function() {
  sr_route(id = "rmst", version = 1L, family = "survival", purpose = "explain-causal",
           estimand = .sr_rmst_estimand, spec_schema = .sr_rmst_schema(),
           compile = .sr_rmst_compile, present = .sr_rmst_present_boot)
}

if (!(.sr_route_key("rmst", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
  sr_route_register(.sr_rmst_route())
}
