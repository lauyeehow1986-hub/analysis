# diagnostics/plots.R - the figures: ggplot objects built from the data frames the routes and the
# diagnostics emit, and a deterministic PNG export.
#
#   sr_plot(data, kind)          a ggplot for one of the kinds below
#   sr_figure_save(plot, path)   write it as a PNG (ragg, white background) -> list(path, sha)
#
# The plots read only the data frames they are given (never a model object, the store or the
# project). A frame that lacks a required column is an error naming the missing columns. Every kind
# takes exactly what an earlier task emits:
#
#   km          list(km, km_risk) = figures$km / figures$km_risk of the cox and rmst routes (a bare km
#               frame also works: the strip is then read off its n.risk column at the pretty() times).
#               km: strata, time, surv (+ lower, upper, n.event, n.censor, n.risk as survfit gives them);
#               km_risk: strata, time, n.risk. One plot, two panels: the curves and the strip.
#   forest      an effects frame (SR_EFFECT_COLS): needs term, est, lo, hi, scale. est/lo/hi are
#               already on the reported scale. All rows "ratio": log10 x axis, reference line at 1;
#               all "difference": linear axis, reference line at 0. Mixed scales are refused. Rows with
#               a non-finite est are not drawn; rows without an interval are drawn as points.
#   resid_panel figures$resid_panel of the lm route: fitted, resid, std_resid, leverage, cooks.
#               Reshaped to one long frame (panel, x, y, flag) and drawn as four facets.
#   schoenfeld  plot_data of the ph check: term, time, resid (scaled Schoenfeld residuals).
#   loglog      a km frame: log(time) against log(-log(surv)) by stratum (parallel curves are
#               what proportional hazards looks like; descriptive, no test decides). No route emits
#               a separate loglog frame: it is the km frame transformed here.
#   smd         Table 1 SMD rows, as the table1 route's effects frame (term, est; lo, hi optional;
#               when a measure column exists only the "SMD" rows are kept): dots with +-0.1 lines.
#   linearity   plot_data of the linearity check: var, x, linear, rcs (the linear and the RCS curve).
#
# The export is deterministic: fixed device arguments (pixel size, dpi, white background, one font
# family) and no timestamp or text chunk, so the same plot saved twice gives identical bytes.

SR_PLOT_KINDS <- c("km", "forest", "resid_panel", "schoenfeld", "loglog", "smd", "linearity")

# Okabe-Ito, colour-blind safe; beyond 8 groups the default hue palette.
.sr_pl_palette <- function(n) {
  base <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442", "#000000")
  if (n <= length(base)) base[seq_len(n)] else scales::hue_pal()(n)
}

# The one font family of a figure: the same rule as the flow diagram (Arial on Windows, the generic
# sans elsewhere).
.sr_pl_family <- function() .sr_fl_family()

.sr_pl_theme <- function() {
  ggplot2::theme_bw(base_size = 10, base_family = .sr_pl_family()) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "bottom",
                   strip.background = ggplot2::element_rect(fill = "grey92", colour = "grey60"))
}

# ---- argument checks --------------------------------------------------------------------------------

# `d` as a plain data.frame with the required columns, or stop naming what is missing.
.sr_pl_need <- function(d, cols, kind) {
  if (!is.data.frame(d)) stop("sr_plot: ", kind, " needs a data frame", call. = FALSE)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  miss <- setdiff(cols, names(d))
  if (length(miss)) {
    stop("sr_plot: the ", kind, " data lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  }
  d
}

.sr_pl_fin <- function(x) is.finite(suppressWarnings(as.numeric(x)))

# ---- km -------------------------------------------------------------------------------------------

# The number at risk at the pretty() times, read off a survfit-style km frame: the n.risk of the first
# row whose time is at or after t (0 beyond the last row).
.sr_pl_risk_from_km <- function(km) {
  tt <- pretty(range(km$time), n = 6)
  tt <- tt[tt <= max(km$time)]
  out <- lapply(unique(km$strata), function(s) {
    k <- km[km$strata == s, , drop = FALSE]
    k <- k[order(k$time), , drop = FALSE]
    nr <- vapply(tt, function(t) {
      i <- which(k$time >= t)
      if (length(i)) k$n.risk[i[1L]] else 0
    }, numeric(1))
    data.frame(strata = s, time = tt, n.risk = nr, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

SR_PLOT_RISK_STEP <- 0.14     # spacing of the strip rows, in the units of the (hidden) strip axis
SR_PLOT_RISK_BASE <- 100      # the strip rows sit at 100 + k * step, apart from the 0-1 survival axis

.sr_pl_km_build <- function(data) {
  if (is.data.frame(data)) data <- list(km = data)
  if (!is.list(data) || is.null(data$km)) stop("sr_plot: km needs a data frame or list(km, km_risk)", call. = FALSE)
  km <- .sr_pl_need(data$km, c("strata", "time", "surv"), "km")
  risk <- data$km_risk
  if (is.null(risk)) {
    km <- .sr_pl_need(km, c("strata", "time", "surv", "n.risk"), "km")
    risk <- .sr_pl_risk_from_km(km)
  } else {
    risk <- .sr_pl_need(risk, c("strata", "time", "n.risk"), "km_risk")
  }
  km <- km[.sr_pl_fin(km$time) & .sr_pl_fin(km$surv), , drop = FALSE]
  if (!nrow(km)) stop("sr_plot: km has no finite row", call. = FALSE)
  lv <- unique(as.character(km$strata))
  km$strata <- factor(as.character(km$strata), levels = lv)
  risk$strata <- factor(as.character(risk$strata), levels = lv)
  risk <- risk[!is.na(risk$strata), , drop = FALSE]
  k <- length(lv)

  # every curve starts at (0, 1)
  start <- do.call(rbind, lapply(lv, function(s) {
    ks <- km[km$strata == s, , drop = FALSE]
    if (min(ks$time) > 0) {
      data.frame(strata = factor(s, levels = lv), time = 0, surv = 1, lower = 1, upper = 1)
    } else {
      NULL
    }
  }))
  panel_lv <- c("Survival probability", "Number at risk")
  mk <- function(d) {
    d$panel <- factor(rep(panel_lv[1L], nrow(d)), levels = panel_lv)
    d
  }
  curve <- km[, intersect(c("strata", "time", "surv", "lower", "upper"), names(km)), drop = FALSE]
  for (nm in setdiff(c("lower", "upper"), names(curve))) curve[[nm]] <- NA_real_
  curve <- curve[, c("strata", "time", "surv", "lower", "upper")]
  if (!is.null(start)) curve <- rbind(start, curve)
  curve <- mk(curve[order(curve$strata, curve$time), , drop = FALSE])
  cens <- if ("n.censor" %in% names(km)) km[.sr_pl_fin(km$n.censor) & km$n.censor > 0, , drop = FALSE] else NULL
  cens <- if (!is.null(cens) && nrow(cens)) mk(cens[, c("strata", "time", "surv")]) else NULL

  risk$ypos <- SR_PLOT_RISK_BASE + SR_PLOT_RISK_STEP * (k - as.integer(risk$strata) + 1L)
  risk$label <- sprintf("%.0f", risk$n.risk)
  risk$panel <- factor(rep(panel_lv[2L], nrow(risk)), levels = panel_lv)
  ylim_s <- data.frame(x = 0, y = c(0, 1), panel = factor(panel_lv[1L], levels = panel_lv))
  ylim_r <- data.frame(x = 0, y = SR_PLOT_RISK_BASE + SR_PLOT_RISK_STEP * c(0.4, k + 0.6),
                       panel = factor(panel_lv[2L], levels = panel_lv))
  list(curve = curve, cens = cens, risk = risk, ylim = rbind(ylim_s, ylim_r), lv = lv, k = k)
}

.sr_pl_km <- function(data) {
  b <- .sr_pl_km_build(data)
  fam <- .sr_pl_family()
  pal <- stats::setNames(.sr_pl_palette(b$k), b$lv)
  k <- b$k
  brk <- function(lim) {
    if (lim[1L] >= SR_PLOT_RISK_BASE / 2) SR_PLOT_RISK_BASE + SR_PLOT_RISK_STEP * seq_len(k)
    else c(0, 0.25, 0.5, 0.75, 1)
  }
  lab <- function(x) {
    ifelse(x >= SR_PLOT_RISK_BASE / 2,
           b$lv[pmin(pmax(k - round((x - SR_PLOT_RISK_BASE) / SR_PLOT_RISK_STEP) + 1L, 1L), k)],
           sprintf("%.2f", x))
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_blank(data = b$ylim, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_step(data = b$curve, ggplot2::aes(x = .data$time, y = .data$surv, colour = .data$strata),
                       direction = "hv", linewidth = 0.7)
  if (any(is.finite(b$curve$lower))) {
    p <- p +
      ggplot2::geom_step(data = b$curve, ggplot2::aes(x = .data$time, y = .data$lower, colour = .data$strata),
                         direction = "hv", linewidth = 0.3, linetype = "dotted", alpha = 0.7, na.rm = TRUE) +
      ggplot2::geom_step(data = b$curve, ggplot2::aes(x = .data$time, y = .data$upper, colour = .data$strata),
                         direction = "hv", linewidth = 0.3, linetype = "dotted", alpha = 0.7, na.rm = TRUE)
  }
  if (!is.null(b$cens)) {
    p <- p + ggplot2::geom_point(data = b$cens, ggplot2::aes(x = .data$time, y = .data$surv, colour = .data$strata),
                                 shape = 3, size = 1.2)
  }
  p +
    ggplot2::geom_text(data = b$risk, ggplot2::aes(x = .data$time, y = .data$ypos, label = .data$label,
                                                   colour = .data$strata),
                       family = fam, size = 3, show.legend = FALSE) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$panel), scales = "free_y", space = "free_y") +
    ggplot2::scale_colour_manual(values = pal, breaks = b$lv, name = NULL, drop = FALSE) +
    ggplot2::scale_y_continuous(breaks = brk, labels = lab, expand = ggplot2::expansion(mult = 0.04)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.06))) +
    ggplot2::labs(x = "Time", y = NULL) +
    .sr_pl_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = if (k > 1L) "bottom" else "none")
}

# ---- forest ---------------------------------------------------------------------------------------

.sr_pl_forest <- function(data) {
  d <- .sr_pl_need(data, c("term", "est", "lo", "hi", "scale"), "forest")
  d <- d[.sr_pl_fin(d$est), , drop = FALSE]
  if (!nrow(d)) stop("sr_plot: forest has no row with a finite estimate", call. = FALSE)
  sc <- unique(as.character(d$scale))
  if (!all(sc %in% c("ratio", "difference"))) {
    stop("sr_plot: forest scale must be 'ratio' or 'difference'", call. = FALSE)
  }
  if (length(sc) > 1L) {
    stop("sr_plot: forest has mixed scales (ratio and difference rows); plot one scale at a time", call. = FALSE)
  }
  d$est <- as.numeric(d$est)
  d$lo <- suppressWarnings(as.numeric(d$lo))
  d$hi <- suppressWarnings(as.numeric(d$hi))
  d$lo[!is.finite(d$lo) | !is.finite(d$hi)] <- NA_real_
  d$hi[is.na(d$lo)] <- NA_real_
  ratio <- identical(sc, "ratio")
  if (ratio && any(c(d$est, d$lo, d$hi) <= 0, na.rm = TRUE)) {
    stop("sr_plot: forest ratio estimates and limits must be positive", call. = FALSE)
  }
  n <- nrow(d)
  d$y <- rev(seq_len(n))
  term <- as.character(d$term)
  measure <- if ("measure" %in% names(d)) unique(as.character(d$measure[!is.na(d$measure)])) else character()
  xlab <- paste0(if (length(measure) == 1L) measure else "Effect", " (95% CI)", if (ratio) ", log scale" else "")
  ref <- data.frame(xintercept = if (ratio) 1 else 0)
  p <- ggplot2::ggplot(d, ggplot2::aes(y = .data$y)) +
    ggplot2::geom_vline(data = ref, ggplot2::aes(xintercept = .data$xintercept), linetype = "dashed",
                        colour = "grey40", inherit.aes = FALSE) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$lo, xmax = .data$hi), linewidth = 0.6, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(x = .data$est), size = 2.2, na.rm = TRUE) +
    ggplot2::scale_y_continuous(breaks = d$y, labels = term, expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::labs(x = xlab, y = NULL) +
    .sr_pl_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  if (ratio) {
    p + ggplot2::scale_x_log10(breaks = scales::log_breaks(n = 6, base = 10),
                               labels = function(x) format(x, trim = TRUE, scientific = FALSE, drop0trailing = TRUE))
  } else {
    p + ggplot2::scale_x_continuous()
  }
}

# ---- resid_panel ----------------------------------------------------------------------------------

SR_PLOT_RESID_PANELS <- c("Residuals vs fitted", "Normal Q-Q", "Scale-location", "Residuals vs leverage")

# lowess() of y on x as a data frame (x, y); NULL when it cannot be computed (fewer than 4 distinct x).
.sr_pl_lowess <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(unique(x)) < 4L) return(NULL)
  lw <- tryCatch(stats::lowess(x, y), error = function(e) NULL)
  if (is.null(lw)) NULL else data.frame(x = lw$x, y = lw$y)
}

# The long frame of the four residual panels: panel (factor), x, y, flag (Cook's D > 1).
.sr_pl_resid_long <- function(d) {
  num <- function(v) suppressWarnings(as.numeric(d[[v]]))
  fitted <- num("fitted")
  resid <- num("resid")
  sres <- num("std_resid")
  lev <- num("leverage")
  cooks <- num("cooks")
  flag <- !is.na(cooks) & cooks > 1
  piece <- function(i, x, y, f = rep(FALSE, length(x))) {
    ok <- is.finite(x) & is.finite(y)
    data.frame(panel = rep(SR_PLOT_RESID_PANELS[i], sum(ok)), x = x[ok], y = y[ok], flag = f[ok],
               stringsAsFactors = FALSE)
  }
  ok_q <- is.finite(sres)
  theo <- rep(NA_real_, length(sres))
  if (any(ok_q)) theo[ok_q] <- stats::qnorm(stats::ppoints(sum(ok_q)))[rank(sres[ok_q], ties.method = "first")]
  out <- rbind(piece(1L, fitted, resid), piece(2L, theo, sres), piece(3L, fitted, sqrt(abs(sres))),
               piece(4L, lev, sres, flag))
  out$panel <- factor(out$panel, levels = SR_PLOT_RESID_PANELS)
  rownames(out) <- NULL
  out
}

.sr_pl_resid <- function(data) {
  d <- .sr_pl_need(data, c("fitted", "resid", "std_resid", "leverage", "cooks"), "resid_panel")
  long <- .sr_pl_resid_long(d)
  if (!nrow(long)) stop("sr_plot: resid_panel has no finite row", call. = FALSE)
  pn <- function(i) factor(SR_PLOT_RESID_PANELS[i], levels = SR_PLOT_RESID_PANELS)
  sm <- lapply(c(1L, 3L), function(i) {
    s <- long[long$panel == SR_PLOT_RESID_PANELS[i], , drop = FALSE]
    lw <- .sr_pl_lowess(s$x, s$y)
    if (is.null(lw)) NULL else cbind(panel = pn(i), lw)
  })
  sm <- do.call(rbind, sm)
  zero <- data.frame(panel = factor(SR_PLOT_RESID_PANELS[c(1L, 4L)], levels = SR_PLOT_RESID_PANELS), y = 0)
  q <- long[long$panel == SR_PLOT_RESID_PANELS[2L], , drop = FALSE]
  qq <- NULL
  if (nrow(q) >= 3L) {
    qy <- stats::quantile(q$y, c(0.25, 0.75), names = FALSE)
    qx <- stats::quantile(q$x, c(0.25, 0.75), names = FALSE)
    if (qx[2L] > qx[1L]) {
      slope <- (qy[2L] - qy[1L]) / (qx[2L] - qx[1L])
      qq <- data.frame(panel = pn(2L), slope = slope, intercept = qy[1L] - slope * qx[1L])
    }
  }
  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_hline(data = zero, ggplot2::aes(yintercept = .data$y), colour = "grey60", linetype = "dashed")
  if (!is.null(qq)) {
    p <- p + ggplot2::geom_abline(data = qq, ggplot2::aes(slope = .data$slope, intercept = .data$intercept),
                                  colour = "grey40", linetype = "dashed")
  }
  p <- p + ggplot2::geom_point(ggplot2::aes(colour = .data$flag), size = 1.2, alpha = 0.7)
  if (!is.null(sm) && nrow(sm)) {
    p <- p + ggplot2::geom_line(data = sm, ggplot2::aes(x = .data$x, y = .data$y), colour = "#D55E00",
                                linewidth = 0.7, inherit.aes = FALSE)
  }
  p +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel), ncol = 2L, scales = "free") +
    ggplot2::scale_colour_manual(values = c("FALSE" = "grey25", "TRUE" = "#D55E00"), guide = "none") +
    ggplot2::labs(x = NULL, y = NULL) +
    .sr_pl_theme()
}

# ---- schoenfeld -----------------------------------------------------------------------------------

.sr_pl_schoenfeld <- function(data) {
  d <- .sr_pl_need(data, c("term", "time", "resid"), "schoenfeld")
  d <- d[.sr_pl_fin(d$time) & .sr_pl_fin(d$resid), c("term", "time", "resid"), drop = FALSE]
  if (!nrow(d)) stop("sr_plot: schoenfeld has no row with a finite time and residual", call. = FALSE)
  d$time <- as.numeric(d$time)
  d$resid <- as.numeric(d$resid)
  lv <- unique(as.character(d$term))
  d$term <- factor(as.character(d$term), levels = lv)
  sm <- do.call(rbind, lapply(lv, function(t) {
    s <- d[d$term == t, , drop = FALSE]
    lw <- .sr_pl_lowess(s$time, s$resid)
    if (is.null(lw)) NULL else data.frame(term = factor(t, levels = lv), time = lw$x, resid = lw$y)
  }))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$time, y = .data$resid)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed") +
    ggplot2::geom_point(size = 1.2, alpha = 0.6, colour = "grey25")
  if (!is.null(sm) && nrow(sm)) {
    p <- p + ggplot2::geom_line(data = sm, colour = "#D55E00", linewidth = 0.7)
  }
  p +
    ggplot2::facet_wrap(ggplot2::vars(.data$term), scales = "free_y") +
    ggplot2::labs(x = "Time", y = "Scaled Schoenfeld residual") +
    .sr_pl_theme()
}

# ---- loglog ---------------------------------------------------------------------------------------

.sr_pl_loglog <- function(data) {
  if (is.list(data) && !is.data.frame(data) && !is.null(data$km)) data <- data$km
  d <- .sr_pl_need(data, c("strata", "time", "surv"), "loglog")
  time <- suppressWarnings(as.numeric(d$time))
  surv <- suppressWarnings(as.numeric(d$surv))
  ok <- is.finite(time) & is.finite(surv) & time > 0 & surv > 0 & surv < 1
  if (!any(ok)) stop("sr_plot: loglog has no row with time > 0 and 0 < surv < 1", call. = FALSE)
  lv <- unique(as.character(d$strata[ok]))
  df <- data.frame(strata = factor(as.character(d$strata[ok]), levels = lv),
                   x = log(time[ok]), y = log(-log(surv[ok])))
  df <- df[order(df$strata, df$x), , drop = FALSE]
  ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$strata)) +
    ggplot2::geom_step(direction = "hv", linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = stats::setNames(.sr_pl_palette(length(lv)), lv), name = NULL) +
    ggplot2::labs(x = "log(time)", y = "log(-log(survival))") +
    .sr_pl_theme() +
    ggplot2::theme(legend.position = if (length(lv) > 1L) "bottom" else "none")
}

# ---- smd ------------------------------------------------------------------------------------------

.sr_pl_smd <- function(data) {
  d <- .sr_pl_need(data, c("term", "est"), "smd")
  if ("measure" %in% names(d)) d <- d[!is.na(d$measure) & toupper(as.character(d$measure)) == "SMD", , drop = FALSE]
  d <- d[.sr_pl_fin(d$est), , drop = FALSE]
  if (!nrow(d)) stop("sr_plot: smd has no row with a finite SMD", call. = FALSE)
  d$est <- as.numeric(d$est)
  d$lo <- if ("lo" %in% names(d)) suppressWarnings(as.numeric(d$lo)) else NA_real_
  d$hi <- if ("hi" %in% names(d)) suppressWarnings(as.numeric(d$hi)) else NA_real_
  d$lo[!is.finite(d$lo) | !is.finite(d$hi)] <- NA_real_
  d$hi[is.na(d$lo)] <- NA_real_
  d <- d[order(abs(d$est)), , drop = FALSE]
  d$y <- seq_len(nrow(d))
  d$term <- as.character(d$term)
  d$imbalance <- factor(ifelse(abs(d$est) > 0.1, "|SMD| > 0.1", "|SMD| <= 0.1"),
                        levels = c("|SMD| <= 0.1", "|SMD| > 0.1"))
  rownames(d) <- NULL
  ggplot2::ggplot(d, ggplot2::aes(y = .data$y)) +
    ggplot2::geom_vline(data = data.frame(xintercept = 0), ggplot2::aes(xintercept = .data$xintercept),
                        colour = "grey40", inherit.aes = FALSE) +
    ggplot2::geom_vline(data = data.frame(xintercept = c(-0.1, 0.1)), ggplot2::aes(xintercept = .data$xintercept),
                        colour = "grey40", linetype = "dashed", inherit.aes = FALSE) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$lo, xmax = .data$hi), colour = "grey50", na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(x = .data$est, colour = .data$imbalance), size = 2.4) +
    ggplot2::scale_colour_manual(values = c("|SMD| <= 0.1" = "#0072B2", "|SMD| > 0.1" = "#D55E00"),
                                 name = NULL, drop = FALSE) +
    ggplot2::scale_y_continuous(breaks = d$y, labels = d$term, expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::labs(x = "Standardised mean difference", y = NULL) +
    .sr_pl_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

# ---- linearity ------------------------------------------------------------------------------------

.sr_pl_linearity <- function(data) {
  d <- .sr_pl_need(data, c("var", "x", "linear", "rcs"), "linearity")
  one <- function(col, label) {
    ok <- .sr_pl_fin(d$x) & .sr_pl_fin(d[[col]])
    data.frame(var = as.character(d$var[ok]), x = as.numeric(d$x[ok]), y = as.numeric(d[[col]][ok]),
               curve = rep(label, sum(ok)), stringsAsFactors = FALSE)
  }
  long <- rbind(one("linear", "Linear"), one("rcs", "RCS"))
  if (!nrow(long)) stop("sr_plot: linearity has no finite row", call. = FALSE)
  long$var <- factor(long$var, levels = unique(as.character(d$var)))
  long$curve <- factor(long$curve, levels = c("Linear", "RCS"))
  long <- long[order(long$var, long$curve, long$x), , drop = FALSE]
  rownames(long) <- NULL
  ggplot2::ggplot(long, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$curve, linetype = .data$curve)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(ggplot2::vars(.data$var), scales = "free") +
    ggplot2::scale_colour_manual(values = c(Linear = "#0072B2", RCS = "#D55E00"), name = NULL) +
    ggplot2::scale_linetype_manual(values = c(Linear = "dashed", RCS = "solid"), name = NULL) +
    ggplot2::labs(x = NULL, y = "Fitted partial effect") +
    .sr_pl_theme()
}

# ---- public ---------------------------------------------------------------------------------------

#' Build the figure of one kind from the data frame(s) the routes and diagnostics emit.
#' @param data the frame the kind reads (see the header); `km` also takes list(km, km_risk).
#' @param kind one of SR_PLOT_KINDS.
#' @return a ggplot.
sr_plot <- function(data, kind) {
  if (!is.character(kind) || length(kind) != 1L || is.na(kind) || !(kind %in% SR_PLOT_KINDS)) {
    stop("sr_plot: kind must be one of ", paste(SR_PLOT_KINDS, collapse = ", "), call. = FALSE)
  }
  switch(kind,
         km = .sr_pl_km(data), forest = .sr_pl_forest(data), resid_panel = .sr_pl_resid(data),
         schoenfeld = .sr_pl_schoenfeld(data), loglog = .sr_pl_loglog(data), smd = .sr_pl_smd(data),
         linearity = .sr_pl_linearity(data))
}

#' Save a plot as a PNG, deterministically.
#'
#' Rendered with ragg::agg_png at the pixel size round(mm / 25.4 * dpi) on a white background in the
#' one figure font family; the PNG carries no timestamp or text chunk, so the same plot saved twice
#' gives identical bytes. The file is written atomically (the previous file stays until the new one is
#' complete) and the parent folder is created.
#' @param plot a ggplot.
#' @param path a .png file name.
#' @param width_mm,height_mm size in millimetres (default 170 x 120).
#' @param dpi resolution (default 300).
#' @return list(path, sha): sha is the SHA-256 (lowercase hex) of the written bytes.
sr_figure_save <- function(plot, path, width_mm = 170, height_mm = 120, dpi = 300) {
  if (!inherits(plot, "ggplot")) stop("sr_figure_save: plot must be a ggplot", call. = FALSE)
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("sr_figure_save: path must be one file name", call. = FALSE)
  }
  if (!grepl("\\.png$", path, ignore.case = TRUE)) stop("sr_figure_save: path must end in .png", call. = FALSE)
  pos <- function(x, nm) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
      stop("sr_figure_save: ", nm, " must be one positive number", call. = FALSE)
    }
    x
  }
  width_mm <- pos(width_mm, "width_mm")
  height_mm <- pos(height_mm, "height_mm")
  dpi <- pos(dpi, "dpi")
  w <- max(1L, as.integer(round(width_mm / 25.4 * dpi)))
  h <- max(1L, as.integer(round(height_mm / 25.4 * dpi)))

  scratch <- tempfile(fileext = ".png")
  on.exit(unlink(scratch), add = TRUE)
  ragg::agg_png(scratch, width = w, height = h, units = "px", res = dpi, background = "white")
  dev <- unname(grDevices::dev.cur())
  on.exit(if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev), add = TRUE)
  print(plot)
  grDevices::dev.off(dev)
  bytes <- .sr_read_bytes(scratch)
  if (length(bytes) < 8L || !identical(bytes[1:4], as.raw(c(0x89, 0x50, 0x4E, 0x47)))) {
    stop("sr_figure_save: the device wrote no PNG", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  .sr_write_atomic(path, bytes)
  list(path = path, sha = .sr_sha256(bytes))
}
