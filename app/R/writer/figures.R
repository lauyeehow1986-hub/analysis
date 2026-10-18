# writer/figures.R - the figure specs of the manuscript and their PNG rendering. A figure block is
#   list(type = "figure", id, caption, kind = "km" | "forest" | "flow", data, where, width_mm, height_mm)
# `data` is what sr_plot() (km, forest) or sr_flow_plot() (flow) reads. width_mm / height_mm are NA
# unless the figure sets its own size (a forest plot grows with its rows); the journal preset fills the
# rest at render time. A figure number is never part of the block: the SEQ field of the docx numbers it.
# Captions are templates: data-derived names (analysis id, exposure) are L tokens, no digit appears.
# The helpers .sr_wr_label / .sr_wr_opts and the cache switch are in writer/tables.R.

# ---- the blocks -------------------------------------------------------------------------------------

.sr_wr_fig_block <- function(id, caption, kind, data, width_mm = NA_real_, height_mm = NA_real_) {
  list(type = "figure", id = id, caption = caption, kind = kind, data = data, where = "main",
       width_mm = as.numeric(width_mm), height_mm = as.numeric(height_mm))
}

# One Kaplan-Meier block per analysis whose result has figures$km. The number-at-risk strip carries the
# suppressed labels ("<5" for a count of 1 to 4).
.sr_wr_fig_km <- function(project, ids, reg, k) {
  out <- list()
  for (id in ids) {
    figs <- .sr_ph_result(project, id)$figures
    km <- figs$km
    if (!is.data.frame(km) || !nrow(km)) next
    risk <- figs$km_risk
    if (!is.data.frame(risk)) risk <- .sr_pl_risk_from_km(km)
    expo <- project$state$analyses[[id]]$spec$exposure
    by <- if (is.character(expo) && length(expo) == 1L && !is.na(expo) && nzchar(expo)) {
      paste0(" by ", .sr_wr_label(reg, expo))
    } else {
      ""
    }
    cap <- sprintf("Kaplan-Meier curves%s (%s)", by, .sr_wr_label(reg, id))
    out[[length(out) + 1L]] <- .sr_wr_fig_block(paste0("km_", id), cap, "km",
                                                list(km = km, km_risk = sr_sdc_km_risk(risk, k)))
  }
  out
}

# One forest block per scale: the primary rows with a finite interval, named "<analysis>: <term>".
.sr_wr_fig_forest <- function(project, ids, reg) {
  rows <- list()
  for (id in ids) {
    e <- .sr_ph_result(project, id)$effects
    e <- e[e$role == "primary" & is.finite(e$est) & is.finite(e$lo) & is.finite(e$hi) &
             e$scale %in% c("ratio", "difference"), , drop = FALSE]
    e <- e[e$scale != "ratio" | (e$est > 0 & e$lo > 0 & e$hi > 0), , drop = FALSE]
    if (!nrow(e)) next
    rows[[length(rows) + 1L]] <- data.frame(term = paste0(id, ": ", e$term), measure = as.character(e$measure),
                                            est = e$est, lo = e$lo, hi = e$hi, scale = as.character(e$scale),
                                            stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(list())
  d <- do.call(rbind, rows)
  out <- list()
  for (sc in c("ratio", "difference")) {
    ds <- d[d$scale == sc, , drop = FALSE]
    if (!nrow(ds)) next
    rownames(ds) <- NULL
    out[[length(out) + 1L]] <- .sr_wr_fig_block(
      paste0("forest_", sc), sprintf("Primary and secondary estimates (%s scale)", sc), "forest", ds,
      height_mm = max(60, 9 * nrow(ds) + 35))
  }
  out
}

# The participant flow of the table of the primary analyses (all of `ids` when none is primary), when it
# has at least one step.
.sr_wr_fig_flow <- function(project, ids, reg) {
  lab <- vapply(ids, function(id) as.character(.sr_wr_analysis(project, id)$label %||% NA_character_), "")
  use <- ids[!is.na(lab) & lab == "primary"]
  if (!length(use)) use <- ids
  tabs <- unique(vapply(use, function(id) as.character(.sr_wr_analysis(project, id)$table), ""))
  flows <- list()
  for (tab in tabs) {
    fl <- sr_flow(project, tab)
    if (nrow(fl) >= 1L) flows[[tab]] <- fl
  }
  out <- list()
  for (tab in names(flows)) {
    one <- length(flows) == 1L
    cap <- if (one) "Flow of rows from import to analysis" else {
      sprintf("Flow of rows from import to analysis (%s)", .sr_wr_label(reg, tab))
    }
    out[[length(out) + 1L]] <- .sr_wr_fig_block(if (one) "flow" else paste0("flow_", tab), cap, "flow", flows[[tab]])
  }
  out
}

#' The figure blocks of the main document: Kaplan-Meier curves, forest plots and the flow diagram.
#'
#' @param ids the analyses of the main document.
#' @param opts plain list: table2, suppress_k (the strip of a Kaplan-Meier plot hides counts of 1 to
#'   suppress_k - 1).
#' @return a list of figure blocks.
.sr_wr_figures <- function(project, ids, reg, opts) {
  opts <- .sr_wr_opts(opts)
  if (.sr_wr_cache_on()) on.exit(.sr_ph_ctx$cache <- NULL, add = TRUE)
  for (id in ids) .sr_wr_analysis(project, id)
  c(.sr_wr_fig_km(project, ids, reg, opts$suppress_k), .sr_wr_fig_forest(project, ids, reg),
    .sr_wr_fig_flow(project, ids, reg))
}

# ---- rendering --------------------------------------------------------------------------------------

#' Render one figure block to <dir>/<block id>.png.
#'
#' km and forest go through sr_plot() and sr_figure_save() at the preset's width and dpi (a block's own
#' width_mm / height_mm win); the flow goes through sr_flow_plot() at the preset's width.
#' @param block a figure block.
#' @param dir the folder of the PNG (created).
#' @param preset plain list: fig_width_mm, fig_height_mm, dpi.
#' @return list(path, sha): sha is the SHA-256 of the PNG bytes.
sr_wr_figure_render <- function(block, dir, preset) {
  if (!is.list(block) || !identical(block$type, "figure") || !(block$kind %in% c("km", "forest", "flow"))) {
    stop("`block` must be a figure block (km, forest or flow)", call. = FALSE)
  }
  pos <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0
  if (!is.list(preset) || !all(vapply(preset[c("fig_width_mm", "fig_height_mm", "dpi")], pos, NA))) {
    stop("`preset` must hold positive numbers fig_width_mm, fig_height_mm and dpi", call. = FALSE)
  }
  path <- file.path(dir, paste0(block$id, ".png"))
  w <- if (pos(block$width_mm)) block$width_mm else preset$fig_width_mm
  h <- if (pos(block$height_mm)) block$height_mm else preset$fig_height_mm
  if (identical(block$kind, "flow")) {
    sr_flow_plot(block$data, path, width = w / 25.4)
    return(list(path = path, sha = .sr_sha256(.sr_read_bytes(path))))
  }
  r <- sr_figure_save(sr_plot(block$data, block$kind), path, width_mm = w, height_mm = h, dpi = preset$dpi)
  list(path = r$path, sha = r$sha)
}
