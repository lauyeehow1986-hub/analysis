# writer/tables.R - the tables of the manuscript: the main-results table, the route tables, the
# multiplicity-family table and the deviation table. A table block is
#   list(type = "table", id, caption, df, footnotes, analysis, where = "main" | "supplement", n_suppressed)
# caption and footnotes are templates; `df` cells are text and may hold {{E}} / {{L}} tokens.
#
# What is a token and what is not (rulings R1b and R6):
#   - the main-results table, the family table and every caption and footnote are built from tokens;
#     a number of the main table is an `effect:` or `family:` source, so it is bookmarked and verified;
#   - the cells of a route table stay as the evaluated text the route produced (small cells hidden);
#   - data-derived names in a caption or footnote (analysis id, family id, method) are L tokens, and so
#     is every fixed constant that holds a digit ("95%", the 1 of "Table 1"): no template holds a digit.
#
# Sources registered here (resolvers at the end of the file):
#   family:<fid>:<row>:p | p_adj      row <row> of sr_family_adjust(project, fid)
#   deviation:<row>:<k>               the k-th number of the reason of row <row> of sr_deviations()

SR_WR_MEASURE_NAMES <- c(
  MD = "Mean difference", OR = "Odds ratio", RR = "Risk ratio", RD = "Risk difference",
  HR = "Hazard ratio", RMST = "Restricted mean survival time",
  RMST_diff = "Difference in restricted mean survival time", NNT = "Number needed to treat",
  SMD = "Standardised mean difference", coef = "Coefficient", risk = "Risk")

SR_WR_ROUTE_NAMES <- c(
  table1 = "Table 1", lm = "Linear regression", lm_describe = "Linear regression",
  logistic = "Logistic regression", logistic_describe = "Logistic regression",
  modpois = "Modified Poisson regression", cox = "Cox regression",
  rmst = "Restricted mean survival time")

SR_WR_NONCOLLAPSIBLE_NOTE <- paste0(
  "Odds and hazard ratios are conditional on the covariates of their model and are not collapsible; ",
  "marginal estimates are shown where reported.")

# ---- small helpers ----------------------------------------------------------------------------------

# The options of the tables as a plain list: table2 "label" | "suppress", suppress_k.
.sr_wr_opts <- function(opts) {
  if (!is.list(opts)) stop("`opts` must be a list (table2, suppress_k)", call. = FALSE)
  t2 <- opts$table2 %||% "label"
  if (!(is.character(t2) && length(t2) == 1L && !is.na(t2) && t2 %in% c("label", "suppress"))) {
    stop("opts$table2 must be \"label\" or \"suppress\"", call. = FALSE)
  }
  k <- opts$suppress_k %||% SR_SDC_K
  if (!(is.numeric(k) && length(k) == 1L && is.finite(k) && k >= 2 && k == round(k))) {
    stop("opts$suppress_k must be a whole number of at least 2", call. = FALSE)
  }
  list(table2 = t2, suppress_k = as.integer(k))
}

# An L token for a data-derived name or a fixed constant (source label:<text>).
.sr_wr_label <- function(reg, text) {
  sr_ph_put(reg, as.character(text), "label", paste0("label:", text))
}

# `text` with every number (SR_NUM_RE) replaced by an L token, so that a template holds no digit.
.sr_wr_safe <- function(reg, text) {
  m <- gregexpr(SR_NUM_RE, text, perl = TRUE)
  hit <- regmatches(text, m)[[1L]]
  if (!length(hit)) return(text)
  regmatches(text, m) <- list(vapply(hit, function(h) .sr_wr_label(reg, h), "", USE.NAMES = FALSE))
  text
}

.sr_wr_route_name <- function(route) {
  nm <- unname(SR_WR_ROUTE_NAMES[as.character(route)])
  if (length(nm) != 1L || is.na(nm)) as.character(route) else nm
}

# The analysis `id` of the project, or an error.
.sr_wr_analysis <- function(project, id) {
  a <- project$state$analyses[[id]]
  if (is.null(a)) stop("unknown analysis '", id, "'", call. = FALSE)
  a
}

# Within one builder, sr_result() of an analysis is read once (the cache sr_ph_verify() uses). Returns TRUE
# when it switched the cache on; the caller then switches it off on exit:
#   if (.sr_wr_cache_on()) on.exit(.sr_ph_ctx$cache <- NULL, add = TRUE)
.sr_wr_cache_on <- function() {
  if (!is.null(.sr_ph_ctx$cache)) return(invisible(FALSE))
  .sr_ph_ctx$cache <- list()
  invisible(TRUE)
}

# The text of one effects field, derived the way sr_ph_verify() derives it.
.sr_wr_eff_text <- function(project, id, i, field) {
  .sr_ph_resolve_effect(project, c(id, as.character(i), field), NA_character_)
}

.sr_wr_null_of <- function(scale) {
  if (is.na(scale)) NA_real_ else if (identical(scale, "ratio")) 1 else 0
}

.sr_wr_block <- function(id, caption, df, footnotes, analysis, where, n_suppressed) {
  rownames(df) <- NULL
  list(type = "table", id = id, caption = caption, df = df, footnotes = as.character(footnotes),
       analysis = analysis, where = where, n_suppressed = as.integer(n_suppressed))
}

# ---- small-cell suppression of a text table ---------------------------------------------------------

# The columns the row rule of sr_sdc_table() must ignore: the Overall column (named "Overall (n=...)" in a
# Table 1), the Missing column (a count of its own, not a group) and the SMD column.
.sr_wr_skip_cols <- function(nms) {
  nms[grepl("^Overall\\b", nms) | grepl("^Missing\\b", nms) | grepl("^SMD\\b", nms)]
}

# The group size of a header ("A (n=3)") is a count of the same cells: a size of 1 to k - 1 prints as "<k".
.sr_wr_sdc <- function(df, k) {
  out <- sr_sdc_table(df, k = k, skip_cols = .sr_wr_skip_cols(names(df)))
  mask <- function(s) {
    m <- gregexpr("[(]n=[0-9]+[)]", s)
    hit <- regmatches(s, m)[[1L]]
    if (!length(hit)) return(s)
    n <- as.integer(gsub("[^0-9]", "", hit))
    regmatches(s, m) <- list(ifelse(n >= 1L & n <= k - 1L, paste0("(n=", .sr_sdc_text(k), ")"), hit))
    s
  }
  names(out$table) <- vapply(names(out$table), mask, "", USE.NAMES = FALSE)
  out
}

# ---- the main-results table -------------------------------------------------------------------------

#' The "Main results" table: one row per primary-role effects row of `ids`.
#'
#' Columns Analysis, Comparison, Measure, Estimate (95% CI), p, and Adjusted p when an analysis of `ids`
#' is in a multiplicity family. The estimate cell is "{{E est}} ({{E lo_hi}})", p and adjusted p are E
#' tokens (a missing p, or a row outside a family, prints "-").
#' @param opts plain list: table2, suppress_k.
#' @return a table block, where "main".
# Contract: `ids` are analyses that have run (an unrun id errors in sr_result); the caller filters them.
.sr_wr_main_table <- function(project, ids, reg, opts) {
  opts <- .sr_wr_opts(opts)
  if (.sr_wr_cache_on()) on.exit(.sr_ph_ctx$cache <- NULL, add = TRUE)
  fams <- names(project$state$sap$families %||% list())
  family_of <- function(a) {
    f <- a$family
    if (is.null(f) || length(f) != 1L || is.na(f) || !(as.character(f) %in% fams)) NA_character_ else as.character(f)
  }
  an <- lapply(ids, function(id) .sr_wr_analysis(project, id))
  names(an) <- ids
  any_family <- any(!is.na(vapply(an, family_of, "")))

  fa_cache <- list()
  cells <- list()
  noncoll <- FALSE
  fam_used <- character()
  boot_methods <- character()
  for (id in ids) {
    res <- .sr_ph_result(project, id)
    eff <- res$effects
    idx <- which(eff$role == "primary")
    if (!length(idx)) next
    fid <- family_of(an[[id]])
    fa <- NULL
    if (!is.na(fid)) {
      if (is.null(fa_cache[[fid]])) fa_cache[[fid]] <- sr_family_adjust(project, fid)
      fa <- fa_cache[[fid]]
    }
    flagged <- res$flags$term[res$flags$flag == "non_collapsible"]
    first_measure <- eff$measure[idx[1L]]
    for (i in idx) {
      scale <- eff$scale[i]
      null <- .sr_wr_null_of(scale)
      est <- sr_ph_put(reg, .sr_wr_eff_text(project, id, i, "est"), "est", sprintf("effect:%s:%d:est", id, i),
                       analysis = id, value = eff$est[i], scale = scale, null = null)
      ci <- sr_ph_put(reg, .sr_wr_eff_text(project, id, i, "lo_hi"), "ci", sprintf("effect:%s:%d:lo_hi", id, i),
                      analysis = id)
      p <- if (is.na(eff$p[i])) {
        "-"
      } else {
        sr_ph_put(reg, .sr_wr_eff_text(project, id, i, "p"), "p", sprintf("effect:%s:%d:p", id, i),
                  analysis = id, value = eff$p[i])
      }
      padj <- "-"
      if (!is.null(fa) && identical(eff$measure[i], first_measure)) {
        r <- which(fa$analysis == id & fa$term == eff$term[i])
        if (length(r) && !is.na(fa$p_adj[r[1L]])) {
          r <- r[1L]
          padj <- sr_ph_put(reg, sr_fmt_p(fa$p_adj[r]), "p", sprintf("family:%s:%d:p_adj", fid, r),
                            analysis = id, value = fa$p_adj[r])
          if (!(fid %in% fam_used)) fam_used <- c(fam_used, fid)
        }
      }
      if (eff$measure[i] %in% c("OR", "HR") && isTRUE(eff$conditional[i]) && eff$term[i] %in% flagged) {
        noncoll <- TRUE
      }
      cm <- as.character(eff$ci_method[i])
      if (!is.na(cm) && startsWith(cm, "boot_") && !(cm %in% boot_methods)) boot_methods <- c(boot_methods, cm)
      mn <- unname(SR_WR_MEASURE_NAMES[as.character(eff$measure[i])])
      cells[[length(cells) + 1L]] <- list(
        analysis = .sr_wr_safe(reg, id), comparison = .sr_wr_safe(reg, as.character(eff$term[i])),
        measure = if (is.na(mn)) .sr_wr_safe(reg, as.character(eff$measure[i])) else mn,
        estimate = paste0(est, " (", ci, ")"), p = p, padj = padj)
    }
  }

  est_col <- paste0("Estimate (", .sr_wr_label(reg, "95%"), " CI)")
  col <- function(k) vapply(cells, function(r) r[[k]], "", USE.NAMES = FALSE)
  df <- data.frame(Analysis = col("analysis"), Comparison = col("comparison"), Measure = col("measure"),
                   stringsAsFactors = FALSE)
  df[[est_col]] <- col("estimate")
  df$p <- col("p")
  if (any_family) df[["Adjusted p"]] <- col("padj")

  fn <- character()
  if (noncoll) fn <- c(fn, SR_WR_NONCOLLAPSIBLE_NOTE)
  for (fid in fam_used) {
    fn <- c(fn, sprintf("Adjusted p: %s over family %s.", .sr_wr_label(reg, fa_cache[[fid]]$method[1L]),
                        .sr_wr_label(reg, fid)))
  }
  for (m in boot_methods) fn <- c(fn, sprintf("Bootstrap interval (%s).", .sr_wr_label(reg, m)))

  .sr_wr_block("main_results", "Main results", df, fn, NA_character_, "main", 0L)
}

# ---- the route tables -------------------------------------------------------------------------------

#' The tables of one analysis: every element of `sr_result()$tables`, small cells hidden.
#'
#' `table1$table1` goes to the main document, every other table to the supplement. The caption is
#' "<Route name> {{L id}}: <table name>". On an explain-causal route whose result has table2 flags,
#' a table with a row for a flagged term gets the Table 2 footnote (opts$table2 "label") or loses
#' those rows (opts$table2 "suppress").
#' @return a list of table blocks.
# Contract: `id` is an analysis that has run (an unrun id errors in sr_result); the caller filters it.
.sr_wr_route_tables <- function(project, id, reg, opts) {
  opts <- .sr_wr_opts(opts)
  if (.sr_wr_cache_on()) on.exit(.sr_ph_ctx$cache <- NULL, add = TRUE)
  a <- .sr_wr_analysis(project, id)
  res <- .sr_ph_result(project, id)
  route <- as.character(a$route)
  rname <- .sr_wr_safe(reg, .sr_wr_route_name(route))
  flagged <- as.character(res$flags$term[res$flags$flag == "table2"])
  out <- list()
  for (nm in names(res$tables)) {
    df <- res$tables[[nm]]
    if (!is.data.frame(df)) next
    fn <- character()
    if (length(flagged) && nrow(df) && ncol(df)) {
      hit <- as.character(df[[1L]]) %in% flagged
      if (any(hit)) {
        fallacy <- .sr_wr_safe(reg, "Table 2 fallacy")
        if (identical(opts$table2, "suppress")) {
          df <- df[!hit, , drop = FALSE]
          fn <- paste0("Covariate coefficients are not shown (", fallacy, ") {{C:westreich2013}}.")
        } else {
          fn <- paste0("Covariate coefficients are adjusted for different sets of variables and are not ",
                       "estimates of their effects (", fallacy, ") {{C:westreich2013}}.")
        }
      }
    }
    sd <- .sr_wr_sdc(df, opts$suppress_k)
    caption <- sprintf("%s %s: %s", rname, .sr_wr_label(reg, id), .sr_wr_safe(reg, gsub("_", " ", nm, fixed = TRUE)))
    main <- identical(route, "table1") && identical(nm, "table1")
    out[[length(out) + 1L]] <- .sr_wr_block(paste0(id, "_", nm), caption, sd$table, fn, id,
                                           if (main) "main" else "supplement", sd$n_suppressed)
  }
  out
}

# ---- the family table -------------------------------------------------------------------------------

#' The supplement table of the multiplicity families: one row per sr_family_adjust() row.
#' @return a table block, or NULL when the SAP has no family.
# Contract: the families name analyses that have run (an unrun id errors in sr_result); the caller filters them.
.sr_wr_family_table <- function(project, reg) {
  fams <- names(project$state$sap$families %||% list())
  if (!length(fams)) return(NULL)
  rows <- list()
  for (fid in fams) {
    fa <- sr_family_adjust(project, fid)
    for (r in seq_len(nrow(fa))) {
      one <- function(x, field) {
        if (is.na(x)) return("-")
        sr_ph_put(reg, sr_fmt_p(x), "p", sprintf("family:%s:%d:%s", fid, r, field), analysis = fa$analysis[r], value = x)
      }
      rows[[length(rows) + 1L]] <- c(
        .sr_wr_label(reg, fid), .sr_wr_safe(reg, fa$analysis[r]), .sr_wr_safe(reg, fa$term[r]), one(fa$p[r], "p"), one(fa$p_adj[r], "p_adj"),
        .sr_wr_label(reg, fa$method[r]))
    }
  }
  m <- do.call(rbind, rows)
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  names(df) <- c("Family", "Analysis", "Comparison", "p", "Adjusted p", "Method")
  .sr_wr_block("families", "Multiplicity-adjusted p values by family", df, character(), NA_character_,
               "supplement", 0L)
}

# ---- the deviation table ----------------------------------------------------------------------------

#' The supplement table of sr_deviations(): the numbers of a reason are lifted into tokens.
#' @return a table block, or NULL when the project has no deviation.
.sr_wr_deviation_table <- function(project, reg) {
  dv <- sr_deviations(project)
  if (!nrow(dv)) return(NULL)
  txt <- function(x) {
    x <- as.character(x)
    ifelse(is.na(x), "-", vapply(x, function(s) if (is.na(s)) "-" else .sr_wr_safe(reg, s), "", USE.NAMES = FALSE))
  }
  reason <- vapply(seq_len(nrow(dv)), function(r) {
    s <- dv$reason[r]
    if (is.na(s) || !nzchar(s)) "-" else sr_ph_lift(s, reg, sprintf("deviation:%d", r), dv$analysis[r])
  }, "")
  df <- data.frame(Analysis = vapply(as.character(dv$analysis), function(a) .sr_wr_safe(reg, a), "", USE.NAMES = FALSE), Label = txt(dv$label), Kind = txt(dv$kind),
                   Reason = reason, Trigger = txt(dv$trigger), stringsAsFactors = FALSE)
  .sr_wr_block("deviations", "Deviations from the locked analysis plan", df, character(), NA_character_,
               "supplement", 0L)
}

# ---- resolvers --------------------------------------------------------------------------------------

.sr_ph_resolve_family <- function(project, parts, kind) {
  if (length(parts) != 3L) stop("a family source is family:<fid>:<row>:<field>", call. = FALSE)
  fa <- sr_family_adjust(project, parts[1L])
  i <- .sr_ph_int_part(parts[2L], "the family row")
  if (i > nrow(fa)) stop("family '", parts[1L], "' has no row ", i, call. = FALSE)
  switch(parts[3L],
    p = sr_fmt_p(fa$p[i]),
    p_adj = sr_fmt_p(fa$p_adj[i]),
    stop("unknown family field '", parts[3L], "'", call. = FALSE))
}

.sr_ph_resolve_deviation <- function(project, parts, kind) {
  if (length(parts) != 2L) stop("a deviation source is deviation:<row>:<k>", call. = FALSE)
  dv <- sr_deviations(project)
  r <- .sr_ph_int_part(parts[1L], "the deviation row")
  if (r > nrow(dv)) stop("there is no deviation row ", r, call. = FALSE)
  k <- .sr_ph_int_part(parts[2L], "the number position")
  txt <- .sr_ph_num_matches(as.character(dv$reason[r]))$txt
  if (k > length(txt)) stop("the reason of deviation ", r, " has no number ", k, call. = FALSE)
  txt[k]
}

.sr_ph_register_resolver("family", .sr_ph_resolve_family)
.sr_ph_register_resolver("deviation", .sr_ph_resolve_deviation)
