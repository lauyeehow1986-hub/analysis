# writer/model.R - the document model: every block of the manuscript, in order, built from a locked and
# run project. The model holds no number of its own: each number is a placeholder of `reg`, and a
# paragraph is a template over those placeholders (writer/fragments.R). The docx renderer, the claim
# guard and the reporting checklist all read this one model.
#
#   sr_writer_opts()  the validated options of a build
#   sr_doc_model()    list(meta, reg, main, supplement, not_reported, dropped_families)

#' The options of a manuscript build, validated.
#'
#' @param preset the journal preset name (Task 8 resolves it to a size and a resolution).
#' @param suppress_k the small-cell threshold of the whole document (ruling R15): prose, tables and figures hide
#'   a count of 1 to suppress_k - 1. A whole number of at least SR_SDC_K, the floor: a looser threshold is refused.
#' @param table2 "label" keeps the covariate coefficients of an explain-causal model under a Table 2
#'   footnote, "suppress" drops them.
#' @param alpha the significance level, strictly between 0 and 1.
#' @return list(preset, suppress_k, table2, alpha).
sr_writer_opts <- function(preset = "default", suppress_k = SR_SDC_K, table2 = c("label", "suppress"),
                           alpha = 0.05) {
  if (!(is.character(preset) && length(preset) == 1L && !is.na(preset) && nzchar(preset))) {
    stop("`preset` must be a single non-empty string", call. = FALSE)
  }
  if (!(is.numeric(suppress_k) && length(suppress_k) == 1L && is.finite(suppress_k) && suppress_k >= SR_SDC_K &&
        suppress_k == round(suppress_k))) {
    stop("`suppress_k` must be a whole number of at least ", SR_SDC_K, " (the small-cell floor SR_SDC_K)", call. = FALSE)
  }
  if (identical(table2, c("label", "suppress"))) table2 <- "label"
  if (!(is.character(table2) && length(table2) == 1L && !is.na(table2) && table2 %in% c("label", "suppress"))) {
    stop("`table2` must be \"label\" or \"suppress\"", call. = FALSE)
  }
  if (!(is.numeric(alpha) && length(alpha) == 1L && is.finite(alpha) && alpha > 0 && alpha < 1)) {
    stop("`alpha` must be a number strictly between 0 and 1", call. = FALSE)
  }
  list(preset = preset, suppress_k = as.integer(suppress_k), table2 = table2, alpha = as.numeric(alpha))
}

# `opts` must be what sr_writer_opts() returns; it is validated again so a hand-made list cannot slip through.
.sr_wd_opts <- function(opts) {
  need <- c("preset", "suppress_k", "table2", "alpha")
  if (!is.list(opts) || !all(need %in% names(opts))) {
    stop("`opts` must be the list returned by sr_writer_opts() (fields ", paste(need, collapse = ", "), ")",
         call. = FALSE)
  }
  do.call(sr_writer_opts, opts[need])
}

# The analyses with their labels after any fired replace_primary rule. sap.rule_fired already relabels the
# state (the label at the lock is then kept as `sap_label`); a state without that relabelling is relabelled
# here, so the alternative is reported as primary and the replaced analysis as sensitivity either way.
.sr_wd_analyses <- function(project) {
  an <- project$state$analyses %||% list()
  for (r in project$state$sap$fired %||% list()) {
    if (!identical(r$action, "replace_primary")) next
    a <- an[[r$analysis]]
    alt <- an[[r$alternative]]
    if (is.null(a) || is.null(alt) || !is.null(a$sap_label)) next
    an[[r$alternative]]$label <- a$label
    an[[r$analysis]]$label <- "sensitivity"
  }
  an
}

# TRUE per id when the analysis has an ok run (its latest ok run is what sr_result() reads).
.sr_wd_has_ok <- function(project, ids) {
  runs <- project$state$runs %||% list()
  vapply(ids, function(id) {
    k <- runs[[id]]$ok_key
    !is.null(k) && length(k) == 1L && !is.na(k)
  }, NA, USE.NAMES = FALSE)
}

#' The document model of a locked and run project.
#'
#' `main` and `supplement` are ordered lists of blocks: heading blocks `list(type = "heading", text,
#' level)`, paragraph blocks `list(type = "para", id, text, analysis, section)`, and the table and figure
#' blocks of writer/tables.R and writer/figures.R. `reg` is the placeholder registry every template reads.
#' `meta` records the lock, the library snapshot, the options, the purpose of each reported analysis and its
#' wording (confirmatory or exploratory). `not_reported` lists the secondary, sensitivity and exploratory
#' analyses with no ok run (id, label, reason "not run" or "latest run failed"). `dropped_families` names the
#' multiplicity families whose adjusted p values could not be computed, each with the reason.
#'
#' Refused, naming every offender: an unlocked SAP, a primary analysis with no ok run, and an analysis
#' whose stored result cannot be read.
#' @param project a project whose SAP is locked.
#' @param opts from sr_writer_opts().
sr_doc_model <- function(project, opts = sr_writer_opts()) {
  opts <- .sr_wd_opts(opts)
  if (!isTRUE(project$state$sap$locked)) {
    stop("the SAP is not locked: lock it (sr_sap_lock) before writing the manuscript", call. = FALSE)
  }
  if (.sr_wr_cache_on()) on.exit(.sr_ph_ctx$cache <- NULL, add = TRUE)

  an <- .sr_wd_analyses(project)
  all_ids <- names(an)
  lab <- vapply(all_ids, function(id) as.character(an[[id]]$label %||% NA_character_), "", USE.NAMES = FALSE)
  ok <- .sr_wd_has_ok(project, all_ids)

  bad <- all_ids[!ok & lab %in% "primary"]
  if (length(bad)) {
    stop("cannot write the manuscript: primary analysis ", paste(bad, collapse = ", "),
         " has no successful run; run it, or change its label", call. = FALSE)
  }
  res <- list()
  failed <- character()
  for (id in all_ids[ok]) {
    r <- tryCatch(.sr_ph_result(project, id), error = function(e) e)
    if (inherits(r, "error")) {
      failed <- c(failed, paste0(id, " (", conditionMessage(r), ")"))
    } else {
      res[[id]] <- r
    }
  }
  if (length(failed)) {
    stop("cannot write the manuscript: the result of ", paste(failed, collapse = "; "), call. = FALSE)
  }

  # the reported analyses: the main text first (by label, then in plan order), then the sensitivity analyses
  rep_ids <- all_ids[ok]
  rep_lab <- lab[ok]
  rank <- unname(SR_WR_LABEL_RANK[rep_lab])
  rank[is.na(rank)] <- max(SR_WR_LABEL_RANK) + 1L
  rep_ids <- rep_ids[order(rank, seq_along(rep_ids))]
  rep_lab <- lab[match(rep_ids, all_ids)]
  is_supp <- rep_lab %in% "sensitivity"
  main_ids <- rep_ids[!is_supp]
  supp_ids <- rep_ids[is_supp]
  label_of <- function(id) lab[match(id, all_ids)]

  # the analyses with no ok run, and why
  runs <- project$state$runs %||% list()
  miss <- all_ids[!ok]
  miss <- miss[!(label_of(miss) %in% "primary")]
  not_reported <- data.frame(
    id = as.character(miss), label = as.character(label_of(miss)),
    reason = vapply(miss, function(id) if (!is.null(runs[[id]]) && identical(runs[[id]]$ok, FALSE)) "latest run failed" else "not run", "",
                    USE.NAMES = FALSE),
    stringsAsFactors = FALSE)

  # the families whose adjusted p values can be computed (every member ran); the others are left out of the
  # view the table builders read
  fams <- project$state$sap$families %||% list()
  # only the sr_family_adjust() call is caught, and its message is kept: a dropped family is always reported
  fam_err <- vapply(names(fams), function(fid) {
    e <- tryCatch(sr_family_adjust(project, fid), error = function(e) conditionMessage(e))
    if (is.character(e)) e else NA_character_
  }, "", USE.NAMES = TRUE)
  usable <- names(fams)[is.na(fam_err)]
  dropped <- fam_err[!is.na(fam_err)]
  view <- project
  view$state$sap$families <- fams[usable]
  fa_cache <- lapply(stats::setNames(usable, usable), function(fid) sr_family_adjust(view, fid))
  # a fired replace_primary alternative is in the family of the analysis it replaced (ruling R16)
  fa_of <- function(id) {
    fid <- .sr_wr_family_id_of(view, id)
    if (is.na(fid)) NULL else list(fid = fid, fa = fa_cache[[fid]])
  }

  # purpose and wording of each reported analysis
  purpose_of <- function(id) {
    a <- an[[id]]
    r <- tryCatch(sr_route_get(as.character(a$route), a$route_version), error = function(e) NULL)
    if (is.null(r)) NA_character_ else as.character(r$purpose)
  }
  purposes <- vapply(rep_ids, purpose_of, "", USE.NAMES = TRUE)
  wording_of <- function(id) {
    dec <- project$state$feasibility[[id]]$decision$decision
    if (label_of(id) %in% c("exploratory", "post_hoc") || identical(dec, "exploratory")) "exploratory" else "confirmatory"
  }
  wording <- vapply(rep_ids, wording_of, "", USE.NAMES = TRUE)

  reg <- sr_ph_registry(k = opts$suppress_k)
  keys <- lapply(stats::setNames(rep_ids, rep_ids), function(id) .sr_wr_keys(project, id, res[[id]]))
  methods_para <- function(id) {
    .sr_wr_para(paste0("methods_", id),
                .sr_wr_m_analysis(project, id, res[[id]], label_of(id), reg, keys[[id]]$methods), id, "methods")
  }
  results_paras <- function(id) {
    out <- list()
    if (identical(project$state$analyses[[id]]$route, "table1")) {
      out <- c(out, list(.sr_wr_para(paste0("results_table1_", id), .sr_wr_r_table1(project, id, reg), id, "results")))
    }
    if (!identical(purposes[[id]], "describe")) {
      txt <- .sr_wr_r_effects(project, id, res[[id]], wording[[id]], reg, fa_of)
      if (!is.null(txt)) out <- c(out, list(.sr_wr_para(paste0("results_", id), txt, id, "results")))
    }
    out
  }

  # ---- the main document ----
  main <- list(.sr_wr_head("Methods", 1L), .sr_wr_head("Statistical analysis", 2L),
               .sr_wr_para("methods_lock", .sr_wr_m_lock(project, reg), NA_character_, "methods"))
  for (id in main_ids) main <- c(main, list(methods_para(id)))
  for (id in main_ids[label_of(main_ids) %in% "primary"]) {
    txt <- .sr_wr_m_feas(project, id, reg)
    if (!is.null(txt)) main <- c(main, list(.sr_wr_para(paste0("methods_feas_", id), txt, id, "methods")))
  }
  for (fid in usable) {
    main <- c(main, list(.sr_wr_para(paste0("methods_family_", fid), .sr_wr_m_family(fid, fa_cache[[fid]], reg),
                                     NA_character_, "methods")))
  }
  main <- c(main, list(
    .sr_wr_para("methods_deviations", .sr_wr_m_deviations(project), NA_character_, "methods"),
    .sr_wr_para("methods_software", .sr_wr_m_software(reg, unlist(lapply(keys, function(k) k$packages))),
                NA_character_, "methods")))
  main <- c(main, list(.sr_wr_head("Results", 1L)))
  for (id in main_ids) main <- c(main, results_paras(id))
  if (length(supp_ids)) {
    main <- c(main, list(.sr_wr_para("results_sensitivity_note",
                                     "Sensitivity analyses are reported in the supplementary material.", NA_character_,
                                     "results")))
  }
  nr <- .sr_wr_r_not_reported(not_reported, dropped, reg)
  if (!is.null(nr)) main <- c(main, list(.sr_wr_para("results_not_reported", nr, NA_character_, "results")))

  # ---- the supplement ----
  supplement <- list()
  if (length(supp_ids)) {
    supplement <- list(.sr_wr_head("Sensitivity analyses", 1L))
    for (id in supp_ids) supplement <- c(supplement, list(methods_para(id)), results_paras(id))
  }

  # ---- tables and figures (writer/tables.R, writer/figures.R) ----
  topts <- list(table2 = opts$table2, suppress_k = opts$suppress_k)
  tables <- list()
  for (id in rep_ids) tables <- c(tables, .sr_wr_route_tables(view, id, reg, topts))
  main_tab <- .sr_wr_main_table(view, main_ids, reg, topts)
  fam_tab <- .sr_wr_family_table(view, reg)
  dev_tab <- .sr_wr_deviation_table(view, reg)
  figs <- .sr_wr_figures(view, main_ids, reg, topts)
  in_main <- function(b) identical(b$where, "main")
  main <- c(main, Filter(in_main, tables), if (nrow(main_tab$df)) list(main_tab), figs)
  supp_blocks <- c(Filter(Negate(in_main), tables), if (!is.null(fam_tab)) list(fam_tab), if (!is.null(dev_tab)) list(dev_tab))
  if (length(supp_blocks)) supplement <- c(supplement, list(.sr_wr_head("Supplementary tables", 1L)), supp_blocks)

  lock <- project$state$sap$lock
  meta <- list(sap_hash = as.character(lock$sap_hash), locked_at = as.character(lock$locked_at),
               libsnap = sr_libsnap(), preset = opts$preset, alpha = opts$alpha, purposes = purposes,
               wording = wording)
  list(meta = meta, reg = reg, main = main, supplement = supplement, not_reported = not_reported,
       dropped_families = dropped)
}
