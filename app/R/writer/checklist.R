# writer/checklist.R - the STROBE reporting checklist, scored from the artifacts of the document model.
# The items and their short labels are in inst/templates/checklists/strobe.csv (item, section, label,
# rule); a rule says which artifact of the manuscript an item looks for. Items the authors alone can
# answer (the title, the background, the limitations) are amber with "for the authors to write".
#
#   sr_checklist_strobe(model, project)   data.frame(item, section, label, status, detail)
#                                         status is green, amber or red
#
# The checklist reads the model but is not part of the claim guard (ruling R27): its labels are fixed text.

SR_CK_RULES <- c("author", "flow", "roles", "feasibility", "terms", "methods", "interactions", "missing",
                 "sensitivity", "table1", "table1_missing", "followup", "events", "primary_ci", "abs_risk")

# The survival routes (route ids), for the follow-up item.
SR_CK_SURVIVAL_ROUTES <- c("cox", "rmst")

# inst/templates/checklists/<name>.csv: next to app/ in the repo and in the portable bundle.
.sr_ck_template_path <- function(name) .sr_wr_template("checklists", paste0(name, ".csv"), what = "checklist file")

# The STROBE items: data.frame(item, section, label, rule), every rule known.
.sr_ck_template <- function() {
  tpl <- utils::read.csv(.sr_ck_template_path("strobe"), stringsAsFactors = FALSE, colClasses = "character",
                         encoding = "UTF-8")
  need <- c("item", "section", "label", "rule")
  if (!identical(names(tpl), need)) {
    stop("strobe.csv must have the columns ", paste(need, collapse = ", "), call. = FALSE)
  }
  bad <- setdiff(unique(tpl$rule), SR_CK_RULES)
  if (length(bad)) stop("strobe.csv names an unknown rule: ", paste(bad, collapse = ", "), call. = FALSE)
  tpl
}

.sr_ck_score <- function(status, detail) list(status = status, detail = detail)

.sr_ck_ids <- function(ids) paste(ids, collapse = ", ")

#' The STROBE checklist of a document model.
#'
#' @param model a document model from sr_doc_model(); a checklist block in it is ignored.
#' @param project the project the model was built from.
#' @return data.frame(item, section, label, status, detail), one row per STROBE item. `detail` says which
#'   artifact was found or what is missing. An item that an analysis listed in `model$not_reported` bears on
#'   gets "; not reported: <ids>" and is downgraded from green to amber.
sr_checklist_strobe <- function(model, project) {
  if (!(is.list(model) && all(c("main", "supplement", "meta", "not_reported") %in% names(model)))) {
    stop("`model` must be a document model (sr_doc_model)", call. = FALSE)
  }
  if (!(is.list(project) && is.list(project$state))) stop("`project` must be an open project", call. = FALSE)
  tpl <- .sr_ck_template()

  blocks <- Filter(function(b) is.list(b) && !identical(b$type, "checklist"), c(model$main, model$supplement))
  of_type <- function(type) Filter(function(b) identical(b$type, type), blocks)
  paras <- of_type("para")
  tabs <- of_type("table")
  figs <- of_type("figure")

  analyses <- .sr_wd_analyses(project)
  rep_ids <- names(model$meta$purposes)
  rep_ids <- if (is.null(rep_ids)) character() else rep_ids
  label_of <- function(id) as.character(analyses[[id]]$label %||% NA_character_)
  route_of <- function(id) as.character(analyses[[id]]$route %||% NA_character_)
  spec_of <- function(id) analyses[[id]]$spec
  purpose_of <- function(id) as.character(model$meta$purposes[[id]] %||% NA_character_)
  has_ia <- function(id) length(spec_of(id)[["interactions"]]) > 0L
  is_surv <- function(id) route_of(id) %in% SR_CK_SURVIVAL_ROUTES

  has_methods <- vapply(rep_ids, function(id) {
    any(vapply(paras, function(b) identical(b$id, paste0("methods_", id)), NA))
  }, NA, USE.NAMES = FALSE)
  prim_inf <- rep_ids[vapply(rep_ids, function(id) identical(label_of(id), "primary") && !identical(purpose_of(id), "describe"), NA)]
  by_route <- function(route) rep_ids[vapply(rep_ids, function(id) identical(route_of(id), route), NA)]
  t1_ids <- by_route("table1")

  eff <- model$meta$effects
  res <- lapply(stats::setNames(rep_ids, rep_ids), function(id) .sr_ph_result(project, id))
  finite_col <- function(e, nm) if (is.data.frame(e) && nm %in% names(e)) is.finite(as.numeric(e[[nm]])) else rep(FALSE, nrow(e))

  # ---- one scorer per rule ----
  s_author <- function() .sr_ck_score("amber", "for the authors to write")
  s_flow <- function() {
    fl <- Filter(function(b) identical(b$kind, "flow"), figs)
    if (length(fl)) .sr_ck_score("green", paste0("flow figure ", fl[[1L]]$id, " is in the manuscript"))
    else .sr_ck_score("amber", "no flow figure in the manuscript (no recorded exclusion step); describe the participant flow in the text")
  }
  s_methods <- function() {
    if (all(has_methods)) {
      .sr_ck_score("green", paste0("a Methods paragraph for every reported analysis (", .sr_ck_ids(rep_ids), ")"))
    } else {
      .sr_ck_score("red", paste0("no Methods paragraph for ", .sr_ck_ids(rep_ids[!has_methods])))
    }
  }
  s_feasibility <- function() {
    if (!length(prim_inf)) return(.sr_ck_score("green", "no primary inferential analysis to assess"))
    ok <- vapply(prim_inf, function(id) !is.null(project$state$feasibility[[id]]$assessment), NA)
    if (all(ok)) {
      .sr_ck_score("green", paste0("a pre-lock feasibility assessment for ", .sr_ck_ids(prim_inf)))
    } else {
      .sr_ck_score("red", paste0("no feasibility assessment for ", .sr_ck_ids(prim_inf[!ok])))
    }
  }
  s_terms <- function() .sr_ck_score("green", "terms and levels are defined in the Methods and the locked plan")
  s_interactions <- function() {
    ia <- rep_ids[vapply(rep_ids, has_ia, NA)]
    if (length(ia)) {
      .sr_ck_score("green", paste0("interactions are specified in ", .sr_ck_ids(ia)))
    } else {
      .sr_ck_score("amber", "no interaction or subgroup analysis was pre-specified; say so")
    }
  }
  s_missing <- function() {
    mt <- Filter(function(b) grepl("_missing$", as.character(b$id)) && as.character(b$analysis) %in% t1_ids, tabs)
    frag <- unlist(lapply(rep_ids, function(id) {
      nm <- names(res[[id]]$fragments)
      hit <- nm[nm %in% c("mi_note", "mi_auto", "exclude_rows")]
      if (length(hit)) paste0(id, ":", hit) else character()
    }))
    found <- c(if (length(mt)) paste0("missing-data table ", vapply(mt, function(b) as.character(b$id), "")), frag)
    if (length(found)) .sr_ck_score("green", paste0("reported: ", paste(found, collapse = ", ")))
    else .sr_ck_score("red", "no missing-data table, imputation note or excluded-rows note is reported")
  }
  s_sensitivity <- function() {
    se <- rep_ids[vapply(rep_ids, function(id) identical(label_of(id), "sensitivity"), NA)]
    if (length(se)) .sr_ck_score("green", paste0("sensitivity analysis reported: ", .sr_ck_ids(se)))
    else .sr_ck_score("amber", "no sensitivity analysis is reported")
  }
  s_table1 <- function() {
    if (length(t1_ids)) .sr_ck_score("green", paste0("Table 1 analysis reported: ", .sr_ck_ids(t1_ids)))
    else .sr_ck_score("red", "no Table 1 analysis is reported")
  }
  s_followup <- function() {
    sv <- rep_ids[vapply(rep_ids, is_surv, NA)]
    if (length(sv)) .sr_ck_score("amber", paste0("report the follow-up time (survival analysis: ", .sr_ck_ids(sv), ")"))
    else .sr_ck_score("green", "not applicable: no survival analysis is reported")
  }
  s_events <- function() {
    prim <- if (nrow(eff)) eff[eff$role %in% "primary", , drop = FALSE] else eff
    ev <- vapply(seq_len(nrow(prim)), function(i) {
      e <- res[[prim$analysis[i]]]$effects
      is.data.frame(e) && "events" %in% names(e) && is.finite(suppressWarnings(as.numeric(e$events[prim$row[i]])))
    }, NA)
    ob <- Filter(function(b) grepl("_observed_risk$", as.character(b$id)), tabs)
    # the effect rows carry an events field, but only a model_fit table (or an observed-risk table) prints it
    mf <- Filter(function(b) {
      grepl("_model_fit$", as.character(b$id)) && is.data.frame(b$df) && "Statistic" %in% names(b$df) &&
        any(startsWith(as.character(b$df$Statistic), "Events"))
    }, tabs)
    shown <- c(mf, ob)
    if (length(shown)) .sr_ck_score("green", paste0("event counts are printed in table ", as.character(shown[[1L]]$id)))
    else if (any(ev)) .sr_ck_score("amber", paste0("event counts exist for ", .sr_ck_ids(unique(prim$analysis[ev])),
                                                     " but no table prints them"))
    else .sr_ck_score("amber", "no event count or observed-risk table is reported")
  }
  s_primary_ci <- function() {
    if (!length(prim_inf)) return(.sr_ck_score("green", "no primary inferential analysis"))
    ok <- vapply(prim_inf, function(id) {
      e <- res[[id]]$effects
      is.data.frame(e) && any(e$role %in% "primary" & finite_col(e, "lo") & finite_col(e, "hi"))
    }, NA)
    if (all(ok)) .sr_ck_score("green", paste0("a primary estimate with a confidence interval for ", .sr_ck_ids(prim_inf)))
    else .sr_ck_score("red", paste0("no primary estimate with a finite interval for ", .sr_ck_ids(prim_inf[!ok])))
  }
  s_abs_risk <- function() {
    hit <- eff[eff$measure %in% c("RD", "NNT", "risk"), , drop = FALSE]
    if (nrow(hit)) {
      .sr_ck_score("green", paste0("absolute-risk rows reported (", .sr_ck_ids(sort(unique(hit$measure))), ") in ",
                                   .sr_ck_ids(unique(hit$analysis))))
    } else {
      .sr_ck_score("amber", "no risk difference, number needed to treat or risk row is reported")
    }
  }
  scorer <- list(author = s_author, flow = s_flow, roles = s_methods, methods = s_methods,
                 feasibility = s_feasibility, terms = s_terms, interactions = s_interactions, missing = s_missing,
                 sensitivity = s_sensitivity, table1 = s_table1, table1_missing = s_table1, followup = s_followup,
                 events = s_events, primary_ci = s_primary_ci, abs_risk = s_abs_risk)

  # the analyses with no ok run that an item bears on (the primary ones can never be missing)
  nr_ids <- as.character(model$not_reported$id)
  nr_for <- function(rule) {
    switch(rule,
           roles = , methods = , sensitivity = , events = , abs_risk = nr_ids,
           table1 = , table1_missing = nr_ids[vapply(nr_ids, function(id) identical(route_of(id), "table1"), NA)],
           interactions = nr_ids[vapply(nr_ids, has_ia, NA)],
           followup = nr_ids[vapply(nr_ids, is_surv, NA)],
           character())
  }

  status <- character(nrow(tpl))
  detail <- character(nrow(tpl))
  for (i in seq_len(nrow(tpl))) {
    sc <- scorer[[tpl$rule[i]]]()
    miss <- nr_for(tpl$rule[i])
    if (length(miss)) {
      sc$detail <- paste0(sc$detail, "; not reported: ", .sr_ck_ids(miss))
      if (identical(sc$status, "green")) sc$status <- "amber"
    }
    status[i] <- sc$status
    detail[i] <- sc$detail
  }
  out <- data.frame(item = tpl$item, section = tpl$section, label = tpl$label, status = status, detail = detail,
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}
