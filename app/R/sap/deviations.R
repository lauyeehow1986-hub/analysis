# sap/deviations.R - after the lock: analyses added later, acting on diagnostic fails, the list of
# deviations from the SAP and the multiplicity adjustment of a family.
#
# Analyses added after the lock (sr_analysis_add, event analysis.added) carry prespecified = FALSE, a
# label exploratory | post_hoc | sensitivity and a required reason. They are validated like a
# pre-specified analysis (spec, layer stack) and also compiled, so an analysis that can never run is
# refused at add time (ruling R19). They never enter the SAP hash.
#
# sr_diag_apply() acts on each `fail` verdict of an analysis's diagnostics (state$diag[[id]]):
#   * a SAP rule for that analysis and check fires (event sap.rule_fired, kept in state$sap$fired).
#     replace_primary relabels: the alternative takes the analysis's label and the analysis becomes a
#     sensitivity analysis (both stay pre-specified; the label at the lock is kept as `sap_label`, so
#     sr_sap_hash() still equals the lock's hash). add_sensitivity adds nothing (the alternative is
#     already pre-specified);
#   * without a rule, the sensitivity analysis of the check (.sr_diag_sensitivity, ruling R35) is
#     added as "<analysis>_<check>" (label sensitivity, parent the analysis, trigger the check). The
#     primary analysis never changes without a rule (spec principle 2, M1.1);
#   * a check with no sensitivity analysis (inf_censoring), or one whose analysis cannot be built, is
#     recorded as a limitation (event diag.limitation: state$diag[[id]][[check]]$limitation), never
#     skipped. A later sr_diag_run of that check replaces its record, limitation included; apply again.
# Applying twice changes nothing: a fired rule, an added analysis and a recorded limitation are not
# repeated. An id taken by another analysis is refused before anything is written.

#' The id pattern of an analysis added after the lock: at most 40 characters.
SR_ANALYSIS_ID_RE <- "^[a-z][a-z0-9_]{0,39}$"

# ---- reducers --------------------------------------------------------------------------------------

sr_reducer_register("analysis.added", function(state, event) {
  p <- event$payload
  id <- .sr_sap_chr1(p$id)
  if (is.na(id) || !nzchar(id)) stop("analysis.added needs an id", call. = FALSE)
  rec <- list(id = id, route = as.character(p$route), route_version = as.integer(p$route_version),
              table = as.character(p$table), label = as.character(p$label), family = NA_character_,
              spec = p$spec, parent = .sr_sap_chr1(p$parent), trigger = .sr_sap_chr1(p$trigger),
              addresses = character(), prespecified = FALSE, reason = .sr_sap_chr1(p$reason))
  an <- state$analyses %||% list()
  an[[id]] <- rec
  state$analyses <- an
  state
})

sr_reducer_register("sap.rule_fired", function(state, event) {
  p <- event$payload
  rec <- list(rule = .sr_sap_chr1(p$rule), analysis = .sr_sap_chr1(p$analysis), check = .sr_sap_chr1(p$check),
              action = .sr_sap_chr1(p$action), alternative = .sr_sap_chr1(p$alternative),
              detail = .sr_sap_chr1(p$detail))
  if (is.na(rec$rule)) stop("sap.rule_fired needs a rule id", call. = FALSE)
  fired <- state$sap$fired %||% list()
  fired[[rec$rule]] <- rec
  state$sap$fired <- fired
  an <- state$analyses %||% list()
  a <- an[[rec$analysis]]
  alt <- an[[rec$alternative]]
  if (identical(rec$action, "replace_primary") && !is.null(a) && !is.null(alt)) {
    if (is.null(alt$sap_label)) an[[rec$alternative]]$sap_label <- alt$label
    if (is.null(a$sap_label)) an[[rec$analysis]]$sap_label <- a$label
    an[[rec$alternative]]$label <- a$label
    an[[rec$analysis]]$label <- "sensitivity"
    state$analyses <- an
  }
  state
})

sr_reducer_register("diag.limitation", function(state, event) {
  p <- event$payload
  id <- .sr_sap_chr1(p$analysis)
  ck <- .sr_sap_chr1(p$check)
  if (is.na(id) || is.na(ck)) stop("diag.limitation needs an analysis id and a check id", call. = FALSE)
  dg <- state$diag %||% list()
  one <- dg[[id]] %||% list()
  rec <- one[[ck]] %||% list(check = ck)
  rec$limitation <- .sr_sap_chr1(p$reason)
  one[[ck]] <- rec
  dg[[id]] <- one
  state$diag <- dg
  state
})

# ---- analyses added after the lock -----------------------------------------------------------------

#' Add an analysis after the SAP lock (`analysis.added`, prespecified = FALSE). Refused before the
#' lock, without a reason, for an id that is taken or does not match SR_ANALYSIS_ID_RE, a table that
#' was not part of the lock, an unknown parent, a trigger that is not a diagnostic id, a spec that
#' does not validate for the route, a refused or unresolved layer stack, or a spec the route cannot
#' compile (R19: an analysis that can never run is refused here, not at its first run).
#' @param label exploratory, post_hoc or sensitivity
#' @param reason why the analysis was added (required; shown in sr_deviations)
#' @param parent the analysis this one is a sensitivity analysis of (any analysis), or NA
#' @param trigger the diagnostic check that led to it, or NA
#' @return the project
sr_analysis_add <- function(project, id, route, spec, table, label = c("exploratory", "post_hoc", "sensitivity"),
                            reason, parent = NA, trigger = NA) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  if (!isTRUE(project$state$sap$locked)) {
    stop("the SAP is not locked; add pre-specified analyses with sr_sap_add", call. = FALSE)
  }
  .sr_sap_id(id, "id")
  if (!grepl(SR_ANALYSIS_ID_RE, id)) {
    stop("id ", .sr_sap_q(id), " must match ", SR_ANALYSIS_ID_RE, call. = FALSE)
  }
  if (!is.null(project$state$analyses[[id]])) {
    stop("analysis ", .sr_sap_q(id), " already exists; an analysis added after the lock never replaces one",
         call. = FALSE)
  }
  label <- match.arg(label)
  if (missing(reason) || !.sr_is_text1(reason) || !nzchar(trimws(reason))) {
    stop("a reason is required for an analysis added after the lock", call. = FALSE)
  }
  .sr_sap_id(route, "route")
  if (!is.list(spec)) stop("spec must be a list", call. = FALSE)
  .sr_store_check_table(table)
  if (!(table %in% sr_store_tables(project))) {
    stop("table ", .sr_sap_q(table), " is not in the project store", call. = FALSE)
  }
  if (is.null(project$state$sap$lock$tables[[table]])) {
    stop("table ", .sr_sap_q(table), " was not part of the SAP lock", call. = FALSE)
  }
  parent <- if (length(parent) == 1L && is.na(parent)) NA_character_ else .sr_sap_id(parent, "parent")
  if (!is.na(parent) && is.null(project$state$analyses[[parent]])) {
    stop("unknown parent analysis ", .sr_sap_q(parent), call. = FALSE)
  }
  trigger <- if (length(trigger) == 1L && is.na(trigger)) NA_character_ else trigger
  if (!is.na(trigger) && !(is.character(trigger) && length(trigger) == 1L && trigger %in% SR_DIAG_IDS)) {
    stop("trigger must be NA or one of: ", paste(SR_DIAG_IDS, collapse = ", "), call. = FALSE)
  }
  r <- sr_route_get(route)
  v <- sr_spec_validate(spec, r)
  if (!v$ok) {
    stop("the spec is not valid for route ", .sr_sap_q(route), ": ", paste(v$errors, collapse = "; "),
         call. = FALSE)
  }
  cc <- sr_compose_check(spec)
  if (!cc$ok) {
    stop("the layer combination is not allowed: ",
         paste(c(cc$refused, cc$unmet, names(cc$choices)), collapse = ", "),
         " (", paste(cc$notes, collapse = "; "), ")", call. = FALSE)
  }
  .sr_sap_compiles(r, spec, id, route)
  sr_project_append(project, "analysis.added", list(
    id = id, route = route, route_version = as.integer(r$version), table = table, label = label, spec = spec,
    parent = parent, trigger = trigger, reason = reason))
}

# ---- the sensitivity analysis of a failed check (ruling R35) ----------------------------------------

#' The id of the sensitivity analysis of `check` for analysis `analysis_id`: "<analysis>_<check>",
#' lower-cased with other characters as "_", the analysis part shortened so the whole id has at most
#' 40 characters and keeps the check.
.sr_diag_sensitivity_id <- function(analysis_id, check) {
  a <- gsub("[^a-z0-9_]", "_", tolower(analysis_id))
  if (!grepl("^[a-z]", a)) a <- paste0("a", a)
  keep <- 40L - nchar(check) - 1L
  paste0(substr(a, 1L, keep), "_", check)
}

#' The sensitivity analysis a failing `check` asks for (sr_diag_checks()$sensitivity, as code):
#' list(route, spec, note) built from the parent analysis's spec, or list(limitation = reason) when
#' none can be built. `rec` is the check's state$diag record; `analyses` (state$analyses) is where a
#' pre-specified RMST horizon is looked up (R5). Nothing here reads data.
#'   ph                 rmst (unadjusted) on time / status / status_event / exposure / seed, layers
#'                      (mi, boot) kept; tau of a pre-specified rmst analysis of this parent, else
#'                      tau_suggest; a clustered Cox parent gives an unclustered RMST (and says so)
#'   influence          same route, exclude_rows += the flagged rows (positions in the analysis table)
#'   separation         logistic with penalty = "firth" (model SE, no marginal effects); a clustered
#'                      parent gives an unclustered Firth analysis (and says so), Firth having no
#'                      cluster SE
#'   few_clusters       same route, the cluster bootstrap with B = 2000, percentile interval
#'   linearity          same route, the flagged term as RCS with 4 knots
#'   heteroscedasticity same route, se = "robust"; a limitation for a clustered parent, whose
#'                      cluster-robust SE is already heteroscedasticity-robust
#'   inf_censoring      a limitation (IPCW is P9)
.sr_diag_sensitivity <- function(check, parent, rec, analyses = list()) {
  lim <- function(...) list(limitation = paste0(...))
  ps <- .sr_spec_from_state(parent$spec)
  route <- parent$route
  base <- .sr_diag_base(route)
  model <- base %in% c("lm", "logistic", "modpois", "cox")
  switch(check,
    ph = {
      if (base != "cox") return(lim("the ph sensitivity analysis (RMST) needs a Cox analysis"))
      tau <- NA_real_
      src <- "tau_suggest, the 75th centile of the event times"
      kids <- Filter(function(a) {
        isTRUE(a$prespecified) && identical(.sr_diag_base(a$route), "rmst") && identical(.sr_sap_chr1(a$parent), parent$id)
      }, analyses)
      if (length(kids)) {
        k <- kids[[sort(names(kids), method = "radix")[1L]]]
        tau <- suppressWarnings(as.numeric(.sr_spec_from_state(k$spec)$tau))
        src <- paste0("the horizon of the pre-specified RMST analysis ", .sr_sap_q(k$id))
      }
      if (!(length(tau) == 1L && is.finite(tau) && tau > 0)) tau <- rec$tau_suggest
      if (!(is.numeric(tau) && length(tau) == 1L && is.finite(tau) && tau > 0)) {
        return(lim("no RMST horizon: no pre-specified RMST analysis of ", .sr_sap_q(parent$id),
                   " and no tau_suggest from the ph check"))
      }
      rr <- sr_route_get("rmst")
      L <- ps$layers
      notes <- sprintf("RMST difference to tau = %s (%s)", format(tau, digits = 6L), src)
      if (isTRUE(L$clustered)) {
        L$clustered <- FALSE
        notes <- c(notes, "the Cox analysis is clustered but RMST does not support clustering, so this RMST analysis is unclustered")
      }
      sp <- list(purpose = rr$purpose, time = ps$time, status = ps$status)
      if (!is.null(ps$status_event)) sp$status_event <- ps$status_event
      sp$exposure <- ps$exposure
      if (!is.null(ps$exposure_ref)) sp$exposure_ref <- ps$exposure_ref
      sp$tau <- tau
      sp$layers <- L
      for (k in c("mi", "boot")) if (!is.null(ps[[k]])) sp[[k]] <- ps[[k]]
      if (!is.null(ps$boot_type)) {
        notes <- c(notes, paste0("boot_type ", ps$boot_type, " is not carried over (the RMST bootstrap resamples ",
                                 "individuals)"))
      }
      sp$seed <- ps$seed
      if (length(ps$terms) || length(.sr_spec_chr(ps$strata))) {
        notes <- c(notes, "the RMST difference is unadjusted: model terms and strata are not carried over")
      }
      if (!identical(ps$purpose, rr$purpose)) notes <- c(notes, paste0("purpose ", rr$purpose, " (the rmst route)"))
      list(route = "rmst", spec = sp, note = paste(notes, collapse = "; "))
    },
    influence = {
      if (!model) return(lim("the influence sensitivity analysis needs an lm, logistic, modpois or cox analysis"))
      rows <- as.integer(unlist(rec$rows))
      rows <- rows[!is.na(rows)]
      if (!length(rows)) return(lim("the influence check flagged no rows, so there is nothing to exclude"))
      excl <- .sr_exclude_rows(ps)
      if (length(excl)) {
        # the flagged positions are positions of the already-reduced data: map them back to the table
        cand <- setdiff(seq_len(max(rows) + length(excl)), excl)
        rows <- cand[rows]
      }
      sp <- ps
      sp$exclude_rows <- as.integer(sort(unique(c(excl, rows))))
      list(route = route, spec = sp,
           note = sprintf("%d influential row(s) excluded (exclude_rows)", length(unique(rows))))
    },
    separation = {
      if (base != "logistic") return(lim("the separation sensitivity analysis (Firth) needs a logistic analysis"))
      sp <- ps
      notes <- "Firth penalised logistic regression"
      sp$penalty <- "firth"
      if (!is.null(sp$se) && !identical(sp$se, "model")) {
        notes <- c(notes, paste0("model-based (profile likelihood) intervals instead of se = ", sp$se))
        sp$se <- "model"
      }
      if (isTRUE(sp$layers$clustered) || length(.sr_spec_chr(sp$cluster))) {
        notes <- c(notes, "the logistic analysis is clustered but Firth has no cluster-robust SE, so this Firth analysis is unclustered")
        sp$layers$clustered <- FALSE
        sp$cluster <- NULL
        if (!is.null(sp$boot_type)) {
          notes <- c(notes, paste0("boot_type ", sp$boot_type, " is not carried over (the bootstrap resamples individuals)"))
          sp$boot_type <- NULL
        }
      }
      if (isTRUE(sp$marginal)) {
        notes <- c(notes, "no marginal effects (Firth has none)")
        sp$marginal <- FALSE
      }
      list(route = route, spec = sp, note = paste(notes, collapse = "; "))
    },
    few_clusters = {
      if (!model) return(lim("the few_clusters sensitivity analysis needs an lm, logistic, modpois or cox analysis"))
      sp <- ps
      sp$layers$resampling <- "bootstrap"
      sp$boot_type <- "cluster"
      sp$boot <- list(B = 2000L, type = "percentile")
      list(route = route, spec = sp, note = "cluster bootstrap, B = 2000, percentile interval")
    },
    linearity = {
      if (!model) return(lim("the linearity sensitivity analysis needs an lm, logistic, modpois or cox analysis"))
      var <- .sr_sap_chr1(rec$var)
      if (is.na(var)) return(lim("the linearity check named no term"))
      if (identical(var, ps$exposure)) {
        return(lim("the flagged term ", .sr_sap_q(var), " is the exposure, which cannot be entered as a spline ",
                   "in P1c; consider a categorised exposure"))
      }
      idx <- which(vapply(ps$terms, function(t) identical(t[["var"]], var), NA))
      if (length(idx) != 1L) return(lim("the flagged term ", .sr_sap_q(var), " is not a term of the analysis"))
      t <- ps$terms[[idx]]
      if (!is.null(t$type) && !identical(t$type, "linear")) {
        return(lim("the flagged term ", .sr_sap_q(var), " is not entered linearly"))
      }
      t$type <- "rcs"
      t$knots <- 4L
      t$ref <- NULL
      sp <- ps
      sp$terms[[idx]] <- t
      list(route = route, spec = sp, note = sprintf("%s as a restricted cubic spline with 4 knots", var))
    },
    heteroscedasticity = {
      if (base != "lm") return(lim("the heteroscedasticity sensitivity analysis needs an lm analysis"))
      if (identical(ps$se, "cluster")) {
        return(lim("the analysis already has cluster-robust (HC1) standard errors, which are robust to ",
                   "heteroscedasticity; no sensitivity analysis is built"))
      }
      sp <- ps
      sp$se <- "robust"
      list(route = route, spec = sp, note = "robust (HC3) standard errors")
    },
    inf_censoring = lim("possibly informative censoring is not corrected in P1c (inverse probability of censoring ",
                        "weighting arrives in P9); reported as a limitation, no sensitivity analysis is built"),
    lim("no sensitivity analysis is defined for check ", .sr_sap_q(check))
  )
}

# ---- acting on the diagnostics ---------------------------------------------------------------------

#' Act on every `fail` verdict in state$diag[[analysis_id]] (see the header): fire the matching SAP
#' rules, else add the check's sensitivity analysis as "<analysis>_<check>", else record a
#' limitation. Never changes the primary analysis without a rule. Refused (nothing written) for a
#' read-only project, an unknown analysis, an unlocked SAP, an analysis without diagnostics, or an id
#' "<analysis>_<check>" that another analysis already holds.
#' @return the project
sr_diag_apply <- function(project, analysis_id) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  .sr_sap_id(analysis_id, "analysis_id")
  a <- project$state$analyses[[analysis_id]]
  if (is.null(a)) stop("unknown analysis ", .sr_sap_q(analysis_id), call. = FALSE)
  if (!isTRUE(project$state$sap$locked)) stop("the SAP is not locked", call. = FALSE)
  dg <- project$state$diag[[analysis_id]]
  if (!length(dg)) {
    stop("analysis ", .sr_sap_q(analysis_id), " has no diagnostics yet; run sr_diag_run first", call. = FALSE)
  }
  checks <- intersect(SR_DIAG_IDS, names(dg))
  fails <- checks[vapply(checks, function(ck) identical(dg[[ck]]$verdict, "fail"), NA)]
  rules <- project$state$sap$rules %||% list()
  fired <- project$state$sap$fired %||% list()

  plan <- list()
  step <- function(...) plan[[length(plan) + 1L]] <<- list(...)
  for (ck in fails) {
    rec <- dg[[ck]]
    detail <- .sr_sap_chr1(rec$detail)
    why <- sprintf("%s verdict fail: %s", ck, if (is.na(detail)) "no detail recorded" else detail)
    hit <- Filter(function(r) identical(r$analysis, analysis_id) && identical(r$check, ck), rules)
    if (length(hit)) {
      for (rid in sort(names(hit), method = "radix")) {
        if (is.null(fired[[rid]])) step(type = "rule", check = ck, rule = hit[[rid]], why = why)
      }
      next
    }
    m <- .sr_diag_sensitivity(ck, a, rec, project$state$analyses)
    if (!is.null(m$limitation)) {
      step(type = "limitation", check = ck, reason = paste0(why, "; ", m$limitation))
      next
    }
    sid <- .sr_diag_sensitivity_id(analysis_id, ck)
    ex <- project$state$analyses[[sid]]
    if (!is.null(ex)) {
      if (!isTRUE(ex$prespecified) && identical(ex$parent, analysis_id) && identical(ex$trigger, ck)) next
      stop("cannot add the ", ck, " sensitivity analysis of ", .sr_sap_q(analysis_id), ": the id ", .sr_sap_q(sid),
           " is taken by another analysis (nothing was changed)", call. = FALSE)
    }
    step(type = "add", check = ck, id = sid, route = m$route, spec = m$spec,
         reason = paste(c(why, m$note), collapse = "; "))
  }

  limit <- function(project, ck, reason) {
    if (identical(project$state$diag[[analysis_id]][[ck]]$limitation, reason)) return(project)
    sr_project_append(project, "diag.limitation", list(analysis = analysis_id, check = ck, reason = reason))
  }
  for (s in plan) {
    project <- switch(s$type,
      rule = sr_project_append(project, "sap.rule_fired", list(
        rule = s$rule$id, analysis = analysis_id, check = s$check, action = s$rule$action,
        alternative = s$rule$alternative, detail = s$why)),
      limitation = limit(project, s$check, s$reason),
      add = tryCatch(
        sr_analysis_add(project, s$id, s$route, s$spec, a$table, "sensitivity", reason = s$reason,
                        parent = analysis_id, trigger = s$check),
        error = function(e) {
          limit(project, s$check, paste0(s$reason, "; the sensitivity analysis (route ", s$route,
                                         ") could not be built: ", conditionMessage(e)))
        })
    )
  }
  project
}

# ---- deviations ------------------------------------------------------------------------------------

#' Every departure from the locked SAP, one row each: data.frame(analysis, label, kind, reason,
#' trigger). kind: exploratory / post_hoc (an analysis added after the lock with that label),
#' unplanned_sensitivity (a sensitivity analysis added after the lock, by hand or by sr_diag_apply),
#' rule_fired (a pre-specified rule acted on a diagnostic; analysis = the analysis the check failed
#' for), lib_changed (the latest run of the analysis used a library snapshot other than the lock's).
sr_deviations <- function(project) {
  st <- project$state
  an <- st$analyses %||% list()
  out <- list()
  add <- function(analysis, label, kind, reason, trigger) {
    out[[length(out) + 1L]] <<- data.frame(analysis = analysis, label = label, kind = kind, reason = reason,
                                           trigger = trigger, stringsAsFactors = FALSE)
  }
  label_of <- function(id) {
    lb <- an[[id]]$label
    if (is.null(lb)) NA_character_ else as.character(lb)
  }
  for (id in names(an)) {
    a <- an[[id]]
    if (isTRUE(a$prespecified)) next
    kind <- if (identical(a$label, "sensitivity")) "unplanned_sensitivity" else as.character(a$label)
    add(id, a$label, kind, .sr_sap_chr1(a$reason), .sr_sap_chr1(a$trigger))
  }
  for (f in st$sap$fired %||% list()) {
    what <- if (identical(f$action, "replace_primary")) {
      sprintf("replace_primary: the pre-specified %s takes the place of %s, which becomes a sensitivity analysis",
              .sr_sap_q(f$alternative), .sr_sap_q(f$analysis))
    } else {
      sprintf("add_sensitivity: the pre-specified %s is reported", .sr_sap_q(f$alternative))
    }
    add(f$analysis, label_of(f$analysis), "rule_fired", sprintf("rule %s fired (%s); %s", f$rule, f$detail, what),
        f$check)
  }
  runs <- st$runs %||% list()
  for (id in names(runs)) {
    if (isTRUE(runs[[id]]$lib_changed)) {
      add(id, label_of(id), "lib_changed",
          "the latest run used a library snapshot that differs from the one recorded at the SAP lock", NA_character_)
    }
  }
  if (!length(out)) {
    return(data.frame(analysis = character(), label = character(), kind = character(), reason = character(),
                      trigger = character(), stringsAsFactors = FALSE))
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

# ---- multiplicity ----------------------------------------------------------------------------------

#' The multiplicity adjustment of a family: data.frame(analysis, term, p, p_adj, method). Each member
#' contributes the primary-role effects rows of its latest ok run (sr_result) that share the first
#' primary row's measure, so the model's exposure effect counts once (one row per non-reference
#' level) and its marginal re-expressions (RD, RR, NNT) do not; stats::p.adjust(method) then adjusts
#' over all rows (a p of NA, as under the bootstrap, stays NA and is not counted). A member replaced
#' by a fired replace_primary rule is represented by its alternative. Refused for an unknown family,
#' or when a member has not run successfully (the error names every such member).
sr_family_adjust <- function(project, family_id) {
  .sr_sap_id(family_id, "family_id")
  f <- project$state$sap$families[[family_id]]
  if (is.null(f)) stop("unknown family ", .sr_sap_q(family_id), call. = FALSE)
  members <- .sr_sap_chrv(f$members)
  method <- as.character(f$method)
  fired <- project$state$sap$fired %||% list()
  use <- vapply(members, function(m) {
    for (r in fired) if (identical(r$action, "replace_primary") && identical(r$analysis, m)) return(r$alternative)
    m
  }, "", USE.NAMES = FALSE)
  runs <- project$state$runs %||% list()
  not_run <- use[vapply(use, function(m) {
    r <- runs[[m]]
    is.null(r) || is.null(r$ok_key) || is.na(r$ok_key)
  }, NA)]
  if (length(not_run)) {
    stop("family ", .sr_sap_q(family_id), ": member(s) not yet run successfully: ", paste(not_run, collapse = ", "),
         call. = FALSE)
  }
  rows <- lapply(use, function(m) {
    e <- sr_result(project, m)$effects
    pr <- e[e$role == "primary", , drop = FALSE]
    if (!nrow(pr)) stop("analysis ", .sr_sap_q(m), " has no primary-role effect row", call. = FALSE)
    pr <- pr[pr$measure == pr$measure[1L], , drop = FALSE]
    data.frame(analysis = m, term = as.character(pr$term), p = as.numeric(pr$p), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$p_adj <- stats::p.adjust(out$p, method = method)
  out$method <- rep(method, nrow(out))
  rownames(out) <- NULL
  out
}
