# ui/analyses_view.R - the pure view builders behind the Analyses panel (no Shiny, no side effects).
#
# Every builder reads the replayed project (p$state) and, where a card shows a result, sr_result() or a
# small cached RDS under p$paths$fits, so nothing here scales with row count. Numbers are only rounded
# (never computed): est / lo / hi / mcse to 3 decimals, p-values to 3 significant digits, times to 0.1 s.
# `live` is a named character vector c(<analysis id> = <job>) of the run in progress, character() when none.

SR_AN_ROLE_ORDER <- c("primary", "covariate", "descriptive")

# One string, "" when it is missing.
.sr_an_s <- function(x) if (is.null(x) || !length(x) || is.na(x[[1L]])) "" else as.character(x[[1L]])

# The ids that are in the live job set.
.sr_an_live_ids <- function(live) if (is.null(live)) character() else names(live)

# TRUE when the analysis `id` is live with the job `job` (a stored "running" entry is live only for its own job).
.sr_an_is_live <- function(live, id, job) {
  if (is.null(live) || !length(live) || !(id %in% names(live))) return(FALSE)
  j <- .sr_an_s(job)
  !nzchar(j) || identical(as.character(live[[id]]), j)
}

.sr_an_analyses <- function(p) p$state$analyses %||% list()

# ---- order and status ------------------------------------------------------------------------------

#' The analyses in display order: data.frame(id, depth). Primary analyses then secondary (each by id),
#' each followed depth-first by its children (parent == id; pre-specified before unplanned, then by id);
#' then every analysis not yet placed, by id, at depth 0.
.sr_an_order <- function(p) {
  an <- .sr_an_analyses(p)
  ids <- sort(as.character(names(an)), method = "radix")
  lab <- vapply(ids, function(i) .sr_an_s(an[[i]]$label), "")
  out_id <- character()
  out_depth <- integer()
  place <- function(id, depth) {
    if (id %in% out_id) return(invisible())
    out_id <<- c(out_id, id)
    out_depth <<- c(out_depth, depth)
    kids <- ids[vapply(ids, function(k) identical(.sr_an_s(an[[k]]$parent), id) && !(k %in% out_id), NA)]
    if (length(kids)) {
      pre <- vapply(kids, function(k) isTRUE(an[[k]]$prespecified), NA)
      kids <- c(kids[pre], kids[!pre])
      for (k in kids) place(k, depth + 1L)
    }
    invisible()
  }
  for (l in c("primary", "secondary")) for (id in ids[lab == l]) place(id, 0L)
  for (id in ids) place(id, 0L)
  data.frame(id = out_id, depth = as.integer(out_depth), stringsAsFactors = FALSE)
}

#' The status of every analysis (named character): "running" (the id is live), "not run", else the run's
#' status ("ok", "failed", "cancelled"; "ok" / "failed" from `ok` in an old state), with a stored
#' "running" that is not live shown as "interrupted".
.sr_an_status <- function(p, live = character()) {
  o <- .sr_an_order(p)
  runs <- p$state$runs %||% list()
  st <- vapply(o$id, function(id) {
    r <- runs[[id]]
    # a live id with no stored run (its run.started is not replayed yet) is running; with a stored
    # "running" entry it is running only for that entry's own job
    if (id %in% .sr_an_live_ids(live) && (is.null(r) || !identical(r$status, "running") || .sr_an_is_live(live, id, r$job))) {
      return("running")
    }
    if (is.null(r)) return("not run")
    s <- r$status
    if (is.null(s) || length(s) != 1L || is.na(s)) s <- if (isTRUE(r$ok)) "ok" else "failed"
    if (identical(s, "running")) "interrupted" else s
  }, "")
  stats::setNames(st, o$id)
}

# "<n> ok, <n> warn, <n> fail" over the recorded diagnostics of an analysis (zero counts omitted).
.sr_an_diag_summary <- function(p, id) {
  dg <- p$state$diag[[id]]
  if (!length(dg)) return("")
  v <- vapply(dg, function(r) .sr_an_s(r$verdict), "")
  parts <- vapply(c("ok", "warn", "fail"), function(k) if (any(v == k)) paste(sum(v == k), k) else "", "")
  paste(parts[nzchar(parts)], collapse = ", ")
}

#' The Runs card table: data.frame(id, label, route, status, last_run, elapsed, cached, diagnostics).
#' id is indented by depth ("  - child"); last_run is the latest history entry's end (else start).
.sr_an_runs_table <- function(p, live = character()) {
  o <- .sr_an_order(p)
  an <- .sr_an_analyses(p)
  runs <- p$state$runs %||% list()
  st <- .sr_an_status(p, live)
  last <- function(id) {
    h <- runs[[id]]$history
    if (!length(h)) return("")
    e <- h[[length(h)]]
    ended <- .sr_an_s(e$ended)
    if (nzchar(ended)) ended else .sr_an_s(e$started)
  }
  elapsed <- function(id) {
    if (!(st[[id]] %in% c("ok", "failed"))) return(NA_real_)
    x <- runs[[id]]$elapsed
    if (is.null(x) || !length(x) || is.na(x[[1L]])) NA_real_ else round(as.numeric(x[[1L]]), 1L)
  }
  cached <- function(id) {
    r <- runs[[id]]
    if (!(st[[id]] %in% c("ok", "failed")) || is.null(r) || is.null(r$cached)) "" else if (isTRUE(r$cached)) "yes" else "no"
  }
  data.frame(
    id = vapply(seq_len(nrow(o)), function(i) {
      if (o$depth[i] > 0L) paste0(strrep("  ", o$depth[i]), "- ", o$id[i]) else o$id[i]
    }, ""),
    label = vapply(o$id, function(i) .sr_an_s(an[[i]]$label), ""),
    route = vapply(o$id, function(i) .sr_an_s(an[[i]]$route), ""),
    status = unname(st[o$id]),
    last_run = vapply(o$id, last, ""),
    elapsed = vapply(o$id, elapsed, 0),
    cached = vapply(o$id, cached, ""),
    diagnostics = vapply(o$id, function(i) .sr_an_diag_summary(p, i), ""),
    stringsAsFactors = FALSE, row.names = NULL)
}

# ---- queue and gate --------------------------------------------------------------------------------

#' The ids to run: "selected" gives `selected` (every id must be a known analysis); "all" gives the
#' analyses, in display order, whose status is not "ok".
.sr_an_queue <- function(p, mode = c("selected", "all"), selected = NULL) {
  mode <- match.arg(mode)
  if (identical(mode, "all")) {
    st <- .sr_an_status(p)
    return(names(st)[st != "ok"])
  }
  selected <- as.character(selected)
  if (!length(selected) || anyNA(selected) || !all(nzchar(selected))) stop("select an analysis first", call. = FALSE)
  bad <- setdiff(selected, names(.sr_an_analyses(p)))
  if (length(bad)) stop("unknown analysis ", paste(sQuote(bad, FALSE), collapse = ", "), call. = FALSE)
  selected
}

#' NULL when a run may start, else the reason it may not (checked in this order).
.sr_an_can_run <- function(p, busy = FALSE) {
  if (is.null(p)) return("open a project first")
  if (isTRUE(busy)) return("wait for the running import or analysis to finish")
  if (isTRUE(p$readonly)) return("the project is read-only (locked by another session)")
  if (sr_blind(p)) return("lock the SAP first")
  stp <- sr_project_stopped(p)
  if (!is.null(stp)) return(paste0("the project is stopped: ", stp$reason %||% "no reason"))
  NULL
}

# ---- a result --------------------------------------------------------------------------------------

# Round the numeric columns that are shown (a copy; NA stays NA).
.sr_an_round_effects <- function(e) {
  for (k in intersect(c("est", "lo", "hi", "mcse"), names(e))) e[[k]] <- round(e[[k]], 3L)
  if ("p" %in% names(e)) e$p <- signif(e$p, 3L)
  e
}

#' The effects frame for the Result card: primary rows, then covariate, then descriptive (stable), numbers
#' rounded, plus `flags` (the reporting flags of the row's term joined by "; ").
.sr_an_effects <- function(presented) {
  e <- presented$effects
  fl <- presented$flags
  rk <- match(as.character(e$role), SR_AN_ROLE_ORDER)
  rk[is.na(rk)] <- length(SR_AN_ROLE_ORDER) + 1L
  e <- e[order(rk, seq_len(nrow(e))), , drop = FALSE]
  e$flags <- if (is.data.frame(fl) && nrow(fl) && nrow(e)) {
    vapply(as.character(e$term), function(t) paste(fl$flag[fl$term == t], collapse = "; "), "", USE.NAMES = FALSE)
  } else {
    rep("", nrow(e))
  }
  e <- .sr_an_round_effects(e)
  rownames(e) <- NULL
  e
}

#' The figures to draw: named list of list(kind, data). km carries km_risk (it is not drawn alone); any
#' other name in SR_PLOT_KINDS is its own kind; other names are dropped.
.sr_an_figures <- function(presented) {
  fg <- presented$figures
  out <- list()
  for (nm in names(fg)) {
    if (identical(nm, "km")) {
      d <- list(km = fg$km)
      if (!is.null(fg$km_risk)) d$km_risk <- fg$km_risk
      out[[nm]] <- list(kind = "km", data = d)
    } else if (nm %in% SR_PLOT_KINDS) {
      out[[nm]] <- list(kind = nm, data = fg[[nm]])
    }
  }
  out
}

# ---- diagnostics -----------------------------------------------------------------------------------

.sr_an_diag_empty <- function() {
  data.frame(check = character(), verdict = character(), value = numeric(), warn = numeric(),
             fail = numeric(), detail = character(), stringsAsFactors = FALSE)
}

#' One row per recorded check of the analysis, in SR_DIAG_IDS order.
.sr_an_diag_rows <- function(p, id) {
  dg <- p$state$diag[[id]]
  cks <- intersect(SR_DIAG_IDS, names(dg))
  if (!length(cks)) return(.sr_an_diag_empty())
  num <- function(x) if (is.null(x) || !length(x) || is.na(x[[1L]])) NA_real_ else as.numeric(x[[1L]])
  data.frame(check = cks,
             verdict = vapply(cks, function(k) .sr_an_s(dg[[k]]$verdict), "", USE.NAMES = FALSE),
             value = vapply(cks, function(k) num(dg[[k]]$value), 0, USE.NAMES = FALSE),
             warn = vapply(cks, function(k) num(dg[[k]]$warn), 0, USE.NAMES = FALSE),
             fail = vapply(cks, function(k) num(dg[[k]]$fail), 0, USE.NAMES = FALSE),
             detail = vapply(cks, function(k) .sr_an_s(dg[[k]]$detail), "", USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}

#' The plot of a check, list(kind, data), or NULL: ph -> schoenfeld, linearity -> linearity; the data is
#' plot_data of the cached check value (NULL on any read error or an empty frame).
.sr_an_diag_plot <- function(p, id, check) {
  if (length(check) != 1L || is.na(check)) return(NULL)
  kind <- c(ph = "schoenfeld", linearity = "linearity")[check]
  if (is.na(kind)) return(NULL)
  key <- p$state$diag[[id]][[check]]$key
  if (is.null(key) || length(key) != 1L || is.na(key)) return(NULL)
  pd <- tryCatch(suppressWarnings(readRDS(file.path(p$paths$fits, paste0(key, ".rds"))))$value$plot_data,
                 error = function(e) NULL)
  if (!is.data.frame(pd) || !nrow(pd)) return(NULL)
  list(kind = unname(kind), data = pd)
}

#' The rows of sr_diag_apply_plan in words, ids in backticks.
.sr_an_plan_words <- function(p, plan) {
  if (is.null(plan) || !nrow(plan)) return(character())
  q <- function(x) paste0("`", x, "`")
  vapply(seq_len(nrow(plan)), function(i) {
    switch(plan$type[i],
      rule = {
        r <- p$state$sap$rules[[plan$target[i]]]
        if (is.null(r)) {
          paste0("fire rule ", q(plan$target[i]))
        } else {
          paste0("fire rule ", q(plan$target[i]), " (", .sr_an_s(r$action), " -> ", q(.sr_an_s(r$alternative)), ")")
        }
      },
      add = paste0("add sensitivity ", q(plan$target[i]), " (", .sr_an_s(plan$route[i]), ")"),
      limitation = paste0("record limitation: ", .sr_an_s(plan$reason[i])),
      paste(plan$type[i], .sr_an_s(plan$reason[i])))
  }, "")
}

# ---- primary vs sensitivity ------------------------------------------------------------------------

.sr_an_compare_empty <- function() {
  data.frame(block = character(), analysis = character(), role = character(), status = character(),
             measure = character(), term = character(), est = numeric(), lo = numeric(), hi = numeric(),
             stringsAsFactors = FALSE)
}

# The comparison rows of one analysis. `status` is what the Status column shows when the result is ok;
# `measure` NULL picks the analysis's own first primary measure (and returns it as attr "measure").
.sr_an_cmp_rows <- function(p, block, id, role, st, ok_label, measure = NULL) {
  row <- function(status, measure = NA_character_, term = NA_character_, est = NA_real_, lo = NA_real_, hi = NA_real_) {
    data.frame(block = block, analysis = id, role = role, status = status, measure = measure, term = term,
               est = est, lo = lo, hi = hi, stringsAsFactors = FALSE)
  }
  s <- st[[id]] %||% "not run"
  if (!identical(s, "ok")) return(row(s))
  e <- tryCatch(sr_result(p, id)$effects, error = function(e) NULL)
  if (is.null(e)) return(row("result missing"))
  e <- .sr_an_round_effects(e)
  if (is.null(measure)) {
    pr <- e[e$role == "primary", , drop = FALSE]
    if (!nrow(pr)) return(row(ok_label))
    measure <- as.character(pr$measure[1L])
    sel <- pr[pr$measure == measure, , drop = FALSE]
  } else {
    sel <- e[e$measure == measure & e$role == "primary", , drop = FALSE]
    if (!nrow(sel)) sel <- e[e$measure == measure, , drop = FALSE]
    if (!nrow(sel)) {
      # no row of the parent's measure (an RMST sensitivity of a Cox primary, say): show this analysis's
      # own first primary measure, so the Measure column says it is a different one
      pr <- e[e$role == "primary", , drop = FALSE]
      if (!nrow(pr)) return(row(ok_label))
      sel <- pr[pr$measure == as.character(pr$measure[1L]), , drop = FALSE]
    }
  }
  out <- row(ok_label, as.character(sel$measure), as.character(sel$term), sel$est, sel$lo, sel$hi)
  attr(out, "measure") <- measure
  out
}

#' The Primary vs sensitivity card: data.frame(block, analysis, role, status, measure, term, est, lo, hi).
#' One block per primary / secondary analysis (display order): its primary-role rows of its first primary
#' measure (role "main"), then the rows of that measure of each descendant sensitivity analysis. An
#' analysis without an ok result is one row with its status and NA numbers. A fired replace_primary shows
#' the alternative as "now primary" and, in its block, the replaced analysis as "replaced".
.sr_an_compare <- function(p, live = character()) {
  o <- .sr_an_order(p)
  an <- .sr_an_analyses(p)
  st <- .sr_an_status(p, live)
  lab <- vapply(o$id, function(i) .sr_an_s(an[[i]]$label), "")
  fired <- Filter(function(f) identical(f$action, "replace_primary"), p$state$sap$fired %||% list())
  rows <- list()
  top <- which(o$depth == 0L & lab %in% c("primary", "secondary"))
  # the rows below row i that are its descendants and have no block of their own
  desc <- function(i) {
    j <- i + 1L
    out <- integer()
    while (j <= nrow(o) && o$depth[j] > o$depth[i]) {
      if (!(lab[j] %in% c("primary", "secondary"))) out <- c(out, j)
      j <- j + 1L
    }
    out
  }
  for (i in top) {
    id <- o$id[i]
    replaces <- vapply(Filter(function(f) identical(f$alternative, id), fired), function(f) .sr_an_s(f$analysis), "")
    now_primary <- length(replaces) > 0L
    main <- .sr_an_cmp_rows(p, id, id, "main", st, if (now_primary) "now primary" else "ok")
    m <- attr(main, "measure")
    rows[[length(rows) + 1L]] <- main
    for (j in desc(i)) rows[[length(rows) + 1L]] <- .sr_an_cmp_rows(p, id, o$id[j], "sensitivity", st, "ok", m)
    # the "replaced" label overrides the replaced analysis' own status; its descendants stay in this block
    for (r in setdiff(replaces, "")) {
      if (!(r %in% o$id)) next
      rows[[length(rows) + 1L]] <- .sr_an_cmp_rows(p, id, r, "sensitivity", st, "replaced", m)
      for (j in desc(match(r, o$id))) rows[[length(rows) + 1L]] <- .sr_an_cmp_rows(p, id, o$id[j], "sensitivity", st, "ok", m)
    }
  }
  # a sensitivity analysis in no block (a missing parent, say) gets a block of its own: never blank
  seen <- unlist(lapply(rows, function(r) r$analysis))
  for (id in setdiff(o$id[!(lab %in% c("primary", "secondary"))], seen)) {
    rows[[length(rows) + 1L]] <- .sr_an_cmp_rows(p, id, id, "sensitivity", st, "ok")
  }
  if (!length(rows)) return(.sr_an_compare_empty())
  out <- do.call(rbind, lapply(rows, function(r) {
    attr(r, "measure") <- NULL
    r
  }))
  rownames(out) <- NULL
  out
}

# ---- multiplicity and history ----------------------------------------------------------------------

#' One entry per multiplicity family (by id): list(id, method, table, waiting). table is the adjusted
#' p-value table (rounded) or NULL; waiting the analyses (after replace_primary) with no ok result.
.sr_an_families <- function(p) {
  fam <- p$state$sap$families %||% list()
  runs <- p$state$runs %||% list()
  out <- list()
  for (id in sort(as.character(names(fam)), method = "radix")) {
    used <- tryCatch(.sr_family_used(p, id), error = function(e) character())
    waiting <- used[vapply(used, function(m) {
      k <- runs[[m]]$ok_key
      is.null(k) || length(k) != 1L || is.na(k)
    }, NA)]
    err <- NULL
    tb <- if (length(waiting)) NULL else tryCatch(sr_family_adjust(p, id), error = function(e) {
      err <<- conditionMessage(e)
      NULL
    })
    if (is.data.frame(tb)) tb$p <- signif(tb$p, 3L)
    if (is.data.frame(tb)) tb$p_adj <- signif(tb$p_adj, 4L)
    out[[length(out) + 1L]] <- list(id = id, method = .sr_an_s(fam[[id]]$method), table = tb,
                                    waiting = unname(waiting), error = err)
  }
  out
}

#' The cancelled and interrupted attempts: data.frame(analysis, job, started, ended, outcome, reason). An
#' open "running" entry whose analysis is not live counts as interrupted; `ended` is "interrupted" for
#' the interrupted ones.
.sr_an_history <- function(p, live = character()) {
  runs <- p$state$runs %||% list()
  rows <- list()
  for (id in sort(as.character(names(runs)), method = "radix")) {
    for (e in runs[[id]]$history %||% list()) {
      oc <- .sr_an_s(e$outcome)
      if (identical(oc, "running") && !.sr_an_is_live(live, id, e$job)) oc <- "interrupted"
      if (!(oc %in% c("cancelled", "interrupted"))) next
      rows[[length(rows) + 1L]] <- data.frame(
        analysis = id, job = .sr_an_s(e$job), started = .sr_an_s(e$started),
        ended = if (identical(oc, "interrupted")) "interrupted" else .sr_an_s(e$ended),
        outcome = oc, reason = .sr_an_s(e$reason), stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) {
    return(data.frame(analysis = character(), job = character(), started = character(), ended = character(),
                      outcome = character(), reason = character(), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
