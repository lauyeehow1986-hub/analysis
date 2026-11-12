# writer/sapdoc.R - the statistical analysis plan and the feasibility report as Word files.
#
#   sr_sap_docx(project, path = NULL)          out/sap/SAP_v1.docx (locked) or SAP_draft.docx (unlocked)
#   sr_feasibility_docx(project, path = NULL)  out/feasibility/feasibility-<NNN>.docx
#
# Both build a small document model (heading / para / table blocks) and hand it to sr_docx_render(). The text
# is plain: these documents hold no prose claim, so no placeholder is used and the claim guard is not run.
# The numbers are the pre-lock feasibility numbers (exposure-blind marginals, spec O) and counts of the
# specification; nothing in either document is an effect estimate. Each call appends a doc.exported event
# (kind "sap" or "feasibility").

# ---- text helpers -------------------------------------------------------------------------------------

# A cell or sentence as one clean string: NA is "", and no placeholder-looking token survives (a data-derived
# text such as a decision reason is never read as a template).
.sr_sd_txt <- function(x) {
  x <- paste(as.character(unlist(x, use.names = FALSE)), collapse = ", ")
  if (is.na(x)) x <- ""
  gsub("}}", "} }", gsub("{{", "{ {", x, fixed = TRUE), fixed = TRUE)
}

.sr_sd_col <- function(x) vapply(x, .sr_sd_txt, "", USE.NAMES = FALSE)

.sr_sd_num <- function(x) vapply(x, function(v) if (is.finite(v)) sr_fmt_num(v) else "", "", USE.NAMES = FALSE)

.sr_sd_whole <- function(x) vapply(x, function(v) if (is.finite(v)) sprintf("%d", as.integer(round(v))) else "", "", USE.NAMES = FALSE)

.sr_sd_count <- function(x, k) vapply(x, function(v) if (is.finite(v)) sr_sdc_value(v, k = k)$text else "", "", USE.NAMES = FALSE)

# The exposure prevalence (n_k / n_cc) is printed beside the visible n (complete cases), so it gives n_k back:
# hidden as "<k" where n_k = prevalence * n_cc is 1 to k - 1.
.sr_sd_prev <- function(prev, n_cc, k) {
  mapply(function(p, n) {
    if (!is.finite(p)) return("")
    nk <- if (is.finite(n)) round(p * n) else NA_real_
    if (is.finite(nk) && nk >= 1 && nk <= k - 1L) .sr_sdc_text(k) else sr_fmt_num(p)
  }, prev, n_cc, USE.NAMES = FALSE)
}

.sr_sd_df <- function(...) {
  cols <- list(...)
  as.data.frame(lapply(cols, .sr_sd_col), stringsAsFactors = FALSE, check.names = FALSE)
}

.sr_sd_head <- function(text, level = 1L) list(type = "heading", text = .sr_sd_txt(text), level = as.integer(level))

.sr_sd_para <- function(text) list(type = "para", id = "sapdoc", text = .sr_sd_txt(text), analysis = NA_character_, section = "methods")

.sr_sd_table <- function(df, caption) {
  list(type = "table", id = "sapdoc", caption = .sr_sd_txt(caption), df = df, footnotes = character(), where = "main")
}

# A model that sr_docx_render() accepts: only the main part is used.
.sr_sd_model <- function(blocks) {
  list(meta = list(preset = "default"), reg = sr_ph_registry(), main = blocks, supplement = list())
}

# ---- the files ----------------------------------------------------------------------------------------

.sr_sd_norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

# The path of a file relative to the project folder; a file outside the folder is recorded by its name only.
.sr_sd_rel <- function(project, path) {
  root <- .sr_sd_norm(project$paths$dir)
  full <- .sr_sd_norm(path)
  pre <- paste0(sub("/+$", "", root), "/")
  if (startsWith(full, pre)) substring(full, nchar(pre) + 1L) else basename(full)
}

.sr_sd_check_k <- function(k) {
  if (!(is.numeric(k) && length(k) == 1L && is.finite(k) && k >= SR_SDC_K && k == round(k))) {
    stop("`suppress_k` must be a whole number of at least ", SR_SDC_K, " (the small-cell floor SR_SDC_K)", call. = FALSE)
  }
  as.integer(k)
}

.sr_sd_check_path <- function(path) {
  if (!(is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path))) {
    stop("`path` must be NULL or one file name", call. = FALSE)
  }
  invisible(path)
}

# Render the model, then append the doc.exported event; the project comes back with it applied.
.sr_sd_write <- function(project, blocks, path, kind, id, sap_hash, title, footer = NULL) {
  made <- !file.exists(path)
  tip <- sr_audit_tip(project$paths$audit)
  props <- list(audit_tip = tip, sap_hash = sap_hash, export_id = id, created = .sr_now(), title = title)
  if (!is.null(footer)) props$footer <- footer
  fig_dir <- file.path(dirname(path), paste0(".figures-", id))
  # a render that fails after creating the file leaves no unrecorded file behind (it would block the retry)
  tryCatch(
    sr_docx_render(.sr_sd_model(blocks), "main", path, fig_dir, props),
    error = function(e) {
      if (made) unlink(path)
      unlink(fig_dir, recursive = TRUE)
      stop(e)
    })
  unlink(fig_dir, recursive = TRUE)
  files <- list(list(path = .sr_sd_rel(project, path), sha = .sr_sha256(.sr_read_bytes(path))))
  # a failed append leaves no unrecorded file behind (it would block the retry): remove it, re-raise the condition
  tryCatch(
    sr_project_append(project, "doc.exported",
                      list(kind = kind, id = id, files = files, audit_tip = tip, sap_hash = sap_hash)),
    error = function(e) {
      if (made) unlink(path)
      stop(e)
    })
}

# ---- the SAP ------------------------------------------------------------------------------------------

# The five fields of the estimand of a route for a spec: a fixed estimand or a function of the spec.
.sr_sd_estimand <- function(a) {
  none <- c(population = "", variables = "", intercurrent = "", summary = "", level = "")
  r <- tryCatch(sr_route_get(as.character(a$route), a$route_version), error = function(e) NULL)
  if (is.null(r)) return(none)
  e <- tryCatch(if (is.function(r$estimand)) r$estimand(a$spec) else r$estimand, error = function(e) NULL)
  if (is.null(e)) return(none)
  c(population = .sr_sd_txt(e[["population"]]), variables = .sr_sd_txt(e[["variables"]]),
    intercurrent = .sr_sd_txt(e[["intercurrent"]]), summary = .sr_sd_txt(e[["summary"]]),
    level = .sr_sd_txt(e[["level"]]))
}

.sr_sd_outcome <- function(spec) {
  if (!is.null(spec[["outcome"]])) return(.sr_sd_txt(spec[["outcome"]]))
  tm <- .sr_spec_chr(spec[["time"]])
  st <- .sr_spec_chr(spec[["status"]])
  if (length(tm) || length(st)) {
    return(paste0("time: ", paste(tm, collapse = ", "), "; status: ", paste(st, collapse = ", ")))
  }
  ""
}

.sr_sd_terms <- function(spec) {
  tt <- spec[["terms"]]
  if (!length(tt)) return("")
  paste(vapply(tt, function(t) paste0(.sr_sd_txt(t[["var"]]), ": ", .sr_sd_txt(t[["type"]] %||% "linear")), ""),
        collapse = "; ")
}

# One analysis: a two-column table of its fields.
.sr_sd_analysis_blocks <- function(a) {
  spec <- a$spec
  est <- .sr_sd_estimand(a)
  r <- tryCatch(sr_route_get(as.character(a$route), a$route_version), error = function(e) NULL)
  fam <- if (is.null(a$family) || is.na(a$family)) "" else a$family
  df <- .sr_sd_df(
    Field = c("Id", "Label", "Route", "Route version", "Purpose", "Table", "Outcome", "Exposure", "Terms",
              "Estimand: population", "Estimand: variables", "Estimand: intercurrent events", "Estimand: summary",
              "Estimand: level", "Family"),
    Value = c(a$id, a$label, a$route, a$route_version, if (is.null(r)) "" else r$purpose, a$table,
              .sr_sd_outcome(spec), spec[["exposure"]] %||% "", .sr_sd_terms(spec),
              est[["population"]], est[["variables"]], est[["intercurrent"]], est[["summary"]], est[["level"]], fam))
  # the fitting method of a cox analysis that stores one (the default "exact" is never stored, so such a SAP
  # reads as before P1e.2)
  if (identical(as.character(a$route), "cox") && !is.null(spec[["cox_method"]])) {
    df <- rbind(df, .sr_sd_df(Field = "Method", Value = .sr_cox_method_text(spec)))
  }
  list(.sr_sd_head(paste("Analysis", a$id), 2L), .sr_sd_table(df, paste("Specification of", a$id)))
}

# The engine of each analysis (scale/engine.R): the locked one after the lock, the one resolved now in a draft.
# One row per analysis with an engine record, in id order; the resolution reason of each is a footnote. NULL
# when no analysis has a record (a SAP locked before P1e), so such a document is unchanged.
.sr_sd_engine_table <- function(project) {
  eg <- .sr_sap_engines(project)
  an <- .sr_sap_analyses(project)
  ids <- sort(as.character(intersect(names(eg), names(an))), method = "radix")
  if (!length(ids)) return(NULL)
  name <- function(e) switch(as.character(e$engine %||% NA_character_), memory = "In memory", stream = "Streaming", "Not resolved")
  df <- .sr_sd_df(Analysis = ids, Label = vapply(ids, function(i) as.character(an[[i]]$label %||% ""), ""),
                  Route = vapply(ids, function(i) as.character(an[[i]]$route %||% ""), ""),
                  Engine = vapply(ids, function(i) name(eg[[i]]), ""))
  notes <- vapply(ids, function(i) .sr_sd_txt(paste0(i, ": ", eg[[i]]$reason %||% "")), "")
  tb <- .sr_sd_table(df, "Computational engine of each analysis")
  tb$footnotes <- unname(notes)
  tb
}

.sr_sd_feas_table <- function(rep, k = SR_SDC_K) {
  .sr_sd_df(Analysis = rep$analysis, Contrast = rep$contrast, Measure = rep$measure, `n (complete cases)` = .sr_sd_whole(rep$n_cc),
            Events = .sr_sd_count(rep$events, k), `Half-width` = .sr_sd_num(rep$h), `Min. detectable effect` = .sr_sd_num(rep$mde),
            Delta = .sr_sd_num(rep$delta), Tier = rep$tier, Decision = rep$decision, Reason = rep$reason)
}

.sr_sd_sap_blocks <- function(project, locked, k = SR_SDC_K) {
  lock <- project$state$sap$lock
  hash <- if (locked) as.character(lock$sap_hash) else sr_sap_hash(project)
  blocks <- list(.sr_sd_head(if (locked) "Statistical analysis plan v1 (locked)" else "Statistical analysis plan (DRAFT, not locked)", 1L))
  status <- if (locked) {
    paste0("Status: locked. SAP hash: ", hash, ". Locked at: ", as.character(lock$locked_at),
           ". Library snapshot hash: ", as.character(lock$libsnap_hash), ".")
  } else {
    paste0("Status: DRAFT, not locked. Current SAP hash (it changes with every edit): ", hash, ".")
  }
  blocks <- c(blocks, list(.sr_sd_para(status)))

  tabs <- lock$tables
  if (locked && length(tabs)) {
    nm <- sort(names(tabs), method = "radix")
    blocks <- c(blocks, list(.sr_sd_head("Tables", 1L),
      .sr_sd_table(.sr_sd_df(Table = nm, `Store hash (first 16)` = vapply(nm, function(t) substr(as.character(tabs[[t]]$store_hash), 1L, 16L), "")),
                   "Analysed tables and their store hashes at the lock")))
  }

  an <- .sr_sap_analyses(project)
  an <- an[order(names(an), method = "radix")]
  blocks <- c(blocks, list(.sr_sd_head("Analyses", 1L)))
  eng <- .sr_sd_engine_table(project)
  if (!is.null(eng)) blocks <- c(blocks, list(eng))
  for (a in an) blocks <- c(blocks, .sr_sd_analysis_blocks(a))

  fams <- project$state$sap$families %||% list()
  if (length(fams)) {
    fams <- fams[order(names(fams), method = "radix")]
    blocks <- c(blocks, list(.sr_sd_head("Families", 1L), .sr_sd_table(
      .sr_sd_df(Family = vapply(fams, function(f) f$id, ""), Members = vapply(fams, function(f) paste(f$members, collapse = ", "), ""),
                Method = vapply(fams, function(f) f$method, "")), "Multiplicity families")))
  }
  rules <- project$state$sap$rules %||% list()
  if (length(rules)) {
    rules <- rules[order(names(rules), method = "radix")]
    chr <- function(f) vapply(rules, function(r) as.character(r[[f]]), "")
    blocks <- c(blocks, list(.sr_sd_head("Rules", 1L), .sr_sd_table(
      .sr_sd_df(Rule = chr("id"), Analysis = chr("analysis"), Check = chr("check"), Action = chr("action"),
                Alternative = chr("alternative")), "Pre-specified responses to diagnostics")))
  }

  rep <- sr_feasibility_report(project)
  if (nrow(rep)) {
    blocks <- c(blocks, list(.sr_sd_head("Feasibility", 1L), .sr_sd_table(.sr_sd_feas_table(rep, k), "Feasibility assessment before the lock")))
  }
  chk <- sr_sap_check(project)
  blocks <- c(blocks, list(.sr_sd_head("Lock check", 1L),
                           .sr_sd_table(.sr_sd_df(Item = chk$item, Status = chk$status, Detail = chk$detail), "What the lock checks")))
  list(blocks = blocks, hash = hash)
}

#' Write the statistical analysis plan as a Word file.
#'
#' A locked SAP goes to out/sap/SAP_v1.docx and is written once: a second call refuses. An unlocked one goes to
#' out/sap/SAP_draft.docx, titled DRAFT, with DRAFT in every page footer, and is overwritten freely. The content
#' is the SAP hash, lock time and library hash, the analysed tables, every analysis with its estimand, the
#' families, the rules, the feasibility table and the lock check; numbers are plain text. A doc.exported event
#' (kind "sap", id "SAP_v1" or "SAP_draft") is appended.
#' @param project an open, writable project with at least one analysis in its SAP.
#' @param path the file to write, or NULL for the default.
#' @param suppress_k the small-cell threshold: an Events count of 1 to suppress_k - 1 prints as "<k" (the
#'   complete-case n is a sample size and stays visible).
#' @return the project with the doc.exported event applied.
sr_sap_docx <- function(project, path = NULL, suppress_k = SR_SDC_K) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  k <- .sr_sd_check_k(suppress_k)
  locked <- isTRUE(project$state$sap$locked) && !is.null(project$state$sap$lock)
  id <- if (locked) "SAP_v1" else "SAP_draft"
  default <- file.path(project$paths$out, "sap", paste0(id, ".docx"))
  if (is.null(path)) path <- default else .sr_sd_check_path(path)
  # the id is also refused when the event log already holds it, whatever path was given
  done <- any(vapply(project$state$exports %||% list(), function(e) identical(e$id, "SAP_v1"), NA))
  if (locked && (done || file.exists(default) || file.exists(path))) {
    stop("the locked SAP was already exported (", file.path("out", "sap", "SAP_v1.docx"), "); it is never overwritten", call. = FALSE)
  }
  if (!length(.sr_sap_analyses(project))) stop("the SAP has no analysis to write", call. = FALSE)
  built <- .sr_sd_sap_blocks(project, locked, k)
  title <- if (locked) "Statistical analysis plan v1 (locked)" else "Statistical analysis plan (DRAFT, not locked)"
  .sr_sd_write(project, built$blocks, path, "sap", id, built$hash, title, footer = if (locked) NULL else "DRAFT")
}

# ---- the feasibility report -----------------------------------------------------------------------------

# The feasibility sentence of the manuscript (writer/fragments.R) resolved through a fresh registry, as plain
# text; a clustered analysis gets the numbered citation and its reference line.
.sr_sd_feas_sentence <- function(project, id, k = SR_SDC_K) {
  reg <- sr_ph_registry(k = k)
  txt <- .sr_wr_m_feas(project, id, reg)
  if (is.null(txt)) return(list(text = "No precision statement: the assessment has no computed precision.", ref = NULL))
  clustered <- .sr_wr_clustered(project, id)
  res <- sr_ph_resolve(txt, reg, cite = sr_cite_fn("eldridge2006"))
  ref <- if (clustered) sr_cite_vancouver(sr_cite_csl("eldridge2006")) else NULL
  list(text = res, ref = if (is.null(ref)) NULL else paste0("[1] ", sub("^1[.]? *", "", ref[1L])))
}

#' Write the feasibility assessment as a Word file, out/feasibility/feasibility-<NNN>.docx.
#'
#' One section per assessed analysis: the feasibility sentence of the Methods, a table of its contrast rows,
#' the blockers and warnings, the decision and reason, whether the assessment is stale, and the cluster-trial
#' citation when the analysis is clustered. Allowed before and after the lock. A doc.exported event (kind
#' "feasibility") is appended. An existing file is never overwritten.
#' @param project an open, writable project with feasibility assessments.
#' @param path the file to write, or NULL for the default.
#' @param suppress_k the small-cell threshold, as for sr_sap_docx(): it also reaches the feasibility sentence.
#' @return the project with the doc.exported event applied.
sr_feasibility_docx <- function(project, path = NULL, suppress_k = SR_SDC_K) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  k <- .sr_sd_check_k(suppress_k)
  ex <- project$state$exports %||% list()
  n_prev <- sum(vapply(ex, function(e) identical(e$kind, "feasibility"), NA))
  id <- sprintf("feasibility-%03d", n_prev + 1L)
  if (is.null(path)) path <- file.path(project$paths$out, "feasibility", paste0(id, ".docx")) else .sr_sd_check_path(path)
  if (file.exists(path)) stop("the feasibility report ", basename(path), " already exists; it is never overwritten", call. = FALSE)
  rep <- sr_feasibility_report(project)
  if (!nrow(rep)) stop("no analysis has a feasibility assessment", call. = FALSE)

  blocks <- list(.sr_sd_head("Feasibility report", 1L))
  for (a in unique(rep$analysis)) {
    r <- rep[rep$analysis == a, , drop = FALSE]
    sent <- .sr_sd_feas_sentence(project, a, k)
    blocks <- c(blocks, list(.sr_sd_head(paste0(a, " (", r$label[1L], ", ", r$route[1L], ")"), 2L), .sr_sd_para(sent$text)))
    if (nrow(r) && any(!is.na(r$contrast))) {
      df <- .sr_sd_df(Contrast = r$contrast, Measure = r$measure, `n (complete cases)` = .sr_sd_whole(r$n_cc),
                      Events = .sr_sd_count(r$events, k), Prevalence = .sr_sd_prev(r$prevalence, r$n_cc, k), `Half-width` = .sr_sd_num(r$h),
                      `Min. detectable effect` = .sr_sd_num(r$mde), `Upper SE` = .sr_sd_num(r$se_upper),
                      Delta = .sr_sd_num(r$delta), Tier = r$tier)
      blocks <- c(blocks, list(.sr_sd_table(df, paste("Feasibility of", a, "by contrast"))))
    }
    txt <- function(x) if (is.na(x) || !nzchar(x)) "none" else x
    blocks <- c(blocks, list(
      .sr_sd_para(paste0("Blockers: ", txt(r$blockers[1L]))),
      .sr_sd_para(paste0("Warnings: ", txt(r$warnings[1L]))),
      .sr_sd_para(paste0("Decision: ", txt(r$decision[1L]), ". Reason: ", txt(r$reason[1L]), ".")),
      .sr_sd_para(paste0("Stale: ", if (is.na(r$stale[1L])) "no" else paste0("yes (the ", r$stale[1L], " changed)"), "."))))
    if (!is.null(sent$ref)) blocks <- c(blocks, list(.sr_sd_para(sent$ref)))
  }
  locked <- isTRUE(project$state$sap$locked) && !is.null(project$state$sap$lock)
  hash <- if (locked) as.character(project$state$sap$lock$sap_hash) else sr_sap_hash(project)
  .sr_sd_write(project, blocks, path, "feasibility", id, hash, "Feasibility report")
}
