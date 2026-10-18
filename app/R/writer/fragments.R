# writer/fragments.R - the Methods and Results prose of the manuscript, as templates over placeholders.
# A paragraph block is
#   list(type = "para", id, text, analysis, section = "methods" | "results")
# `text` is a template: no digit appears in it (rulings R1b and R14). Every number is an {{E}} token and
# every data-derived name or fixed constant that holds a digit ("SHA-256", "95%", "80%", "Table 1") is an
# {{L}} token whose source is label:<text>. The words of a sentence are the only fixed text, and no
# sentence says higher or lower: the direction of an effect is the claim guard's business (Task 6).
#
# Sources registered here (resolvers at the end of the file):
#   sap:locked_at | sap:sap_hash     the day of the SAP lock; the first 16 hex characters of its hash
#   feas:<analysis>:<field>          field n | events (counts) | h | mde | delta of sr_feasibility_fragment()
#   libsnap:r                        the version number of the R that wrote the manuscript

SR_WR_LABEL_TEXT <- c(primary = "Primary", secondary = "Secondary", sensitivity = "Sensitivity",
                      exploratory = "Exploratory", post_hoc = "Post hoc")

# The order of the analyses of a document: the main text first, then by label.
SR_WR_LABEL_RANK <- c(primary = 1L, secondary = 2L, exploratory = 3L, post_hoc = 4L, sensitivity = 5L)

# ---- blocks ---------------------------------------------------------------------------------------

.sr_wr_head <- function(text, level) list(type = "heading", text = text, level = as.integer(level))

.sr_wr_para <- function(id, text, analysis, section) {
  list(type = "para", id = id, text = text, analysis = as.character(analysis), section = section)
}

# ---- counts ---------------------------------------------------------------------------------------

# A count placeholder: a small cell (1 to SR_SDC_K - 1) is masked here, so no caller can print it.
.sr_wr_put_count <- function(reg, v, source, analysis) {
  sv <- sr_sdc_value(v)
  sr_ph_put(reg, sv$text, "count", source, analysis = analysis,
            value = if (sv$suppressed) NA_real_ else as.numeric(v), suppressed = sv$suppressed)
}

# ---- Methods --------------------------------------------------------------------------------------

# The sentence that says when the SAP was locked.
.sr_wr_m_lock <- function(project, reg) {
  lock <- project$state$sap$lock
  date <- sr_ph_put(reg, substr(as.character(lock$locked_at), 1L, 10L), "text", "sap:locked_at")
  hash <- sr_ph_put(reg, substr(as.character(lock$sap_hash), 1L, 16L), "text", "sap:sap_hash")
  paste0("The statistical analysis plan was locked on ", date, " (", .sr_wr_label(reg, "SHA-256"), " ", hash,
         ") before any outcome was analysed.")
}

# The citation tokens of a block of keys: " {{C:k}} {{C:k}}", or "" without a key.
.sr_wr_cite_tokens <- function(keys) {
  keys <- unique(as.character(keys))
  keys <- keys[!is.na(keys) & grepl("^[a-z][a-z0-9_]*$", keys)]
  if (!length(keys)) return("")
  paste0(" ", paste0("{{C:", keys, "}}", collapse = " "))
}

# The Methods paragraph of one analysis: its lead, each fragment of its result (numbers lifted into
# placeholders), then its citations (Task 5; absent until `.sr_cite_keys` exists).
.sr_wr_m_analysis <- function(project, id, res, label, reg, keys) {
  lead <- paste0(.sr_wr_label(reg, SR_WR_LABEL_TEXT[[label]]), " analysis ", .sr_wr_label(reg, id), ": ")
  parts <- character()
  for (nm in names(res$fragments)) {
    f <- res$fragments[[nm]]
    if (!(is.character(f) && length(f) == 1L && !is.na(f) && nzchar(trimws(f)))) next
    parts <- c(parts, sr_ph_lift(f, reg, paste0("fragment:", id, ":", nm), id))
  }
  body <- if (length(parts)) paste(parts, collapse = " ") else "the specification is given in the supplied analysis code."
  paste0(lead, body, .sr_wr_cite_tokens(keys))
}

# The citation keys of an analysis: list(methods, packages), empty until Task 5 defines `.sr_cite_keys`.
.sr_wr_keys <- function(project, id, res) {
  none <- list(methods = character(), packages = character())
  if (!exists(".sr_cite_keys", mode = "function")) return(none)
  a <- project$state$analyses[[id]]
  k <- get(".sr_cite_keys", mode = "function")(as.character(a$route), a$spec, res$effects)
  list(methods = as.character(k$methods %||% character()), packages = as.character(k$packages %||% character()))
}

# The feasibility sentence of a primary analysis with an assessment, or NULL when there is nothing to say
# (no assessment, the assessment is not applicable, or it has no precision).
.sr_wr_m_feas <- function(project, id, reg) {
  fr <- tryCatch(sr_feasibility_fragment(project, id), error = function(e) NULL)
  if (is.null(fr)) return(NULL)
  v <- fr$values
  if (!(is.finite(v$h) && is.finite(v$mde) && is.finite(v$n))) return(NULL)
  if (!is.character(v$tier) || length(v$tier) != 1L || is.na(v$tier) || identical(v$tier, "not_applicable")) return(NULL)
  src <- function(f) paste0("feas:", id, ":", f)
  num <- function(x, f) sr_ph_put(reg, sr_fmt_num(x), "num", src(f), analysis = id, value = x)
  events <- if (is.finite(v$events)) {
    paste0(", expected events ", .sr_wr_put_count(reg, v$events, src("events"), id))
  } else {
    ""
  }
  delta <- if (is.finite(v$delta)) {
    from <- if (is.character(v$delta_source) && length(v$delta_source) == 1L && !is.na(v$delta_source) &&
                nzchar(v$delta_source)) {
      paste0(" (", .sr_wr_label(reg, v$delta_source), ")")
    } else {
      ""
    }
    paste0(", against a clinically important difference of ", num(v$delta, "delta"), from)
  } else {
    ""
  }
  txt <- paste0("Before the plan was locked, the expected precision was assessed from the complete cases (n = ",
                .sr_wr_put_count(reg, v$n, src("n"), id), events, "): expected ", .sr_wr_label(reg, "95%"),
                " CI half-width ", num(v$h, "h"), " and minimum detectable effect at ", .sr_wr_label(reg, "80%"),
                " power ", num(v$mde, "mde"), delta, "; precision was rated ",
                .sr_wr_label(reg, gsub("_", " ", as.character(v$tier), fixed = TRUE)), ".")
  dec <- project$state$feasibility[[id]]$decision
  if (identical(dec$decision, "exploratory")) {
    why <- dec$reason
    why <- if (is.character(why) && length(why) == 1L && !is.na(why) && nzchar(why)) {
      paste0(" (", .sr_wr_label(reg, why), ")")
    } else {
      ""
    }
    txt <- paste0(txt, " Because of this, the analysis is reported as exploratory and hypothesis-generating", why, ".")
  }
  txt
}

# The sentence about a multiplicity family; `fa` is its sr_family_adjust() table.
.sr_wr_m_family <- function(fid, fa, reg) {
  paste0("P-values of family ", .sr_wr_label(reg, fid), " (", .sr_wr_label(reg, paste(unique(fa$analysis), collapse = ", ")),
         ") were adjusted by the ", .sr_wr_label(reg, fa$method[1L]), " method.")
}

# The sentence about the deviations from the locked plan (the table itself is in the supplement).
.sr_wr_m_deviations <- function(project) {
  if (nrow(sr_deviations(project))) {
    "Deviations from the locked plan are listed in the supplementary material."
  } else {
    "There were no deviations from the locked plan."
  }
}

# The software sentence; `packages` are the packages of the reported routes (Task 5), none until then.
.sr_wr_m_software <- function(reg, packages) {
  r <- sr_ph_put(reg, .sr_wr_r_version(), "text", "libsnap:r")
  packages <- sort(unique(as.character(packages)))
  with <- if (length(packages)) {
    paste0(" with the packages ", .sr_wr_label(reg, paste(packages, collapse = ", ")), ", each cited in the references;")
  } else {
    ";"
  }
  paste0("Analyses were run in R ", r, " {{C:r_core}}", with, " the exact code is supplied as analysis.R.")
}

.sr_wr_r_version <- function(r = sr_libsnap(character())$r) {
  sub("^R version ([0-9]+(\\.[0-9]+)*).*$", "\\1", as.character(r))
}

# ---- Results --------------------------------------------------------------------------------------

.sr_wr_adjective <- function(eff, i, has_cov) {
  marginal <- grepl("(marginal", as.character(eff$term[i]), fixed = TRUE) ||
    isTRUE(startsWith(as.character(eff$ci_method[i]), "delta_"))
  if (isTRUE(eff$conditional[i])) {
    "adjusted (conditional)"
  } else if (marginal) {
    "marginal (standardised)"
  } else if (has_cov) {
    "adjusted"
  } else {
    "unadjusted"
  }
}

# "; p = <token>" or "; p <token>" when the formatted p carries its own sign ("<0.001").
.sr_wr_p_clause <- function(lead, text, token) paste0(lead, if (grepl("^[<>]", text)) " " else " = ", token)

# The Results paragraph of an analysis: one sentence per primary-role effects row. `fa_of` is
# function(analysis record) -> list(fid, fa) for a member of a usable multiplicity family, else NULL.
.sr_wr_r_effects <- function(project, id, res, wording, reg, fa_of) {
  eff <- res$effects
  idx <- which(eff$role == "primary")
  if (!length(idx)) return(NULL)
  fam <- fa_of(project$state$analyses[[id]])
  has_cov <- any(eff$role == "covariate")
  first_measure <- eff$measure[idx[1L]]
  lead <- if (identical(wording, "exploratory")) {
    paste0("In exploratory, hypothesis-generating analysis ", .sr_wr_label(reg, id), ", ")
  } else {
    paste0("In analysis ", .sr_wr_label(reg, id), ", ")
  }
  out <- character()
  for (i in idx) {
    scale <- eff$scale[i]
    est <- sr_ph_put(reg, .sr_wr_eff_text(project, id, i, "est"), "est", sprintf("effect:%s:%d:est", id, i),
                     analysis = id, value = eff$est[i], scale = scale, null = .sr_wr_null_of(scale))
    ci <- sr_ph_put(reg, .sr_wr_eff_text(project, id, i, "lo_hi"), "ci", sprintf("effect:%s:%d:lo_hi", id, i),
                    analysis = id)
    cm <- as.character(eff$ci_method[i])
    ci_name <- if (!is.na(cm) && startsWith(cm, "boot_")) paste0("bootstrap CI (", .sr_wr_label(reg, cm), ")") else "CI"
    clause <- ""
    if (is.finite(eff$p[i])) {
      ptxt <- .sr_wr_eff_text(project, id, i, "p")
      ptok <- sr_ph_put(reg, ptxt, "p", sprintf("effect:%s:%d:p", id, i), analysis = id, value = eff$p[i])
      clause <- .sr_wr_p_clause("; p", ptxt, ptok)
      if (!is.null(fam) && identical(eff$measure[i], first_measure)) {
        fa <- fam$fa
        r <- which(fa$analysis == id & fa$term == eff$term[i])
        if (length(r) && !is.na(fa$p_adj[r[1L]])) {
          r <- r[1L]
          atxt <- sr_fmt_p(fa$p_adj[r])
          atok <- sr_ph_put(reg, atxt, "p", sprintf("family:%s:%d:p_adj", fam$fid, r), analysis = id,
                            value = fa$p_adj[r])
          clause <- paste0(clause, .sr_wr_p_clause(", adjusted p", atxt, atok))
        }
      }
    }
    mn <- unname(SR_WR_MEASURE_NAMES[as.character(eff$measure[i])])
    if (is.na(mn)) mn <- as.character(eff$measure[i])
    out <- c(out, paste0(lead, "the ", .sr_wr_adjective(eff, i, has_cov), " ", .sr_wr_label(reg, tolower(mn)),
                         " for ", .sr_wr_label(reg, as.character(eff$term[i])), " was ", est, " (",
                         .sr_wr_label(reg, "95%"), " ", ci_name, " ", ci, ")", clause, "."))
  }
  paste(out, collapse = " ")
}

# The Table 1 sentence of a table1 analysis.
.sr_wr_r_table1 <- function(project, id, reg) {
  by <- as.character(unlist(project$state$analyses[[id]]$spec$by))
  by <- by[!is.na(by) & nzchar(by)]
  paste0("Baseline characteristics", if (length(by)) paste0(" by ", .sr_wr_label(reg, paste(by, collapse = ", "))) else "",
         " are shown in ", .sr_wr_label(reg, "Table 1"), ".")
}

# "Not reported: ..." for the analyses with no ok run, and the families whose adjusted p values could not
# be computed for that reason.
.sr_wr_r_not_reported <- function(nr, dropped, reg) {
  if (!nrow(nr)) return(NULL)
  items <- paste0(nr$id, " (", nr$label, ", ", nr$reason, ")")
  txt <- paste0("Not reported: ", .sr_wr_label(reg, paste(items, collapse = ", ")), ".")
  for (fid in dropped) {
    txt <- paste0(txt, " Adjusted p values of family ", .sr_wr_label(reg, fid),
                  " are not reported because a member of the family was not run.")
  }
  txt
}

# ---- resolvers ------------------------------------------------------------------------------------

.sr_ph_resolve_sap <- function(project, parts, kind) {
  lock <- project$state$sap$lock
  if (!isTRUE(project$state$sap$locked) || is.null(lock)) stop("the SAP is not locked", call. = FALSE)
  switch(paste(parts, collapse = ":"),
    locked_at = substr(as.character(lock$locked_at), 1L, 10L),
    sap_hash = substr(as.character(lock$sap_hash), 1L, 16L),
    stop("unknown sap field '", paste(parts, collapse = ":"), "'", call. = FALSE))
}

.sr_ph_resolve_feas <- function(project, parts, kind) {
  if (length(parts) != 2L) stop("a feasibility source is feas:<analysis>:<field>", call. = FALSE)
  v <- sr_feasibility_fragment(project, parts[1L])$values
  whole <- function(x) sprintf("%d", as.integer(round(x)))
  switch(parts[2L],
    n = whole(v$n),
    events = whole(v$events),
    h = sr_fmt_num(v$h),
    mde = sr_fmt_num(v$mde),
    delta = sr_fmt_num(v$delta),
    stop("unknown feasibility field '", parts[2L], "'", call. = FALSE))
}

.sr_ph_resolve_libsnap <- function(project, parts, kind) {
  if (!identical(parts, "r")) stop("a library source is libsnap:r", call. = FALSE)
  .sr_wr_r_version()
}

.sr_ph_register_resolver("sap", .sr_ph_resolve_sap)
.sr_ph_register_resolver("feas", .sr_ph_resolve_feas)
.sr_ph_register_resolver("libsnap", .sr_ph_resolve_libsnap)
