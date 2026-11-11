# ui/archive_view.R - the pure view helpers behind the Archive panel (no Shiny reactivity, no side effects).
#
# Prefix .sr_ar_. Each helper formats what the archive core reports; reasons copy the core's message text and
# no number is computed here (display rounding only).

# ---- freeze gates ----------------------------------------------------------------------------------

# One row per refusal of .sr_freeze_gate, all evaluated (the gate itself stops at the first failure):
# data.frame(gate, ok, reason); `reason` is "" when ok.
.sr_ar_freeze_gates <- function(p) {
  mk <- function(gate, ok, reason) {
    data.frame(gate = gate, ok = isTRUE(ok), reason = if (isTRUE(ok)) "" else reason, stringsAsFactors = FALSE)
  }
  ids_txt <- function(ids) paste0("'", ids, "'", collapse = ", ")
  runs <- p$state$runs %||% list()
  an <- .sr_sap_analyses(p)
  prim <- names(an)[vapply(an, function(a) identical(a$label, "primary"), NA)]
  no_run <- prim[vapply(prim, function(id) is.na(.sr_freeze_chr1(runs[[id]]$ok_key)), NA)]
  need <- sr_tables_needing_hash(p)
  stored <- character()
  for (id in names(runs)) {
    key <- .sr_freeze_chr1(runs[[id]]$ok_key)
    if (is.na(key)) next
    sha <- .sr_freeze_chr1(runs[[id]]$ok_extract_sha)
    if (!file.exists(file.path(p$paths$fits, paste0(key, ".rds")))) {
      stored <- c(stored, paste0("the stored result of '", id, "' is missing; run it again"))
    } else if (is.na(sha) || !file.exists(file.path(p$paths$dir, "data", "extracts", paste0(sha, ".rds")))) {
      stored <- c(stored, paste0("the extract of '", id, "' is missing; run it again"))
    }
  }
  v <- sr_project_verify(p)
  v_ok <- isTRUE(v$ok) && is.null(v$unaudited)
  rbind(
    mk("Project writable", !isTRUE(p$readonly), "project is read-only"),
    mk("SAP locked", !sr_blind(p), "the SAP is not locked; lock it before freezing"),
    mk("Sources hashed", !length(need),
       paste0("the full SHA-256 of the source of table(s) ", ids_txt(need),
              " is missing; run sr_source_hash() first")),
    mk("Primary analyses run", !length(no_run),
       paste0("primary analysis ", ids_txt(no_run), " has no successful run")),
    mk("Stored results present", !length(stored), paste(stored, collapse = "; ")),
    mk("Project verifies", v_ok,
       paste0("the project does not verify: ",
              paste(c(v$problems, if (!is.null(v$unaudited)) "an event has no audit entry"), collapse = "; ")))
  )
}

# NULL, or why `label` cannot be a new freeze label.
.sr_ar_label_problem <- function(p, label) {
  if (!is.character(label) || length(label) != 1L || is.na(label) || !grepl(SR_FREEZE_LABEL_RE, label)) {
    return(paste0("the label must match ", SR_FREEZE_LABEL_RE))
  }
  if (label %in% names(p$state$freezes) || file.exists(.sr_freeze_dir(p, label))) {
    return(paste0("freeze label '", label, "' is already used"))
  }
  NULL
}

# ---- freezes, verify, diff -------------------------------------------------------------------------

# The freezes table: data.frame(label, created, events_seq, libsnap) with the library hash cut to 12 characters.
.sr_ar_freezes <- function(p) {
  f <- sr_freezes(p)
  data.frame(label = f$label, created = f$created, events_seq = f$events_seq,
             libsnap = substr(f$libsnap_hash, 1L, 12L), stringsAsFactors = FALSE)
}

# list(ok, problems, absent) of a verify result; absent data files are information, not problems.
.sr_ar_verify_rows <- function(v) {
  list(ok = isTRUE(v$ok), problems = as.character(v$problems), absent = as.character(v$absent))
}

# The non-empty sections of sr_freeze_diff as a named list of data frames (tables, library, sap, analyses).
.sr_ar_diff_tables <- function(d) {
  out <- list()
  if (NROW(d$tables)) out$tables <- d$tables
  if (NROW(d$library)) out$library <- d$library
  if (isTRUE(d$sap$changed)) {
    out$sap <- data.frame(item = "SAP hash", from = as.character(d$sap$from)[1L], to = as.character(d$sap$to)[1L],
                          stringsAsFactors = FALSE)
  }
  if (NROW(d$analyses)) out$analyses <- d$analyses
  out
}

# sr_freeze_results_diff with its numeric columns shown to 4 significant digits.
.sr_ar_results_diff <- function(r) {
  for (n in names(r)) {
    if (is.double(r[[n]])) r[[n]] <- signif(r[[n]], 4L)
  }
  r
}

# ---- export ----------------------------------------------------------------------------------------

# What an export would do: list(mode, bytes, absent). Same rule as sr_srproj_export ("auto" follows the size
# limit and any data file missing from the folder).
.sr_ar_srproj_plan <- function(p, label, data) {
  r <- .sr_srproj_mode(p, label, data)
  if (is.na(r$bytes)) {
    st <- .sr_srproj_data_stats(p, label)
    r$bytes <- st$bytes
    r$absent <- st$absent
  }
  r
}

# NULL, or why `path` cannot be the archive of an export.
.sr_ar_path_problem <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !grepl("[.]srproj$", path, ignore.case = TRUE)) {
    return("the path must end in .srproj")
  }
  if (file.exists(path) || file.exists(paste0(path, ".partial"))) return(paste0(path, " already exists"))
  parent <- dirname(path)
  if (!dir.exists(parent)) return(paste0("the folder ", parent, " does not exist"))
  NULL
}

# NULL when sr.user / SR_USER is set; else the warning that the actor would be the OS user name.
.sr_ar_actor_warning <- function() {
  if (!identical(sr_opt_source("user"), "default")) return(NULL)
  SR_SRPROJ_ACTOR_WARNING
}
