# archive/baseline.R - the library baseline gate and the explicit, logged re-baseline.
#
#   sr_lib_baseline(project)                 state$lib_baseline (hash, snap, from, label) or NULL
#   sr_rebaseline_diff(project, snap)        what changed between the baseline and the library now
#   sr_rebaseline(project, reason, snap)     accept the current library as the new baseline (logged)
#   .sr_baseline_check(project, lib_hash)    the gate sr_run calls: stops when the library differs
#
# The gate compares hashes only and never recomputes one from the stored snapshot: after an event replay
# the stored pkgs is a list, not a named character vector.

# A snapshot field as a named character vector; NULL and NA entries are NA (absent).
.sr_libsnap_items <- function(snap) {
  one <- function(x) {
    x <- unlist(x, use.names = FALSE)
    if (is.null(x) || !length(x)) NA_character_ else as.character(x)[1L]
  }
  p <- snap$pkgs
  pk <- if (length(p)) vapply(p, one, "", USE.NAMES = FALSE) else character()
  names(pk) <- names(p) %||% character()
  c(c(R = one(snap$r), snapshot_date = one(snap$snapshot_date)), pk)
}

#' Differences between two library snapshots.
#' @return data.frame(item, from, to, change), sorted by item; zero rows when equal.
.sr_libsnap_diff <- function(from, to) {
  f <- .sr_libsnap_items(from)
  t <- .sr_libsnap_items(to)
  items <- sort(union(names(f), names(t)), method = "radix")
  fv <- unname(f[items])
  tv <- unname(t[items])
  fv[is.na(fv)] <- NA_character_
  tv[is.na(tv)] <- NA_character_
  differ <- !(is.na(fv) & is.na(tv)) & (is.na(fv) != is.na(tv) | (!is.na(fv) & !is.na(tv) & fv != tv))
  change <- ifelse(is.na(fv), "added", ifelse(is.na(tv), "removed", "updated"))
  k <- which(differ)
  data.frame(item = items[k], from = fv[k], to = tv[k], change = change[k], stringsAsFactors = FALSE)
}

sr_lib_baseline <- function(project) project$state$lib_baseline

sr_rebaseline_diff <- function(project, snap = sr_libsnap()) {
  b <- sr_lib_baseline(project)
  if (is.null(b)) stop("no freeze yet: there is no library baseline", call. = FALSE)
  .sr_libsnap_diff(b$snap, snap)
}

sr_reducer_register("freeze.rebaselined", function(state, event) {
  p <- event$payload
  state$lib_baseline <- list(hash = p$to_hash, snap = p$to_snap, from = "rebaseline", label = p$label)
  state$rebaselines <- c(state$rebaselines %||% list(), list(p))
  state
})

#' Accept the current library as the new baseline, with a reason. Refused for a read-only project, when
#' there is no freeze, for an empty reason, and when the library already matches the baseline.
sr_rebaseline <- function(project, reason, snap = sr_libsnap()) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  b <- sr_lib_baseline(project)
  if (is.null(b)) stop("no freeze yet: there is no library baseline", call. = FALSE)
  if (!is.character(reason) || length(reason) != 1L || is.na(reason) || !nzchar(trimws(reason))) {
    stop("a reason is required", call. = FALSE)
  }
  to_hash <- sr_libsnap_hash(snap)
  if (identical(to_hash, b$hash)) {
    stop("the library matches the baseline; nothing to re-baseline", call. = FALSE)
  }
  d <- .sr_libsnap_diff(b$snap, snap)
  sr_project_append(project, "freeze.rebaselined", list(
    from_hash = b$hash, to_hash = to_hash, to_snap = .sr_libsnap_json(snap),
    diff = unname(lapply(seq_len(nrow(d)), function(i) as.list(d[i, ]))),
    reason = trimws(reason), label = b$label))
}

.sr_baseline_check <- function(project, lib_hash) {
  b <- sr_lib_baseline(project)
  if (is.null(b) || identical(lib_hash, b$hash)) return(invisible(TRUE))
  n <- nrow(.sr_libsnap_diff(b$snap, sr_libsnap()))
  stop("the library differs from the baseline of freeze '", b$label, "' (", n, " change(s)); ",
       "review sr_rebaseline_diff() and call sr_rebaseline() with a reason before running", call. = FALSE)
}
