# archive/diff.R - what changed between two freezes (or a freeze and the project now).
#
#   sr_freeze_diff(project, from, to = NULL)      list(tables, library, sap, analyses)
#   .sr_freeze_now(project)                       a manifest-shaped list of the current state (writes nothing)
#   sr_freeze_results_diff(project, label)        frozen vs current stored result of each analysis
#   .sr_fz_numbers(x)                             every numeric leaf of x as a named double vector
#
# The diff reads frozen files and stored fits only. It never recomputes a number.

.sr_fz_numbers <- function(x) {
  rapply(list(x), as.numeric, classes = c("numeric", "integer"), how = "unlist")
}

#' The current project as a manifest-shaped list: tables, runs, libsnap, libsnap_hash, sap_hash.
.sr_freeze_now <- function(project) {
  tables <- list()
  for (t in names(project$state$tables %||% list())) {
    m <- if (file.exists(.sr_store_manifest_path(project, t))) sr_store_manifest(project, t) else NULL
    re <- project$state$reimports[[t]]
    tables[[t]] <- list(
      store_hash = if (is.null(m)) NA_character_ else sr_store_hash(m),
      rows = if (is.null(m)) 0 else sum(vapply(m$parts, function(p) p$rows, 0)),
      base = as.character(re$table %||% t), version = as.integer(re$version %||% 0L))
  }
  runs <- project$state$runs %||% list()
  runl <- list()
  for (id in names(runs)) {
    key <- .sr_freeze_chr1(runs[[id]]$ok_key)
    if (is.na(key)) next
    runl[[id]] <- list(label = id, ok_key = key, ok_extract_sha = .sr_freeze_chr1(runs[[id]]$ok_extract_sha),
                       ok_lib_hash = .sr_freeze_chr1(runs[[id]]$ok_lib_hash))
  }
  snap <- sr_libsnap()
  list(tables = tables, runs = runl, libsnap = .sr_libsnap_json(snap), libsnap_hash = sr_libsnap_hash(snap),
       sap_hash = .sr_freeze_chr1(project$state$sap$lock$sap_hash))
}

.sr_fz_num1 <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
  if (length(x)) x[1L] else default
}

# The library difference of two manifest-shaped lists. A snapshot whose pkgs lost its names (a manifest
# written before the names fix) cannot be compared package by package: only R and the snapshot date are,
# and a differing library hash is then reported as one "packages" row with "unknown" values.
.sr_fz_library_diff <- function(a, b) {
  named <- function(s) {
    p <- s$pkgs
    !length(p) || !is.null(names(p))
  }
  if (named(a$libsnap) && named(b$libsnap)) return(.sr_libsnap_diff(a$libsnap, b$libsnap))
  strip <- function(s) {
    s$pkgs <- NULL
    s
  }
  d <- .sr_libsnap_diff(strip(a$libsnap), strip(b$libsnap))
  ha <- .sr_freeze_chr1(a$libsnap_hash)
  hb <- .sr_freeze_chr1(b$libsnap_hash)
  if (!identical(ha, hb)) {
    d <- rbind(d, data.frame(item = "packages", from = "unknown", to = "unknown", change = "updated",
                             stringsAsFactors = FALSE))
  }
  rownames(d) <- NULL
  d
}

# Diff of two manifest-shaped lists (a freeze manifest or .sr_freeze_now()).
.sr_freeze_diff_manifests <- function(a, b) {
  ta <- a$tables %||% list()
  tb <- b$tables %||% list()
  names_all <- union(names(ta), names(tb))
  pick <- function(t, f, default) {
    e <- if (t %in% names(ta)) ta[[t]] else tb[[t]]
    e[[f]] %||% default
  }
  rows <- lapply(names_all, function(t) {
    ea <- ta[[t]]
    eb <- tb[[t]]
    ha <- if (is.null(ea)) NA_character_ else .sr_freeze_chr1(ea$store_hash)
    hb <- if (is.null(eb)) NA_character_ else .sr_freeze_chr1(eb$store_hash)
    change <- if (is.null(ea)) "added" else if (is.null(eb)) "removed" else if (identical(ha, hb)) "same" else "changed"
    data.frame(table = t, base = as.character(pick(t, "base", t))[1L],
               version = as.integer(pick(t, "version", 0L))[1L], change = change,
               from_hash = ha, to_hash = hb,
               from_rows = if (is.null(ea)) NA_real_ else .sr_fz_num1(ea$rows),
               to_rows = if (is.null(eb)) NA_real_ else .sr_fz_num1(eb$rows),
               stringsAsFactors = FALSE)
  })
  tables <- if (length(rows)) do.call(rbind, rows) else
    data.frame(table = character(), base = character(), version = integer(), change = character(),
               from_hash = character(), to_hash = character(), from_rows = numeric(), to_rows = numeric(),
               stringsAsFactors = FALSE)
  tables <- tables[order(tables$base, tables$version, tables$table, method = "radix"), , drop = FALSE]
  rownames(tables) <- NULL

  ra <- a$runs %||% list()
  rb <- b$runs %||% list()
  ids <- sort(union(names(ra), names(rb)), method = "radix")
  fk <- vapply(ids, function(i) .sr_freeze_chr1(ra[[i]]$ok_key), "", USE.NAMES = FALSE)
  tk <- vapply(ids, function(i) .sr_freeze_chr1(rb[[i]]$ok_key), "", USE.NAMES = FALSE)
  change <- ifelse(is.na(fk), "added", ifelse(is.na(tk), "removed", ifelse(fk == tk, "same", "rerun")))
  analyses <- data.frame(analysis = ids, change = as.character(change), from_key = fk, to_key = tk,
                         stringsAsFactors = FALSE)
  sa <- .sr_freeze_chr1(a$sap_hash)
  sb <- .sr_freeze_chr1(b$sap_hash)
  list(tables = tables, library = .sr_fz_library_diff(a, b),
       sap = list(from = sa, to = sb, changed = !identical(sa, sb)), analyses = analyses)
}

#' What changed between freeze `from` and freeze `to`, or the project now when `to` is NULL.
#' @return list(tables, library, sap, analyses); see the file header.
sr_freeze_diff <- function(project, from, to = NULL) {
  a <- sr_freeze_manifest(project, from)
  b <- if (is.null(to)) .sr_freeze_now(project) else sr_freeze_manifest(project, to)
  .sr_freeze_diff_manifests(a, b)
}

.sr_fz_unreadable <- structure(list(), class = "sr_fz_unreadable")

# Exact comparison of two named numeric vectors. Status: structure (names or length differ), changed
# (NA positions differ, or a value differs), identical. Equal values, infinities included, give a difference
# of 0; a difference involving an infinity is Inf, so the maxima are never NaN. NA and NaN count as NA, and
# the maxima are NA when the NA positions differ.
.sr_fz_compare <- function(x, y) {
  res <- function(status, n = NA_integer_, ab = NA_real_, rel = NA_real_) {
    list(status = status, n = as.integer(n), max_abs = ab, max_rel = rel)
  }
  if (!identical(names(x), names(y)) || length(x) != length(y)) return(res("structure"))
  n <- length(x)
  if (!identical(is.na(x), is.na(y))) return(res("changed", n))
  ok <- !is.na(x)
  xo <- x[ok]
  yo <- y[ok]
  same <- xo == yo
  d <- ifelse(same, 0, abs(xo - yo))
  r <- ifelse(same, 0, ifelse(is.infinite(d), Inf, d / pmax(abs(xo), .Machine$double.eps)))
  mx <- function(v) if (length(v)) max(v) else 0
  res(if (all(same)) "identical" else "changed", n, mx(d), mx(r))
}

#' The frozen stored result of each analysis against the current one.
#' Status: same_run (equal keys), identical (numbers equal), changed, structure (numeric leaves differ in
#' names or count), not_run (no current ok run), missing (a fit file is absent).
#' @return data.frame(analysis, status, n_values, max_abs_diff, max_rel_diff)
sr_freeze_results_diff <- function(project, label) {
  m <- sr_freeze_manifest(project, label)
  runs <- project$state$runs %||% list()
  ids <- sort(names(m$runs %||% list()), method = "radix")
  one <- function(id) {
    out <- function(status, n = NA_integer_, ab = NA_real_, rel = NA_real_) {
      data.frame(analysis = id, status = status, n_values = as.integer(n), max_abs_diff = ab,
                 max_rel_diff = rel, stringsAsFactors = FALSE)
    }
    fk <- .sr_freeze_chr1(m$runs[[id]]$ok_key)
    ck <- .sr_freeze_chr1(runs[[id]]$ok_key)
    if (is.na(ck)) return(out("not_run"))
    if (identical(fk, ck)) return(out("same_run"))
    ff <- file.path(project$paths$fits, paste0(fk, ".rds"))
    cf <- file.path(project$paths$fits, paste0(ck, ".rds"))
    if (is.na(fk) || !file.exists(ff) || !file.exists(cf)) return(out("missing"))
    vx <- tryCatch(readRDS(ff)$value, error = function(e) .sr_fz_unreadable)
    vy <- tryCatch(readRDS(cf)$value, error = function(e) .sr_fz_unreadable)
    if (identical(vx, .sr_fz_unreadable) || identical(vy, .sr_fz_unreadable)) return(out("missing"))
    if (is.null(vx) || is.null(vy)) return(out("structure"))
    cmp <- .sr_fz_compare(.sr_fz_numbers(vx), .sr_fz_numbers(vy))
    out(cmp$status, cmp$n, cmp$max_abs, cmp$max_rel)
  }
  rows <- lapply(ids, one)
  if (!length(rows)) {
    return(data.frame(analysis = character(), status = character(), n_values = integer(),
                      max_abs_diff = numeric(), max_rel_diff = numeric(), stringsAsFactors = FALSE))
  }
  r <- do.call(rbind, rows)
  rownames(r) <- NULL
  r
}
