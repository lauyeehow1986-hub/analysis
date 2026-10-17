# core/libsnap.R - a snapshot of the R and package versions a result was computed with.
# The snapshot is part of the content hash, so results are not reused across library changes.

#' Packages whose versions are recorded in the snapshot. P1c adds the analysis packages.
.sr_core_pkgs <- function() {
  c("jsonlite", "openssl", "callr", "processx", "ps", "shiny", "bslib",
    "survival", "mice", "sandwich", "boot", "Hmisc", "logistf", "ggplot2", "ragg")
}

#' Snapshot of the R version, the configured CRAN snapshot date and the installed versions of
#' `pkgs`. A package that is not installed is recorded as NA_character_ (never an error).
sr_libsnap <- function(pkgs = .sr_core_pkgs()) {
  vers <- vapply(pkgs, function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  }, character(1), USE.NAMES = FALSE)
  names(vers) <- pkgs
  list(
    r = R.version.string,
    snapshot_date = sr_opt("cran_snapshot", NA),
    pkgs = vers
  )
}

#' SHA-256 of the canonical JSON of a library snapshot.
sr_libsnap_hash <- function(snap = sr_libsnap()) {
  snap$pkgs <- as.list(snap$pkgs)
  .sr_sha256(sr_canonical_json(snap))
}
