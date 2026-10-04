# io/preflight.R - the ingest pre-flight: will the data fit, how long will it take, is the source on a
# network share. It runs before a file is streamed into the project's Parquet store. Nothing here
# reads the data beyond the (already cheap) sniff sample, and nothing touches the network: free space
# and mount flags come from local OS calls only, and a UNC path is flagged without being probed.

# GB = 1024^3 and MB = 1024^2 throughout.
.sr_pf_gb <- 1024^3
.sr_pf_mb <- 1024^2

# Parquet is budgeted at the upper end of the 15-30% of the uncompressed size.
.sr_pf_parquet_frac <- 0.30
# The ratio assumed for a gz / bz2 / xz source is never below this (and is used when it cannot be measured).
.sr_pf_min_ratio <- 3
# Sample of the decompressed head used to estimate the compression ratio.
.sr_pf_ratio_sample <- 1048576L
# An Excel file larger than this on disk is refused (the 1,048,576-row cap is enforced at ingest).
.sr_pf_xlsx_max <- 200 * .sr_pf_mb
# A JSON document (not NDJSON) larger than this, uncompressed, is refused.
.sr_pf_json_max <- 512 * .sr_pf_mb
# Above this on-disk size an external (by-reference) source is suggested instead of a copy.
.sr_pf_external_above <- 2 * .sr_pf_gb
# Ingest throughput by hardware profile, bytes of UNCOMPRESSED text per second. The laptop figure is
# measured (the delimited-text ingest of a 36 MB, 1M-row CSV took about 4 s end to end, about 9 MB/s;
# compressed and re-encoded sources add a staging pass, so they are no faster). locked16 is not
# measured: it is scaled by the same 60:150 ratio as before and so is deliberately conservative.
.sr_pf_throughput <- c(locked16 = 4 * .sr_pf_mb, laptop = 9 * .sr_pf_mb)

# The nearest existing directory of `path`: the path itself when it is a directory, else the closest
# existing parent (a file, or a path that does not exist yet).
.sr_pf_probe_dir <- function(path) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  while (!dir.exists(p)) {
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }
  p
}

# Does the partition holding `dir` carry the "remote" mount option? The partition is the one with the
# longest mount point that prefixes `dir`. Returns TRUE / FALSE, or NA when no partition matches.
.sr_pf_partition_remote <- function(dir, parts) {
  norm <- function(x) {
    x <- tolower(gsub(intToUtf8(92L), "/", x, fixed = TRUE))
    ifelse(endsWith(x, "/"), x, paste0(x, "/"))
  }
  target <- norm(dir)
  mounts <- norm(as.character(parts$mountpoint))
  hit <- which(startsWith(target, mounts))
  if (!length(hit)) return(NA)
  best <- hit[which.max(nchar(mounts[hit]))]
  opts <- strsplit(as.character(parts$options[best]), ",", fixed = TRUE)[[1L]]
  "remote" %in% trimws(opts)
}

#' Free bytes on the drive holding `path` and whether it is remote, from local OS calls only:
#' list(free_bytes, remote, warnings). `remote` is TRUE for a UNC path (never probed: free_bytes is NA
#' then) or for a partition whose options contain "remote". When the free space cannot be determined
#' `free_bytes` is NA and `warnings` says so (sr_preflight then refuses: fail closed). `usage` and
#' `partitions` are the ps functions, arguments so tests can replace them.
.sr_drive_info <- function(path, usage = ps::ps_disk_usage, partitions = ps::ps_disk_partitions) {
  out <- list(free_bytes = NA_real_, remote = FALSE, warnings = character())
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    out$warnings <- "cannot determine free space: no path"
    return(out)
  }
  if (.sr_is_unc(path)) {
    out$remote <- TRUE
    out$warnings <- "path is on a network share (UNC): its free space is not probed"
    return(out)
  }
  dir <- .sr_pf_probe_dir(path)
  free <- tryCatch(as.numeric(usage(dir)$available), error = function(e) NA_real_)
  if (length(free) != 1L || !is.finite(free)) {
    free <- NA_real_
    out$warnings <- c(out$warnings, "cannot determine free space (the disk query failed)")
  }
  out$free_bytes <- free
  remote <- tryCatch(.sr_pf_partition_remote(dir, partitions(all = TRUE)), error = function(e) NA)
  if (is.na(remote)) {
    out$warnings <- c(out$warnings, "cannot tell whether the path is on a network drive")
  } else {
    out$remote <- remote
  }
  out
}

# The uncompressed size of the source: list(bytes, warnings).
#  - none: the file size; zip: the exact `Length` of the chosen member (all members when none is chosen);
#  - gz / bz2 / xz: size * ratio, the ratio measured on a sample of the decompressed head
#    (head size / its memCompress size) and never below 3. The whole file is never decompressed.
.sr_pf_uncompressed <- function(path, sniff) {
  size <- as.numeric(sniff$size_bytes)
  comp <- sniff$compression
  if (is.null(comp) || is.na(comp) || identical(comp, "none")) return(list(bytes = size, warnings = character()))
  if (identical(comp, "zip")) {
    lens <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) NULL)
    if (!is.null(lens) && nrow(lens)) {
      if (!is.null(sniff$member) && !is.na(sniff$member) && sniff$member %in% lens$Name) {
        return(list(bytes = as.numeric(lens$Length[match(sniff$member, lens$Name)]), warnings = character()))
      }
      return(list(bytes = sum(as.numeric(lens$Length)), warnings = character()))
    }
    return(list(bytes = size * .sr_pf_min_ratio,
                warnings = "cannot read the zip directory: size estimated at 3 x the archive"))
  }
  type <- c(gz = "gzip", bz2 = "bzip2", xz = "xz")[[comp]]
  ratio <- tryCatch({
    head <- .sr_sniff_head(path, comp, sniff$member, .sr_pf_ratio_sample)
    if (!length(head)) .sr_pf_min_ratio else length(head) / length(memCompress(head, type = type))
  }, error = function(e) NA_real_)
  warn <- character()
  if (!is.finite(ratio)) {
    ratio <- .sr_pf_min_ratio
    warn <- "cannot sample the compressed file: size estimated at 3 x the file"
  }
  list(bytes = size * max(.sr_pf_min_ratio, ratio), warnings = warn)
}

.sr_pf_fmt_gb <- function(bytes) format(round(bytes / .sr_pf_gb, 1), scientific = FALSE, trim = TRUE)

#' Pre-flight check of ingesting `path` into `project`. Returns
#' list(ok, size_bytes, uncompressed_bytes, need_bytes, free_bytes, est_seconds, network_share,
#'      external_suggested, warnings, refuse_reason).
#'  - size_bytes is the file's size on disk; uncompressed_bytes the estimate (see .sr_pf_uncompressed).
#'  - need_bytes = 0.30 x uncompressed (the Parquet) + 1 x the DuckDB memory cap (spill) +
#'    max(2 GB, 5% of uncompressed) (headroom), plus the on-disk size (the copy of the source) unless
#'    `external` (the source is referenced in place).
#'  - Free space is read from the project's data dir (where the Parquet and spill files land); the share
#'    flag from the source path. `drive_info(path)` returns list(free_bytes, remote, warnings).
#'  - est_seconds = uncompressed / throughput (60 MB/s on locked16, 150 MB/s on laptop).
#'  - external_suggested: the file is larger than 2 GB on disk.
#'  - Refused (ok = FALSE, with refuse_reason): an Excel file over 200 MB; a JSON document over 512 MB
#'    uncompressed; unknown free space (fail closed); free space below need_bytes.
sr_preflight <- function(project, path, sniff = sr_sniff(path), drive_info = .sr_drive_info,
                         external = FALSE) {
  profile <- project$meta$profile
  caps <- sr_profile_caps(profile)
  data_dir <- sr_project_paths(project$paths$dir)$data
  external <- isTRUE(external)

  size <- as.numeric(sniff$size_bytes)
  unc <- .sr_pf_uncompressed(path, sniff)
  usize <- unc$bytes

  parquet <- .sr_pf_parquet_frac * usize
  spill <- caps$duckdb_memory_gb * .sr_pf_gb
  headroom <- max(2 * .sr_pf_gb, 0.05 * usize)
  need <- parquet + spill + headroom + if (external) 0 else size

  tp <- unname(.sr_pf_throughput[profile])
  est_seconds <- usize / tp

  dest <- drive_info(data_dir)
  free <- suppressWarnings(as.numeric(dest$free_bytes))
  if (length(free) != 1L) free <- NA_real_
  src <- drive_info(path)
  network <- isTRUE(src$remote)

  warnings <- c(sniff$warnings, unc$warnings, dest$warnings, src$warnings)
  if (network) {
    warnings <- c(warnings, paste(
      "the source file is on a network share: copy it to a local drive first (reads over a share are",
      "slow, and the share can vanish part-way through an ingest)"))
  }

  fmt <- sniff$format
  refuse <- NULL
  if (!is.null(fmt) && !is.na(fmt) && fmt %in% c("xlsx", "xlsm", "xls") && size > .sr_pf_xlsx_max) {
    refuse <- "XLSX too large: export to CSV"
  } else if (!is.null(fmt) && !is.na(fmt) && fmt == "json" && usize > .sr_pf_json_max) {
    refuse <- "convert to NDJSON"
  } else if (is.na(free)) {
    refuse <- "cannot determine free space"
  } else if (free < need) {
    refuse <- sprintf("insufficient space: need %s GB, have %s GB", .sr_pf_fmt_gb(need), .sr_pf_fmt_gb(free))
  }

  list(ok = is.null(refuse), size_bytes = size, uncompressed_bytes = usize, need_bytes = need,
       free_bytes = free, est_seconds = est_seconds, network_share = network,
       external_suggested = size > .sr_pf_external_above, warnings = unique(warnings),
       refuse_reason = refuse)
}