# io/fingerprint.R - cheap file fingerprints (to notice a moved or changed source) and a streaming
# whole-file SHA-256 with optional progress. Pure R over openssl; nothing here touches the network.

SR_FP_BLOCK <- 1048576      # 1 MiB sampled at the start, the middle and the end
SR_HASH_BLOCK <- 8388608    # 8 MiB per read when hashing with progress

.sr_file_must_exist <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path) || dir.exists(path)) {
    stop("file not found: ", paste(format(path), collapse = ","), call. = FALSE)
  }
  invisible(path)
}

# A fingerprint of a file: size (bytes), mtime (ISO-8601 UTC, ms) and sample_sha, the SHA-256 of
# three 1 MiB blocks (start, middle, end; fewer when the file is smaller). Cheap enough to take on
# a multi-GB source; not a content hash (see sr_sha256_file).
sr_fingerprint <- function(path) {
  .sr_file_must_exist(path)
  size <- as.numeric(file.size(path))
  mtime <- format(file.mtime(path), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  offs <- unique(pmax(0, c(0, floor((size - SR_FP_BLOCK) / 2), size - SR_FP_BLOCK)))
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  blocks <- lapply(offs, function(off) {
    seek(con, where = off, origin = "start", rw = "read")
    readBin(con, "raw", n = min(SR_FP_BLOCK, size - off))
  })
  list(size = size, mtime = mtime, sample_sha = .sr_sha256(do.call(c, c(list(raw(0)), blocks))))
}

# SHA-256 (lowercase hex) of a whole file, streamed. With progress (function(done_bytes, total))
# the file is read in 8 MiB blocks into an incremental hash and progress() is called after each.
sr_sha256_file <- function(path, progress = NULL) {
  .sr_file_must_exist(path)
  if (!is.null(progress) && !is.function(progress)) stop("progress must be a function or NULL", call. = FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  hex <- function(h) paste(as.character(unclass(h)), collapse = "")
  total <- as.numeric(file.size(path))
  if (is.null(progress)) return(hex(openssl::sha256(con)))
  ns <- asNamespace("openssl")
  if (!all(vapply(c("md_init", "md_feed", "md_final"), exists, logical(1), envir = ns, inherits = FALSE))) {
    out <- hex(openssl::sha256(con))   # no incremental API in this openssl build: one report at the end
    progress(total, total)
    return(out)
  }
  md <- get("md_init", envir = ns)("sha256")
  feed <- get("md_feed", envir = ns)
  done <- 0
  repeat {
    data <- readBin(con, "raw", n = SR_HASH_BLOCK)
    if (!length(data)) break
    feed(md, data)
    done <- done + length(data)
    progress(done, total)
  }
  if (done == 0) progress(0, total)
  hex(get("md_final", envir = ns)(md))
}
