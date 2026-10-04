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

# One-shot SHA-256 of an open binary connection (lowercase hex).
.sr_sha256_con <- function(con) paste(as.character(unclass(openssl::sha256(con))), collapse = "")

# An incremental SHA-256: list(feed = function(raw), final = function() hex), or NULL when it cannot
# be set up. NOTE: this relies on the UNEXPORTED openssl functions md_init / md_feed / md_final
# (openssl has no exported incremental API), so every call into them is wrapped by the caller and a
# failure falls back to the one-shot hash.
.sr_md_incremental <- function() {
  tryCatch({
    ns <- asNamespace("openssl")
    md <- get("md_init", envir = ns)("sha256")
    feed <- get("md_feed", envir = ns)
    final <- get("md_final", envir = ns)
    list(feed = function(data) feed(md, data),
         final = function() paste(as.character(unclass(final(md))), collapse = ""))
  }, error = function(e) NULL)
}

# SHA-256 (lowercase hex) of a whole file, streamed. With progress (function(done_bytes, total))
# the file is read in 8 MiB blocks into an incremental hash and progress() is called after each.
# Only the md_* hashing calls are guarded: if they are unavailable or error (or option
# sr.sha256_force_fallback is TRUE, used by tests), the file is re-read with openssl::sha256 and
# progress() is called once, at the end. An error raised by the caller's progress() is NOT caught.
sr_sha256_file <- function(path, progress = NULL) {
  .sr_file_must_exist(path)
  if (!is.null(progress) && !is.function(progress)) stop("progress must be a function or NULL", call. = FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  total <- as.numeric(file.size(path))
  if (is.null(progress)) return(.sr_sha256_con(con))
  inc <- if (isTRUE(getOption("sr.sha256_force_fallback"))) NULL else .sr_md_incremental()
  out <- NULL
  if (!is.null(inc)) {
    done <- 0
    failed <- FALSE
    repeat {
      data <- readBin(con, "raw", n = SR_HASH_BLOCK)
      if (!length(data)) break
      if (!isTRUE(tryCatch({ inc$feed(data); TRUE }, error = function(e) FALSE))) { failed <- TRUE; break }
      done <- done + length(data)
      progress(done, total)                    # outside any tryCatch: the caller's errors propagate
    }
    if (!failed) {
      if (done == 0) progress(0, total)
      out <- tryCatch(inc$final(), error = function(e) NULL)
    }
  }
  if (is.null(out)) {
    seek(con, where = 0, origin = "start", rw = "read")
    out <- .sr_sha256_con(con)
    progress(total, total)
  }
  out
}
