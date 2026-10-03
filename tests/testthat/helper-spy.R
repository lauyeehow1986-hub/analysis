# A spy that proves no network fetch or file read happens for a given string (air-gap tests).
#
# local_fetch_spy(markers) traces base::url, base::file, curl::curl and curl::curl_fetch_memory for the
# rest of the calling test (use one spy per test). A call whose first argument is exactly one of
# `markers` is recorded and refused (so nothing is ever fetched or read); every other call runs
# untouched. Returns an environment whose `calls` holds the names of the functions so called.
local_fetch_spy <- function(markers, env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$calls <- character()
  hit <- function(fn, x) {
    if (is.character(x) && length(x) == 1L && !is.na(x) && x %in% markers) {
      seen$calls <- c(seen$calls, fn)
      stop("blocked by test spy: ", fn, call. = FALSE)
    }
  }
  requireNamespace("curl", quietly = TRUE)
  targets <- list(
    list(fn = "url", arg = "description", where = baseenv()),
    list(fn = "file", arg = "description", where = baseenv()),
    list(fn = "curl", arg = "url", where = asNamespace("curl")),
    list(fn = "curl_fetch_memory", arg = "url", where = asNamespace("curl"))
  )
  for (t in targets) {
    tracer <- bquote(.(hit)(.(t$fn), .(as.name(t$arg))))
    suppressMessages(trace(t$fn, tracer = tracer, where = t$where, print = FALSE))
  }
  withr::defer({
    for (t in targets) suppressMessages(untrace(t$fn, where = t$where))
  }, envir = env)
  seen
}

# local_rename_spy() records the `to` argument of every base::file.rename call for the rest of the
# calling test (the calls still run). Returns an environment whose `to` holds them.
local_rename_spy <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$to <- character()
  rec <- function(x) seen$to <- c(seen$to, normalizePath(x, winslash = "/", mustWork = FALSE))
  suppressMessages(trace("file.rename", tracer = bquote(.(rec)(to)), where = baseenv(), print = FALSE))
  withr::defer(suppressMessages(untrace("file.rename", where = baseenv())), envir = env)
  seen
}

# local_failing_audit(pattern) makes sr_audit_append fail, for the rest of the calling test, for
# every action matching the regex `pattern` (other actions are written normally). With
# `torn = TRUE` the failing call first writes the start of its line with no LF, as a crash mid
# write would. The core is sourced into the global environment, so the mock is a rebinding there.
local_failing_audit <- function(pattern, torn = FALSE, env = parent.frame()) {
  real <- get("sr_audit_append", envir = globalenv())
  assign("sr_audit_append", function(log_path, action, actor, detail = list()) {
    if (grepl(pattern, action)) {
      if (torn) {
        con <- file(log_path, "ab")
        writeBin(charToRaw(paste0("{\"ts\":\"2026-01-01T00:00:00.000Z\",\"action\":\"", action)), con)
        close(con)
      }
      stop("simulated audit write failure", call. = FALSE)
    }
    real(log_path, action, actor, detail)
  }, envir = globalenv())
  withr::defer(assign("sr_audit_append", real, envir = globalenv()), envir = env)
  invisible(real)
}
