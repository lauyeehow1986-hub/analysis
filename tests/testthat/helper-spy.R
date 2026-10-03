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
