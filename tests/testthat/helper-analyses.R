# Shared fixtures for the Analyses panel tests. Synthetic data only.

# A locked project with three stub analyses: a1 (primary), a2 (secondary) and s1 (a sensitivity analysis
# of a1), all on the stub route of helper-run.R. The stub route exists only in this test session: a test
# that spawns a real worker uses the registered lm / cox routes on .dv_project() data instead.
.fx_an_project <- function(body_fn = NULL, env = parent.frame()) {
  p <- .run_drafted("stub_run", body_fn, env)
  p <- sr_sap_add(p, "a2", "stub_run", .run_spec(), "t", label = "secondary")
  p <- sr_sap_add(p, "s1", "stub_run", .run_spec(), "t", label = "sensitivity", parent = "a1")
  sr_sap_lock(.fx_feasible(p))
}

# Kill the processes behind the ps handles `hs` that are still running (test clean-up; never errors).
.fx_an_kill_handles <- function(hs) {
  for (h in hs) {
    try(if (isTRUE(ps::ps_is_running(h))) ps::ps_kill(h), silent = TRUE)
  }
  invisible(NULL)
}

# Wait (up to `timeout` s) for the job file `jf` and at least one descendant of the worker it names, once
# `ready()` is TRUE. Returns list(worker, child, handles); worker is NA and child empty when they never
# appeared.
.fx_an_wait_child <- function(jf, ready, timeout = 60) {
  out <- list(worker = NA_integer_, child = integer(), handles = list())
  t0 <- Sys.time()
  while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < timeout) {
    l <- if (file.exists(jf)) tryCatch(readLines(jf, warn = FALSE), error = function(e) character()) else character()
    if (length(l) == 1L && grepl("^[0-9]+ [0-9.]+$", l)) {
      out$worker <- as.integer(strsplit(l, " ", fixed = TRUE)[[1L]][1L])
      wh <- tryCatch(ps::ps_handle(out$worker), error = function(e) NULL)
      kids <- if (is.null(wh)) list() else tryCatch(ps::ps_children(wh, recursive = TRUE), error = function(e) list())
      out$handles <- c(if (!is.null(wh)) list(wh), kids)
      if (length(kids) && isTRUE(ready())) {
        out$child <- vapply(kids, function(k) as.integer(ps::ps_pid(k)), 0L)
        break
      }
    }
    Sys.sleep(0.25)
  }
  out
}

# Test clean-up for a mirai worker: kill the processes found, kill by the job file (when the worker wrote
# it after the test gave up), and stop the mirai. Never errors.
.fx_an_cleanup <- function(m, jf, handles) {
  .fx_an_kill_handles(handles)
  try(.sr_an_kill(jf, wait = 2), silent = TRUE)
  try(mirai::stop_mirai(m), silent = TRUE)
  invisible(NULL)
}
