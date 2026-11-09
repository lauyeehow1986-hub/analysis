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
