# Fixtures shared by the UI module tests (Design and SAP panels).

# Record every shiny::showNotification() call: $msg and $type (character vectors, in order). Mocked for the
# calling test only.
.ui_spy_notify <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$type <- character()
  seen$msg <- character()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., type = "message") {
      seen$type <- c(seen$type, type)
      seen$msg <- c(seen$msg, paste(as.character(ui), collapse = " "))
      invisible(NULL)
    }, .package = "shiny", .env = env)
  seen
}

# An open project with table "t" (the writer frame) ingested, roles confirmed and every DQ finding decided,
# not yet SAP-locked. Roles: id = id, y = outcome, grp = exposure, age and z1 = covariate, time = time,
# status = event (all valid in SR_ROLES and all columns of the frame). The caller's `env` owns the folder.
.fx_ui_project <- function(env = parent.frame()) {
  .fx_writer_routes()
  .fx_ready_project(.fx_writer_frame(), "t",
                    c(id = "id", y = "outcome", grp = "exposure", age = "covariate", z1 = "covariate",
                      time = "time", status = "event"), env)
}

# The binary-outcome (logistic) spec of the writer fixture, with default layers. The outcome y is "yes"/"no"
# text, so event_value = "yes" is set: sr_sap_add refuses a non-0/1 outcome with no event_value
# (.sr_sap_event_problem), and event_value is in the logistic spec_schema.
.ui_bin_spec <- function() {
  list(purpose = "explain-causal", outcome = "y", exposure = "grp", exposure_ref = "A",
       terms = list(list(var = "age"), list(var = "z1")), marginal = TRUE, layers = .fx_writer_layers(),
       seed = 42L, event_value = "yes")
}

# Replace the global function `name` for the rest of the calling test; returns the original.
.ui_stub <- function(name, fn, env = parent.frame()) {
  old <- get(name, envir = globalenv())
  assign(name, fn, envir = globalenv())
  withr::defer(assign(name, old, envir = globalenv()), envir = env)
  invisible(old)
}

# A stand-in for .sr_job_spawn whose job stays pending until the test calls ctl$resolve(value) or
# ctl$reject(error) (no worker process); each call is recorded in ctl$calls (fn, args, opts).
.ui_pending_spawn <- function(ctl) {
  ctl$calls <- list()
  function(app_dir, fn, args = list(), opts = list()) {
    ctl$calls[[length(ctl$calls) + 1L]] <- list(app_dir = app_dir, fn = fn, args = args, opts = opts)
    promises::promise(function(resolve, reject) {
      ctl$resolve <- resolve
      ctl$reject <- reject
    })
  }
}
