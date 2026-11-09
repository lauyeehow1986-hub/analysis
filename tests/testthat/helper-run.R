# Shared helpers for the run tests (test-run.R and later the Analyses panel tests). Synthetic data only.

.run_frame <- function() {
  lung <- survival::lung
  data.frame(id = seq_len(nrow(lung)), time = lung$time, died = as.integer(lung$status == 2L),
             age = lung$age, sex = ifelse(lung$sex == 1L, "M", "F"), ecog = lung$ph.ecog,
             wt_loss = lung$wt.loss, stringsAsFactors = FALSE)
}

.run_roles <- c(id = "id", time = "time", died = "outcome", sex = "exposure")

.run_drive <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())

# Register a stub route whose compile returns `body_fn(spec)`; removed when the calling test ends.
.run_route <- function(id = "stub_run", body_fn = NULL, env = parent.frame()) {
  if (is.null(body_fn)) {
    body_fn <- function(spec) {
      sprintf("list(n = nrow(sr_data), m = mean(sr_data[[%s]]))", .sr_lit(spec$outcome))
    }
  }
  est <- sr_estimand(population = "adults", variables = "died", intercurrent = "none",
                     summary = "stub", level = "marginal")
  present <- function(value, spec) {
    eff <- data.frame(term = "m", measure = "MD", est = value$m, lo = NA_real_, hi = NA_real_,
                      p = NA_real_, scale = NA_character_, level = NA_character_, mcse = NA_real_,
                      role = "descriptive", conditional = FALSE, ci_method = NA_character_,
                      n = value$n, events = NA_real_, stringsAsFactors = FALSE)
    list(tables = list(), figures = list(), fragments = list(), effects = eff)
  }
  r <- sr_route(id = id, version = 1L, family = "stub", purpose = "explain-causal", estimand = est,
                spec_schema = list(outcome = list(type = "chr", required = TRUE),
                                   seed = list(type = "num", required = FALSE)),
                compile = body_fn, present = present)
  sr_route_register(r)
  withr::defer(rm(list = .sr_route_key(id, 1L), envir = .sr_routes), envir = env)
  invisible(r)
}

.run_spec <- function(outcome = "died", ...) {
  layers <- list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                 framework = "frequentist", clustered = FALSE)
  c(list(purpose = "explain-causal", outcome = outcome, layers = layers, seed = 42L), list(...))
}

# A ready project with analysis a1 (route `id`) drafted, SAP not yet locked.
.run_drafted <- function(id = "stub_run", body_fn = NULL, env = parent.frame()) {
  .run_route(id, body_fn, env = env)
  p <- .fx_ready_project(.run_frame(), "t", .run_roles, env = env)
  sr_sap_add(p, "a1", id, .run_spec(), "t", label = "primary")
}

.run_locked <- function(id = "stub_run", body_fn = NULL, env = parent.frame()) {
  sr_sap_lock(.fx_feasible(.run_drafted(id, body_fn, env = env)))
}

# Re-open the project in `dir` (the old handle is closed first).
.run_reopen <- function(p, env = parent.frame()) {
  dir <- p$paths$dir
  sr_duck_close(p)
  sr_project_close(p)
  q <- sr_project_open(dir)
  withr::defer(sr_duck_close(q), envir = env)
  withr::defer(sr_project_close(q), envir = env)
  q
}

n_events <- function(p) length(sr_events_read(p$paths$events))
types_of <- function(p) vapply(sr_events_read(p$paths$events), function(e) e$type, "")
