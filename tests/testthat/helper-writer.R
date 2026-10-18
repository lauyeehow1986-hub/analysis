# Fixture for the P1d writer tests: a locked and run project with the five route kinds, a multiplicity
# family and one analysis added after the lock. Synthetic data only (seeded).
#
#   .fx_writer_project(env)   a fresh open copy of a template project built once per R session
#   .fx_writer_build(env, decision, boot, drop, unrun)   the uncached builder, for variant tests
#   .fx_writer_result(p, id)  sr_result(p, id)

.fx_writer_layers <- function(resampling = "none") {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = resampling,
       framework = "frequentist", clustered = FALSE)
}

# n = 400: grp (A/B exposure), age, z1 (binary), stage (I/II/III with III on exactly 3 rows), y (yes/no),
# time / status (exponential, about 30% censored) and agecat (a factor with a "65+" level).
.fx_writer_frame <- function() {
  withr::with_seed(11, {
    n <- 400L
    grp <- sample(c("A", "B"), n, replace = TRUE)
    b <- as.numeric(grp == "B")
    age <- round(stats::rnorm(n, 60, 10), 1)
    z1 <- stats::rbinom(n, 1L, 0.4)
    stage <- sample(c("I", "II"), n, replace = TRUE)
    stage[sample.int(n, 3L)] <- "III"
    y <- ifelse(stats::runif(n) < stats::plogis(-0.4 + 0.6 * b + 0.02 * (age - 60) + 0.5 * z1), "yes", "no")
    ev <- stats::rexp(n, 0.1 * exp(0.4 * b + 0.01 * (age - 60)))
    ce <- stats::rexp(n, 0.04)
    data.frame(id = seq_len(n), grp = grp, age = age, z1 = z1, stage = stage, y = y,
               time = round(pmin(ev, ce), 3), status = as.integer(ev <= ce),
               agecat = factor(ifelse(age >= 65, "65+", "<65"), levels = c("<65", "65+")),
               stringsAsFactors = FALSE)
  })
}

# The routes, registered again when an earlier test file emptied the registry.
.fx_writer_routes <- function() {
  make <- list(lm = .sr_lm_route, logistic = .sr_logistic_route, modpois = .sr_modpois_route,
               cox = .sr_cox_route, rmst = .sr_rmst_route, table1 = .sr_table1_route)
  for (k in names(make)) {
    if (!(.sr_route_key(k, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make[[k]]())
  }
  invisible(TRUE)
}

# Build the project (not cached). `decision` is the feasibility decision of every primary analysis,
# `boot` gives a_mp the bootstrap layer (B = 200), `drop` lists analyses never added (x_age too) and
# `unrun` analyses that are not run. The caller's `env` owns the project folder and its cleanup.
.fx_writer_build <- function(env = parent.frame(), decision = "proceed", boot = FALSE,
                             drop = character(), unrun = character()) {
  .fx_writer_routes()
  df <- .fx_writer_frame()
  tau <- round(unname(stats::quantile(df$time, 0.8)), 3)
  p <- .fx_ready_project(df, "t", c(id = "id", y = "outcome"), env = env)
  # one exclusion step, for the flow figure: the analyses read the table, not the recipe output
  p <- sr_recipe_add(p, "t", list(op = "drop_cases", args = list(where = list(col = "age", op = ">", value = 70)),
                                  reason = "age above the study range"))
  lay <- .fx_writer_layers()
  reg <- function(p, id, route, spec, label, family = NA) {
    if (id %in% drop) return(p)
    sr_sap_add(p, id, route, spec, "t", label = label, family = family)
  }
  p <- reg(p, "t1", "table1",
           list(purpose = "describe", by = "grp", vars = c("age", "z1", "stage", "agecat"),
                layers = lay, seed = 42L), "primary")
  bin <- list(purpose = "explain-causal", outcome = "y", exposure = "grp", exposure_ref = "A",
              terms = list(list(var = "age"), list(var = "z1")), marginal = TRUE, layers = lay,
              seed = 42L)
  surv <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 1,
               exposure = "grp", exposure_ref = "A", layers = lay, seed = 42L)
  p <- reg(p, "a_lr", "logistic", bin, "primary", family = "fam1")
  p <- reg(p, "a_cox", "cox", c(surv, list(terms = list(list(var = "age")))), "secondary",
           family = "fam1")
  p <- reg(p, "a_rmst", "rmst", c(surv, list(tau = tau)), "secondary")
  mp <- bin
  if (isTRUE(boot)) {
    mp$layers <- .fx_writer_layers("bootstrap")
    mp$boot <- list(B = 200L)
  }
  p <- reg(p, "a_mp", "modpois", mp, "sensitivity")
  members <- setdiff(c("a_lr", "a_cox"), drop)
  if (length(members)) p <- sr_sap_family(p, "fam1", members, "holm")
  p <- sr_sap_lock(.fx_feasible(p, decision))
  if (!identical(decision, "stop")) {
    for (id in setdiff(c("t1", "a_lr", "a_cox", "a_rmst", "a_mp"), c(drop, unrun))) p <- sr_run(p, id)
  }
  if (!("x_age" %in% drop)) {
    xs <- list(purpose = "explain-causal", outcome = "age", exposure = "grp", exposure_ref = "A",
               layers = lay, seed = 42L)
    p <- sr_analysis_add(p, "x_age", "lm", xs, "t", "exploratory", reason = "test fixture")
    if (!identical(decision, "stop") && !("x_age" %in% unrun)) p <- sr_run(p, "x_age")
  }
  p
}

.fx_writer_cache <- new.env(parent = emptyenv())

# The template project folder: built once per R session under tempdir(), closed and kept.
.fx_writer_template <- function() {
  tpl <- .fx_writer_cache$dir
  if (!is.null(tpl) && dir.exists(tpl)) return(tpl)
  # the build runs in a frame of its own, so the project folder and lock are released when it is done
  build <- function() {
      p <- .fx_writer_build(env = environment())
      sr_duck_close(p)
      sr_project_close(p)
      tpl <- tempfile("fx-writer-tpl-")
      dir.create(tpl)
      file.copy(p$paths$dir, tpl, recursive = TRUE)
      file.path(tpl, basename(p$paths$dir))
  }
  .fx_writer_cache$dir <- build()
  .fx_writer_cache$dir
}

# A fresh, open copy of the cached template project; closed and removed when `env` finishes.
.fx_writer_project <- function(env = parent.frame()) {
  tpl <- .fx_writer_template()
  dest <- withr::local_tempdir(.local_envir = env)
  file.copy(tpl, dest, recursive = TRUE)
  p <- sr_project_open(file.path(dest, basename(tpl)))
  withr::defer(sr_duck_close(p), envir = env)
  withr::defer(sr_project_close(p), envir = env)
  p
}

.fx_writer_result <- function(p, id) sr_result(p, id)
