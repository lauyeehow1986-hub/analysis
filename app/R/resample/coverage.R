# resample/coverage.R - the CI-coverage simulation cells (in-process; no callr, no project).
#
# A cell is a data generator with a known true parameter plus one analysis spec of a model route. A
# replication draws a dataset of n rows, runs the route's OWN compiled code on it exactly as sr_run
# does (the RNG preamble of a seed, then the body of sr_compile(route, spec) evaluated with `sr_data`
# bound in a fresh environment under the global env, then route$present(value, spec) checked by
# sr_result_check) and keeps the estimate and 95% interval of the route's primary effect row as
# reported. Nothing here re-implements the statistics: the harness only generates data and scores.
#
# Cells (the plan's table; reps, B and m are the defaults of sr_coverage_cell):
#   cc              linear, MCAR 20% in a covariate        lm, complete case                  1000
#   mi_mar          linear, MAR 30% in a covariate         lm, MI m = 20                       500
#   boot            logistic, no missing                   marginal RD, percentile B = 500     500
#   boot_mi         linear, MAR 30%                        lm, MI m = 10 x bootstrap B = 200,  500
#                                                          boot_then_impute
#   cluster_robust  linear, 50 clusters, ICC 0.2           lm, se = "cluster"                 1000
#   cluster_boot    linear, 50 clusters, ICC 0.2           lm, cluster bootstrap percentile    500
#                                                          B = 500
# Generators (n rows; every draw from the replication's stream):
#   linear     z1, z2 ~ N(0, 1); x ~ Bernoulli(plogis(0.5 z1)); y = 1 + 0.5 x + 0.8 z1 + 0.4 z2 + N(0, 1).
#              MCAR: each z1 missing with probability 0.2. MAR: z1 missing with probability
#              plogis(a + y_std + z2), y_std the standardised y and `a` solved (uniroot) so that the
#              mean probability is exactly 0.3 -- missingness depends on the outcome and on another
#              covariate. Analysis y ~ x + z1 + z2; estimand the x coefficient, truth 0.5.
#   logistic   z ~ N(0, 1); x ~ Bernoulli(plogis(0.5 z)); y ~ Bernoulli(plogis(-0.5 + 0.8 x + 0.6 z)).
#              Analysis logistic y ~ x + z with marginal = TRUE; estimand the g-computation marginal
#              risk difference (the route's primary RD row, bootstrapped with g-computation re-run in
#              every replicate, rulings R4 / R27). Truth E_z[plogis(0.3 + 0.6 z) - plogis(-0.5 + 0.6 z)]
#              over z ~ N(0, 1), computed EXACTLY by numerical integration (stats::integrate,
#              rel.tol 1e-10; .sr_cov_rd_truth), not by Monte Carlo.
#   cluster    50 clusters of (near) equal size; a cluster-level exposure x (exactly half of the
#              clusters, at random); z ~ N(0, 1); y = 1 + 0.5 x + 0.5 z + u + e with u ~ N(0, variance 0.2) per
#              cluster and e ~ N(0, 0.8), so the residual ICC is 0.2. Analysis y ~ x + z; truth 0.5.
#
# Random numbers (ruling R37a): replication r ALWAYS uses the r-th L'Ecuyer-CMRG stream of `seed`
# (sr_streams: nextRNGStream applied r times to the state set.seed(seed) leaves under L'Ecuyer-CMRG).
# From that stream it draws its dataset, then one integer, the seed of the run preamble its analysis
# code runs under (the MI and bootstrap layers then take their own streams from that preamble, as in
# sr_run). So a result depends only on (cell, reps, n, seed, B, m), never on the number of workers:
# workers > 1 runs the replications on a PSOCK cluster of base `parallel` whose workers load the app
# core from app/global.R. The caller's RNG kind and .Random.seed are restored on exit.
#
# Scoring (rulings R37c, R37e): over the replications that succeeded,
#   coverage = share of intervals [lo, hi] containing the truth,
#   mcse = sqrt(coverage (1 - coverage) / reps_ok), pass = |coverage - 0.95| < 3 mcse,
#   bias = mean(est) - truth, mean_width = mean(hi - lo).
# Coverage 1 (or 0) gives mcse 0 and pass FALSE: over-coverage is a finding, never a pass.
# A replication whose generation, fit or presentation errors, or whose primary row is not finite, is
# FAILED: it is excluded from every statistic above (reps_ok = reps - failed is the denominator), and
# counted in `failed` with its messages in `errors` ("<message> (<count>)"). A failed replication in
# the full run is a finding to investigate, not noise.

# ---- the cells ------------------------------------------------------------------------------------

.sr_cov_layers <- function(...) {
  utils::modifyList(list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                         framework = "frequentist", clustered = FALSE), list(...))
}

.sr_cov_lin_spec <- function(...) {
  utils::modifyList(list(purpose = "explain-causal", outcome = "y", exposure = "x",
                         terms = list(list(var = "z1"), list(var = "z2")), layers = .sr_cov_layers()),
                    list(...))
}

.sr_cov_cl_spec <- function(...) {
  utils::modifyList(list(purpose = "explain-causal", outcome = "y", exposure = "x",
                         terms = list(list(var = "z")), cluster = "cl",
                         layers = .sr_cov_layers(clustered = TRUE)),
                    list(...))
}

# The cell definitions: defaults, generator, route constructor, spec builder (B, m), the measure of
# the primary row that is scored, the true value, and one-line descriptions for the table.
.sr_cov_defs <- function() {
  list(
    cc = list(reps = 1000L, B = NA_integer_, m = NA_integer_, gen = .sr_cov_gen_cc, route = .sr_lm_route,
              measure = "MD", truth = function() 0.5,
              spec = function(B, m) .sr_cov_lin_spec(),
              generator = "linear, MCAR 20% in a covariate", analysis = "lm, complete case"),
    mi_mar = list(reps = 500L, B = NA_integer_, m = 20L, gen = .sr_cov_gen_mar, route = .sr_lm_route,
                  measure = "MD", truth = function() 0.5,
                  spec = function(B, m) .sr_cov_lin_spec(layers = .sr_cov_layers(missing = "mi"),
                                                         mi = list(m = m)),
                  generator = "linear, MAR 30% in a covariate (on the outcome and another covariate)",
                  analysis = "lm, MI (mice pmm, Rubin)"),
    boot = list(reps = 500L, B = 500L, m = NA_integer_, gen = .sr_cov_gen_logistic, route = .sr_logistic_route,
                measure = "RD", truth = .sr_cov_rd_truth,
                spec = function(B, m) list(purpose = "explain-causal", outcome = "y", exposure = "x",
                                           terms = list(list(var = "z")), marginal = TRUE,
                                           layers = .sr_cov_layers(resampling = "bootstrap"),
                                           boot = list(B = B, type = "percentile")),
                generator = "logistic, no missing", analysis = "logistic marginal RD, bootstrap percentile"),
    boot_mi = list(reps = 500L, B = 200L, m = 10L, gen = .sr_cov_gen_mar, route = .sr_lm_route,
                   measure = "MD", truth = function() 0.5,
                   spec = function(B, m) .sr_cov_lin_spec(layers = .sr_cov_layers(missing = "mi", resampling = "bootstrap"),
                                                          mi = list(m = m), boot = list(B = B, type = "percentile"),
                                                          choices = list(mi_x_boot = "boot_then_impute")),
                   generator = "linear, MAR 30% in a covariate", analysis = "lm, MI x bootstrap percentile, boot_then_impute"),
    cluster_robust = list(reps = 1000L, B = NA_integer_, m = NA_integer_, gen = .sr_cov_gen_cluster,
                          route = .sr_lm_route, measure = "MD", truth = function() 0.5,
                          spec = function(B, m) .sr_cov_cl_spec(se = "cluster"),
                          generator = "linear, 50 clusters, ICC 0.2, cluster-level exposure",
                          analysis = "lm, se = cluster (HC1, t on G - 1 df)"),
    cluster_boot = list(reps = 500L, B = 500L, m = NA_integer_, gen = .sr_cov_gen_cluster, route = .sr_lm_route,
                        measure = "MD", truth = function() 0.5,
                        spec = function(B, m) .sr_cov_cl_spec(layers = .sr_cov_layers(clustered = TRUE, resampling = "bootstrap"),
                                                              boot_type = "cluster", boot = list(B = B, type = "percentile")),
                        generator = "linear, 50 clusters, ICC 0.2, cluster-level exposure",
                        analysis = "lm, cluster bootstrap percentile")
  )
}

#' The coverage cells: data.frame(cell, reps, B, m, generator, analysis), with the default replications,
#' bootstrap replicates (NA: no bootstrap) and imputations (NA: no MI) of each.
sr_coverage_cells <- function() {
  d <- .sr_cov_defs()
  data.frame(cell = names(d), reps = vapply(d, function(x) x$reps, 0L, USE.NAMES = FALSE),
             B = vapply(d, function(x) x$B, 0L, USE.NAMES = FALSE),
             m = vapply(d, function(x) x$m, 0L, USE.NAMES = FALSE),
             generator = vapply(d, function(x) x$generator, "", USE.NAMES = FALSE),
             analysis = vapply(d, function(x) x$analysis, "", USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}

# ---- the generators -------------------------------------------------------------------------------

.sr_cov_gen_linear <- function(n) {
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  x <- stats::rbinom(n, 1L, stats::plogis(0.5 * z1))
  y <- 1 + 0.5 * x + 0.8 * z1 + 0.4 * z2 + stats::rnorm(n)
  data.frame(y = y, x = x, z1 = z1, z2 = z2)
}

.sr_cov_gen_cc <- function(n) {
  d <- .sr_cov_gen_linear(n)
  d$z1[stats::runif(n) < 0.2] <- NA
  d
}

# The intercept `a` with mean(plogis(a + lp)) == p.
.sr_cov_calibrate <- function(lp, p) {
  stats::uniroot(function(a) mean(stats::plogis(a + lp)) - p, c(-30, 30), tol = 1e-10)$root
}

.sr_cov_gen_mar <- function(n) {
  d <- .sr_cov_gen_linear(n)
  lp <- (d$y - mean(d$y)) / stats::sd(d$y) + d$z2
  pr <- stats::plogis(.sr_cov_calibrate(lp, 0.3) + lp)
  d$z1[stats::runif(n) < pr] <- NA
  d
}

.sr_cov_gen_logistic <- function(n) {
  z <- stats::rnorm(n)
  x <- stats::rbinom(n, 1L, stats::plogis(0.5 * z))
  y <- stats::rbinom(n, 1L, stats::plogis(-0.5 + 0.8 * x + 0.6 * z))
  data.frame(y = y, x = x, z = z)
}

# The true marginal risk difference of the logistic generator, by numerical integration over z.
.sr_cov_rd_truth <- function() {
  f <- function(z) (stats::plogis(-0.5 + 0.8 + 0.6 * z) - stats::plogis(-0.5 + 0.6 * z)) * stats::dnorm(z)
  stats::integrate(f, -Inf, Inf, rel.tol = 1e-10)$value
}

.sr_cov_gen_cluster <- function(n) {
  G <- 50L
  cl <- sort(rep_len(seq_len(G), n))
  xg <- sample(rep_len(0:1, G))
  u <- stats::rnorm(G, 0, sqrt(0.2))
  z <- stats::rnorm(n)
  y <- 1 + 0.5 * xg[cl] + 0.5 * z + u[cl] + stats::rnorm(n, 0, sqrt(0.8))
  data.frame(y = y, x = xg[cl], z = z, cl = cl)
}

# ---- one replication ------------------------------------------------------------------------------

.sr_cov_whole <- function(x, lo, what) {
  if (!(is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x == round(x) && x >= lo)) {
    stop(what, " must be a whole number of at least ", lo, call. = FALSE)
  }
  as.integer(x)
}

#' The compiled and checked body of `spec` on `route`, as sr_run would build it: stops with "<what>:
#' invalid spec: ..." on a spec that does not validate, or "<what>: the layers do not compose".
#' Shared with resample/feas_sim.R.
.sr_cov_compile <- function(route, spec, what) {
  v <- sr_spec_validate(spec, route)
  if (!v$ok) stop(what, ": invalid spec: ", paste(v$errors, collapse = "; "), call. = FALSE)
  cc <- sr_compose_check(spec)
  if (!cc$ok) stop(what, ": the layers do not compose", call. = FALSE)
  body <- sr_compile(route, spec)
  .sr_emit_check(body, "analysis")
  body
}

#' Everything a replication needs, built once per cell: the generator, the route, the validated spec
#' (spec$seed = `seed`; the preamble seed of each replication is drawn from its stream), the compiled
#' and checked body, the scored measure and the true value. Stops as sr_run would on a spec that does
#' not validate or a layer stack that does not compose.
.sr_cov_setup <- function(cell, n, B, m, seed) {
  def <- .sr_cov_defs()[[cell]]
  B <- if (is.null(B)) def$B else B
  m <- if (is.null(m)) def$m else m
  spec <- def$spec(B, m)
  spec$seed <- as.integer(seed)
  route <- def$route()
  body <- .sr_cov_compile(route, spec, paste("coverage cell", cell))
  list(cell = cell, n = as.integer(n), gen = def$gen, route = route, spec = spec, body = body,
       measure = def$measure, truth = def$truth())
}

#' The primary row of `setup$measure` from the route's own compiled body run on `df`: the run preamble
#' of `run_seed`, then setup$body evaluated in a fresh environment under the global env with sr_data
#' bound (as the sr_exec worker does), then setup$route$present() checked by sr_result_check. Returns
#' c(est, lo, hi); stops when the value is not plain data, there is not exactly one primary row of the
#' measure, or the row is not finite. Shared with resample/feas_sim.R.
.sr_cov_primary <- function(df, run_seed, setup) {
  env <- new.env(parent = globalenv())
  env$sr_data <- df
  value <- eval(parse(text = paste(sr_rng_preamble(run_seed), setup$body, sep = "\n")), envir = env)
  if (!.sr_value_plain(value)) stop("the result is not plain data", call. = FALSE)
  res <- sr_result_check(setup$route$present(value, setup$spec), setup$route)
  eff <- res$effects
  row <- eff[eff$role == "primary" & eff$measure == setup$measure, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop("expected one primary ", setup$measure, " row, found ", nrow(row), call. = FALSE)
  }
  out <- c(est = row$est, lo = row$lo, hi = row$hi)
  if (!all(is.finite(out))) stop("the primary ", setup$measure, " row is not finite", call. = FALSE)
  out
}

#' One replication from its L'Ecuyer stream `stream` (a .Random.seed vector): generate the data, draw
#' the preamble seed, run the compiled body in a fresh environment with sr_data bound (as the sr_exec
#' worker does), present and check the result, and return list(ok, est, lo, hi, error) for the
#' primary row of the scored measure. Any error, or a non-finite row, gives ok = FALSE with the
#' message. Warnings are not surfaced (as in the sr_exec worker). Sets the global RNG state; the
#' caller restores it.
.sr_cov_rep <- function(stream, setup) {
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  assign(".Random.seed", stream, envir = globalenv())
  tryCatch(suppressWarnings({
    df <- setup$gen(setup$n)
    run_seed <- sample.int(.Machine$integer.max, 1L)
    out <- .sr_cov_primary(df, run_seed, setup)
    list(ok = TRUE, est = out[["est"]], lo = out[["lo"]], hi = out[["hi"]], error = NA_character_)
  }), error = function(e) {
    list(ok = FALSE, est = NA_real_, lo = NA_real_, hi = NA_real_, error = conditionMessage(e))
  })
}

# ---- scoring --------------------------------------------------------------------------------------

#' Coverage, its Monte Carlo SE, pass, bias and mean width of the intervals [lo, hi] around `truth`
#' (successful replications only). No replication: everything NA and pass FALSE.
.sr_cov_summarise <- function(est, lo, hi, truth) {
  k <- length(est)
  if (!k) {
    return(list(coverage = NA_real_, mcse = NA_real_, pass = FALSE, bias = NA_real_, mean_width = NA_real_))
  }
  coverage <- mean(lo <= truth & truth <= hi)
  mcse <- sqrt(coverage * (1 - coverage) / k)
  list(coverage = coverage, mcse = mcse, pass = abs(coverage - 0.95) < 3 * mcse,
       bias = mean(est) - truth, mean_width = mean(hi - lo))
}

#' The result of a cell from its replications `out` (lists of .sr_cov_rep).
.sr_cov_collect <- function(cell, out, truth) {
  ok <- vapply(out, function(o) isTRUE(o$ok), NA)
  pick <- function(f) vapply(out[ok], function(o) o[[f]], 0)
  s <- .sr_cov_summarise(pick("est"), pick("lo"), pick("hi"), truth)
  errs <- vapply(out[!ok], function(o) o$error, "")
  tab <- table(factor(errs, levels = sort(unique(errs), method = "radix")))   # locale-free order
  list(cell = cell, reps = length(out), coverage = s$coverage, mcse = s$mcse, pass = s$pass,
       bias = s$bias, mean_width = s$mean_width, failed = sum(!ok), truth = truth,
       errors = if (length(errs)) sprintf("%s (%d)", names(tab), as.integer(tab)) else character())
}

# ---- the cell -------------------------------------------------------------------------------------

# Run the replications in this process, restoring the caller's RNG kind and seed afterwards. `fun`
# is the replication function (fun(item, setup)), applied to each of `streams` in order.
.sr_cov_serial <- function(streams, setup, fun = .sr_cov_rep) {
  genv <- globalenv()
  had_seed <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = genv, inherits = FALSE) else NULL
  old_kind <- RNGkind()
  on.exit({
    suppressWarnings(RNGkind(old_kind[1L], old_kind[2L], old_kind[3L]))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = genv)
    } else if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      rm(".Random.seed", envir = genv)
    }
  }, add = TRUE)
  lapply(streams, fun, setup = setup)
}

# A PSOCK worker takes this process's library paths (a portable bundle sets them at start-up) and
# loads the app core (the same code as this process).
.sr_cov_worker_init <- function(root, libs) {
  .libPaths(libs)
  source(file.path(root, "global.R"), local = FALSE, encoding = "UTF-8")
  TRUE
}

# Run the replications on a local PSOCK cluster of `workers` processes (results in replication order);
# `fun` as for .sr_cov_serial (a function of the global env, which the workers have loaded).
.sr_cov_parallel <- function(streams, setup, workers, fun = .sr_cov_rep) {
  cl <- parallel::makePSOCKcluster(min(workers, length(streams)))
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterCall(cl, .sr_cov_worker_init, sr_root, .libPaths())
  parallel::parLapplyLB(cl, streams, fun, setup = setup)
}

#' Run one coverage cell.
#' @param cell one of sr_coverage_cells()$cell
#' @param reps the number of replications (at least 2)
#' @param n rows per dataset (at least 100)
#' @param seed replication r uses the r-th L'Ecuyer-CMRG stream of this seed
#' @param B bootstrap replicates (bootstrap cells only; NULL: the cell's default)
#' @param workers processes; above 1 a PSOCK cluster runs the replications (same result)
#' @param m imputations (MI cells only; NULL: the cell's default)
#' @return list(cell, reps, coverage, mcse, pass, bias, mean_width, failed, truth, errors); see the
#'   file header for the definitions and for how failed replications enter.
sr_coverage_cell <- function(cell, reps, n = 200L, seed = 1L, B = NULL, workers = 1L, m = NULL) {
  defs <- .sr_cov_defs()
  if (!(is.character(cell) && length(cell) == 1L && !is.na(cell) && cell %in% names(defs))) {
    stop("unknown cell; one of: ", paste(names(defs), collapse = ", "), call. = FALSE)
  }
  reps <- .sr_cov_whole(reps, 2, "reps")
  n <- .sr_cov_whole(n, 100, "n")
  .sr_check_seed(seed)
  workers <- .sr_cov_whole(workers, 1, "workers")
  def <- defs[[cell]]
  if (!is.null(B)) {
    if (is.na(def$B)) stop("B applies only to the bootstrap cells (boot, boot_mi, cluster_boot)", call. = FALSE)
    B <- .sr_cov_whole(B, 20, "B")
  }
  if (!is.null(m)) {
    if (is.na(def$m)) stop("m applies only to the multiple-imputation cells (mi_mar, boot_mi)", call. = FALSE)
    m <- .sr_cov_whole(m, 2, "m")
  }
  setup <- .sr_cov_setup(cell, n, B, m, seed)
  streams <- sr_streams(seed, reps)
  out <- if (workers == 1L) .sr_cov_serial(streams, setup) else .sr_cov_parallel(streams, setup, workers)
  .sr_cov_collect(cell, out, setup$truth)
}
