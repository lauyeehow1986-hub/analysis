# resample/feas_sim.R - the feasibility SE validation (spec catalogue O): the null-effect standard
# error the feasibility engine predicts (sap/feas_engine.R) against the empirical SD of the route's own
# estimates over simulated datasets. In-process; no callr, no project.
#
# A cell is a null-effect data generator plus one analysis spec of a model route. A replication draws
# a dataset of n rows and runs the route's OWN compiled code on it exactly as the coverage harness does
# (resample/coverage.R: .sr_cov_compile, .sr_cov_primary), keeping the estimate of the route's primary
# row of the cell's measure (log scale for a ratio). On every `pred_every`-th replication (1, 1 +
# pred_every, ...) it also runs the engine on the same dataset -- .sr_feas_inputs(df, spec, route) then
# .sr_feas_compute(inputs, spec, route, assumptions = the generator's TRUE r2_y / icc) -- and keeps the
# predicted SE of the measure's row.
#
# Cells (every one null-effect; reps 1000). Outside the clustered cells the exposure depends on a
# covariate, x ~ Bernoulli(plogis(0.8 z1)) with z1 ~ N(0, 1), so the exposure model has r2x > 0; in the
# clustered cells the exposure is at cluster level (exactly half of the clusters, at random).
#   md      n 400  z1, z2 ~ N(0,1); y = 1 + 0.8 z1 + 0.4 z2 + N(0,1)        lm y ~ x + z1 + z2, MD row
#                  (true r2_y = 0.8 / 1.8)
#   rd      n 600  y ~ Bernoulli(0.3)                                       logistic y ~ x + z1, marginal RD row
#   rr      n 600  as rd                                                    modpois marginal = FALSE, RR row
#   or      n 600  as rd                                                    logistic marginal = FALSE, OR row
#   hr      n 400  T ~ Exp(1), C ~ U(0, c_max), c_max solving               cox ~ x + z1, HR row
#                  (1 - exp(-c)) / c = 0.3 (30% censored); time = min(T, C)
#   rmst    n 400  as hr but x ~ Bernoulli(0.5) and no covariate; tau = the rmst by x, RMST_diff row
#                  75th percentile of the observed time min(T, C), solving    (prediction every 10th rep)
#                  exp(-t) (1 - t / c_max) = 0.25
#   md_cl   n 400  40 clusters x 10; z ~ N(0,1); y = 1 + 0.5 z + u + e,     lm y ~ x + z, se = "cluster"
#                  u ~ N(0, 0.2), e ~ N(0, 0.8); r2_y = 0.2, icc = 0.2
#   rd_cl   n 600  40 clusters x 15; cluster risk ~ Beta(5.7, 13.3) (mean   as rd, se = "cluster"
#   rr_cl          0.3, a + b = 19, ICC 1 / (a + b + 1) = 0.05); y ~         as rr, se = "cluster"
#   or_cl          Bernoulli(cluster risk); z ~ N(0,1) unrelated; icc 0.05   as or, se = "cluster"
#   hr_cl   n 400  40 clusters x 10; gamma frailty w ~ Gamma(shape 1/0.3,   cox ~ x + z, layers$clustered
#                  rate 1/0.3) (variance 0.3); T ~ Exp(rate w); administrative
#                  censoring at t = 1.5; z ~ N(0,1) unrelated; icc = SR_FEAS_SIM_HR_CL_ICC
# Ruling: rmst predicts on every 10th replication only. Its prediction is itself a 500-draw simulation
# (option sr.feas_rmst_sims), and the mean of 100 predictions estimates the mean predicted SE to well
# under 1%. Clustered RMST is refused by the route, so it has no cell.
#
# Random numbers: replication r ALWAYS uses the r-th L'Ecuyer-CMRG stream of `seed` (sr_streams). From
# it the replication draws its dataset, then one integer, the seed of the run preamble its analysis
# code runs under (as in the coverage harness), then one more integer, the engine seed of its
# prediction: the prediction runs with spec$seed set to it, so the engine's RMST simulation (which takes
# its own stream from spec$seed and restores the RNG) has independent Monte Carlo error in every
# replication and averaging the predictions reduces it. The two extra draws come after the dataset, so
# they never change the data. So a result depends only on (cell, reps, n, seed), never on the number of
# workers. The caller's RNG kind and .Random.seed are restored on exit.
#
# Scoring, over the replications that succeeded:
#   emp_sd = SD of the estimates (log scale for OR, RR, HR), pred_se = mean of the predicted SEs,
#   ratio = pred_se / emp_sd, pass = |ratio - 1| <= 0.10.
# A replication whose generation, fit, presentation or prediction errors -- a non-finite primary row,
# a verdict with blockers, or no finite predicted SE -- is FAILED: it is excluded from both statistics,
# counted in `failed`, with its messages in `errors` ("<message> (<count>)").

# The event-indicator ICC of the hr_cl generator (events 1{T <= 1.5}, clusters of 10, gamma frailty of
# variance 0.3), computed once by the one-way ANOVA estimator (MSB - MSW) / (MSB + (m - 1) MSW) on
# 400 000 draws (40 000 clusters of 10; set.seed(1, kind = "L'Ecuyer-CMRG"); event share 0.7116). The
# closed form (.sr_fsim_hr_cl_icc_exact) gives 0.16386, within the Monte Carlo error (the ANOVA
# estimator's SE at 40 000 clusters of 10 is about 0.0015); the smoke test checks the two agree within
# 0.005 (about 3 SE).
SR_FEAS_SIM_HR_CL_ICC <- 0.162553

# The closed-form event-indicator ICC of the hr_cl generator: P(event | w) = 1 - exp(-t w) with
# t = 1.5, and E exp(-s w) = (1 + v s)^(-1 / v) for w ~ Gamma(shape 1/v, rate 1/v), v = 0.3. With
# a1 = E exp(-t w), a2 = E exp(-2 t w) and p = 1 - a1, the ICC is Var(P(event | w)) / (p (1 - p)) =
# (a2 - a1^2) / (p (1 - p)). Audits SR_FEAS_SIM_HR_CL_ICC; not used by the simulation.
.sr_fsim_hr_cl_icc_exact <- function(t = 1.5, v = 0.3) {
  a1 <- (1 + v * t)^(-1 / v)
  a2 <- (1 + v * 2 * t)^(-1 / v)
  p <- 1 - a1
  (a2 - a1^2) / (p * (1 - p))
}

SR_FEAS_SIM_TOL <- 0.10                  # pass: |pred_se / emp_sd - 1| <= this

# ---- the generators -------------------------------------------------------------------------------

# The upper limit of the uniform censoring time that censors 30% of Exp(1) event times.
.sr_fsim_cmax <- function() {
  stats::uniroot(function(c) (1 - exp(-c)) / c - 0.3, c(0.5, 20), tol = 1e-12)$root
}

# The 75th percentile of the observed time min(T, C), T ~ Exp(1), C ~ U(0, c_max).
.sr_fsim_tau <- function() {
  cm <- .sr_fsim_cmax()
  stats::uniroot(function(t) exp(-t) * (1 - t / cm) - 0.25, c(1e-6, cm - 1e-6), tol = 1e-12)$root
}

.sr_fsim_gen_md <- function(n) {
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  x <- stats::rbinom(n, 1L, stats::plogis(0.8 * z1))
  y <- 1 + 0.8 * z1 + 0.4 * z2 + stats::rnorm(n)
  data.frame(y = y, x = x, z1 = z1, z2 = z2)
}

.sr_fsim_gen_bin <- function(n) {
  z1 <- stats::rnorm(n)
  x <- stats::rbinom(n, 1L, stats::plogis(0.8 * z1))
  y <- stats::rbinom(n, 1L, 0.3)
  data.frame(y = y, x = x, z1 = z1)
}

# (time, status) of n Exp(1) times under U(0, c_max) censoring.
.sr_fsim_surv <- function(n) {
  t <- stats::rexp(n)
  c <- stats::runif(n, 0, .sr_fsim_cmax())
  list(time = pmin(t, c), status = as.integer(t <= c))
}

.sr_fsim_gen_hr <- function(n) {
  z1 <- stats::rnorm(n)
  x <- stats::rbinom(n, 1L, stats::plogis(0.8 * z1))
  s <- .sr_fsim_surv(n)
  data.frame(time = s$time, status = s$status, x = x, z1 = z1)
}

.sr_fsim_gen_rmst <- function(n) {
  x <- stats::rbinom(n, 1L, 0.5)
  s <- .sr_fsim_surv(n)
  data.frame(time = s$time, status = s$status, x = x)
}

# The cluster of each of n rows (G clusters of near-equal size, sorted) and the cluster-level exposure
# (exactly half of the clusters, at random).
.sr_fsim_clusters <- function(n, G) {
  list(cl = sort(rep_len(seq_len(G), n)), xg = sample(rep_len(0:1, G)))
}

.sr_fsim_gen_md_cl <- function(n) {
  k <- .sr_fsim_clusters(n, 40L)
  u <- stats::rnorm(40L, 0, sqrt(0.2))
  z <- stats::rnorm(n)
  y <- 1 + 0.5 * z + u[k$cl] + stats::rnorm(n, 0, sqrt(0.8))
  data.frame(y = y, x = k$xg[k$cl], z = z, cl = k$cl)
}

.sr_fsim_gen_bin_cl <- function(n) {
  k <- .sr_fsim_clusters(n, 40L)
  pg <- stats::rbeta(40L, 5.7, 13.3)
  z <- stats::rnorm(n)
  y <- stats::rbinom(n, 1L, pg[k$cl])
  data.frame(y = y, x = k$xg[k$cl], z = z, cl = k$cl)
}

.sr_fsim_gen_hr_cl <- function(n) {
  k <- .sr_fsim_clusters(n, 40L)
  w <- stats::rgamma(40L, shape = 1 / 0.3, rate = 1 / 0.3)
  z <- stats::rnorm(n)
  t <- stats::rexp(n, rate = w[k$cl])
  data.frame(time = pmin(t, 1.5), status = as.integer(t <= 1.5), x = k$xg[k$cl], z = z, cl = k$cl)
}

# ---- the cells ------------------------------------------------------------------------------------

.sr_fsim_spec <- function(route_id, clustered, ...) {
  surv <- route_id %in% c("cox", "rmst")
  base <- list(purpose = "explain-causal", exposure = "x",
               layers = .sr_cov_layers(clustered = clustered))
  base <- if (surv) c(base, list(time = "time", status = "status", status_event = 1)) else c(base, list(outcome = "y"))
  if (clustered) base$cluster <- "cl"
  utils::modifyList(base, list(...))
}

.sr_fsim_assume <- function(r2_y = NULL, icc = NULL) {
  out <- list()
  if (!is.null(r2_y)) out$r2_y <- list(value = r2_y, justification = "the generator's true R2 of the covariates")
  if (!is.null(icc)) out$icc <- list(value = icc, justification = "the generator's true ICC")
  out
}

# The cell definitions: route id and constructor, the scored measure, generator, spec builder, the
# generator's true assumptions, n and pred_every.
.sr_fsim_defs <- function() {
  z1 <- list(list(var = "z1"))
  z <- list(list(var = "z"))
  cell <- function(route, ctor, measure, gen, spec, assumptions = list(), n, pred_every = 1L, clustered = FALSE) {
    list(route = route, ctor = ctor, measure = measure, gen = gen, spec = spec, assumptions = assumptions,
         n = as.integer(n), pred_every = as.integer(pred_every), clustered = clustered, reps = 1000L)
  }
  list(
    md = cell("lm", .sr_lm_route, "MD", .sr_fsim_gen_md,
              function() .sr_fsim_spec("lm", FALSE, terms = list(list(var = "z1"), list(var = "z2"))),
              .sr_fsim_assume(r2_y = 0.8 / 1.8), n = 400L),
    rd = cell("logistic", .sr_logistic_route, "RD", .sr_fsim_gen_bin,
              function() .sr_fsim_spec("logistic", FALSE, terms = z1, marginal = TRUE), n = 600L),
    rr = cell("modpois", .sr_modpois_route, "RR", .sr_fsim_gen_bin,
              function() .sr_fsim_spec("modpois", FALSE, terms = z1, marginal = FALSE), n = 600L),
    or = cell("logistic", .sr_logistic_route, "OR", .sr_fsim_gen_bin,
              function() .sr_fsim_spec("logistic", FALSE, terms = z1, marginal = FALSE), n = 600L),
    hr = cell("cox", .sr_cox_route, "HR", .sr_fsim_gen_hr,
              function() .sr_fsim_spec("cox", FALSE, terms = z1), n = 400L),
    rmst = cell("rmst", .sr_rmst_route, "RMST_diff", .sr_fsim_gen_rmst,
                function() .sr_fsim_spec("rmst", FALSE, tau = .sr_fsim_tau()), n = 400L, pred_every = 10L),
    md_cl = cell("lm", .sr_lm_route, "MD", .sr_fsim_gen_md_cl,
                 function() .sr_fsim_spec("lm", TRUE, terms = z, se = "cluster"),
                 .sr_fsim_assume(r2_y = 0.2, icc = 0.2), n = 400L, clustered = TRUE),
    rd_cl = cell("logistic", .sr_logistic_route, "RD", .sr_fsim_gen_bin_cl,
                 function() .sr_fsim_spec("logistic", TRUE, terms = z, se = "cluster", marginal = TRUE),
                 .sr_fsim_assume(icc = 0.05), n = 600L, clustered = TRUE),
    rr_cl = cell("modpois", .sr_modpois_route, "RR", .sr_fsim_gen_bin_cl,
                 function() .sr_fsim_spec("modpois", TRUE, terms = z, se = "cluster", marginal = FALSE),
                 .sr_fsim_assume(icc = 0.05), n = 600L, clustered = TRUE),
    or_cl = cell("logistic", .sr_logistic_route, "OR", .sr_fsim_gen_bin_cl,
                 function() .sr_fsim_spec("logistic", TRUE, terms = z, se = "cluster", marginal = FALSE),
                 .sr_fsim_assume(icc = 0.05), n = 600L, clustered = TRUE),
    hr_cl = cell("cox", .sr_cox_route, "HR", .sr_fsim_gen_hr_cl,
                 function() .sr_fsim_spec("cox", TRUE, terms = z),
                 .sr_fsim_assume(icc = SR_FEAS_SIM_HR_CL_ICC), n = 400L, clustered = TRUE)
  )
}

#' The feasibility SE-validation cells: data.frame(cell, route, measure, clustered, reps, n,
#' pred_every), with the default replications and rows of each.
sr_feas_sim_cells <- function() {
  d <- .sr_fsim_defs()
  data.frame(cell = names(d), route = vapply(d, function(x) x$route, "", USE.NAMES = FALSE),
             measure = vapply(d, function(x) x$measure, "", USE.NAMES = FALSE),
             clustered = vapply(d, function(x) x$clustered, NA, USE.NAMES = FALSE),
             reps = vapply(d, function(x) x$reps, 0L, USE.NAMES = FALSE),
             n = vapply(d, function(x) x$n, 0L, USE.NAMES = FALSE),
             pred_every = vapply(d, function(x) x$pred_every, 0L, USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}

# ---- one replication ------------------------------------------------------------------------------

# Everything a replication needs, built once per cell: the generator, the route, the validated spec
# (spec$seed = `seed`), the compiled and checked body, the scored measure and the true assumptions.
.sr_fsim_setup <- function(cell, n, seed) {
  def <- .sr_fsim_defs()[[cell]]
  spec <- def$spec()
  spec$seed <- as.integer(seed)
  route <- def$ctor()
  body <- .sr_cov_compile(route, spec, paste("feasibility cell", cell))
  list(cell = cell, n = as.integer(n), gen = def$gen, route = route, spec = spec, body = body,
       measure = def$measure, assumptions = def$assumptions)
}

# The engine's predicted SE of setup$measure on `df` (log scale for a ratio), with the engine seed
# `pred_seed` (spec$seed; it seeds the RMST simulation). Stops when the verdict has blockers or the
# measure has no single finite SE.
.sr_fsim_predict <- function(df, setup, pred_seed) {
  spec <- setup$spec
  spec$seed <- as.integer(pred_seed)
  inputs <- .sr_feas_inputs(df, spec, setup$route)
  v <- .sr_feas_compute(inputs, spec, setup$route, assumptions = setup$assumptions)
  if (length(v$blockers)) stop("the feasibility verdict has blockers: ", paste(v$blockers, collapse = "; "), call. = FALSE)
  se <- v$contrasts$se[v$contrasts$measure == setup$measure]
  if (length(se) != 1L || !is.finite(se)) stop("no single finite predicted ", setup$measure, " SE", call. = FALSE)
  se
}

#' One replication: `item` is list(stream, predict), stream a .Random.seed vector. Generates the data,
#' draws the preamble seed and the engine seed, runs the route (.sr_cov_primary) and, when
#' item$predict, the engine with its own seed.
#' Returns list(ok, est, pred, error): est on the log scale for a ratio, pred NA when not predicted.
#' Any error gives ok = FALSE with the message. Warnings are not surfaced. Sets the global RNG state;
#' the caller restores it.
.sr_fsim_rep <- function(item, setup) {
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  assign(".Random.seed", item$stream, envir = globalenv())
  tryCatch(suppressWarnings({
    df <- setup$gen(setup$n)
    run_seed <- sample.int(.Machine$integer.max, 1L)
    pred_seed <- sample.int(.Machine$integer.max, 1L)
    est <- .sr_cov_primary(df, run_seed, setup)[["est"]]
    if (.sr_feas_ratio(setup$measure)) est <- log(est)
    if (!is.finite(est)) stop("the primary ", setup$measure, " estimate is not finite on its scale", call. = FALSE)
    pred <- if (isTRUE(item$predict)) .sr_fsim_predict(df, setup, pred_seed) else NA_real_
    list(ok = TRUE, est = est, pred = pred, error = NA_character_)
  }), error = function(e) {
    list(ok = FALSE, est = NA_real_, pred = NA_real_, error = conditionMessage(e))
  })
}

# ---- scoring --------------------------------------------------------------------------------------

#' The result of a cell from its replications `out` (lists of .sr_fsim_rep).
.sr_fsim_collect <- function(cell, out) {
  ok <- vapply(out, function(o) isTRUE(o$ok), NA)
  est <- vapply(out[ok], function(o) o$est, 0)
  pred <- vapply(out[ok], function(o) o$pred, 0)
  pred <- pred[!is.na(pred)]
  emp_sd <- if (length(est) >= 2L) stats::sd(est) else NA_real_
  pred_se <- if (length(pred)) mean(pred) else NA_real_
  ratio <- pred_se / emp_sd
  errs <- vapply(out[!ok], function(o) o$error, "")
  tab <- table(factor(errs, levels = sort(unique(errs), method = "radix")))   # locale-free order
  list(cell = cell, reps = length(out), pred_se = pred_se, emp_sd = emp_sd, ratio = ratio,
       pass = isTRUE(abs(ratio - 1) <= SR_FEAS_SIM_TOL), failed = sum(!ok),
       errors = if (length(errs)) sprintf("%s (%d)", names(tab), as.integer(tab)) else character())
}

# ---- the cell -------------------------------------------------------------------------------------

#' Run one feasibility SE-validation cell.
#' @param cell one of sr_feas_sim_cells()$cell
#' @param reps the number of replications (at least 2; NULL: the cell's default, 1000)
#' @param n rows per dataset (at least 100; NULL: the cell's default)
#' @param seed replication r uses the r-th L'Ecuyer-CMRG stream of this seed
#' @param workers processes; above 1 a PSOCK cluster runs the replications (same result)
#' @return list(cell, reps, pred_se, emp_sd, ratio, pass, failed, errors); see the file header.
sr_feas_sim_cell <- function(cell, reps = NULL, n = NULL, seed = 1L, workers = 1L) {
  defs <- .sr_fsim_defs()
  if (!(is.character(cell) && length(cell) == 1L && !is.na(cell) && cell %in% names(defs))) {
    stop("unknown cell; one of: ", paste(names(defs), collapse = ", "), call. = FALSE)
  }
  def <- defs[[cell]]
  reps <- .sr_cov_whole(if (is.null(reps)) def$reps else reps, 2, "reps")
  n <- .sr_cov_whole(if (is.null(n)) def$n else n, 100, "n")
  .sr_check_seed(seed)
  workers <- .sr_cov_whole(workers, 1, "workers")
  setup <- .sr_fsim_setup(cell, n, seed)
  streams <- sr_streams(seed, reps)
  items <- lapply(seq_len(reps), function(r) list(stream = streams[[r]], predict = (r - 1L) %% def$pred_every == 0L))
  out <- if (workers == 1L) {
    .sr_cov_serial(items, setup, fun = .sr_fsim_rep)
  } else {
    .sr_cov_parallel(items, setup, workers, fun = .sr_fsim_rep)
  }
  .sr_fsim_collect(cell, out)
}
