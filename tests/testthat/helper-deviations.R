# Shared fixtures for test-deviations.R and the Analyses panel tests (moved unchanged).

.dv_layers <- function(...) {
  utils::modifyList(list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                         framework = "frequentist", clustered = FALSE), list(...))
}

.dv_mod <- function(s, ...) {
  mod <- list(...)
  for (nm in names(mod)) s[nm] <- list(mod[[nm]])
  s
}

.dv_cox <- function(...) {
  .dv_mod(list(purpose = "explain-causal", time = "time", status = "status", exposure = "x",
               layers = .dv_layers(), seed = 11L), ...)
}
.dv_lm <- function(...) {
  .dv_mod(list(purpose = "explain-causal", outcome = "y", exposure = "x", terms = list(list(var = "z")),
               layers = .dv_layers(), seed = 11L), ...)
}
.dv_logit <- function(...) {
  .dv_mod(list(purpose = "explain-causal", outcome = "y", exposure = "x", terms = list(list(var = "z")),
               layers = .dv_layers(), seed = 11L), ...)
}

# The routes, registered again when an earlier test file emptied the registry.
.dv_routes <- function() {
  make <- list(lm = .sr_lm_route, lm_describe = .sr_lm_describe_route, logistic = .sr_logistic_route,
               logistic_describe = .sr_logistic_describe_route, modpois = .sr_modpois_route,
               cox = .sr_cox_route, cox_describe = .sr_cox_describe_route, rmst = .sr_rmst_route,
               table1 = .sr_table1_route)
  for (k in names(make)) {
    if (!(.sr_route_key(k, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make[[k]]())
  }
  invisible(TRUE)
}

# Survival times with a binary exposure x: baseline hazard log(2), hazard ratio h1 before t = cut and
# h2 after it (piecewise exponential), administrative censoring at `cens` (Task 11 generator).
.dv_surv <- function(n, h1, h2, seed, cut = 1, cens = 4) {
  set.seed(seed)
  x <- rbinom(n, 1, 0.5)
  lam <- log(2)
  r1 <- lam * ifelse(x == 1, h1, 1)
  r2 <- lam * ifelse(x == 1, h2, 1)
  e <- rexp(n)
  t <- ifelse(e < r1 * cut, e / r1, cut + (e - r1 * cut) / r2)
  st <- as.integer(t <= cens)
  t <- pmin(t, cens)
  data.frame(time = t, status = st, x = x)
}

# A ready (not yet locked) project on the non-PH data: hazard ratio 3 before t = 1, 0.8 after.
.dv_project <- function(env = parent.frame()) {
  .dv_routes()
  d <- .dv_surv(400, 3, 0.8, 101)
  set.seed(102)
  d$z <- round(rnorm(400), 3)
  d$id <- seq_len(nrow(d))
  .fx_ready_project(d, "t", c(id = "id", status = "outcome"), env = env)
}
