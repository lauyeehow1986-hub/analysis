# Tests for router/gate.R: the router gate of the SAP lock and the router part of the SAP hash. The REAL
# nodes are used (no isolation). Synthetic data only.

# 300 rows; death alternates 0/1; bmi_6m is a function of death (the planted post-outcome column).
.rg_df <- function() {
  n <- 300L
  death <- rep(0:1, n / 2L)
  data.frame(id = seq_len(n), age = round(50 + 10 * sin(seq_len(n)), 1),
             sex = rep(c(0L, 0L, 1L, 1L), n / 4L), bmi_6m = death * 3L + 2L, death = death,
             stringsAsFactors = FALSE)
}

.rg_roles <- c(id = "id", age = "covariate", bmi_6m = "covariate", death = "outcome", sex = "exposure")

# A project with a primary logistic analysis adjusting for age and bmi_6m (not routed, not feasible).
.rg_project <- function(env = parent.frame()) {
  if (!(.sr_route_key("logistic", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
    sr_route_register(.sr_logistic_route())
  }
  p <- .fx_ready_project(.rg_df(), "t", .rg_roles, env = env)
  layers <- list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                 framework = "frequentist", clustered = FALSE)
  spec <- list(purpose = "explain-causal", outcome = "death", exposure = "sex",
               terms = list(list(var = "age"), list(var = "bmi_6m")), layers = layers, seed = 1L)
  sr_sap_add(p, "a1", "logistic", spec, "t", label = "primary")
}

.rg_lock_msg <- function(p) tryCatch({ sr_sap_lock(p); "" }, error = function(e) conditionMessage(e))

.rg_row <- function(p, item) {
  g <- .sr_router_gate(p)
  g[g$item == item, , drop = FALSE]
}

test_that("the lock needs a decided design and purpose", {
  p <- .fx_feasible(.rg_project(), routed = FALSE)
  msg <- .rg_lock_msg(p)
  expect_match(msg, "study design", fixed = TRUE)
  expect_match(msg, "study purpose", fixed = TRUE)
  expect_match(msg, "sr_router_decide(p, 'design'", fixed = TRUE)
  expect_identical(.sr_router_gate(p)$item[1:2], c("study design", "study purpose"))
})

test_that("a stale design or purpose blocks the lock", {
  p <- .fx_feasible(.rg_project())
  expect_identical(.rg_row(p, "study design")$status, "ok")
  expect_identical(.rg_row(p, "study design")$detail, "cohort_retrospective")
  p <- sr_pico_set(p, population = "test population", outcome = "test outcome", aim = "predict")
  expect_match(.rg_lock_msg(p), "changed inputs", fixed = TRUE)
  expect_identical(.rg_row(p, "study purpose")$status, "block")
})

test_that("a primary causal analysis may not adjust for a post-outcome covariate", {
  p <- .rg_project()
  p <- .fx_feasible(p)
  p <- sr_router_decide(p, "covariate_timing", "post_outcome", reason = "measured after death",
                        table = "t", column = "bmi_6m")
  msg <- .rg_lock_msg(p)
  expect_match(msg, "bmi_6m", fixed = TRUE)
  expect_match(msg, "post_outcome", fixed = TRUE)
  expect_identical(.rg_row(p, "covariate timing")$status[1L], "block")
  p <- sr_router_decide(p, "covariate_timing", "baseline", reason = "measured at enrolment", table = "t",
                        column = "bmi_6m", override = TRUE)
  expect_identical(.rg_lock_msg(p), "")
  expect_true(any(.rg_row(p, "covariate timing")$status == "warn"))   # age has no timing decision
  expect_false(sr_blind(sr_sap_lock(p)))
})

test_that("purpose and route mismatches warn", {
  p <- .fx_feasible(.rg_project())
  p <- sr_router_decide(p, "purpose", "describe", reason = "descriptive aim")
  r <- .rg_row(p, "purpose fits the analyses")
  expect_identical(r$status, "warn")
  expect_match(r$detail, "a1", fixed = TRUE)
  expect_match(r$detail, "logistic", fixed = TRUE)
  expect_match(r$detail, "explain-causal", fixed = TRUE)
  expect_match(r$detail, "describe", fixed = TRUE)
  expect_identical(.rg_lock_msg(p), "")
  expect_false(sr_blind(sr_sap_lock(p)))
})

test_that("a decided primary route that no primary analysis uses warns", {
  p <- .fx_feasible(.rg_project())
  expect_identical(nrow(.rg_row(p, "primary route")), 1L)
  expect_identical(.rg_row(p, "primary route")$status, "ok")
  p <- sr_router_decide(p, "primary_route", "lm", reason = "treat as continuous", table = "t")
  r <- .rg_row(p, "primary route")
  expect_identical(r$status, "warn")
  expect_match(r$detail, "lm", fixed = TRUE)
})

test_that("the hash covers the question and the decisions", {
  p <- .fx_feasible(.rg_project())
  h1 <- sr_sap_hash(p)
  p2 <- sr_router_decide(p, "design", "cohort_retrospective", reason = "a different reason")
  expect_false(identical(sr_sap_hash(p2), h1))
  p3 <- sr_pico_set(p, population = "test population", outcome = "test outcome", setting = "ward")
  expect_false(identical(sr_sap_hash(p3), h1))
  # a project with no pico.set and no decision keeps its pre-change hash
  q <- .rg_project()
  expect_null(.sr_router_hash_part(q))
  by_id <- function(x) if (length(x)) unname(x[order(names(x), method = "radix")]) else list()
  an <- .sr_sap_analyses(q)
  parts <- list(analyses = by_id(lapply(an, function(a) a)),
                families = by_id(q$state$sap$families %||% list()),
                rules = by_id(q$state$sap$rules %||% list()))
  expect_identical(sr_sap_hash(q), .sr_sha256(sr_canonical_json(parts)))
  # the locked hash is the recomputed one
  expect_identical(sr_sap_hash(sr_sap_lock(p)), sr_sap_hash(p))
})

test_that("existing lock fixtures still lock", {
  p <- sr_sap_lock(.fx_feasible(.rg_project()))
  expect_false(sr_blind(p))
  expect_identical(p$state$sap$lock$sap_hash, sr_sap_hash(p))
})
