# Tests for routes/result.R: the effects frame, sr_result_check and its flags.

fx_eff <- function(...) {
  base <- data.frame(
    term = c("trt", "age"), measure = c("HR", "HR"), est = c(0.8, 1.02), lo = c(0.6, 1.00),
    hi = c(1.1, 1.04), p = c(0.2, 0.03), scale = "ratio", level = c("B vs A", NA),
    mcse = NA_real_, role = c("primary", "covariate"), conditional = TRUE, ci_method = "wald",
    n = 100, events = 40, stringsAsFactors = FALSE
  )
  over <- list(...)
  for (nm in names(over)) base[[nm]] <- over[[nm]]
  base
}
fx_pres <- function(eff, tables = list()) {
  list(tables = tables, figures = list(), fragments = list(), effects = eff)
}
fx_route <- function(purpose = "explain-causal", family = "regression") {
  sr_route(id = "fake.reg", version = 1L, family = family, purpose = purpose,
           estimand = sr_estimand("adults", "x", "none", "hr"),
           spec_schema = list(), compile = function(spec) "1",
           present = function(value, spec) list())
}

test_that("sr_effects_empty has the contract columns, types and no rows", {
  e <- sr_effects_empty()
  expect_identical(names(e), SR_EFFECT_COLS)
  expect_identical(nrow(e), 0L)
  expect_type(e$conditional, "logical")
  expect_type(e$est, "double")
  expect_type(e$term, "character")
  expect_identical(SR_EFFECT_COLS, c("term", "measure", "est", "lo", "hi", "p", "scale", "level",
                                     "mcse", "role", "conditional", "ci_method", "n", "events"))
  # an empty frame is a valid effects table
  out <- sr_result_check(fx_pres(e), fx_route())
  expect_identical(nrow(out$flags), 0L)
  expect_identical(names(out$flags), c("term", "flag", "detail"))
})

test_that("sr_result_check passes a good result and returns it with flags", {
  out <- sr_result_check(fx_pres(fx_eff()), fx_route())
  expect_named(out, c("tables", "figures", "fragments", "effects", "flags"))
  expect_identical(names(out$effects), SR_EFFECT_COLS)
  expect_s3_class(out$flags, "data.frame")
  # columns arrive in any order and leave in the canonical one
  shuffled <- fx_eff()[, rev(SR_EFFECT_COLS)]
  expect_identical(names(sr_result_check(fx_pres(shuffled), fx_route())$effects), SR_EFFECT_COLS)
})

test_that("a missing effects column, lo > est or a non-finite primary CI is an error naming the row", {
  e <- fx_eff()
  expect_error(sr_result_check(fx_pres(e[, setdiff(names(e), "mcse")]), fx_route()), "mcse")
  expect_error(sr_result_check(fx_pres(cbind(e, extra = 1)), fx_route()), "extra")
  expect_error(sr_result_check(list(tables = list(), figures = list(), fragments = list()), fx_route()),
               "effects")
  expect_error(sr_result_check(fx_pres(list(a = 1)), fx_route()), "data.frame")
  expect_error(sr_result_check(fx_pres(fx_eff(lo = c(0.9, 1.0))), fx_route()), "trt.*lo <= est")
  expect_error(sr_result_check(fx_pres(fx_eff(hi = c(NA, 1.04))), fx_route()), "trt.*finite")
  expect_error(sr_result_check(fx_pres(fx_eff(est = c(Inf, 1.02), hi = c(Inf, 1.04))), fx_route()), "trt")
  expect_error(sr_result_check(fx_pres(fx_eff(est = c(NA, 1.02))), fx_route()), "trt")
  expect_error(sr_result_check(fx_pres(fx_eff(hi = c(0.7, 1.04))), fx_route()), "trt")
  # only primary rows are held to it
  ok <- fx_eff(lo = c(0.6, NA), hi = c(1.1, NA))
  expect_no_error(sr_result_check(fx_pres(ok), fx_route()))
})

test_that("a describe-family route accepts NA CIs, also on a primary row", {
  e <- fx_eff(measure = c("SMD", "SMD"), role = c("descriptive", "descriptive"), lo = NA_real_,
              hi = NA_real_, conditional = FALSE)
  out <- sr_result_check(fx_pres(e), fx_route(purpose = "describe", family = "describe"))
  expect_identical(nrow(out$flags), 0L)
  e2 <- fx_eff(measure = c("SMD", "SMD"), role = c("primary", "descriptive"), lo = NA_real_,
               hi = NA_real_, conditional = FALSE)
  expect_no_error(sr_result_check(fx_pres(e2), fx_route(purpose = "describe", family = "describe")))
  # the same rows are refused for a non-describe route
  expect_error(sr_result_check(fx_pres(e2), fx_route(purpose = "predict", family = "regression")), "finite")
})

test_that("covariate rows of an explain-causal route get table2; other purposes do not", {
  e <- fx_eff(conditional = FALSE, measure = c("RD", "coef"), scale = c("difference", "difference"))
  out <- sr_result_check(fx_pres(e), fx_route())
  expect_identical(out$flags$term, "age")
  expect_identical(out$flags$flag, "table2")
  expect_true(nzchar(out$flags$detail))
  out2 <- sr_result_check(fx_pres(e), fx_route(purpose = "predict"))
  expect_identical(nrow(out2$flags), 0L)
})

test_that("a conditional OR/HR gets non_collapsible; a marginal RD/RR does not", {
  e <- fx_eff(measure = c("HR", "HR"), conditional = c(TRUE, FALSE), role = c("primary", "primary"),
              lo = c(0.6, 0.9), est = c(0.8, 1.0), hi = c(1.1, 1.2))
  out <- sr_result_check(fx_pres(e), fx_route())
  expect_identical(out$flags$term, "trt")
  expect_identical(out$flags$flag, "non_collapsible")
  expect_match(out$flags$detail, "conditional; see the marginal RD/RR", fixed = TRUE)
  m <- fx_eff(measure = c("RD", "RR"), scale = c("difference", "ratio"), conditional = FALSE,
              role = c("primary", "primary"), lo = c(-0.1, 0.5), est = c(-0.05, 0.8), hi = c(0.0, 1.2))
  expect_identical(nrow(sr_result_check(fx_pres(m), fx_route())$flags), 0L)
  # NA conditional is not conditional
  m2 <- fx_eff(measure = c("OR", "OR"), conditional = c(NA, FALSE), role = c("primary", "primary"),
               lo = c(0.6, 0.9), est = c(0.8, 1.0), hi = c(1.1, 1.2))
  expect_identical(nrow(sr_result_check(fx_pres(m2), fx_route())$flags), 0L)
  # a covariate that is a conditional HR gets both flags
  both <- sr_result_check(fx_pres(fx_eff()), fx_route())
  expect_setequal(paste(both$flags$term, both$flags$flag),
                  c("trt non_collapsible", "age non_collapsible", "age table2"))
})

test_that("sr_result_check is idempotent and ignores a stale flags element", {
  p <- fx_pres(fx_eff())
  p$flags <- data.frame(term = "zz", flag = "stale", detail = "x")
  once <- sr_result_check(p, fx_route())
  expect_false("zz" %in% once$flags$term)
  expect_identical(sr_result_check(once, fx_route()), once)
})

test_that("unknown measure, scale or role values and p-value columns are refused", {
  expect_error(sr_result_check(fx_pres(fx_eff(measure = c("HR", "ZZ"))), fx_route()), "measure.*age")
  expect_error(sr_result_check(fx_pres(fx_eff(role = c("primary", "other"))), fx_route()), "role")
  expect_error(sr_result_check(fx_pres(fx_eff(scale = c("ratio", "log"))), fx_route()), "scale")
  expect_error(sr_result_check(fx_pres(fx_eff(conditional = "yes")), fx_route()), "logical")
  expect_error(sr_result_check(fx_pres(fx_eff(est = c("a", "b"))), fx_route()), "numeric")
  tb <- list(coef = data.frame(term = "a", p.value = 0.1))
  expect_error(sr_result_check(fx_pres(fx_eff(), tb), fx_route()), "p-value column")
  ok <- list(coef = data.frame(term = "a", p = 0.1))
  expect_no_error(sr_result_check(fx_pres(fx_eff(), ok), fx_route()))
})
