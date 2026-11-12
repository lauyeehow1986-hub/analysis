# P1e.2 Task 1: the survival spec fields cox_method / pem_cuts / stream_chunk, their validation, the engine
# mapping and the form entries. Synthetic spec values only; no data is read.

.sf_layers <- function(...) {
  x <- list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
            framework = "frequentist", clustered = FALSE)
  m <- list(...)
  for (n in names(m)) x[[n]] <- m[[n]]
  x
}

.sf_spec <- function(route = "cox", ...) {
  s <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 2, exposure = "sex",
            layers = .sf_layers(), seed = 7L)
  if (identical(route, "rmst")) s$tau <- 365
  m <- list(...)
  for (n in names(m)) s[n] <- list(m[[n]])
  s
}

.sf_route <- function(id) {
  mk <- list(cox = .sr_cox_route, cox_describe = .sr_cox_describe_route, rmst = .sr_rmst_route)
  if (!(.sr_route_key(id, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(mk[[id]]())
  sr_route_get(id, 1L)
}

.sf_val <- function(spec, id = "cox") sr_spec_validate(spec, .sf_route(id))
.sf_err <- function(v) paste(v$errors, collapse = " ")

test_that("cox and rmst accept the stream engine", {
  for (id in c("cox", "cox_describe", "rmst")) {
    s <- .sf_spec(if (identical(id, "rmst")) "rmst" else "cox", engine = "stream", stream_chunk = 500)
    if (identical(id, "cox_describe")) s$purpose <- "describe"
    v <- .sf_val(s, id)
    expect_true(v$ok, info = paste(id, .sf_err(v)))
    expect_false(grepl("arrives in P1e.2", .sf_err(v), fixed = TRUE), info = id)
  }
  expect_null(.sr_engine_refusal("cox", list(engine = "stream")))
  expect_null(.sr_engine_refusal("rmst", list()))
  expect_true("cox_method" %in% names(.sf_route("cox")$spec_schema))
  expect_identical(.sf_route("cox")$spec_schema$cox_method$values, c("exact", "pem"))
  expect_identical(.sf_route("cox_describe")$spec_schema, .sf_route("cox")$spec_schema)
  expect_true("stream_chunk" %in% names(.sf_route("rmst")$spec_schema))
  expect_true(.sf_val(.sf_spec("cox", cox_method = "pem"))$ok)
  expect_false(.sf_val(.sf_spec("cox", cox_method = "weibull"))$ok)
})

test_that("method and cut accessors default to exact and quantile 10", {
  expect_identical(.sr_cox_method_of(list()), "exact")
  expect_identical(.sr_cox_method_of(list(cox_method = "pem")), "pem")
  expect_identical(.sr_pem_cuts_of(list()), list(rule = "quantile", k = 10))
  cu <- list(rule = "explicit", at = c(1, 2))
  expect_identical(.sr_pem_cuts_of(list(pem_cuts = cu)), cu)
})

test_that("pem_cuts needs cox_method pem", {
  v <- .sf_val(.sf_spec("cox", pem_cuts = list(rule = "quantile", k = 5)))
  expect_false(v$ok)
  expect_match(.sf_err(v), "pem_cuts needs cox_method = \"pem\"", fixed = TRUE)
  v2 <- .sf_val(.sf_spec("cox", cox_method = "exact", pem_cuts = list(rule = "quantile", k = 5)))
  expect_false(v2$ok)
  expect_true(.sf_val(.sf_spec("cox", cox_method = "pem", pem_cuts = list(rule = "quantile", k = 5)))$ok)
  expect_true(.sf_val(.sf_spec("cox", cox_method = "pem"))$ok)
})

test_that("explicit cuts must be increasing finite and positive", {
  msg <- "pem_cuts$at must be increasing, finite numbers above zero"
  for (at in list(c(2, 1), c(1, 1), c(0, 1), c(1, Inf), "a")) {
    s <- .sf_spec("cox", cox_method = "pem", pem_cuts = list(rule = "explicit", at = at))
    v <- .sf_val(s)
    expect_false(v$ok, info = paste(at, collapse = ","))
    # An infinite cut never reaches the cross-field check through sr_spec_validate: the spec cannot be
    # serialised canonically (non-finite number), which fails first. The check itself still names it.
    if (!any(is.infinite(at))) expect_match(.sf_err(v), msg, fixed = TRUE)
    expect_error(.sr_cox_check(s), msg, fixed = TRUE)
  }
  expect_true(.sf_val(.sf_spec("cox", cox_method = "pem", pem_cuts = list(rule = "explicit", at = c(1, 2.5))))$ok)
})

test_that("quantile k is a whole number in 2..50", {
  for (k in list(1, 51, 2.5)) {
    v <- .sf_val(.sf_spec("cox", cox_method = "pem", pem_cuts = list(rule = "quantile", k = k)))
    expect_false(v$ok, info = k)
    expect_match(.sf_err(v), "pem_cuts$k must be a whole number from 2 to 50", fixed = TRUE)
  }
  for (k in c(2, 50)) {
    expect_true(.sf_val(.sf_spec("cox", cox_method = "pem", pem_cuts = list(rule = "quantile", k = k)))$ok)
  }
  v <- .sf_val(.sf_spec("cox", cox_method = "pem", pem_cuts = list(rule = "decile", k = 5)))
  expect_false(v$ok)
  expect_match(.sf_err(v), "pem_cuts$rule must be \"quantile\" or \"explicit\"", fixed = TRUE)
})

test_that("pem refuses MI and bootstrap on both engines", {
  msg <- "PEM with multiple imputation / bootstrap arrives in P1e.3"
  for (eng in c("memory", "stream")) {
    mi <- .sf_spec("cox", cox_method = "pem", engine = eng, layers = .sf_layers(missing = "mi"),
                   mi = list(m = 3))
    bt <- .sf_spec("cox", cox_method = "pem", engine = eng, layers = .sf_layers(resampling = "bootstrap"),
                   boot = list(B = 50))
    for (s in list(mi, bt)) {
      v <- .sf_val(s)
      expect_false(v$ok, info = eng)
      expect_match(.sf_err(v), msg, fixed = TRUE)
    }
  }
})

test_that("pem estimand summary names the piecewise baseline", {
  r <- .sf_route("cox")
  expect_identical(r$estimand(.sf_spec("cox", cox_method = "pem"))$summary,
                   "hazard ratio from a piecewise-constant baseline hazard (piecewise-exponential model)")
  expect_identical(r$estimand(.sf_spec("cox"))$summary, "hazard ratio")
  expect_identical(r$estimand(.sf_spec("cox", cox_method = "exact", terms = list(list(var = "age"))))$summary,
                   "adjusted hazard ratio")
  expect_identical(r$estimand(.sf_spec("cox", strata = "inst"))$summary,
                   "hazard ratio stratified by inst (no other model terms)")
})

test_that("SAP text names the method", {
  expect_match(.sr_cox_method_text(.sf_spec("cox")),
               "Cox proportional hazards model (partial likelihood, Efron ties)", fixed = TRUE)
  expect_match(.sr_cox_method_text(.sf_spec("cox", cox_method = "exact", ties = "breslow")),
               "Cox proportional hazards model (partial likelihood, Breslow ties)", fixed = TRUE)
  expect_match(.sr_cox_method_text(.sf_spec("cox", cox_method = "pem", pem_cuts = list(rule = "quantile", k = 10))),
               "piecewise-exponential Poisson model with 10 intervals at event-time quantiles", fixed = TRUE)
  expect_match(.sr_cox_method_text(.sf_spec("cox", cox_method = "pem")),
               "with 10 intervals at event-time quantiles", fixed = TRUE)
  expect_match(.sr_cox_method_text(.sf_spec("cox", cox_method = "pem",
                                            pem_cuts = list(rule = "explicit", at = c(1, 2)))),
               "(rule explicit: 1, 2)", fixed = TRUE)
})

test_that("engine kind and fragment cover survival", {
  expect_identical(.sr_engine_kind("cox"), "cox")
  expect_identical(.sr_engine_kind("cox_describe"), "cox")
  expect_identical(.sr_engine_kind("rmst"), "rmst")
  f <- .sr_engine_fragment(list(n = 10, chunk = 5), "cox")
  expect_match(f, "Newton-Raphson iteration on the partial likelihood", fixed = TRUE)
  expect_match(.sr_engine_fragment(list(n = 10, chunk = 5), "cox", "stream"),
               "Newton-Raphson iteration on the partial likelihood", fixed = TRUE)
  expect_match(.sr_engine_fragment(list(n = 10, chunk = 5, method = "pem"), "cox"),
               "iteratively reweighted least squares", fixed = TRUE)
  expect_identical(.sr_engine_fragment(list(n = 1000003, chunk = 50000), "rmst"),
                   paste0("The restricted mean survival times were computed exactly from Kaplan-Meier counts ",
                          "accumulated over all 1000003 rows in chunks of 50000 rows."))
})

test_that("the cox form offers method and cut fields", {
  expect_true("stream" %in% .sr_form_engine_choices(.sf_route("cox")$spec_schema))
  expect_true("stream" %in% .sr_form_engine_choices(.sf_route("rmst")$spec_schema))
  expect_identical(SR_FORM_KINDS[["cox_method"]], "enum")
  expect_identical(SR_FORM_KINDS[["pem_cuts"]], "hidden")
  ids <- vapply(SR_FORM_EXTRAS, `[[`, "", "id")
  expect_true(all(c("pem_rule", "pem_k", "pem_at") %in% ids))
  pick <- function(id) SR_FORM_EXTRAS[[match(id, ids)]]
  expect_identical(pick("pem_rule")$to, c("pem_cuts", "rule"))
  expect_identical(pick("pem_k")$to, c("pem_cuts", "k"))
  expect_identical(pick("pem_at")$to, c("pem_cuts", "at"))
  expect_identical(pick("pem_rule")$kind, "radio")
  expect_identical(unname(pick("pem_rule")$choices), c("quantile", "explicit"))
  expect_identical(names(pick("pem_rule")$choices), c("K intervals at event-time quantiles", "explicit"))
  expect_identical(.SR_FORM_LABELS[["cox_method"]], "Method")
  expect_identical(.SR_FORM_CHOICE_TEXTS[["cox_method"]],
                   c("Cox (exact partial likelihood)" = "exact", "Piecewise-exponential (Poisson)" = "pem"))
  # the collected spec carries the cuts only for a PEM method
  base <- list(time = "time", status = "status", status_event = "2", exposure = "sex", seed = 1,
               layers_missing = "cc")
  s <- sr_spec_collect("cox", c(base, list(cox_method = "pem", pem_rule = "explicit", pem_k = 10,
                                           pem_at = "1, 2.5")))
  expect_identical(s$cox_method, "pem")
  expect_identical(s$pem_cuts, list(rule = "explicit", at = c(1, 2.5)))
  s2 <- sr_spec_collect("cox", c(base, list(cox_method = "exact", pem_rule = "explicit", pem_at = "1, 2")))
  expect_null(s2$cox_method)
  expect_null(s2$pem_cuts)
  s3 <- sr_spec_collect("cox", c(base, list(cox_method = "pem", pem_rule = "quantile", pem_k = 6,
                                            pem_at = "1, 2")))
  expect_identical(s3$pem_cuts, list(rule = "quantile", k = 6))
})
