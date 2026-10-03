# Tests for the route contract, registry and spec validation (design/spec.R).

mk_est <- function() {
  sr_estimand(population = "adults", variables = "x", intercurrent = "none", summary = "mean",
              level = "marginal")
}

toy_compile <- function(spec) sprintf("mean(data[[%s]])", deparse(spec$var))
toy_present <- function(result, spec) list(text = "mean")
toy_schema <- function() {
  list(var = list(type = "chr", required = TRUE),
       digits = list(type = "num", required = FALSE),
       flag = list(type = "lgl", required = FALSE),
       kind = list(type = "chr", required = FALSE, values = c("a", "b")))
}

toy_route <- function(id = "toy.mean", version = 1L, purpose = "describe", est = mk_est(),
                      schema = toy_schema(), compile = toy_compile, present = toy_present, ...) {
  sr_route(id = id, version = version, family = "toy", purpose = purpose, estimand = est,
           spec_schema = schema, compile = compile, present = present, ...)
}

test_that("SR_PURPOSES lists the three purposes", {
  expect_equal(SR_PURPOSES, c("describe", "predict", "explain-causal"))
})

test_that("sr_estimand builds a classed list and validates every field", {
  e <- mk_est()
  expect_s3_class(e, "sr_estimand")
  expect_equal(e$level, "marginal")
  expect_equal(sr_estimand("p", c("a", "b"), "i", "s", level = "not-applicable")$variables, c("a", "b"))
  expect_equal(sr_estimand("p", "a", "i", "s")$level, "marginal")  # first level is the default
  expect_error(sr_estimand("p", "a", "i", "s", level = "weird"))
  expect_error(sr_estimand("", "a", "i", "s"), "population")
  expect_error(sr_estimand("  ", "a", "i", "s"), "population")
  expect_error(sr_estimand(NA_character_, "a", "i", "s"), "population")
  expect_error(sr_estimand(c("p", "q"), "a", "i", "s"), "population")
  expect_error(sr_estimand("p", character(), "i", "s"), "variables")
  expect_error(sr_estimand("p", c("a", ""), "i", "s"), "variables")
  expect_error(sr_estimand("p", 1, "i", "s"), "variables")
  expect_error(sr_estimand("p", "a", "", "s"), "intercurrent")
  expect_error(sr_estimand("p", "a", "i", NULL), "summary")
  expect_error(sr_estimand("p", "a", "i"), "summary")
})

test_that("registering lists the route", {
  .sr_routes_reset()
  expect_equal(nrow(sr_routes()), 0L)
  r <- toy_route()
  expect_s3_class(r, "sr_route")
  sr_route_register(r)
  tab <- sr_routes()
  expect_equal(tab$id, "toy.mean")
  expect_equal(tab$version, 1L)
  expect_equal(tab$family, "toy")
  expect_equal(tab$purpose, "describe")
  expect_s3_class(sr_route_get("toy.mean"), "sr_route")
  .sr_routes_reset()
})

test_that("sr_routes() is a typed zero-row data frame when empty", {
  .sr_routes_reset()
  tab <- sr_routes()
  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), 0L)
  expect_equal(names(tab), c("id", "version", "family", "purpose"))
  expect_type(tab$id, "character")
  expect_true(is.numeric(tab$version))
  expect_type(tab$family, "character")
  expect_type(tab$purpose, "character")
})

test_that("a missing or unknown purpose cannot be registered", {
  .sr_routes_reset()
  r0 <- sr_route(id = "toy.mean", version = 1L, family = "toy", estimand = mk_est(),
                 spec_schema = toy_schema(), compile = toy_compile, present = toy_present)
  expect_error(sr_route_register(r0), "purpose")
  expect_error(sr_route_register(toy_route(purpose = "causal")), "purpose")
  expect_error(sr_route_register(toy_route(purpose = c("describe", "predict"))), "purpose")
  expect_equal(nrow(sr_routes()), 0L)
})

test_that("a route without a valid estimand cannot be registered", {
  .sr_routes_reset()
  r0 <- sr_route(id = "toy.mean", version = 1L, family = "toy", purpose = "describe",
                 spec_schema = toy_schema(), compile = toy_compile, present = toy_present)
  expect_error(sr_route_register(r0), "estimand")
  expect_error(sr_route_register(toy_route(est = "mean")), "estimand")
  expect_error(sr_route_register(toy_route(est = list(population = "x"))), "estimand")
  expect_equal(nrow(sr_routes()), 0L)
})

test_that("compile and present must be functions", {
  .sr_routes_reset()
  expect_error(sr_route_register(toy_route(compile = "mean(x)")), "compile")
  expect_error(sr_route_register(toy_route(present = NULL)), "present")
  expect_error(sr_route_register(toy_route(present = 3)), "present")
  expect_equal(nrow(sr_routes()), 0L)
})

test_that("spec_schema entries must be well formed", {
  .sr_routes_reset()
  bad_type <- list(var = list(type = "string", required = TRUE))
  bad_req <- list(var = list(type = "chr", required = "yes"))
  no_req <- list(var = list(type = "chr"))
  unnamed <- list(list(type = "chr", required = TRUE))
  bad_values <- list(var = list(type = "chr", required = TRUE, values = 1:3))
  expect_error(sr_route_register(toy_route(schema = bad_type)), "spec_schema.*var")
  expect_error(sr_route_register(toy_route(schema = bad_req)), "required")
  expect_error(sr_route_register(toy_route(schema = no_req)), "required")
  expect_error(sr_route_register(toy_route(schema = unnamed)), "spec_schema")
  expect_error(sr_route_register(toy_route(schema = bad_values)), "values")
  expect_error(sr_route_register(toy_route(schema = "x")), "spec_schema")
  expect_equal(nrow(sr_routes()), 0L)
  # an empty schema is fine
  expect_silent(sr_route_register(toy_route(schema = list())))
  .sr_routes_reset()
})

test_that("duplicate id@version errors; a new version registers and is the latest", {
  .sr_routes_reset()
  sr_route_register(toy_route())
  expect_error(sr_route_register(toy_route()), "toy.mean@1")
  sr_route_register(toy_route(version = 2L))
  expect_equal(sr_route_get("toy.mean")$version, 2L)
  expect_equal(sr_route_get("toy.mean", 1L)$version, 1L)
  expect_equal(sr_route_get("toy.mean", 2)$version, 2L)
  tab <- sr_routes()
  expect_equal(tab$version, 1:2)
  expect_equal(nrow(tab), 2L)
  .sr_routes_reset()
})

test_that("sr_route_get errors on an unknown id or version, naming the id", {
  .sr_routes_reset()
  expect_error(sr_route_get("nope.route"), "nope.route")
  sr_route_register(toy_route())
  expect_error(sr_route_get("toy.mean", 7L), "toy.mean")
  expect_error(sr_route_get("toy.mean", 7L), "7")
  expect_error(sr_route_get("other"), "other")
  .sr_routes_reset()
})

test_that("sr_routes() is sorted by id then numeric version", {
  .sr_routes_reset()
  sr_route_register(toy_route(id = "b.route", version = 1L))
  sr_route_register(toy_route(id = "a.route", version = 10L))
  sr_route_register(toy_route(id = "a.route", version = 2L))
  tab <- sr_routes()
  expect_equal(tab$id, c("a.route", "a.route", "b.route"))
  expect_equal(tab$version, c(2L, 10L, 1L))
  expect_equal(sr_route_get("a.route")$version, 10L)
  .sr_routes_reset()
})

test_that("route ids and versions are checked", {
  expect_error(toy_route(id = ""), "id")
  expect_error(toy_route(id = c("a", "b")), "id")
  expect_error(toy_route(version = 0), "version")
  expect_error(toy_route(version = 1.5), "version")
  expect_error(toy_route(version = c(1, 2)), "version")
  expect_error(toy_route(version = NA), "version")
})

test_that("sr_spec_validate reports a missing required field", {
  r <- toy_route()
  v <- sr_spec_validate(list(purpose = "describe"), r)
  expect_false(v$ok)
  expect_true(any(grepl("var", v$errors)))
  ok <- sr_spec_validate(list(purpose = "describe", var = "x"), r)
  expect_true(ok$ok)
  expect_length(ok$errors, 0L)
  expect_type(ok$errors, "character")
})

test_that("a spec with another purpose is rejected (cross-talk guard)", {
  r <- toy_route()
  v <- sr_spec_validate(list(purpose = "predict", var = "x"), r)
  expect_false(v$ok)
  expect_true(any(grepl("purpose", v$errors)))
  v2 <- sr_spec_validate(list(var = "x"), r)
  expect_false(v2$ok)
  expect_true(any(grepl("purpose", v2$errors)))
})

test_that("types and allowed values are enforced and all errors are returned", {
  r <- toy_route()
  v <- sr_spec_validate(list(purpose = "describe", var = 3, digits = "two", flag = "yes",
                             kind = "c"), r)
  expect_false(v$ok)
  expect_length(v$errors, 4L)
  for (f in c("var", "digits", "flag", "kind")) expect_true(any(grepl(f, v$errors)), info = f)
  # integers count as numeric; a vector with one bad element fails values
  expect_true(sr_spec_validate(list(purpose = "describe", var = "x", digits = 2L), r)$ok)
  expect_true(sr_spec_validate(list(purpose = "describe", var = "x", kind = c("a", "b")), r)$ok)
  bad <- sr_spec_validate(list(purpose = "describe", var = "x", kind = c("a", "z")), r)
  expect_false(bad$ok)
  expect_true(any(grepl("kind", bad$errors)))
  # a list-typed field
  r2 <- toy_route(schema = list(layers = list(type = "list", required = TRUE)))
  expect_true(sr_spec_validate(list(purpose = "describe", layers = list(1, 2)), r2)$ok)
  expect_false(sr_spec_validate(list(purpose = "describe", layers = "a"), r2)$ok)
})

test_that("optional fields may be absent; unknown fields are allowed", {
  r <- toy_route()
  expect_true(sr_spec_validate(list(purpose = "describe", var = "x", extra = list(a = 1)), r)$ok)
  expect_true(sr_spec_validate(list(purpose = "describe", var = "x", digits = NULL), r)$ok)
  # a required field that is NULL counts as missing
  expect_false(sr_spec_validate(list(purpose = "describe", var = NULL), r)$ok)
})

test_that("a non-list spec is invalid", {
  v <- sr_spec_validate("x", toy_route())
  expect_false(v$ok)
  expect_true(length(v$errors) >= 1L)
})

test_that("a spec that cannot be canonically hashed is invalid", {
  r <- toy_route()
  v <- sr_spec_validate(list(purpose = "describe", var = "x", when = Sys.Date()), r)
  expect_false(v$ok)
  expect_true(any(grepl("canonical", v$errors)))
})

test_that("a function-form estimand is evaluated on the spec", {
  est_fn <- function(spec) {
    sr_estimand("adults", spec$var, "none", "mean")
  }
  r <- toy_route(est = est_fn)
  .sr_routes_reset()
  expect_silent(sr_route_register(r))
  expect_true(sr_spec_validate(list(purpose = "describe", var = "x"), r)$ok)
  junk <- toy_route(est = function(spec) "not an estimand")
  v <- sr_spec_validate(list(purpose = "describe", var = "x"), junk)
  expect_false(v$ok)
  expect_true(any(grepl("estimand", v$errors)))
  thrower <- toy_route(est = function(spec) stop("boom"))
  v2 <- sr_spec_validate(list(purpose = "describe", var = "x"), thrower)
  expect_false(v2$ok)
  expect_true(any(grepl("estimand", v2$errors)))
  .sr_routes_reset()
})

test_that("sr_compile returns code that evaluates", {
  r <- toy_route()
  code <- sr_compile(r, list(purpose = "describe", var = "x"))
  expect_type(code, "character")
  expect_length(code, 1L)
  data <- data.frame(x = 1:3)
  expect_equal(eval(parse(text = code)), 2)
})

test_that("sr_compile refuses an invalid spec with the joined errors", {
  r <- toy_route()
  expect_error(sr_compile(r, list(purpose = "predict")), "purpose")
  expect_error(sr_compile(r, list(purpose = "predict")), "var")
  expect_error(sr_compile(r, list(purpose = "describe")), "var")
})

test_that("sr_compile requires compile() to return one non-NA string", {
  spec <- list(purpose = "describe", var = "x")
  expect_error(sr_compile(toy_route(compile = function(spec) 1), spec), "compile")
  expect_error(sr_compile(toy_route(compile = function(spec) c("a", "b")), spec), "compile")
  expect_error(sr_compile(toy_route(compile = function(spec) NA_character_), spec), "compile")
  expect_error(sr_compile(toy_route(compile = function(spec) character()), spec), "compile")
  expect_error(sr_compile(toy_route(compile = function(spec) NULL), spec), "compile")
})

test_that("empty values count as missing for required fields", {
  r <- toy_route(schema = list(var = list(type = "chr", required = TRUE),
                               n = list(type = "num", required = TRUE),
                               k = list(type = "chr", required = FALSE, values = c("a", "b"))))
  chk <- function(...) sr_spec_validate(c(list(purpose = "describe"), list(...)), r)
  good <- chk(var = "x", n = 1)
  expect_true(good$ok)
  for (bad in list("", "  ", NA_character_, character(0), c(NA_character_, NA_character_))) {
    v <- chk(var = bad, n = 1)
    expect_false(v$ok)
    expect_true(any(grepl("var", v$errors)))
  }
  expect_true(any(grepl("required", chk(var = "", n = 1)$errors)))
  expect_true(any(grepl("required", chk(var = character(0), n = 1)$errors)))
  v <- chk(var = "x", n = NA_real_)
  expect_false(v$ok)
  expect_true(any(grepl("n", v$errors)))
  expect_false(chk(var = "x", n = numeric(0))$ok)
})

test_that("a present chr value must have no NA or blank element", {
  r <- toy_route()
  v <- sr_spec_validate(list(purpose = "describe", var = c("ok", "")), r)
  expect_false(v$ok)
  expect_true(any(grepl("var", v$errors)))
  v2 <- sr_spec_validate(list(purpose = "describe", var = "x", kind = c("a", NA)), r)
  expect_false(v2$ok)
  expect_true(any(grepl("kind", v2$errors)))
  expect_false(sr_spec_validate(list(purpose = "describe", var = "x", kind = ""), r)$ok)
})

test_that("a length-0 value never satisfies a values constraint", {
  r <- toy_route()
  v <- sr_spec_validate(list(purpose = "describe", var = "x", kind = character(0)), r)
  expect_false(v$ok)
  expect_true(any(grepl("kind", v$errors)))
})

test_that("sr_route rejects a non-finite or oversized version", {
  expect_error(toy_route(version = Inf), "version")
  expect_error(toy_route(version = 1e10), "version")
  expect_error(toy_route(version = "1"), "version")
  expect_error(toy_route(version = TRUE), "version")
  expect_equal(toy_route(version = .Machine$integer.max)$version, .Machine$integer.max)
})

test_that("sr_route_register re-checks id and version on a mutated route", {
  .sr_routes_reset()
  for (bad in list("x", NULL, Inf, 0, 1.5, 1e10, NA_real_, c(1, 2), TRUE)) {
    r <- toy_route()
    r["version"] <- list(bad)
    expect_error(sr_route_register(r), "version")
  }
  for (bad in list(NULL, "", NA_character_, c("a", "b"), 3)) {
    r <- toy_route()
    r["id"] <- list(bad)
    expect_error(sr_route_register(r), "id")
  }
  expect_equal(nrow(sr_routes()), 0L)
  sr_route_register(toy_route())
  expect_equal(nrow(sr_routes()), 1L)
  expect_equal(sr_routes()$id, "toy.mean")
  .sr_routes_reset()
})

test_that("an empty or forged sr_estimand shell is not an estimand", {
  .sr_routes_reset()
  shell <- structure(list(), class = "sr_estimand")
  expect_error(sr_route_register(toy_route(est = shell)), "estimand")
  forged <- mk_est()
  forged$level <- "bogus"
  expect_error(sr_route_register(toy_route(est = forged)), "estimand")
  forged2 <- mk_est()
  forged2$variables <- character()
  expect_error(sr_route_register(toy_route(est = forged2)), "estimand")
  expect_equal(nrow(sr_routes()), 0L)
  r <- toy_route(est = function(spec) structure(list(), class = "sr_estimand"))
  v <- sr_spec_validate(list(purpose = "describe", var = "x"), r)
  expect_false(v$ok)
  expect_true(any(grepl("estimand", v$errors)))
  r2 <- toy_route(est = function(spec) forged)
  expect_false(sr_spec_validate(list(purpose = "describe", var = "x"), r2)$ok)
  expect_silent(sr_route_register(toy_route(est = mk_est())))
  .sr_routes_reset()
})

test_that("composes_with and applicable have defaults", {
  r <- toy_route()
  expect_equal(r$composes_with, character())
  a <- r$applicable(list(), list())
  expect_true(a$ok)
  expect_equal(a$reasons, character())
  r2 <- toy_route(composes_with = c("a", "b"))
  expect_equal(r2$composes_with, c("a", "b"))
})
