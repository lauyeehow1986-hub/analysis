# Tests for the pure spec-form builder (ui/spec_form.R).

# The P1 routes (the describe twins too), registered again when an earlier test file emptied the registry.
.ui_p1_routes <- function() {
  make <- list(lm = .sr_lm_route, lm_describe = .sr_lm_describe_route, logistic = .sr_logistic_route,
               logistic_describe = .sr_logistic_describe_route, modpois = .sr_modpois_route,
               cox = .sr_cox_route, cox_describe = .sr_cox_describe_route, rmst = .sr_rmst_route,
               table1 = .sr_table1_route)
  for (k in names(make)) {
    if (!(.sr_route_key(k, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make[[k]]())
  }
  names(make)
}

.ui_layer_values <- function(missing = "cc", resampling = "none", clustered = "FALSE") {
  list(layers_missing = missing, layers_weights = "none", layers_matching = "FALSE",
       layers_resampling = resampling, layers_framework = "frequentist", layers_clustered = clustered)
}

# The writer-fixture specs of each route, as form values (keyed by entry id, as the inputs give them).
.ui_valid_fill <- function(r) {
  lay <- .ui_layer_values()
  bin <- list(outcome = "y", event_value = "yes", exposure = "grp", exposure_ref = "A",
              terms = list(list(var = "age", type = "linear", knots = ""),
                           list(var = "z1", type = "linear", knots = "")),
              marginal = TRUE, seed = "42")
  surv <- list(time = "time", status = "status", status_event = "1", exposure = "grp", exposure_ref = "A",
               seed = "42")
  v <- switch(r,
    lm = , lm_describe = list(outcome = "age", exposure = "grp", exposure_ref = "A",
                              terms = list(list(var = "z1", type = "linear", knots = "")), seed = "42"),
    logistic = , logistic_describe = , modpois = bin,
    cox = , cox_describe = c(surv, list(terms = list(list(var = "age", type = "linear", knots = "")),
                                        ties = "efron")),
    rmst = c(surv, list(tau = "20")),
    table1 = list(by = "grp", vars = c("age", "z1", "stage"), seed = "42"),
    stop("no valid fill for route ", r))
  c(v, lay)
}

test_that("every registered route builds, and a valid fill passes sr_spec_validate", {
  ids <- .ui_p1_routes()
  expect_true(all(ids %in% sr_routes()$id))
  p <- .fx_ui_project()
  for (r in ids) {
    f <- sr_spec_form(r, "t", p)
    spec <- sr_spec_collect(r, .ui_valid_fill(r))
    v <- sr_spec_validate(spec, sr_route_get(r))
    expect_true(v$ok, info = paste(r, paste(v$errors, collapse = "; ")))
    expect_true(sr_compose_check(spec)$ok, info = r)
    ids_f <- unname(vapply(f, `[[`, "", "id"))
    expect_identical(names(f), ids_f, info = r)
    expect_false(any(c("exclude_rows", "engine", "stream_chunk") %in% ids_f), info = r)
    expect_false("purpose" %in% ids_f, info = r)
    expect_identical(spec$purpose, sr_routes()$purpose[match(r, sr_routes()$id)], info = r)
    expect_true(all(paste0("layers_", c("missing", "weights", "matching", "resampling", "framework",
                                        "clustered")) %in% ids_f), info = r)
    # schema order first, then the extras
    sch <- setdiff(names(sr_route_get(r)$spec_schema), c("exclude_rows", "engine", "stream_chunk", "layers"))
    expect_identical(ids_f[ids_f %in% sch], sch, info = r)
    expect_identical("exposure_ref" %in% ids_f, "exposure" %in% sch, info = r)
  }
  # the logistic fill collects to the writer fixture's spec (seed as a number)
  got <- sr_spec_collect("logistic", .ui_valid_fill("logistic"))
  want <- .ui_bin_spec()
  want$terms <- lapply(want$terms, function(t) c(t, type = "linear"))
  expect_equal(got[order(names(got))], want[order(names(want))], ignore_attr = TRUE)
})

test_that("an unknown field refuses the route", {
  .ui_p1_routes()
  p <- .fx_ui_project()
  # a newer lm version with a field the form does not know: sr_route_get("lm") now returns it
  r <- .sr_lm_route()
  r$version <- 99L
  r$spec_schema$wobble <- list(type = "chr", required = FALSE)
  sr_route_register(r)
  withr::defer(rm(list = .sr_route_key("lm", 99L), envir = .sr_routes))
  expect_identical(sr_route_get("lm")$version, 99L)
  expect_error(sr_spec_form("lm", "t", p),
               "the form cannot build field 'wobble' of route 'lm' yet", fixed = TRUE)
  expect_error(sr_spec_collect("lm", .ui_valid_fill("lm")),
               "the form cannot build field 'wobble' of route 'lm' yet", fixed = TRUE)
})

test_that("prefill takes a decided router choice over a role over a default, and ignores stale", {
  .ui_p1_routes()
  p <- .fx_ui_project()
  p <- sr_pico_set(p, population = "test population", outcome = "test outcome")
  p <- sr_router_decide(p, "purpose", "explain-causal", reason = "test fixture")
  p <- sr_router_decide(p, "outcome_col", "z1", reason = "chosen for the test", table = "t")
  f <- sr_spec_form("lm", "t", p)
  expect_identical(f[["outcome"]]$value, "z1")
  expect_identical(f[["outcome"]]$source, "router")
  expect_identical(f[["exposure"]]$value, "grp")
  expect_identical(f[["exposure"]]$source, "roles")
  expect_equal(f[["seed"]]$value, 42)
  expect_identical(f[["seed"]]$source, "default")
  expect_identical(vapply(f[["terms"]]$value, `[[`, "", "var"), c("age", "z1"))
  expect_identical(unique(vapply(f[["terms"]]$value, `[[`, "", "type")), "linear")
  expect_identical(f[["cluster"]]$source, NA_character_)
  expect_null(f[["cluster"]]$value)
  expect_identical(f[["layers_clustered"]]$value, FALSE)
  expect_identical(f[["layers_missing"]]$value, "cc")
  # table1: by from the exposure role, vars from the covariates
  t1 <- sr_spec_form("table1", "t", p)
  expect_identical(t1[["by"]]$value, "grp")
  expect_identical(t1[["vars"]]$value, c("age", "z1"))
  p <- sr_router_decide(p, "primary_route", "lm", reason = "chosen for the test", table = "t")
  expect_identical(sr_spec_form_route(p, "t"), "lm")
  # the upstream purpose changes: outcome_col is stale and never used
  p <- sr_router_decide(p, "purpose", "describe", reason = "changed for the test")
  expect_true(is.na(sr_router_choice(p, "outcome_col", "t")))
  f2 <- sr_spec_form("lm", "t", p)
  expect_identical(f2[["outcome"]]$value, "y")
  expect_identical(f2[["outcome"]]$source, "roles")
  expect_true(is.na(sr_spec_form_route(p, "t")))
})

test_that("collect maps MI and bootstrap extras only when their layer is on", {
  .ui_p1_routes()
  base <- .ui_valid_fill("lm")
  s <- sr_spec_collect("lm", utils::modifyList(base, list(layers_missing = "cc", mi_m = "5", boot_B = "500")))
  expect_null(s$mi)
  expect_null(s$boot)
  s <- sr_spec_collect("lm", utils::modifyList(base, list(layers_missing = "mi", mi_m = "auto")))
  expect_identical(s$mi$m, "auto")
  s <- sr_spec_collect("lm", utils::modifyList(base, list(layers_missing = "mi", mi_m = "5", mi_maxit = "20",
                                                          mi_aux = c("stage"))))
  expect_identical(s$mi$m, 5)
  expect_identical(s$mi$maxit, 20)
  expect_identical(s$mi$aux, "stage")
  expect_null(s$choices)
  s <- sr_spec_collect("lm", utils::modifyList(base, list(layers_resampling = "bootstrap", layers_clustered = "TRUE",
                                                          boot_B = "200", boot_type = "bca", boot_cluster = TRUE)))
  expect_identical(s$boot_type, "cluster")
  expect_identical(s$boot, list(B = 200, type = "bca"))
  expect_identical(s$layers$clustered, TRUE)
  expect_identical(s$layers$matching, FALSE)
  s <- sr_spec_collect("lm", utils::modifyList(base, list(layers_resampling = "bootstrap", boot_cluster = FALSE)))
  expect_null(s$boot_type)
  s <- sr_spec_collect("lm", utils::modifyList(base, list(layers_missing = "mi", layers_resampling = "bootstrap",
                                                          mi_m = "5", mi_x_boot = "impute_then_boot")))
  expect_identical(s$choices$mi_x_boot, "impute_then_boot")
  expect_true(sr_compose_check(s)$ok)
  # the form offers the extras only when their layer is on
  p <- .fx_ui_project()
  ids <- names(sr_spec_form("lm", "t", p))
  expect_false(any(c("mi_m", "boot_B", "mi_x_boot") %in% ids))
  ids <- names(sr_spec_form("lm", "t", p, layers = list(missing = "mi", resampling = "bootstrap")))
  expect_true(all(c("mi_m", "mi_maxit", "mi_aux", "boot_B", "boot_type", "boot_cluster", "mi_x_boot") %in% ids))
  expect_false("exposure_ref" %in% names(sr_spec_form("table1", "t", p)))
})

test_that("collect casts terms, interactions, events and drops empty optional fields", {
  .ui_p1_routes()
  v <- .ui_valid_fill("cox")
  nw <- list(
    terms = list(list(var = "age", type = "rcs", knots = ""), list(var = "z1", type = "rcs", knots = "4"),
                 list(var = "stage", type = "rcs", knots = "1, 2.5,4"), list(var = "", type = "linear", knots = "")),
    interactions = list(c("grp", "age"), c("grp", "")),
    status_event = "dead", strata = "", cluster = NA, ties = NULL, exclude_rows = c(1, 2))
  v[names(nw)] <- nw  # not modifyList: it would merge the term lists and drop the NULL
  expect_true("ties" %in% names(v))
  s <- sr_spec_collect("cox", v)
  expect_identical(s$terms, list(list(var = "age", type = "rcs"), list(var = "z1", type = "rcs", knots = 4),
                                 list(var = "stage", type = "rcs", knots = c(1, 2.5, 4))))
  expect_identical(s$interactions, list(c("grp", "age")))
  expect_identical(s$status_event, "dead")
  expect_identical(sr_spec_collect("cox", .ui_valid_fill("cox"))$status_event, 1)
  expect_false(any(c("strata", "cluster", "exclude_rows") %in% names(s)))
  expect_identical(s$ties, NULL)
})

test_that("disabled layer values carry the reason", {
  .ui_p1_routes()
  f <- sr_spec_form("lm", "t", .fx_ui_project())
  expect_identical(f[["layers_weights"]]$disabled[["ipw"]], "not runnable in P1")
  expect_identical(f[["layers_weights"]]$disabled[["overlap"]], "not runnable in P1")
  expect_identical(f[["layers_weights"]]$disabled[["survey"]], "not runnable in P1")
  expect_identical(f[["layers_matching"]]$disabled[["TRUE"]], "not runnable in P1")
  expect_identical(sort(names(f[["layers_framework"]]$disabled)), c("bayesian", "both"))
  expect_length(f[["layers_missing"]]$disabled, 0L)
  expect_identical(f[["layers_weights"]]$choices, SR_LAYER_SETS$weights)
  expect_identical(f[["layers_framework"]]$default, "frequentist")
})

test_that("the primary route helper reads only a decided, current choice", {
  .ui_p1_routes()
  p <- .fx_ui_project()
  expect_true(is.na(sr_spec_form_route(p, "t")))
})

test_that("every SR_FORM_KINDS entry is a known kind and the extras are the eight of the plan", {
  expect_true(all(SR_FORM_KINDS %in% c("column", "columns", "terms", "interactions", "layers", "enum",
                                       "logical", "number", "text", "event", "hidden")))
  expect_identical(vapply(SR_FORM_EXTRAS, `[[`, "", "id"),
                   c("exposure_ref", "mi_m", "mi_maxit", "mi_aux", "boot_B", "boot_type", "boot_cluster", "mi_x_boot"))
})
