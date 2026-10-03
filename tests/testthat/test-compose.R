# Tests for the composition matrix (design/compose.R).

# A spec with the plain layers (cc, none, no matching, none, frequentist, not clustered); `...`
# overrides layer fields, `extra` adds top-level spec fields (choices, pool, boot_type, ...).
mk_cspec <- function(..., extra = list()) {
  layers <- list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                 framework = "frequentist", clustered = FALSE)
  ov <- list(...)
  for (n in names(ov)) layers[[n]] <- ov[[n]]
  c(list(layers = layers), extra)
}

test_that("sr_compose_rules lists the ten pinned rules in table order", {
  r <- sr_compose_rules()
  expect_equal(vapply(r, function(x) x$id, ""),
               c("mi_x_ps", "mi_x_boot", "bayes_x_boot", "bayes_x_mi", "match_x_boot",
                 "ipw_x_boot", "survey_x_mi", "survey_x_boot", "cluster_x_boot",
                 "overlap_target"))
  for (x in r) {
    expect_true(x$outcome %in% c("refuse", "choice", "require", "note"))
    expect_true(is.function(x$when))
    expect_true(is.character(x$msg) && length(x$msg) == 1L && nzchar(x$msg))
    expect_true(is.character(x$options))
    if (x$outcome == "require") expect_true(is.function(x$require)) else expect_null(x$require)
    if (x$outcome == "choice") expect_gt(length(x$options), 0L) else expect_length(x$options, 0L)
  }
})

test_that("a plain spec is ok with no refusals, choices, unmet or notes", {
  r <- sr_compose_check(mk_cspec())
  expect_true(r$ok)
  expect_equal(r$refused, character(0))
  expect_equal(r$unmet, character(0))
  expect_equal(r$notes, character(0))
  expect_length(r$choices, 0L)
  expect_true(is.list(r$choices))
})

test_that("mi_x_ps: mi with ipw or overlap forces the MIte / MIps choice", {
  for (w in c("ipw", "overlap")) {
    s <- mk_cspec(missing = "mi", weights = w)
    r <- sr_compose_check(s)
    expect_false(r$ok)
    expect_equal(r$choices$mi_x_ps, c("MIte", "MIps"))
    expect_equal(r$refused, character(0))
    expect_equal(r$unmet, character(0))
  }
  for (v in c("MIte", "MIps")) {
    s <- mk_cspec(missing = "mi", weights = "ipw", extra = list(choices = c(mi_x_ps = v)))
    r <- sr_compose_check(s)
    expect_true(r$ok)
    expect_null(r$choices$mi_x_ps)
    expect_length(r$choices, 0L)
  }
  # does not trigger for cc, or for survey / none weights
  expect_true(sr_compose_check(mk_cspec(missing = "cc", weights = "ipw"))$ok)
  expect_true(sr_compose_check(mk_cspec(missing = "mi", weights = "survey",
                                        extra = list(design_vars_in_imputation = TRUE)))$ok)
})

test_that("mi_x_boot: mi with bootstrap forces boot_then_impute / impute_then_boot", {
  s <- mk_cspec(missing = "mi", resampling = "bootstrap")
  r <- sr_compose_check(s)
  expect_false(r$ok)
  expect_equal(r$choices$mi_x_boot, c("boot_then_impute", "impute_then_boot"))
  for (v in c("boot_then_impute", "impute_then_boot")) {
    s2 <- mk_cspec(missing = "mi", resampling = "bootstrap",
                   extra = list(choices = c(mi_x_boot = v)))
    r2 <- sr_compose_check(s2)
    expect_true(r2$ok)
    expect_length(r2$choices, 0L)
  }
})

test_that("a choice value outside the options leaves the choice unresolved", {
  s <- mk_cspec(missing = "mi", weights = "ipw", extra = list(choices = c(mi_x_ps = "other")))
  r <- sr_compose_check(s)
  expect_false(r$ok)
  expect_equal(r$choices$mi_x_ps, c("MIte", "MIps"))
  expect_true(length(r$notes) >= 1L)
  # not a single string, NA, or empty
  for (bad in list(c("MIte", "MIps"), NA_character_, character(0), 1L, TRUE)) {
    s2 <- mk_cspec(missing = "mi", weights = "ipw", extra = list(choices = list(mi_x_ps = bad)))
    expect_false(sr_compose_check(s2)$ok)
  }
  # a choice for a different id does not resolve this one
  s3 <- mk_cspec(missing = "mi", weights = "ipw", extra = list(choices = c(mi_x_boot = "MIte")))
  expect_false(sr_compose_check(s3)$ok)
})

test_that("a choice may be given as a list element", {
  s <- mk_cspec(missing = "mi", resampling = "bootstrap",
                extra = list(choices = list(mi_x_boot = "impute_then_boot")))
  expect_true(sr_compose_check(s)$ok)
})

test_that("choices are matched exactly, never partially", {
  s <- mk_cspec(missing = "mi", weights = "ipw", extra = list(choices = list(mi_x = "MIte")))
  expect_false(sr_compose_check(s)$ok)
})

test_that("bayes_x_boot: bayesian or both with bootstrap is refused", {
  for (f in c("bayesian", "both")) {
    r <- sr_compose_check(mk_cspec(framework = f, resampling = "bootstrap"))
    expect_false(r$ok)
    expect_true("bayes_x_boot" %in% r$refused)
    expect_true(any(grepl("posterior already carries uncertainty", r$notes)))
  }
  # no way to satisfy a refusal
  s <- mk_cspec(framework = "bayesian", resampling = "bootstrap",
                extra = list(choices = c(bayes_x_boot = "x"), boot_type = "pair"))
  expect_false(sr_compose_check(s)$ok)
  # bayesian without bootstrap is fine
  expect_true(sr_compose_check(mk_cspec(framework = "bayesian"))$ok)
  expect_true(sr_compose_check(mk_cspec(framework = "both"))$ok)
})

test_that("bayes_x_mi: bayesian with mi requires pool == draws", {
  s <- mk_cspec(framework = "bayesian", missing = "mi")
  r <- sr_compose_check(s)
  expect_false(r$ok)
  expect_equal(r$unmet, "bayes_x_mi")
  expect_true(any(grepl("pool draws", r$notes)))
  expect_true(sr_compose_check(mk_cspec(framework = "bayesian", missing = "mi",
                                        extra = list(pool = "draws")))$ok)
  expect_false(sr_compose_check(mk_cspec(framework = "bayesian", missing = "mi",
                                         extra = list(pool = "rubin")))$ok)
  # framework "both" does not trigger it (pinned: bayesian only)
  expect_true(sr_compose_check(mk_cspec(framework = "both", missing = "mi"))$ok)
})

test_that("match_x_boot: matching with bootstrap requires boot_type pair or cluster", {
  s <- mk_cspec(matching = TRUE, resampling = "bootstrap")
  r <- sr_compose_check(s)
  expect_false(r$ok)
  expect_equal(r$unmet, "match_x_boot")
  expect_true(any(grepl("Abadie-Imbens", r$notes)))
  for (bt in c("pair", "cluster")) {
    expect_true(sr_compose_check(mk_cspec(matching = TRUE, resampling = "bootstrap",
                                          extra = list(boot_type = bt)))$ok)
  }
  expect_false(sr_compose_check(mk_cspec(matching = TRUE, resampling = "bootstrap",
                                         extra = list(boot_type = "naive")))$ok)
})

test_that("ipw_x_boot: ipw or overlap with bootstrap requires refit_ps_each_rep", {
  for (w in c("ipw", "overlap")) {
    s <- mk_cspec(weights = w, resampling = "bootstrap")
    r <- sr_compose_check(s)
    expect_false(r$ok)
    expect_equal(r$unmet, "ipw_x_boot")
    s2 <- mk_cspec(weights = w, resampling = "bootstrap",
                   extra = list(refit_ps_each_rep = TRUE))
    expect_true(sr_compose_check(s2)$ok)
    s3 <- mk_cspec(weights = w, resampling = "bootstrap",
                   extra = list(refit_ps_each_rep = FALSE))
    expect_false(sr_compose_check(s3)$ok)
  }
})

test_that("survey_x_mi: survey weights with mi require design_vars_in_imputation", {
  s <- mk_cspec(weights = "survey", missing = "mi")
  r <- sr_compose_check(s)
  expect_false(r$ok)
  expect_equal(r$unmet, "survey_x_mi")
  expect_true(sr_compose_check(mk_cspec(weights = "survey", missing = "mi",
                                        extra = list(design_vars_in_imputation = TRUE)))$ok)
  expect_false(sr_compose_check(mk_cspec(weights = "survey", missing = "mi",
                                         extra = list(design_vars_in_imputation = FALSE)))$ok)
})

test_that("survey_x_boot: survey weights with bootstrap require replicate_weights", {
  s <- mk_cspec(weights = "survey", resampling = "bootstrap")
  r <- sr_compose_check(s)
  expect_false(r$ok)
  expect_equal(r$unmet, "survey_x_boot")
  expect_true(sr_compose_check(mk_cspec(weights = "survey", resampling = "bootstrap",
                                        extra = list(boot_type = "replicate_weights")))$ok)
  expect_false(sr_compose_check(mk_cspec(weights = "survey", resampling = "bootstrap",
                                         extra = list(boot_type = "pair")))$ok)
})

test_that("cluster_x_boot: clustered with bootstrap requires cluster, pair or replicate_weights", {
  s <- mk_cspec(clustered = TRUE, resampling = "bootstrap")
  r <- sr_compose_check(s)
  expect_false(r$ok)
  expect_equal(r$unmet, "cluster_x_boot")
  for (bt in c("cluster", "pair", "replicate_weights")) {
    expect_true(sr_compose_check(mk_cspec(clustered = TRUE, resampling = "bootstrap",
                                          extra = list(boot_type = bt)))$ok)
  }
  expect_false(sr_compose_check(mk_cspec(clustered = TRUE, resampling = "bootstrap",
                                         extra = list(boot_type = "naive")))$ok)
  # clustered without bootstrap does not trigger
  expect_true(sr_compose_check(mk_cspec(clustered = TRUE))$ok)
})

test_that("overlap_target is always ok and adds an ATO note", {
  r <- sr_compose_check(mk_cspec(weights = "overlap"))
  expect_true(r$ok)
  expect_equal(r$refused, character(0))
  expect_equal(r$unmet, character(0))
  expect_length(r$choices, 0L)
  expect_true(any(grepl("ATO", r$notes)))
  expect_length(r$notes, 1L)
})

test_that("satisfied rules add no notes", {
  s <- mk_cspec(framework = "bayesian", missing = "mi", extra = list(pool = "draws"))
  r <- sr_compose_check(s)
  expect_true(r$ok)
  expect_equal(r$notes, character(0))
})

test_that("several rules firing together are all collected, in table order", {
  s <- mk_cspec(missing = "mi", weights = "ipw", resampling = "bootstrap",
                framework = "bayesian")
  r <- sr_compose_check(s)
  expect_false(r$ok)
  expect_equal(names(r$choices), c("mi_x_ps", "mi_x_boot"))
  expect_equal(r$refused, "bayes_x_boot")
  expect_equal(r$unmet, c("bayes_x_mi", "ipw_x_boot"))
  expect_length(r$notes, 5L)
  # resolving everything but the refusal still leaves ok FALSE
  s2 <- mk_cspec(missing = "mi", weights = "ipw", resampling = "bootstrap",
                 framework = "bayesian",
                 extra = list(choices = c(mi_x_ps = "MIps", mi_x_boot = "boot_then_impute"),
                              pool = "draws", refit_ps_each_rep = TRUE))
  r2 <- sr_compose_check(s2)
  expect_false(r2$ok)
  expect_equal(r2$refused, "bayes_x_boot")
  expect_equal(r2$unmet, character(0))
  expect_length(r2$choices, 0L)
  # the same combination with a frequentist framework is fully satisfiable
  s3 <- s2
  s3$layers$framework <- "frequentist"
  s3$pool <- NULL
  expect_true(sr_compose_check(s3)$ok)
})

test_that("a stack of layers reports every requirement", {
  s <- mk_cspec(missing = "mi", weights = "survey", matching = TRUE, resampling = "bootstrap",
                clustered = TRUE)
  r <- sr_compose_check(s)
  expect_false(r$ok)
  expect_equal(names(r$choices), "mi_x_boot")
  expect_equal(r$unmet, c("match_x_boot", "survey_x_mi", "survey_x_boot", "cluster_x_boot"))
  s2 <- mk_cspec(missing = "mi", weights = "survey", matching = TRUE, resampling = "bootstrap",
                 clustered = TRUE,
                 extra = list(choices = c(mi_x_boot = "impute_then_boot"),
                              design_vars_in_imputation = TRUE, boot_type = "replicate_weights"))
  r2 <- sr_compose_check(s2)
  # replicate_weights is not pair/cluster, so the matching requirement stays unmet
  expect_equal(r2$unmet, "match_x_boot")
})

test_that("require functions return a single TRUE/FALSE when the spec field is absent", {
  req <- Filter(function(x) x$outcome == "require", sr_compose_rules())
  expect_length(req, 6L)
  for (x in req) {
    expect_identical(x$require(list(layers = list())), FALSE)
    expect_identical(x$require(list()), FALSE)
  }
})

test_that("sr_compose_check errors naming the field for a malformed spec", {
  expect_error(sr_compose_check("x"), "spec")
  expect_error(sr_compose_check(list()), "layers")
  expect_error(sr_compose_check(list(layers = "cc")), "layers")
  for (f in c("missing", "weights", "matching", "resampling", "framework", "clustered")) {
    s <- mk_cspec()
    s$layers[[f]] <- NULL
    expect_error(sr_compose_check(s), f, info = paste("absent", f))
  }
  bad <- list(missing = "zzz", weights = "zzz", matching = "yes", resampling = "zzz",
              framework = "zzz", clustered = 1)
  for (f in names(bad)) {
    s <- mk_cspec()
    s$layers[[f]] <- bad[[f]]
    expect_error(sr_compose_check(s), f, info = paste("invalid", f))
  }
  s <- mk_cspec(); s$layers$matching <- NA
  expect_error(sr_compose_check(s), "matching")
  s <- mk_cspec(); s$layers$missing <- NA_character_
  expect_error(sr_compose_check(s), "missing")
  s <- mk_cspec(); s$layers$weights <- c("none", "ipw")
  expect_error(sr_compose_check(s), "weights")
  s <- mk_cspec(); s$layers$clustered <- c(TRUE, FALSE)
  expect_error(sr_compose_check(s), "clustered")
})

test_that("unknown extra layer names are ignored", {
  s <- mk_cspec()
  s$layers$something_else <- "whatever"
  expect_true(sr_compose_check(s)$ok)
})

test_that("compose.R is ASCII only", {
  f <- file.path(sr_root, "R", "design", "compose.R")
  skip_if_not(file.exists(f))
  expect_false(any(utf8ToInt(paste(readLines(f, warn = FALSE), collapse = "\n")) > 127L))
})
