# Tests for routes/terms.R: spec columns, the fixed-knot / fixed-level term builder and outcome coding.

fx_lung <- function() {
  d <- survival::lung
  d$sex <- as.integer(d$sex)
  d$ph.ecog <- as.integer(d$ph.ecog)
  d
}

# Run setup + design (+ outcome code) the way a route will: sr_data in scope, then .h_design(d).
fx_build <- function(spec, data, kind = NULL) {
  tc <- sr_terms_code(spec)
  env <- new.env(parent = globalenv())
  env$sr_data <- data
  code <- paste(tc$setup, tc$design, sep = "\n")
  expect_silent(.sr_emit_check(code, kind = "analysis"))
  eval(parse(text = code), env)
  des <- env$.h_design(data)
  if (!is.null(kind)) {
    oc <- sr_outcome_code(spec, kind)
    expect_silent(.sr_emit_check(oc, kind = "analysis"))
    env$des <- des
    eval(parse(text = oc), env)
    des <- env$des
  }
  list(tc = tc, env = env, des = des)
}

test_that("sr_spec_cols returns the documented order without duplicates", {
  spec <- list(outcome = "o", time = "tm", status = "st", exposure = "ex",
               terms = list(list(var = "a", type = "linear"), list(var = "ex", type = "linear"),
                            list(var = "b", type = "factor")),
               cluster = "cl", strata = c("s1", "a"), mi = list(aux = c("x1", "cl", "x2")))
  expect_identical(sr_spec_cols(spec),
                   c("o", "tm", "st", "ex", "a", "b", "cl", "s1", "x1", "x2"))
  expect_identical(sr_spec_cols(list(outcome = "y", exposure = "x")), c("y", "x"))
  expect_identical(sr_spec_cols(list(outcome = "y", cluster = NULL, strata = NULL, mi = list(aux = NULL))), "y")
  expect_identical(sr_spec_cols(list()), character())
})

test_that("an RCS(4) term fixes knots from observed data and matches a direct fit", {
  lung <- fx_lung()
  spec <- list(outcome = "status", event_value = 2, exposure = "sex",
               terms = list(list(var = "age", type = "rcs", knots = 4)))
  b <- fx_build(spec, lung, "binary")
  knots <- Hmisc::rcspline.eval(lung$age, nk = 4, knots.only = TRUE)
  expect_equal(b$env$.sr_fixed$knots[["t2"]], knots, tolerance = 0)
  expect_true(all(c("t1", "t2", "t2_s1", "t2_s2") %in% names(b$des)))
  expect_identical(b$tc$rhs, "t1 + t2 + t2_s1 + t2_s2")
  basis <- Hmisc::rcspline.eval(lung$age, knots, inclx = TRUE)
  expect_equal(b$des$t2, as.numeric(basis[, 1]), tolerance = 1e-12)
  expect_equal(b$des$t2_s1, as.numeric(basis[, 2]), tolerance = 1e-12)
  expect_equal(b$des$t2_s2, as.numeric(basis[, 3]), tolerance = 1e-12)
  f1 <- lm(as.formula(paste(".y ~", b$tc$rhs)), data = b$des)
  y <- as.integer(lung$status == 2)
  f2 <- lm(y ~ lung$sex + basis)
  expect_equal(unname(coef(f1)), unname(coef(f2)), tolerance = 1e-10)
  # the static coef_map has one row per derived column ...
  expect_identical(b$tc$coef_map$coef, c("t1", "t2", "t2_s1", "t2_s2"))
  # ... and sr_coef_source maps the real coefficient names back
  src <- sr_coef_source(names(coef(f1))[-1], b$tc$coef_map)
  expect_identical(src$kind, c("linear", "rcs", "rcs", "rcs"))
  expect_identical(src$source_var, c("sex", "age", "age", "age"))
  expect_true(all(is.na(src$level)))
})

test_that("explicit knots are used as given and a bad knot spec is refused", {
  lung <- fx_lung()
  spec <- list(outcome = "status", event_value = 2,
               terms = list(list(var = "age", type = "rcs", knots = c(70, 50, 60))))
  b <- fx_build(spec, lung)
  expect_identical(unname(b$env$.sr_fixed$knots[["t1"]]), c(50, 60, 70))
  expect_identical(b$tc$rhs, "t1 + t1_s1")
  bad <- function(k) sr_terms_code(list(outcome = "status", terms = list(list(var = "age", type = "rcs", knots = k))))
  expect_error(bad(2), "knots")
  expect_error(bad(8), "knots")
  expect_error(bad(c(50, 60)), "knots")
  expect_error(bad("a"), "knots")
})

test_that("RCS basis columns do not change when the design is built on a subset", {
  lung <- fx_lung()
  spec <- list(outcome = "status", event_value = 2,
               terms = list(list(var = "age", type = "rcs", knots = 5)))
  b <- fx_build(spec, lung)
  sub <- lung[lung$age > 65 & !is.na(lung$age), ]
  des_sub <- b$env$.h_design(sub)
  expect_identical(des_sub$t1_s1, b$des$t1_s1[b$des$t1 > 65 & !is.na(b$des$t1)])
  expect_identical(rownames(des_sub), rownames(sub))
})

test_that("a factor term puts ref first, keeps all levels on a subset, and maps coefficients", {
  lung <- fx_lung()
  spec <- list(outcome = "status", event_value = 2, exposure = "sex",
               terms = list(list(var = "ph.ecog", type = "factor", ref = "2")))
  b <- fx_build(spec, lung)
  expect_identical(b$env$.sr_fixed$levels[["t2"]], c("2", "0", "1", "3"))
  expect_identical(levels(b$des$t2f), c("2", "0", "1", "3"))
  sub <- lung[which(lung$ph.ecog %in% c(0, 1)), ]
  des_sub <- b$env$.h_design(sub)
  expect_identical(levels(des_sub$t2f), c("2", "0", "1", "3"))
  expect_identical(b$tc$rhs, "t1 + t2f")
  f <- lm(as.formula(paste(".y ~", b$tc$rhs)), data = transform(b$des, .y = as.numeric(lung$status == 2)))
  expect_identical(names(coef(f))[-1], c("t1", "t2f0", "t2f1", "t2f3"))
  expect_identical(b$tc$coef_map$coef, c("t1", "t2f"))
  expect_identical(b$tc$coef_map$kind, c("linear", "factor"))
  src <- sr_coef_source(names(coef(f))[-1], b$tc$coef_map)
  expect_identical(src$kind, c("linear", "factor", "factor", "factor"))
  expect_identical(src$source_var, c("sex", "ph.ecog", "ph.ecog", "ph.ecog"))
  expect_identical(src$level, c(NA, "0", "1", "3"))
  # no ref: first sorted level is the reference
  spec$terms[[1]]$ref <- NULL
  b2 <- fx_build(spec, lung)
  expect_identical(b2$env$.sr_fixed$levels[["t2"]], c("0", "1", "2", "3"))
})

test_that("factor checks that need data run in the emitted setup", {
  lung <- fx_lung()
  spec <- list(outcome = "status", event_value = 2,
               terms = list(list(var = "ph.ecog", type = "factor", ref = "9")))
  tc <- sr_terms_code(spec)   # compiles without data
  run <- function(tc, data) {
    env <- new.env(parent = globalenv())
    env$sr_data <- data
    eval(parse(text = tc$setup), env)
  }
  expect_error(run(tc, lung), "reference level '9'")
  # a ref that is observed in the full table but absent from this table
  spec$terms[[1]]$ref <- "3"
  tc3 <- sr_terms_code(spec)
  expect_error(run(tc3, lung[lung$ph.ecog %in% c(0, 1), ]), "reference level '3'")
  # a single observed level
  tc1 <- sr_terms_code(list(outcome = "status", terms = list(list(var = "ph.ecog", type = "factor"))))
  expect_error(run(tc1, lung[which(lung$ph.ecog == 1), ]), "at least two")
})

test_that("a missing column stops the setup first, naming the columns", {
  lung <- fx_lung()
  spec <- list(outcome = "status", exposure = "sex",
               terms = list(list(var = "nope", type = "factor"), list(var = "gone", type = "linear")))
  tc <- sr_terms_code(spec)
  env <- new.env(parent = globalenv())
  env$sr_data <- lung
  expect_error(eval(parse(text = tc$setup), env), "column(s) not in the data: nope, gone", fixed = TRUE)
})

test_that("a factor exposure takes its levels from exposure_ref; a quadratic term adds t<i>_q", {
  lung <- fx_lung()
  lung$grp <- ifelse(lung$sex == 1, "male", "female")
  spec <- list(outcome = "status", event_value = 2, exposure = "grp", exposure_ref = "male",
               terms = list(list(var = "age", type = "quad")))
  b <- fx_build(spec, lung)
  expect_identical(b$env$.sr_fixed$levels[["t1"]], c("male", "female"))
  expect_identical(b$tc$rhs, "t1f + t2 + t2_q")
  expect_equal(b$des$t2_q, b$des$t2^2, tolerance = 0)
  f <- lm(as.formula(paste("as.numeric(lung$status == 2) ~", b$tc$rhs)), data = b$des)
  expect_identical(names(coef(f))[-1], c("t1ffemale", "t2", "t2_q"))
  src <- sr_coef_source(names(coef(f))[-1], b$tc$coef_map)
  expect_identical(src$kind, c("factor", "quad", "quad"))
  expect_identical(src$source_var, c("grp", "age", "age"))
  expect_identical(src$level, c("female", NA, NA))
})

test_that("interactions map variable names to derived terms and sr_coef_source resolves them", {
  lung <- fx_lung()
  spec <- list(outcome = "status", event_value = 2, exposure = "sex",
               terms = list(list(var = "age", type = "linear"),
                            list(var = "ph.ecog", type = "factor", ref = "0")),
               interactions = list(c("sex", "age"), c("ph.ecog", "sex")))
  b <- fx_build(spec, lung)
  expect_identical(b$tc$rhs, "t1 + t2 + t3f + t1:t2 + t1:t3f")
  expect_identical(b$tc$coef_map$coef, c("t1", "t2", "t3f", "t1:t2", "t1:t3f"))
  f <- lm(as.formula(paste("as.numeric(lung$status == 2) ~", b$tc$rhs)), data = b$des)
  nm <- names(coef(f))[-1]
  expect_identical(nm, c("t1", "t2", "t3f1", "t3f2", "t3f3", "t1:t2", "t1:t3f1", "t1:t3f2", "t1:t3f3"))
  src <- sr_coef_source(nm, b$tc$coef_map)
  expect_identical(src$source_var, c("sex", "age", rep("ph.ecog", 3), "sex:age", rep("sex:ph.ecog", 3)))
  expect_identical(src$kind, c("linear", "linear", rep("factor", 3), rep("interaction", 4)))
  expect_identical(src$level, c(NA, NA, "1", "2", "3", NA, "1", "2", "3"))
})

test_that("interactions with a quad or rcs term, an unknown variable or a self pair are refused", {
  mk <- function(type, ia) {
    list(outcome = "status", event_value = 2, exposure = "sex",
         terms = list(list(var = "age", type = type), list(var = "wt.loss", type = "linear")),
         interactions = ia)
  }
  msg <- "interactions are between the exposure and linear or factor terms in P1c"
  expect_error(sr_terms_code(mk("quad", list(c("sex", "age")))), msg, fixed = TRUE)
  expect_error(sr_terms_code(mk("rcs", list(c("age", "wt.loss")))), msg, fixed = TRUE)
  expect_error(sr_terms_code(mk("linear", list(c("sex", "nope")))), "not in the model")
  expect_error(sr_terms_code(mk("linear", list(c("sex", "sex")))), "two different")
  expect_error(sr_terms_code(mk("linear", list("sex"))), "length-2")
})

test_that("coefficient names never collide: factor t1 with levels 0 and 2 next to linear t10-t12", {
  set.seed(1)
  n <- 120
  d <- data.frame(f = sample(c(0, 1, 2), n, TRUE), y = rnorm(n))
  for (j in 2:12) d[[paste0("x", j)]] <- rnorm(n)
  spec <- list(outcome = "y", terms = c(
    list(list(var = "f", type = "factor", ref = "1")),
    lapply(2:12, function(j) list(var = paste0("x", j), type = "linear"))))
  b <- fx_build(spec, d, "continuous")
  f <- lm(as.formula(paste(".y ~", b$tc$rhs)), data = b$des)
  nm <- names(coef(f))[-1]
  expect_identical(anyDuplicated(nm), 0L)
  expect_true(all(c("t1f0", "t1f2", "t10", "t11", "t12") %in% nm))
  src <- sr_coef_source(nm, b$tc$coef_map)
  expect_identical(src$source_var[match(c("t1f0", "t1f2", "t10", "t11", "t12"), nm)],
                   c("f", "f", "x10", "x11", "x12"))
  expect_identical(src$level[match(c("t1f0", "t1f2", "t10"), nm)], c("0", "2", NA))
  expect_identical(src$kind[match(c("t1f0", "t10"), nm)], c("factor", "linear"))
})

test_that("sr_coef_source maps every coefficient of a factor exposure x linear interaction", {
  lung <- fx_lung()
  lung$wt <- lung$wt.loss
  spec <- list(outcome = "status", event_value = 2, time = "time", status = "status", status_event = 2,
               exposure = "ph.ecog", exposure_ref = "0",
               terms = list(list(var = "age", type = "linear")),
               interactions = list(c("ph.ecog", "age")))
  b <- fx_build(spec, lung, "binary")
  expect_identical(b$tc$rhs, "t1f + t2 + t1f:t2")
  fits <- list(
    lm = lm(as.formula(paste(".y ~", b$tc$rhs)), data = b$des),
    glm = suppressWarnings(glm(as.formula(paste(".y ~", b$tc$rhs)), data = b$des, family = binomial())),
    cox = survival::coxph(as.formula(paste("survival::Surv(time, status) ~", b$tc$rhs)), data = b$des)
  )
  for (nm in names(fits)) {
    cn <- setdiff(names(coef(fits[[nm]])), "(Intercept)")
    expect_identical(cn, c("t1f1", "t1f2", "t1f3", "t2", "t1f1:t2", "t1f2:t2", "t1f3:t2"))
    src <- sr_coef_source(cn, b$tc$coef_map)
    expect_identical(src$source_var, c(rep("ph.ecog", 3), "age", rep("ph.ecog:age", 3)))
    expect_identical(src$kind, c(rep("factor", 3), "linear", rep("interaction", 3)))
    expect_identical(src$level, c("1", "2", "3", NA, "1", "2", "3"))
  }
})

test_that("sr_coef_source maps every coefficient of a factor exposure x factor interaction", {
  lung <- fx_lung()
  lung$sx <- ifelse(lung$sex == 1, "male", "female")
  spec <- list(outcome = "status", event_value = 2, exposure = "ph.ecog", exposure_ref = "0",
               terms = list(list(var = "sx", type = "factor", ref = "male")),
               interactions = list(c("sx", "ph.ecog")))
  b <- fx_build(spec, lung, "binary")
  expect_identical(b$tc$rhs, "t1f + t2f + t1f:t2f")
  f <- lm(as.formula(paste(".y ~", b$tc$rhs)), data = b$des)
  cn <- setdiff(names(coef(f)), "(Intercept)")
  expect_identical(cn, c("t1f1", "t1f2", "t1f3", "t2ffemale", "t1f1:t2ffemale", "t1f2:t2ffemale", "t1f3:t2ffemale"))
  src <- sr_coef_source(cn, b$tc$coef_map)
  expect_identical(src$source_var, c(rep("ph.ecog", 3), "sx", rep("ph.ecog:sx", 3)))
  expect_identical(src$kind, c(rep("factor", 4), rep("interaction", 3)))
  expect_identical(src$level, c("1", "2", "3", "female", "1:female", "2:female", "3:female"))
})

test_that("sr_coef_source keeps a colon inside a factor level, also in a factor-first interaction", {
  set.seed(2)
  n <- 90
  d <- data.frame(y = rnorm(n), g = sample(c("0", "x:y", "a:b"), n, TRUE), z = rnorm(n),
                  h = sample(c("p", "q:r"), n, TRUE), stringsAsFactors = FALSE)
  spec <- list(outcome = "y", exposure = "g", exposure_ref = "0",
               terms = list(list(var = "z", type = "linear"), list(var = "h", type = "factor", ref = "p")),
               interactions = list(c("g", "z"), c("g", "h")))
  b <- fx_build(spec, d, "continuous")
  f <- lm(as.formula(paste(".y ~", b$tc$rhs)), data = b$des)
  cn <- setdiff(names(coef(f)), "(Intercept)")
  src <- sr_coef_source(cn, b$tc$coef_map)
  expect_identical(anyDuplicated(cn), 0L)
  i <- match(c("t1fx:y", "t1fa:b", "t3fq:r", "t1fx:y:t2", "t1fx:y:t3fq:r"), cn)
  expect_false(anyNA(i))
  expect_identical(src$kind[i], c("factor", "factor", "factor", "interaction", "interaction"))
  expect_identical(src$level[i], c("x:y", "a:b", "q:r", "x:y", "x:y:q:r"))
  expect_identical(src$source_var[i], c("g", "g", "h", "g:z", "g:h"))
  # every coefficient resolved
  expect_identical(nrow(src), length(cn))
})
test_that("sr_coef_source refuses unknown, ambiguous and out-of-model names", {
  cm <- sr_terms_code(list(outcome = "y", exposure = "a",
                           terms = list(list(var = "b", type = "linear"), list(var = "c", type = "factor")),
                           interactions = list(c("b", "c"))))$coef_map
  expect_error(sr_coef_source("zzz", cm), "does not belong")
  expect_error(sr_coef_source("t3f", cm), "does not belong")   # a factor prefix alone is no coefficient
  expect_error(sr_coef_source("t1:t2", cm), "does not belong")  # not an interaction of the model
  expect_error(sr_coef_source("t1:t3fx", cm), "does not belong")
  dup <- rbind(cm, cm[1, ])
  expect_error(sr_coef_source("t1", dup), "more than one")
  expect_identical(nrow(sr_coef_source(character(), cm)), 0L)
  # a level containing a colon still resolves
  s <- sr_coef_source(c("t3fu:v", "t2:t3fu:v"), cm)
  expect_identical(s$level, c("u:v", "u:v"))
  expect_identical(s$kind, c("factor", "interaction"))
})

test_that("event_value codes the binary outcome and survival outcomes get .time / .event", {
  lung <- fx_lung()
  spec <- list(outcome = "status", event_value = 2, exposure = "sex")
  b <- fx_build(spec, lung, "binary")
  expect_identical(b$des$.y, as.integer(lung$status == 2))
  spec$time <- "time"
  spec$status <- "status"
  spec$status_event <- 2
  s <- fx_build(spec, lung, "surv")
  expect_identical(s$des$.event, as.integer(lung$status == 2))
  expect_identical(s$des$.time, as.numeric(lung$time))
  # 0/1 and logical outcomes need no event_value; a 3-valued one does
  d <- data.frame(y = c(0, 1, 1, NA, 0), z = c(TRUE, FALSE, NA, TRUE, TRUE), w = c(1, 2, 3, 1, 2), x = 1:5)
  expect_identical(fx_build(list(outcome = "y", exposure = "x"), d, "binary")$des$.y, c(0L, 1L, 1L, NA, 0L))
  expect_identical(fx_build(list(outcome = "z", exposure = "x"), d, "binary")$des$.y, c(1L, 0L, NA, 1L, 1L))
  expect_error(fx_build(list(outcome = "w", exposure = "x"), d, "binary"), "event_value")
  expect_error(sr_outcome_code(list(outcome = "w"), "surv"), "time")
  cont <- fx_build(list(outcome = "w", exposure = "x"), d, "continuous")
  expect_identical(cont$des$.y, as.numeric(d$w))
  # character outcome with a text event_value
  d$s <- c("yes", "no", "yes", NA, "no")
  expect_identical(fx_build(list(outcome = "s", event_value = "yes", exposure = "x"), d, "binary")$des$.y,
                   c(1L, 0L, 1L, NA, 0L))
})

test_that("hostile column names and levels reach emitted code only as quoted names and literals", {
  hostile <- "x`); system('calc'); ("
  gname <- paste0("g", intToUtf8(10L), "q")
  d <- data.frame(y = c(0, 1, 1, 0, 1, 0, 1, 1), stringsAsFactors = FALSE)
  d[[hostile]] <- c(1.5, 2, 3.5, 4, 5.5, 6, 7.5, 9)
  d[[gname]] <- rep(c("a`);system('x');(", "b"), 4)
  d$e <- c(0, 1, 0, 1, 1, 0, 0, 1)
  spec <- list(outcome = "y", exposure = "e", cluster = hostile,
               terms = list(list(var = hostile, type = "rcs", knots = 3),
                            list(var = gname, type = "factor", ref = "b")),
               interactions = list(c("e", gname)))
  b <- fx_build(spec, d, "binary")
  code <- paste(b$tc$setup, b$tc$design, sep = "\n")
  expect_silent(.sr_emit_check(code, kind = "analysis"))
  expect_false("system" %in% all.names(parse(text = code)))
  expect_identical(b$des[[hostile]], d[[hostile]])
  f <- lm(as.formula(paste(".y ~", b$tc$rhs)), data = b$des)
  src <- sr_coef_source(names(coef(f))[-1], b$tc$coef_map)
  expect_identical(src$source_var[src$kind == "rcs"], rep(hostile, 2))
  expect_true(all(src$source_var[src$kind == "factor"] == gname))
  expect_identical(src$level[src$kind == "factor"], "a`);system('x');(")
})

test_that("a passthrough column named like a derived column is refused; bad specs fail early", {
  expect_error(sr_terms_code(list(outcome = "t1", exposure = "e")), "derived")
  expect_error(sr_terms_code(list(outcome = "t2f", exposure = "e")), "derived")
  expect_error(sr_terms_code(list(outcome = "y", terms = list(list(var = "a", type = "cubic")))), "type")
  expect_error(sr_terms_code(list(outcome = "y", exposure = "e", terms = list(list(var = "e", type = "linear")))), "twice")
})

test_that("no exposure and no terms gives an intercept-only rhs", {
  tc <- sr_terms_code(list(outcome = "y"))
  expect_identical(tc$rhs, "1")
  expect_identical(nrow(tc$coef_map), 0L)
  expect_identical(names(tc$coef_map), c("coef", "source_var", "kind"))
})

test_that(".sr_outcome_cov_refuse names the first outcome column used as a term, interaction, stratum or cluster", {
  msg <- function(c) paste0("^the outcome column '", c, "' cannot be a term, stratum or cluster$")
  expect_invisible(.sr_outcome_cov_refuse(list(outcome = "y", exposure = "x", terms = list(list(var = "z")))))
  expect_error(.sr_outcome_cov_refuse(list(outcome = "y", terms = list(list(var = "y")))), msg("y"))
  expect_error(.sr_outcome_cov_refuse(list(outcome = "y", interactions = list(c("x", "y")))), msg("y"))
  expect_error(.sr_outcome_cov_refuse(list(time = "t", status = "s", strata = "s")), msg("s"))
  expect_error(.sr_outcome_cov_refuse(list(time = "t", status = "s", cluster = "t")), msg("t"))
  # the first offending outcome column in the order outcome, time, status
  expect_error(.sr_outcome_cov_refuse(list(time = "t", status = "s", strata = "s", cluster = "t")), msg("t"))
})

test_that("the glm, lm, cox and rmst checks refuse an outcome column as a covariate (positive controls pass)", {
  msg <- function(c) paste0("^the outcome column '", c, "' cannot be a term, stratum or cluster$")
  put <- function(s, v) { s[names(v)] <- v; s }    # replaces fields whole (modifyList would merge terms)
  lay <- .sr_cov_layers()
  bin <- list(purpose = "explain-causal", outcome = "y", exposure = "x", terms = list(list(var = "z")),
              layers = lay)
  expect_invisible(.sr_glm_check(bin, "logistic"))
  expect_error(.sr_glm_check(put(bin, list(terms = list(list(var = "y")))), "logistic"), msg("y"))
  expect_error(.sr_glm_check(put(bin, list(cluster = "y", se = "cluster",
                                          layers = .sr_cov_layers(clustered = TRUE))), "modpois"),
               msg("y"))
  expect_invisible(.sr_lm_check(bin))
  expect_error(.sr_lm_check(put(bin, list(interactions = list(c("x", "y"))))), msg("y"))
  expect_error(.sr_lm_check(put(bin, list(strata = "y"))), msg("y"))
  surv <- list(purpose = "explain-causal", time = "time", status = "status", exposure = "x",
               terms = list(list(var = "z")), layers = lay)
  expect_invisible(.sr_cox_check(surv))
  expect_error(.sr_cox_check(put(surv, list(strata = "status"))), msg("status"))
  expect_error(.sr_cox_check(put(surv, list(terms = list(list(var = "time"))))), msg("time"))
  expect_error(.sr_cox_check(put(surv, list(cluster = "time", layers = .sr_cov_layers(clustered = TRUE)))),
               msg("time"))
  rm <- list(purpose = "explain-causal", time = "time", status = "status", exposure = "x", tau = 5,
             layers = lay, seed = 1)
  expect_invisible(.sr_rmst_check(rm))
  expect_error(.sr_rmst_check(put(rm, list(cluster = "status"))), msg("status"))
  expect_error(.sr_rmst_check(put(rm, list(terms = list(list(var = "time"))))), msg("time"))
  # through the route: spec validation reports the refusal from the function-form estimand
  v <- sr_spec_validate(put(surv, list(strata = "status", seed = 1)), .sr_cox_route())
  expect_false(v$ok)
  expect_true(any(grepl("the outcome column 'status' cannot be a term, stratum or cluster", v$errors, fixed = TRUE)))
})
