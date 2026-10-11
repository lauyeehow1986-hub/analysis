# Tests for routes/terms.R: spec columns, the fixed-knot / fixed-level term builder and outcome coding.

fx_lung <- function() {
  d <- survival::lung
  d$sex <- as.integer(d$sex)
  d$ph.ecog <- as.integer(d$ph.ecog)
  d
}

# Run setup + design (+ outcome code) the way a route will: sr_data in scope, then .h_design(d).
fx_build <- function(spec, data, kind = NULL) {
  tc <- sr_terms_code(spec, data = data)
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
  # coef_map names equal the names coef() returns (without the intercept)
  expect_identical(b$tc$coef_map$coef, names(coef(f1))[-1])
  expect_identical(b$tc$coef_map$kind, c("linear", "rcs", "rcs", "rcs"))
  expect_identical(b$tc$coef_map$source_var, c("sex", "age", "age", "age"))
})

test_that("explicit knots are used as given and a bad knot spec is refused", {
  lung <- fx_lung()
  spec <- list(outcome = "status", event_value = 2,
               terms = list(list(var = "age", type = "rcs", knots = c(70, 50, 60))))
  b <- fx_build(spec, lung)
  expect_identical(unname(b$env$.sr_fixed$knots[["t1"]]), c(50, 60, 70))
  expect_identical(b$tc$rhs, "t1 + t1_s1")
  bad <- function(k) sr_terms_code(list(outcome = "status", terms = list(list(var = "age", type = "rcs", knots = k))), data = lung)
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
  expect_identical(levels(b$des$t2), c("2", "0", "1", "3"))
  sub <- lung[which(lung$ph.ecog %in% c(0, 1)), ]
  des_sub <- b$env$.h_design(sub)
  expect_identical(levels(des_sub$t2), c("2", "0", "1", "3"))
  f <- lm(as.formula(paste(".y ~", b$tc$rhs)), data = transform(b$des, .y = as.numeric(lung$status == 2)))
  expect_identical(b$tc$coef_map$coef, names(coef(f))[-1])
  expect_identical(b$tc$coef_map$coef, c("t1", "t20", "t21", "t23"))
  expect_identical(b$tc$coef_map$kind, c("linear", "factor", "factor", "factor"))
  expect_identical(b$tc$coef_map$source_var, c("sex", "ph.ecog", "ph.ecog", "ph.ecog"))
  # no ref: first sorted level is the reference
  spec$terms[[1]]$ref <- NULL
  b2 <- fx_build(spec, lung)
  expect_identical(b2$env$.sr_fixed$levels[["t2"]], c("0", "1", "2", "3"))
  # a ref that is not an observed level is refused when the code runs
  spec$terms[[1]]$ref <- "9"
  expect_error(suppressMessages(fx_build(spec, lung)), "reference level")
})

test_that("a factor exposure takes its levels from exposure_ref; a quadratic term adds t<i>_q", {
  lung <- fx_lung()
  lung$grp <- ifelse(lung$sex == 1, "male", "female")
  spec <- list(outcome = "status", event_value = 2, exposure = "grp", exposure_ref = "male",
               terms = list(list(var = "age", type = "quad")))
  b <- fx_build(spec, lung)
  expect_identical(b$env$.sr_fixed$levels[["t1"]], c("male", "female"))
  expect_identical(b$tc$rhs, "t1 + t2 + t2_q")
  expect_identical(b$tc$coef_map$coef, c("t1female", "t2", "t2_q"))
  expect_identical(b$tc$coef_map$kind, c("factor", "quad", "quad"))
  expect_equal(b$des$t2_q, b$des$t2^2, tolerance = 0)
  f <- lm(as.formula(paste("as.numeric(lung$status == 2) ~", b$tc$rhs)), data = b$des)
  expect_identical(b$tc$coef_map$coef, names(coef(f))[-1])
})

test_that("interactions map variable names to derived terms and coef_map follows coef()", {
  lung <- fx_lung()
  spec <- list(outcome = "status", event_value = 2, exposure = "sex",
               terms = list(list(var = "age", type = "linear"),
                            list(var = "ph.ecog", type = "factor", ref = "0")),
               interactions = list(c("sex", "age"), c("ph.ecog", "sex")))
  b <- fx_build(spec, lung)
  expect_identical(b$tc$rhs, "t1 + t2 + t3 + t1:t2 + t1:t3")
  f <- lm(as.formula(paste("as.numeric(lung$status == 2) ~", b$tc$rhs)), data = b$des)
  expect_identical(b$tc$coef_map$coef, names(coef(f))[-1])
  ix <- b$tc$coef_map[b$tc$coef_map$kind == "interaction", ]
  expect_identical(unique(ix$source_var), c("sex:age", "sex:ph.ecog"))
})

test_that("interactions with a quad or rcs term, an unknown variable or a self pair are refused", {
  lung <- fx_lung()
  mk <- function(type, ia) {
    list(outcome = "status", event_value = 2, exposure = "sex",
         terms = list(list(var = "age", type = type), list(var = "wt.loss", type = "linear")),
         interactions = ia)
  }
  msg <- "interactions are between the exposure and linear or factor terms in P1c"
  expect_error(sr_terms_code(mk("quad", list(c("sex", "age"))), data = lung), msg, fixed = TRUE)
  expect_error(sr_terms_code(mk("rcs", list(c("age", "wt.loss"))), data = lung), msg, fixed = TRUE)
  expect_error(sr_terms_code(mk("linear", list(c("sex", "nope"))), data = lung), "not in the model")
  expect_error(sr_terms_code(mk("linear", list(c("sex", "sex"))), data = lung), "two different")
  expect_error(sr_terms_code(mk("linear", list("sex")), data = lung), "length-2")
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
  expect_identical(b$tc$coef_map$source_var[b$tc$coef_map$kind == "rcs"], rep(hostile, 2))
  f <- lm(as.formula(paste(".y ~", b$tc$rhs)), data = b$des)
  expect_identical(b$tc$coef_map$coef, names(coef(f))[-1])
})

test_that("a passthrough column named like a derived column is refused; bad specs fail early", {
  d <- data.frame(t1 = c(0, 1, 1, 0), e = 1:4)
  expect_error(sr_terms_code(list(outcome = "t1", exposure = "e"), data = d), "derived")
  expect_error(sr_terms_code(list(outcome = "y", terms = list(list(var = "a", type = "cubic"))), data = d), "type")
  expect_error(sr_terms_code(list(outcome = "y", exposure = "e", terms = list(list(var = "e", type = "linear"))), data = d), "twice")
  expect_error(sr_terms_code(list(outcome = "y", terms = list(list(var = "a", type = "factor"))), data = NULL),
               "data")
})

test_that("no exposure and no terms gives an intercept-only rhs", {
  tc <- sr_terms_code(list(outcome = "y"))
  expect_identical(tc$rhs, "1")
  expect_identical(nrow(tc$coef_map), 0L)
  expect_identical(names(tc$coef_map), c("coef", "source_var", "kind"))
})