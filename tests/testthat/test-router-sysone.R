# Tests for router/sysone.R and router/abstain.R: the System One contract and the abstain rule.

.so_crit <- function(n) {
  k <- sprintf("k%03d", seq_len(n))
  stats::setNames(as.list(rep("d", n)), k)
}

test_that("sr_sysone_choice enforces the criteria limits", {
  expect_error(sr_sysone_choice("pick", .so_crit(256L)), "255")
  expect_error(sr_sysone_choice("pick", list()))
  expect_error(sr_sysone_choice("pick", list(a = "x", a = "y")))
  expect_error(sr_sysone_choice("pick", list(a = "x", "y")))
  expect_error(sr_sysone_choice("", list(a = "x")))
  ch <- sr_sysone_choice("pick", .so_crit(255L))
  expect_identical(ch$type, "choice")
  expect_length(ch$criteria, 255L)
})

test_that("the request body is canonical and its sha is stable", {
  q1 <- sr_sysone_choice("one", list(a = "A", b = "B"))
  q2 <- sr_sysone_choice("two", list(x = "X", y = "Y"))
  r1 <- sr_sysone_request("state", list(first = q1, second = q2), model = "m")
  r2 <- sr_sysone_request("state", list(second = q2, first = q1), model = "m")
  expect_identical(sr_sysone_sha(r1), sr_sysone_sha(r2))
  expect_identical(sr_sysone_body(r1), sr_sysone_body(r2))
  expect_match(sr_sysone_sha(r1), "^[0-9a-f]{64}$")
  r3 <- sr_sysone_request("other", list(first = q1, second = q2), model = "m")
  expect_false(identical(sr_sysone_sha(r1), sr_sysone_sha(r3)))
})

test_that("sr_sysone_confidence follows the TypeSafe formula", {
  expect_equal(sr_sysone_confidence(c(a = 0.7, b = 0.2, c = 0.1)), (0.7 - 1 / 3) / (2 / 3))
  expect_equal(sr_sysone_confidence(c(a = 0.25, b = 0.25, c = 0.25, d = 0.25)), 0)
  expect_equal(sr_sysone_confidence(c(a = 1)), 1)
})

.so_req <- function() {
  sr_sysone_request("s", list(
    design = sr_sysone_choice("design?", list(case_control = "cc", cohort_retrospective = "cr",
                                              rct = "r"))), model = "m")
}

test_that("sr_sysone_parse accepts a well-formed answer", {
  resp <- list(answers = list(design = list(type = "choice", choice = "case_control",
    probabilities = list(case_control = 0.8, cohort_retrospective = 0.2), confidence = 0.99)))
  out <- sr_sysone_parse(resp, .so_req())
  expect_named(out, "design")
  d <- out$design
  expect_true(d$ok)
  expect_identical(d$choice, "case_control")
  expect_equal(unname(d$probabilities[c("case_control", "cohort_retrospective", "rct")]),
               c(0.8, 0.2, 0))
  expect_equal(d$confidence, (0.8 - 1 / 3) / (2 / 3))
})

test_that("sr_sysone_parse rejects malformed answers", {
  one <- function(ans) {
    a <- if (is.null(ans)) list() else list(design = ans)
    sr_sysone_parse(list(answers = a), .so_req())$design
  }
  cases <- list(
    badchoice = list(type = "choice", choice = "zzz", probabilities = list(case_control = 1)),
    sum12 = list(type = "choice", choice = "rct", probabilities = list(rct = 0.7, case_control = 0.5)),
    negative = list(type = "choice", choice = "rct", probabilities = list(rct = 1.2, case_control = -0.2)),
    missing = NULL,
    score = list(type = "score", choice = "rct", probabilities = list(rct = 1)),
    badnames = list(type = "choice", choice = "rct", probabilities = list(rct = 0.5, nope = 0.5))
  )
  for (nm in names(cases)) {
    r <- one(cases[[nm]])
    expect_false(r$ok, info = nm)
    expect_true(is.character(r$error) && nzchar(r$error), info = nm)
  }
})

test_that("sr_abstain acts only below the ask cost", {
  cost <- sr_cost_matrix(c("a", "b"))
  r1 <- sr_abstain(c(a = 0.95, b = 0.05), cost)
  expect_true(r1$act)
  expect_identical(r1$choice, "a")
  r2 <- sr_abstain(c(a = 0.85, b = 0.15), cost)
  expect_false(r2$act)
})

test_that("a dangerous cell moves the choice", {
  cost <- sr_cost_matrix(c("a", "b"), danger = list(c("b", "a")))
  expect_equal(cost["b", "a"], SR_ROUTER_DANGER_COST)
  expect_error(sr_cost_matrix(c("a", "b"), danger = list(c("b", "zz"))))
  r <- sr_abstain(c(a = 0.6, b = 0.4), cost)
  expect_equal(r$expected_cost, c(a = 4, b = 0.6))
  expect_identical(r$choice, "b")
  expect_false(r$act)
})

test_that("the backend registry", {
  sr_backend_register("fake", ask = function(request, ctx) list(ok = TRUE),
                      available = function(project) list(ok = TRUE, reason = ""))
  withr::defer(rm(list = "fake", envir = .sr_backend_env))
  expect_true("fake" %in% sr_backends())
  expect_true(is.function(.sr_backend_get("fake")$ask))
  expect_error(.sr_backend_get("nope"), "unknown backend")
})

test_that("sr_sysone_parse matches the answer fields exactly (no partial matching)", {
  one <- function(ans) sr_sysone_parse(list(answers = list(design = ans)), .so_req())$design
  pr <- list(rct = 0.9, case_control = 0.1)
  cases <- list(
    typ = list(typ = "choice", choice = "rct", probabilities = pr),
    typex = list(typeX = "choice", choice = "rct", probabilities = pr),
    choices = list(type = "choice", choices = "rct", probabilities = pr),
    probs = list(type = "choice", choice = "rct", probabilitiesX = pr)
  )
  for (nm in names(cases)) {
    r <- one(cases[[nm]])
    expect_false(r$ok, info = nm)
    expect_true(is.character(r$error) && nzchar(r$error), info = nm)
  }
  expect_false(sr_sysone_parse(list(answersX = list(design = list(type = "choice", choice = "rct",
                                                                  probabilities = pr))), .so_req())$design$ok)
  expect_true(one(list(type = "choice", choice = "rct", probabilities = pr))$ok)
})
