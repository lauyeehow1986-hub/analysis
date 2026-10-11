# Tests for routes/emit.R: quoting, literals, helper inlining and the emitted-code checker.
# Text with backslashes, quotes and newlines is built from code points.

bs <- intToUtf8(92L)
nl <- intToUtf8(10L)
dq <- intToUtf8(34L)
hostile <- c("a b", "x`); system('calc'); (`", paste0("new", nl, "line"), bs, intToUtf8(0xE9L),
             "if", "x$y", "1abc", "a.b", paste0("tab", intToUtf8(9L), "x"), "p.value")

test_that(".sr_q round-trips any name as exactly one symbol", {
  for (x in hostile) {
    code <- .sr_q(x)
    nm <- names(eval(parse(text = paste0("list(", code, " = 1)"))))
    expect_identical(nm, x)
    expect_length(all.names(parse(text = code)[[1L]]), 1L)
    expect_length(parse(text = code), 1L)
  }
  expect_identical(.sr_q("age"), "age")
})

test_that(".sr_q refuses anything but one non-empty string", {
  expect_error(.sr_q(NA_character_), "non-NA")
  expect_error(.sr_q(""), "non-empty")
  expect_error(.sr_q(c("a", "b")), "one")
  expect_error(.sr_q(1), "string")
  expect_error(.sr_q(NULL), "string")
})

test_that(".sr_lit round-trips atomic vectors on one line", {
  rt <- function(x) {
    code <- .sr_lit(x)
    expect_false(grepl(nl, code, fixed = TRUE))
    expect_identical(eval(parse(text = code)), x)
  }
  rt(c(1 / 3, NA, Inf, -0))
  rt(NA_character_)
  rt(paste0("a", dq, "b", nl))
  rt(c(paste0("q", dq, bs, "z"), intToUtf8(0xE9L), NA))
  rt(TRUE)
  rt(5L)
  rt(c(a = 1.5, b = NA))
  rt(seq(0.1, 3, by = 0.1))
  rt(1e-300)
  rt(pi * 1e10)
  rt(factor(c("u", "v", "u")))
  rt(as.Date("2026-01-02"))
  rt(character())
  rt(NULL)
  rt(list(a = 1, b = list("x", NA_integer_)))
  rt(data.frame(x = 1:3, y = c("a", NA, "c")))
  # 17 digits, not 15
  expect_match(.sr_lit(0.1 + 0.2), "0.30000000000000004", fixed = TRUE)
})

test_that(".sr_lit refuses values that are not plain data", {
  expect_error(.sr_lit(mean), "plain")
  expect_error(.sr_lit(y ~ x), "plain")
  expect_error(.sr_lit(globalenv()), "plain")
})

test_that(".sr_inline is deterministic, parses and defines a working function", {
  fn <- function(x) x + 1
  a <- .sr_inline(fn, ".h_add")
  expect_identical(a, .sr_inline(fn, ".h_add"))
  expect_true(startsWith(a, ".h_add <- function"))
  env <- new.env()
  eval(parse(text = a), env)
  expect_identical(env$.h_add(2), 3)
  # the closure's environment is not part of the text
  make <- function(k) function(x) x + 1
  expect_identical(.sr_inline(make(1), ".h_add"), .sr_inline(make(2), ".h_add"))
  # a multi-line function stays one definition
  big <- function(x) {
    y <- x * 2
    if (y > 3) y else -y
  }
  env2 <- new.env()
  eval(parse(text = .sr_inline(big, ".h_big")), env2)
  expect_identical(env2$.h_big(2), 4)
  expect_length(parse(text = .sr_inline(big, ".h_big")), 1L)
})

test_that(".sr_inline validates the name and the function", {
  fn <- function(x) x
  for (bad in c("h_add", ".h_Add", ".h_", ".h_a-b", "add", ".h_a b", NA_character_, "")) {
    expect_error(.sr_inline(fn, bad), "name")
  }
  expect_error(.sr_inline("not a function", ".h_a"), "function")
})

test_that(".sr_value_plain accepts plain data only", {
  expect_true(.sr_value_plain(list(a = 1, b = data.frame(x = 1))))
  expect_true(.sr_value_plain(NULL))
  expect_true(.sr_value_plain(factor("a")))
  expect_true(.sr_value_plain(matrix(1:4, 2)))
  expect_true(.sr_value_plain(list(a = NULL, b = list(c = "z"))))
  expect_false(.sr_value_plain(list(f = lm)))
  expect_false(.sr_value_plain(y ~ x))
  expect_false(.sr_value_plain(quote(f(x))))
  expect_false(.sr_value_plain(as.name("a")))
  expect_false(.sr_value_plain(globalenv()))
  expect_false(.sr_value_plain(list(a = 1, e = new.env())))
  expect_false(.sr_value_plain(lm(dist ~ speed, data = cars)))
  # an environment hidden in an attribute
  expect_false(.sr_value_plain(structure(1, hidden = new.env())))
})

# ---- the checker ----

test_that(".sr_emit_check rejects the forbidden calls, naming the call", {
  bad <- c(
    'system("x")', 'base::system("x")', 'utils::download.file("u")', "foo::bar()",
    "eval(quote(1))", 'do.call("system", list())', 'Sys.setenv(A = "1")', 'setwd("x")',
    'unlink("x")', 'source("x")', "library(survival)", "get(nm)", 'assign("a", 1)',
    'saveRDS(1, "f")', 'writeLines("a", "f")', "q()", 'shell("dir")', 'file.copy("a", "b")',
    "url('u')", "base:::system('x')", 'do.call(paste0("sy", "stem"), list())',
    "lapply(1:2, system)", 'sapply("calc", "system")', 'Reduce("system", list())',
    "f()()", "(function() system('x'))()", "x$f(1)",
    "function(a = system('x')) a", "local(system('x'))", "baseenv()", "match.fun(nm)(1)"
  )
  for (code in bad) {
    expect_error(.sr_emit_check(code, "analysis"), "rejected", info = code)
    expect_error(.sr_emit_check(code, "diagnostic"), "rejected", info = code)
  }
  expect_error(.sr_emit_check('system("x")'), "system")
  expect_error(.sr_emit_check('base::system("x")'), "system")
  expect_error(.sr_emit_check('utils::download.file("u")'), "download.file")
  expect_error(.sr_emit_check("foo::bar()"), "foo")
})

test_that(".sr_emit_check passes ordinary analysis code", {
  ok <- c(
    "stats::lm(y ~ x, data = d)",
    "fit <- survival::coxph(survival::Surv(t, e) ~ x, data = d)",
    "m <- stats::glm(y ~ splines::ns(x, df = 3), family = binomial, data = d)",
    "r <- lapply(split(d, d$g), function(s) mean(s$y))",
    "(function(x) x + 1)(2)",
    "out <- list(a = 1, b = c(1, 2)[2])",
    "x <- d[d$source > 1 & !is.na(d$url), ]",
    "stats::lm(`source` ~ `get` + `q`, data = d)",
    "if (nrow(d) > 3) 1 else 2",
    "f <- function(x, k = 2) { y <- x^k; y }",
    "Hmisc::rcspline.eval(x, knots = c(1, 2, 3), inclx = TRUE)",
    "do.call(rbind, parts)", "do.call(f, list(1))",
    "mice::mice(d, m = 5, printFlag = FALSE)",
    "sandwich::vcovHC(fit, type = 'HC3')", "logistf::logistf(y ~ x, data = d)",
    "parallel::nextRNGStream(s)", "utils::head(d)", "methods::is(d, 'data.frame')"
  )
  for (code in ok) expect_true(.sr_emit_check(code, "analysis"), info = code)
  expect_invisible(.sr_emit_check("stats::lm(y ~ x, data = d)"))
  # the empty-argument corners of the parse tree
  expect_true(.sr_emit_check("d[1, ]; d[, 2]; switch(k, a = , b = 1)"))
  expect_true(.sr_emit_check("function() NULL"))
})

test_that(".sr_emit_check: diagnostics are barred from analysis code, allowed in diagnostic code", {
  expect_error(.sr_emit_check("stats::shapiro.test(x)", "analysis"), "shapiro.test")
  expect_error(.sr_emit_check("shapiro.test(x)", "analysis"), "diagnostic")
  expect_error(.sr_emit_check("survival::cox.zph(f)", "analysis"), "cox.zph")
  expect_error(.sr_emit_check("r <- res$p.value", "analysis"), "p-value")
  expect_error(.sr_emit_check('r <- res[["pvalue"]]', "analysis"), "p-value")
  expect_error(.sr_emit_check("pval <- 1", "analysis"), "p-value")
  expect_true(.sr_emit_check("survival::cox.zph(f)", "diagnostic"))
  expect_true(.sr_emit_check("z <- survival::cox.zph(f); out <- z$table", "diagnostic"))
  expect_true(.sr_emit_check("r <- stats::shapiro.test(x); out <- list(w = r$statistic)", "diagnostic"))
})

test_that(".sr_emit_check: no branch may depend on a diagnostic", {
  expect_error(.sr_emit_check("if (survival::cox.zph(f)$table[1, 3] < 0.05) 1", "diagnostic"), "branch")
  expect_error(.sr_emit_check("if (stats::shapiro.test(x)$p.value > 0.05) 1 else 2", "diagnostic"), "branch")
  expect_error(.sr_emit_check("ifelse(stats::shapiro.test(x)$p.value > 0.05, 1, 2)", "diagnostic"), "branch")
  expect_error(.sr_emit_check("switch(survival::cox.zph(f)$table[1, 3] < 0.05, 1, 2)", "diagnostic"), "branch")
  expect_error(.sr_emit_check("while (stats::ks.test(x, 'pnorm')$statistic > 1) 1", "diagnostic"), "branch")
  # through a variable, and through a chain of them
  expect_error(.sr_emit_check("z <- survival::cox.zph(f); if (z$table[1, 3] < 0.05) 1", "diagnostic"), "branch")
  expect_error(.sr_emit_check("z <- survival::cox.zph(f); w <- z$table; if (w[1, 3] < 0.05) 1", "diagnostic"), "branch")
  # a $p access in a condition
  expect_error(.sr_emit_check("if (r$p < 0.05) 1", "diagnostic"), "branch")
  # branching on something else is fine, also next to a diagnostic that is only reported
  expect_true(.sr_emit_check("z <- survival::cox.zph(f); if (nrow(d) > 10) out <- z$table", "diagnostic"))
  expect_true(.sr_emit_check("p <- spec$p; if (p > 3) 1", "diagnostic"))
})

test_that(".sr_emit_check: prologue-only calls", {
  for (code in c('readRDS("f")', 'readBin("f", "raw", 10L)', 'file.size("f")', 'base::readRDS("f")',
                 "openssl::sha256(raw())")) {
    expect_error(.sr_emit_check(code, "analysis", prologue = FALSE), "rejected", info = code)
    expect_true(.sr_emit_check(code, "analysis", prologue = TRUE), info = code)
  }
  expect_error(.sr_emit_check('readRDS("f")', prologue = FALSE), "prologue")
  expect_error(.sr_emit_check("openssl::sha256(raw())"), "openssl")
  # the prologue is still barred from everything else
  expect_error(.sr_emit_check('system("x")', prologue = TRUE), "system")
  expect_error(.sr_emit_check('lapply("f", readRDS)', prologue = FALSE), "readRDS")
  expect_true(.sr_emit_check('lapply("f", readRDS)', prologue = TRUE))
})

test_that(".sr_emit_check reports code that does not parse, and bad input", {
  expect_error(.sr_emit_check("f(("), "parse")
  expect_error(.sr_emit_check(1), "string")
  expect_error(.sr_emit_check(c("a", "b")), "string")
  expect_error(.sr_emit_check(NA_character_), "string")
  expect_error(.sr_emit_check("1", kind = "other"))
})

test_that("hostile column names built with .sr_q pass the checker and add no call", {
  for (x in hostile) {
    code <- paste0("stats::lm(", .sr_q("y"), " ~ ", .sr_q(x), ", data = d)")
    expect_true(.sr_emit_check(code, "diagnostic"), info = x)
    # the calls in the tree are exactly lm, ~ and ::
    funs <- all.names(parse(text = code)[[1L]], functions = TRUE, unique = TRUE)
    expect_false(any(c("system", "eval") %in% funs), info = x)
  }
  hn <- "x`); system('calc'); (`"
  code <- paste0("list(", .sr_q(hn), " = ", .sr_lit(hn), ")")
  expect_true(.sr_emit_check(code, "analysis"))
  expect_identical(eval(parse(text = code)), setNames(list(hn), hn))
  nms <- all.names(parse(text = paste0("stats::lm(y ~ ", .sr_q(hn), ")")), functions = TRUE, unique = TRUE)
  expect_length(setdiff(nms, c("::", "stats", "lm", "~", "y", hn)), 0L)
})

test_that("emit.R and result.R pass the lint", {
  root <- normalizePath(testthat::test_path("../.."), winslash = "/")
  files <- file.path(root, "app", "R", "routes", c("emit.R", "result.R"))
  expect_identical(nrow(sr_lint_pgate(files)), 0L)
  expect_identical(nrow(sr_lint_airgap(files)), 0L)
})
