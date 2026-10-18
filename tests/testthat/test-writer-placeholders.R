# Tests for writer/placeholders.R: the locked placeholder registry, the number lift and the source
# verify. Synthetic data only (the writer fixture of helper-writer.R).

test_that("sr_ph_put hands out E and L tokens in order and returns an existing (source, kind) as is", {
  reg <- sr_ph_registry()
  expect_s3_class(reg, "sr_ph")
  expect_identical(sr_ph_put(reg, "1.70", "est", "effect:a:1:est"), "{{E1}}")
  expect_identical(sr_ph_put(reg, "0.011", "p", "effect:a:1:p"), "{{E2}}")
  expect_identical(sr_ph_put(reg, "1.70", "est", "effect:a:1:est"), "{{E1}}")
  expect_identical(sr_ph_put(reg, "grp", "label", "label:grp"), "{{L1}}")
  expect_identical(sr_ph_put(reg, "1.70", "num", "effect:a:1:est"), "{{E3}}")   # same source, other kind
  expect_identical(sr_ph_put(reg, "age", "label", "label:age"), "{{L2}}")
  tb <- sr_ph_table(reg)
  expect_identical(tb$id, c("E1", "E2", "E3", "L1", "L2"))
  expect_identical(names(tb), c("id", "kind", "text", "value", "analysis", "source", "scale", "null",
                                "suppressed"))
  expect_error(sr_ph_put(reg, "1", "bogus", "effect:a:1:est"), "kind")
  expect_error(sr_ph_put(reg, "1", "est", "effect:b:1:est", scale = "other"), "scale")
})

test_that("sr_ph_resolve fills E and L tokens, drops or renders citations, and names an unknown token", {
  reg <- sr_ph_registry()
  e1 <- sr_ph_put(reg, "1.70", "est", "effect:a:1:est")
  l1 <- sr_ph_put(reg, "grp", "label", "label:grp")
  x <- sprintf("OR %s [%s]{{C:zou2004}}", e1, l1)
  expect_identical(sr_ph_resolve(x, reg), "OR 1.70 [grp]")
  expect_identical(sr_ph_resolve(x, reg, cite = function(key) "[1]"), "OR 1.70 [grp][1]")
  expect_identical(sr_ph_resolve(c("no tokens", NA), reg), c("no tokens", NA_character_))
  expect_error(sr_ph_resolve("see {{E99}}", reg), "E99")
  expect_error(sr_ph_resolve("see {{L7}}", reg), "L7")
})

test_that("sr_ph_lift turns every number into a placeholder and leaves digits inside names alone", {
  reg <- sr_ph_registry()
  txt <- paste("Logistic regression of y on grp; 398 row(s) analysed (120 events), 2 excluded;",
               "95% CI; spline with 3 knots; z1 and age_10 kept.")
  out <- sr_ph_lift(txt, reg, "fragment:a_lr:glm_note", "a_lr")
  tb <- sr_ph_table(reg)
  expect_identical(nrow(tb), 5L)
  expect_identical(tb$text, c("398", "120", "2", "95%", "3"))
  expect_identical(tb$kind, c("count", "count", "count", "num", "num"))
  expect_equal(tb$value, c(398, 120, 2, 95, 3))
  expect_identical(tb$source, sprintf("fragment:a_lr:glm_note:%d", 1:5))
  expect_true(all(tb$analysis == "a_lr"))
  expect_identical(sr_ph_digits(out), character(0))
  expect_match(out, "z1 and age_10 kept", fixed = TRUE)
  expect_identical(sr_ph_resolve(out, reg), txt)
  # a count word may follow an opening bracket
  reg2 <- sr_ph_registry()
  sr_ph_lift("5 (events) and 7 (knots)", reg2, "fragment:a:f", "a")
  expect_identical(sr_ph_table(reg2)$kind, c("count", "num"))
  # a text with no number is returned as it is
  expect_identical(sr_ph_lift("no number here", reg2, "fragment:a:g", "a"), "no number here")
})

test_that("sr_ph_digits sees only the digits outside placeholder tokens", {
  expect_identical(sr_ph_digits("the {{L1}} group"), character(0))
  expect_identical(sr_ph_digits("over 65 years"), "65")
  expect_identical(sr_ph_digits("OR {{E1}} (95% CI {{E2}}); n = 40.5, z1, age_10 and -3"), c("95%", "40.5", "-3"))
  expect_identical(sr_ph_digits(c("a 1", "b 2 and 3")), c("1", "2", "3"))
})

test_that("a label with digits is a placeholder, so its digits do not count", {
  reg <- sr_ph_registry()
  l <- sr_ph_put(reg, "65+", "label", "label:65+")
  expect_identical(sr_ph_digits(paste("the", l, "group")), character(0))
  expect_identical(sr_ph_resolve(paste("the", l, "group"), reg), "the 65+ group")
})

test_that("sr_ph_verify re-derives effects, fragments and labels from the fixture project", {
  p <- .fx_writer_project()
  res <- .fx_writer_result(p, "a_lr")
  i <- which(res$effects$role == "primary")[1L]
  reg <- sr_ph_registry()
  src <- function(field) sprintf("effect:a_lr:%d:%s", i, field)
  sr_ph_put(reg, sub(" \\(.*$", "", sr_fmt_ci(res$effects$est[i], res$effects$lo[i], res$effects$hi[i],
                                              res$effects$mcse[i])), "est", src("est"), analysis = "a_lr")
  ci <- sr_fmt_ci(res$effects$est[i], res$effects$lo[i], res$effects$hi[i], res$effects$mcse[i])
  sr_ph_put(reg, sub("\\)$", "", sub("^[^(]*\\(", "", ci)), "ci", src("lo_hi"), analysis = "a_lr")
  sr_ph_put(reg, sr_fmt_p(res$effects$p[i]), "p", src("p"), analysis = "a_lr")
  sr_ph_put(reg, sprintf("%d", as.integer(res$effects$n[i])), "count", src("n"), analysis = "a_lr")
  sr_ph_put(reg, sprintf("%d", as.integer(res$effects$events[i])), "count", src("events"), analysis = "a_lr")
  sr_ph_put(reg, "95%", "label", "label:95%")
  sr_ph_put(reg, "grp", "label", "label:grp")
  note <- sr_ph_lift(res$fragments$glm_note, reg, "fragment:a_lr:glm_note", "a_lr")
  expect_identical(sr_ph_resolve(note, reg), res$fragments$glm_note)
  v <- sr_ph_verify(reg, p)
  expect_identical(names(v), c("id", "source", "expected", "got", "ok"))
  expect_identical(nrow(v), nrow(sr_ph_table(reg)))
  expect_true(all(v$ok), info = paste(v$got[!v$ok], collapse = "; "))
  # the est text is the part before the bracket and the interval the part inside it
  expect_false(grepl("[()]", v$got[v$source == src("est")]))
  expect_match(v$got[v$source == src("lo_hi")], " to ")
  # tamper one row: that row alone fails
  reg$rows$text[reg$rows$id == "E2"] <- "9.99"
  v2 <- sr_ph_verify(reg, p)
  expect_identical(v2$id[!v2$ok], "E2")
  expect_identical(v2$got[v2$id == "E2"], v$got[v$id == "E2"])
})

test_that("sr_ph_verify turns a resolver error into ok = FALSE and passes a suppressed small count", {
  p <- .fx_writer_project()
  reg <- sr_ph_registry()
  sr_ph_put(reg, "1.00", "est", "effect:no_such_analysis:1:est")
  sr_ph_put(reg, "1.00", "est", "effect:a_lr:99:est")
  sr_ph_put(reg, "1.00", "est", "effect:a_lr:1:bogus")
  sr_ph_put(reg, "95", "num", "effect:a_lr:1:level")   # the CI level is a label, not an effect field
  sr_ph_put(reg, "1", "num", "fragment:a_lr:glm_note:99")
  sr_ph_put(reg, "1", "num", "mystery:thing")
  sr_ph_put(reg, "1", "num", "nocolon")
  v <- expect_no_error(sr_ph_verify(reg, p))
  expect_false(any(v$ok))
  expect_true(all(nzchar(v$got)))
  expect_match(v$got[v$source == "effect:a_lr:1:level"], "unknown effect field")
  expect_match(v$got[1L], "no_such_analysis")
  # stage III is on exactly 3 rows: a count of 3 is a small cell and "<5" passes; a mask of 12 does not
  fx <- .fx_writer_frame()
  expect_identical(sum(fx$stage == "III"), 3L)
  reg2 <- sr_ph_registry()
  sr_ph_put(reg2, "<5", "count", "fragment:a_lr:glm_note:2", suppressed = TRUE)   # 209 events: not small
  sr_ph_put(reg2, "<5", "count", "label:3", suppressed = TRUE)
  sr_ph_put(reg2, "<5", "count", "label:12", suppressed = TRUE)
  sr_ph_put(reg2, "<5", "count", "label:0", suppressed = TRUE)
  v2 <- sr_ph_verify(reg2, p)
  expect_identical(v2$ok, c(FALSE, TRUE, FALSE, FALSE))
})

test_that("the registry is stable: the same puts in the same order give the same table", {
  build <- function() {
    reg <- sr_ph_registry()
    sr_ph_put(reg, "1.7", "est", "effect:a:1:est", analysis = "a", value = 1.7, scale = "ratio", null = 1)
    sr_ph_put(reg, "x", "label", "label:x")
    sr_ph_lift("n = 10 rows, 95% CI", reg, "fragment:a:f", "a")
    reg
  }
  expect_identical(sr_ph_table(build()), sr_ph_table(build()))
})

test_that("later tasks can register a resolver and verify uses it", {
  p <- .fx_writer_project()
  old <- if (exists("unit", envir = .sr_ph_resolvers, inherits = FALSE)) .sr_ph_resolvers$unit else NULL
  withr::defer(if (is.null(old)) rm("unit", envir = .sr_ph_resolvers) else .sr_ph_register_resolver("unit", old))
  .sr_ph_register_resolver("unit", function(project, parts, kind) paste(parts, collapse = "-"))
  reg <- sr_ph_registry()
  sr_ph_put(reg, "a-b", "text", "unit:a:b")
  expect_true(sr_ph_verify(reg, p)$ok)
})
