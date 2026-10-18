# Tests for writer/sdc_out.R: small-cell suppression of text tables, with row and block secondary
# suppression, and the KM number-at-risk labels. Synthetic cells only.

# One level row under a header row: a block of a single row.
.sdc_row <- function(overall, a, b) {
  data.frame(Variable = c("Stage", "    III"), Overall = c("", overall), A = c("", a), B = c("", b),
             Missing = "", stringsAsFactors = FALSE)
}

test_that(".sr_sdc_count reads a count cell and nothing else", {
  expect_identical(.sr_sdc_count(c("3", "12 (4.5%)", "12.3 (4.5)", "12 [10 to 14]", "0", "x")),
                   c(3L, 12L, NA, NA, 0L, NA))
  expect_identical(.sr_sdc_count(c(NA, "", "<5")), rep(NA_integer_, 3L))
})

test_that("sr_sdc_value suppresses 1..(k-1) and shows zero", {
  v4 <- sr_sdc_value(4)
  expect_identical(v4$text, "<5")
  expect_true(v4$suppressed)
  v0 <- sr_sdc_value(0)
  expect_identical(v0$text, "0")
  expect_false(v0$suppressed)
  v5 <- sr_sdc_value(5)
  expect_identical(v5$text, "5")
  expect_false(v5$suppressed)
  expect_identical(sr_sdc_value(9, k = 10L)$text, "<10")
})

test_that("row rule: one hidden group cell hides the smallest other non-zero one, never Overall", {
  # A is small and B is zero: B is not a candidate, Overall keeps its value
  r <- sr_sdc_table(.sdc_row("10 (5.0%)", "3 (3.0%)", "0 (0.0%)"))
  expect_identical(r$table$A[2], "<5")
  expect_identical(r$table$B[2], "0 (0.0%)")
  expect_identical(r$table$Overall[2], "10 (5.0%)")
  expect_identical(r$n_suppressed, 1L)
  # the same row with a non-zero B: B is hidden as well, Overall still shows
  r2 <- sr_sdc_table(.sdc_row("10 (5.0%)", "3 (3.0%)", "7 (7.0%)"))
  expect_identical(r2$table$A[2], "<5")
  expect_identical(r2$table$B[2], "<5")
  expect_identical(r2$table$Overall[2], "10 (5.0%)")
  expect_identical(r2$n_suppressed, 2L)
  # the header row and the non-count Missing column are untouched
  expect_identical(r2$table$A[1], "")
  expect_identical(r2$table$Missing, c("", ""))
})

test_that("a small Overall count is itself a primary cell", {
  r <- sr_sdc_table(.sdc_row("3 (1.5%)", "3 (3.0%)", "0 (0.0%)"))
  expect_identical(r$table$Overall[2], "<5")
  expect_identical(r$table$A[2], "<5")
})

test_that("block rule: one hidden cell in a column hides the smallest other non-zero cell", {
  d <- data.frame(Variable = c("Stage", "    I", "    II", "    III"),
                  Overall = c("", "100 (50.0%)", "97 (48.5%)", "10 (5.0%)"),
                  A = c("", "50 (50.0%)", "47 (47.0%)", "3 (3.0%)"), stringsAsFactors = FALSE)
  r <- sr_sdc_table(d)
  # A holds 50 / 47 / <5: the 47 is the smallest other cell of the block, so it is hidden too
  expect_identical(r$table$A, c("", "50 (50.0%)", "<5", "<5"))
  expect_identical(r$table$Overall, d$Overall)
  expect_identical(r$table$Variable, d$Variable)
  expect_identical(r$n_suppressed, 2L)
  # a column outside the indented rows is not a block
  d2 <- d
  d2$Variable <- c("Stage", "I", "II", "III")
  expect_identical(sr_sdc_table(d2)$table$A, c("", "50 (50.0%)", "47 (47.0%)", "<5"))
})

test_that("the secondary rules reach a fixed point: no row or block keeps exactly one hidden cell", {
  d <- data.frame(Variable = c("Grade", "    a", "    b", "    c", "    d"),
                  Overall = c("", "60 (30.0%)", "70 (35.0%)", "62 (31.0%)", "8 (4.0%)"),
                  A = c("", "30 (30.0%)", "40 (40.0%)", "27 (27.0%)", "3 (3.0%)"),
                  B = c("", "30 (30.0%)", "30 (30.0%)", "35 (35.0%)", "4 (4.0%)"),
                  stringsAsFactors = FALSE)
  res <- sr_sdc_table(d)
  r <- res$table
  hid <- function(x) x == "<5"
  expect_true(all(rowSums(hid(as.matrix(r[, c("A", "B")]))) != 1L))
  expect_true(all(colSums(hid(as.matrix(r[2:5, c("Overall", "A", "B")]))) != 1L))
  # the primary cells are hidden, the Overall cell is not
  expect_identical(r$A[5], "<5")
  expect_identical(r$B[5], "<5")
  expect_identical(r$Overall, d$Overall)
  expect_identical(res$n_suppressed, sum(hid(as.matrix(r))))
  # the cascade is real: a one-cell primary hit spread to the other rows
  expect_gt(res$n_suppressed, 2L)
})

test_that("the row and block rules feed each other until nothing changes", {
  # A[c] is small. Row c hides B[c]; the block rule then hides A[a] and B[b]; those two force
  # B[a] and A[b] through the row rule: every cell ends hidden.
  d <- data.frame(Variable = c("    a", "    b", "    c"),
                  A = c("30 (30.0%)", "40 (40.0%)", "3 (3.0%)"),
                  B = c("30 (30.0%)", "9 (9.0%)", "6 (6.0%)"), stringsAsFactors = FALSE)
  r <- sr_sdc_table(d)
  expect_identical(r$table$A, rep("<5", 3L))
  expect_identical(r$table$B, rep("<5", 3L))
  expect_identical(r$n_suppressed, 6L)
})

test_that("tables of other shapes: counts, skip_cols, k and no count cells", {
  d <- data.frame(Group = c("A", "B", "C"), n = c("3", "12", "0"), events = c("2", "40", "1"),
                  stringsAsFactors = FALSE)
  r <- sr_sdc_table(d)
  expect_identical(r$table$n, c("<5", "12", "0"))
  expect_identical(r$table$events, c("<5", "40", "<5"))
  expect_identical(r$n_suppressed, 3L)
  # skip_cols keeps a column out of the row rule only: it is still a primary column
  d2 <- data.frame(Group = c("A", "B"), n = c("3", "12"), events = c("20", "40"), stringsAsFactors = FALSE)
  expect_identical(sr_sdc_table(d2)$table$events, c("<5", "40"))
  r2 <- sr_sdc_table(d2, skip_cols = "events")
  expect_identical(r2$table$n, c("<5", "12"))
  expect_identical(r2$table$events, c("20", "40"))
  d3 <- data.frame(Group = "A", n = "2", events = "1", stringsAsFactors = FALSE)
  expect_identical(sr_sdc_table(d3, skip_cols = "events")$table$events, "<5")
  # a coefficient table has no count cell and is returned unchanged
  cf <- data.frame(Term = c("age", "sex"), `OR (95% CI)` = c("1.02 (0.99 to 1.05)", "0.80 (0.5 to 1.2)"),
                   check.names = FALSE, stringsAsFactors = FALSE)
  rc <- sr_sdc_table(cf)
  expect_identical(rc$table, cf)
  expect_identical(rc$n_suppressed, 0L)
  # k changes the threshold and the text
  r3 <- sr_sdc_table(d, k = 3L)
  # row A: events 2 is hidden, so the row rule also hides n = 3
  expect_identical(r3$table$n, c("<3", "12", "0"))
  expect_identical(r3$table$events, c("<3", "40", "<3"))
})

test_that("sr_sdc_km_risk labels n.risk and leaves the rest of the frame alone", {
  km_risk <- data.frame(strata = c("a", "a", "b", "b"), time = c(0, 10, 0, 10),
                        n.risk = c(60, 4, 58, 0))
  r <- sr_sdc_km_risk(km_risk)
  expect_identical(r$label, c("60", "<5", "58", "0"))
  expect_identical(r$n.risk, km_risk$n.risk)
  expect_identical(names(r), c(names(km_risk), "label"))
})
