# Tests for writer/checklist.R: the STROBE checklist scored from the artifacts of the document model.
# Synthetic data only (the writer fixture of helper-writer.R); variants use the uncached builder.

.ck_row <- function(df, item) df[df$item == item, , drop = FALSE]
.ck_status <- function(df, item) .ck_row(df, item)$status

test_that("strobe.csv has the 34 items, every rule is known and no label is long", {
  tpl <- .sr_ck_template()
  expect_identical(names(tpl), c("item", "section", "label", "rule"))
  expect_identical(nrow(tpl), 34L)
  expect_false(anyDuplicated(tpl$item) > 0L)
  expect_true(all(tpl$rule %in% SR_CK_RULES))
  expect_true(all(SR_CK_RULES %in% tpl$rule))
  words <- lengths(strsplit(trimws(tpl$label), "[[:space:]]+"))
  expect_true(all(words >= 1L & words <= 10L), info = paste(tpl$item[words > 10L], collapse = ", "))
  # the rules of the items, as the plan lists them
  expect_identical(tpl$item[tpl$rule == "author"],
                   c("1a", "1b", "2", "6b", "8", "9", "12d", "16b", "18", "19", "20", "21", "22"))
  expect_identical(tpl$item[tpl$rule == "flow"], c("6a", "13a", "13b", "13c"))
  expect_identical(tpl$item[tpl$rule == "sensitivity"], c("12e", "17"))
  expect_true(all(vapply(tpl$label, function(x) !grepl("[^ -~]", x), NA)))
})

test_that("the checklist of the fixture is scored from its artifacts", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  blk <- Filter(function(b) identical(b$type, "checklist"), m$supplement)
  expect_length(blk, 1L)
  expect_identical(blk[[1L]]$id, "strobe")
  ck <- blk[[1L]]$df
  expect_s3_class(ck, "data.frame")
  expect_identical(names(ck), c("item", "section", "label", "status", "detail"))
  expect_identical(nrow(ck), 34L)
  expect_true(all(ck$status %in% c("green", "amber", "red")))
  expect_true(all(nzchar(ck$detail)))
  for (it in c("14a", "14b", "16a", "16c", "12e", "6a", "13a", "13b", "13c", "7", "10", "11", "12a", "12c", "15", "17")) {
    expect_identical(.ck_status(ck, it), "green", info = it)
  }
  expect_identical(.ck_status(ck, "1a"), "amber")
  expect_match(.ck_row(ck, "1a")$detail, "for the authors to write", fixed = TRUE)
  # no interaction in the fixture, and a survival route is reported
  expect_identical(.ck_status(ck, "12b"), "amber")
  expect_match(.ck_row(ck, "12b")$detail, "no interaction or subgroup analysis was pre-specified; say so", fixed = TRUE)
  expect_identical(.ck_status(ck, "14c"), "amber")
  expect_match(.ck_row(ck, "14c")$detail, "report the follow-up time", fixed = TRUE)
  expect_match(.ck_row(ck, "12e")$detail, "a_mp", fixed = TRUE)
  expect_match(.ck_row(ck, "14a")$detail, "t1", fixed = TRUE)
  # sr_checklist_strobe on the model without its checklist block gives the same table
  m0 <- m
  m0$supplement <- Filter(function(b) !identical(b$type, "checklist"), m$supplement)
  expect_identical(sr_checklist_strobe(m0, p), ck)
})

test_that("the checklist block is last in the supplement and the STROBE reference is cited in the text", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  expect_identical(m$supplement[[length(m$supplement)]]$type, "checklist")
  sw <- Filter(function(b) identical(b$id, "methods_software"), m$main)[[1L]]
  expect_match(sw$text, "Reporting follows the STROBE statement {{C:vonelm2007}}; the checklist is in the supplementary material.",
               fixed = TRUE)
  expect_false(grepl("[0-9]", gsub("[{][{][^}]*[}][}]", "", sw$text)))
  refs <- m$main[[length(m$main)]]
  expect_identical(refs$type, "refs")
  expect_identical(sum(refs$keys == "vonelm2007"), 1L)
  # numbered by first use: the study-design paragraph opens the Methods and cites it first
  expect_identical(match("vonelm2007", refs$keys), 1L)
  expect_false(anyDuplicated(refs$keys) > 0L)
  g <- sr_claim_guard(m)
  expect_identical(nrow(g), 0L, info = paste(g$block, g$rule, g$detail, collapse = "\n"))
})

test_that("a copy without t1 has no Table 1 and no missing-data table", {
  p <- .fx_writer_build(environment(), drop = "t1")
  m <- sr_doc_model(p)
  ck <- Filter(function(b) identical(b$type, "checklist"), m$supplement)[[1L]]$df
  expect_identical(.ck_status(ck, "14a"), "red")
  expect_identical(.ck_status(ck, "14b"), "red")
  expect_identical(.ck_status(ck, "12c"), "red")
  expect_identical(.ck_status(ck, "16a"), "green")
})

test_that("a secondary analysis that never ran downgrades item 17 and names it", {
  p <- .fx_writer_build(environment(), unrun = "a_rmst")
  m <- sr_doc_model(p)
  expect_identical(m$not_reported$id, "a_rmst")
  ck <- Filter(function(b) identical(b$type, "checklist"), m$supplement)[[1L]]$df
  expect_identical(.ck_status(ck, "17"), "amber")
  expect_match(.ck_row(ck, "17")$detail, "; not reported: a_rmst", fixed = TRUE)
  expect_identical(.ck_status(ck, "12e"), "amber")
  expect_identical(.ck_status(ck, "15"), "amber")
  expect_match(.ck_row(ck, "15")$detail, "not reported: a_rmst", fixed = TRUE)
  # an item the analysis has nothing to do with is untouched
  expect_identical(.ck_status(ck, "14a"), "green")
  expect_false(grepl("not reported", .ck_row(ck, "14a")$detail, fixed = TRUE))
})

test_that("a red item stays red when an analysis is not reported", {
  p <- .fx_writer_build(environment(), drop = "t1", unrun = "a_rmst")
  m <- sr_doc_model(p)
  ck <- Filter(function(b) identical(b$type, "checklist"), m$supplement)[[1L]]$df
  expect_identical(.ck_status(ck, "14a"), "red")
  expect_identical(.ck_status(ck, "17"), "amber")
})

test_that("the rules read the model: no flow figure, no sensitivity analysis, an interaction", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  m0 <- m
  m0$supplement <- Filter(function(b) !identical(b$type, "checklist"), m$supplement)
  no_flow <- m0
  no_flow$main <- Filter(function(b) !(identical(b$type, "figure") && identical(b$kind, "flow")), m0$main)
  ck <- sr_checklist_strobe(no_flow, p)
  expect_identical(.ck_status(ck, "13a"), "amber")
  expect_identical(.ck_status(ck, "6a"), "amber")
  # a model whose analysis has no Methods paragraph
  no_methods <- m0
  no_methods$main <- Filter(function(b) !identical(b$id, "methods_a_cox"), m0$main)
  ck2 <- sr_checklist_strobe(no_methods, p)
  expect_identical(.ck_status(ck2, "12a"), "red")
  expect_match(.ck_row(ck2, "12a")$detail, "a_cox", fixed = TRUE)
  # an interaction in a reported spec
  p2 <- p
  p2$state$analyses$a_lr$spec$interactions <- list(c("grp", "age"))
  ck3 <- sr_checklist_strobe(m0, p2)
  expect_identical(.ck_status(ck3, "12b"), "green")
  # no sensitivity analysis reported
  p3 <- p
  p3$state$analyses$a_mp$label <- "secondary"
  ck4 <- sr_checklist_strobe(m0, p3)
  expect_identical(.ck_status(ck4, "12e"), "amber")
})

test_that("a bad model or project is refused", {
  p <- .fx_writer_project(environment())
  expect_error(sr_checklist_strobe(list(), p), "document model")
  expect_error(sr_checklist_strobe(sr_doc_model(p), "x"), "project")
})

test_that("item 15 (events) is green only when a table prints the counts; otherwise amber and says so", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  ck <- sr_checklist_strobe(m, p)
  expect_identical(.ck_status(ck, "15"), "green")
  expect_match(.ck_row(ck, "15")$detail, "event counts are printed in table", fixed = TRUE)
  strip <- function(m) {
    drop <- function(bl) Filter(function(b) !(identical(b$type, "table") && grepl("_(model_fit|observed_risk)$", as.character(b$id))), bl)
    m$main <- drop(m$main)
    m$supplement <- drop(m$supplement)
    m
  }
  ck2 <- sr_checklist_strobe(strip(m), p)
  expect_identical(.ck_status(ck2, "15"), "amber")
  expect_match(.ck_row(ck2, "15")$detail, "but no table prints them", fixed = TRUE)
})
