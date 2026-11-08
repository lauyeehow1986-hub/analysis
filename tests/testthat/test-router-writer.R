# Tests for the router part of the writer: the "Study design" Methods paragraph, the analysis-decisions
# supplementary table and STROBE items 3-5 (writer/fragments.R, model.R, checklist.R). Synthetic data only.

.rw_para <- function(blocks, id) {
  hit <- Filter(function(b) identical(b$type, "para") && identical(b$id, id), blocks)
  if (length(hit)) hit[[1L]] else NULL
}

.rw_say <- function(m, b) sr_ph_resolve(b$text, m$reg)

.rw_setting <- list(pico = list(setting = "two hospitals in Singapore", period = "2010 to 2020 & <5 y follow-up"))

# The decision table of the supplement with its tokens resolved, as a data.frame.
.rw_decisions <- function(m) {
  tb <- Filter(function(b) identical(b$type, "table") && identical(b$id, "analysis_decisions"), m$supplement)
  expect_length(tb, 1L)
  df <- tb[[1L]]$df
  for (j in seq_along(df)) df[[j]] <- vapply(df[[j]], function(x) sr_ph_resolve(x, m$reg), "", USE.NAMES = FALSE)
  df
}

test_that("the Methods open with the study design", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  expect_identical(m$main[[1L]]$text, "Methods")
  expect_identical(m$main[[2L]]$text, "Study design")
  expect_identical(m$main[[3L]]$id, "methods_design")
  expect_identical(m$main[[4L]]$text, "Statistical analysis")
  txt <- .rw_say(m, m$main[[3L]])
  expect_match(txt, "retrospective cohort", fixed = TRUE)
  expect_match(txt, "STROBE", fixed = TRUE)
  expect_match(txt, "decision log", fixed = TRUE)
  expect_false(grepl("[0-9]", gsub("[{][{][^}]*[}][}]", "", m$main[[3L]]$text)))
  expect_identical(nrow(sr_claim_guard(m)[sr_claim_guard(m)$severity == "block", ]), 0L)
})

test_that("PICO text with digits and markup passes the guard and the docx", {
  p <- do.call(.fx_writer_project, c(list(env = environment()), .rw_setting))
  m <- sr_doc_model(p)
  g <- sr_claim_guard(m)
  expect_identical(sum(g$severity == "block"), 0L)
  txt <- .rw_say(m, .rw_para(m$main, "methods_design"))
  expect_match(txt, "Setting: two hospitals in Singapore, 2010 to 2020 & <5 y follow-up.", fixed = TRUE)
  f <- sr_docx_render(m, "main", tempfile(fileext = ".docx"), tempfile("fig-"),
                      list(audit_tip = "a1b2c3d4e5f60718293a4b5c6d7e8f90", sap_hash = "f5bdde8442476060aabbccddeeff0011",
                           export_id = "export-001", created = "2026-10-18T09:30:00Z"))
  s <- officer::docx_summary(officer::read_docx(f), remove_fields = TRUE)
  expect_true(any(grepl("2010 to 2020 & <5 y follow-up", s$text, fixed = TRUE)))
})

test_that("the decision table lists overrides without probabilities", {
  # a user decision with override = TRUE, made through the router before the lock
  p <- .fx_writer_project(environment(), decide = list(list(
    node = "exposure_col", choice = "grp", table = "t", override = TRUE,
    reason = "clinician review 2 of 3")))
  m <- sr_doc_model(p)
  df <- .rw_decisions(m)
  expect_identical(names(df), c("Decision", "Choice", "Decided by", "Override", "Reason"))
  hit <- df[df$Override == "yes", , drop = FALSE]
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$Reason, "clinician review 2 of 3")
  expect_identical(hit[["Decided by"]], "user")
  expect_match(hit$Decision, "(t)", fixed = TRUE)
  expect_true(all(df$`Decided by` %in% c("rule", "user")))
  expect_false(any(grepl("^0[.][0-9]+$", unlist(df))))
  expect_identical(sum(sr_claim_guard(m)$severity == "block"), 0L)
})


test_that("STROBE items 3-5 turn green", {
  p <- do.call(.fx_writer_project, c(list(env = environment()), .rw_setting))
  ck <- Filter(function(b) identical(b$type, "checklist"), sr_doc_model(p)$supplement)[[1L]]$df
  expect_identical(ck$status[ck$item %in% c("3", "4", "5")], rep("green", 3L))
  expect_match(ck$detail[ck$item == "4"], "retrospective cohort", fixed = TRUE)
  p0 <- .fx_writer_project(environment())
  ck0 <- Filter(function(b) identical(b$type, "checklist"), sr_doc_model(p0)$supplement)[[1L]]$df
  expect_identical(ck0$status[ck0$item == "3"], "green")
  expect_identical(ck0$status[ck0$item == "4"], "green")
  expect_identical(ck0$status[ck0$item == "5"], "amber")
  expect_match(ck0$detail[ck0$item == "5"], "add the setting and dates to the PICO", fixed = TRUE)
})

test_that("a CONSORT design swaps the checklist for a note", {
  p <- .fx_writer_project(environment(), design = "rct")
  expect_identical(sr_router_choice(p, "guideline"), "consort")
  m <- sr_doc_model(p)
  expect_length(Filter(function(b) identical(b$type, "checklist"), m$supplement), 0L)
  note <- Filter(function(b) identical(b$type, "para") && grepl("CONSORT checklist", b$text, fixed = TRUE), m$supplement)
  expect_length(note, 1L)
  expect_match(.rw_say(m, .rw_para(m$main, "methods_design")), "CONSORT", fixed = TRUE)
  expect_match(.rw_say(m, .rw_para(m$main, "methods_design")), "randomised controlled trial", fixed = TRUE)
  expect_identical(sum(sr_claim_guard(m)$severity == "block"), 0L)
})

test_that("an undecided design leaves the Methods without the design paragraph", {
  p <- .fx_writer_project(environment())
  # the project is locked, so the router reads the snapshot of the lock: drop the design there too
  p$state$router$decisions[[sr_router_key("design")]] <- NULL
  expect_false(is.null(.sr_wr_m_design(p, sr_ph_registry())))
  p$state$sap$lock$router$decisions[[sr_router_key("design")]] <- NULL
  expect_null(.sr_wr_m_design(p, sr_ph_registry()))
})

test_that("the design text covers every design and the objective follows the purpose", {
  expect_identical(names(SR_WR_DESIGN_TEXT), c("cohort_prospective", "cohort_retrospective", "case_control",
                                               "cross_sectional", "rct", "other"))
  expect_identical(unname(SR_WR_DESIGN_TEXT[["rct"]]), "randomised controlled trial")
  expect_identical(unname(SR_WR_DESIGN_TEXT[["other"]]), "observational")
  # without an exposure the objective sentence is left out
  p <- .fx_writer_project(environment())
  reg <- sr_ph_registry()
  raw <- .sr_wr_m_design(p, reg)
  expect_false(grepl("objective", sr_ph_resolve(raw, reg), fixed = TRUE))
  p <- .fx_writer_project(environment(), pico = list(exposure = "drug A", comparator = "drug B"))
  reg <- sr_ph_registry()
  raw <- .sr_wr_m_design(p, reg)
  expect_match(sr_ph_resolve(raw, reg),
               "The objective was to estimate the effect of drug A compared with drug B on test outcome in test population.",
               fixed = TRUE)
})
test_that("the objective item has no red state", {
  p <- .fx_writer_project(environment())
  p$state$pico$population <- NA_character_
  m <- sr_doc_model(p)
  ck <- Filter(function(b) identical(b$type, "checklist"), m$supplement)[[1L]]$df
  expect_identical(ck$status[ck$item == "3"], "amber")
  expect_match(ck$detail[ck$item == "3"], "set the PICO population and outcome", fixed = TRUE)
})

test_that("the design sentence takes the article of its first letter", {
  expect_identical(.sr_wr_article("observational"), "an")
  expect_identical(.sr_wr_article("retrospective cohort"), "a")
  expect_identical(.sr_wr_article("Observational"), "an")
  expect_identical(.sr_wr_article("case-control"), "a")
  p <- .fx_writer_project(environment())
  p$state$sap$lock$router$decisions$design$choice <- "other"
  # the paragraph is built before sr_ph_resolve reads the registry (its argument is lazy)
  say <- function(p) {
    reg <- sr_ph_registry()
    para <- .sr_wr_m_design(p, reg)
    sr_ph_resolve(para, reg)
  }
  expect_match(say(p), "^This was an observational study[.]")
  p$state$sap$lock$router$decisions$design$choice <- "cohort_retrospective"
  expect_match(say(p), "^This was a retrospective cohort study[.]")
})

test_that("the software sentence names a reporting guideline only for STROBE or CONSORT", {
  say <- function(g) .sr_wr_m_software(sr_ph_registry(), character(), g)
  expect_match(say("strobe"), "Reporting follows the STROBE statement", fixed = TRUE)
  expect_match(say("consort"), "Reporting follows the CONSORT statement", fixed = TRUE)
  for (g in list("none", NA_character_, "other")) {
    txt <- say(g)
    expect_false(grepl("Reporting follows", txt, fixed = TRUE), info = g)
    expect_false(grepl("checklist", txt, fixed = TRUE), info = g)
    expect_match(txt, "the exact code is supplied as analysis.R.$", info = g)
  }
})
