# The real app (app/app.R) under testServer: the Data, Design and SAP modules share one project(), so a
# change made through the Data module must reach the SAP lock checklist, and the slow checklist must not be
# computed while no SAP output is being read.

skip_on_cran()

.aw_app <- function() {
  old <- setwd(file.path(normalizePath(testthat::test_path("../.."), winslash = "/"), "app"))
  on.exit(setwd(old))
  shiny::shinyAppDir(".")
}

.aw_blocks <- function(html) {
  s <- paste(html, collapse = " ")
  m <- regmatches(s, regexpr("Lock SAP [(][0-9]+ blocks[)]", s))
  if (length(m)) m else NA_character_
}


test_that("a DQ run and decisions made in the Data tab reach the SAP lock checklist", {
  withr::local_options(sr.mode = "airgap", sr.ingest_sync = TRUE)
  .fx_writer_routes()
  td <- withr::local_tempdir()
  csv <- file.path(td, "t.csv")
  .fx_write_csv(.fx_writer_frame(), csv)
  pdir <- file.path(td, "proj")

  shiny::testServer(.aw_app(), {
    session$setInputs(`project-dir` = pdir, `project-name` = "probe")
    session$setInputs(`project-create` = 1)
    session$setInputs(`data-path` = csv, `data-tname` = "t")
    session$setInputs(`data-import` = 1)
    session$setInputs(`data-table` = "t")
    project(sr_roles_set(project(), "t", c(id = "id", y = "outcome", grp = "exposure", age = "covariate",
                                            z1 = "covariate", time = "time", status = "event")))
    project(sr_sap_add(project(), "a1", "logistic", .ui_bin_spec(), "t", label = "primary"))
    project(.fx_routed(project()))
    session$flushReact()

    n0 <- sum(sr_sap_check(project())$status == "block")
    expect_gt(n0, 0L)
    expect_identical(.aw_blocks(output$`sap-lock_btn`$html), sprintf("Lock SAP (%d blocks)", n0))
    expect_match(paste(output$`sap-check`, collapse = " "), "no DQ run recorded", fixed = TRUE)

    session$setInputs(`data-dq_run` = 1)
    session$flushReact()

    n1 <- sum(sr_sap_check(project())$status == "block")
    expect_lt(n1, n0)
    expect_identical(.aw_blocks(output$`sap-lock_btn`$html), sprintf("Lock SAP (%d blocks)", n1))
    expect_false(grepl("no DQ run recorded", paste(output$`sap-check`, collapse = " "), fixed = TRUE))
    try(sr_project_close(project()), silent = TRUE)
  })
})

# testServer evaluates every output on each flush (it has no hidden tabs), so "no computation while the SAP tab
# is hidden" cannot be driven here. This pins the mechanism instead: the checklist is a lazy reactive, not an
# observe-based flag (which would run sr_sap_check on every project change made in any tab).
test_that("the lock checklist is a lazy reactive, not an observer-driven flag", {
  src <- paste(readLines(file.path(normalizePath(testthat::test_path("../.."), winslash = "/"),
                                   "app", "R", "ui", "mod_sap.R"), warn = FALSE), collapse = "
")
  expect_match(src, "chk <- shiny::reactive(.sr_sap_check_view(project()))", fixed = TRUE)
  expect_false(grepl("chk <- .sr_sap_flag", src, fixed = TRUE))
})
