test_that("the actor refuses with no project, while busy, and on a read-only project", {
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(NULL); hit <- 0L
  act <- .sr_ui_actor(rv, busy = function() FALSE)
  act("Save", function(p) hit <<- hit + 1L)
  p <- .fx_project(); rv(p)
  .sr_ui_actor(rv, busy = function() TRUE)("Save", function(p) hit <<- hit + 1L)
  p$readonly <- TRUE; rv(p)
  act("Save", function(p) hit <<- hit + 1L)
  expect_equal(hit, 0L)
  expect_equal(seen$msg, c("Save: open a project first", "Save: wait for the import to finish",
                           "Save: the project is read-only (locked by another session)"))
  expect_equal(seen$type, rep("error", 3L))
})

test_that("the actor shows a core error verbatim and writes back a returned project", {
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_project())
  act <- .sr_ui_actor(rv)
  expect_null(act("Save", function(p) stop("boom")))
  expect_true(any(grepl("boom", seen$msg, fixed = TRUE)))
  res <- act("PICO", function(p) sr_pico_set(p, population = "a", outcome = "b"))
  expect_false(is.null(shiny::isolate(rv())$state$pico))
  expect_identical(res$paths$dir, shiny::isolate(rv())$paths$dir)
})

test_that("the banner names read-only before a locked SAP, and is NULL when editable", {
  expect_null(.sr_ui_readonly_banner(NULL))
  p <- .fx_project()
  expect_true(.sr_ui_editable(p))
  expect_null(.sr_ui_readonly_banner(p))
  expect_false(.sr_ui_editable(NULL))
  p$state$sap <- list(locked = TRUE, lock = list(locked_at = "2026-11-08T00:00:00Z"))
  expect_false(.sr_ui_editable(p))
  b <- as.character(.sr_ui_readonly_banner(p))
  expect_true(grepl("The SAP is locked (2026-11-08T00:00:00Z)", b, fixed = TRUE))
  expect_true(grepl("alert-warning", b, fixed = TRUE))
  p$readonly <- TRUE
  b <- as.character(.sr_ui_readonly_banner(p))
  expect_true(grepl("read-only", b, fixed = TRUE))
  expect_false(grepl("SAP is locked", b, fixed = TRUE))
})

test_that(".sr_ui_exports lists a kind's files as absolute paths", {
  p <- .fx_project()
  empty <- .sr_ui_exports(p, "sap")
  expect_equal(names(empty), c("id", "path"))
  expect_equal(nrow(empty), 0L)
  p$state$exports <- list(
    list(kind = "sap", id = "SAP_v1", files = list(list(path = "out/SAP_v1.docx"))),
    list(kind = "manuscript", id = "m1", files = list(list(path = "out/m1.docx"))),
    list(kind = "sap", id = "SAP_v2", files = list(list(path = "out/SAP_v2.docx"), list(path = "x"))))
  d <- .sr_ui_exports(p, "sap")
  expect_equal(d$id, c("SAP_v1", "SAP_v2"))
  expect_equal(d$path, file.path(p$paths$dir, c("out/SAP_v1.docx", "out/SAP_v2.docx")))
  expect_equal(nrow(.sr_ui_exports(p, "nothing")), 0L)
})

test_that("the fixtures build: a ready UI project accepts the binary spec", {
  p <- .fx_ui_project()
  p <- sr_sap_add(p, "a1", "logistic", .ui_bin_spec(), "t", label = "primary")
  expect_true("a1" %in% names(p$state$analyses))
})
