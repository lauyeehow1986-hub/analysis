# Tests for router/shadow.R. Synthetic toy nodes only.

# These tests register toy nodes: set the real P1 nodes aside for this file.
.fx_isolate_nodes()

.rs_project <- function(env = parent.frame()) {
  p <- .fx_project(env = env)
  p$state$roles <- list(t = c(a = "outcome"))
  p
}

# A project with an ingested table t (the column-scope node's request needs its manifest).
.rs_table_project <- function(env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  csv <- file.path(dirname(p$paths$dir), "t.csv")
  .fx_write_csv(data.frame(id = 1:4, age = c(61.5, 72.25, 48.75, 55.5)), csv)
  p <- sr_ingest(p, csv, table = "t",
                 options = list(drive_info = function(path) list(free_bytes = 1e13, remote = FALSE,
                                                                 warnings = character())))
  sr_roles_set(p, "t", c(id = "id", age = "covariate"))
}

# A column-scope toy node with a question: four instances (c1..c4) on table t.
.rs_col_node <- function(env = parent.frame()) {
  nd <- sr_node("toy_col", "Toy column", c("a1", "a2"), scope = "column",
                columns = function(ctx) c("c1", "c2", "c3", "c4"),
                question = function(ctx) list(instructions = "Pick the better of the two toy options.",
                                              criteria = list(a1 = "first", a2 = "second")))
  sr_node_register(nd)
  withr::defer(.sr_node_unregister("toy_col"), envir = env)
  invisible("toy_col")
}

test_that("agreement is joined to the final decision", {
  .fx_toy_nodes()
  env <- environment()
  .fx_fake_backend(env, list(choice = "a2", probabilities = c(a1 = 0.03, a2 = 0.97)))
  p <- sr_router_run(.rs_project(), backends = "jev")
  sh <- sr_router_shadow(p)
  expect_identical(nrow(sh), 1L)
  expect_identical(names(sh), c("key", "node", "table", "column", "backend", "model", "model_choice",
                                "confidence", "would_act", "error", "final_choice", "resolver",
                                "override", "agree", "latency_ms"))
  expect_true(is.na(sh$final_choice))
  expect_true(is.na(sh$agree))
  p <- sr_router_decide(p, "toy_a", "a2")
  sh <- sr_router_shadow(p)
  expect_identical(sh$final_choice, "a2")
  expect_identical(sh$resolver, "user")
  expect_true(sh$agree)
  expect_false(sh$override)
  p <- sr_router_decide(p, "toy_a", "a1", reason = "clinical judgement", override = TRUE)
  sh <- sr_router_shadow(p)
  expect_identical(sh$final_choice, "a1")
  expect_false(sh$agree)
  expect_true(sh$override)
  expect_identical(sh$model_choice, "a2")
})

test_that("the summary rates", {
  .rs_col_node()
  env <- environment()
  .fx_fake_backend(env, list(choice = "a2", probabilities = c(a1 = 0.03, a2 = 0.97)))
  p <- sr_router_run(.rs_table_project(), backends = "jev")
  expect_identical(env$.fake_calls, 4L)
  # c4's latest prediction is a failure.
  ev <- Filter(function(e) identical(e$type, "router.predicted") && identical(e$payload$column, "c4"),
               sr_events_read(p$paths$events))
  fail <- ev[[1]]$payload
  fail$choice <- NULL
  fail$probabilities <- list()
  fail$confidence <- NA
  fail$would_act <- FALSE
  fail$error <- "planted failure"
  p <- sr_project_append(p, "router.predicted", fail)
  p <- sr_router_decide(p, "toy_col", "a2", table = "t", column = "c1")
  p <- sr_router_decide(p, "toy_col", "a2", table = "t", column = "c2")
  p <- sr_router_decide(p, "toy_col", "a1", reason = "disagree", table = "t", column = "c3",
                        override = TRUE)
  p <- sr_router_decide(p, "toy_col", "a1", table = "t", column = "c4")
  sm <- sr_router_shadow_summary(p)
  expect_identical(nrow(sm), 1L)
  expect_identical(names(sm), c("node", "backend", "n", "decided", "agree_rate", "would_act_rate",
                                "act_accuracy", "error_rate"))
  expect_identical(sm$node, "toy_col")
  expect_identical(sm$backend, "jev")
  expect_identical(sm$n, 4L)
  expect_identical(sm$decided, 4L)
  expect_equal(sm$agree_rate, 2 / 3)
  expect_equal(sm$error_rate, 1 / 4)
  expect_equal(sm$would_act_rate, 3 / 4)
  expect_equal(sm$act_accuracy, 2 / 3)
})

test_that("export writes one JSON line per prediction and never overwrites", {
  .fx_toy_nodes()
  env <- environment()
  .fx_fake_backend(env, list(choice = "a2", probabilities = c(a1 = 0.03, a2 = 0.97)))
  key <- .fx_connected()
  p <- sr_router_run(.rs_project(), backends = "jev")
  p <- sr_router_run(p, backends = "jev")
  p <- sr_router_decide(p, "toy_a", "a2")
  path <- file.path(withr::local_tempdir(), "golden.jsonl")
  expect_identical(sr_router_shadow_export(p, path), path)
  lines <- readLines(path, warn = FALSE)
  expect_length(lines, nrow(sr_router_shadow(p)))
  rec <- .sr_parse_json(lines[[1]])
  expect_identical(rec$key, "toy_a")
  expect_identical(rec$final_choice, "a2")
  expect_match(rec$payload, "questions", fixed = TRUE)
  expect_false(is.null(rec$raw))
  expect_identical(rec$payload_sha, .sr_sha256(rec$payload))
  expect_false(is.null(rec$input_hash))
  expect_false(grepl(key, paste(lines, collapse = "\n"), fixed = TRUE))
  expect_error(sr_router_shadow_export(p, path), "exists")
})

test_that("an empty project gives empty tables and an empty export", {
  .fx_toy_nodes()
  p <- .rs_project()
  expect_identical(nrow(sr_router_shadow(p)), 0L)
  expect_identical(nrow(sr_router_shadow_summary(p)), 0L)
  path <- file.path(withr::local_tempdir(), "empty.jsonl")
  sr_router_shadow_export(p, path)
  expect_identical(length(readLines(path, warn = FALSE)), 0L)
})
