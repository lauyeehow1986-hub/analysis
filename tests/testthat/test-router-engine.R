# Tests for router/backends/rules.R and router/engine.R. Synthetic toy nodes only.

# These tests register toy nodes: set the real P1 nodes aside for this file.
.fx_isolate_nodes()

.re_project <- function(env = parent.frame()) {
  p <- .fx_project(env = env)
  p$state$roles <- list(t = c(a = "outcome"))
  p
}

# Close and reopen the project, so its state is replayed from events.jsonl.
.re_reopen <- function(p) {
  dir <- dirname(p$paths$json)
  sr_project_close(p)
  sr_project_open(dir)
}

.re_events <- function(p, type) {
  Filter(function(e) identical(e$type, type), sr_events_read(p$paths$events))
}

test_that("rules decide and dependent rules follow in one run", {
  .fx_toy_nodes()
  p <- .re_project()
  p <- sr_router_decide(p, "toy_a", "a1")
  p <- sr_router_run(p)
  expect_identical(sr_router_choice(p, "toy_b"), "b1")
  tb <- sr_router_table(p)
  expect_identical(tb$resolver[tb$key == "toy_b"], "rules")
  expect_false(tb$override[tb$key == "toy_b"])
  expect_identical(tb$reason[tb$key == "toy_b"], "copied from toy_a")
  expect_identical(tb$status[tb$key == "toy_c@t"], "pending")
})

test_that("a model prediction is logged and never acts", {
  .fx_toy_nodes()
  env <- environment()
  .fx_fake_backend(env, list(choice = "a2", probabilities = c(a1 = 0.03, a2 = 0.97)))
  p <- .re_project()
  p <- sr_router_run(p, backends = "jev")
  ev <- .re_events(p, "router.predicted")
  expect_length(ev, 1L)
  expect_true(ev[[1]]$payload$would_act)
  expect_identical(ev[[1]]$payload$choice, "a2")
  expect_identical(ev[[1]]$payload$backend, "jev")
  expect_match(ev[[1]]$payload$payload, "questions", fixed = TRUE)
  expect_identical(ev[[1]]$payload$payload_sha, .sr_sha256(ev[[1]]$payload$payload))
  tb <- sr_router_table(p)
  expect_identical(tb$status[tb$key == "toy_a"], "pending")
  expect_identical(tb$model_choice[tb$key == "toy_a"], "a2")
  expect_true(tb$would_act[tb$key == "toy_a"])
  expect_true(is.na(sr_router_choice(p, "toy_a")))
  expect_length(.re_events(p, "router.decided"), 0L)
})

test_that("a low-confidence prediction would not act", {
  .fx_toy_nodes()
  env <- environment()
  .fx_fake_backend(env, list(choice = "a2", probabilities = c(a1 = 0.4, a2 = 0.6)))
  p <- sr_router_run(.re_project(), backends = "jev")
  expect_false(.re_events(p, "router.predicted")[[1]]$payload$would_act)
})

test_that("a past decision is never re-asked", {
  .fx_toy_nodes()
  env <- environment()
  .fx_fake_backend(env, list(choice = "a2", probabilities = c(a1 = 0.03, a2 = 0.97)))
  p <- .re_project()
  p <- sr_router_decide(p, "toy_a", "a1")
  p <- sr_router_run(p)
  n_dec <- length(.re_events(p, "router.decided"))
  p <- .re_reopen(p)
  p <- sr_router_run(p, backends = "jev")
  expect_identical(env$.fake_calls, 0L)
  expect_length(.re_events(p, "router.decided"), n_dec)
  expect_identical(sr_router_choice(p, "toy_a"), "a1")
})

test_that("a prediction is replayed, not re-asked, for the same input", {
  .fx_toy_nodes()
  env <- environment()
  .fx_fake_backend(env, list(choice = "a2", probabilities = c(a1 = 0.4, a2 = 0.6)))
  p <- .re_project()
  p <- sr_router_run(p, backends = "jev")
  p <- sr_router_run(p, backends = "jev")
  expect_identical(env$.fake_calls, 1L)
  p <- .re_reopen(p)
  p <- sr_router_run(p, backends = "jev")
  expect_identical(env$.fake_calls, 1L)
})

test_that("a failed prediction is logged and asked again by the next run", {
  .fx_toy_nodes()
  env <- environment()
  .fx_fake_backend(env, list(), fail = TRUE)
  p <- .re_project()
  p <- sr_router_run(p, backends = "jev")
  p <- sr_router_run(p, backends = "jev")
  expect_identical(env$.fake_calls, 2L)
  ev <- .re_events(p, "router.predicted")
  expect_length(ev, 2L)
  expect_identical(ev[[1]]$payload$error, "fake failure")
  expect_false(ev[[1]]$payload$would_act)
  tb <- sr_router_table(p)
  expect_true(is.na(tb$model_choice[tb$key == "toy_a"]))
})

test_that("backends are skipped when unregistered or not requested", {
  .fx_toy_nodes()
  p <- .re_project()
  p <- sr_router_run(p, backends = c("jev", "nope"))
  expect_length(.re_events(p, "router.predicted"), 0L)
  env <- environment()
  .fx_fake_backend(env, list(choice = "a2", probabilities = c(a1 = 0.4, a2 = 0.6)))
  p <- sr_router_run(p)
  expect_identical(env$.fake_calls, 0L)
})

test_that("escalation stops at the first backend that would act", {
  .fx_toy_nodes()
  env <- environment()
  .fx_fake_backend(env, list(choice = "a2", probabilities = c(a1 = 0.03, a2 = 0.97)), id = "local")
  # env2 is not a frame, so the helper's deferred restore never runs: restore the real jev here.
  jev0 <- .sr_backend_get("jev")
  withr::defer(sr_backend_register("jev", jev0$ask, jev0$available))
  env2 <- new.env()
  .fx_fake_backend(env2,list(choice = "a1", probabilities = c(a1 = 0.97, a2 = 0.03)), id = "jev")
  p <- sr_router_run(.re_project(), backends = c("jev", "local"))
  expect_identical(env$.fake_calls, 1L)
  expect_identical(env2$.fake_calls, 0L)
})

test_that("an upstream change makes dependents stale", {
  .fx_toy_nodes()
  p <- .re_project()
  p <- sr_router_decide(p, "toy_a", "a1")
  p <- sr_router_run(p)
  expect_identical(sr_router_choice(p, "toy_b"), "b1")
  p <- sr_router_decide(p, "toy_a", "a2")
  tb <- sr_router_table(p)
  expect_identical(tb$status[tb$key == "toy_b"], "stale")
  expect_true(is.na(sr_router_choice(p, "toy_b")))
  expect_identical(tb$status[tb$key == "toy_c@t"], "waiting")
  p <- sr_router_run(p)
  expect_identical(sr_router_choice(p, "toy_b"), "b2")
  tb <- sr_router_table(p)
  expect_identical(tb$status[tb$key == "toy_b"], "decided")
})

test_that("choices, overrides and vetoes", {
  .fx_toy_nodes()
  p <- .re_project()
  p <- sr_router_decide(p, "toy_a", "a1")
  expect_error(sr_router_decide(p, "toy_a", "zz"), "a1, a2")
  expect_error(sr_router_decide(p, "toy_c", "c1", table = "t"), "undecided dependency")
  expect_error(sr_router_decide(p, "nope", "x"), "no instance")
  expect_error(sr_router_decide(p, "toy_b", "b2"), "reason")
  q <- sr_router_decide(p, "toy_b", "b2", reason = "clinical judgement")
  d <- q$state$router$decisions[["toy_b"]]
  expect_true(d$override)
  expect_identical(d$suggested, "b1")
  expect_identical(d$resolver, "user")
  expect_identical(d$reason, "clinical judgement")

  old <- sr_node_get("toy_a")
  veto_a <- sr_node("toy_a", "Toy A", c("a1", "a2"), question = old$question,
                    veto = function(ctx, choice) {
                      if (identical(choice, "a2")) "a2 is not allowed here" else NULL
                    })
  sr_node_register(veto_a)
  withr::defer(sr_node_register(old))
  p <- .fx_project()
  expect_error(sr_router_decide(.fx_project(), "toy_a", "a2"), "not allowed here")
  for (i in 1:3) {
    cnd <- tryCatch(sr_router_decide(p, "toy_a", "a2"), sr_router_vetoed = function(c) c)
    expect_s3_class(cnd, "sr_router_vetoed")
    expect_identical(cnd$reason, "a2 is not allowed here")
    p <- cnd$project
  }
  expect_length(.re_events(p, "router.vetoed"), 3L)
  tb <- sr_router_table(p)
  expect_identical(tb$status[tb$key == "toy_a"], "override_required")
  expect_identical(tb$vetoes[tb$key == "toy_a"], 3L)
  expect_error(sr_router_decide(p, "toy_a", "a2", override = TRUE), "reason")
  p <- sr_router_decide(p, "toy_a", "a2", reason = "reviewed with the PI", override = TRUE)
  expect_identical(sr_router_choice(p, "toy_a"), "a2")
  expect_true(p$state$router$decisions[["toy_a"]]$override)
})

test_that("a model suggestion is overridden only with a reason", {
  .fx_toy_nodes()
  env <- environment()
  .fx_fake_backend(env, list(choice = "a2", probabilities = c(a1 = 0.03, a2 = 0.97)))
  p <- sr_router_run(.re_project(), backends = "jev")
  expect_error(sr_router_decide(p, "toy_a", "a1"), "reason")
  q <- sr_router_decide(p, "toy_a", "a2")
  expect_false(q$state$router$decisions[["toy_a"]]$override)
  expect_identical(q$state$router$decisions[["toy_a"]]$suggested, "a2")
})

test_that("decisions are frozen by the lock", {
  .fx_toy_nodes()
  p <- .fx_unblind(.re_project())
  expect_error(sr_router_decide(p, "toy_a", "a1"), "frozen")
  expect_identical(sr_router_run(p), p)
})

test_that("a rule outside the options is a code error", {
  .fx_toy_nodes()
  old <- sr_node_get("toy_b")
  bad <- sr_node("toy_b", "Toy B", c("b1", "b2"), depends = "toy_a",
                 rule = function(ctx) list(choice = "x", reason = "bug"))
  sr_node_register(bad)
  withr::defer(sr_node_register(old))
  p <- .re_project()
  p <- sr_router_decide(p, "toy_a", "a1")
  expect_error(sr_router_run(p), "toy_b")
})

test_that("a column-option prediction is stored in column names", {
  nd <- sr_node("toy_pick", "Pick a column", function(ctx) c("id", "age"), scope = "table",
                facts_columns = TRUE, danger = list(c("age", "id")),
                question = function(ctx) list(instructions = "Which column is the exposure?",
                                              criteria = list(c001 = "first", c002 = "second")))
  sr_node_register(nd)
  withr::defer(.sr_node_unregister("toy_pick"))
  env <- environment()
  .fx_fake_backend(env, list(choice = "c002", probabilities = c(c001 = 0.02, c002 = 0.98)))
  df <- data.frame(id = 1:4, age = c(61.5, 72.25, 48.75, 55.5))
  p <- .fx_project()
  withr::defer(sr_duck_close(p))
  csv <- file.path(dirname(p$paths$dir), "t.csv")
  .fx_write_csv(df, csv)
  p <- sr_ingest(p, csv, table = "t",
                 options = list(drive_info = function(path) list(free_bytes = 1e13, remote = FALSE,
                                                                 warnings = character())))
  p <- sr_roles_set(p, "t", c(id = "id", age = "covariate"))
  p <- sr_router_run(p, backends = "jev")
  ev <- .re_events(p, "router.predicted")
  expect_length(ev, 1L)
  expect_identical(ev[[1]]$payload$choice, "age")
  expect_identical(names(ev[[1]]$payload$probabilities), c("id", "age"))
  expect_true(ev[[1]]$payload$would_act)
  tb <- sr_router_table(p)
  expect_identical(tb$model_choice[tb$key == "toy_pick@t"], "age")
  q <- sr_router_decide(p, "toy_pick", "age", table = "t")
  expect_identical(sr_router_choice(q, "toy_pick", "t"), "age")
})

test_that("a rule error in a run carries the project with the earlier decisions", {
  .fx_toy_nodes()
  old <- sr_node_get("toy_b")
  bad <- sr_node("toy_b", "Toy B", c("b1", "b2"), depends = "toy_a",
                 rule = function(ctx) list(choice = "x", reason = "bug"))
  sr_node_register(bad)
  withr::defer(sr_node_register(old))
  p <- .re_project()
  p <- sr_router_decide(p, "toy_a", "a1")
  cnd <- tryCatch(sr_router_run(p), sr_router_error = function(c) c)
  expect_s3_class(cnd, "sr_router_error")
  expect_match(conditionMessage(cnd), "toy_b")
  expect_identical(cnd$project$state$router$decisions[["toy_a"]]$choice, "a1")
  # an earlier instance decided by rule in the same run is kept
  olda <- sr_node_get("toy_a")
  a2 <- sr_node("toy_a", "Toy A", c("a1", "a2"), rule = function(ctx) list(choice = "a1", reason = "fixed"))
  sr_node_register(a2)
  withr::defer(sr_node_register(olda))
  p2 <- .re_project()
  cnd2 <- tryCatch(sr_router_run(p2), sr_router_error = function(c) c)
  expect_s3_class(cnd2, "sr_router_error")
  expect_identical(cnd2$project$state$router$decisions[["toy_a"]]$resolver, "rules")
})

test_that("would_act is FALSE when the abstain argmin differs from the model choice", {
  .fx_toy_nodes()
  old <- sr_node_get("toy_a")
  dng <- sr_node("toy_a", "Toy A", c("a1", "a2"), question = old$question, danger = list(c("a2", "a1")))
  sr_node_register(dng)
  withr::defer(sr_node_register(old))
  env <- environment()
  .fx_fake_backend(env, list(choice = "a1", probabilities = c(a1 = 0.9, a2 = 0.1)))
  p <- sr_router_run(.re_project(), backends = "jev")
  ev <- .re_events(p, "router.predicted")
  expect_false(ev[[1]]$payload$would_act)
  expect_identical(ev[[1]]$payload$choice, "a1")
  tb <- sr_router_table(p)
  expect_identical(tb$status[tb$key == "toy_a"], "pending")
  expect_identical(tb$model_choice[tb$key == "toy_a"], "a1")
})
