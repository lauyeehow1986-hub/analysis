# Tests for router/backends/jev.R: the gates, the approval of the exact payload, the failure mapping and
# the key never being stored. The transport is always injected; nothing reaches the network.

SENTINEL <- "sk-test-SENTINEL-0000"

.rj_project <- function(env = parent.frame()) {
  p <- .fx_project(env = env)
  sr_pico_set(p, population = "adults with heart failure", outcome = "one-year mortality")
}

# All four gates met.
.rj_ready <- function(p) {
  p <- sr_router_jev_ack(p, "DPA / PDPA and IT sign-off documented, ref SYN-1")
  sr_router_consent(p)
}

.rj_events <- function(p, type) {
  Filter(function(e) identical(e$type, type), sr_events_read(p$paths$events))
}

# A valid answer for every question: the first criterion with probability 1.
.rj_ok_body <- function(body) {
  rq <- .sr_parse_json(body)
  ans <- lapply(rq$questions, function(q) {
    k <- names(q$criteria)
    list(type = "choice", choice = k[1L],
         probabilities = as.list(stats::setNames(c(1, rep(0, length(k) - 1L)), k)))
  })
  .sr_json(list(model = "jev-test", answers = ans))
}

.rj_all_shas <- function(p) {
  vapply(c("design", "purpose"), function(n) sr_router_preview(p, n)$sha, "")
}

.rj_request <- function(p, node = "design") {
  sr_router_request(.sr_router_ctx(p, sr_node_get(node), NA_character_, NA_character_))$request
}

.rj_design_row <- function(p) {
  tb <- sr_router_table(p)
  tb[tb$key == "design", ]
}

test_that("the session id is 16 hex characters, stable, and leaves .Random.seed alone", {
  set.seed(1)
  s0 <- .Random.seed
  id <- sr_router_session()
  expect_match(id, "^[0-9a-f]{16}$")
  expect_identical(sr_router_session(), id)
  expect_identical(.Random.seed, s0)
})

test_that("air-gap mode never calls the transport", {
  env <- environment()
  withr::local_options(sr.jev_transport = .fx_fake_transport(env, body = .rj_ok_body),
                       sr.jev_key = SENTINEL)
  p <- .rj_ready(.rj_project())
  expect_identical(sr_mode(), "airgap")
  av <- .sr_backend_get("jev")$available(p)
  expect_false(av$ok)
  expect_identical(av$reason, "air-gap mode")
  p <- sr_router_run(p, backends = "jev", approve = .rj_all_shas(p))
  expect_length(env$.calls, 0L)
  expect_length(.rj_events(p, "router.predicted"), 0L)
})

test_that("each gate is required, in order", {
  env <- environment()
  .fx_connected(env, key = "")
  p <- .rj_project()
  reason <- function(p) .sr_backend_get("jev")$available(p)$reason
  expect_match(reason(p), "no Jev key")
  withr::local_options(sr.jev_key = SENTINEL)
  expect_match(reason(p), "no DPA")
  p <- sr_router_jev_ack(p, "DPA and IT sign-off documented")
  expect_match(reason(p), "no consent")
  # consent recorded under another session id does not count
  p <- sr_project_append(p, "router.consent", list(session = "0000000000000000", granted = TRUE))
  expect_match(reason(p), "no consent")
  p <- sr_router_consent(p, granted = FALSE)
  expect_match(reason(p), "no consent")
  p <- sr_router_consent(p)
  expect_true(.sr_backend_get("jev")$available(p)$ok)
})

test_that("the acknowledgement needs a reason and a writable project", {
  p <- .rj_project()
  expect_error(sr_router_jev_ack(p, ""), "reason")
  expect_error(sr_router_jev_ack(p, NA_character_), "reason")
  p$readonly <- TRUE
  expect_error(sr_router_jev_ack(p, "DPA and IT sign-off"), "read-only")
  expect_error(sr_router_consent(p), "read-only")
})

test_that("an unapproved payload is never sent", {
  env <- environment()
  .fx_connected(env)
  withr::local_options(sr.jev_transport = .fx_fake_transport(env, body = .rj_ok_body))
  p <- .rj_ready(.rj_project())
  p <- sr_router_run(p, backends = "jev", approve = character())
  expect_length(env$.calls, 0L)
  ev <- .rj_events(p, "router.predicted")
  expect_true(length(ev) >= 1L)
  expect_true(all(vapply(ev, function(e) identical(e$payload$error, "payload not approved"), NA)))
  expect_identical(.rj_design_row(p)$status, "pending")
})

test_that("an approved payload is sent exactly as previewed", {
  env <- environment()
  .fx_connected(env)
  withr::local_options(sr.jev_transport = .fx_fake_transport(env, body = .rj_ok_body))
  p <- .rj_ready(.rj_project())
  pv <- sr_router_preview(p, "design")
  p <- sr_router_run(p, backends = "jev", approve = pv$sha)
  expect_length(env$.calls, 1L)
  call <- env$.calls[[1L]]
  expect_identical(call$body, pv$body)
  expect_identical(call$url, "https://api.typesafe.ai/v1/systemone")
  expect_identical(call$key, SENTINEL)
  expect_identical(call$timeout, 20)
  ev <- Filter(function(e) identical(e$payload$node, "design"), .rj_events(p, "router.predicted"))
  expect_length(ev, 1L)
  expect_identical(ev[[1]]$payload$payload_sha, pv$sha)
  expect_true(is.null(ev[[1]]$payload$error) || is.na(ev[[1]]$payload$error))
  expect_identical(ev[[1]]$payload$model, "jev-test")
  expect_false(is.null(ev[[1]]$payload$choice))
  expect_true(is.logical(ev[[1]]$payload$would_act))
  expect_identical(.rj_design_row(p)$status, "pending")
  expect_length(.rj_events(p, "router.decided"), 0L)
})

test_that("failures are logged and the run carries on", {
  env <- environment()
  .fx_connected(env)
  p0 <- .rj_ready(.rj_project())
  approve <- .rj_all_shas(p0)
  bad_choice <- function(b) {
    rq <- .sr_parse_json(b)
    ans <- lapply(rq$questions, function(q) {
      list(type = "choice", choice = "not_a_key", probabilities = list(not_a_key = 1))
    })
    .sr_json(list(model = "m", answers = ans))
  }
  cases <- list(
    list(tr = .fx_fake_transport(env, 400L, '{"detail":"bad"}'), want = "HTTP 400: bad"),
    list(tr = .fx_fake_transport(env, 422L, '{"detail":[{"msg":"first"},{"msg":"second"}]}'),
         want = "HTTP 422: first; second"),
    list(tr = .fx_fake_transport(env, 500L, "oops"), want = "HTTP 500"),
    list(tr = .fx_fake_transport(env, 200L, "<html>"), want = "invalid JSON"),
    list(tr = function(url, body, key, timeout) stop("Timeout was reached"),
         want = "Timeout was reached"),
    list(tr = .fx_fake_transport(env, 200L, bad_choice),
         want = "choice is not one of the criteria keys"))
  for (i in seq_along(cases)) {
    cs <- cases[[i]]
    n_calls <- 0L
    inner <- cs$tr
    counted <- function(url, body, key, timeout) {
      n_calls <<- n_calls + 1L
      inner(url, body, key, timeout)
    }
    withr::local_options(sr.jev_transport = counted)
    p <- sr_router_run(p0, backends = "jev", approve = approve)
    # one transport call per approved instance (design, purpose): never a retry
    expect_identical(n_calls, 2L, info = cs$want)
    ev <- .rj_events(p, "router.predicted")
    expect_true(length(ev) >= 2L * i, info = cs$want)
    dd <- Filter(function(e) identical(e$payload$node, "design"), ev)
    d <- dd[[length(dd)]]$payload
    expect_identical(d$error, cs$want)
    expect_true(is.null(d$choice) || is.na(d$choice))
    expect_true(is.na(p$state$router$predictions[["design"]][["jev"]]$choice))
    # the second pending instance was still processed
    expect_true(any(vapply(ev, function(e) identical(e$payload$node, "purpose"), NA)), info = cs$want)
  }
})

test_that("the key is never written anywhere", {
  env <- environment()
  .fx_connected(env)
  p <- .rj_ready(.rj_project())
  approve <- .rj_all_shas(p)
  rq <- .rj_request(p)
  msgs <- character()
  trs <- list(
    function(url, body, key, timeout) stop("connect failed using ", key),
    .fx_fake_transport(env, 400L, paste0('{"detail":"echo ', SENTINEL, '"}')),
    .fx_fake_transport(env, 200L, paste0("<html>", SENTINEL)),
    .fx_fake_transport(env, 200L, .rj_ok_body))
  for (tr in trs) {
    withr::local_options(sr.jev_transport = tr)
    caught <- character()
    r <- withCallingHandlers({
      p <- sr_router_run(p, backends = "jev", approve = approve)
      .sr_backend_get("jev")$ask(rq, list(project = p, approve = approve))
    }, condition = function(cnd) caught <<- c(caught, conditionMessage(cnd)))
    msgs <- c(msgs, r$error, r$raw, caught)
    # a throwing transport is caught inside ask: no condition surfaces the key
    expect_false(any(grepl("SENTINEL", caught, fixed = TRUE)))
  }
  expect_true(length(.rj_events(p, "router.predicted")) >= 1L)
  files <- list.files(dirname(p$paths$json), recursive = TRUE, full.names = TRUE, all.files = TRUE)
  expect_true(length(files) > 0L)
  for (f in files) {
    bin <- readBin(f, "raw", file.size(f))
    txt <- rawToChar(bin[bin != as.raw(0)])
    expect_false(grepl("SENTINEL", txt, fixed = TRUE), info = f)
  }
  expect_false(any(grepl("SENTINEL", msgs, fixed = TRUE)))
  expect_true(any(grepl("<redacted>", msgs, fixed = TRUE)))
})

test_that("only jev.R references curl", {
  app <- normalizePath(test_path("..", "..", "app", "R"), winslash = "/")
  files <- list.files(app, pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
  expect_true(any(grepl("router/backends/jev[.]R$", files)))
  expect_identical(nrow(sr_lint_airgap(files)), 0L)
  tmp <- file.path(withr::local_tempdir(), "jev_copy.R")
  file.copy(file.path(app, "router", "backends", "jev.R"), tmp)
  expect_gt(nrow(sr_lint_airgap(tmp)), 0L)
})
