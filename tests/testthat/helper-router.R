# Test helpers for the router: toy nodes registered for one test and removed again.

#' Register toy_a (project), toy_b (depends on toy_a, rule), toy_c (table, depends on toy_b).
.fx_toy_nodes <- function(env = parent.frame()) {
  a <- sr_node("toy_a", "Toy A", c("a1", "a2"),
               question = function(ctx) list(instructions = "Pick the better of the two toy options.",
                                             criteria = list(a1 = "first", a2 = "second")))
  b <- sr_node("toy_b", "Toy B", c("b1", "b2"), depends = "toy_a",
               rule = function(ctx) {
                 a <- ctx$dep$toy_a
                 if (is.null(a) || is.na(a)) return(NULL)
                 list(choice = paste0("b_", substring(a, 2L)) |> sub(pattern = "^b_", replacement = "b"),
                      reason = "copied from toy_a")
               })
  cc <- sr_node("toy_c", "Toy C", c("c1", "c2"), scope = "table", depends = "toy_b")
  for (n in list(a, b, cc)) sr_node_register(n)
  withr::defer(for (id in c("toy_c", "toy_b", "toy_a")) .sr_node_unregister(id), envir = env)
  invisible(c("toy_a", "toy_b", "toy_c"))
}

#' Register a fake backend `id` (default "jev", the one sr_router_run can reach in a test). `ask` counts
#' its calls in `env$.fake_calls` and answers `answer` (list(choice, probabilities), in criteria keys) for
#' the request's single question; with `fail = TRUE` it counts the call and returns ok = FALSE. Whatever
#' was registered under `id` is restored when the caller's `frame` exits (or the id is removed).
.fx_fake_backend <- function(env, answer, fail = FALSE, id = "jev", frame = parent.frame()) {
  old <- tryCatch(.sr_backend_get(id), error = function(e) NULL)
  env$.fake_calls <- 0L
  ask <- function(request, ctx) {
    env$.fake_calls <- env$.fake_calls + 1L
    if (isTRUE(fail)) {
      return(list(ok = FALSE, response = NULL, raw = NA_character_, model = NA_character_,
                  latency_ms = 1, error = "fake failure"))
    }
    qn <- names(request$questions)[1L]
    ans <- list(type = "choice", choice = answer$choice, probabilities = as.list(answer$probabilities))
    list(ok = TRUE, response = list(answers = stats::setNames(list(ans), qn)), raw = "{}",
         model = "fake-model", latency_ms = 1, error = NA_character_)
  }
  sr_backend_register(id, ask = ask, available = function(project) list(ok = TRUE, reason = NA_character_))
  withr::defer({
    if (is.null(old)) {
      rm(list = id, envir = .sr_backend_env)
    } else {
      sr_backend_register(old$id, old$ask, old$available)
    }
  }, envir = frame)
  invisible(id)
}

#' Set the node registry aside (empty) until `env` exits, so a test file of toy nodes does not meet the
#' real P1 nodes. File level: `.fx_isolate_nodes()` (it cleans up when the file ends).
.fx_isolate_nodes <- function(env = parent.frame()) {
  nodes <- .sr_node_env$nodes
  order <- .sr_node_env$order
  .sr_node_env$nodes <- list()
  .sr_node_env$order <- character()
  withr::defer({
    .sr_node_env$nodes <- nodes
    .sr_node_env$order <- order
  }, envir = env)
  invisible(NULL)
}

#' A fake Jev transport. Records each call (url, body, key, timeout) in `env$.calls`. `body` is a string,
#' or a function of the request body that gives the string. Returns list(status, body).
.fx_fake_transport <- function(env, status = 200L, body = "{}") {
  env$.calls <- list()
  function(url, body_in, key, timeout) {
    env$.calls[[length(env$.calls) + 1L]] <- list(url = url, body = body_in, key = key, timeout = timeout)
    list(status = status, body = if (is.function(body)) body(body_in) else body)
  }
}

#' Connected mode with the sentinel key, until `env` exits.
.fx_connected <- function(env = parent.frame(), key = "sk-test-SENTINEL-0000") {
  withr::local_options(sr.mode = "connected", sr.jev_key = key, .local_envir = env)
  invisible(key)
}
