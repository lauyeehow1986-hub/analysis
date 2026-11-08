# router/sysone.R - the pure request/response contract for TypeSafe /v1/systemone "choice"
# questions, plus the backend registry. No network code lives here.

SR_SYSONE_MAX_CRITERIA <- 255L

#' One choice question. `criteria` is a named list of option key -> description (chr(1) or NULL).
sr_sysone_choice <- function(instructions, criteria) {
  if (!is.character(instructions) || length(instructions) != 1L || is.na(instructions) ||
      !nzchar(trimws(instructions))) {
    stop("sr_sysone_choice: instructions must be a non-empty string", call. = FALSE)
  }
  n <- length(criteria)
  if (n < 1L || n > SR_SYSONE_MAX_CRITERIA) {
    stop("sr_sysone_choice: need between 1 and ", SR_SYSONE_MAX_CRITERIA, " criteria, got ", n,
         call. = FALSE)
  }
  keys <- names(criteria)
  if (is.null(keys) || anyNA(keys) || any(!nzchar(keys))) {
    stop("sr_sysone_choice: every criterion needs a non-empty key", call. = FALSE)
  }
  if (anyDuplicated(keys)) stop("sr_sysone_choice: duplicated criteria keys", call. = FALSE)
  ok <- vapply(criteria, function(d) is.null(d) ||
                 (is.character(d) && length(d) == 1L && !is.na(d)), logical(1))
  if (!all(ok)) stop("sr_sysone_choice: a description must be a string or NULL", call. = FALSE)
  list(type = "choice", instructions = instructions, criteria = criteria)
}

#' A request: model, a state string and a named list of choice questions.
sr_sysone_request <- function(state, questions, model = sr_opt("jev_model", "jev-latest")) {
  if (!is.character(state) || length(state) != 1L || is.na(state)) {
    stop("sr_sysone_request: state must be a string", call. = FALSE)
  }
  qn <- names(questions)
  if (length(questions) < 1L || is.null(qn) || anyNA(qn) || any(!nzchar(qn)) || anyDuplicated(qn)) {
    stop("sr_sysone_request: questions must be a named list with unique names", call. = FALSE)
  }
  list(model = model, state = state, questions = questions[order(qn)])
}

#' The exact bytes sent (canonical JSON; keys sorted).
sr_sysone_body <- function(request) sr_canonical_json(request)

#' SHA-256 of the request body.
sr_sysone_sha <- function(request) .sr_sha256(sr_sysone_body(request))

#' TypeSafe confidence: (max(p) - 1/n) / (1 - 1/n); 1 when n is 1.
sr_sysone_confidence <- function(p) {
  n <- length(p)
  if (n <= 1L) return(1)
  (max(p) - 1 / n) / (1 - 1 / n)
}

.sr_sysone_fail <- function(msg) {
  list(ok = FALSE, choice = NA_character_, probabilities = NULL, confidence = NA_real_, error = msg)
}

.sr_sysone_parse_one <- function(ans, keys) {
  if (is.null(ans) || !is.list(ans)) return(.sr_sysone_fail("answer missing"))
  if (!identical(ans[["type"]], "choice")) return(.sr_sysone_fail("answer type is not 'choice'"))
  ch <- ans[["choice"]]
  if (!is.character(ch) || length(ch) != 1L || is.na(ch) || !ch %in% keys) {
    return(.sr_sysone_fail("choice is not one of the criteria keys"))
  }
  pr <- ans[["probabilities"]]
  pv <- suppressWarnings(vapply(pr, function(v) as.numeric(v)[1L], numeric(1)))
  pn <- names(pr)
  if (length(pr) < 1L || is.null(pn) || anyNA(pv) || anyNA(pn) || !all(pn %in% keys) ||
      anyDuplicated(pn)) {
    return(.sr_sysone_fail("probabilities are missing, non-numeric or name unknown keys"))
  }
  if (any(pv < 0 | pv > 1)) return(.sr_sysone_fail("a probability is outside [0, 1]"))
  if (abs(sum(pv) - 1) > 1e-3) return(.sr_sysone_fail("probabilities do not sum to 1"))
  p <- stats::setNames(rep(0, length(keys)), keys)
  p[pn] <- pv
  list(ok = TRUE, choice = ch, probabilities = p, confidence = sr_sysone_confidence(p),
       error = NA_character_)
}

#' Validate a parsed response against its request: one result per question.
sr_sysone_parse <- function(response, request) {
  answers <- if (is.list(response)) response[["answers"]] else NULL
  out <- lapply(names(request$questions), function(q) {
    .sr_sysone_parse_one(answers[[q]], names(request$questions[[q]]$criteria))
  })
  stats::setNames(out, names(request$questions))
}

# ---- backend registry -------------------------------------------------------------------------

.sr_backend_env <- new.env(parent = emptyenv())

#' Register a backend. ask(request, ctx) -> list(ok, response, raw, model, latency_ms, error);
#' available(project) -> list(ok, reason).
sr_backend_register <- function(id, ask, available) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    stop("sr_backend_register: id must be a non-empty string", call. = FALSE)
  }
  if (!is.function(ask) || !is.function(available)) {
    stop("sr_backend_register: ask and available must be functions", call. = FALSE)
  }
  assign(id, list(id = id, ask = ask, available = available), envir = .sr_backend_env)
  invisible(id)
}

#' Ids of the registered backends.
sr_backends <- function() sort(ls(.sr_backend_env, all.names = TRUE))

.sr_backend_get <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !exists(id, envir = .sr_backend_env,
                                                                    inherits = FALSE)) {
    stop("unknown backend: ", paste(format(id), collapse = ","), call. = FALSE)
  }
  get(id, envir = .sr_backend_env, inherits = FALSE)
}
