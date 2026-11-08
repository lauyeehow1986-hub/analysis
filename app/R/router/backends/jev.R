# router/backends/jev.R - the Jev cloud backend of the System One router (TypeSafe /v1/systemone).
# Connected mode only, behind four gates (mode, key, DPA / IT acknowledgement, per-session consent)
# and a per-payload approval: the exact request body that was previewed. This is the only file that
# references curl. The key is never logged, never stored and never part of an error.

.sr_router_env <- new.env(parent = emptyenv())

#' The router session id: 16 hex characters, made once per R process. Uses openssl, never the R RNG,
#' so .Random.seed is untouched.
sr_router_session <- function() {
  id <- .sr_router_env$session
  if (is.null(id)) {
    id <- paste(as.character(openssl::rand_bytes(8)), collapse = "")
    .sr_router_env$session <- id
  }
  id
}

#' Record (or withdraw) consent to send payloads to Jev in this session.
sr_router_consent <- function(project, granted = TRUE) {
  sr_project_append(project, "router.consent",
                    list(session = sr_router_session(), granted = isTRUE(granted)))
}

#' Record that the DPA / PDPA and the institutional IT sign-off for Jev are documented. `reason` is
#' required and cites them.
sr_router_jev_ack <- function(project, reason) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  if (!is.character(reason) || length(reason) != 1L || is.na(reason) || !nzchar(trimws(reason))) {
    stop("a Jev acknowledgement needs a reason citing the DPA / PDPA and the IT sign-off", call. = FALSE)
  }
  sr_project_append(project, "router.jev_ack", list(reason = trimws(reason)))
}

# The first failing gate, in order, or ok = TRUE.
.sr_jev_available <- function(project) {
  no <- function(reason) list(ok = FALSE, reason = reason)
  if (!identical(sr_mode(), "connected")) return(no("air-gap mode"))
  key <- sr_opt("jev_key", "")
  if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
    return(no("no Jev key (set SR_JEV_KEY)"))
  }
  if (is.null(project$state$router$jev_ack)) return(no("no DPA / IT acknowledgement for this project"))
  if (!isTRUE(project$state$router$consent[[sr_router_session()]])) return(no("no consent in this session"))
  list(ok = TRUE, reason = NA_character_)
}

# The real transport: POST `body` with the bearer key. Never called by a test.
.sr_jev_curl <- function(url, body, key, timeout) {
  h <- curl::new_handle()
  curl::handle_setheaders(h, Authorization = paste("Bearer", key), `Content-Type` = "application/json")
  curl::handle_setopt(h, postfields = body, timeout = timeout)
  res <- curl::curl_fetch_memory(url, handle = h)
  txt <- rawToChar(res$content)
  Encoding(txt) <- "UTF-8"
  list(status = as.integer(res$status_code), body = txt)
}

.sr_jev_fail <- function(msg, raw = NA_character_, latency = NA_real_) {
  list(ok = FALSE, response = NULL, raw = raw, model = NA_character_, latency_ms = latency,
       error = msg)
}

# Remove the key from any text that may be stored.
.sr_jev_scrub <- function(x, key) {
  x <- as.character(x)
  if (nzchar(key)) x <- gsub(key, "<redacted>", x, fixed = TRUE)
  x
}

# One-line error text for a non-200 response.
.sr_jev_http_error <- function(status, body) {
  parsed <- tryCatch(.sr_parse_json(body), error = function(e) NULL)
  det <- if (is.list(parsed)) parsed$detail else NULL
  if (identical(status, 400L) && !is.null(det)) {
    return(paste0("HTTP 400: ", paste(unlist(det), collapse = "; ")))
  }
  if (identical(status, 422L) && is.list(det)) {
    msgs <- vapply(det, function(d) {
      m <- if (is.list(d)) d$msg else d
      if (is.null(m) || length(m) != 1L) "" else as.character(m)
    }, "")
    msgs <- msgs[nzchar(msgs)]
    if (length(msgs)) return(paste0("HTTP 422: ", paste(msgs, collapse = "; ")))
  }
  paste0("HTTP ", status)
}

.sr_jev_ask <- function(request, ctx) {
  av <- .sr_jev_available(ctx$project)
  if (!isTRUE(av$ok)) return(.sr_jev_fail(av$reason))
  sha <- sr_sysone_sha(request)
  if (!sha %in% ctx$approve) return(.sr_jev_fail("payload not approved"))
  key <- sr_opt("jev_key", "")
  url <- sr_opt("jev_url", "https://api.typesafe.ai/v1/systemone")
  timeout <- as.numeric(sr_opt("jev_timeout", 20))
  transport <- sr_opt("jev_transport", NULL)
  if (is.null(transport)) transport <- .sr_jev_curl
  body <- sr_sysone_body(request)
  t0 <- proc.time()[["elapsed"]]
  res <- tryCatch(transport(url, body, key, timeout), error = function(e) {
    structure(list(msg = .sr_jev_scrub(conditionMessage(e), key)), class = "sr_jev_transport_error")
  })
  latency <- round((proc.time()[["elapsed"]] - t0) * 1000, 1)
  if (inherits(res, "sr_jev_transport_error")) return(.sr_jev_fail(res$msg, latency = latency))
  status <- suppressWarnings(as.integer(res$status))
  txt <- if (is.null(res$body) || length(res$body) != 1L || is.na(res$body)) "" else
    .sr_jev_scrub(res$body, key)
  if (length(status) != 1L || is.na(status)) return(.sr_jev_fail("invalid HTTP status", latency = latency))
  if (status != 200L) {
    return(.sr_jev_fail(.sr_jev_scrub(.sr_jev_http_error(status, txt), key), latency = latency))
  }
  response <- tryCatch(.sr_parse_json(txt), error = function(e) NULL)
  if (!is.list(response)) return(.sr_jev_fail("invalid JSON", raw = txt, latency = latency))
  model <- response$model
  model <- if (is.character(model) && length(model) == 1L) model else NA_character_
  for (q in names(request$questions)) {
    one <- sr_sysone_parse(response, request)[[q]]
    if (!isTRUE(one$ok)) return(.sr_jev_fail(one$error, raw = txt, latency = latency))
  }
  list(ok = TRUE, response = response, raw = txt, model = model, latency_ms = latency,
       error = NA_character_)
}

sr_backend_register("jev", ask = .sr_jev_ask, available = .sr_jev_available)
