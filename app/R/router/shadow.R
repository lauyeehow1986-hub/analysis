# router/shadow.R - the shadow-mode report: how the model's logged predictions compare with the final
# decisions, the per-node summary rates, and the golden-set export (P3 feed).
#
# Everything here is computed in R from the logged rows; no model is involved. A prediction is never
# applied in P1, so "final" is always a rule's or the user's decision.

.sr_shadow_cols <- c("key", "node", "table", "column", "backend", "model", "model_choice", "confidence",
                     "would_act", "error", "final_choice", "resolver", "override", "agree", "latency_ms")

.sr_shadow_empty <- function() {
  data.frame(key = character(), node = character(), table = character(), column = character(),
             backend = character(), model = character(), model_choice = character(),
             confidence = numeric(), would_act = logical(), error = character(),
             final_choice = character(), resolver = character(), override = logical(),
             agree = logical(), latency_ms = numeric(), stringsAsFactors = FALSE)
}

#' One row per (key, backend): the latest prediction joined with the current decision. `final_choice`
#' is NA when the instance is undecided or its decision is stale; `agree` is NA when either side is NA.
sr_router_shadow <- function(project) {
  preds <- project$state$router$predictions
  rows <- list()
  for (key in names(preds)) {
    dec <- project$state$router$decisions[[key]]
    final <- .sr_router_current(project, key)
    cur <- !is.na(final)
    for (be in names(preds[[key]])) {
      pr <- preds[[key]][[be]]
      mc <- pr$choice
      rows[[length(rows) + 1L]] <- data.frame(
        key = key, node = pr$node, table = pr$table, column = pr$column, backend = be,
        model = pr$model, model_choice = mc, confidence = pr$confidence,
        would_act = isTRUE(pr$would_act), error = pr$error, final_choice = final,
        resolver = if (cur) dec$resolver else NA_character_,
        override = if (cur) isTRUE(dec$override) else NA,
        agree = if (is.na(mc) || !cur) NA else identical(mc, final),
        latency_ms = pr$latency_ms, stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(.sr_shadow_empty())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[, .sr_shadow_cols, drop = FALSE]
}

#' Per (node, backend): n predictions; decided (with a final choice); agree_rate (over decided rows
#' without an error); would_act_rate (over n); act_accuracy (agreement among decided rows that would
#' act, NA when none); error_rate (over n).
sr_router_shadow_summary <- function(project) {
  sh <- sr_router_shadow(project)
  out <- data.frame(node = character(), backend = character(), n = integer(), decided = integer(),
                    agree_rate = numeric(), would_act_rate = numeric(), act_accuracy = numeric(),
                    error_rate = numeric(), stringsAsFactors = FALSE)
  if (!nrow(sh)) return(out)
  rate <- function(x) if (length(x)) mean(x) else NA_real_
  groups <- unique(sh[, c("node", "backend")])
  rows <- lapply(seq_len(nrow(groups)), function(i) {
    g <- sh[sh$node == groups$node[i] & sh$backend == groups$backend[i], , drop = FALSE]
    dec <- !is.na(g$final_choice)
    ok <- dec & is.na(g$error)
    act <- dec & g$would_act & !is.na(g$agree)
    data.frame(node = groups$node[i], backend = groups$backend[i], n = nrow(g), decided = sum(dec),
               agree_rate = rate(g$agree[ok & !is.na(g$agree)]), would_act_rate = mean(g$would_act),
               act_accuracy = rate(g$agree[act]), error_rate = mean(!is.na(g$error)),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$n <- as.integer(out$n)
  out$decided <- as.integer(out$decided)
  rownames(out) <- NULL
  out
}

#' Write the golden-set feed as JSON lines (one per (key, backend) prediction: payload, raw, input_hash,
#' payload_sha, the prediction and the final decision). Refuses to overwrite. No key is ever stored, so
#' none is written. Returns `path`.
sr_router_shadow_export <- function(project, path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("sr_router_shadow_export: path must be a single string", call. = FALSE)
  }
  if (file.exists(path)) stop("export exists: ", basename(path), " (it is not overwritten)", call. = FALSE)
  sh <- sr_router_shadow(project)
  last <- list()
  for (e in sr_events_read(project$paths$events)) {
    if (identical(e$type, "router.predicted")) {
      last[[paste(e$payload$key, e$payload$backend, sep = "\r")]] <- e$payload
    }
  }
  lines <- character()
  for (i in seq_len(nrow(sh))) {
    ev <- last[[paste(sh$key[i], sh$backend[i], sep = "\r")]]
    pr <- project$state$router$predictions[[sh$key[i]]][[sh$backend[i]]]
    rec <- list(key = sh$key[i], node = sh$node[i], table = sh$table[i], column = sh$column[i],
                backend = sh$backend[i], model = sh$model[i], input_hash = pr$input_hash,
                payload_sha = pr$payload_sha, payload = ev$payload, raw = ev$raw,
                choice = sh$model_choice[i], probabilities = as.list(pr$probabilities),
                confidence = sh$confidence[i], would_act = sh$would_act[i], error = sh$error[i],
                final_choice = sh$final_choice[i], resolver = sh$resolver[i],
                override = sh$override[i], agree = sh$agree[i], latency_ms = sh$latency_ms[i])
    lines <- c(lines, .sr_json(rec))
  }
  bytes <- if (length(lines)) charToRaw(enc2utf8(paste0(paste(lines, collapse = "\n"), "\n"))) else raw(0)
  .sr_write_atomic(path, bytes)
  invisible(path)
}
