# router/engine.R - the routing engine: reducers for the router events, the run loop (rules first, then
# the escalation of backends in shadow mode), user decisions with vetoes and overrides, and the decision
# table.
#
# Models route; they never decide. A prediction is logged (router.predicted) and, in P1, never applied:
# only a rule or the user makes a router.decided event. A decision carries the input hash it was made
# under, so it goes stale when the facts, the options or an upstream choice change.

SR_ROUTER_MAX_VETOES <- 3L
SR_ROUTER_ESCALATION <- c("local", "clm", "jev")

# ---- reducers --------------------------------------------------------------------------------------

# A payload field as a length-1 character / number, NA when absent.
.sr_rt_chr <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) NA_character_ else as.character(x)
}

.sr_rt_num <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) NA_real_ else as.numeric(x)
}

sr_reducer_register("router.predicted", function(state, event) {
  p <- event$payload
  pr <- p$probabilities
  pv <- if (length(pr)) vapply(pr, function(v) as.numeric(v)[1L], numeric(1)) else numeric()
  rec <- list(key = .sr_rt_chr(p$key), node = .sr_rt_chr(p$node), table = .sr_rt_chr(p$table),
              column = .sr_rt_chr(p$column), backend = .sr_rt_chr(p$backend), model = .sr_rt_chr(p$model),
              input_hash = .sr_rt_chr(p$input_hash), payload_sha = .sr_rt_chr(p$payload_sha),
              choice = .sr_rt_chr(p$choice), probabilities = pv, confidence = .sr_rt_num(p$confidence),
              would_act = isTRUE(p$would_act), latency_ms = .sr_rt_num(p$latency_ms),
              error = .sr_rt_chr(p$error), at = event$ts)
  state$router$predictions[[rec$key]][[rec$backend]] <- rec
  state
})

sr_reducer_register("router.decided", function(state, event) {
  p <- event$payload
  state$router$decisions[[p$key]] <- list(
    key = .sr_rt_chr(p$key), node = .sr_rt_chr(p$node), table = .sr_rt_chr(p$table),
    column = .sr_rt_chr(p$column), choice = .sr_rt_chr(p$choice), resolver = .sr_rt_chr(p$resolver),
    input_hash = .sr_rt_chr(p$input_hash), reason = .sr_rt_chr(p$reason),
    override = isTRUE(p$override), suggested = .sr_rt_chr(p$suggested), at = event$ts)
  state
})

sr_reducer_register("router.vetoed", function(state, event) {
  key <- event$payload$key
  n <- state$router$vetoes[[key]]
  state$router$vetoes[[key]] <- (if (is.null(n)) 0L else as.integer(n)) + 1L
  state
})

sr_reducer_register("router.consent", function(state, event) {
  p <- event$payload
  state$router$consent[[as.character(p$session)]] <- isTRUE(p$granted)
  state
})

sr_reducer_register("router.jev_ack", function(state, event) {
  state$router$jev_ack <- list(reason = .sr_rt_chr(event$payload$reason), at = event$ts)
  state
})

# ---- helpers ---------------------------------------------------------------------------------------

# An instance's identity fields as a payload head.
.sr_rt_head <- function(key, node, table, column) {
  list(key = key, node = node, table = table, column = column)
}

# The latest successful prediction made under `hash`, or NULL. The latest by event time; ties go to the
# backend later in the escalation order.
.sr_router_latest_pred <- function(project, key, hash) {
  preds <- project$state$router$predictions[[key]]
  ok <- Filter(function(r) is.na(r$error) && !is.na(r$choice) && identical(r$input_hash, hash), preds)
  if (!length(ok)) return(NULL)
  at <- vapply(ok, function(r) .sr_rt_chr(r$at), "")
  rank <- match(names(ok), SR_ROUTER_ESCALATION)
  rank[is.na(rank)] <- 0L
  ok[[order(at, rank, decreasing = TRUE)[1L]]]
}

.sr_router_applies <- function(ctx) {
  nd <- ctx$node
  is.null(nd$applies) || isTRUE(nd$applies(ctx))
}

.sr_router_waiting <- function(ctx) {
  length(ctx$dep) > 0L && any(vapply(ctx$dep, function(d) is.na(d), logical(1)))
}

# Ask one backend and log one router.predicted. The request is for the criteria keys; the stored choice
# and probabilities are in option names. would_act is the abstain rule's verdict, and only when the
# option it would act on is the model's own choice.
.sr_router_predict <- function(project, ctx, key, hash, id, reg, rq) {
  nd <- ctx$node
  qn <- names(rq$request$questions)[1L]
  resp <- tryCatch(reg$ask(rq$request, ctx), error = function(e) {
    list(ok = FALSE, error = conditionMessage(e))
  })
  out <- list(choice = NA_character_, probabilities = list(), confidence = NA_real_, would_act = FALSE,
              error = NA_character_)
  if (!isTRUE(resp$ok)) {
    out$error <- .sr_rt_chr(resp$error)
    if (is.na(out$error)) out$error <- "the backend gave no answer"
  } else {
    parsed <- sr_sysone_parse(resp$response, rq$request)[[qn]]
    if (!isTRUE(parsed$ok)) {
      out$error <- parsed$error
    } else {
      keymap <- rq$keymap
      keys <- names(rq$request$questions[[qn]]$criteria)
      inv <- stats::setNames(names(keymap), unname(keymap))
      cells <- Filter(function(d) all(d %in% names(inv)), nd$danger)
      cells <- lapply(cells, function(d) unname(inv[d]))
      cost <- sr_cost_matrix(keys, cells)
      ab <- sr_abstain(parsed$probabilities, cost)
      out$choice <- unname(keymap[parsed$choice])
      out$probabilities <- as.list(stats::setNames(as.numeric(parsed$probabilities),
                                                   unname(keymap[names(parsed$probabilities)])))
      out$confidence <- parsed$confidence
      # would_act needs abstain's argmin to equal the model's own choice, not only a low expected cost.
      out$would_act <- isTRUE(ab$act) && identical(ab$choice, parsed$choice)
    }
  }
  payload <- c(.sr_rt_head(key, nd$id, ctx$table, ctx$column),
               list(backend = id, model = .sr_rt_chr(resp$model), input_hash = hash,
                    payload_sha = rq$sha, payload = sr_sysone_body(rq$request),
                    raw = .sr_rt_chr(resp$raw), choice = out$choice, probabilities = out$probabilities,
                    confidence = out$confidence, would_act = out$would_act,
                    latency_ms = .sr_rt_num(resp$latency_ms), error = out$error))
  sr_project_append(project, "router.predicted", payload)
}

# An error that carries the project as it stands, so the caller keeps the events appended before the
# failure. `class` is the condition class added in front of error/condition; `...` are extra fields.
.sr_router_signal <- function(class, message, project, ...) {
  stop(structure(class = c(class, "error", "condition"),
                 c(list(message = message, call = NULL, project = project), list(...))))
}

# ---- the run loop ----------------------------------------------------------------------------------

#' Run the router over every instance, in order. Rules decide first. Where no rule decides and the node
#' has a question, the backends named in `backends` are asked in escalation order (shadow mode: the
#' answer is logged and never applied) until one would act. `approve` goes to the backends as
#' `ctx$approve`. Instances with an undecided dependency, that do not apply, or that are decided and
#' current are skipped; a project whose SAP is locked is returned unchanged.
sr_router_run <- function(project, backends = character(), approve = character()) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  if (isTRUE(project$state$sap$locked)) return(project)
  inst <- .sr_router_instances(project)
  for (i in seq_len(nrow(inst))) {
    node <- sr_node_get(inst$node[i])
    tb <- inst$table[i]
    cl <- inst$column[i]
    key <- inst$key[i]
    ctx <- .sr_router_ctx(project, node, tb, cl)
    ctx$approve <- approve
    if (!.sr_router_applies(ctx) || .sr_router_waiting(ctx)) next
    if (!is.na(.sr_router_current(project, key))) next
    hash <- sr_router_input_hash(ctx)
    rule <- tryCatch(.sr_router_rule(ctx), error = function(e) {
      .sr_router_signal("sr_router_error", conditionMessage(e), project)
    })
    if (!is.null(rule)) {
      project <- sr_project_append(project, "router.decided", c(
        .sr_rt_head(key, node$id, tb, cl),
        list(choice = rule$choice, resolver = "rules", input_hash = hash, reason = rule$reason,
             override = FALSE, suggested = rule$choice)))
      next
    }
    rq <- sr_router_request(ctx)
    if (is.null(rq)) next
    for (id in intersect(SR_ROUTER_ESCALATION, backends)) {
      reg <- tryCatch(.sr_backend_get(id), error = function(e) NULL)
      if (is.null(reg) || !isTRUE(reg$available(project)$ok)) next
      prior <- project$state$router$predictions[[key]][[id]]
      if (!is.null(prior) && is.na(prior$error) && identical(prior$input_hash, hash)) {
        if (isTRUE(prior$would_act)) break
        next
      }
      project <- .sr_router_predict(project, ctx, key, hash, id, reg, rq)
      if (isTRUE(project$state$router$predictions[[key]][[id]]$would_act)) break
    }
  }
  project
}

# ---- decisions -------------------------------------------------------------------------------------

#' Record the user's decision for a node instance. Refused after the SAP lock. A choice that differs from
#' the rule's (or, without a rule, the latest model prediction) is an override and needs a reason. A veto
#' blocks the choice and is logged (router.vetoed) unless `override = TRUE` with a reason.
#' A vetoed attempt appends router.vetoed and signals class sr_router_vetoed; the condition's `$project`
#' is the project with the veto counted (and `$reason` the veto reason).
sr_router_decide <- function(project, node, choice, reason = NA_character_, table = NA_character_,
                             column = NA_character_, override = FALSE) {
  .sr_sap_open(project, "the SAP is locked: router decisions are frozen")
  nid <- if (inherits(node, "sr_node")) node$id else node
  key <- sr_router_key(nid, table, column)
  inst <- .sr_router_instances(project)
  if (!nid %in% sr_nodes() || !key %in% inst$key) stop("router: no instance '", key, "'", call. = FALSE)
  nd <- sr_node_get(nid)
  ctx <- .sr_router_ctx(project, nd, table, column)
  if (!.sr_router_applies(ctx)) stop("router: '", key, "' does not apply", call. = FALSE)
  und <- names(ctx$dep)[vapply(ctx$dep, function(d) is.na(d), logical(1))]
  if (length(und)) {
    stop("router: '", key, "' waits for an undecided dependency: ", paste(und, collapse = ", "),
         call. = FALSE)
  }
  opts <- .sr_node_options(nd, ctx)
  if (!is.character(choice) || length(choice) != 1L || is.na(choice) || !choice %in% opts) {
    stop("router: '", paste(format(choice), collapse = ","), "' is not an option of '", key,
         "'; options: ", paste(opts, collapse = ", "), call. = FALSE)
  }
  has_reason <- !is.null(reason) && length(reason) == 1L && !is.na(reason) && nzchar(trimws(reason))
  override <- isTRUE(override)
  veto <- if (is.null(nd$veto)) NULL else nd$veto(ctx, choice)
  if (!is.null(veto) && !is.na(veto) && nzchar(veto) && !override) {
    p2 <- sr_project_append(project, "router.vetoed", c(.sr_rt_head(key, nid, table, column),
                                                       list(choice = choice, reason = veto)))
    .sr_router_signal("sr_router_vetoed", paste0("router: '", choice, "' is vetoed for '", key, "': ", veto),
                      p2, reason = veto)
  }
  if (override && !has_reason) stop("router: an override needs a reason", call. = FALSE)
  hash <- sr_router_input_hash(ctx)
  rule <- .sr_router_rule(ctx)
  suggested <- if (!is.null(rule)) {
    rule$choice
  } else {
    pr <- .sr_router_latest_pred(project, key, hash)
    if (is.null(pr)) NA_character_ else pr$choice
  }
  differs <- !is.na(suggested) && !identical(suggested, choice)
  if (differs && !has_reason) {
    stop("router: '", choice, "' differs from the suggested '", suggested, "'; an override needs a reason",
         call. = FALSE)
  }
  sr_project_append(project, "router.decided", c(
    .sr_rt_head(key, nid, table, column),
    list(choice = choice, resolver = "user", input_hash = hash,
         reason = if (has_reason) trimws(reason) else NA_character_,
         override = override || differs, suggested = suggested)))
}

#' The current choice of an instance, or NA when it is undecided or stale.
sr_router_choice <- function(project, node, table = NA, column = NA) {
  nid <- if (inherits(node, "sr_node")) node$id else node
  .sr_router_current(project, sr_router_key(nid, table, column))
}

# ---- the decision table ----------------------------------------------------------------------------

#' The expert decision table, one row per applicable instance. `status`: "decided", "stale" (decided
#' under an older input hash; the stored choice is still shown), "pending", "waiting" (a dependency is
#' undecided) or "override_required" (pending, with SR_ROUTER_MAX_VETOES or more vetoes). The model
#' columns come from the latest successful prediction under the current input hash.
sr_router_table <- function(project) {
  inst <- .sr_router_instances(project)
  rows <- list()
  for (i in seq_len(nrow(inst))) {
    nd <- sr_node_get(inst$node[i])
    tb <- inst$table[i]
    cl <- inst$column[i]
    key <- inst$key[i]
    ctx <- .sr_router_ctx(project, nd, tb, cl)
    if (!.sr_router_applies(ctx)) next
    waiting <- .sr_router_waiting(ctx)
    hash <- sr_router_input_hash(ctx)
    dec <- project$state$router$decisions[[key]]
    vetoes <- project$state$router$vetoes[[key]]
    vetoes <- if (is.null(vetoes)) 0L else as.integer(vetoes)
    status <- if (!is.null(dec)) {
      if (!is.na(.sr_router_current(project, key))) "decided" else "stale"
    } else if (waiting) {
      "waiting"
    } else if (vetoes >= SR_ROUTER_MAX_VETOES) {
      "override_required"
    } else {
      "pending"
    }
    rule <- if (waiting) NULL else .sr_router_rule(ctx)
    pr <- .sr_router_latest_pred(project, key, hash)
    rows[[length(rows) + 1L]] <- list(
      key = key, node = nd$id, label = nd$label, table = tb, column = cl, status = status,
      choice = if (is.null(dec)) NA_character_ else dec$choice,
      resolver = if (is.null(dec)) NA_character_ else dec$resolver,
      override = if (is.null(dec)) NA else isTRUE(dec$override),
      reason = if (is.null(dec)) NA_character_ else dec$reason,
      suggested = if (!is.null(rule)) rule$choice else if (!is.null(pr)) pr$choice else NA_character_,
      backend = if (is.null(pr)) NA_character_ else pr$backend,
      model_choice = if (is.null(pr)) NA_character_ else pr$choice,
      confidence = if (is.null(pr)) NA_real_ else pr$confidence,
      would_act = if (is.null(pr)) NA else isTRUE(pr$would_act),
      vetoes = vetoes)
  }
  col <- function(f, type) vapply(rows, function(r) r[[f]], type)
  data.frame(key = col("key", ""), node = col("node", ""), label = col("label", ""),
             table = col("table", ""), column = col("column", ""), status = col("status", ""),
             choice = col("choice", ""), resolver = col("resolver", ""), override = col("override", NA),
             reason = col("reason", ""), suggested = col("suggested", ""), backend = col("backend", ""),
             model_choice = col("model_choice", ""), confidence = col("confidence", 0),
             would_act = col("would_act", NA), vetoes = col("vetoes", 0L), stringsAsFactors = FALSE)
}
