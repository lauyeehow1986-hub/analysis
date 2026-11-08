# router/gate.R - what the router adds to the SAP lock: a gate (the study design and purpose must be
# decided, current, and fit the primary analyses) and a hash part (the PICO and the current decisions go
# into the SAP hash, so the question and the router's calls are tamper-evident after the lock).

# The purpose of a route id, NA when the route is unknown.
.sr_gate_route_purpose <- function(route) {
  rt <- sr_routes()
  as.character(rt$purpose[match(route, rt$id)])
}

# The primary analyses, as a list of list(id, route, table, terms (the adjusted columns), purpose).
.sr_gate_primaries <- function(project) {
  an <- .sr_sap_analyses(project)
  if (length(an)) an <- an[order(names(an), method = "radix")]
  out <- list()
  for (id in names(an)) {
    a <- an[[id]]
    if (!identical(a$label, "primary")) next
    tv <- unlist(lapply(a$spec$terms, function(t) as.character(t$var)))
    out[[length(out) + 1L]] <- list(id = id, route = as.character(a$route), table = as.character(a$table),
                                    terms = unique(tv), purpose = .sr_gate_route_purpose(a$route))
  }
  out
}

# One project-scope decision as a gate row: block while undecided or stale, else ok with the choice.
.sr_gate_decided <- function(tab, node, item, fix) {
  r <- tab[tab$node == node, , drop = FALSE]
  st <- if (nrow(r)) r$status[1L] else "pending"
  if (identical(st, "decided")) return(list(item = item, status = "ok", detail = r$choice[1L]))
  if (identical(st, "stale")) {
    return(list(item = item, status = "block",
                detail = sprintf("the %s has changed inputs; decide it again (%s)", item, fix)))
  }
  list(item = item, status = "block", detail = sprintf("the %s is not decided (%s)", item, fix))
}

# The router's part of the SAP check: data.frame(item, status, detail), rows in the order study design,
# study purpose, purpose fits the analyses, primary route (only when a route is decided), covariate
# timing. block: design or purpose undecided or stale; a causal primary analysis adjusting for a column
# decided mediator or post_outcome. warn: a primary analysis whose route serves another purpose than the
# decided one; a decided primary route that no primary analysis uses; adjusted covariates with no
# current timing decision.
.sr_router_gate <- function(project) {
  tab <- sr_router_table(project)
  rows <- list()
  add <- function(item, status, detail) rows[[length(rows) + 1L]] <<- list(item = item, status = status, detail = detail)
  addr <- function(r) add(r$item, r$status, r$detail)
  addr(.sr_gate_decided(tab, "design", "study design", "sr_router_decide(p, 'design', ...)"))
  addr(.sr_gate_decided(tab, "purpose", "study purpose", "sr_router_decide(p, 'purpose', ...)"))
  purpose <- sr_router_choice(project, "purpose")
  prim <- .sr_gate_primaries(project)

  n0 <- length(rows)
  if (!is.na(purpose)) {
    for (a in prim) {
      if (!is.na(a$purpose) && !identical(a$purpose, purpose)) {
        add("purpose fits the analyses", "warn",
            sprintf("%s uses route %s, which serves the %s purpose; the decided purpose is %s",
                    a$id, a$route, a$purpose, purpose))
      }
    }
  }
  if (length(rows) == n0) add("purpose fits the analyses", "ok", "every primary analysis serves the decided purpose")

  pr <- tab[tab$node == "primary_route" & tab$status == "decided", , drop = FALSE]
  n1 <- length(rows)
  for (i in seq_len(nrow(pr))) {
    used <- vapply(prim, function(a) identical(a$table, pr$table[i]) && identical(a$route, pr$choice[i]), NA)
    if (!any(used)) {
      add("primary route", "warn",
          sprintf("table %s: the router decided route %s, but no primary analysis on it uses that route",
                  pr$table[i], pr$choice[i]))
    }
  }
  if (nrow(pr) && length(rows) == n1) {
    add("primary route", "ok", "every decided primary route is used by a primary analysis")
  }

  n2 <- length(rows)
  untimed <- 0L
  for (a in prim) {
    if (!identical(a$purpose, "explain-causal")) next
    for (v in a$terms) {
      ch <- sr_router_choice(project, "covariate_timing", a$table, v)
      if (is.na(ch)) {
        untimed <- untimed + 1L
      } else if (ch %in% c("mediator", "post_outcome")) {
        add("covariate timing", "block",
            sprintf("%s is a causal primary analysis adjusting for %s, decided %s", a$id, v, ch))
      }
    }
  }
  if (untimed) {
    add("covariate timing", "warn",
        sprintf("%d adjusted covariate(s) have no current timing decision", untimed))
  }
  if (length(rows) == n2) add("covariate timing", "ok", "no causal primary analysis adjusts for a mediator or post-outcome column")

  data.frame(item = vapply(rows, function(r) r$item, ""), status = vapply(rows, function(r) r$status, ""),
             detail = vapply(rows, function(r) r$detail, ""), stringsAsFactors = FALSE)
}

# The router's part of the SAP hash: list(pico = the eight fields, decisions = every current decision
# ordered by key as list(key, choice, resolver, override, reason, input_hash)), or NULL when there is no
# pico.set and no current decision (so a SAP that never used the router keeps its hash).
.sr_router_hash_part <- function(project) {
  q <- sr_pico(project)
  pico <- if (is.null(q)) NULL else q[SR_PICO_FIELDS]
  dec <- project$state$router$decisions %||% list()
  keys <- sort(as.character(names(dec)), method = "radix")
  keys <- keys[vapply(keys, function(k) !is.na(.sr_router_current(project, k)), NA)]
  if (is.null(pico) && !length(keys)) return(NULL)
  ds <- lapply(keys, function(k) {
    d <- dec[[k]]
    list(key = k, choice = d$choice, resolver = d$resolver, override = isTRUE(d$override),
         reason = d$reason, input_hash = d$input_hash)
  })
  list(pico = pico, decisions = unname(ds))
}
