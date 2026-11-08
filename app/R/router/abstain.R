# router/abstain.R - the expected-cost act/abstain rule.

SR_ROUTER_ASK_COST <- 0.1
SR_ROUTER_DANGER_COST <- 10

#' Cost matrix: rows = true option, columns = chosen option; 0 diagonal, 1 elsewhere. Each
#' `danger` element c(true, chosen) is set to SR_ROUTER_DANGER_COST.
sr_cost_matrix <- function(options, danger = list()) {
  m <- matrix(1, length(options), length(options), dimnames = list(options, options))
  diag(m) <- 0
  for (d in danger) {
    if (!is.character(d) || length(d) != 2L || !all(d %in% options)) {
      stop("sr_cost_matrix: danger cells must be c(true, chosen) of known options", call. = FALSE)
    }
    m[d[1L], d[2L]] <- SR_ROUTER_DANGER_COST
  }
  m
}

#' Pick the minimum-expected-cost option; act only when that cost is below `ask_cost`.
sr_abstain <- function(probabilities, cost, ask_cost = SR_ROUTER_ASK_COST) {
  opts <- colnames(cost)
  if (!setequal(names(probabilities), rownames(cost)) || !setequal(opts, rownames(cost))) {
    stop("sr_abstain: probabilities and cost matrix must name the same options", call. = FALSE)
  }
  p <- probabilities[rownames(cost)]
  ec <- vapply(opts, function(a) sum(p * cost[, a]), numeric(1))
  choice <- opts[which.min(ec)]
  list(act = unname(ec[choice] < ask_cost), choice = choice, expected_cost = ec)
}
