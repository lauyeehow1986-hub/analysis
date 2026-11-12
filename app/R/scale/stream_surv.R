# scale/stream_surv.R - the streamed Kaplan-Meier counts, the near-tie map and the RMST port of the
# streaming survival engine (P1e.2, plan Task 2). They reproduce .sr_h_km (routes/cox.R) and .sr_h_rmst
# (routes/rmst.R), which are survfit and the rmean branch of survival:::survmean.
#
# Every .h_st_* function here is emitted into the analysis code with .sr_inline(), so it is
# self-contained: base and stats only, ASCII text, no StatRouter call.
#
#   timefix  .h_st_timefix_map(times) / .h_st_timefix_apply(t, map): the aeqSurv near-tie merge (S3)
#   counts   .h_st_km_add(st, time, event, group) keeps rowsum(cbind(d, c)) keyed by (group, raw time);
#            .h_st_km_counts(st, map) collapses, maps the time and re-sums, sorted by group then time
#   finish   .h_st_km_finish(counts, levels, trange) gives list(km, risk) like .sr_h_km
#   rmst     .h_st_rmst(counts1, tau) gives c(rmst, se) like .sr_h_rmst

#' The near-tie map of the sorted unique finite raw times (aeqSurv, S3): list(from, to) for the times that
#' move down onto the first time of their chain; both are empty when none move.
.h_st_timefix_map <- function(times) {
  y <- sort(unique(times[is.finite(times)]))
  if (length(y) < 2L) return(list(from = numeric(), to = numeric()))
  tol <- sqrt(.Machine$double.eps)
  dy <- diff(y)
  tied <- dy <= tol | dy / mean(abs(y)) <= tol
  tied[is.na(tied)] <- FALSE
  cuts <- y[c(TRUE, !tied)]
  to <- cuts[findInterval(y, cuts)]
  mv <- to != y
  list(from = y[mv], to = to[mv])
}

#' t with each map$from value replaced by its map$to.
.h_st_timefix_apply <- function(t, map) {
  if (is.null(map) || !length(map$from)) return(t)
  i <- match(t, map$from)
  w <- !is.na(i)
  t[w] <- map$to[i[w]]
  t
}

#' Add one chunk of the Kaplan-Meier counts. st is NULL at the start, then list(parts, n): the parts are
#' data.frame(g, t, d, c) with g as character, keyed by (group, raw time); every 16 chunks the parts are
#' collapsed into one by rowsum.
.h_st_km_add <- function(st, time, event, group) {
  merge1 <- function(g, t, m) {
    o <- order(g, t)
    g <- g[o]; t <- t[o]; m <- m[o, , drop = FALSE]
    k <- length(g)
    first <- if (k) c(TRUE, g[-1L] != g[-k] | t[-1L] != t[-k]) else logical()
    s <- rowsum(m, cumsum(first), reorder = FALSE)
    data.frame(g = g[first], t = t[first], d = as.numeric(s[, 1L]), c = as.numeric(s[, 2L]),
               stringsAsFactors = FALSE)
  }
  if (is.null(st)) st <- list(parts = list(), n = 0L)
  ev <- as.numeric(event)
  st$parts[[length(st$parts) + 1L]] <- merge1(as.character(group), as.numeric(time), cbind(d = ev, c = 1 - ev))
  st$n <- st$n + 1L
  if (st$n %% 16L == 0L && length(st$parts) > 1L) {
    p <- do.call(rbind, st$parts)
    st$parts <- list(merge1(p$g, p$t, cbind(p$d, p$c)))
  }
  st
}

#' The counts of the state: data.frame(g, t, d, c) with the parts collapsed, the near-tie map applied to t
#' (NULL: none), re-summed by rowsum and sorted by g, then t.
.h_st_km_counts <- function(st, map = NULL) {
  merge1 <- function(g, t, m) {
    o <- order(g, t)
    g <- g[o]; t <- t[o]; m <- m[o, , drop = FALSE]
    k <- length(g)
    first <- if (k) c(TRUE, g[-1L] != g[-k] | t[-1L] != t[-k]) else logical()
    s <- rowsum(m, cumsum(first), reorder = FALSE)
    data.frame(g = g[first], t = t[first], d = as.numeric(s[, 1L]), c = as.numeric(s[, 2L]),
               stringsAsFactors = FALSE)
  }
  if (is.null(st) || !length(st$parts)) {
    return(data.frame(g = character(), t = numeric(), d = numeric(), c = numeric(),
                      stringsAsFactors = FALSE))
  }
  p <- do.call(rbind, st$parts)
  if (!is.null(map) && length(map$from)) {
    i <- match(p$t, map$from)
    w <- !is.na(i)
    p$t[w] <- map$to[i[w]]
  }
  merge1(p$g, p$t, cbind(p$d, p$c))
}

#' list(km, risk) of the counts, like .sr_h_km: km = data.frame(strata, time, surv, lower, upper, n.risk,
#' n.event, n.censor) (survfit, conf.type "log": Greenwood, lower/upper = exp(log(surv) -/+ z * std.err),
#' upper capped at 1, NA once surv is 0); risk = data.frame(strata, time, n.risk) at the pretty() times of
#' trange (the raw time range) that do not exceed its maximum. levels is the group order; a level without
#' counts is dropped.
.h_st_km_finish <- function(counts, levels, trange) {
  lv <- as.character(levels)
  lv <- lv[lv %in% counts$g]
  z <- stats::qnorm(0.975)
  tt <- pretty(trange, n = 6)
  tt <- tt[tt <= max(trange)]
  km <- vector("list", length(lv))
  rk <- vector("list", length(lv))
  for (j in seq_along(lv)) {
    cj <- counts[counts$g == lv[j], , drop = FALSE]
    cj <- cj[order(cj$t), , drop = FALSE]
    m <- cj$d + cj$c
    n <- sum(m)
    nr <- n - c(0, cumsum(m)[-length(m)])
    surv <- cumprod(1 - cj$d / nr)
    se <- sqrt(cumsum(cj$d / (nr * (nr - cj$d))))
    lower <- exp(log(surv) - z * se)
    upper <- pmin(1, exp(log(surv) + z * se))
    zero <- !is.finite(se)
    lower[zero] <- NA_real_
    upper[zero] <- NA_real_
    km[[j]] <- data.frame(strata = rep(lv[j], length(m)), time = cj$t, surv = surv, lower = lower,
                          upper = upper, n.risk = nr, n.event = cj$d, n.censor = cj$c,
                          stringsAsFactors = FALSE)
    rj <- vapply(tt, function(x) sum(m[cj$t >= x]), numeric(1))
    rk[[j]] <- data.frame(strata = rep(lv[j], length(tt)), time = as.numeric(tt), n.risk = rj,
                          stringsAsFactors = FALSE)
  }
  list(km = do.call(rbind, km), risk = do.call(rbind, rk))
}

#' c(rmst, se) up to tau of one group, from its counts (g, t, d, c) already mapped by the group's own
#' near-tie map: the rmean branch of survival:::survmean (end.time = tau, start.time = min(0, time)).
.h_st_rmst <- function(counts1, tau) {
  p <- counts1[order(counts1$t), , drop = FALSE]
  first <- c(TRUE, p$t[-1L] != p$t[-nrow(p)])
  id <- cumsum(first)
  time <- p$t[first]
  d <- as.numeric(rowsum(p$d, id, reorder = FALSE)[, 1L])
  cen <- as.numeric(rowsum(p$c, id, reorder = FALSE)[, 1L])
  m <- d + cen
  n.risk <- sum(m) - c(0, cumsum(m)[-length(m)])
  surv <- cumprod(1 - d / n.risk)
  start.time <- min(0, time)
  hh <- ifelse((n.risk - d) == 0, 0, d / (n.risk * (n.risk - d)))
  keep <- which(time <= tau)
  if (length(keep) == 0L) {
    temptime <- tau
    tempsurv <- 1
    hh <- 0
  } else {
    temptime <- c(time[keep], tau)
    tempsurv <- c(surv[keep], surv[max(keep)])
    hh <- c(hh[keep], 0)
  }
  n <- length(temptime)
  delta <- diff(c(start.time, temptime))
  rectangles <- delta * c(1, tempsurv[-n])
  varmean <- sum(cumsum(rev(rectangles[-1L]))^2 * rev(hh)[-1L])
  c(rmst = sum(rectangles) + start.time, se = sqrt(varmean))
}
