# resample/boot.R - the bootstrap layer (layers$resampling = "bootstrap") of the model routes lm,
# logistic, modpois, cox and rmst (and their describe twins).
#
# spec$boot = list(B = 2000, type = "percentile" | "bca" | "normal"); both fields are optional (B a
# whole number of at least 20). With layers$clustered = TRUE the spec must name spec$boot_type:
# "cluster" resamples whole clusters (spec$cluster) and the BCa jackknife leaves out whole clusters;
# "pair" and "replicate_weights" are refused at compile, naming them. Emitted code never calls the
# boot package; the intervals below follow boot::boot.ci (boot 1.3) and are checked against it in
# the tests.
#
# Plain helpers, inlined into the emitted code (ruling R4: pure functions of their arguments; a
# helper that needs another one takes it as an argument):
#   .sr_h_boot          the B x k replicate matrix: rows (or whole clusters) drawn with replacement;
#                       chunk j of `chunk` replicates runs under the j-th parallel::nextRNGStream of
#                       the L'Ecuyer-CMRG stream current at entry, which is restored on exit. A
#                       replicate that fails or is not finite is a row of NA (counted); fewer than 2
#                       finite replicates stop.
#   .sr_h_boot_ci       percentile, bias-corrected normal and BCa intervals of one statistic, with the
#                       Monte Carlo error of each endpoint (10 consecutive batches of the replicates:
#                       sd of the batch endpoints / sqrt(10)). BCa falls back to the percentile
#                       interval, with the reason, when z0 or the acceleration is not finite or an
#                       adjusted percentile falls outside the replicates (index outside 1..B).
#   .sr_h_boot_jack     the jackknife values for the BCa acceleration: leave out one unit (row or
#                       cluster) when there are at most 1000, else one of 1000 seeded groups.
#   .sr_h_boot_summary  one row per statistic: estimate, bootstrap mean and SE, interval, MCSE.
#   .sr_h_boot_coef_stat / .sr_h_boot_rmst_stat  the replicate statistic of a route value: the
#                       primary coefficients (log scale for OR / RR / HR) and the marginal RD / log RR
#                       of g-computation (re-run in every replicate), or the RMST differences.
#   .sr_h_boot_within   impute_then_boot: the bootstrap inside one completed dataset.
#
# The code of a route under the bootstrap (.sr_boot_compile) is the route's own complete-case (or
# MI) code, verbatim, wrapped as .h_fit(sr_data) after the shared setup (so factor levels and knots
# are fixed from the observed data once): it is run on the data for the estimate and on every
# replicate. The entry stream of .h_boot is the one current after the run preamble (ruling R27h).
# The covariate and descriptive rows keep their model-based intervals; only the primary rows take
# the bootstrap interval (ci_method "boot_<type>", mcse = the larger endpoint MCSE, no p).
#
# With multiple imputation spec$choices$mi_x_boot is required (ruling R27f):
#   boot_then_impute  every bootstrap sample is imputed (spec$mi$m times; "auto" uses 5) and pooled;
#                     the estimate is the mean of the B pooled estimates, the interval their
#                     percentile interval (another type is refused);
#   impute_then_boot  every completed dataset is bootstrapped (B replicates); the bootstrap variance
#                     replaces the model variance of the primary terms in Rubin's rules
#                     (ci_method "rubin_bootvar").

.sr_boot_types <- c("percentile", "bca", "normal")

# Whether the spec asks for the bootstrap.
.sr_boot_on <- function(spec) identical(spec[["layers"]][["resampling"]], "bootstrap")

#' The validated bootstrap settings of a spec; stops naming the field or the combination.
#' @return list(B, type, boot_type ("unit" or "cluster"), cluster, choice, m_inner, m_auto)
.sr_boot_settings <- function(spec, id) {
  bt <- spec[["boot"]]
  if (!is.null(bt) && !is.list(bt)) {
    stop("spec$boot must be a list(B, type) (the bootstrap replicates and the interval type)", call. = FALSE)
  }
  B <- bt[["B"]]
  if (is.null(B)) B <- 2000
  if (!(is.numeric(B) && length(B) == 1L && !is.na(B) && is.finite(B) && B == round(B) && B >= 20)) {
    stop("spec$boot$B must be a whole number of at least 20 (the bootstrap replicates; default 2000)", call. = FALSE)
  }
  type <- bt[["type"]]
  if (is.null(type)) type <- "percentile"
  if (!(is.character(type) && length(type) == 1L && !is.na(type) && type %in% .sr_boot_types)) {
    stop("spec$boot$type must be one of ", paste(.sr_boot_types, collapse = ", "),
         " (the bootstrap interval; default percentile)", call. = FALSE)
  }
  bty <- spec[["boot_type"]]
  clustered <- isTRUE(spec[["layers"]][["clustered"]])
  if (!is.null(bty)) {
    if (!(is.character(bty) && length(bty) == 1L && !is.na(bty))) {
      stop("spec$boot_type must be one string (\"cluster\")", call. = FALSE)
    }
    if (bty %in% c("pair", "replicate_weights")) {
      stop(id, "@1 does not implement boot_type = \"", bty, "\" (only \"cluster\": resample whole clusters)",
           call. = FALSE)
    }
    if (!identical(bty, "cluster")) {
      stop("spec$boot_type must be \"cluster\" (resample whole clusters), not \"", bty, "\"", call. = FALSE)
    }
  }
  if (clustered && is.null(bty)) {
    stop(id, "@1: the bootstrap of clustered data (layers$clustered = TRUE) needs spec$boot_type = \"cluster\" ",
         "(resample whole clusters)", call. = FALSE)
  }
  cl <- spec[["cluster"]]
  if (identical(bty, "cluster")) {
    if (!clustered) {
      stop("boot_type = \"cluster\" needs layers$clustered = TRUE (and the cluster column in spec$cluster)",
           call. = FALSE)
    }
    if (!(is.character(cl) && length(cl) == 1L && !is.na(cl) && nzchar(cl))) {
      stop("boot_type = \"cluster\" needs the cluster column (spec$cluster)", call. = FALSE)
    }
  }
  choice <- NA_character_
  m_inner <- NA_integer_
  m_auto <- FALSE
  if (.sr_mi_on(spec)) {
    ch <- .sr_choice_value(spec, "mi_x_boot")
    opts <- c("boot_then_impute", "impute_then_boot")
    if (!(is.character(ch) && length(ch) == 1L && !is.na(ch) && ch %in% opts)) {
      stop(id, "@1: multiple imputation with the bootstrap needs spec$choices$mi_x_boot = \"boot_then_impute\" ",
           "or \"impute_then_boot\"", call. = FALSE)
    }
    choice <- ch
    if (identical(ch, "boot_then_impute")) {
      if (!identical(type, "percentile")) {
        stop(id, "@1: boot_then_impute gives a percentile interval only (spec$boot$type = \"percentile\"), not ",
             type, call. = FALSE)
      }
      m <- spec[["mi"]][["m"]]
      m_auto <- identical(m, "auto")
      m_inner <- if (is.numeric(m) && length(m) == 1L && !is.na(m)) as.integer(m) else 5L
    } else {
      type <- NA_character_
    }
  }
  list(B = as.integer(B), type = type, boot_type = if (identical(bty, "cluster")) "cluster" else "unit",
       cluster = if (identical(bty, "cluster")) cl else NULL, choice = choice, m_inner = m_inner,
       m_auto = m_auto)
}

# ---- helpers inlined into the emitted code ------------------------------------------------------

#' The bootstrap replicate matrix.
#' @param data a data frame; rows are resampled (or whole clusters, see `cluster`)
#' @param stat function(resampled data frame) -> named numeric vector of k statistics
#' @param B the number of replicates
#' @param cluster NULL (resample rows) or a vector of cluster ids, one per row of `data`
#' @param chunk replicates per RNG stream: chunk j runs under the j-th parallel::nextRNGStream of the
#'   stream current at entry (L'Ecuyer-CMRG); the caller's stream is restored on exit
#' @param keys the names of the statistics (NULL: the names of the first successful replicate)
#' @return a B x k matrix (colnames = keys); a failed or non-finite replicate is a row of NA; the
#'   attribute "first_error" is the message of the first failure (NA when none failed)
.sr_h_boot <- function(data, stat, B, cluster = NULL, chunk = 100L, keys = NULL) {
  data <- as.data.frame(data)
  B <- as.integer(B)
  chunk <- as.integer(chunk)
  if (length(B) != 1L || is.na(B) || B < 1L) stop("B must be one whole number of at least 1")
  if (length(chunk) != 1L || is.na(chunk) || chunk < 1L) stop("chunk must be one whole number of at least 1")
  genv <- globalenv()
  s0 <- genv[[".Random.seed"]]
  if (is.null(s0) || s0[1L] %% 100L != 7L) {
    stop("the bootstrap needs the L'Ecuyer-CMRG generator of the run preamble (RNGkind(\"L'Ecuyer-CMRG\"))")
  }
  on.exit(genv[[".Random.seed"]] <- s0, add = TRUE)
  n <- nrow(data)
  if (n < 1L) stop("the bootstrap needs at least one row")
  grp <- NULL
  if (!is.null(cluster)) {
    if (length(cluster) != n) stop("cluster must have one value per row of the data")
    grp <- split(seq_len(n), match(cluster, unique(cluster)))
  }
  G <- if (is.null(grp)) n else length(grp)
  res <- vector("list", B)
  s <- s0
  b <- 0L
  while (b < B) {
    s <- parallel::nextRNGStream(s)
    genv[[".Random.seed"]] <- s
    for (r in seq_len(min(chunk, B - b))) {
      idx <- if (is.null(grp)) {
        sample.int(n, n, replace = TRUE)
      } else {
        unlist(grp[sample.int(G, G, replace = TRUE)], use.names = FALSE)
      }
      b <- b + 1L
      res[[b]] <- tryCatch(suppressWarnings(stat(data[idx, , drop = FALSE])), error = function(e) e)
    }
  }
  first_error <- NA_character_
  for (v in res) {
    if (inherits(v, "error")) {
      first_error <- conditionMessage(v)
      break
    }
  }
  named <- TRUE
  if (is.null(keys)) {
    for (v in res) {
      if (!inherits(v, "error") && is.numeric(v) && length(v)) {
        named <- !is.null(names(v))
        keys <- if (named) names(v) else paste0("v", seq_along(v))
        break
      }
    }
  }
  fail <- function() {
    stop("fewer than 2 of the ", B, " bootstrap replicates gave a finite statistic",
         if (!is.na(first_error)) paste0("; first failure: ", first_error) else "")
  }
  if (is.null(keys) || !length(keys)) fail()
  k <- length(keys)
  out <- matrix(NA_real_, B, k, dimnames = list(NULL, keys))
  for (i in seq_len(B)) {
    v <- res[[i]]
    if (inherits(v, "error") || !is.numeric(v) || length(v) != k) next
    x <- if (named) {
      if (is.null(names(v)) || !all(keys %in% names(v))) next
      as.numeric(v[keys])
    } else {
      as.numeric(v)
    }
    if (all(is.finite(x))) out[i, ] <- x
  }
  if (sum(stats::complete.cases(out)) < 2L) fail()
  attr(out, "first_error") <- first_error
  out
}

#' The bootstrap interval of one statistic.
#' @param t0 the estimate on the data
#' @param t the replicates (non-finite ones are dropped)
#' @param type "percentile", "bca" or "normal" (bias-corrected: 2 t0 - mean(t) -/+ z sd(t))
#' @param jack the jackknife values of the statistic (BCa only; the acceleration is
#'   sum(d^3) / (6 sum(d^2)^1.5) with d = mean(jack) - jack)
#' @param level the confidence level
#' @return list(lo, hi, type_used, fallback_reason, mcse_lo, mcse_hi): type_used is "percentile" and
#'   fallback_reason the reason when BCa fell back (NA otherwise); the MCSE of an endpoint is the sd of
#'   that endpoint over 10 consecutive batches of the replicates / sqrt(10) (NA for fewer than 20)
.sr_h_boot_ci <- function(t0, t, type = c("percentile", "bca", "normal"), jack = NULL, level = 0.95) {
  type <- match.arg(type)
  t <- t[is.finite(t)]
  R <- length(t)
  if (R < 2L) stop("the bootstrap interval needs at least 2 finite replicates")
  if (!(is.numeric(t0) && length(t0) == 1L && is.finite(t0))) stop("t0 must be one finite number")
  # the interpolated order statistic of boot:::norm.inter, clamped to the extreme replicates
  inter <- function(x, alpha) {
    x <- sort(x)
    m <- length(x)
    vapply(alpha, function(a) {
      rk <- (m + 1) * a
      k <- trunc(rk)
      if (k <= 0) return(x[1L])
      if (k >= m) return(x[m])
      if (k == rk) return(x[k])
      x[k] + (stats::qnorm(a) - stats::qnorm(k / (m + 1))) /
        (stats::qnorm((k + 1) / (m + 1)) - stats::qnorm(k / (m + 1))) * (x[k + 1L] - x[k])
    }, numeric(1))
  }
  alpha <- (1 + c(-level, level)) / 2
  used <- type
  reason <- NA_character_
  adj <- alpha
  if (type == "bca") {
    z0 <- stats::qnorm(sum(t < t0) / R)
    d <- if (is.null(jack)) numeric() else mean(jack) - jack
    a <- sum(d^3) / (6 * sum(d^2)^1.5)
    if (!is.finite(z0)) {
      reason <- sprintf("bias correction z0 is not finite (%d of %d replicates below the estimate)",
                        as.integer(sum(t < t0)), R)
    } else if (!length(d) || !is.finite(a)) {
      reason <- "acceleration a is not finite (the jackknife values are constant or missing)"
    } else {
      za <- stats::qnorm(alpha)
      adj <- stats::pnorm(z0 + (z0 + za) / (1 - a * (z0 + za)))
      rk <- (R + 1) * adj
      if (any(!is.finite(rk)) || any(rk < 1) || any(rk > R)) {
        reason <- sprintf("adjusted percentile index outside 1..B (%s with B = %d finite replicates)",
                          paste(format(round(rk, 2)), collapse = ", "), R)
      }
    }
    if (!is.na(reason)) {
      used <- "percentile"
      adj <- alpha
    }
  }
  ends <- function(x) {
    if (used == "normal") {
      half <- sqrt(stats::var(x)) * stats::qnorm((1 + level) / 2)
      c(t0 - (mean(x) - t0) - half, t0 - (mean(x) - t0) + half)
    } else {
      inter(x, adj)
    }
  }
  e <- ends(t)
  mc <- c(NA_real_, NA_real_)
  if (R >= 20L) {
    g <- ((seq_len(R) - 1L) * 10L) %/% R + 1L
    eb <- vapply(1:10, function(j) ends(t[g == j]), numeric(2))
    mc <- apply(eb, 1L, stats::sd) / sqrt(10)
  }
  list(lo = e[1L], hi = e[2L], type_used = used, fallback_reason = reason, mcse_lo = mc[1L], mcse_hi = mc[2L])
}

#' The jackknife values of `stat` for the BCa acceleration.
#' @param data,stat,cluster,keys as for .sr_h_boot
#' @param max_groups above this many units (rows, or clusters) the units are split into this many
#'   groups at random (the current RNG stream), and one group is left out at a time
#' @return list(values (G x k, a failed fit is a row of NA), units, groups, grouped, over ("units",
#'   "clusters" or "groups"))
.sr_h_boot_jack <- function(data, stat, cluster = NULL, max_groups = 1000L, keys = NULL) {
  data <- as.data.frame(data)
  n <- nrow(data)
  if (!is.null(cluster) && length(cluster) != n) stop("cluster must have one value per row of the data")
  uid <- if (is.null(cluster)) seq_len(n) else match(cluster, unique(cluster))
  U <- if (n) max(uid) else 0L
  if (U < 2L) stop("the jackknife needs at least two units")
  grouped <- U > max_groups
  G <- as.integer(if (grouped) max_groups else U)
  gid <- if (grouped) sample(rep_len(seq_len(G), U))[uid] else uid
  vals <- vector("list", G)
  for (g in seq_len(G)) {
    vals[[g]] <- tryCatch(suppressWarnings(stat(data[gid != g, , drop = FALSE])), error = function(e) NULL)
  }
  named <- TRUE
  if (is.null(keys)) {
    for (v in vals) {
      if (is.numeric(v) && length(v)) {
        named <- !is.null(names(v))
        keys <- if (named) names(v) else paste0("v", seq_along(v))
        break
      }
    }
  }
  if (is.null(keys)) keys <- "v1"
  out <- matrix(NA_real_, G, length(keys), dimnames = list(NULL, keys))
  for (g in seq_len(G)) {
    v <- vals[[g]]
    if (!is.numeric(v) || length(v) != length(keys)) next
    if (named) {
      if (is.null(names(v)) || !all(keys %in% names(v))) next
      out[g, ] <- as.numeric(v[keys])
    } else {
      out[g, ] <- as.numeric(v)
    }
  }
  list(values = out, units = as.integer(U), groups = G, grouped = grouped,
       over = if (grouped) "groups" else if (is.null(cluster)) "units" else "clusters")
}

#' One row per statistic (column of `t`): the estimate t0, the bootstrap mean and SE, the interval
#' of `ci_fun` (.h_boot_ci), the type used, the fallback reason and the endpoint MCSEs.
.sr_h_boot_summary <- function(t0, t, type, jack = NULL, ci_fun) {
  keys <- colnames(t)
  rows <- lapply(seq_along(keys), function(j) {
    x <- t[, j]
    x <- x[is.finite(x)]
    e0 <- unname(t0[[keys[j]]])
    ci <- if (is.finite(e0)) {
      ci_fun(e0, x, type, jack = if (is.null(jack)) NULL else jack[, j])
    } else {
      list(lo = NA_real_, hi = NA_real_, type_used = type, fallback_reason = "the estimate is not finite",
           mcse_lo = NA_real_, mcse_hi = NA_real_)
    }
    data.frame(key = keys[j], t0 = e0, mean = mean(x), se = stats::sd(x), lo = ci$lo, hi = ci$hi,
               type_used = ci$type_used, fallback_reason = ci$fallback_reason, mcse_lo = ci$mcse_lo,
               mcse_hi = ci$mcse_hi, mcse = max(ci$mcse_lo, ci$mcse_hi), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' The replicate statistic of an lm / glm / cox value: the coefficients `pn` (their model scale: log
#' for OR / RR / HR) and, for each level `ml` of the marginal block, the risk or mean difference
#' (mrd<j>) and, when `ratio`, the log risk ratio (mlrr<j>). A missing coefficient or level is NA.
.sr_h_boot_coef_stat <- function(v, pn, ml, ratio = FALSE) {
  cf <- v$coef
  out <- stats::setNames(as.numeric(cf$est[match(pn, cf$term)]), pn)
  if (length(ml)) {
    rows <- v$marginal$rows
    j <- if (is.null(rows)) rep(NA_integer_, length(ml)) else match(ml, rows$level)
    pick <- function(col) if (is.null(rows)) rep(NA_real_, length(ml)) else as.numeric(rows[[col]][j])
    out <- c(out, stats::setNames(pick("rd"), paste0("mrd", seq_along(ml))))
    if (isTRUE(ratio)) out <- c(out, stats::setNames(pick("log_rr"), paste0("mlrr", seq_along(ml))))
  }
  out
}

#' The replicate statistic of an rmst value: the RMST difference of each level `lv[-1]` against the
#' reference lv[1] (keys d<j>, j the position of the level); a missing level is NA.
.sr_h_boot_rmst_stat <- function(v, lv) {
  d <- if (!is.null(v$diffs)) {
    as.numeric(v$diffs$est[match(lv[-1L], v$diffs$level)])
  } else {
    e <- as.numeric(v$groups$rmst[match(lv, v$groups$level)])
    e[-1L] - e[1L]
  }
  stats::setNames(d, paste0("d", seq_along(lv)[-1L]))
}

#' impute_then_boot: fit one completed dataset `ci` with fit1(ci, rows) and bootstrap it (B replicates
#' of its rows, `boot` = .h_boot); the bootstrap covariance replaces the model covariance of the
#' primary coefficients (`prim`, NULL for all) and the bootstrap SD the SE of the primary pooled
#' scalars, so Rubin's rules pool the bootstrap variance. The failed / finite replicate counts are
#' added to `num` (.boot_failed, .boot_finite). The RNG stream then moves to its next substream, so
#' every completed dataset is bootstrapped under a stream of its own.
.sr_h_boot_within <- function(ci, fit1, B, prim, boot) {
  genv <- globalenv()
  s <- genv[[".Random.seed"]]
  f <- fit1(ci)
  pn <- if (is.null(prim)) names(f$est) else intersect(prim, names(f$est))
  sk <- if (is.null(f$scal) || !nrow(f$scal)) character() else as.character(f$scal$key[as.logical(f$scal$prim)])
  stat <- function(d) {
    g <- fit1(ci, d$.i)
    c(g$est[pn], stats::setNames(as.numeric(g$scal$est[match(sk, g$scal$key)]), sk))
  }
  tm <- boot(data.frame(.i = seq_len(nrow(ci))), stat, B, NULL, 100L, c(pn, sk))
  ok <- stats::complete.cases(tm)
  tt <- tm[ok, , drop = FALSE]
  if (length(pn)) f$vcov[pn, pn] <- stats::cov(tt[, pn, drop = FALSE])
  if (length(sk)) f$scal$se[match(sk, f$scal$key)] <- apply(tt[, sk, drop = FALSE], 2L, stats::sd)
  f$num <- c(f$num, .boot_failed = sum(!ok), .boot_finite = sum(ok))
  genv[[".Random.seed"]] <- parallel::nextRNGSubStream(s)
  f
}

# ---- compile ------------------------------------------------------------------------------------

# The expression (emitted code) of the primary coefficient names: the exposure term t1, or one t1f<level>
# per non-reference level of a factor exposure.
.sr_boot_prim_expr <- function(spec) {
  plan <- .sr_term_plan(spec)
  if (!length(plan) || !identical(plan[[1L]]$role, "exposure")) {
    "NULL"
  } else if (identical(plan[[1L]]$type, "factor")) {
    "paste0(\"t1f\", .sr_fixed$levels[[\"t1\"]][-1L])"
  } else {
    "\"t1\""
  }
}

#' The emitted code of a route under the bootstrap.
#' @param spec the analysis spec (layers$resampling = "bootstrap")
#' @param compile_fn the route's compile function (called on the spec with resampling = "none")
#' @param id the route id (messages)
#' @param kind "lm", "glm", "cox" or "rmst": the statistic and the setup of the code
#' @return one string of R code
.sr_boot_compile <- function(spec, compile_fn, id, kind = c("lm", "glm", "cox", "rmst")) {
  kind <- match.arg(kind)
  bs <- .sr_boot_settings(spec, id)
  inner <- spec
  inner$layers$resampling <- "none"
  bti <- identical(bs$choice, "boot_then_impute")
  itb <- identical(bs$choice, "impute_then_boot")
  if (bti) inner$mi$m <- bs$m_inner
  if (itb) inner[[".sr_boot_within"]] <- list(B = bs$B)
  body <- compile_fn(inner)
  setup <- if (kind == "rmst") {
    if (.sr_mi_on(inner)) sr_terms_code(.sr_rmst_isp(inner))$setup else ""
  } else {
    sr_terms_code(inner)$setup
  }
  if (nzchar(setup)) {
    pre <- paste0(setup, "\n")
    if (!startsWith(body, pre)) stop("internal: the code of ", id, "@1 does not start with its setup", call. = FALSE)
    rest <- substring(body, nchar(pre) + 1L)
  } else {
    rest <- body
  }
  fit <- c(".h_fit <- function(sr_data) {", rest, "}")
  info <- function(ci, failed, finite, first, jack, n_units) {
    paste0("c(.b_val, list(boot = list(B = ", .sr_lit(bs$B), ", type = ", .sr_lit(bs$type),
           ", boot_type = ", .sr_lit(bs$boot_type), ", choice = ", .sr_lit(bs$choice),
           ", m_inner = ", .sr_lit(bs$m_inner), ", m_auto = ", .sr_lit(bs$m_auto), ", n_units = ", n_units,
           ", ci = ", ci, ", n_failed = ", failed, ", n_finite = ", finite, ", first_error = ", first,
           ", jack = ", jack, ")))")
  }
  if (itb) {
    code <- c(
      if (nzchar(setup)) setup,
      .sr_inline(.sr_h_boot, ".h_boot"),
      .sr_inline(.sr_h_boot_within, ".h_boot_within"),
      fit,
      ".b_val <- .h_fit(sr_data)",
      info("NULL", ".b_val$mi$boot[[\"failed\"]]", ".b_val$mi$boot[[\"finite\"]]", "NA_character_", "NULL",
           "nrow(sr_data)"))
    return(paste(code, collapse = "\n"))
  }
  bca <- identical(bs$type, "bca")
  cl <- bs$cluster
  stat <- if (kind == "rmst") {
    c(".b_lv <- as.character(.b_val$groups$level)",
      ".b_stat <- function(d) .h_boot_rmst_stat(.h_fit(d), .b_lv)",
      ".b_t0 <- .h_boot_rmst_stat(.b_val, .b_lv)")
  } else {
    ratio <- .sr_lit(kind == "glm")
    c(sprintf(".b_pn <- %s", .sr_boot_prim_expr(inner)),
      ".b_pn <- .b_pn[.b_pn %in% .b_val$coef$term]",
      ".b_ml <- .b_val$marginal$rows$level",
      sprintf(".b_stat <- function(d) .h_boot_coef_stat(.h_fit(d), .b_pn, .b_ml, %s)", ratio),
      sprintf(".b_t0 <- .h_boot_coef_stat(.b_val, .b_pn, .b_ml, %s)", ratio))
  }
  code <- c(
    if (nzchar(setup)) setup,
    .sr_inline(.sr_h_boot, ".h_boot"),
    .sr_inline(.sr_h_boot_ci, ".h_boot_ci"),
    .sr_inline(.sr_h_boot_summary, ".h_boot_summary"),
    if (bca) .sr_inline(.sr_h_boot_jack, ".h_boot_jack"),
    if (kind == "rmst") .sr_inline(.sr_h_boot_rmst_stat, ".h_boot_rmst_stat") else
      .sr_inline(.sr_h_boot_coef_stat, ".h_boot_coef_stat"),
    ".b_rng <- globalenv()[[\".Random.seed\"]]",
    fit,
    ".b_data <- as.data.frame(sr_data)",
    ".b_val <- .h_fit(sr_data)",
    stat,
    "if (!length(.b_t0)) stop(\"the bootstrap has no primary effect to resample: the exposure is not estimable\")",
    "# every replicate stream derives from the stream of the run preamble",
    ".b_env <- globalenv()",
    ".b_env[[\".Random.seed\"]] <- .b_rng",
    sprintf(".b_cl <- %s", if (is.null(cl)) "NULL" else sprintf(".b_data[[%s]]", .sr_lit(cl))),
    sprintf(".b_t <- .h_boot(.b_data, .b_stat, %s, .b_cl, 100L, names(.b_t0))", .sr_lit(bs$B)),
    ".b_jk <- NULL",
    if (bca) ".b_jk <- .h_boot_jack(.b_data, .b_stat, .b_cl, 1000L, names(.b_t0))",
    sprintf(".b_ci <- .h_boot_summary(.b_t0, .b_t, %s, .b_jk$values, .h_boot_ci)", .sr_lit(bs$type)),
    ".b_ok <- stats::complete.cases(.b_t)",
    info(".b_ci", "sum(!.b_ok)", "sum(.b_ok)", "attr(.b_t, \"first_error\")",
         "if (is.null(.b_jk)) NULL else .b_jk[c(\"units\", \"groups\", \"grouped\", \"over\")]",
         "if (is.null(.b_cl)) nrow(.b_data) else length(unique(.b_cl))"))
  paste(code, collapse = "\n")
}

# ---- present ------------------------------------------------------------------------------------

# The fragments every bootstrap result carries: how it was done, failures, BCa fallbacks.
.sr_boot_fragments <- function(bt, kind) {
  n_units <- as.integer(round(bt$n_units))
  how <- if (identical(bt$boot_type, "cluster")) {
    sprintf("whole clusters resampled with replacement (%d clusters)", n_units)
  } else {
    sprintf("rows resampled with replacement (%d rows)", n_units)
  }
  type_text <- switch(as.character(bt$type),
    percentile = "percentile intervals",
    normal = "bias-corrected normal intervals (2 x estimate - bootstrap mean, -/+ z x bootstrap SD)",
    bca = {
      jk <- bt$jack
      over <- if (is.null(jk)) "" else if (isTRUE(jk$grouped)) {
        sprintf(", the acceleration from a jackknife over %d random groups of the %d units", as.integer(jk$groups),
                as.integer(jk$units))
      } else {
        sprintf(", the acceleration from a leave-one-out jackknife over the %d %s", as.integer(jk$groups), jk$over)
      }
      paste0("bias-corrected and accelerated (BCa) intervals", over)
    },
    "intervals")
  scale <- if (kind %in% c("glm", "cox")) " Ratios (OR, RR, HR) were bootstrapped on the log scale and exponentiated." else ""
  frag <- list(boot_note = paste0(
    sprintf("Bootstrap: B = %d replicates, %s; the analysis, including any marginal (g-computation) effect, was re-run in every replicate; %s for the primary effects.",
            as.integer(bt$B), how, type_text), scale,
    " The Monte Carlo error of each interval endpoint is the SD of that endpoint over 10 consecutive batches of the replicates divided by sqrt(10).",
    " No bootstrap P value is reported; the covariate and descriptive rows keep their model-based intervals.",
    " Each block of 100 replicates draws from its own L'Ecuyer-CMRG stream of the run seed."))
  nf <- as.numeric(bt$n_failed)
  if (isTRUE(nf > 0)) {
    frag$boot_failures <- sprintf(
      "%d of %d bootstrap replicate(s) failed or gave a non-finite estimate and were excluded%s.",
      as.integer(nf), as.integer(bt$B),
      if (is.character(bt$first_error) && !is.na(bt$first_error)) paste0(" (first failure: ", bt$first_error, ")") else "")
    if (nf > 0.1 * as.numeric(bt$B)) {
      frag$boot_failure_warning <- paste(
        "More than 10% of the bootstrap replicates failed: the bootstrap distribution describes only the",
        "resamples where the model could be fitted, so the intervals may be too narrow or biased.")
    }
  }
  ci <- bt$ci
  if (!is.null(ci)) {
    fb <- ci[!is.na(ci$fallback_reason), , drop = FALSE]
    if (nrow(fb)) {
      frag$boot_fallback <- paste0(
        "The BCa interval fell back to the percentile interval for ",
        paste(sprintf("%s (%s)", fb$key, fb$fallback_reason), collapse = "; "), ".")
    }
  }
  frag
}

#' The present step of a route under the bootstrap: the route's own present on the value with the
#' bootstrap bounds put in place of the model bounds of the primary rows, then the bootstrap
#' ci_method, MCSE and fragments. Without a bootstrap value it is the route's present.
.sr_boot_present <- function(value, spec, present_fn, kind = c("lm", "glm", "cox", "rmst")) {
  kind <- match.arg(kind)
  bt <- value$boot
  if (is.null(bt)) return(present_fn(value, spec))
  if (identical(bt$choice, "impute_then_boot")) {
    res <- present_fn(value, spec)
    e <- res$effects
    i <- e$role == "primary" & e$ci_method == "rubin"
    e$ci_method[i] <- "rubin_bootvar"
    res$effects <- e
    res$fragments$impute_then_boot <- sprintf(paste0(
      "Impute, then bootstrap: each of the %d imputed datasets was bootstrapped (B = %d replicates of its rows); ",
      "the bootstrap variance replaced the model variance of the primary effects in Rubin's rules ",
      "(ci_method rubin_bootvar); the covariates keep their model variance."),
      as.integer(round(value$mi$m)), as.integer(bt$B))
    nf <- as.numeric(bt$n_failed)
    if (isTRUE(nf > 0)) {
      tot <- nf + as.numeric(bt$n_finite)
      frag <- sprintf("%d of %d bootstrap replicate(s), over all imputed datasets, failed or gave a non-finite estimate and were excluded.",
                      as.integer(nf), as.integer(tot))
      res$fragments$boot_failures <- frag
      if (nf > 0.1 * tot) {
        res$fragments$boot_failure_warning <- "More than 10% of the bootstrap replicates failed: the bootstrap variances may be biased."
      }
    }
    return(res)
  }
  ci <- bt$ci
  bti <- identical(bt$choice, "boot_then_impute")
  est <- if (bti) ci$mean else ci$t0
  out <- is.finite(est) & is.finite(ci$lo) & is.finite(ci$hi) & (est < ci$lo | est > ci$hi)
  if (any(out)) {
    stop("The bootstrap interval (", sr_fmt_num(ci$lo[out][1L]), ", ", sr_fmt_num(ci$hi[out][1L]),
         ") of a primary effect does not contain its estimate ", sr_fmt_num(est[out][1L]),
         ": the bootstrap distribution is strongly biased, so the effect cannot be reported with it.",
         call. = FALSE)
  }
  key_row <- function(k) ci[match(k, ci$key), , drop = FALSE]
  bterm <- character()
  if (kind == "rmst") {
    if (bti) {
      lv <- as.character(value$groups$level)
      r <- key_row(paste0("d", match(value$diffs$level, lv)))
      d <- value$diffs
      d$est <- r$mean
      d$lo <- r$lo
      d$hi <- r$hi
      d$se <- r$se
      d$p <- NA_real_
      value$diffs <- d
    }
  } else {
    cf <- value$coef
    hit <- cf$term %in% ci$key
    r <- key_row(cf$term[hit])
    if (bti) cf$est[hit] <- r$mean
    cf$lo[hit] <- r$lo
    cf$hi[hit] <- r$hi
    cf$se[hit] <- r$se
    cf$p[hit] <- NA_real_
    value$coef <- cf
    bterm <- cf$term[cf$term != "(Intercept)"]
    mg <- value$marginal$rows
    if (!is.null(mg) && nrow(mg)) {
      j <- seq_len(nrow(mg))
      rd <- key_row(paste0("mrd", j))
      if (bti) mg$rd <- rd$mean
      mg$rd_lo <- rd$lo
      mg$rd_hi <- rd$hi
      mg$rd_p <- NA_real_
      mg$rd_mcse <- rd$mcse
      lr <- if (kind == "glm") key_row(paste0("mlrr", j)) else NULL
      if (bti && !is.null(lr)) mg$log_rr <- lr$mean
      mg$log_rr_lo <- if (is.null(lr)) NA_real_ else lr$lo
      mg$log_rr_hi <- if (is.null(lr)) NA_real_ else lr$hi
      mg$log_rr_p <- NA_real_
      mg$log_rr_mcse <- if (is.null(lr)) NA_real_ else lr$mcse
      value$marginal$rows <- mg
    }
  }
  res <- present_fn(value, spec)
  e <- res$effects
  method <- function(r) paste0("boot_", r$type_used)
  if (kind == "rmst") {
    lv <- as.character(value$groups$level)
    i <- which(e$role == "primary" & e$measure == "RMST_diff")
    r <- key_row(paste0("d", match(e$level[i], lv)))
    if (bti) e$est[i] <- r$mean
    e$lo[i] <- r$lo
    e$hi[i] <- r$hi
    e$p[i] <- NA_real_
    e$mcse[i] <- r$mcse
    e$ci_method[i] <- method(r)
    dt <- res$tables$rmst_difference
    if (!is.null(dt) && nrow(dt) == length(i)) {
      dt[["RMST difference (95% CI)"]] <- sr_fmt_ci(e$est[i], e$lo[i], e$hi[i])
      dt$SE <- .sr_lm_num(r$se)
      dt$P <- rep("NA", length(i))
      res$tables$rmst_difference <- dt
    }
  } else {
    nb <- length(bterm)
    for (i in seq_len(nb)) {
      if (!(bterm[i] %in% ci$key)) next
      r <- key_row(bterm[i])
      e$mcse[i] <- r$mcse
      e$ci_method[i] <- method(r)
    }
    nmg <- NROW(value$marginal$rows)
    per <- if (kind == "glm") 3L else 1L
    if (nmg > 0L && nrow(e) >= nb + nmg * per) {
      first <- nrow(e) - nmg * per
      for (j in seq_len(nmg)) {
        at <- first + (j - 1L) * per
        r <- key_row(paste0("mrd", j))
        e$mcse[at + 1L] <- r$mcse
        e$ci_method[at + 1L] <- method(r)
        if (per == 3L) {
          r <- key_row(paste0("mlrr", j))
          e$mcse[at + 2L] <- r$mcse
          e$ci_method[at + 2L] <- method(r)
        }
      }
    }
    for (nm in c("lm_marginal", "glm_marginal")) {
      f <- res$fragments[[nm]]
      if (!bti && is.character(f)) res$fragments[[nm]] <- sub("delta-method interval", "bootstrap interval", f, fixed = TRUE)
    }
  }
  res$effects <- e
  frag <- .sr_boot_fragments(bt, kind)
  if (bti) {
    frag$boot_then_impute <- sprintf(paste0(
      "Bootstrap, then impute: each of the B = %d bootstrap samples was multiply imputed (m = %d) and analysed, ",
      "pooled by Rubin's rules; the estimate of a primary effect is the mean of the B pooled estimates and its ",
      "interval their percentile interval."), as.integer(bt$B), as.integer(bt$m_inner))
    if (isTRUE(bt$m_auto)) {
      frag$boot_m_auto <- sprintf(
        "spec$mi$m = \"auto\" is not run inside every bootstrap sample: each sample was imputed m = %d times.",
        as.integer(bt$m_inner))
    }
  }
  res$fragments <- c(res$fragments, frag)
  res
}
