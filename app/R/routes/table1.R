# routes/table1.R - the Table 1 route: baseline characteristics by group with standardised
# mean differences (SMD). A descriptive route: no p-values anywhere (spec C), the effects `p`
# column is NA and every effect row has role "descriptive".
#
# The compiled code is data-free: it only calls .h_t1_main(sr_data, by, vars, nonnormal), and the
# helpers it needs are inlined above that call. Which variable is continuous and which categorical,
# and the levels of each, are decided by the emitted code at run time:
#   * a numeric column with more than two distinct observed values is continuous (mean (SD), or
#     median [IQR] when listed in `nonnormal`); an all-missing numeric column is continuous with NA
#     summaries;
#   * every other column (text, logical, factor, a numeric column with at most two distinct values
#     such as 0/1) is categorical: n (%) per level, percentages of the non-missing values; more
#     than 50 levels is refused (an identifier typed in `vars`), a date or time column is refused.
# Rows with a missing grouping value are dropped (and counted in `n_excluded`); "Overall" is the
# same set of rows.
#
# SMD, group k against the first level of `by` (levels sorted):
#   continuous   d = (m_k - m_0) / sqrt((s_k^2 + s_0^2) / 2)
#   two levels   d = (p_k - p_0) / sqrt((p_k (1 - p_k) + p_0 (1 - p_0)) / 2), p the proportion of the
#                last (second) sorted level
#   > 2 levels   Yang-Dalton: dropping the first level, d = sqrt((p_k - p_0)' S^-1 (p_k - p_0)),
#                S = (S_k + S_0) / 2, S_g[j, j] = p_gj (1 - p_gj), S_g[j, l] = -p_gj p_gl
#   CI (continuous and two-level): d +- 1.96 sqrt((n_k + n_0) / (n_k n_0) + d^2 / (2 (n_k + n_0)))
#   the multi-level SMD has no CI (NA, ci_method "none").
# Singular S in the Yang-Dalton distance (a level empty in both groups, or a level with no
# within-group variance): levels with p_k + p_0 = 0 are dropped first (exact, they contribute
# nothing); if S is still ill-conditioned (rcond < 1e-12) its Moore-Penrose pseudo-inverse (svd) is
# used when the difference lies in the range of S, and the SMD is NA (undefined: the groups are
# separated by a level with no within-group variance) when it does not. An SMD whose denominator
# is zero or not finite is NA as well.

# ---- helpers inlined into the emitted code -----------------------------------------------------

#' c(lo, hi) of the approximate 95 percent CI of an SMD `d` from group sizes n1 and n0.
.h_t1_ci <- function(d, n1, n0) {
  if (is.na(d) || n1 < 1 || n0 < 1) return(c(NA_real_, NA_real_))
  se <- sqrt((n1 + n0) / (n1 * n0) + d^2 / (2 * (n1 + n0)))
  c(d - 1.96 * se, d + 1.96 * se)
}

#' c(d, lo, hi): the SMD of a continuous variable, group 1 (m1, s1, n1) against group 0.
.h_t1_smd_cont <- function(m1, s1, n1, m0, s0, n0) {
  den <- sqrt((s1^2 + s0^2) / 2)
  if (is.na(den) || !is.finite(den) || den <= 0) return(rep(NA_real_, 3L))
  d <- (m1 - m0) / den
  c(d, .h_t1_ci(d, n1, n0))
}

#' c(d, lo, hi): the SMD of a two-level variable from the level counts c1 and c0 (second level = 1).
.h_t1_smd_bin <- function(c1, c0) {
  n1 <- sum(c1)
  n0 <- sum(c0)
  if (n1 < 1 || n0 < 1) return(rep(NA_real_, 3L))
  pr1 <- c1[2L] / n1
  pr0 <- c0[2L] / n0
  den <- sqrt((pr1 * (1 - pr1) + pr0 * (1 - pr0)) / 2)
  if (is.na(den) || !is.finite(den) || den <= 0) return(rep(NA_real_, 3L))
  d <- (pr1 - pr0) / den
  c(d, .h_t1_ci(d, n1, n0))
}

#' Moore-Penrose pseudo-inverse of a symmetric matrix by svd.
.h_t1_pinv <- function(S) {
  sv <- svd(S)
  pos <- sv$d > max(dim(S)) * max(sv$d) * .Machine$double.eps
  if (!any(pos)) return(matrix(0, nrow(S), ncol(S)))
  sv$v[, pos, drop = FALSE] %*% (t(sv$u[, pos, drop = FALSE]) / sv$d[pos])
}

#' c(d, NA, NA): the Yang-Dalton multivariate SMD from the level counts c1 and c0 of two groups.
.h_t1_smd_multi <- function(c1, c0) {
  n1 <- sum(c1)
  n0 <- sum(c0)
  if (n1 < 1 || n0 < 1) return(rep(NA_real_, 3L))
  pr1 <- (c1 / n1)[-1L]
  pr0 <- (c0 / n0)[-1L]
  use <- (pr1 + pr0) > 0
  if (!any(use)) return(c(0, NA_real_, NA_real_))
  pr1 <- pr1[use]
  pr0 <- pr0[use]
  cov_of <- function(pr) {
    S <- -outer(pr, pr)
    diag(S) <- pr * (1 - pr)
    S
  }
  S <- (cov_of(pr1) + cov_of(pr0)) / 2
  dif <- pr1 - pr0
  Sinv <- NULL
  if (rcond(S) >= 1e-12) Sinv <- tryCatch(solve(S), error = function(e) NULL)
  if (is.null(Sinv)) {
    Sinv <- .h_t1_pinv(S)
    if (max(abs(dif - drop(S %*% Sinv %*% dif))) > 1e-8) return(rep(NA_real_, 3L))
  }
  d2 <- drop(t(dif) %*% Sinv %*% dif)
  c(sqrt(max(d2, 0)), NA_real_, NA_real_)
}

#' The summaries of every variable of `vars` by the levels of `by` (see the header of this file).
.h_t1_main <- function(dat, by, vars, nonnormal) {
  g <- dat[[by]]
  keep <- !is.na(g)
  lv <- as.character(sort(unique(g[keep])))
  if (length(lv) < 2L) {
    stop("the grouping column needs at least two observed groups (found ", length(lv), ")")
  }
  gc <- as.character(g[keep])
  idx <- lapply(lv, function(l) gc == l)
  idx[[length(lv) + 1L]] <- rep(TRUE, length(gc))
  ng <- length(lv)
  out <- vector("list", length(vars))
  for (i in seq_along(vars)) {
    x <- dat[[vars[i]]][keep]
    if (inherits(x, c("Date", "POSIXt"))) {
      stop("column '", vars[i], "' is a date or time; derive a numeric variable first")
    }
    nd <- length(unique(x[!is.na(x)]))
    if (is.numeric(x) && (nd > 2L || nd == 0L)) {
      st <- vapply(idx, function(ix) {
        z <- x[ix]
        nm <- sum(is.na(z))
        z <- z[!is.na(z)]
        n <- length(z)
        if (n == 0L) return(c(0, nm, rep(NA_real_, 5L)))
        qs <- stats::quantile(z, c(0.25, 0.5, 0.75), names = FALSE)
        c(n, nm, mean(z), if (n > 1L) stats::sd(z) else NA_real_, qs[2L], qs[1L], qs[3L])
      }, numeric(7L))
      sm <- vapply(2:ng, function(k) {
        .h_t1_smd_cont(st[3L, k], st[4L, k], st[1L, k], st[3L, 1L], st[4L, 1L], st[1L, 1L])
      }, numeric(3L))
      out[[i]] <- list(var = vars[i], kind = "continuous", nonnormal = vars[i] %in% nonnormal,
                       n = st[1L, ], n_miss = st[2L, ], mean = st[3L, ], sd = st[4L, ],
                       median = st[5L, ], q1 = st[6L, ], q3 = st[7L, ],
                       smd = sm[1L, ], smd_lo = sm[2L, ], smd_hi = sm[3L, ],
                       smd_method = "continuous")
    } else {
      xc <- as.character(x)
      lvl <- as.character(sort(unique(x[!is.na(x)])))
      if (length(lvl) > 50L) {
        stop("column '", vars[i], "' has ", length(lvl), " distinct values; a categorical variable ",
             "may have at most 50 levels")
      }
      cnt <- matrix(0, nrow = length(lvl), ncol = ng + 1L)
      for (k in seq_len(ng + 1L)) {
        for (j in seq_along(lvl)) cnt[j, k] <- sum(xc[idx[[k]]] == lvl[j], na.rm = TRUE)
      }
      nmiss <- vapply(idx, function(ix) sum(is.na(xc[ix])), numeric(1L))
      method <- if (length(lvl) == 0L) "none" else if (length(lvl) == 2L) "binary" else "yang_dalton"
      sm <- vapply(2:ng, function(k) {
        if (method == "none") return(rep(NA_real_, 3L))
        if (method == "binary") return(.h_t1_smd_bin(cnt[, k], cnt[, 1L]))
        .h_t1_smd_multi(cnt[, k], cnt[, 1L])
      }, numeric(3L))
      out[[i]] <- list(var = vars[i], kind = "categorical", levels = lvl, count = cnt,
                       n = colSums(cnt), n_miss = nmiss,
                       smd = sm[1L, ], smd_lo = sm[2L, ], smd_hi = sm[3L, ], smd_method = method,
                       pos_level = if (length(lvl) == 2L) lvl[2L] else NA_character_)
    }
  }
  list(by = by, groups = lv, n = vapply(idx, sum, numeric(1L)), n_excluded = sum(!keep), vars = out)
}

# ---- compile ------------------------------------------------------------------------------------

.sr_t1_compile <- function(spec) {
  by <- spec[["by"]]
  if (!is.character(by) || length(by) != 1L || is.na(by) || !nzchar(by)) {
    stop("table1: 'by' must be a single column name", call. = FALSE)
  }
  vars <- unname(as.character(unlist(spec[["vars"]])))
  if (!length(vars) || anyNA(vars) || any(!nzchar(vars))) {
    stop("table1: 'vars' must name at least one column", call. = FALSE)
  }
  if (anyDuplicated(vars)) {
    stop("table1: column '", vars[anyDuplicated(vars)], "' is listed twice in vars", call. = FALSE)
  }
  if (by %in% vars) {
    stop("table1: the grouping column by ('", by, "') must not also be listed in vars", call. = FALSE)
  }
  nn <- unname(as.character(unlist(spec[["nonnormal"]])))
  if (length(setdiff(nn, vars))) {
    stop("table1: nonnormal names column(s) that are not in vars: ",
         paste(setdiff(nn, vars), collapse = ", "), call. = FALSE)
  }
  helpers <- c(
    .sr_inline(.h_t1_ci, ".h_t1_ci"), .sr_inline(.h_t1_smd_cont, ".h_t1_smd_cont"),
    .sr_inline(.h_t1_smd_bin, ".h_t1_smd_bin"), .sr_inline(.h_t1_pinv, ".h_t1_pinv"),
    .sr_inline(.h_t1_smd_multi, ".h_t1_smd_multi"), .sr_inline(.h_t1_main, ".h_t1_main")
  )
  paste(c(helpers, sprintf(".h_t1_main(sr_data, %s, %s, %s)", .sr_lit(by), .sr_lit(vars), .sr_lit(nn))),
        collapse = "\n")
}

# ---- present ------------------------------------------------------------------------------------

# "NA" for NA, else the shared number format.
.sr_t1_num <- function(x, digits = as.integer(sr_opt("digits", 2))) {
  x <- as.numeric(x)
  out <- rep("NA", length(x))
  ok <- !is.na(x)
  if (any(ok)) out[ok] <- sr_fmt_num(x[ok], digits = digits)
  out
}

# The SMD cell of each non-reference group: "d (lo to hi)", or "d" without a CI, "NA" if undefined.
.sr_t1_smd_cells <- function(v) {
  vapply(seq_along(v$smd), function(k) {
    if (is.na(v$smd[k])) return("NA")
    if (is.na(v$smd_lo[k]) || is.na(v$smd_hi[k])) return(.sr_t1_num(v$smd[k]))
    sr_fmt_ci(v$smd[k], v$smd_lo[k], v$smd_hi[k])
  }, "")
}

.sr_t1_present <- function(value, spec) {
  lv <- as.character(value$groups)
  ng <- length(lv)
  cnt <- function(x) sprintf("%d", as.integer(round(x)))
  ord <- c(ng + 1L, seq_len(ng))                   # Overall first, then the groups
  smd_names <- if (ng == 2L) "SMD" else paste0("SMD: ", lv[-1L], " vs ", lv[1L])
  col_names <- make.unique(c("Characteristic", paste0("Overall (n=", cnt(value$n[ng + 1L]), ")"),
                             paste0(lv, " (n=", cnt(value$n[seq_len(ng)]), ")"),
                             "Missing, n", smd_names))
  nc <- length(col_names)
  rows <- list()
  add_row <- function(label, cells, missing, smd) {
    rows[[length(rows) + 1L]] <<- c(label, cells, missing, smd)
  }
  eff <- list()
  miss <- matrix(0L, nrow = length(value$vars), ncol = ng + 1L)
  for (i in seq_along(value$vars)) {
    v <- value$vars[[i]]
    miss[i, ] <- as.integer(round(v$n_miss[c(ng + 1L, seq_len(ng))]))
    smd_cells <- .sr_t1_smd_cells(v)
    if (identical(v$kind, "continuous")) {
      f <- .sr_t1_num
      cells <- vapply(ord, function(k) {
        if (v$n[k] == 0) return("NA")
        if (isTRUE(v$nonnormal)) sprintf("%s [%s, %s]", f(v$median[k]), f(v$q1[k]), f(v$q3[k]))
        else sprintf("%s (%s)", f(v$mean[k]), f(v$sd[k]))
      }, "")
      add_row(paste0(v$var, if (isTRUE(v$nonnormal)) ", median [IQR]" else ", mean (SD)"), cells,
              cnt(v$n_miss[ng + 1L]), smd_cells)
    } else {
      add_row(paste0(v$var, ", n (%)"), rep("", ng + 1L), cnt(v$n_miss[ng + 1L]), smd_cells)
      for (j in seq_along(v$levels)) {
        cells <- vapply(ord, function(k) {
          if (v$n[k] == 0) return(cnt(0))
          sprintf("%s (%s%%)", cnt(v$count[j, k]), .sr_t1_num(100 * v$count[j, k] / v$n[k], digits = 1L))
        }, "")
        add_row(paste0("    ", v$levels[j]), cells, "", rep("", ng - 1L))
      }
    }
    for (k in seq_len(ng - 1L)) {
      lo <- v$smd_lo[k]
      eff[[length(eff) + 1L]] <- data.frame(
        term = if (ng == 2L) v$var else paste0(v$var, ": ", lv[k + 1L], " vs ", lv[1L]),
        measure = "SMD", est = v$smd[k], lo = lo, hi = v$smd_hi[k], p = NA_real_,
        scale = "difference",
        level = if (identical(v$kind, "categorical") && !is.na(v$pos_level)) v$pos_level else NA_character_,
        mcse = NA_real_, role = "descriptive", conditional = FALSE,
        ci_method = if (is.na(lo)) "none" else "wald", n = v$n[1L] + v$n[k + 1L], events = NA_real_,
        stringsAsFactors = FALSE)
    }
  }
  mat <- matrix(unlist(rows), ncol = nc, byrow = TRUE)
  tb <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(tb) <- col_names
  mt <- data.frame(Variable = vapply(value$vars, function(v) v$var, ""), miss,
                   stringsAsFactors = FALSE, check.names = FALSE)
  names(mt) <- make.unique(c("Variable", "Overall", lv))
  rownames(mt) <- NULL
  effects <- if (length(eff)) do.call(rbind, eff) else sr_effects_empty()
  rownames(effects) <- NULL
  list(tables = list(table1 = tb, missing = mt), figures = list(), fragments = list(),
       effects = effects)
}

# ---- the route ----------------------------------------------------------------------------------

.sr_table1_route <- function() {
  sr_route(
    id = "table1", version = 1L, family = "describe", purpose = "describe",
    estimand = sr_estimand(population = "analysed sample", variables = "the variables listed in the spec",
                           intercurrent = "not applicable (descriptive)",
                           summary = "baseline characteristics by group", level = "not-applicable"),
    spec_schema = list(
      by = list(type = "chr", required = TRUE),
      vars = list(type = "chr", required = TRUE),
      nonnormal = list(type = "chr", required = FALSE),
      seed = list(type = "num", required = TRUE)
    ),
    compile = .sr_t1_compile, present = .sr_t1_present
  )
}

if (!(.sr_route_key("table1", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
  sr_route_register(.sr_table1_route())
}
