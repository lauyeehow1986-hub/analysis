# writer/numfmt.R - MCSE-limited number, interval and p-value formatting for report tables.
# Only the digits that Monte Carlo error leaves meaningful are printed (capped at digits + 2).

#' Validate a numeric argument; a bare logical NA vector is accepted and coerced.
.sr_num_arg <- function(x, name) {
  if (is.logical(x) && all(is.na(x))) x <- as.numeric(x)
  if (!is.numeric(x)) stop("`", name, "` must be numeric", call. = FALSE)
  x
}

#' Validate the `digits` setting: one non-negative whole number.
.sr_digits_arg <- function(digits) {
  d <- suppressWarnings(as.integer(digits))
  if (length(d) != 1L || is.na(d) || d < 0L) {
    stop("`digits` must be a single non-negative whole number", call. = FALSE)
  }
  d
}

#' Validate mcse (NA allowed, otherwise finite and >= 0). If `n` is given, also require
#' length 1 or n and recycle to n.
.sr_mcse_arg <- function(mcse, n = NULL) {
  mcse <- .sr_num_arg(mcse, "mcse")
  if (!is.null(n) && length(mcse) != 1L && length(mcse) != n) {
    stop("`mcse` must have length 1 or the same length as the values (", n, "), got length ",
         length(mcse), call. = FALSE)
  }
  bad <- !is.na(mcse) & (!is.finite(mcse) | mcse < 0)
  if (any(bad)) stop("`mcse` must be finite and >= 0 (or NA)", call. = FALSE)
  if (is.null(n)) mcse else rep_len(mcse, n)
}

#' Decimals to print given the Monte Carlo standard error: `digits` when mcse is NA or 0,
#' otherwise min(max(0, floor(-log10(2 * mcse))), digits + 2). Vectorised; returns integer.
sr_mcse_digits <- function(mcse, digits = as.integer(sr_opt("digits", 2))) {
  digits <- .sr_digits_arg(digits)
  mcse <- .sr_mcse_arg(mcse)
  d <- rep(as.numeric(digits), length(mcse))
  use <- !is.na(mcse) & mcse > 0
  if (any(use)) d[use] <- pmin(pmax(0, floor(-log10(2 * mcse[use]))), digits + 2)
  as.integer(d)
}

#' Fixed-point formatting of x with per-element decimals d; NA -> "NA"; never "-0.00".
.sr_fixed <- function(x, d) {
  d <- rep_len(d, length(x))
  out <- rep("NA", length(x))
  ok <- !is.na(x)
  out[ok] <- vapply(which(ok), function(i) {
    formatC(round(x[i], d[i]), format = "f", digits = d[i])
  }, character(1))
  sub("^-(0[.]?0*)$", "\\1", out)
}

#' Format numbers with MCSE-limited decimals (see sr_mcse_digits).
sr_fmt_num <- function(x, mcse = NA, digits = as.integer(sr_opt("digits", 2))) {
  x <- .sr_num_arg(x, "x")
  mcse <- .sr_mcse_arg(mcse, length(x))
  .sr_fixed(x, sr_mcse_digits(mcse, digits))
}

#' Format "est (lo to hi)" with one shared number of decimals; NA components print "NA".
sr_fmt_ci <- function(est, lo, hi, mcse = NA, digits = as.integer(sr_opt("digits", 2))) {
  est <- .sr_num_arg(est, "est")
  lo <- .sr_num_arg(lo, "lo")
  hi <- .sr_num_arg(hi, "hi")
  lens <- c(length(est), length(lo), length(hi))
  n <- max(lens)
  if (any(lens != n & lens != 1L)) {
    stop("`est`, `lo` and `hi` must have the same length (or length 1)", call. = FALSE)
  }
  if (any(lens == 0L)) n <- 0L
  est <- rep_len(est, n)
  lo <- rep_len(lo, n)
  hi <- rep_len(hi, n)
  d <- sr_mcse_digits(.sr_mcse_arg(mcse, n), digits)
  paste0(.sr_fixed(est, d), " (", .sr_fixed(lo, d), " to ", .sr_fixed(hi, d), ")")
}

#' Format one p-value (scalar, non-NA, in [0, 1]).
.sr_fmt_p1 <- function(p) {
  if (p < 0.001) return("<0.001")
  if (p > 0.99) return(">0.99")
  dp <- if (p < 0.01) 3L else 2L
  if (p < 0.05) {
    while (round(p, dp) >= 0.05 && dp < 6L) dp <- dp + 1L
    if (round(p, dp) >= 0.05) return("<0.05")
  }
  formatC(round(p, dp), format = "f", digits = dp)
}

#' Format p-values: "<0.001", ">0.99", 3 dp below 0.01, else 2 dp; a p < 0.05 never prints as
#' "0.05" (decimals are raised up to 6, then "<0.05").
sr_fmt_p <- function(p) {
  p <- .sr_num_arg(p, "p")
  if (any(!is.na(p) & (!is.finite(p) | p < 0 | p > 1))) {
    stop("`p` must be between 0 and 1 (or NA)", call. = FALSE)
  }
  out <- rep("NA", length(p))
  ok <- !is.na(p)
  out[ok] <- vapply(p[ok], .sr_fmt_p1, character(1))
  out
}
