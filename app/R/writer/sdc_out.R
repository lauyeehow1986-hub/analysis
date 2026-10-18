# writer/sdc_out.R - small-cell suppression of the text tables and figures the writer emits.
# A count of 1 to SR_SDC_K - 1 prints as "<5" (paste0("<", k)); zero is shown, because the rule hides
# small NON-ZERO cells. Hiding one cell of a row or of a column of a categorical block is not enough
# when the margin is printed, so two secondary rules hide the smallest other non-zero cell:
#   row rule    a row whose group cells hold exactly one hidden cell also hides its smallest other
#               non-zero group cell. The "Overall" column is never the cell hidden by this rule.
#   block rule  a block is a run of consecutive rows whose block_col cell starts with four spaces (the
#               level rows of table1). One column of a block holding exactly one hidden cell also hides
#               its smallest other non-zero cell.
# The two rules repeat until nothing changes. A count cell matches ^[0-9]+$ or ^[0-9]+ \([0-9.]+%\)$;
# "m (sd)", "n [lo to hi]" and every other text is left alone.
# SR_SDC_K is defined in writer/placeholders.R.

#' The count a text cell shows
#'
#' @param cell character vector.
#' @return integer vector, NA where the cell is not a count cell (`"3"` or `"12 (4.5%)"`).
.sr_sdc_count <- function(cell) {
  cell <- as.character(cell)
  is_cnt <- grepl("^[0-9]+$", cell) | grepl("^[0-9]+ \\([0-9.]+%\\)$", cell)
  out <- rep(NA_integer_, length(cell))
  if (any(is_cnt)) {
    out[is_cnt] <- suppressWarnings(as.integer(sub("^([0-9]+).*$", "\\1", cell[is_cnt])))
  }
  out
}

# The text printed for a suppressed cell.
.sr_sdc_text <- function(k) paste0("<", k)

# Index (within `idx`) of the smallest non-zero, not yet hidden count among cnt[idx], or NA. Ties go to
# the first.
.sr_sdc_smallest <- function(cnt, hidden, idx) {
  ok <- idx[!hidden[idx] & !is.na(cnt[idx]) & cnt[idx] > 0L]
  if (!length(ok)) return(NA_integer_)
  ok[which.min(cnt[ok])]
}

#' Suppress the small cells of a text table
#'
#' @param df a data frame of text cells (a Table 1, a counts table, a coefficient table).
#' @param k counts of 1..(k-1) are hidden as `"<k"`.
#' @param block_col the column whose four-space indent marks the level rows of a categorical block;
#'   it is never itself a count column.
#' @param skip_cols columns the row rule ignores (they are still suppressed as primary cells).
#' @return list(table = the table with suppressed cells as text, n_suppressed = integer).
sr_sdc_table <- function(df, k = SR_SDC_K, block_col = 1L, skip_cols = character()) {
  stopifnot(is.data.frame(df))
  nr <- nrow(df)
  nc <- ncol(df)
  if (!nr || !nc) return(list(table = df, n_suppressed = 0L))
  bc <- if (is.character(block_col)) match(block_col, names(df)) else as.integer(block_col)
  cols <- setdiff(seq_len(nc), bc)
  cnt <- matrix(NA_integer_, nr, nc)
  for (j in cols) cnt[, j] <- .sr_sdc_count(df[[j]])
  hidden <- !is.na(cnt) & cnt >= 1L & cnt <= k - 1L
  if (!any(!is.na(cnt))) return(list(table = df, n_suppressed = 0L))

  grp_cols <- cols[!(names(df)[cols] %in% c("Overall", skip_cols))]
  # the blocks: runs of consecutive rows whose block_col cell starts with four spaces
  blocks <- list()
  if (length(bc) == 1L && !is.na(bc)) {
    ind <- grepl("^    ", as.character(df[[bc]]))
    ind[is.na(ind)] <- FALSE
    r <- rle(ind)
    ends <- cumsum(r$lengths)
    for (i in which(r$values)) blocks[[length(blocks) + 1L]] <- seq(ends[i] - r$lengths[i] + 1L, ends[i])
  }

  repeat {
    changed <- FALSE
    for (i in seq_len(nr)) {
      if (sum(hidden[i, grp_cols]) == 1L) {
        j <- .sr_sdc_smallest(cnt[i, ], hidden[i, ], grp_cols)
        if (!is.na(j)) {
          hidden[i, j] <- TRUE
          changed <- TRUE
        }
      }
    }
    for (b in blocks) {
      for (j in cols) {
        if (sum(hidden[b, j]) == 1L) {
          i <- .sr_sdc_smallest(cnt[, j], hidden[, j], b)
          if (!is.na(i)) {
            hidden[i, j] <- TRUE
            changed <- TRUE
          }
        }
      }
    }
    if (!changed) break
  }

  txt <- .sr_sdc_text(k)
  for (j in which(colSums(hidden) > 0L)) {
    v <- as.character(df[[j]])
    v[hidden[, j]] <- txt
    df[[j]] <- v
  }
  list(table = df, n_suppressed = as.integer(sum(hidden)))
}

#' One count as text
#'
#' @param x a single count.
#' @param k suppression threshold.
#' @return list(text, suppressed): `"<5"` / TRUE for 1..(k-1), otherwise the integer text / FALSE.
sr_sdc_value <- function(x, k = SR_SDC_K) {
  if (is.finite(x) && x >= 1 && x <= k - 1L) {
    list(text = .sr_sdc_text(k), suppressed = TRUE)
  } else {
    list(text = sprintf("%d", as.integer(round(x))), suppressed = FALSE)
  }
}

#' Number-at-risk labels with small cells hidden
#'
#' @param km_risk the km_risk frame (strata, time, n.risk).
#' @param k suppression threshold.
#' @return km_risk plus a character `label` column: `"<5"` for n.risk of 1..(k-1), else the integer text.
sr_sdc_km_risk <- function(km_risk, k = SR_SDC_K) {
  n <- km_risk$n.risk
  small <- is.finite(n) & n >= 1 & n <= k - 1L
  km_risk$label <- ifelse(small, .sr_sdc_text(k), ifelse(is.finite(n), sprintf("%d", as.integer(round(n))), NA_character_))
  km_risk
}
