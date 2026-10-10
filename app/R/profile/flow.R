# profile/flow.R - the participant flow: row counts through the exclusion steps, and a CONSORT-style diagram.
#
#   sr_flow(project, table)            data.frame(step, label, n_before, n_after, n_excluded, reason)
#   sr_flow_plot(flow, path, ...)      draw it as a PNG (consort boxes, ragg device), return the path
#
# The rows are the cohort steps (state$cohorts[[table]]$flow, when a cohort was built), then the active
# drop_cases steps of the current recipe branch, in order. Only a branch bound to `table` counts; a branch
# bound to another table, or an empty one, adds nothing. The attribute "n_total" holds the number of rows
# of the source table, and "label" the name of the first box, so a flow with no step still plots one box.
# The counts of a recipe step come from sr_recipe_counts() (DuckDB over the store view).
#
# Outcome-blind: while the SAP is not locked (sr_blind()), the number of cases a filter on the outcome
# column removes is a marginal outcome count. A drop_cases step whose filter uses an outcome-role column,
# and every row after it (whose counts descend from it), show NA for n_before, n_after and n_excluded,
# and their label ends in SR_FLOW_HIDDEN; the diagram prints "n = hidden" for them. The reasons stay.
# The same holds for a cohort row flagged outcome_dependent (the cohort builder sets it for a criterion
# whose filter uses an outcome-role column): it and every later row, cohort or recipe, are hidden.
# With a cohort, the n_total attribute (the first box) is the number of ids the cohort started from, not
# the rows of the table, whose size is the count after the last criterion.
#
# Text: the labels the code generates are ASCII only (anything else becomes <U+XXXX>); a reason is the
# user's own text and passes through as UTF-8. The diagram uses the Windows font Arial through ragg and
# downloads nothing.

SR_FLOW_HIDDEN <- " (count hidden until SAP lock)"
SR_FLOW_COLS <- c("step", "label", "n_before", "n_after", "n_excluded", "reason")

# ---- text -------------------------------------------------------------------------------------------

# Generated text as ASCII: each non-ASCII character becomes <U+XXXX> (an invalid string becomes "?").
.sr_fl_ascii <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s)) return(NA_character_)
    cp <- suppressWarnings(utf8ToInt(enc2utf8(s)))
    if (anyNA(cp)) return("?")
    if (all(cp < 128L)) return(enc2utf8(s))
    paste(vapply(cp, function(i) if (i < 128L) intToUtf8(i) else sprintf("<U+%04X>", i), ""), collapse = "")
  }, "", USE.NAMES = FALSE)
}

# A user-supplied reason: UTF-8, on one line (a line break inside it would break the box layout).
.sr_fl_reason <- function(x) {
  x <- enc2utf8(as.character(x))
  x[is.na(x)] <- ""
  trimws(gsub("[[:space:]]+", " ", x))
}

# A count for a box: a whole number, or "hidden" for NA.
.sr_fl_n <- function(n) ifelse(is.na(n), "hidden", sprintf("%.0f", n))

# ---- the rows ---------------------------------------------------------------------------------------

.sr_fl_empty <- function() {
  data.frame(step = character(), label = character(), n_before = numeric(), n_after = numeric(),
             n_excluded = numeric(), reason = character(), stringsAsFactors = FALSE)
}

# The cohort flow of the state (written by the cohort builder) as the flow columns; NULL or empty gives
# no row. Missing optional columns are filled in: n_excluded = n_before - n_after. While `blind`, a row
# with outcome_dependent TRUE and every row after it have NA counts and the hidden label. Attributes:
# "hidden" (TRUE when a row was hidden: the recipe rows that follow are hidden too) and "n_start" (the
# true n_before of the first row, NA with no row).
.sr_fl_cohort <- function(x, blind = FALSE) {
  if (is.null(x) || !length(x)) return(.sr_fl_empty())
  if (!is.data.frame(x)) {
    x <- tryCatch(do.call(rbind, lapply(x, function(r) as.data.frame(r, stringsAsFactors = FALSE))),
                  error = function(e) stop("the cohort flow is not a table of steps", call. = FALSE))
  }
  if (!nrow(x)) return(.sr_fl_empty())
  if (!all(c("n_before", "n_after") %in% names(x))) stop("the cohort flow needs n_before and n_after", call. = FALSE)
  n <- nrow(x)
  nb <- as.numeric(x$n_before)
  na <- as.numeric(x$n_after)
  label <- .sr_fl_ascii(if (is.null(x$label)) paste0("Cohort step ", seq_len(n)) else x$label)
  dep <- if (is.null(x$outcome_dependent)) rep(FALSE, n) else as.logical(x$outcome_dependent)
  hide <- blind & cumsum(!is.na(dep) & dep) > 0L
  n_start <- nb[1L]
  nex <- if (is.null(x$n_excluded)) nb - na else as.numeric(x$n_excluded)
  nb[hide] <- NA_real_
  na[hide] <- NA_real_
  nex[hide] <- NA_real_
  label[hide] <- paste0(label[hide], SR_FLOW_HIDDEN)
  out <- data.frame(
    step = .sr_fl_ascii(if (is.null(x$step)) paste0("c", seq_len(n)) else x$step),
    label = label, n_before = nb, n_after = na, n_excluded = nex,
    reason = .sr_fl_reason(if (is.null(x$reason)) rep("", n) else x$reason),
    stringsAsFactors = FALSE)
  attr(out, "hidden") <- any(hide)
  attr(out, "n_start") <- n_start
  out
}

# The active drop_cases steps of the current branch when it is bound to `table`, as flow rows, with the
# counts hidden from the first outcome-dependent step on while blind (from the first step when `hidden`:
# an outcome-dependent cohort row came before).
.sr_fl_recipe <- function(project, table, hidden = FALSE) {
  br <- .sr_rc_branch(project)
  steps <- .sr_rc_active(br)
  if (!length(steps) || !identical(br$table, table)) return(.sr_fl_empty())
  ct <- sr_recipe_counts(project, table)
  blind <- sr_blind(project)
  outcome <- sr_role_cols(project, table, "outcome")
  hidden <- blind && isTRUE(hidden)
  rows <- list()
  for (st in steps) {
    if (!identical(st$op, "drop_cases")) next
    if (blind && length(intersect(.sr_rc_where_cols(st$args[["where"]]), outcome))) hidden <- TRUE
    k <- match(st$id, ct$step_id)
    nb <- ct$rows_before[k]
    na <- ct$rows_after[k]
    label <- .sr_fl_ascii(sprintf("Remaining after step %s", st$id))
    if (hidden) {
      nb <- NA_real_
      na <- NA_real_
      label <- paste0(label, SR_FLOW_HIDDEN)
    }
    rows[[length(rows) + 1L]] <- data.frame(
      step = .sr_fl_ascii(st$id), label = label, n_before = nb, n_after = na, n_excluded = nb - na,
      reason = .sr_fl_reason(st$reason), stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(.sr_fl_empty())
  do.call(rbind, rows)
}

#' The participant flow of `table`: the cohort steps (when a cohort was built, state$cohorts[[table]]$flow),
#' then the active drop_cases steps of the current recipe branch (when it is bound to `table`), in order.
#' While the SAP is not locked, an exclusion that filters on the outcome column and every row after it have
#' NA counts and a label ending "(count hidden until SAP lock)": that count is an outcome statistic. The same
#' goes for a cohort row with outcome_dependent TRUE, and every row after it.
#' @return data.frame(step, label, n_before, n_after, n_excluded, reason), with attributes "n_total" (the
#'   rows of the table; with a cohort, the ids it started from) and "label" (the first box of the diagram).
sr_flow <- function(project, table) {
  .sr_store_check_table(table)
  if (!(table %in% sr_store_tables(project))) stop("table '", table, "' is not in the store", call. = FALSE)
  sr_store_view(project, table)
  con <- sr_duck(project)
  n_total <- as.numeric(sr_duck_query(con, paste("SELECT count(*) AS n FROM", .sr_rc_q(con, table)))$n)
  coh <- .sr_fl_cohort(project$state$cohorts[[table]]$flow, sr_blind(project))
  if (nrow(coh)) n_total <- attr(coh, "n_start")
  out <- rbind(coh, .sr_fl_recipe(project, table, isTRUE(attr(coh, "hidden"))))
  rownames(out) <- NULL
  attr(out, "hidden") <- NULL                 # helpers of .sr_fl_cohort, not part of the result
  attr(out, "n_start") <- NULL
  attr(out, "n_total") <- n_total
  attr(out, "label") <- .sr_fl_ascii(paste0("Records in ", table))
  out
}

# ---- the diagram ------------------------------------------------------------------------------------

.sr_fl_check <- function(flow, path, width, height) {
  if (!is.data.frame(flow) || !all(SR_FLOW_COLS %in% names(flow))) {
    stop("flow must be the data.frame that sr_flow() returns (columns ", paste(SR_FLOW_COLS, collapse = ", "), ")",
         call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("path must be a single file name", call. = FALSE)
  }
  if (!grepl("[.]png$", path, ignore.case = TRUE)) stop("path must be a .png file", call. = FALSE)
  pos <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0
  if (!pos(width)) stop("width must be a single positive number of inches", call. = FALSE)
  if (!is.null(height) && !pos(height)) stop("height must be NULL or a single positive number of inches", call. = FALSE)
  n_total <- attr(flow, "n_total")
  if (is.null(n_total) || length(n_total) != 1L) {
    if (nrow(flow)) {
      n_total <- flow$n_before[1L]
    } else {
      stop("a flow with no step needs its n_total attribute (use sr_flow())", call. = FALSE)
    }
  }
  n_total
}

# The font family of the diagram: Arial on Windows (always installed there), the generic sans elsewhere.
.sr_fl_family <- function() if (identical(.Platform$OS.type, "windows")) "Arial" else "sans"

#' Draw a flow as a CONSORT-style diagram (a PNG at 300 dpi). Boxes read "<label>\nn = <n>", side boxes
#' "Excluded (n = <k>):\n<reason>"; a hidden count reads "n = hidden". `height` defaults to a size computed
#' from the number of boxes. The file is written beside `path` and renamed, the parent folder is created.
#' @param flow what sr_flow() returns.
#' @param path an .png file name.
#' @param width,height inches.
#' @return `path`.
sr_flow_plot <- function(flow, path, width = 7, height = NULL) {
  n_total <- .sr_fl_check(flow, path, width, height)
  n <- nrow(flow)
  if (is.null(height)) height <- max(2, 1.5 * (n + 1L) + 0.5)
  first <- attr(flow, "label") %||% "All records"
  gp <- grid::gpar(fontfamily = .sr_fl_family(), cex = 0.9)
  tw <- 45

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  ragg::agg_png(tmp, width = width, height = height, units = "in", res = 300, background = "white")
  dev <- unname(grDevices::dev.cur())
  on.exit({
    if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev)
    unlink(tmp)
  }, add = TRUE)

  # the boxes are measured on the open device (the font and size they are drawn with)
  first_txt <- sprintf("%s\nn = %s", .sr_fl_ascii(first), .sr_fl_n(n_total))
  if (!n) {
    grid::grid.draw(consort::textbox(first_txt, txt_gp = gp, box_fn = grid::rectGrob, box_gp = grid::gpar(fill = "white")))
  } else {
    g <- consort::add_box(txt = first_txt, txt_gp = gp, text_width = tw)
    for (i in seq_len(n)) {
      side <- sprintf("Excluded (n = %s):", .sr_fl_n(flow$n_excluded[i]))
      if (nzchar(flow$reason[i])) side <- paste0(side, "\n", .sr_fl_reason(flow$reason[i]))
      g <- consort::add_side_box(g, txt = side, txt_gp = gp, text_width = tw)
      g <- consort::add_box(g, txt = sprintf("%s\nn = %s", flow$label[i], .sr_fl_n(flow$n_after[i])),
                            txt_gp = gp, text_width = tw)
    }
    plot(g)
  }
  grDevices::dev.off(dev)
  .sr_rename_retry(tmp, path)
  path
}
