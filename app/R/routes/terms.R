# routes/terms.R - spec columns, the term builder and outcome coding shared by the model routes.
#
# sr_terms_code() turns the exposure and spec$terms into three pieces of R code text plus a map from
# coefficients back to the user's variables:
#   setup  - run once on `sr_data` (the observed table): fixes RCS knots and factor levels into
#            `.sr_fixed`, so every later design (a subset, a bootstrap resample, an imputed copy) has
#            the same columns and the same coefficient names;
#   design - defines `.h_design(d)`: the passthrough columns plus derived columns t1, t2, ... (t<i>f for a
#            factor, t<i> and t<i>_s<j> for the RCS basis, t<i> and t<i>_q for the quadratic). It reads `.sr_fixed` and the .h_ helpers
#            that `setup` defines, so `setup` is always emitted first;
#   rhs    - the formula right-hand side over the derived names (interactions as t<a>:t<b>).
# User text (column names, levels) reaches the code only through .sr_q() / .sr_lit(); the derived
# names are generated. Terms are numbered in order: the exposure first (when present), then spec$terms.

.sr_term_types <- c("linear", "factor", "rcs", "quad")

# Names a passthrough column must not take: the derived design columns and the outcome columns.
.sr_reserved_col <- function(x) grepl("^t[0-9]+(f|_s[0-9]+|_q)?$", x) | x %in% c(".y", ".time", ".event")

# ---- helpers inlined into the emitted code (via .sr_inline) -------------------------------------

#' Sorted unique non-NA levels of x as text, `ref` first; refuses a missing ref or fewer than 2 levels.
.h_levels <- function(x, ref = NULL) {
  lv <- as.character(sort(unique(x[!is.na(x)])))
  if (!is.null(ref)) {
    ref <- as.character(ref)
    if (!(ref %in% lv)) stop("reference level '", ref, "' is not among the observed levels")
    lv <- c(ref, setdiff(lv, ref))
  }
  if (length(lv) < 2L) stop("a factor term needs at least two observed levels")
  lv
}

#' x as numeric; refuses anything but numeric or logical (no silent NA from a text column).
.h_num <- function(x, nm) {
  if (!(is.numeric(x) || is.logical(x))) {
    stop("column '", nm, "' is not numeric (set a reference level to treat it as a factor)")
  }
  as.numeric(x)
}

#' nk RCS knots at the quantiles Hmisc chooses for the observed values; refuses tied knots.
.h_knots <- function(x, nk) {
  k <- Hmisc::rcspline.eval(x[!is.na(x)], nk = nk, knots.only = TRUE)
  if (length(k) != nk) stop("could not place ", nk, " distinct knots (too many tied values)")
  k
}

#' 0/1 integer from a logical or 0/1 numeric column; anything else needs an event value.
.h_bin <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x) && all(x[!is.na(x)] %in% c(0, 1))) return(as.integer(x))
  stop("the column is not 0/1 or logical: set event_value (status_event for a survival status)")
}

# ---- spec columns -------------------------------------------------------------------------------

#' Non-NA, non-empty strings of a spec field (NULL gives character(0)).
.sr_spec_chr <- function(x) {
  if (is.null(x)) return(character())
  x <- as.character(unlist(x))
  x[!is.na(x) & nzchar(x)]
}

#' The unique source columns a spec reads, in the order outcome, time, status, exposure, term
#' variables, cluster, strata, mi$aux. Absent fields contribute nothing.
sr_spec_cols <- function(spec) {
  term_vars <- unlist(lapply(spec[["terms"]], function(t) t[["var"]]))
  unique(c(
    .sr_spec_chr(spec[["outcome"]]), .sr_spec_chr(spec[["time"]]), .sr_spec_chr(spec[["status"]]),
    .sr_spec_chr(spec[["exposure"]]), .sr_spec_chr(term_vars), .sr_spec_chr(spec[["cluster"]]),
    .sr_spec_chr(spec[["strata"]]), .sr_spec_chr(spec[["mi"]][["aux"]])
  ))
}

# ---- the term plan ------------------------------------------------------------------------------

.sr_one_string <- function(x, what) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(what, " must be one non-empty string", call. = FALSE)
  }
  x
}

#' Knots of an RCS term: NULL gives 4 knots, one whole number 3-7 gives that many, 3-7 increasing
#' numbers are explicit locations. Returns list(nk, explicit) (explicit is NULL or the sorted knots).
.sr_knot_spec <- function(k, var) {
  bad <- function() {
    stop("knots for '", var, "' must be a number of knots (3 to 7) or 3 to 7 distinct locations",
         call. = FALSE)
  }
  if (is.null(k)) return(list(nk = 4L, explicit = NULL))
  if (!is.numeric(k) || anyNA(k) || !all(is.finite(k))) bad()
  if (length(k) == 1L) {
    if (k != round(k) || k < 3 || k > 7) bad()
    return(list(nk = as.integer(k), explicit = NULL))
  }
  k <- sort(as.numeric(k))
  if (length(k) < 3L || length(k) > 7L || anyDuplicated(k)) bad()
  list(nk = length(k), explicit = k)
}

#' The ordered terms of the model: exposure first, then spec$terms. Each: id, var, type, ref, knots,
#' role. Nothing here looks at data (compile has none); factor levels are fixed by the emitted setup.
.sr_term_plan <- function(spec) {
  items <- list()
  ex <- spec[["exposure"]]
  if (length(.sr_spec_chr(ex))) {
    ref <- spec[["exposure_ref"]]
    has_ref <- length(ref) == 1L && !is.na(ref) && nzchar(as.character(ref))
    items[[1L]] <- list(var = .sr_one_string(ex, "exposure"), type = if (has_ref) "factor" else "linear",
                        ref = if (has_ref) ref else NULL, knots = NULL, role = "exposure")
  }
  for (t in spec[["terms"]]) {
    ty <- if (is.null(t[["type"]])) "linear" else t[["type"]]
    if (!is.character(ty) || length(ty) != 1L || !(ty %in% .sr_term_types)) {
      stop("term type must be one of ", paste(.sr_term_types, collapse = ", "), call. = FALSE)
    }
    ref <- t[["ref"]]
    items[[length(items) + 1L]] <- list(
      var = .sr_one_string(t[["var"]], "term variable"), type = ty,
      ref = if (ty == "factor" && length(ref) == 1L && !is.na(ref)) ref else NULL,
      knots = if (ty == "rcs") .sr_knot_spec(t[["knots"]], t[["var"]]) else NULL, role = "term"
    )
  }
  vars <- vapply(items, function(i) i$var, character(1))
  if (anyDuplicated(vars)) {
    stop("variable '", vars[anyDuplicated(vars)], "' appears twice in the model (exposure and terms)",
         call. = FALSE)
  }
  for (i in seq_along(items)) items[[i]]$id <- paste0("t", i)
  items
}

#' Derived column names of one planned term, in rhs order. A factor's column is t<i>f, so that its
#' coefficients t<i>f<level> can never be mistaken for a linear term t<i><digits>.
.sr_term_cols <- function(it) {
  switch(it$type,
    linear = it$id,
    factor = paste0(it$id, "f"),
    quad = c(it$id, paste0(it$id, "_q")),
    rcs = c(it$id, if (it$knots$nk > 2L) paste0(it$id, "_s", seq_len(it$knots$nk - 2L)))
  )
}

#' Validated interactions as canonical list(a = index, b = index), a < b, no duplicates.
.sr_interaction_plan <- function(spec, plan) {
  vars <- vapply(plan, function(i) i$var, character(1))
  out <- list()
  keys <- character()
  for (ia in spec[["interactions"]]) {
    if (!is.character(ia) || length(ia) != 2L || anyNA(ia)) {
      stop("each interaction must be a length-2 character vector of variable names", call. = FALSE)
    }
    if (ia[1L] == ia[2L]) stop("an interaction needs two different variables", call. = FALSE)
    pos <- match(ia, vars)
    if (anyNA(pos)) {
      stop("interaction variable '", ia[is.na(pos)][1L], "' is not in the model (name the exposure or a term)",
           call. = FALSE)
    }
    if (!all(vapply(plan[pos], function(i) i$type %in% c("linear", "factor"), logical(1)))) {
      stop("interactions are between the exposure and linear or factor terms in P1c", call. = FALSE)
    }
    pos <- sort(pos)
    key <- paste(pos, collapse = ":")
    if (key %in% keys) next
    keys <- c(keys, key)
    out[[length(out) + 1L]] <- list(a = pos[1L], b = pos[2L])
  }
  out
}

#' Static coef_map: one row per derived column (coef, source_var, kind), from the spec alone. A factor
#' has one row whose coef is its prefix t<i>f; its coefficients are t<i>f<level>. An interaction row
#' has coef "<derived a>:<derived b>" (a factor part as its prefix), source_var "<var a>:<var b>",
#' kind "interaction". sr_coef_source() resolves real coefficient names against it.
.sr_coef_map <- function(plan, ia) {
  coef <- character()
  src <- character()
  kind <- character()
  for (it in plan) {
    cols <- .sr_term_cols(it)
    coef <- c(coef, cols)
    src <- c(src, rep(it$var, length(cols)))
    kind <- c(kind, rep(it$type, length(cols)))
  }
  for (p in ia) {
    coef <- c(coef, paste(.sr_term_cols(plan[[p$a]])[1L], .sr_term_cols(plan[[p$b]])[1L], sep = ":"))
    src <- c(src, paste(plan[[p$a]]$var, plan[[p$b]]$var, sep = ":"))
    kind <- c(kind, "interaction")
  }
  data.frame(coef = coef, source_var = src, kind = kind, stringsAsFactors = FALSE)
}

#' Map real coefficient names (e.g. names(coef(fit))) back to the user's variables.
#' Grammar of a coefficient name: t<i> (linear, or the first column of an rcs / quad term),
#' t<i>_s<j> (rcs), t<i>_q (quad), t<i>f<level> (factor, level = the remainder, which may itself
#' contain ":"), and "<part>:<part>" for an interaction.
#' Matching rule, in order: (1) an interaction reading wins: the name is split at each ":" and it
#' counts when both halves resolve as single parts and the pair is an interaction row of coef_map;
#' (2) only when no interaction reading exists, a direct reading: exact match, else factor prefix
#' with the remainder as the level. A name that resolves to nothing, or to more than one reading
#' within the winning class, is an error. Interactions win because their factor-first form
#' ("t1fb:t2") would otherwise also read as factor t1f with level "b:t2". The one name that stays
#' unrecoverable from text alone is a main-effect level spelling ":<derived name>" of a variable that
#' also has that interaction; it reads as the interaction.
#' @param coef_names character vector of coefficient names (without the intercept)
#' @param coef_map the `coef_map` of sr_terms_code()
#' @return data.frame(coef, source_var, kind, level). `level` is the factor level of a factor
#'   coefficient, NA for the others. For an interaction it is the level of its factor part, or the two
#'   part levels joined by ":" when both parts are factors; NA when neither is.
sr_coef_source <- function(coef_names, coef_map) {
  coef_names <- as.character(coef_names)
  base <- coef_map[coef_map$kind != "interaction", , drop = FALSE]
  inter <- coef_map[coef_map$kind == "interaction", , drop = FALSE]
  hits <- function(p) {
    unique(c(which(base$coef == p & base$kind != "factor"),
             which(base$kind == "factor" & startsWith(p, base$coef) & nchar(p) > nchar(base$coef))))
  }
  level_of <- function(row, p) {
    if (base$kind[row] == "factor") substring(p, nchar(base$coef[row]) + 1L) else NA_character_
  }
  one <- function(nm) {
    inter_c <- list()
    direct_c <- list()
    for (r in hits(nm)) {
      direct_c[[length(direct_c) + 1L]] <- list(source_var = base$source_var[r], kind = base$kind[r], level = level_of(r, nm))
    }
    pos <- gregexpr(":", nm, fixed = TRUE)[[1L]]
    for (k in pos[pos > 0L]) {
      left <- substr(nm, 1L, k - 1L)
      right <- substring(nm, k + 1L)
      hl <- hits(left)
      hr <- hits(right)
      if (length(hl) != 1L || length(hr) != 1L) next
      ii <- which(inter$coef == paste(base$coef[hl], base$coef[hr], sep = ":"))
      if (length(ii) != 1L) next
      lv <- c(level_of(hl, left), level_of(hr, right))
      lv <- lv[!is.na(lv)]
      inter_c[[length(inter_c) + 1L]] <- list(source_var = inter$source_var[ii], kind = "interaction",
                                              level = if (length(lv)) paste(lv, collapse = ":") else NA_character_)
    }
    cand <- if (length(inter_c)) inter_c else direct_c
    if (length(cand) == 0L) stop("coefficient '", nm, "' does not belong to any term of the model", call. = FALSE)
    if (length(cand) > 1L) stop("coefficient '", nm, "' matches more than one term", call. = FALSE)
    cand[[1L]]
  }
  res <- lapply(coef_names, one)
  data.frame(
    coef = coef_names,
    source_var = vapply(res, function(r) r$source_var, character(1)),
    kind = vapply(res, function(r) r$kind, character(1)),
    level = vapply(res, function(r) r$level, character(1)),
    stringsAsFactors = FALSE
  )
}

# ---- code emission ------------------------------------------------------------------------------

#' Code for the term builder of a spec (no data needed: levels and knots are fixed by the emitted
#' setup from `sr_data` when it runs).
#' @param spec list with exposure, exposure_ref, terms, interactions (and the columns the passthrough
#'   needs: outcome, time, status, cluster, strata)
#' @return list(setup, design, rhs, coef_map). Interactions are named by variable and refer to the
#'   exposure or a linear / factor term. See .sr_coef_map() and sr_coef_source() for coef_map.
sr_terms_code <- function(spec) {
  plan <- .sr_term_plan(spec)
  ia <- .sr_interaction_plan(spec, plan)
  types <- vapply(plan, function(i) i$type, character(1))

  rhs_parts <- unlist(lapply(plan, .sr_term_cols))
  rhs_parts <- c(rhs_parts, vapply(ia, function(p) {
    paste(.sr_term_cols(plan[[p$a]])[1L], .sr_term_cols(plan[[p$b]])[1L], sep = ":")
  }, character(1)))
  rhs <- if (length(rhs_parts)) paste(rhs_parts, collapse = " + ") else "1"

  pass <- unique(c(.sr_spec_chr(spec[["outcome"]]), .sr_spec_chr(spec[["time"]]),
                   .sr_spec_chr(spec[["status"]]), .sr_spec_chr(spec[["cluster"]]),
                   .sr_spec_chr(spec[["strata"]])))
  clash <- pass[.sr_reserved_col(pass)]
  if (length(clash)) {
    stop("column '", clash[1L], "' has the name of a derived design column (t1, t1f, t1_s1, t1_q, .y, ...); rename it",
         call. = FALSE)
  }
  needed <- unique(c(pass, vapply(plan, function(i) i$var, character(1))))

  # setup: the column check, helpers, then .sr_fixed (factor levels, RCS knots) from the observed table
  setup <- c(
    if (length(needed)) c(
      sprintf(".miss <- setdiff(%s, names(sr_data))", .sr_lit(needed)),
      "if (length(.miss)) stop(\"column(s) not in the data: \", paste(.miss, collapse = \", \"))"
    ),
    if (any(types == "factor")) .sr_inline(.h_levels, ".h_levels"),
    if (any(types != "factor")) .sr_inline(.h_num, ".h_num"),
    if (any(types == "rcs")) .sr_inline(.h_knots, ".h_knots"),
    ".sr_fixed <- list(levels = list(), knots = list())"
  )
  for (it in plan) {
    col <- paste0("sr_data[[", .sr_lit(it$var), "]]")
    if (it$type == "factor") {
      ref <- if (is.null(it$ref)) "NULL" else .sr_lit(as.character(it$ref))
      setup <- c(setup, sprintf(".sr_fixed$levels[[%s]] <- .h_levels(%s, %s)", .sr_lit(it$id), col, ref))
    } else if (it$type == "rcs") {
      kn <- if (is.null(it$knots$explicit)) {
        sprintf(".h_knots(.h_num(%s, %s), %s)", col, .sr_lit(it$var), .sr_lit(it$knots$nk))
      } else {
        .sr_lit(it$knots$explicit)
      }
      setup <- c(setup, sprintf(".sr_fixed$knots[[%s]] <- %s", .sr_lit(it$id), kn))
    }
  }

  # design: .h_design(d)
  body <- c(
    sprintf("  .miss <- setdiff(%s, names(d))", .sr_lit(needed)),
    "  if (length(.miss)) stop(\"columns not found in the data: \", paste(.miss, collapse = \", \"))",
    sprintf("  out <- as.data.frame(d)[%s]", .sr_lit(pass))
  )
  for (it in plan) {
    id <- it$id
    body <- c(body, sprintf("  x <- d[[%s]]", .sr_lit(it$var)))
    set <- function(nm, expr) sprintf("  out[[%s]] <- %s", .sr_lit(nm), expr)
    body <- c(body, switch(it$type,
      linear = set(id, sprintf(".h_num(x, %s)", .sr_lit(it$var))),
      factor = set(paste0(id, "f"), sprintf("factor(as.character(x), levels = .sr_fixed$levels[[%s]])", .sr_lit(id))),
      quad = c(set(id, sprintf(".h_num(x, %s)", .sr_lit(it$var))),
               set(paste0(id, "_q"), sprintf("out[[%s]]^2", .sr_lit(id)))),
      rcs = c(
        sprintf("  b <- Hmisc::rcspline.eval(.h_num(x, %s), knots = .sr_fixed$knots[[%s]], inclx = TRUE)",
                .sr_lit(it$var), .sr_lit(id)),
        set(id, "as.numeric(b[, 1L])"),
        vapply(seq_len(it$knots$nk - 2L), function(j) set(paste0(id, "_s", j), sprintf("as.numeric(b[, %dL])", j + 1L)),
               character(1))
      )
    ))
  }
  design <- paste(c(".h_design <- function(d) {", body, "  out", "}"), collapse = "\n")

  list(setup = paste(setup, collapse = "\n"), design = design, rhs = rhs,
       coef_map = .sr_coef_map(plan, ia))
}

#' Code adding the outcome to the design frame `des` (the value of .h_design(...)):
#'   binary     .y  0/1 integer: `outcome == event_value`, or the outcome itself when it is logical or 0/1
#'   continuous .y  numeric
#'   surv       .time numeric and .event 0/1 integer (`status == status_event`, or status when 0/1)
#' NA stays NA. It refuses a missing event value at run time when the column is not 0/1 or logical.
sr_outcome_code <- function(spec, kind = c("binary", "continuous", "surv")) {
  kind <- match.arg(kind)
  col <- function(field) {
    paste0("des[[", .sr_lit(.sr_one_string(spec[[field]], paste0("spec$", field))), "]]")
  }
  ev <- function(field, value) {
    v <- spec[[value]]
    if (is.null(v)) return(sprintf(".h_bin(%s)", col(field)))
    if (!is.atomic(v) || length(v) != 1L || is.na(v)) {
      stop(value, " must be one non-NA value", call. = FALSE)
    }
    sprintf("as.integer(%s == %s)", col(field), .sr_lit(v))
  }
  switch(kind,
    binary = paste(
      if (is.null(spec[["event_value"]])) .sr_inline(.h_bin, ".h_bin"),
      sprintf("des[[\".y\"]] <- %s", ev("outcome", "event_value")), sep = "\n"),
    continuous = paste(
      .sr_inline(.h_num, ".h_num"),
      sprintf("des[[\".y\"]] <- .h_num(%s, %s)", col("outcome"), .sr_lit(spec[["outcome"]])), sep = "\n"),
    surv = paste(
      .sr_inline(.h_num, ".h_num"),
      if (is.null(spec[["status_event"]])) .sr_inline(.h_bin, ".h_bin"),
      sprintf("des[[\".time\"]] <- .h_num(%s, %s)", col("time"), .sr_lit(spec[["time"]])),
      sprintf("des[[\".event\"]] <- %s", ev("status", "status_event")), sep = "\n")
  )
}
