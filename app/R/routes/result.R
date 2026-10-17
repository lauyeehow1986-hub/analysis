# routes/result.R - the result contract. A route's present() returns
# list(tables, figures, fragments, effects); sr_result_check() validates it and adds the reporting
# flags (Table 2 and non-collapsibility). effects is the one frame every downstream consumer reads.

SR_EFFECT_COLS <- c("term", "measure", "est", "lo", "hi", "p", "scale", "level", "mcse", "role",
                    "conditional", "ci_method", "n", "events")
SR_EFFECT_MEASURES <- c("MD", "OR", "RR", "RD", "HR", "RMST", "RMST_diff", "NNT", "SMD", "coef", "risk")
SR_EFFECT_SCALES <- c("ratio", "difference")
SR_EFFECT_ROLES <- c("primary", "covariate", "descriptive")

#' A zero-row effects frame with the contract's columns and types.
sr_effects_empty <- function() {
  data.frame(term = character(), measure = character(), est = numeric(), lo = numeric(),
             hi = numeric(), p = numeric(), scale = character(), level = character(),
             mcse = numeric(), role = character(), conditional = logical(), ci_method = character(),
             n = numeric(), events = numeric(), stringsAsFactors = FALSE)
}

.sr_flags_empty <- function() {
  data.frame(term = character(), flag = character(), detail = character(), stringsAsFactors = FALSE)
}

#' Validate a presented result against the contract and add `$flags`.
#'
#' Requires `tables`, `figures`, `fragments` and `effects`. `effects` must carry exactly
#' SR_EFFECT_COLS (a missing or extra column, or an unknown measure / scale / role value, is an
#' error; its columns are returned in canonical order) and no table may carry a column named like a
#' p-value. Every `primary` row needs finite est, lo and hi with lo <= est <= hi; routes of the
#' describe family are exempt from that check, and so are bootstrap rows (ci_method boot_*), whose
#' interval may exclude the estimate when the bootstrap distribution is strongly biased.
#' `flags` is a data.frame(term, flag, detail):
#'   table2          every `covariate` row of an explain-causal route (Table 2 fallacy);
#'   non_collapsible every `conditional` OR / HR row.
#' @param presented list returned by route$present()
#' @param route an sr_route
#' @return `presented` with `flags` set
sr_result_check <- function(presented, route) {
  if (!is.list(presented)) stop("sr_result_check: the presented result must be a list", call. = FALSE)
  miss <- setdiff(c("tables", "figures", "fragments", "effects"), names(presented))
  if (length(miss)) {
    stop("sr_result_check: the presented result lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  eff <- presented$effects
  if (!is.data.frame(eff)) stop("sr_result_check: effects must be a data.frame", call. = FALSE)
  lack <- setdiff(SR_EFFECT_COLS, names(eff))
  if (length(lack)) {
    stop("sr_result_check: effects lacks column(s): ", paste(lack, collapse = ", "), call. = FALSE)
  }
  extra <- c(setdiff(names(eff), SR_EFFECT_COLS), names(eff)[duplicated(names(eff))])
  if (length(extra)) {
    stop("sr_result_check: effects must have exactly the contract columns; unexpected: ",
         paste(unique(extra), collapse = ", "), call. = FALSE)
  }
  eff <- eff[, SR_EFFECT_COLS, drop = FALSE]

  for (nm in names(presented$tables)) {
    tb <- presented$tables[[nm]]
    if (is.data.frame(tb) && any(names(tb) %in% .sr_lint_pnames)) {
      stop("sr_result_check: table '", nm, "' carries a p-value column; the effects column is 'p'",
           call. = FALSE)
    }
  }

  if (!is.logical(eff$conditional)) stop("sr_result_check: effects$conditional must be logical", call. = FALSE)
  for (cn in c("est", "lo", "hi", "p", "mcse", "n", "events")) {
    if (!is.numeric(eff[[cn]]) && !all(is.na(eff[[cn]]))) {
      stop("sr_result_check: effects$", cn, " must be numeric", call. = FALSE)
    }
  }
  vocab <- function(col, ok, allow_na) {
    v <- as.character(eff[[col]])
    badv <- if (allow_na) !is.na(v) & !(v %in% ok) else is.na(v) | !(v %in% ok)
    if (any(badv)) {
      i <- which(badv)[1L]
      stop("sr_result_check: effects$", col, " must be one of ", paste(ok, collapse = " | "),
           "; row '", eff$term[i], "' has '", v[i], "'", call. = FALSE)
    }
  }
  vocab("measure", SR_EFFECT_MEASURES, FALSE)
  vocab("role", SR_EFFECT_ROLES, FALSE)
  vocab("scale", SR_EFFECT_SCALES, TRUE)

  describe <- identical(route$purpose, "describe") || identical(route$family, "describe")
  if (!describe) {
    for (i in which(eff$role == "primary")) {
      est <- eff$est[i]
      lo <- eff$lo[i]
      hi <- eff$hi[i]
      if (!all(is.finite(c(est, lo, hi)))) {
        stop("sr_result_check: primary effect '", eff$term[i], "' needs a finite estimate and CI",
             call. = FALSE)
      }
      if (!(lo <= est && est <= hi) && !isTRUE(startsWith(as.character(eff$ci_method[i]), "boot_"))) {
        stop("sr_result_check: primary effect '", eff$term[i], "' violates lo <= est <= hi",
             call. = FALSE)
      }
    }
  }

  flags <- .sr_flags_empty()
  add <- function(idx, flag, detail) {
    if (!length(idx)) return(invisible())
    flags <<- rbind(flags, data.frame(term = as.character(eff$term[idx]), flag = flag,
                                      detail = detail, stringsAsFactors = FALSE))
  }
  if (identical(route$purpose, "explain-causal")) {
    add(which(eff$role == "covariate"), "table2",
        "covariate coefficient: not an effect of that covariate (Table 2 fallacy)")
  }
  add(which(eff$measure %in% c("OR", "HR") & eff$conditional %in% TRUE), "non_collapsible",
      "conditional; see the marginal RD/RR")
  presented$effects <- eff
  presented$flags <- flags
  presented
}
