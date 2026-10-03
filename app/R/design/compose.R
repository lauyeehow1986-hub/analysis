# Composition matrix: which analysis layers may be combined, and what each combination demands.
#
# A design spec carries `spec$layers`, a list of six layer fields:
#   missing    "cc" | "mi"
#   weights    "none" | "ipw" | "overlap" | "survey"
#   matching   TRUE | FALSE
#   resampling "none" | "bootstrap"
#   framework  "frequentist" | "bayesian" | "both"
#   clustered  TRUE | FALSE
# plus optional top-level fields that satisfy requirements (`choices`, `pool`, `boot_type`,
# `refit_ps_each_rep`, `design_vars_in_imputation`). The rules are data (sr_compose_rules); an
# invalid stack is refused, or the spec must carry an explicit choice or requirement (spec rule
# M1.5). This file never modifies the spec.

SR_LAYER_SETS <- list(
  missing = c("cc", "mi"),
  weights = c("none", "ipw", "overlap", "survey"),
  resampling = c("none", "bootstrap"),
  framework = c("frequentist", "bayesian", "both")
)
SR_LAYER_FLAGS <- c("matching", "clustered")

# TRUE when x is a single non-NA string that is one of `set`.
.sr_in_set <- function(x, set) {
  is.character(x) && length(x) == 1L && !is.na(x) && x %in% set
}

# One rule. `when(L)` takes the validated layers list; `require(spec)` (outcome "require" only)
# returns a single TRUE/FALSE.
.sr_rule <- function(id, when, outcome, msg, options = character(0), require = NULL) {
  list(id = id, when = when, outcome = outcome, options = options, require = require, msg = msg)
}

#' The composition rules, in table order.
sr_compose_rules <- function() {
  list(
    .sr_rule("mi_x_ps",
             function(L) L$missing == "mi" && L$weights %in% c("ipw", "overlap"),
             "choice",
             paste("multiple imputation with propensity weights: choose MIte (estimate the",
                   "effect in each imputed set, then pool) or MIps (pool the propensity",
                   "scores across imputations, then estimate once)"),
             options = c("MIte", "MIps")),
    .sr_rule("mi_x_boot",
             function(L) L$missing == "mi" && L$resampling == "bootstrap",
             "choice",
             paste("multiple imputation with the bootstrap: choose boot_then_impute or",
                   "impute_then_boot"),
             options = c("boot_then_impute", "impute_then_boot")),
    .sr_rule("bayes_x_boot",
             function(L) L$framework %in% c("bayesian", "both") && L$resampling == "bootstrap",
             "refuse",
             "bayesian with bootstrap refused: the posterior already carries uncertainty"),
    .sr_rule("bayes_x_mi",
             function(L) L$framework == "bayesian" && L$missing == "mi",
             "require",
             "bayesian with multiple imputation: pool draws, not Rubin (set pool = draws)",
             require = function(spec) .sr_in_set(spec[["pool"]], "draws")),
    .sr_rule("match_x_boot",
             function(L) isTRUE(L$matching) && L$resampling == "bootstrap",
             "require",
             paste("matching with the bootstrap: the naive bootstrap is invalid after matching",
                   "(Abadie-Imbens); set boot_type to pair or cluster"),
             require = function(spec) .sr_in_set(spec[["boot_type"]], c("pair", "cluster"))),
    .sr_rule("ipw_x_boot",
             function(L) L$weights %in% c("ipw", "overlap") && L$resampling == "bootstrap",
             "require",
             "propensity weights with the bootstrap: refit the propensity model in each replicate",
             require = function(spec) isTRUE(spec[["refit_ps_each_rep"]])),
    .sr_rule("survey_x_mi",
             function(L) L$weights == "survey" && L$missing == "mi",
             "require",
             "survey weights with multiple imputation: include the design variables in the imputation model",
             require = function(spec) isTRUE(spec[["design_vars_in_imputation"]])),
    .sr_rule("survey_x_boot",
             function(L) L$weights == "survey" && L$resampling == "bootstrap",
             "require",
             "survey weights with the bootstrap: use replicate weights (set boot_type = replicate_weights)",
             require = function(spec) .sr_in_set(spec[["boot_type"]], "replicate_weights")),
    .sr_rule("cluster_x_boot",
             function(L) isTRUE(L$clustered) && L$resampling == "bootstrap",
             "require",
             paste("clustered data with the bootstrap: resample whole clusters, or use pair or",
                   "replicate weights (set boot_type)"),
             require = function(spec) {
               .sr_in_set(spec[["boot_type"]], c("cluster", "pair", "replicate_weights"))
             }),
    .sr_rule("overlap_target",
             function(L) L$weights == "overlap",
             "note",
             "target population is ATO (overlap weights)")
  )
}

# Validate spec$layers and return the six layer fields (unknown extra names dropped).
.sr_layers <- function(spec) {
  if (!is.list(spec)) stop("sr_compose_check: spec must be a list", call. = FALSE)
  L <- spec[["layers"]]
  if (is.null(L) || !is.list(L)) {
    stop("sr_compose_check: spec$layers must be a list of layer fields", call. = FALSE)
  }
  for (f in names(SR_LAYER_SETS)) {
    v <- L[[f]]
    if (!.sr_in_set(v, SR_LAYER_SETS[[f]])) {
      stop(sprintf("sr_compose_check: layers$%s must be one of: %s", f,
                   paste(SR_LAYER_SETS[[f]], collapse = ", ")), call. = FALSE)
    }
  }
  for (f in SR_LAYER_FLAGS) {
    v <- L[[f]]
    if (!(is.logical(v) && length(v) == 1L && !is.na(v))) {
      stop(sprintf("sr_compose_check: layers$%s must be a single TRUE or FALSE", f), call. = FALSE)
    }
  }
  L[c(names(SR_LAYER_SETS), SR_LAYER_FLAGS)]
}

# The value given for choice rule `id` (exact name match), or NULL when none is given.
.sr_choice_value <- function(spec, id) {
  ch <- spec[["choices"]]
  if (is.null(ch) || !is.vector(ch) || length(ch) == 0L) return(NULL)
  nm <- names(ch)
  if (is.null(nm) || !(id %in% nm)) return(NULL)
  ch[[match(id, nm)]]
}

#' Check a design spec's layer combination.
#' Returns list(ok, refused, choices, unmet, notes):
#'   refused  ids of triggered refuse rules
#'   choices  named list id -> options, for triggered choice rules still unresolved
#'   unmet    ids of triggered require rules whose requirement does not hold
#'   notes    messages of every refused, unmet, unresolved-choice and note rule
sr_compose_check <- function(spec) {
  L <- .sr_layers(spec)
  refused <- character(0)
  unmet <- character(0)
  choices <- list()
  notes <- character(0)
  for (r in sr_compose_rules()) {
    if (!isTRUE(r$when(L))) next
    if (r$outcome == "refuse") {
      refused <- c(refused, r$id)
      notes <- c(notes, r$msg)
    } else if (r$outcome == "choice") {
      if (!.sr_in_set(.sr_choice_value(spec, r$id), r$options)) {
        choices[[r$id]] <- r$options
        notes <- c(notes, r$msg)
      }
    } else if (r$outcome == "require") {
      if (!isTRUE(r$require(spec))) {
        unmet <- c(unmet, r$id)
        notes <- c(notes, r$msg)
      }
    } else if (r$outcome == "note") {
      notes <- c(notes, r$msg)
    }
  }
  list(ok = length(refused) == 0L && length(choices) == 0L && length(unmet) == 0L,
       refused = refused, choices = choices, unmet = unmet, notes = notes)
}
