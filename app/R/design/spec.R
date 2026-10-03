# design/spec.R - the route contract, the route registry and spec validation.
#
# A route compiles a typed spec (a plain list) into a string of R code. Every route carries:
#   * a purpose (describe / predict / explain-causal): a spec only runs on a route of the same
#     purpose, so a prediction spec can never be silently analysed by a causal route (M1.3);
#   * an estimand (a fixed value, or a function of the spec): what is being estimated, in which
#     population, with which intercurrent-event strategy (M1.2);
#   * a spec_schema, compile() and present().
# The registry is a package-private environment keyed "id@version".

#' The three analysis purposes a route can serve.
SR_PURPOSES <- c("describe", "predict", "explain-causal")

# Field types a spec_schema may declare, and the predicate each one means.
.sr_spec_types <- list(
  chr = is.character,
  num = is.numeric,
  lgl = is.logical,
  list = is.list
)

.sr_routes <- new.env(parent = emptyenv())

#' TRUE for a single non-NA, non-blank string.
.sr_is_text1 <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

#' TRUE for a non-empty character vector with no NA and no blank element.
.sr_is_text_vec <- function(x) {
  is.character(x) && length(x) >= 1L && !anyNA(x) && all(nzchar(trimws(x)))
}

.sr_estimand_levels <- c("marginal", "conditional", "not-applicable")

#' TRUE for a genuine estimand: the class plus all five fields valid (same rules as sr_estimand()).
.sr_is_estimand <- function(x) {
  inherits(x, "sr_estimand") && is.list(x) &&
    .sr_is_text1(x[["population"]]) && .sr_is_text_vec(x[["variables"]]) &&
    .sr_is_text1(x[["intercurrent"]]) && .sr_is_text1(x[["summary"]]) &&
    .sr_is_text1(x[["level"]]) && x[["level"]] %in% .sr_estimand_levels
}

#' Stop unless `id` is a single non-blank string and `version` a single finite whole number in
#' 1..integer max. Shared by sr_route() and sr_route_register().
.sr_check_id_version <- function(id, version) {
  if (!.sr_is_text1(id)) stop("route 'id' must be a single non-empty string", call. = FALSE)
  if (!is.numeric(version) || length(version) != 1L || !is.finite(version) || version < 1 ||
      version > .Machine$integer.max || version != floor(version)) {
    stop("route 'version' must be a single whole number from 1 to ", .Machine$integer.max,
         " (route '", id, "')", call. = FALSE)
  }
  invisible(TRUE)
}

#' Build an estimand: who/what is estimated, how intercurrent events are handled, which summary.
#' Every field is required and non-empty; `variables` may name several variables.
sr_estimand <- function(population, variables, intercurrent, summary,
                        level = .sr_estimand_levels) {
  level <- match.arg(level)
  if (missing(population) || !.sr_is_text1(population)) {
    stop("sr_estimand: 'population' must be a single non-empty string", call. = FALSE)
  }
  if (missing(variables) || !.sr_is_text_vec(variables)) {
    stop("sr_estimand: 'variables' must be a non-empty character vector with no blanks", call. = FALSE)
  }
  if (missing(intercurrent) || !.sr_is_text1(intercurrent)) {
    stop("sr_estimand: 'intercurrent' must be a single non-empty string", call. = FALSE)
  }
  if (missing(summary) || !.sr_is_text1(summary)) {
    stop("sr_estimand: 'summary' must be a single non-empty string", call. = FALSE)
  }
  structure(list(population = population, variables = variables, intercurrent = intercurrent,
                 summary = summary, level = level), class = "sr_estimand")
}

#' Build a route object. Shape checks on id and version only; sr_route_register() enforces the
#' contract (purpose, estimand, compile, present, schema, uniqueness).
sr_route <- function(id, version, family, purpose = NULL, estimand = NULL, spec_schema = list(),
                     compile = NULL, present = NULL, composes_with = character(),
                     applicable = function(spec, ctx) list(ok = TRUE, reasons = character())) {
  .sr_check_id_version(if (missing(id)) NULL else id, if (missing(version)) NULL else version)
  if (missing(family)) family <- NA_character_
  structure(list(id = id, version = as.integer(version), family = family, purpose = purpose,
                 estimand = estimand, spec_schema = spec_schema, compile = compile,
                 present = present, composes_with = composes_with, applicable = applicable),
            class = "sr_route")
}

#' A problem with a spec_schema, as a message, or NULL when it is well formed.
.sr_schema_problem <- function(schema) {
  if (!is.list(schema)) return("spec_schema must be a list")
  if (!length(schema)) return(NULL)
  nms <- names(schema)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(nms))) {
    return("spec_schema must be a fully named list (field -> definition)")
  }
  if (anyDuplicated(nms)) {
    return(paste0("spec_schema has duplicate fields: ", paste(unique(nms[duplicated(nms)]), collapse = ", ")))
  }
  for (f in nms) {
    d <- schema[[f]]
    if (!is.list(d)) return(paste0("spec_schema field '", f, "' must be a list(type, required)"))
    type <- d[["type"]]
    if (!is.character(type) || length(type) != 1L || is.na(type) || !(type %in% names(.sr_spec_types))) {
      return(paste0("spec_schema field '", f, "': type must be one of ",
                    paste(names(.sr_spec_types), collapse = ", ")))
    }
    req <- d[["required"]]
    if (!is.logical(req) || length(req) != 1L || is.na(req)) {
      return(paste0("spec_schema field '", f, "': required must be a single TRUE/FALSE"))
    }
    vals <- d[["values"]]
    if (!is.null(vals) && !(is.character(vals) && length(vals) >= 1L && !anyNA(vals))) {
      return(paste0("spec_schema field '", f, "': values must be a non-empty character vector"))
    }
  }
  NULL
}

#' Empty the route registry (tests).
.sr_routes_reset <- function() {
  rm(list = ls(.sr_routes, all.names = TRUE), envir = .sr_routes)
  invisible(NULL)
}

.sr_route_key <- function(id, version) paste0(id, "@", version)

#' Register a route, enforcing the contract. Errors name the broken rule.
sr_route_register <- function(route) {
  if (!inherits(route, "sr_route")) stop("sr_route_register: not an sr_route", call. = FALSE)
  .sr_check_id_version(route[["id"]], route[["version"]])
  route$version <- as.integer(route$version)  # whole doubles are stored (and keyed) as integers
  key <- .sr_route_key(route$id, route$version)
  if (!.sr_is_text1(route$family)) {
    stop("sr_route_register: route '", key, "' needs a single non-empty family", call. = FALSE)
  }
  p <- route$purpose
  if (!is.character(p) || length(p) != 1L || is.na(p) || !(p %in% SR_PURPOSES)) {
    stop("sr_route_register: route '", key, "' needs a purpose, one of: ",
         paste(SR_PURPOSES, collapse = ", "), call. = FALSE)
  }
  e <- route$estimand
  if (!(.sr_is_estimand(e) || is.function(e))) {
    stop("sr_route_register: route '", key,
         "' needs an estimand (an sr_estimand or a function of the spec returning one)", call. = FALSE)
  }
  if (!is.function(route$compile)) {
    stop("sr_route_register: route '", key, "' needs a compile function", call. = FALSE)
  }
  if (!is.function(route$present)) {
    stop("sr_route_register: route '", key, "' needs a present function", call. = FALSE)
  }
  if (!is.character(route$composes_with) || anyNA(route$composes_with)) {
    stop("sr_route_register: route '", key, "': composes_with must be a character vector", call. = FALSE)
  }
  if (!is.function(route$applicable)) {
    stop("sr_route_register: route '", key, "' needs an applicable function", call. = FALSE)
  }
  problem <- .sr_schema_problem(route$spec_schema)
  if (!is.null(problem)) {
    stop("sr_route_register: route '", key, "': ", problem, call. = FALSE)
  }
  if (exists(key, envir = .sr_routes, inherits = FALSE)) {
    stop("sr_route_register: duplicate route ", key, call. = FALSE)
  }
  assign(key, route, envir = .sr_routes)
  invisible(route)
}

#' Fetch a route by id: the latest version, or the exact `version`. Errors if absent.
sr_route_get <- function(id, version = NULL) {
  if (!.sr_is_text1(id)) stop("sr_route_get: 'id' must be a single non-empty string", call. = FALSE)
  routes <- mget(ls(.sr_routes, all.names = TRUE), envir = .sr_routes)
  mine <- Filter(function(r) identical(r$id, id), routes)
  if (!length(mine)) stop("sr_route_get: no route with id '", id, "'", call. = FALSE)
  versions <- vapply(mine, function(r) r$version, integer(1), USE.NAMES = FALSE)
  if (is.null(version)) return(mine[[which.max(versions)]])
  if (!is.numeric(version) || length(version) != 1L || is.na(version)) {
    stop("sr_route_get: 'version' must be a single number (route '", id, "')", call. = FALSE)
  }
  hit <- which(versions == version)
  if (!length(hit)) {
    stop("sr_route_get: route '", id, "' has no version ", version, " (have ",
         paste(sort(versions), collapse = ", "), ")", call. = FALSE)
  }
  mine[[hit[1L]]]
}

#' Every registered route as a data frame (id, version, family, purpose), sorted by id then version.
sr_routes <- function() {
  routes <- mget(ls(.sr_routes, all.names = TRUE), envir = .sr_routes, inherits = FALSE)
  col <- function(f, type) vapply(unname(routes), f, type)
  tab <- data.frame(id = col(function(r) r$id, character(1)),
                    version = col(function(r) r$version, integer(1)),
                    family = col(function(r) r$family, character(1)),
                    purpose = col(function(r) r$purpose, character(1)),
                    stringsAsFactors = FALSE)
  tab <- tab[order(tab$id, tab$version, method = "radix"), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}

#' Validate a spec against a route. Returns list(ok, errors) with every problem found, each naming
#' its field. Checks the purpose (M1.3), the schema's required fields, types and allowed values,
#' and that the spec can be canonically hashed. Fields the schema does not name are allowed. A
#' function-form estimand is evaluated (only once the spec is otherwise valid) and must return an
#' sr_estimand (M1.2).
sr_spec_validate <- function(spec, route) {
  errors <- character()
  add <- function(...) errors <<- c(errors, paste0(...))
  if (!is.list(spec)) {
    add("spec must be a list")
    return(list(ok = FALSE, errors = errors))
  }
  sp <- spec[["purpose"]]
  if (is.null(sp)) {
    add("purpose: spec has no purpose; route '", route$id, "' needs '", route$purpose, "'")
  } else if (!identical(sp, route$purpose)) {
    shown <- if (is.character(sp) && length(sp) == 1L && !is.na(sp)) sp else "<invalid>"
    add("purpose: spec purpose '", shown, "' does not match route '", route$id, "' purpose '",
        route$purpose, "'")
  }
  schema <- route$spec_schema
  for (f in names(schema)) {
    d <- schema[[f]]
    x <- spec[[f]]
    # NULL, length 0 and all-NA values count as missing for a required field
    missing_like <- is.null(x) || length(x) == 0L || (is.atomic(x) && all(is.na(x)))
    if (is.null(x) || (isTRUE(d[["required"]]) && missing_like)) {
      if (isTRUE(d[["required"]])) add("field '", f, "' is required")
      next
    }
    if (!isTRUE(.sr_spec_types[[d[["type"]]]](x))) {
      add("field '", f, "' must be of type ", d[["type"]])
      next
    }
    if (d[["type"]] == "chr" && (anyNA(x) || any(!nzchar(trimws(x))))) {
      add("field '", f, "'", if (isTRUE(d[["required"]])) " is required:" else "",
          " must not contain NA or blank values")
      next
    }
    # values is ignored for list fields; a length-0 value never satisfies it
    if (!is.null(d[["values"]]) && !is.list(x) && (length(x) == 0L || !all(x %in% d[["values"]]))) {
      add("field '", f, "' must be one of: ", paste(d[["values"]], collapse = ", "))
    }
  }
  hashable <- tryCatch({
    sr_canonical_json(spec)
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(hashable)) add("spec cannot be canonically serialised: ", hashable)
  if (!length(errors) && is.function(route$estimand)) {
    est <- tryCatch(route$estimand(spec), error = function(e) e)
    if (inherits(est, "error")) {
      add("estimand function failed: ", conditionMessage(est))
    } else if (!.sr_is_estimand(est)) {
      add("estimand function must return an sr_estimand")
    }
  }
  list(ok = length(errors) == 0L, errors = errors)
}

#' Compile a spec into R code on a route: validate first, then call route$compile(spec), which
#' must return a single non-NA string.
sr_compile <- function(route, spec) {
  v <- sr_spec_validate(spec, route)
  if (!v$ok) stop(paste(v$errors, collapse = "; "), call. = FALSE)
  code <- route$compile(spec)
  if (!is.character(code) || length(code) != 1L || is.na(code)) {
    stop("sr_compile: route '", route$id, "' compile() must return a single non-NA character string",
         call. = FALSE)
  }
  code
}
