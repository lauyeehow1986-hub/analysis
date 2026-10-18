# writer/placeholders.R - the locked placeholder registry of the manuscript writer.
# A model (or a template) never writes a number: every number in the output is a placeholder token
# whose text is evaluated R output. The registry holds one row per placeholder, with the source it was
# derived from, so that sr_ph_verify() can re-derive every row from the project and compare.
#
# Tokens (the only {{...}} forms):
#   {{E<n>}}    a number or a numeric text (bookmarked in the document)
#   {{L<n>}}    a data-derived label: a variable, level, term or analysis id (not bookmarked)
#   {{C:<key>}} a citation, rendered as a Vancouver number [k] in order of first use
#
# A source is "<prefix>:<rest>"; the resolver registered for <prefix> turns it back into text:
#   effect:<analysis>:<row>:<field>        field est | lo_hi | p | n | events
#   fragment:<analysis>:<name>:<k>         the k-th number of that fragment of sr_result()
#   label:<text>                           the text itself

SR_PH_RE <- "\\{\\{(E[0-9]+|L[0-9]+|C:[a-z][a-z0-9_]*)\\}\\}"
SR_NUM_RE <- "(?<![A-Za-z0-9_.])[-]?[0-9]+(?:\\.[0-9]+)?%?(?![A-Za-z0-9_])"

# Small cells: a count of 1 to SR_SDC_K - 1 prints as "<5" (writer/sdc_out.R); zero is not suppressed.
SR_SDC_K <- 5L

# A whole number whose next word is one of these counts rows or people, so it can be a small cell.
# A knot count or a bootstrap B is therefore never suppressed.
SR_SDC_COUNT_WORDS <- "^(rows?|row\\(s\\)|events?|cases?|participants?|patients?|deaths?|subjects?|observations?|excluded)\\b"

SR_PH_KINDS <- c("est", "ci", "p", "count", "num", "text", "label")
SR_PH_COLS <- c("id", "kind", "text", "value", "analysis", "source", "scale", "null", "suppressed")

.sr_ph_rows_empty <- function() {
  data.frame(id = character(), kind = character(), text = character(), value = numeric(),
             analysis = character(), source = character(), scale = character(), null = numeric(),
             suppressed = logical(), stringsAsFactors = FALSE)
}

#' A new, empty placeholder registry: an environment of class "sr_ph" whose `rows` is a data.frame
#' (id, kind, text, value, analysis, source, scale, null, suppressed).
sr_ph_registry <- function() {
  reg <- new.env(parent = emptyenv())
  reg$rows <- .sr_ph_rows_empty()
  class(reg) <- "sr_ph"
  reg
}

.sr_ph_check_reg <- function(reg) {
  if (!inherits(reg, "sr_ph")) stop("`reg` must be a placeholder registry (sr_ph_registry())", call. = FALSE)
  invisible(reg)
}

.sr_ph_one <- function(x, name, allow_na = FALSE) {
  if (!(is.character(x) && length(x) == 1L && (allow_na || !is.na(x)))) {
    stop("`", name, "` must be a single string", call. = FALSE)
  }
  x
}

.sr_ph_num1 <- function(x, name) {
  if (!(is.numeric(x) || (is.logical(x) && is.na(x))) || length(x) != 1L) {
    stop("`", name, "` must be a single number or NA", call. = FALSE)
  }
  as.numeric(x)
}

#' Put a value into the registry and return its token ("{{E1}}", "{{L1}}").
#'
#' An existing (source, kind) pair returns its token unchanged. kind "label" takes the next L number,
#' every other kind the next E number; numbers follow the order of first put, so the same build order
#' gives the same ids.
#' @param text the text the token resolves to
#' @param kind est, ci, p, count, num, text or label
#' @param source where the text comes from (see the header of this file)
#' @param analysis the analysis id the value belongs to, or NA
#' @param value the numeric value, or NA
#' @param scale "ratio", "difference" or NA
#' @param null the no-effect value (1 or 0) for the guard's direction rule, or NA
#' @param suppressed TRUE when the text is a small-cell mask ("<5") of a count
#' @return the token
sr_ph_put <- function(reg, text, kind, source, analysis = NA, value = NA, scale = NA, null = NA,
                      suppressed = FALSE) {
  .sr_ph_check_reg(reg)
  .sr_ph_one(text, "text")
  .sr_ph_one(kind, "kind")
  if (!(kind %in% SR_PH_KINDS)) {
    stop("`kind` must be one of ", paste(SR_PH_KINDS, collapse = ", "), call. = FALSE)
  }
  .sr_ph_one(source, "source")
  analysis <- if (length(analysis) == 1L && is.na(analysis)) NA_character_ else .sr_ph_one(as.character(analysis), "analysis")
  scale <- if (length(scale) == 1L && is.na(scale)) NA_character_ else .sr_ph_one(as.character(scale), "scale")
  if (!is.na(scale) && !(scale %in% c("ratio", "difference"))) {
    stop("`scale` must be ratio, difference or NA", call. = FALSE)
  }
  value <- .sr_ph_num1(value, "value")
  null <- .sr_ph_num1(null, "null")
  if (!(is.logical(suppressed) && length(suppressed) == 1L && !is.na(suppressed))) {
    stop("`suppressed` must be TRUE or FALSE", call. = FALSE)
  }
  rows <- reg$rows
  hit <- which(rows$source == source & rows$kind == kind)
  if (length(hit)) return(paste0("{{", rows$id[hit[1L]], "}}"))
  pre <- if (identical(kind, "label")) "L" else "E"
  n <- sum(startsWith(rows$id, pre)) + 1L
  id <- paste0(pre, n)
  reg$rows <- rbind(rows, data.frame(id = id, kind = kind, text = text, value = value, analysis = analysis,
                                     source = source, scale = scale, null = null, suppressed = suppressed,
                                     stringsAsFactors = FALSE))
  paste0("{{", id, "}}")
}

#' The registry rows in id order: the E rows, then the L rows, each by number.
sr_ph_table <- function(reg) {
  .sr_ph_check_reg(reg)
  rows <- reg$rows
  if (!nrow(rows)) return(rows)
  num <- as.integer(substring(rows$id, 2L))
  out <- rows[order(startsWith(rows$id, "L"), num), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Replace every E / L token of `x` with its text. `cite` is a function(key) returning "[k]"; when it
#' is NULL the C tokens are dropped. An unknown token is an error naming it.
sr_ph_resolve <- function(x, reg, cite = NULL) {
  .sr_ph_check_reg(reg)
  if (!is.null(cite) && !is.function(cite)) stop("`cite` must be NULL or a function(key)", call. = FALSE)
  rows <- reg$rows
  one <- function(s) {
    if (is.na(s)) return(NA_character_)
    m <- gregexpr(SR_PH_RE, s, perl = TRUE)
    toks <- regmatches(s, m)[[1L]]
    if (!length(toks)) return(s)
    inner <- sub("\\}\\}$", "", sub("^\\{\\{", "", toks))
    rep <- vapply(inner, function(t) {
      if (startsWith(t, "C:")) {
        if (is.null(cite)) return("")
        return(as.character(cite(substring(t, 3L))))
      }
      i <- match(t, rows$id)
      if (is.na(i)) stop("unknown placeholder {{", t, "}}", call. = FALSE)
      rows$text[i]
    }, character(1), USE.NAMES = FALSE)
    regmatches(s, m) <- list(rep)
    s
  }
  vapply(as.character(x), one, character(1), USE.NAMES = FALSE)
}

#' The SR_NUM_RE matches of `text` as a list of start / length / text per string.
.sr_ph_num_matches <- function(text) {
  m <- gregexpr(SR_NUM_RE, text, perl = TRUE)[[1L]]
  if (m[1L] == -1L) return(list(start = integer(), len = integer(), txt = character()))
  len <- attr(m, "match.length")
  list(start = as.integer(m), len = as.integer(len), txt = substring(text, m, m + len - 1L))
}

#' Replace every number of `text` (SR_NUM_RE) with a registered placeholder.
#'
#' The source of match k (its position in `text`) is paste0(source_prefix, ":", k). A whole number
#' without "%" whose next word (skipping one "(") matches SR_SDC_COUNT_WORDS is a `count`, every other
#' number a `num`. The value is the parsed number, the "%" dropped. A digit inside a name (z1, age_10)
#' is not a number and is left alone.
#' @param text one string
#' @param source_prefix e.g. "fragment:a_lr:glm_note"
#' @param analysis the analysis id the numbers belong to
#' @return the text with tokens
sr_ph_lift <- function(text, reg, source_prefix, analysis) {
  .sr_ph_check_reg(reg)
  .sr_ph_one(text, "text")
  .sr_ph_one(source_prefix, "source_prefix")
  mm <- .sr_ph_num_matches(text)
  if (!length(mm$txt)) return(text)
  toks <- character(length(mm$txt))
  for (k in seq_along(mm$txt)) {
    t <- mm$txt[k]
    after <- sub("^\\(", "", sub("^[[:space:]]+", "", substring(text, mm$start[k] + mm$len[k])))
    is_count <- grepl("^[0-9]+$", t) && grepl(SR_SDC_COUNT_WORDS, after, perl = TRUE)
    toks[k] <- sr_ph_put(reg, t, if (is_count) "count" else "num", paste0(source_prefix, ":", k),
                         analysis = analysis, value = as.numeric(sub("%$", "", t)))
  }
  m <- gregexpr(SR_NUM_RE, text, perl = TRUE)
  regmatches(text, m) <- list(toks)
  text
}

#' The digits (SR_NUM_RE matches) a template still shows once its placeholder tokens are removed.
sr_ph_digits <- function(template) {
  bare <- gsub(SR_PH_RE, " ", as.character(template), perl = TRUE)
  unlist(lapply(bare, function(s) .sr_ph_num_matches(s)$txt), use.names = FALSE) %||% character(0)
}

# ---- resolvers -----------------------------------------------------------------------------------

.sr_ph_resolvers <- new.env(parent = emptyenv())

#' Register the resolver of a source prefix: function(project, parts, kind) returning the text.
.sr_ph_register_resolver <- function(prefix, fn) {
  .sr_ph_one(prefix, "prefix")
  if (!is.function(fn)) stop("`fn` must be a function(project, parts, kind)", call. = FALSE)
  assign(prefix, fn, envir = .sr_ph_resolvers)
  invisible(prefix)
}

# sr_result() of an analysis; inside sr_ph_verify() each result is read once.
.sr_ph_ctx <- new.env(parent = emptyenv())

.sr_ph_result <- function(project, id) {
  cache <- .sr_ph_ctx$cache
  if (!is.null(cache) && !is.null(cache[[id]])) return(cache[[id]])
  res <- sr_result(project, id)
  if (!is.null(cache)) .sr_ph_ctx$cache[[id]] <- res
  res
}

# Derive the text of one source from the project. Errors name the problem.
.sr_ph_derive <- function(project, source, kind) {
  prefix <- sub(":.*$", "", source)
  if (!grepl(":", source, fixed = TRUE) || !exists(prefix, envir = .sr_ph_resolvers, inherits = FALSE)) {
    stop("no resolver for source '", source, "'", call. = FALSE)
  }
  rest <- sub("^[^:]*:", "", source)
  parts <- if (identical(prefix, "label")) rest else strsplit(rest, ":", fixed = TRUE)[[1L]]
  get(prefix, envir = .sr_ph_resolvers, inherits = FALSE)(project, parts, kind)
}

.sr_ph_int_part <- function(x, what) {
  v <- suppressWarnings(as.integer(x))
  if (length(x) != 1L || is.na(v) || v < 1L || !grepl("^[0-9]+$", x)) {
    stop(what, " must be a positive whole number, got '", paste(x, collapse = ":"), "'", call. = FALSE)
  }
  v
}

.sr_ph_resolve_effect <- function(project, parts, kind) {
  if (length(parts) != 3L) stop("an effect source is effect:<analysis>:<row>:<field>", call. = FALSE)
  eff <- .sr_ph_result(project, parts[1L])$effects
  i <- .sr_ph_int_part(parts[2L], "the effect row")
  if (i > nrow(eff)) stop("analysis '", parts[1L], "' has no effects row ", i, call. = FALSE)
  whole <- function(x) sprintf("%d", as.integer(round(x)))
  switch(parts[3L],
    est = , lo_hi = {
      ci <- sr_fmt_ci(eff$est[i], eff$lo[i], eff$hi[i], eff$mcse[i])
      if (identical(parts[3L], "est")) sub(" \\(.*$", "", ci) else sub("\\)$", "", sub("^[^(]*\\(", "", ci))
    },
    p = sr_fmt_p(eff$p[i]),
    n = whole(eff$n[i]),
    events = whole(eff$events[i]),
    stop("unknown effect field '", parts[3L], "'", call. = FALSE))
}

.sr_ph_resolve_fragment <- function(project, parts, kind) {
  if (length(parts) != 3L) stop("a fragment source is fragment:<analysis>:<name>:<k>", call. = FALSE)
  frag <- .sr_ph_result(project, parts[1L])$fragments[[parts[2L]]]
  if (!is.character(frag) || length(frag) != 1L) {
    stop("analysis '", parts[1L], "' has no fragment '", parts[2L], "'", call. = FALSE)
  }
  k <- .sr_ph_int_part(parts[3L], "the number position")
  txt <- .sr_ph_num_matches(frag)$txt
  if (k > length(txt)) stop("fragment '", parts[2L], "' has no number ", k, call. = FALSE)
  txt[k]
}

.sr_ph_register_resolver("effect", .sr_ph_resolve_effect)
.sr_ph_register_resolver("fragment", .sr_ph_resolve_fragment)
.sr_ph_register_resolver("label", function(project, parts, kind) parts)

#' Re-derive every row of the registry from its source and compare it with the registered text.
#'
#' `got` is the re-derived text (the message when the resolver fails), `expected` the registered text.
#' A suppressed row passes when the re-derived count is in 1..(SR_SDC_K - 1). A resolver error is
#' ok = FALSE, never an error of this function.
#' @return data.frame(id, source, expected, got, ok)
sr_ph_verify <- function(reg, project) {
  .sr_ph_check_reg(reg)
  rows <- sr_ph_table(reg)
  .sr_ph_ctx$cache <- list()
  on.exit(.sr_ph_ctx$cache <- NULL, add = TRUE)
  n <- nrow(rows)
  got <- character(n)
  ok <- logical(n)
  for (i in seq_len(n)) {
    r <- tryCatch(.sr_ph_derive(project, rows$source[i], rows$kind[i]), error = function(e) e)
    if (inherits(r, "error")) {
      got[i] <- conditionMessage(r)
      ok[i] <- FALSE
      next
    }
    got[i] <- as.character(r)
    if (isTRUE(rows$suppressed[i])) {
      v <- suppressWarnings(as.numeric(got[i]))
      ok[i] <- grepl("^[0-9]+$", got[i]) && !is.na(v) && v >= 1 && v <= SR_SDC_K - 1L
    } else {
      ok[i] <- identical(got[i], rows$text[i])
    }
  }
  data.frame(id = rows$id, source = rows$source, expected = rows$text, got = got, ok = ok,
             stringsAsFactors = FALSE)
}
