# core/hash.R - canonical JSON and the content hash that addresses cached results.
#
# The canonical form must be byte-identical across locales and machines:
#   * object keys are sorted bytewise (radix), never by locale collation;
#   * strings are UTF-8, non-ASCII kept as raw bytes, only quote, backslash and C0 controls escaped;
#   * doubles use the shortest of 15/16/17 significant digits that round-trips;
#   * no whitespace.

#' JSON-escape one string (UTF-8) including the surrounding quotes.
.sr_cj_string <- function(s) {
  s <- enc2utf8(s)
  cps <- utf8ToInt(s)
  if (length(cps) == 1L && is.na(cps)) stop("invalid UTF-8 string in canonical JSON", call. = FALSE)
  if (!anyNA(cps) && !any(cps < 32L | cps == 34L | cps == 92L)) {
    return(paste0("\"", s, "\""))
  }
  if (anyNA(cps)) stop("invalid UTF-8 string in canonical JSON", call. = FALSE)
  bs <- intToUtf8(92L)
  piece <- function(cp) {
    if (cp == 34L) return(paste0(bs, "\""))
    if (cp == 92L) return(paste0(bs, bs))
    if (cp == 8L) return(paste0(bs, "b"))
    if (cp == 9L) return(paste0(bs, "t"))
    if (cp == 10L) return(paste0(bs, "n"))
    if (cp == 12L) return(paste0(bs, "f"))
    if (cp == 13L) return(paste0(bs, "r"))
    if (cp < 32L) return(sprintf("%su%04x", bs, cp))
    intToUtf8(cp)
  }
  paste0("\"", paste(vapply(cps, piece, character(1)), collapse = ""), "\"")
}

#' One finite double in shortest round-trip form (15, 16, then 17 significant digits).
.sr_cj_double <- function(x) {
  if (x == 0) return("0")  # also folds -0
  for (d in 15:17) {
    txt <- sprintf(paste0("%.", d, "g"), x)
    if (as.numeric(txt) == x) break
  }
  # normalise the exponent to a sign and at least two digits whatever the C runtime printed
  m <- regmatches(txt, regexec("^(.*)e([+-])0*([0-9]+)$", txt))[[1]]
  if (length(m) == 4L) txt <- sprintf("%se%s%02d", m[2], m[3], as.integer(m[4]))
  txt
}

.sr_cj_atomic_elem <- function(x, i) {
  if (is.character(x)) {
    if (is.na(x[i])) "null" else .sr_cj_string(x[i])
  } else if (is.logical(x)) {
    if (is.na(x[i])) "null" else if (x[i]) "true" else "false"
  } else if (is.integer(x)) {
    if (is.na(x[i])) "null" else as.character(x[i])
  } else {  # double
    if (is.nan(x[i]) || is.infinite(x[i])) {
      stop("non-finite number in canonical JSON", call. = FALSE)
    }
    if (is.na(x[i])) "null" else .sr_cj_double(x[i])
  }
}

.sr_cj <- function(x) {
  if (is.null(x)) return("null")
  if (inherits(x, c("Date", "POSIXct", "POSIXlt", "difftime"))) {
    stop("canonical JSON cannot serialise class ", class(x)[1L], call. = FALSE)
  }
  if (is.factor(x)) x <- as.character(x)
  if (is.list(x)) {
    nms <- names(x)
    if (is.null(nms)) {
      if (length(x) == 0L) return("[]")
      return(paste0("[", paste(vapply(x, .sr_cj, character(1), USE.NAMES = FALSE), collapse = ","), "]"))
    }
    nms <- enc2utf8(nms)
    if (anyNA(nms) || any(!nzchar(nms))) {
      stop("canonical JSON: a list must be fully named or fully unnamed", call. = FALSE)
    }
    if (anyDuplicated(nms)) {
      stop("canonical JSON: duplicate names: ", paste(unique(nms[duplicated(nms)]), collapse = ", "),
           call. = FALSE)
    }
    if (length(x) == 0L) return("{}")
    ord <- order(nms, method = "radix")
    vals <- vapply(unname(x)[ord], .sr_cj, character(1), USE.NAMES = FALSE)
    keys <- vapply(nms[ord], .sr_cj_string, character(1), USE.NAMES = FALSE)
    return(paste0("{", paste0(keys, ":", vals, collapse = ","), "}"))
  }
  if (is.character(x) || is.logical(x) || is.integer(x) || is.double(x)) {
    n <- length(x)
    if (n == 0L) return("[]")
    if (n == 1L) return(.sr_cj_atomic_elem(x, 1L))
    parts <- vapply(seq_len(n), function(i) .sr_cj_atomic_elem(x, i), character(1))
    return(paste0("[", paste(parts, collapse = ","), "]"))
  }
  stop("canonical JSON cannot serialise class ", class(x)[1L], call. = FALSE)
}

#' Canonical JSON of an R value: one UTF-8 string, sorted keys, no whitespace (see file header).
#' Named lists are objects, unnamed lists arrays; length-1 atomics unbox to scalars; NA and NULL
#' are null; attributes other than names (and names on atomic vectors) are ignored.
sr_canonical_json <- function(x) {
  out <- .sr_cj(x)
  out <- enc2utf8(out)
  Encoding(out) <- "UTF-8"
  out
}

#' Content address of a result: hash of (data hash, spec, seed, library snapshot, hardware).
sr_content_hash <- function(data_hash, spec, seed,
                            libsnap_hash = sr_libsnap_hash(), hw_fp = sr_hw_fingerprint()) {
  .sr_sha256(sr_canonical_json(list(data = data_hash, spec = spec, seed = seed,
                                    lib = libsnap_hash, hw = hw_fp)))
}
