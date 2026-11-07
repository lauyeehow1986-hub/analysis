# writer/cite.R - the reference system of the manuscript.
#
# A citation in prose is the token {{C:<key>}} (writer/placeholders.R). A key names one reference item:
#   - a method reference of inst/templates/references.json (zou2004, cox1972, ...), bundled with the app;
#   - pkg_<name>, an R package, written from the installed package DESCRIPTION file (R23: not utils::citation);
#   - r_core, R itself, from citation().
# Nothing is looked up online: a package that is not installed is an error naming it.
#
#   sr_cite_db()          the bundled method references, a named list of CSL-JSON items
#   .sr_cite_keys()       the references one route (with its spec and effects) calls for
#   .sr_cite_pkg()        the CSL item of an installed R package
#   sr_cite_order()       the keys of a document in order of first use (ruling R19)
#   sr_cite_fn()          function(key) -> "[k]", for sr_ph_resolve(cite = )
#   sr_cite_csl()         CSL-JSON items for keys
#   sr_cite_ris()         one RIS record per item
#   sr_cite_vancouver()   one numbered Vancouver entry per item

# The R packages the writer can cite (the ones .sr_cite_keys() names).
SR_CITE_PKGS <- c("boot", "Hmisc", "logistf", "mice", "sandwich", "survival")

# ---- the bundled method references ----------------------------------------------------------------

.sr_cite_cache <- new.env(parent = emptyenv())

# inst/templates/references.json: next to app/ in the repo and in the portable bundle.
.sr_cite_db_path <- function() .sr_wr_template("references.json", what = "references file")

#' The method references, read once: a list of CSL-JSON items named by id.
sr_cite_db <- function() {
  if (!is.null(.sr_cite_cache$db)) return(.sr_cite_cache$db)
  path <- .sr_cite_db_path()
  items <- .sr_parse_json(readChar(path, file.size(path), useBytes = TRUE))
  ids <- vapply(items, function(i) as.character(i$id), "")
  if (anyDuplicated(ids)) stop("references.json has a duplicate id: ", ids[anyDuplicated(ids)], call. = FALSE)
  .sr_cite_cache$db <- stats::setNames(items, ids)
  .sr_cite_cache$db
}

# ---- the keys a route calls for -------------------------------------------------------------------

# The key of an R package: pkg_<name>, lower case, anything but a letter or digit an underscore.
.sr_cite_pkg_key <- function(pkg) paste0("pkg_", tolower(gsub("[^A-Za-z0-9]", "_", as.character(pkg))))

#' The references of one analysis: list(methods = keys of the method references, packages = package names).
#'
#' @param route_id the route id (modpois, logistic, cox, rmst, table1, ...); the family is the prefix.
#' @param spec the analysis spec (penalty, se, terms, layers).
#' @param effects the effects table of its result (ci_method, measure).
.sr_cite_keys <- function(route_id, spec, effects) {
  rid <- as.character(route_id)[1L]
  spec <- if (is.list(spec)) spec else list()
  methods <- character()
  packages <- character()
  add <- function(m = character(), p = character()) {
    methods <<- c(methods, m)
    packages <<- c(packages, p)
  }
  if (startsWith(rid, "modpois")) add("zou2004")
  if (identical(spec[["penalty"]], "firth")) add(c("firth1993", "heinze2002"), "logistf")
  if (startsWith(rid, "cox")) add("cox1972", "survival")
  if (startsWith(rid, "rmst")) add("royston2013", "survival")
  if (startsWith(rid, "table1")) add(c("austin2009", "yang2012"))
  if (identical(spec[["layers"]][["missing"]], "mi")) add(c("rubin1987", "vonhippel2020"), "mice")
  eff <- if (is.data.frame(effects)) effects else NULL
  cim <- if (!is.null(eff) && "ci_method" %in% names(eff)) as.character(eff$ci_method) else character()
  mea <- if (!is.null(eff) && "measure" %in% names(eff)) as.character(eff$measure) else character()
  if (any(cim %in% "boot_bca")) add("efron1987", "boot")
  if (any(mea %in% "NNT")) add("altman1998")
  if (any(cim %in% "wilson")) add("wilson1927")
  ty <- vapply(spec[["terms"]] %||% list(), function(t) as.character(t[["type"]] %||% "linear")[1L], "")
  if (any(ty %in% "rcs")) add("harrell2015", "Hmisc")
  # the modified Poisson route reports robust standard errors unless the spec says otherwise
  se <- spec[["se"]] %||% if (startsWith(rid, "modpois")) "robust" else "model"
  if (any(as.character(se) %in% c("robust", "cluster"))) add(p = "sandwich")
  list(methods = unique(methods), packages = unique(packages))
}

# ---- package items --------------------------------------------------------------------------------

# LaTeX accents in a name or title as the characters they stand for: accent mark, letter, code point
# (so the source stays ASCII). \"o, \'e, \`a, \^o, \~n, in either the \"o or the \"{o} form.
SR_CITE_ACCENTS <- data.frame(
  mark = c(rep("\"", 8L), rep("'", 8L), rep("`", 4L), rep("^", 5L), rep("~", 3L)),
  letter = c("a", "e", "i", "o", "u", "A", "O", "U", "a", "e", "i", "o", "u", "c", "E", "A", "a", "e", "o", "u",
             "a", "e", "o", "u", "i", "n", "a", "o"),
  code = c(228L, 235L, 239L, 246L, 252L, 196L, 214L, 220L, 225L, 233L, 237L, 243L, 250L, 263L, 201L, 193L,
           224L, 232L, 242L, 249L, 226L, 234L, 244L, 251L, 238L, 241L, 227L, 245L),
  stringsAsFactors = FALSE)

.sr_cite_accents <- function(x) {
  for (k in seq_len(nrow(SR_CITE_ACCENTS))) {
    base <- paste0("\\", SR_CITE_ACCENTS$mark[k], SR_CITE_ACCENTS$letter[k])
    braced <- paste0("\\", SR_CITE_ACCENTS$mark[k], "{", SR_CITE_ACCENTS$letter[k], "}")
    ch <- intToUtf8(SR_CITE_ACCENTS$code[k])
    x <- gsub(braced, ch, gsub(base, ch, x, fixed = TRUE), fixed = TRUE)
  }
  x <- gsub("\\c{c}", intToUtf8(231L), gsub("\\c c", intToUtf8(231L), x, fixed = TRUE), fixed = TRUE)
  x <- gsub("\\o{}", intToUtf8(248L), x, fixed = TRUE)
  x <- gsub("\\ss{}", intToUtf8(223L), x, fixed = TRUE)
  x <- gsub("\\\\o(?![A-Za-z])", intToUtf8(248L), x, perl = TRUE)
  x <- gsub("\\\\aa(?![A-Za-z])", intToUtf8(229L), x, perl = TRUE)
  gsub("\\\\ss(?![A-Za-z])", intToUtf8(223L), x, perl = TRUE)
}

# Remove the markup a citation field can carry (\pkg{x}, accents, braces, _italic_).
.sr_cite_clean <- function(x) {
  x <- as.character(x)[1L]
  if (is.na(x)) return(NA_character_)
  x <- .sr_cite_accents(x)
  x <- gsub("\\\\(pkg|proglang|emph|code|strong|samp|file)\\{([^}]*)\\}", "\\2", x)
  x <- gsub("[{}]", "", x)
  x <- sub("^_(.*)_$", "\\1", x)
  trimws(gsub("[[:space:]]+", " ", x))
}

# A trailing Jr, Sr, II, III or IV (with or without a stop) of a family name moves to the suffix:
# "Harrell Jr" -> family "Harrell", suffix "Jr".
.sr_cite_author <- function(family, given) {
  family <- .sr_cite_clean(family)
  given <- .sr_cite_clean(given)
  m <- regmatches(family, regexec("^(.*[^ ,])[ ,]+(Jr|Sr|II|III|IV)[.]?$", family))[[1L]]
  if (length(m) == 3L) {
    list(family = m[2L], given = given, suffix = m[3L])
  } else {
    list(family = family, given = given)
  }
}

# CSL authors from a person object: {family, given[, suffix]}, or {literal} for an organisation.
.sr_cite_authors <- function(a) {
  if (is.null(a) || !length(a)) return(list())
  lapply(seq_along(a), function(i) {
    p <- a[i]
    fam <- paste(as.character(p$family), collapse = " ")
    giv <- paste(as.character(p$given), collapse = " ")
    if (nzchar(fam)) .sr_cite_author(fam, giv) else list(literal = .sr_cite_clean(giv))
  })
}

# The authors of an installed package, from its DESCRIPTION: the "aut" persons of Authors@R; else the
# Author field; else the maintainer's name.
.sr_cite_desc_authors <- function(d) {
  a <- d[["Authors@R"]]
  if (is.character(a) && length(a) == 1L && nzchar(a)) {
    ps <- tryCatch(eval(parse(text = a), envir = list(person = utils::person), enclos = baseenv()),
                   error = function(e) NULL)
    if (inherits(ps, "person")) {
      keep <- vapply(seq_along(ps), function(i) "aut" %in% ps[i]$role, NA)
      if (any(keep)) return(.sr_cite_authors(ps[keep]))
    }
  }
  by_name <- function(nm) {
    nm <- .sr_cite_clean(nm)
    if (!nzchar(nm)) return(NULL)
    suf <- regmatches(nm, regexec("^(.*[^ ,])[ ,]+(Jr|Sr|II|III|IV)[.]?$", nm))[[1L]]
    base <- if (length(suf) == 3L) suf[2L] else nm
    toks <- strsplit(base, " ", fixed = TRUE)[[1L]]
    if (length(toks) < 2L) return(list(literal = nm))
    fam <- toks[length(toks)]
    giv <- paste(toks[-length(toks)], collapse = " ")
    .sr_cite_author(if (length(suf) == 3L) paste(fam, suf[3L]) else fam, giv)
  }
  au <- d[["Author"]]
  if (is.character(au) && length(au) == 1L && nzchar(au)) {
    # "A B [aut, cre], C D [ctb] (note)": split at commas outside brackets; keep "aut" (or unmarked) names
    parts <- trimws(strsplit(gsub("[[:space:]]+", " ", au), ",[[:space:]]*(?![^[]*[]])|;| and | & ", perl = TRUE)[[1L]])
    parts <- parts[nzchar(parts)]
    role <- ifelse(grepl("[[][^]]*[]]", parts), grepl("[[][^]]*aut", parts), TRUE)
    parts <- gsub("[[:space:]]*([[][^]]*[]]|[(][^)]*[)])", "", parts[role])
    out <- Filter(Negate(is.null), lapply(parts, by_name))
    if (length(out)) return(out)
  }
  mt <- sub("[[:space:]]*<.*$", "", as.character(d[["Maintainer"]] %||% ""))
  Filter(Negate(is.null), list(by_name(mt)))
}

# The CSL item of an R package or of R: type software, "<genre> version <v>".
.sr_cite_software <- function(id, authors, title, year, version, genre, doi = NA_character_) {
  it <- list(id = id, type = "software", author = authors, title = title, version = version, genre = genre)
  if (length(year) == 1L && !is.na(year)) it[["issued"]] <- list(`date-parts` = list(list(as.integer(year))))
  if (length(doi) == 1L && !is.na(doi) && nzchar(doi)) it[["DOI"]] <- doi
  it
}

#' The CSL item of an installed R package, written from its installed DESCRIPTION (ruling R23; no network).
#'
#' The id is pkg_<name> in lower case, the title "<pkg>: <Title>", the authors the "aut" persons of
#' Authors@R (else the Author field, else the maintainer), the year that of Date/Publication, else
#' Packaged, else Date, and the version the installed one. The DOI is 10.32614/CRAN.package.<pkg> for a
#' package of CRAN, and absent otherwise (base and recommended packages have no Repository). Nothing
#' of the authors' article leaks in. A package that is not installed is an error naming it.
.sr_cite_pkg <- function(pkg) {
  pkg <- as.character(pkg)
  if (length(pkg) != 1L || is.na(pkg) || !nzchar(pkg)) stop("`pkg` must be one package name", call. = FALSE)
  if (!nzchar(system.file(package = pkg))) {
    stop("the reference for R package '", pkg, "' cannot be written: the package is not installed", call. = FALSE)
  }
  d <- utils::packageDescription(pkg)
  yr <- NA_integer_
  for (f in c("Date/Publication", "Packaged", "Date")) {
    v <- d[[f]]
    if (is.character(v) && length(v) == 1L && grepl("^[0-9]{4}", v)) {
      yr <- as.integer(substr(v, 1L, 4L))
      break
    }
  }
  doi <- if (identical(as.character(d[["Repository"]] %||% ""), "CRAN")) paste0("10.32614/CRAN.package.", pkg) else NA_character_
  .sr_cite_software(.sr_cite_pkg_key(pkg), .sr_cite_desc_authors(d), paste0(pkg, ": ", .sr_cite_clean(d[["Title"]])), yr,
                    as.character(utils::packageVersion(pkg)), "R package", doi)
}

# R itself, from citation().
.sr_cite_r_core <- function() {
  cit <- suppressWarnings(suppressMessages(utils::citation()))[[1L]]
  yr <- suppressWarnings(as.integer(as.character(cit$year)[1L]))
  .sr_cite_software("r_core", .sr_cite_authors(cit$author), .sr_cite_clean(cit$title), yr,
                    paste(R.version$major, R.version$minor, sep = "."), "R")
}

# The item of a key: a method reference, an R package or R itself; an unknown key is an error.
.sr_cite_item <- function(key) {
  key <- as.character(key)
  db <- sr_cite_db()
  if (key %in% names(db)) return(db[[key]])
  if (identical(key, "r_core")) return(.sr_cite_r_core())
  pk <- SR_CITE_PKGS[.sr_cite_pkg_key(SR_CITE_PKGS) == key]
  if (length(pk)) return(.sr_cite_pkg(pk))
  stop("no reference item for citation key '", key, "'", call. = FALSE)
}

# ---- order ----------------------------------------------------------------------------------------

# The citation keys of some text, in order, repeats kept.
.sr_cite_tokens <- function(text) {
  text <- as.character(unlist(text, use.names = FALSE))
  text <- text[!is.na(text)]
  if (!length(text)) return(character())
  toks <- unlist(regmatches(text, gregexpr("\\{\\{C:[a-z][a-z0-9_]*\\}\\}", text)), use.names = FALSE)
  sub("\\}\\}$", "", sub("^\\{\\{C:", "", toks))
}

#' The keys of a document in order of first use (ruling R19), each with an item.
#'
#' The order is the first `{{C:}}` use in the paragraphs of the main text, then in the captions and
#' footnotes of its tables and figures, then the same in the supplement. vonelm2007 and r_core follow
#' if nothing used them. A key with no item (an unknown key, a package that is not installed) is an
#' error, so a reference list never has a hole.
#' @param model a document model (list with main and supplement); a refs block is ignored.
sr_cite_order <- function(model) {
  scan <- function(blocks) {
    blocks <- Filter(function(b) is.list(b) && !identical(b$type, "refs"), blocks %||% list())
    paras <- Filter(function(b) identical(b$type, "para"), blocks)
    rest <- Filter(function(b) b$type %in% c("table", "figure"), blocks)
    c(.sr_cite_tokens(lapply(paras, function(b) b$text)),
      .sr_cite_tokens(lapply(rest, function(b) c(b$caption, b$footnotes))))
  }
  keys <- unique(c(scan(model$main), scan(model$supplement)))
  keys <- c(keys, setdiff(c("vonelm2007", "r_core"), keys))
  for (k in keys) .sr_cite_item(k)
  keys
}

#' function(key) -> "[k]", k the position of the key in `keys` (what sr_cite_order returns).
sr_cite_fn <- function(keys) {
  keys <- as.character(keys)
  function(key) {
    k <- match(as.character(key), keys)
    if (is.na(k)) stop("citation key '", key, "' is not in the reference list", call. = FALSE)
    paste0("[", k, "]")
  }
}

# ---- outputs --------------------------------------------------------------------------------------

#' CSL-JSON items (a list, one per key, in the order of `keys`).
sr_cite_csl <- function(keys) {
  keys <- unique(as.character(keys))
  stats::setNames(lapply(keys, .sr_cite_item), NULL)
}

#' The CSL items as a CSL-JSON text.
sr_cite_csl_json <- function(items) .sr_json(unname(items))

.sr_cite_year <- function(it) {
  y <- it[["issued"]][["date-parts"]][[1L]][[1L]] %||% NA
  if (length(y) == 1L && !is.na(y)) as.character(y) else NA_character_
}

.sr_cite_str <- function(x) {
  x <- as.character(unlist(x, use.names = FALSE))
  if (length(x) && !is.na(x[1L]) && nzchar(x[1L])) x[1L] else NA_character_
}

# Initials of a given name: "Terry M" -> "TM", "PC" -> "PC", "Jean-Paul" -> "JP".
.sr_cite_initials <- function(given) {
  g <- .sr_cite_str(given)
  if (is.na(g)) return("")
  toks <- strsplit(g, "[[:space:].-]+")[[1L]]
  toks <- toks[nzchar(toks)]
  paste(vapply(toks, function(t) if (grepl("^[[:upper:]]+$", t)) t else toupper(substr(t, 1L, 1L)), ""),
        collapse = "")
}

# One Vancouver author: "Zou G", "Harrell FE Jr", or an organisation's literal name.
.sr_cite_vanc_author <- function(a) {
  lit <- .sr_cite_str(a[["literal"]])
  if (!is.na(lit)) return(lit)
  s <- c(.sr_cite_str(a[["family"]]), .sr_cite_initials(a[["given"]]), .sr_cite_str(a[["suffix"]]))
  s <- paste(s[!is.na(s)], collapse = " ")
  trimws(gsub("[[:space:]]+", " ", s))
}

# "A B, C D, E F" for up to six authors, else the first six followed by "et al."
.sr_cite_vanc_authors <- function(authors) {
  if (!length(authors)) return(NA_character_)
  nm <- vapply(authors, .sr_cite_vanc_author, "")
  if (length(nm) > 6L) paste0(paste(nm[1:6], collapse = ", "), ", et al") else paste(nm, collapse = ", ")
}

# Text ended by one full stop (none added after ".", "?" or "!").
.sr_cite_stop <- function(x) if (grepl("[.?!]$", x)) x else paste0(x, ".")

# One Vancouver entry, without its number.
.sr_cite_vanc_one <- function(it) {
  ty <- .sr_cite_str(it[["type"]])
  au <- .sr_cite_vanc_authors(it[["author"]])
  ti <- .sr_cite_str(it[["title"]])
  yr <- .sr_cite_year(it)
  doi <- .sr_cite_str(it[["DOI"]])
  cont <- .sr_cite_str(it[["container-title-short"]])
  if (is.na(cont)) cont <- .sr_cite_str(it[["container-title"]])
  parts <- c(if (!is.na(au)) .sr_cite_stop(au), if (!is.na(ti)) .sr_cite_stop(ti))
  if (identical(ty, "book")) {
    ed <- .sr_cite_str(it[["edition"]])
    pub <- .sr_cite_str(it[["publisher"]])
    place <- .sr_cite_str(it[["publisher-place"]])
    imprint <- paste0(if (!is.na(place)) paste0(place, ": "), if (!is.na(pub)) pub,
                      if (!is.na(yr)) paste0(if (!is.na(place) || !is.na(pub)) "; ", yr))
    parts <- c(parts, if (!is.na(ed)) paste0(ed, " ed."), if (nzchar(imprint)) .sr_cite_stop(imprint))
  } else if (identical(ty, "software")) {
    gen <- .sr_cite_str(it[["genre"]])
    ver <- .sr_cite_str(it[["version"]])
    parts <- c(parts, if (!is.na(ver)) paste0(if (is.na(gen)) "Version" else paste0(gen, " version"), " ", ver, "."),
               if (!is.na(yr)) paste0(yr, "."))
  } else if (identical(ty, "paper-conference")) {
    num <- .sr_cite_str(it[["number"]])
    seg <- cont %|na|% ""
    if (!is.na(yr) && !grepl(yr, seg, fixed = TRUE)) seg <- paste0(seg, if (nzchar(seg)) "; ", yr)
    if (!is.na(num)) seg <- paste0(seg, if (nzchar(seg)) "; ", "paper ", num)
    if (nzchar(seg)) parts <- c(parts, .sr_cite_stop(seg))
  } else {
    vol <- .sr_cite_str(it[["volume"]])
    iss <- .sr_cite_str(it[["issue"]])
    pg <- .sr_cite_str(it[["page"]])
    loc <- paste0(if (!is.na(yr)) yr, if (!is.na(vol)) paste0(";", vol), if (!is.na(iss)) paste0("(", iss, ")"),
                  if (!is.na(pg)) paste0(":", pg))
    parts <- c(parts, if (!is.na(cont)) .sr_cite_stop(cont), if (nzchar(loc)) .sr_cite_stop(loc))
  }
  if (!is.na(doi)) parts <- c(parts, paste0("doi:", doi))
  paste(parts, collapse = " ")
}

`%|na|%` <- function(x, y) if (is.na(x)) y else x

#' One numbered Vancouver entry per item: "<k>. <entry>".
#'
#' An article reads `Authors. Title. Journal. Year;Vol(Issue):Pages. doi:DOI`; up to six authors are
#' listed (surname and initials), more give the first six and "et al". A book gives `Place: Publisher;
#' Year.`, an R package `R package version <v>.`.
sr_cite_vancouver <- function(items) {
  items <- unname(items)
  if (!length(items)) return(character())
  vapply(seq_along(items), function(k) paste0(k, ". ", .sr_cite_vanc_one(items[[k]])), "")
}

# One RIS line.
.sr_cite_ris_line <- function(tag, value) {
  if (is.null(value) || length(value) == 0L || is.na(value[1L]) || !nzchar(as.character(value[1L]))) return(character())
  paste0(tag, "  - ", as.character(value[1L]))
}

# One RIS record of a CSL item.
.sr_cite_ris_one <- function(it) {
  ty <- .sr_cite_str(it[["type"]])
  tag <- switch(ty %|na|% "", `article-journal` = "JOUR", book = "BOOK", `paper-conference` = "CPAPER",
                software = "COMP", "GEN")
  au <- vapply(it[["author"]] %||% list(), function(a) {
    lit <- .sr_cite_str(a[["literal"]])
    if (!is.na(lit)) return(lit)
    fam <- .sr_cite_str(a[["family"]])
    giv <- .sr_cite_str(a[["given"]])
    sfx <- .sr_cite_str(a[["suffix"]])
    paste0(fam, if (!is.na(giv)) paste0(", ", giv), if (!is.na(sfx)) paste0(", ", sfx))
  }, "")
  cont <- .sr_cite_str(it[["container-title"]])
  pg <- .sr_cite_str(it[["page"]])
  sp <- ep <- NA_character_
  if (!is.na(pg)) {
    if (grepl("^[^-]+-[^-]+$", pg)) {
      sp <- sub("-.*$", "", pg)
      ep <- sub("^.*-", "", pg)
    } else {
      sp <- pg
    }
  }
  c(.sr_cite_ris_line("TY", tag),
    unlist(lapply(au, function(a) .sr_cite_ris_line("AU", a))),
    .sr_cite_ris_line("TI", it[["title"]]),
    .sr_cite_ris_line(if (identical(ty, "paper-conference")) "T2" else "JO", cont),
    .sr_cite_ris_line("PY", .sr_cite_year(it)),
    .sr_cite_ris_line("VL", it[["volume"]]),
    .sr_cite_ris_line("IS", it[["issue"]]),
    .sr_cite_ris_line("SP", sp),
    .sr_cite_ris_line("EP", ep),
    .sr_cite_ris_line("PB", it[["publisher"]]),
    .sr_cite_ris_line("CY", it[["publisher-place"]]),
    .sr_cite_ris_line("ET", it[["edition"]] %||% it[["version"]]),
    .sr_cite_ris_line("M1", it[["number"]]),
    .sr_cite_ris_line("DO", it[["DOI"]]),
    "ER  - ")
}

#' One RIS record per item (TY, AU per author, TI, JO / T2, PY, VL, IS, SP, EP, DO, ER).
sr_cite_ris <- function(items) {
  items <- unname(items)
  vapply(items, function(it) paste(.sr_cite_ris_one(it), collapse = "\n"), "")
}
