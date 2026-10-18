# writer/guard.R - the claim guard of the manuscript writer.
# The prose of a manuscript is a template over placeholders, so a model or a person can still type words
# that over-claim. The guard checks every paragraph template, every table and figure caption and footnote,
# and the header and cells of the main-results table, against rules that need no judgement:
#
#   G1 digits                 a number typed into prose instead of coming from a placeholder        block
#   G2 causal_language        causal wording on a route whose purpose is not explain-causal         block
#   G3 risk_factor            "risk factor" / "determinant" on a predict route                      block
#   G4 significance           "significant" without a p value, on the wrong side of alpha, or on a
#                             describe route                                                        block
#   G5 absence_of_evidence    "no difference", "did not differ", ...                                block
#   G6 direction              higher / lower against an estimate on the other side of its null      block
#                             (two estimates in one sentence: "direction not checked")              warn
#   G7 hypothesis_generating  an exploratory analysis must say so, and never claims to confirm      block
#   G8 table2                 a covariate coefficient quoted in the prose of an explain-causal route block
#   G9 non_collapsible        a conditional OR / HR that is not called conditional or adjusted      warn
#
# A lexicon is matched case-insensitively on word boundaries (so "insignificant" is not "significant"),
# a multi-word phrase as a phrase. Rules G2-G9 read the template with every token replaced by a neutral
# mark, so a token never makes or breaks a match; G1 reads the template with the tokens removed
# (sr_ph_digits). A sentence ends at ". " and nowhere else: a semicolon does not split.
#
#   sr_claim_guard(model)                          every block of the model
#   sr_claim_check_text(text, analysis, model)     one edited or polished text (P2 / P3 call it on user text)
#
# Both return data.frame(block, analysis, rule, severity, detail); zero rows means clean.

SR_GUARD_CAUSAL <- c("cause", "causes", "caused", "causal", "effect of", "effects of", "leads to", "led to",
                     "results in", "resulted in", "reduces", "reduced", "increases", "increased", "prevents",
                     "prevented", "protective", "impact of", "attributable to", "due to")
SR_GUARD_RISK <- c("risk factor", "determinant")
SR_GUARD_SIGNIFICANT <- c("significant", "significantly")
SR_GUARD_NOT_SIGNIFICANT <- c("non-significant", "not significant", "non-significantly", "not significantly")
SR_GUARD_ABSENCE <- c("no difference", "no effect", "no association", "did not differ", "was not associated",
                      "were not associated")
SR_GUARD_UP <- c("higher", "greater", "increased", "more", "elevated", "longer")
SR_GUARD_DOWN <- c("lower", "reduced", "decreased", "fewer", "less", "shorter")
SR_GUARD_HYPGEN <- c("exploratory", "hypothesis-generating")
SR_GUARD_CONFIRM <- c("confirm", "confirms", "confirmed", "demonstrate", "demonstrates", "demonstrated", "establish",
                      "establishes", "established", "prove", "proves", "proved", "definitive")
SR_GUARD_ADJUSTED <- c("conditional", "adjusted")

SR_GUARD_COLS <- c("block", "analysis", "rule", "severity", "detail")

# The mark that stands for a token while the words of a sentence are matched. It is no letter or digit, so
# it can neither build a lexicon word nor split one.
SR_GUARD_MARK <- "~"

.sr_gd_empty <- function() {
  data.frame(block = character(), analysis = character(), rule = character(), severity = character(),
             detail = character(), stringsAsFactors = FALSE)
}

.sr_gd_check_model <- function(model) {
  if (!(is.list(model) && is.list(model$meta) && inherits(model$reg, "sr_ph"))) {
    stop("`model` must be the document model returned by sr_doc_model()", call. = FALSE)
  }
  invisible(model)
}

# One regular expression for a lexicon: whole words, a phrase's spaces matching any run of white space.
.sr_gd_pattern <- function(words) {
  w <- gsub(" ", "\\s+", words, fixed = TRUE)
  paste0("\\b(", paste(w, collapse = "|"), ")\\b")
}

# The words of `lex` found in `text`, lower case, in order of appearance (character(0) when none).
.sr_gd_find <- function(text, lex) {
  m <- gregexpr(.sr_gd_pattern(lex), text, perl = TRUE, ignore.case = TRUE)
  tolower(regmatches(text, m)[[1L]])
}

.sr_gd_has <- function(text, lex) length(.sr_gd_find(text, lex)) > 0L

# The sentences of a template: split at a full stop followed by white space, tokens intact.
.sr_gd_sentences <- function(text) {
  s <- strsplit(trimws(text), "(?<=\\.)\\s+", perl = TRUE)[[1L]]
  s[nzchar(trimws(s))]
}

# The E placeholder rows a sentence uses, in order and distinct. Unknown ids are left out.
.sr_gd_rows <- function(sentence, rows) {
  toks <- regmatches(sentence, gregexpr(SR_PH_RE, sentence, perl = TRUE))[[1L]]
  ids <- unique(sub("\\}\\}$", "", sub("^\\{\\{", "", toks)))
  ids <- ids[startsWith(ids, "E")]
  rows[match(ids[ids %in% rows$id], rows$id), , drop = FALSE]
}

# analysis and row of an `effect:<analysis>:<row>:<field>` source, NA when it is some other source.
.sr_gd_effect_key <- function(source) {
  m <- regmatches(source, regexec("^effect:([^:]+):([0-9]+):[a-z_]+$", source))
  pick <- function(i) vapply(m, function(x) if (length(x) == 3L) x[i] else NA_character_, "")
  list(analysis = pick(2L), row = suppressWarnings(as.integer(pick(3L))))
}

# role / measure / conditional of the effects rows behind `rows` (NA where the source is not an effects row,
# or the model does not know the row).
.sr_gd_effect_info <- function(rows, model) {
  key <- .sr_gd_effect_key(rows$source)
  info <- model$meta$effects
  n <- nrow(rows)
  out <- data.frame(analysis = key$analysis, role = rep(NA_character_, n), measure = rep(NA_character_, n),
                    conditional = rep(NA, n), stringsAsFactors = FALSE)
  if (!n || !is.data.frame(info) || !nrow(info)) return(out)
  hit <- match(paste(key$analysis, key$row), paste(info$analysis, info$row))
  out$role <- as.character(info$role[hit])
  out$measure <- as.character(info$measure[hit])
  out$conditional <- as.logical(info$conditional[hit])
  out
}

.sr_gd_snip <- function(s) {
  s <- gsub("\\s+", " ", trimws(s))
  if (nchar(s) > 90L) paste0(substr(s, 1L, 87L), "...") else s
}

.sr_gd_frame <- function(block, analysis, rule, severity, detail) {
  if (!length(rule)) return(.sr_gd_empty())
  data.frame(block = rep(block, length(rule)), analysis = rep(analysis, length(rule)), rule = rule,
             severity = severity, detail = detail, stringsAsFactors = FALSE)
}

# The findings of one sentence. `ctx` holds the facts of the block (purpose, wording, section, alpha, the
# registry rows and the model); `sentence` still carries its tokens.
.sr_gd_sentence <- function(sentence, ctx) {
  bare <- gsub(SR_PH_RE, SR_GUARD_MARK, sentence, perl = TRUE)
  snip <- .sr_gd_snip(sentence)
  rule <- character()
  sev <- character()
  det <- character()
  add <- function(r, s, d) {
    rule <<- c(rule, r)
    sev <<- c(sev, s)
    det <<- c(det, paste0(d, " in: ", snip))
  }
  rows <- .sr_gd_rows(sentence, ctx$rows)
  qt <- function(w) paste0("\"", unique(w), "\"", collapse = ", ")

  # G2 causal language: allowed only on a route declared explain-causal
  if (!identical(ctx$purpose, "explain-causal")) {
    w <- .sr_gd_find(bare, SR_GUARD_CAUSAL)
    if (length(w)) add("causal_language", "block", paste0("causal wording ", qt(w), " on a route that is not explain-causal"))
  }
  # G3 risk factor language on a predict route
  if (identical(ctx$purpose, "predict")) {
    w <- .sr_gd_find(bare, SR_GUARD_RISK)
    if (length(w)) add("risk_factor", "block", paste0(qt(w), " on a prediction route"))
  }

  # G4 significance
  neg <- .sr_gd_find(bare, SR_GUARD_NOT_SIGNIFICANT)
  rest <- gsub(.sr_gd_pattern(SR_GUARD_NOT_SIGNIFICANT), SR_GUARD_MARK, bare, perl = TRUE, ignore.case = TRUE)
  pos <- .sr_gd_find(rest, SR_GUARD_SIGNIFICANT)
  if (length(neg) || length(pos)) {
    said <- qt(c(neg, pos))
    prow <- rows[rows$kind == "p", , drop = FALSE]
    # an adjusted p value (a family source) is what a claim after multiplicity rests on
    adj <- prow[startsWith(prow$source, "family:"), , drop = FALSE]
    judged <- if (nrow(adj)) adj else prow
    pv <- judged$value[is.finite(judged$value)]
    if (identical(ctx$purpose, "describe")) {
      add("significance", "block", paste0(said, " in a descriptive analysis"))
    } else if (!nrow(prow)) {
      add("significance", "block", paste0(said, " with no p value in the sentence"))
    } else if (!length(pv)) {
      add("significance", "block", paste0(said, " with no numeric p value to check"))
    } else {
      if (length(pos) && any(pv >= ctx$alpha)) {
        add("significance", "block", paste0(qt(pos), " but p is not below alpha (", format(ctx$alpha), ")"))
      }
      if (length(neg) && any(pv < ctx$alpha)) {
        add("significance", "block", paste0(qt(neg), " but p is below alpha (", format(ctx$alpha), ")"))
      }
    }
  }

  # G5 absence of evidence
  w <- .sr_gd_find(bare, SR_GUARD_ABSENCE)
  if (length(w)) add("absence_of_evidence", "block", paste0(qt(w), " reads absence of evidence as evidence of absence"))

  # G6 direction
  est <- rows[rows$kind == "est", , drop = FALSE]
  up <- .sr_gd_find(bare, SR_GUARD_UP)
  down <- .sr_gd_find(bare, SR_GUARD_DOWN)
  if (nrow(est) >= 2L) {
    add("direction", "warn", "direction not checked: more than one estimate in the sentence")
  } else if (nrow(est) == 1L && is.finite(est$value) && !is.na(est$null)) {
    if (length(up) && est$value < est$null) {
      add("direction", "block", paste0(qt(up), " but the estimate (", est$text, ") is below its null value"))
    }
    if (length(down) && est$value > est$null) {
      add("direction", "block", paste0(qt(down), " but the estimate (", est$text, ") is above its null value"))
    }
  }

  # G7 an exploratory analysis, in its Results sentences
  if (identical(ctx$wording, "exploratory") && identical(ctx$section, "results")) {
    if (!.sr_gd_has(bare, SR_GUARD_HYPGEN)) {
      add("hypothesis_generating", "block", "a sentence of an exploratory analysis must say \"exploratory\" or \"hypothesis-generating\"")
    }
    w <- .sr_gd_find(bare, SR_GUARD_CONFIRM)
    if (length(w)) add("hypothesis_generating", "block", paste0("confirmatory wording ", qt(w), " for an exploratory analysis"))
  }

  # G8 / G9 read the effects rows behind the estimates and p values of the sentence
  er <- rows[!is.na(rows$source) & startsWith(rows$source, "effect:"), , drop = FALSE]
  if (nrow(er)) {
    info <- .sr_gd_effect_info(er, ctx$model)
    purpose_of <- function(a) {
      p <- ctx$model$meta$purposes[a]
      if (length(p) == 1L && !is.na(a) && !is.na(p)) unname(p) else NA_character_
    }
    cov <- !is.na(info$role) & info$role == "covariate" &
      vapply(info$analysis, function(a) identical(purpose_of(a), "explain-causal"), NA)
    if (any(cov)) {
      add("table2", "block", paste0("a covariate coefficient (", paste(er$text[cov], collapse = ", "),
                                    ") is quoted in the prose of an explain-causal route (Table 2 fallacy)"))
    }
    nc <- er$kind == "est" & !is.na(info$measure) & info$measure %in% c("OR", "HR") & !is.na(info$conditional) &
      info$conditional
    if (any(nc) && !.sr_gd_has(bare, SR_GUARD_ADJUSTED)) {
      add("non_collapsible", "warn", paste0("a conditional ", paste(unique(info$measure[nc]), collapse = "/"),
                                            " is not called \"conditional\" or \"adjusted\" (non-collapsible)"))
    }
  }
  list(rule = rule, severity = sev, detail = det)
}

#' Check one text (a sentence, or a paragraph of several) against the claim rules.
#'
#' @param text one template: words with {{E}}, {{L}} and {{C:key}} tokens.
#' @param analysis the analysis the text belongs to, or NA. It sets the purpose (G2, G3, G4, G8) and the wording
#'   (G7); an unknown or NA analysis has no declared purpose, so the causal ban of G2 applies.
#' @param model from sr_doc_model(): the purposes, wording and alpha of `meta`, and the registry.
#' @param block the block id to report; "text" by default.
#' @param section "results" (default) or another section ("methods", "caption", ...). G7 applies to the
#'   sentences of a Results section only.
#' @return data.frame(block, analysis, rule, severity, detail); zero rows when clean.
sr_claim_check_text <- function(text, analysis, model, block = "text", section = "results") {
  if (!(is.character(text) && length(text) == 1L && !is.na(text))) stop("`text` must be a single string", call. = FALSE)
  .sr_gd_check_model(model)
  if (!(length(analysis) == 1L && (is.na(analysis) || is.character(analysis)))) {
    stop("`analysis` must be a single string or NA", call. = FALSE)
  }
  if (!(is.character(block) && length(block) == 1L && !is.na(block))) stop("`block` must be a single string", call. = FALSE)
  if (!(is.character(section) && length(section) == 1L && !is.na(section))) stop("`section` must be a single string", call. = FALSE)
  analysis <- as.character(analysis)
  a_key <- if (is.na(analysis)) NA_character_ else analysis
  look <- function(x) {
    v <- x[a_key]
    if (is.na(a_key) || length(v) != 1L || is.na(v)) NA_character_ else unname(as.character(v))
  }
  ctx <- list(purpose = look(model$meta$purposes), wording = look(model$meta$wording), section = section,
              alpha = as.numeric(model$meta$alpha %||% 0.05), rows = model$reg$rows, model = model)
  out <- list()
  # G1 digits, on the whole text
  dg <- sr_ph_digits(text)
  if (length(dg)) {
    out[[1L]] <- .sr_gd_frame(block, analysis, "digits", "block",
                              paste0("a number typed into the text: ", paste(unique(dg), collapse = ", "), " in: ",
                                     .sr_gd_snip(text)))
  }
  for (s in .sr_gd_sentences(text)) {
    f <- .sr_gd_sentence(s, ctx)
    if (length(f$rule)) out[[length(out) + 1L]] <- .sr_gd_frame(block, analysis, f$rule, f$severity, f$detail)
  }
  if (!length(out)) return(.sr_gd_empty())
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

#' The claim guard of a document model.
#'
#' Checks every paragraph template (G1-G9), every table and figure caption and footnote (G1-G6, G8, G9), and
#' the header and cells of the main-results table (G1 only). The cells and headers of route tables are
#' evaluated R output and are never scanned (ruling R10).
#' @param model from sr_doc_model().
#' @return data.frame(block, analysis, rule, severity, detail) with severity "block" or "warn"; zero rows
#'   means clean. A block id is the paragraph id, "<table id>:caption", "<table id>:footnote:<k>",
#'   "<table id>:header:<j>" or "<table id>:cell:<i>:<j>" (the main table), "<figure id>:caption".
sr_claim_guard <- function(model) {
  .sr_gd_check_model(model)
  out <- list()
  push <- function(df) if (nrow(df)) out[[length(out) + 1L]] <<- df
  for (b in c(model$main, model$supplement)) {
    an <- as.character(b$analysis %||% NA_character_)
    if (!length(an)) an <- NA_character_
    switch(b$type,
      para = push(sr_claim_check_text(b$text, an, model, block = b$id, section = b$section)),
      table = {
        push(sr_claim_check_text(b$caption, an, model, block = paste0(b$id, ":caption"), section = "caption"))
        for (k in seq_along(b$footnotes)) {
          push(sr_claim_check_text(b$footnotes[k], an, model, block = paste0(b$id, ":footnote:", k), section = "caption"))
        }
        if (identical(b$id, "main_results")) push(.sr_gd_main_table(b, an))
      },
      figure = push(sr_claim_check_text(b$caption, an, model, block = paste0(b$id, ":caption"), section = "caption")),
      NULL)
  }
  if (!length(out)) return(.sr_gd_empty())
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res[, SR_GUARD_COLS, drop = FALSE]
}

# G1 over the header and the cells of the main-results table.
.sr_gd_main_table <- function(b, analysis) {
  df <- b$df
  out <- list()
  one <- function(block, text) {
    dg <- sr_ph_digits(text)
    if (length(dg)) {
      out[[length(out) + 1L]] <<- .sr_gd_frame(block, analysis, "digits", "block",
                                               paste0("a number typed into the main table: ", paste(unique(dg), collapse = ", "),
                                                      " in: ", .sr_gd_snip(text)))
    }
  }
  for (j in seq_along(names(df))) one(paste0(b$id, ":header:", j), names(df)[j])
  for (j in seq_along(df)) {
    v <- as.character(df[[j]])
    for (i in seq_along(v)) if (!is.na(v[i])) one(paste0(b$id, ":cell:", i, ":", j), v[i])
  }
  if (!length(out)) return(.sr_gd_empty())
  do.call(rbind, out)
}
