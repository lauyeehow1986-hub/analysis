# Tests for writer/guard.R: the claim guard. Each rule has one failing sentence and the writer's own prose
# passes. Synthetic data only (the writer fixture of helper-writer.R).

.gd_cols <- c("block", "analysis", "rule", "severity", "detail")

# A token for a number the guard may look at, put in the registry of the model.
.gd_put <- function(m, text, kind, source, value = NA, scale = NA, null = NA) {
  sr_ph_put(m$reg, text, kind, source, analysis = "a_lr", value = value, scale = scale, null = null)
}
.gd_est <- function(m, tag, value, scale = "ratio") {
  .gd_put(m, sprintf("%.2f", value), "est", paste0("effect:test:", tag, ":est"), value = value, scale = scale,
          null = if (identical(scale, "ratio")) 1 else 0)
}
.gd_p <- function(m, tag, value) .gd_put(m, sprintf("%.3f", value), "p", paste0("effect:test:", tag, ":p"), value = value)
.gd_rules <- function(df) sort(unique(df$rule))

test_that("the guard returns the five columns, with zero rows too", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  g <- sr_claim_guard(m)
  expect_s3_class(g, "data.frame")
  expect_identical(names(g), .gd_cols)
  z <- sr_claim_check_text("The groups were compared.", "a_lr", m)
  expect_identical(names(z), .gd_cols)
  expect_identical(nrow(z), 0L)
  expect_type(z$block, "character")
  expect_type(z$severity, "character")
})

test_that("the writer's own document is clean: no block finding", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  g <- sr_claim_guard(m)
  expect_identical(sum(g$severity == "block"), 0L, info = paste(g$block, g$rule, g$detail, collapse = "\n"))
  expect_identical(nrow(g), 0L, info = paste(g$block, g$rule, g$detail, collapse = "\n"))
})

test_that("the guard scans paragraphs, captions, footnotes and the main table, never a route table", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  # a digit typed into a paragraph
  i <- which(vapply(m$main, function(b) identical(b$type, "para") && identical(b$id, "results_a_lr"), NA))
  m$main[[i]]$text <- paste(m$main[[i]]$text, "There were 12 deaths.")
  # a digit typed into the caption and the footnote of a supplement route table
  j <- which(vapply(m$supplement, function(b) identical(b$type, "table") && identical(b$id, "a_lr_coefficients"), NA))
  m$supplement[[j]]$caption <- "Coefficients of the 3 models"
  m$supplement[[j]]$footnotes <- c(m$supplement[[j]]$footnotes, "Based on 7 imputations")
  # the main table: a digit in a header and in a cell
  k <- which(vapply(m$main, function(b) identical(b$type, "table") && identical(b$id, "main_results"), NA))
  names(m$main[[k]]$df)[2L] <- "Comparison 2"
  m$main[[k]]$df[1L, "Measure"] <- "OR 3"
  # a route table's own header and cell are never scanned (ruling R10)
  r <- which(vapply(m$supplement, function(b) identical(b$type, "table") && identical(b$id, "a_cox_coefficients"), NA))
  names(m$supplement[[r]]$df)[2L] <- "HR (95% CI)"
  m$supplement[[r]]$df[1L, 2L] <- "1.2 (0.9 to 1.6)"
  g <- sr_claim_guard(m)
  expect_true(all(g$rule == "digits"))
  expect_true("results_a_lr" %in% g$block)
  expect_true("a_lr_coefficients:caption" %in% g$block)
  expect_true("a_lr_coefficients:footnote:2" %in% g$block)
  expect_true(any(startsWith(g$block, "main_results:header")))
  expect_true(any(startsWith(g$block, "main_results:cell")))
  expect_false(any(startsWith(g$block, "a_cox_coefficients")))
  expect_true(all(g$severity == "block"))
  expect_identical(anyDuplicated(g$block), 0L)
})

test_that("a sentence in an exploratory or causal analysis is checked against that analysis", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  i <- which(vapply(m$main, function(b) identical(b$type, "para") && identical(b$id, "results_x_age"), NA))
  m$main[[i]]$text <- sub("In exploratory, hypothesis-generating analysis", "In analysis", m$main[[i]]$text, fixed = TRUE)
  g <- sr_claim_guard(m)
  expect_identical(g$rule, "hypothesis_generating")
  expect_identical(g$block, "results_x_age")
  expect_identical(g$analysis, "x_age")
})

test_that("G1 digits: a typed number blocks, a number inside a token or a name does not", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  g <- sr_claim_check_text("There were 12 deaths.", "t1", m)
  expect_identical(g$rule, "digits")
  expect_identical(g$severity, "block")
  expect_match(g$detail, "12", fixed = TRUE)
  l65 <- sr_ph_put(m$reg, "65+", "label", "label:65+")
  expect_identical(nrow(sr_claim_check_text(paste0("in the ", l65, " group"), "t1", m)), 0L)
  expect_identical(nrow(sr_claim_check_text("for the variables z1 and age_10 of the model", "t1", m)), 0L)
  expect_identical(.gd_rules(sr_claim_check_text("Seen in 5% of rows.", "t1", m)), "digits")
})

test_that("G2 causal language blocks a route that is not explain-causal", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  tok <- .gd_put(m, "3", "count", "effect:test:g2:n", value = 3)
  g <- sr_claim_check_text(paste0("Age reduces y (", tok, ")."), "t1", m)
  expect_identical(g$rule, "causal_language")
  expect_identical(g$severity, "block")
  # the same sentence on an explain-causal route is allowed
  expect_identical(nrow(sr_claim_check_text(paste0("Age reduces y (", tok, ")."), "a_lr", m)), 0L)
  for (w in c("causes", "caused", "causal", "effect of", "effects of", "leads to", "led to", "results in", "resulted in",
              "increases", "increased", "prevents", "prevented", "protective", "impact of", "attributable to", "due to",
              "reduced", "cause")) {
    g <- sr_claim_check_text(paste("Age", w, "the outcome."), "t1", m)
    expect_true("causal_language" %in% g$rule, info = w)
  }
  # words that merely contain a lexicon word do not match
  expect_identical(nrow(sr_claim_check_text("The causality of the plan was not assessed.", "t1", m)), 0L)
  expect_identical(nrow(sr_claim_check_text("The side effects were listed.", "t1", m)), 0L)
  # a token never creates or breaks a match
  l <- sr_ph_put(m$reg, "dose effect", "label", "label:dose effect")
  expect_identical(nrow(sr_claim_check_text(paste0("The term ", l, " was fitted."), "t1", m)), 0L)
})

test_that("G3 risk factor language blocks a predict route", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  m$meta$purposes[["a_lr"]] <- "predict"
  g <- sr_claim_check_text("Age was a risk factor for the outcome.", "a_lr", m)
  expect_true("risk_factor" %in% g$rule)
  expect_true("risk_factor" %in% sr_claim_check_text("A determinant of the outcome.", "a_lr", m)$rule)
  expect_false("risk_factor" %in% sr_claim_check_text("Age was a risk factor for the outcome.", "t1", m)$rule)
  expect_false("risk_factor" %in% sr_claim_check_text("Age was a risk factor for the outcome.", "x_age", m)$rule)
})

test_that("G4 significance: needs a p value on the right side of alpha, and never describes", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  est <- .gd_est(m, "g4", 1.5)
  hi <- .gd_p(m, "g4hi", 0.2)
  lo <- .gd_p(m, "g4lo", 0.01)
  g <- sr_claim_check_text(paste0("The OR was significantly higher ", est, " (p = ", hi, ")."), "a_lr", m)
  expect_identical(g$rule, "significance")
  expect_identical(g$severity, "block")
  # no p value in the sentence
  expect_identical(sr_claim_check_text(paste0("The OR was significant ", est, "."), "a_lr", m)$rule, "significance")
  # significant with p below alpha is allowed; not significant with p below alpha is not
  expect_identical(nrow(sr_claim_check_text(paste0("The OR was significant ", est, " (p = ", lo, ")."), "a_lr", m)), 0L)
  expect_identical(sr_claim_check_text(paste0("The OR was not significant ", est, " (p = ", lo, ")."), "a_lr", m)$rule,
                   "significance")
  expect_identical(sr_claim_check_text(paste0("The OR was non-significant ", est, " (p = ", lo, ")."), "a_lr", m)$rule,
                   "significance")
  expect_identical(nrow(sr_claim_check_text(paste0("The OR was not significant ", est, " (p = ", hi, ")."), "a_lr", m)), 0L)
  expect_identical(nrow(sr_claim_check_text(paste0("The OR was non-significant ", est, " (p = ", hi, ")."), "a_lr", m)), 0L)
  # a describe route never uses the word
  expect_true("significance" %in% sr_claim_check_text(paste0("Groups differed significantly (p = ", lo, ")."), "t1", m)$rule)
  # the alpha of the document decides
  m$meta$alpha <- 0.001
  expect_identical(sr_claim_check_text(paste0("The OR was significant ", est, " (p = ", lo, ")."), "a_lr", m)$rule,
                   "significance")
  # a word that merely contains "significant" does not match
  expect_identical(nrow(sr_claim_check_text("The insignificant remainder was dropped.", "a_lr", m)), 0L)
})

test_that("G4 judges an adjusted p value when the sentence has one", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  est <- .gd_est(m, "g4a", 1.5)
  raw <- .gd_p(m, "g4raw", 0.01)
  adj <- .gd_put(m, "0.060", "p", "family:test:1:p_adj", value = 0.06)
  s <- paste0("The OR was significant ", est, " (p = ", raw, ", adjusted p = ", adj, ").")
  expect_identical(sr_claim_check_text(s, "a_lr", m)$rule, "significance")
  s2 <- paste0("The OR was not significant ", est, " (p = ", raw, ", adjusted p = ", adj, ").")
  expect_identical(nrow(sr_claim_check_text(s2, "a_lr", m)), 0L)
})

test_that("G5 absence of evidence phrases block", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  g <- sr_claim_check_text("There was no difference between groups.", "a_lr", m)
  expect_identical(g$rule, "absence_of_evidence")
  expect_identical(g$severity, "block")
  for (s in c("There was no effect.", "There was no association.", "The groups did not differ.",
              "Age was not associated with the outcome.", "Both were not associated with the outcome.",
              "No Difference was seen.")) {
    expect_true("absence_of_evidence" %in% sr_claim_check_text(s, "a_lr", m)$rule, info = s)
  }
  expect_identical(nrow(sr_claim_check_text("There was a difference between groups.", "a_lr", m)), 0L)
})

test_that("G6 direction words must agree with the estimate", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  below <- .gd_est(m, "g6lo", 0.8)
  above <- .gd_est(m, "g6hi", 1.4)
  g <- sr_claim_check_text(paste0("Risk was higher (", below, ")."), "a_lr", m)
  expect_identical(g$rule, "direction")
  expect_identical(g$severity, "block")
  expect_identical(nrow(sr_claim_check_text(paste0("Risk was higher (", above, ")."), "a_lr", m)), 0L)
  expect_identical(nrow(sr_claim_check_text(paste0("Risk was lower (", below, ")."), "a_lr", m)), 0L)
  expect_identical(sr_claim_check_text(paste0("Risk was lower (", above, ")."), "a_lr", m)$rule, "direction")
  for (w in c("greater", "more", "elevated", "longer", "increased")) {
    expect_true("direction" %in% sr_claim_check_text(paste0("Risk was ", w, " (", below, ")."), "a_lr", m)$rule, info = w)
  }
  for (w in c("reduced", "decreased", "fewer", "less", "shorter")) {
    expect_true("direction" %in% sr_claim_check_text(paste0("Risk was ", w, " (", above, ")."), "a_lr", m)$rule, info = w)
  }
  # a difference scale has its null at 0
  dneg <- .gd_est(m, "g6d", -1.5, scale = "difference")
  expect_identical(sr_claim_check_text(paste0("The mean was higher (", dneg, ")."), "a_lr", m)$rule, "direction")
  expect_identical(nrow(sr_claim_check_text(paste0("The mean was lower (", dneg, ")."), "a_lr", m)), 0L)
  # an estimate with no null or no finite value is not judged
  nn <- .gd_put(m, "0.80", "est", "effect:test:g6nn:est", value = 0.8)
  expect_identical(nrow(sr_claim_check_text(paste0("Risk was higher (", nn, ")."), "a_lr", m)), 0L)
  # two estimates in one sentence: the direction is not checked, and says so
  g2 <- sr_claim_check_text(paste0("Risk was higher (", below, ") than (", above, ")."), "a_lr", m)
  expect_identical(g2$rule, "direction")
  expect_identical(g2$severity, "warn")
  expect_match(g2$detail, "direction not checked", fixed = TRUE)
})

test_that("G7 an exploratory analysis is worded as hypothesis-generating, never as confirmed", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  l <- sr_ph_put(m$reg, "x_age", "label", "label:x_age")
  g <- sr_claim_check_text(paste0("Analysis ", l, " confirms that groups differ."), "x_age", m)
  expect_true(all(g$rule == "hypothesis_generating"))
  expect_gte(nrow(g), 1L)
  expect_true(all(g$severity == "block"))
  # exploratory wording present, confirmatory word present: still blocked
  g2 <- sr_claim_check_text(paste0("In exploratory analysis ", l, " the finding was established."), "x_age", m)
  expect_identical(g2$rule, "hypothesis_generating")
  # missing the wording only
  g3 <- sr_claim_check_text(paste0("In analysis ", l, " the groups were compared."), "x_age", m)
  expect_identical(g3$rule, "hypothesis_generating")
  expect_match(g3$detail, "exploratory", fixed = TRUE)
  # fine
  expect_identical(nrow(sr_claim_check_text(paste0("This hypothesis-generating analysis ", l, " compared groups."), "x_age", m)), 0L)
  expect_identical(nrow(sr_claim_check_text(paste0("In exploratory analysis ", l, " groups were compared."), "x_age", m)), 0L)
  # each sentence needs it
  two <- paste0("In exploratory analysis ", l, " groups were compared. The groups were compared again.")
  expect_identical(sr_claim_check_text(two, "x_age", m)$rule, "hypothesis_generating")
  # a confirmatory analysis is not held to it, and a methods sentence is not a results sentence
  expect_identical(nrow(sr_claim_check_text("Groups were compared.", "a_lr", m)), 0L)
  expect_identical(nrow(sr_claim_check_text("Groups were compared.", "x_age", m, section = "methods")), 0L)
  for (w in c("confirm", "confirmed", "demonstrates", "demonstrated", "proves", "proved", "definitive", "establishes")) {
    s <- paste0("In exploratory analysis ", l, " the data ", w, " it.")
    expect_identical(sr_claim_check_text(s, "x_age", m)$rule, "hypothesis_generating", info = w)
  }
  expect_identical(nrow(sr_claim_check_text(paste0("In exploratory analysis ", l, " the data are confirmatory."), "x_age", m)), 0L)
})

test_that("G8 Table 2: a covariate coefficient quoted in the prose of a causal route blocks", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  eff <- .fx_writer_result(p, "a_lr")$effects
  ri <- which(eff$role == "covariate" & eff$term == "age")
  expect_length(ri, 1L)
  cov_est <- sr_ph_put(m$reg, "1.03", "est", sprintf("effect:a_lr:%d:est", ri), analysis = "a_lr",
                       value = eff$est[ri], scale = "ratio", null = 1)
  g <- sr_claim_check_text(paste0("The adjusted (conditional) odds ratio for age was ", cov_est, "."), "a_lr", m)
  expect_identical(g$rule, "table2")
  expect_identical(g$severity, "block")
  # the primary row of the same analysis is fine
  pi <- which(eff$role == "primary")[1L]
  prim <- sr_ph_put(m$reg, "1.70", "est", sprintf("effect:a_lr:%d:est", pi), analysis = "a_lr", value = eff$est[pi],
                    scale = "ratio", null = 1)
  expect_identical(nrow(sr_claim_check_text(paste0("The adjusted (conditional) odds ratio for grp was ", prim, "."),
                                            "a_lr", m)), 0L)
  # the p value and the interval of a covariate row are covered too
  cov_p <- sr_ph_put(m$reg, "0.008", "p", sprintf("effect:a_lr:%d:p", ri), analysis = "a_lr", value = eff$p[ri])
  expect_true("table2" %in% sr_claim_check_text(paste0("Age was adjusted (conditional) at p = ", cov_p, "."), "a_lr", m)$rule)
  # a route that is not explain-causal is not held to it
  m$meta$purposes[["a_lr"]] <- "predict"
  expect_false("table2" %in% sr_claim_check_text(paste0("The adjusted (conditional) odds ratio for age was ", cov_est, "."),
                                                 "a_lr", m)$rule)
})

test_that("G9 non-collapsibility: a conditional OR or HR must be called conditional or adjusted", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  eff <- .fx_writer_result(p, "a_lr")$effects
  pi <- which(eff$role == "primary" & eff$conditional)[1L]
  tok <- sr_ph_put(m$reg, "1.70", "est", sprintf("effect:a_lr:%d:est", pi), analysis = "a_lr", value = eff$est[pi],
                   scale = "ratio", null = 1)
  g <- sr_claim_check_text(paste0("The OR was ", tok, "."), "a_lr", m)
  expect_identical(g$rule, "non_collapsible")
  expect_identical(g$severity, "warn")
  expect_identical(nrow(sr_claim_check_text(paste0("The adjusted OR was ", tok, "."), "a_lr", m)), 0L)
  expect_identical(nrow(sr_claim_check_text(paste0("The conditional OR was ", tok, "."), "a_lr", m)), 0L)
  # a marginal row (not conditional) is not flagged
  mi <- which(eff$measure == "RR" & !eff$conditional)[1L]
  mt <- sr_ph_put(m$reg, "1.27", "est", sprintf("effect:a_lr:%d:est", mi), analysis = "a_lr", value = eff$est[mi],
                  scale = "ratio", null = 1)
  expect_identical(nrow(sr_claim_check_text(paste0("The risk ratio was ", mt, "."), "a_lr", m)), 0L)
  # the hazard ratio of the Cox analysis
  ec <- .fx_writer_result(p, "a_cox")$effects
  hi <- which(ec$role == "primary")[1L]
  ht <- sr_ph_put(m$reg, "1.61", "est", sprintf("effect:a_cox:%d:est", hi), analysis = "a_cox", value = ec$est[hi],
                  scale = "ratio", null = 1)
  expect_identical(sr_claim_check_text(paste0("The HR was ", ht, "."), "a_cox", m)$rule, "non_collapsible")
})

test_that("no false flags: the writer's own sentence shapes pass every rule", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  eff <- .fx_writer_result(p, "a_lr")$effects
  pi <- which(eff$role == "primary" & eff$conditional)[1L]
  est <- sr_ph_put(m$reg, "1.70", "est", sprintf("effect:a_lr:%d:est", pi), analysis = "a_lr", value = eff$est[pi],
                   scale = "ratio", null = 1)
  ci <- sr_ph_put(m$reg, "1.2 to 2.4", "ci", sprintf("effect:a_lr:%d:lo_hi", pi), analysis = "a_lr")
  pt <- sr_ph_put(m$reg, "0.011", "p", sprintf("effect:a_lr:%d:p", pi), analysis = "a_lr", value = eff$p[pi])
  l95 <- sr_ph_put(m$reg, "95%", "label", "label:95%")
  s <- paste0("The adjusted (conditional) odds ratio for the exposure was ", est, " (", l95, " CI ", ci, "); p = ", pt, ".")
  expect_identical(nrow(sr_claim_check_text(s, "a_lr", m)), 0L)
  # a sentence ends only at ". ": a semicolon splits nothing
  expect_identical(nrow(sr_claim_check_text(paste0("The adjusted risk was higher ", est, "; the interval was ", ci, "."), "a_lr", m)), 0L)
  # "insignificant" is not "significant"
  expect_identical(nrow(sr_claim_check_text("An insignificant remainder.", "a_lr", m)), 0L)
  l65 <- sr_ph_put(m$reg, "65+", "label", "label:65+")
  expect_identical(nrow(sr_claim_check_text(paste0("in the ", l65, " group"), "t1", m)), 0L)
})

test_that("the check validates its arguments", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  expect_error(sr_claim_check_text(1, "a_lr", m), "text")
  expect_error(sr_claim_check_text(c("a", "b"), "a_lr", m), "text")
  expect_error(sr_claim_check_text("a", "a_lr", list()), "model")
  expect_error(sr_claim_guard(list()), "model")
  expect_identical(nrow(sr_claim_check_text("", "a_lr", m)), 0L)
  expect_identical(nrow(sr_claim_check_text("A sentence.", NA_character_, m)), 0L)
})

test_that("the writer's own document is clean when a primary analysis is exploratory, and with a bootstrap", {
  p <- .fx_writer_build(environment(), decision = "exploratory")
  m <- sr_doc_model(p)
  expect_identical(m$meta$wording[["t1"]], "exploratory")
  g <- sr_claim_guard(m)
  expect_identical(nrow(g), 0L, info = paste(g$block, g$rule, g$detail, collapse = "\n"))
  p2 <- .fx_writer_build(environment(), boot = TRUE, drop = c("x_age", "a_rmst"))
  g2 <- sr_claim_guard(sr_doc_model(p2))
  expect_identical(nrow(g2), 0L, info = paste(g2$block, g2$rule, g2$detail, collapse = "\n"))
})

test_that("G4 reads 'not statistically significant' and 'not significantly' as no-significance claims", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  est <- .gd_est(m, "g4n", 1.5)
  hi <- .gd_p(m, "g4nhi", 0.2)
  lo <- .gd_p(m, "g4nlo", 0.01)
  say <- function(phrase, pt) sr_claim_check_text(paste0("The OR ", phrase, " ", est, " (p = ", pt, ")."), "a_lr", m)
  for (ph in c("was not statistically significant", "was not significantly different", "was not significantly associated",
               "was Not Statistically Significant")) {
    expect_identical(nrow(say(ph, hi)), 0L, info = ph)
    g <- say(ph, lo)
    expect_identical(g$rule, "significance", info = ph)
    expect_identical(g$severity, "block", info = ph)
  }
  # a positive claim is still judged as positive
  expect_identical(say("was statistically significant", hi)$rule, "significance")
  expect_identical(nrow(say("was statistically significant", lo)), 0L)
})

test_that("G2 applies when the analysis is NA", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  g <- sr_claim_check_text("Smoking causes the outcome.", NA_character_, m)
  expect_identical(g$rule, "causal_language")
  expect_true(is.na(g$analysis))
})

test_that("a figure caption finding carries the '<figure id>:caption' block id", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  i <- which(vapply(m$main, function(b) identical(b$type, "figure") && identical(b$id, "forest_ratio"), NA))
  expect_length(i, 1L)
  m$main[[i]]$caption <- "Estimates showing the effect of age"
  g <- sr_claim_guard(m)
  expect_identical(g$block, "forest_ratio:caption")
  expect_identical(g$rule, "causal_language")
})
