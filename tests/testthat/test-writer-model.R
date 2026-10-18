# Tests for writer/fragments.R and writer/model.R: the Methods and Results templates and the document
# model that assembles every block of the manuscript. Synthetic data only (the writer fixture of
# helper-writer.R); variants use the uncached builder.

.md_cite <- function(k) paste0("[", k, "]")
.md_paras <- function(blocks) Filter(function(b) identical(b$type, "para"), blocks)
.md_of <- function(blocks, analysis, section = NULL) {
  Filter(function(b) {
    identical(b$type, "para") && identical(b$analysis, analysis) && (is.null(section) || identical(b$section, section))
  }, blocks)
}
.md_text <- function(m, b) sr_ph_resolve(b$text, m$reg, cite = .md_cite)
.md_all_text <- function(m, blocks) vapply(.md_paras(blocks), function(b) .md_text(m, b), "")
.md_heads <- function(blocks) {
  vapply(Filter(function(b) identical(b$type, "heading"), blocks), function(b) b$text, "")
}
.md_ids <- function(blocks, type) {
  vapply(Filter(function(b) identical(b$type, type), blocks), function(b) b$id, "")
}

# A stub route whose run fails, removed when the calling test ends (as in test-run.R).
.md_fail_route <- function(env = parent.frame()) {
  est <- sr_estimand(population = "adults", variables = "y", intercurrent = "none",
                     summary = "stub", level = "marginal")
  present <- function(value, spec) {
    list(tables = list(), figures = list(), fragments = list(), effects = sr_effects_empty())
  }
  r <- sr_route(id = "stub_md_fail", version = 1L, family = "stub", purpose = "explain-causal", estimand = est,
                spec_schema = list(outcome = list(type = "chr", required = TRUE),
                                   seed = list(type = "num", required = FALSE)),
                compile = function(spec) "stop(\"boom\")", present = present)
  sr_route_register(r)
  withr::defer(rm(list = .sr_route_key("stub_md_fail", 1L), envir = .sr_routes), envir = env)
  invisible(r)
}

test_that("sr_writer_opts returns the validated plain list the table builders read", {
  o <- sr_writer_opts()
  expect_identical(names(o), c("preset", "suppress_k", "table2", "alpha"))
  expect_identical(o$preset, "default")
  expect_identical(o$suppress_k, SR_SDC_K)
  expect_identical(o$table2, "label")
  expect_equal(o$alpha, 0.05)
  expect_identical(.sr_wr_opts(o)$suppress_k, 5L)
  o2 <- sr_writer_opts(preset = "bmj", suppress_k = 10, table2 = "suppress", alpha = 0.01)
  expect_identical(o2$table2, "suppress")
  expect_identical(o2$suppress_k, 10L)
  expect_error(sr_writer_opts(table2 = "hide"), "table2")
  expect_error(sr_writer_opts(suppress_k = 1), "suppress_k")
  # one threshold per document, never looser than the floor (ruling R15)
  for (k in 2:4) expect_error(sr_writer_opts(suppress_k = k), "at least 5")
  expect_identical(sr_writer_opts(suppress_k = 5)$suppress_k, 5L)
  expect_error(sr_writer_opts(suppress_k = 4.5), "suppress_k")
  expect_error(sr_writer_opts(alpha = 1.5), "alpha")
  expect_error(sr_writer_opts(alpha = NA_real_), "alpha")
  expect_error(sr_writer_opts(preset = c("a", "b")), "preset")
  expect_error(sr_writer_opts(preset = NA_character_), "preset")
  expect_error(sr_doc_model(structure(list(), class = "x"), opts = list(table2 = "label")), "opts")
})

test_that("an unlocked SAP, a missing result and a failed primary run are refused, naming every offender", {
  p0 <- .fx_ready_project(.fx_writer_frame(), "t", c(id = "id", y = "outcome"), env = environment())
  expect_error(sr_doc_model(p0), "not locked")

  p <- .fx_writer_project(environment())
  key <- p$state$runs$a_lr$ok_key
  f <- file.path(p$paths$fits, paste0(key, ".rds"))
  expect_true(file.exists(f))
  unlink(f)
  err <- tryCatch(sr_doc_model(p), error = function(e) conditionMessage(e))
  expect_match(err, "a_lr", fixed = TRUE)
  expect_match(err, "run it again", fixed = TRUE)
  # a second missing result is named too (a_rmst shares no fit with a_lr)
  unlink(file.path(p$paths$fits, paste0(p$state$runs$a_rmst$ok_key, ".rds")))
  err2 <- tryCatch(sr_doc_model(p), error = function(e) conditionMessage(e))
  expect_match(err2, "a_lr", fixed = TRUE)
  expect_match(err2, "a_rmst", fixed = TRUE)

  .fx_writer_routes()
  .md_fail_route()
  pf <- .fx_ready_project(.fx_writer_frame(), "t", c(id = "id", y = "outcome"), env = environment())
  pf <- sr_sap_add(pf, "a1", "stub_md_fail", list(purpose = "explain-causal", outcome = "y",
                                                  layers = .fx_writer_layers(), seed = 42L),
                   "t", label = "primary")
  pf <- sr_sap_lock(.fx_feasible(pf))
  pf <- sr_run(pf, "a1")
  expect_false(pf$state$runs$a1$ok)
  err3 <- tryCatch(sr_doc_model(pf), error = function(e) conditionMessage(e))
  expect_match(err3, "a1", fixed = TRUE)
  expect_match(err3, "primary", fixed = TRUE)
})

test_that("the main document and the supplement hold the right analyses, headings and tables", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  expect_named(m, c("meta", "reg", "main", "supplement", "not_reported", "dropped_families"))
  expect_length(m$dropped_families, 0L)
  expect_identical(m$reg$k, 5L)
  expect_s3_class(m$reg, "sr_ph")
  expect_identical(.md_heads(m$main)[1:2], c("Methods", "Statistical analysis"))
  expect_true("Results" %in% .md_heads(m$main))
  expect_identical(.md_heads(m$supplement), c("Sensitivity analyses", "Supplementary tables"))
  who <- function(blocks) unique(vapply(.md_paras(blocks), function(b) b$analysis, NA_character_))
  expect_setequal(stats::na.omit(who(m$main)), c("t1", "a_lr", "a_cox", "a_rmst", "x_age"))
  expect_identical(stats::na.omit(who(m$supplement)), "a_mp")
  expect_false(any(vapply(.md_paras(m$main), function(b) identical(b$analysis, "a_mp"), NA)))
  # every para has the fields the docx renderer reads
  for (b in .md_paras(c(m$main, m$supplement))) {
    expect_true(all(c("id", "text", "analysis", "section") %in% names(b)))
    expect_true(b$section %in% c("methods", "results"))
  }
  ids <- vapply(.md_paras(c(m$main, m$supplement)), function(b) b$id, "")
  expect_identical(anyDuplicated(ids), 0L)
  # the order of the Methods: lock sentence, analyses, then the software sentence last
  meth <- Filter(function(b) identical(b$section, "methods"), .md_paras(m$main))
  expect_match(.md_text(m, meth[[1L]]), "^The statistical analysis plan was locked on ")
  expect_match(.md_text(m, meth[[length(meth)]]), "^Analyses were run in R ")
  # the tables and figures of Task 3
  expect_true("main_results" %in% .md_ids(m$main, "table"))
  expect_true("t1_table1" %in% .md_ids(m$main, "table"))
  expect_true(all(c("families", "deviations") %in% .md_ids(m$supplement, "table")))
  expect_setequal(.md_ids(m$main, "table"), c("t1_table1", "main_results"))
  expect_setequal(.md_ids(m$supplement, "table"),
                  c("t1_missing", "a_lr_coefficients", "a_lr_model_fit", "a_lr_observed_risk", "a_lr_marginal",
                    "a_cox_coefficients", "a_cox_model_fit", "a_rmst_rmst", "a_rmst_rmst_difference",
                    "x_age_coefficients", "x_age_model_fit", "a_mp_coefficients", "a_mp_model_fit",
                    "a_mp_observed_risk", "a_mp_marginal", "families", "deviations"))
  expect_true(length(.md_ids(m$main, "figure")) >= 1L)
  expect_true(all(vapply(Filter(function(b) identical(b$type, "table"), m$main), function(b) identical(b$where, "main"), NA)))
  expect_true(all(vapply(Filter(function(b) identical(b$type, "table"), m$supplement), function(b) identical(b$where, "supplement"), NA)))
  expect_identical(nrow(m$not_reported), 0L)
  expect_named(m$not_reported, c("id", "label", "reason"))
})

test_that("meta records the lock, the library snapshot, the purposes and the wording", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p, sr_writer_opts(preset = "bmj", alpha = 0.01))
  expect_named(m$meta, c("sap_hash", "locked_at", "libsnap", "preset", "alpha", "purposes", "wording", "effects"))
  expect_identical(m$meta$sap_hash, p$state$sap$lock$sap_hash)
  expect_identical(m$meta$locked_at, p$state$sap$lock$locked_at)
  expect_identical(m$meta$preset, "bmj")
  expect_equal(m$meta$alpha, 0.01)
  expect_identical(m$meta$libsnap$r, R.version.string)
  expect_identical(unname(m$meta$purposes[c("t1", "a_lr", "a_mp")]), c("describe", "explain-causal", "explain-causal"))
  expect_setequal(names(m$meta$purposes), c("t1", "a_lr", "a_cox", "a_rmst", "a_mp", "x_age"))
  expect_identical(m$meta$wording[["x_age"]], "exploratory")
  expect_identical(m$meta$wording[["a_lr"]], "confirmatory")
  expect_identical(names(m$meta$wording), names(m$meta$purposes))
})

test_that("no template holds a literal digit, and every placeholder resolves to its source", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  for (b in .md_paras(c(m$main, m$supplement))) {
    expect_identical(sr_ph_digits(b$text), character(0), info = b$id)
  }
  v <- sr_ph_verify(m$reg, p)
  expect_true(nrow(v) > 30L)
  expect_true(all(v$ok), info = paste(v$source[!v$ok], v$got[!v$ok], collapse = "; "))
  # every token of every para is in the registry (resolving errors on an unknown one)
  expect_no_error(.md_all_text(m, c(m$main, m$supplement)))
})


.md_p <- function(lead, p) {
  s <- sr_fmt_p(p)
  paste0(lead, if (grepl("^[<>]", s)) " " else " = ", s)
}
.md_by_id <- function(blocks, id) {
  hit <- Filter(function(b) identical(b$type, "para") && identical(b$id, id), blocks)
  if (length(hit) != 1L) stop("expected exactly one para '", id, "', found ", length(hit), call. = FALSE)
  hit[[1L]]
}
.md_say <- function(m, blocks, id) .md_text(m, .md_by_id(blocks, id))

test_that("the Methods say what was locked, how each analysis was run and what was assessed", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  lock <- p$state$sap$lock
  expect_identical(
    .md_say(m, m$main, "methods_lock"),
    sprintf("The statistical analysis plan was locked on %s (SHA-256 %s) before any outcome was analysed.",
            substr(lock$locked_at, 1, 10), substr(lock$sap_hash, 1, 16)))
  a <- .md_say(m, m$main, "methods_a_lr")
  expect_match(a, "^Primary analysis a_lr: Logistic regression of y on grp")
  expect_match(a, "Marginal risk difference and ratio by g-computation", fixed = TRUE)
  expect_match(a, "(209 events)", fixed = TRUE)
  expect_match(.md_say(m, m$main, "methods_a_cox"), "^Secondary analysis a_cox: ")
  expect_match(.md_say(m, m$main, "methods_x_age"), "^Exploratory analysis x_age: ")
  expect_match(.md_say(m, m$main, "methods_t1"), "^Primary analysis t1: ")
  expect_match(.md_say(m, m$supplement, "methods_a_mp"), "^Sensitivity analysis a_mp: ")
  # the numbers of a fragment are tokens whose sources are the fragment
  expect_true(any(startsWith(sr_ph_table(m$reg)$source, "fragment:a_lr:glm_note:")))
  # the feasibility sentence of a primary analysis with an assessment
  ft <- .md_say(m, m$main, "methods_feas_a_lr")
  expect_match(ft, paste0("^Before the plan was locked, the expected precision was assessed from the complete cases ",
                          "\\(n = 400, expected events 209\\): expected 95% CI half-width [0-9.]+ and minimum ",
                          "detectable effect at 80% power [0-9.]+; precision was rated precision only\\.$"))
  # t1 has an assessment that is not applicable: no sentence
  expect_length(Filter(function(b) identical(b$id, "methods_feas_t1"), m$main), 0L)
  expect_identical(.md_say(m, m$main, "methods_family_fam1"),
                   "P-values of family fam1 (a_lr, a_cox) were adjusted by the holm method.")
  expect_identical(.md_say(m, m$main, "methods_deviations"),
                   "Deviations from the locked plan are listed in the supplementary material.")
  sw <- .md_say(m, m$main, "methods_software")
  rv <- sub("^R version ([0-9.]+) .*$", "\\1", R.version.string)
  expect_true(startsWith(sw, paste0("Analyses were run in R ", rv, " [r_core]")))
  expect_match(sw, "the exact code is supplied as analysis.R.", fixed = TRUE)
  # the sources of the lock fields, the R version and the feasibility numbers
  src <- sr_ph_table(m$reg)
  expect_true(all(c("sap:locked_at", "sap:sap_hash", "libsnap:r", "feas:a_lr:n", "feas:a_lr:events", "feas:a_lr:h",
                    "feas:a_lr:mde") %in% src$source))
  expect_identical(src$kind[src$source == "feas:a_lr:n"], "count")
})

test_that("a Results sentence per primary effects row carries the row's number, scale and null", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  tab <- sr_ph_table(m$reg)
  e <- sr_result(p, "a_lr")$effects
  e <- e[e$role == "primary", , drop = FALSE]
  expect_identical(nrow(e), 3L)
  lr <- .md_say(m, m$main, "results_a_lr")
  s <- strsplit(lr, "(?<=\\.) (?=In analysis)", perl = TRUE)[[1L]]
  expect_length(s, 3L)
  est1 <- .sr_wr_eff_text(p, "a_lr", 1L, "est")
  ci1 <- .sr_wr_eff_text(p, "a_lr", 1L, "lo_hi")
  expect_identical(s[1L], sprintf(
    "In analysis a_lr, the adjusted (conditional) odds ratio for grp: B vs A was %s (95%% CI %s)%s%s.",
    est1, ci1, .md_p("; p", e$p[1L]), .md_p(", adjusted p", sr_family_adjust(p, "fam1")$p_adj[1L])))
  expect_match(s[2L], "^In analysis a_lr, the marginal \\(standardised\\) risk difference for grp: B vs A \\(marginal RD\\) was ")
  expect_match(s[3L], "the marginal \\(standardised\\) risk ratio for ")
  expect_false(grepl("adjusted p", s[2L], fixed = TRUE))   # only the first measure is in the family
  # the est placeholder has the row's scale and null
  r1 <- tab[tab$source == "effect:a_lr:1:est", ]
  expect_identical(r1$scale, "ratio")
  expect_equal(r1$null, 1)
  r6 <- tab[tab$source == "effect:a_lr:6:est", ]
  expect_identical(r6$scale, "difference")
  expect_equal(r6$null, 0)
  expect_match(.md_say(m, m$main, "results_a_cox"),
               "^In analysis a_cox, the adjusted \\(conditional\\) hazard ratio for grp: B vs A was ")
  rm <- .md_say(m, m$main, "results_a_rmst")
  expect_match(rm, "^In analysis a_rmst, the unadjusted difference in restricted mean survival time for grp: B vs A was ")
  expect_false(grepl("adjusted p", rm, fixed = TRUE))
  expect_match(rm, "; p (= |<|>)")
  expect_equal(tab$null[tab$source == "effect:a_rmst:1:est"], 0)
  # an exploratory analysis says so in the lead
  expect_match(.md_say(m, m$main, "results_x_age"),
               "^In exploratory, hypothesis-generating analysis x_age, the unadjusted mean difference for grp: B vs A was ")
  # a model with covariates but no conditional flag is adjusted
  expect_match(.md_say(m, m$supplement, "results_a_mp"), "^In analysis a_mp, the adjusted risk ratio for grp: B vs A was ")
  # no direction words anywhere
  txt <- .md_all_text(m, c(m$main, m$supplement))
  expect_false(any(grepl("\\b(higher|lower|greater|increase|decrease)", txt)))
  # the Table 1 sentence and the pointers
  expect_identical(.md_say(m, m$main, "results_table1_t1"), "Baseline characteristics by grp are shown in Table 1.")
  expect_identical(.md_say(m, m$main, "results_sensitivity_note"),
                   "Sensitivity analyses are reported in the supplementary material.")
  # the describe analysis has no effect sentence
  expect_length(Filter(function(b) identical(b$id, "results_t1"), m$main), 0L)
})

test_that("a decision to treat a primary analysis as exploratory changes its Methods and Results wording", {
  p <- .fx_writer_build(environment(), decision = "exploratory")
  m <- sr_doc_model(p)
  expect_identical(m$meta$wording[["a_lr"]], "exploratory")
  expect_identical(m$meta$wording[["a_cox"]], "confirmatory")
  expect_match(.md_say(m, m$main, "results_a_lr"), "^In exploratory, hypothesis-generating analysis a_lr, the adjusted")
  # the Table 1 sentence of an exploratory analysis says so too (claim guard G7)
  expect_identical(.md_say(m, m$main, "results_table1_t1"),
                   "In exploratory, hypothesis-generating analysis t1, baseline characteristics by grp are shown in Table 1.")
  ft <- .md_say(m, m$main, "methods_feas_a_lr")
  expect_true(endsWith(ft, " Because of this, the analysis is reported as exploratory and hypothesis-generating (test fixture)."))
  expect_true(all(sr_ph_verify(m$reg, p)$ok))
})

test_that("a bootstrap interval has no p clause and names its method", {
  p <- .fx_writer_build(environment(), boot = TRUE)
  m <- sr_doc_model(p)
  e <- sr_result(p, "a_mp")$effects
  expect_true(any(startsWith(e$ci_method, "boot_")))
  expect_true(all(is.na(e$p[startsWith(e$ci_method, "boot_")])))
  txt <- .md_say(m, m$supplement, "results_a_mp")
  expect_match(txt, "bootstrap CI (boot_", fixed = TRUE)
  expect_false(grepl("; p = ", txt, fixed = TRUE))
  expect_false(grepl("95% CI", txt, fixed = TRUE))
  expect_true(all(sr_ph_verify(m$reg, p)$ok))
})

test_that("a fired replace_primary rule reports the alternative as primary and the replaced one as sensitivity", {
  p <- .fx_writer_project(environment())
  p <- sr_project_append(p, "sap.rule_fired", list(rule = "r1", analysis = "a_lr", check = "ph",
                                                  action = "replace_primary", alternative = "a_mp", detail = "test"))
  expect_identical(p$state$analyses$a_mp$label, "primary")
  expect_identical(p$state$analyses$a_lr$label, "sensitivity")
  m <- sr_doc_model(p)
  expect_true("a_mp" %in% stats::na.omit(vapply(.md_paras(m$main), function(b) b$analysis, NA_character_)))
  expect_match(.md_say(m, m$main, "methods_a_mp"), "^Primary analysis a_mp: ")
  expect_match(.md_say(m, m$supplement, "methods_a_lr"), "^Sensitivity analysis a_lr: ")
  expect_match(.md_say(m, m$supplement, "results_a_lr"), "^In analysis a_lr, ")
  expect_length(Filter(function(b) identical(b$id, "methods_a_lr"), m$main), 0L)
  # the rule is a deviation: it is in the supplement table
  expect_true("deviations" %in% .md_ids(m$supplement, "table"))
  expect_true(all(sr_ph_verify(m$reg, p)$ok))
})

test_that("a fired replace_primary alternative is reported in the family of the analysis it replaced", {
  p <- .fx_writer_project(environment())
  p <- sr_project_append(p, "sap.rule_fired", list(rule = "r1", analysis = "a_lr", check = "ph",
                                                  action = "replace_primary", alternative = "a_mp", detail = "test"))
  expect_identical(.sr_wr_family_id_of(p, "a_mp"), "fam1")
  expect_identical(.sr_wr_family_id_of(p, "a_cox"), "fam1")
  expect_true(is.na(.sr_wr_family_id_of(p, "x_age")))
  expect_true(is.na(.sr_wr_family_id_of(p, "nope")))
  m <- sr_doc_model(p)
  fa <- sr_family_adjust(p, "fam1")
  r <- which(fa$analysis == "a_mp")[1L]
  expect_false(is.na(fa$p_adj[r]))
  padj <- sr_fmt_p(fa$p_adj[r])
  txt <- .md_say(m, m$main, "results_a_mp")
  expect_match(txt, paste0(", adjusted p ", if (grepl("^[<>]", padj)) "" else "= ", padj), fixed = TRUE)
  # the main-table row of a_mp carries the same adjusted p
  mt <- Filter(function(b) identical(b$id, "main_results"), m$main)[[1L]]$df
  expect_true("Adjusted p" %in% names(mt))
  row <- mt[mt$Analysis == "a_mp", , drop = FALSE]
  expect_true(nrow(row) >= 1L)
  expect_identical(sr_ph_resolve(row[["Adjusted p"]][1L], m$reg, cite = .md_cite), padj)
  expect_true(all(sr_ph_verify(m$reg, p)$ok))
  # the replaced analysis is a sensitivity analysis now: no adjusted p of its own
  expect_false(grepl("adjusted p", .md_say(m, m$supplement, "results_a_lr"), fixed = TRUE))
})

test_that("a count of one to four in a fragment prints as <5, suppressed, with no value", {
  p <- .fx_writer_project(environment())
  real <- sr_result
  withr::defer(assign("sr_result", real, envir = globalenv()))
  assign("sr_result", function(project, analysis_id) {
    r <- real(project, analysis_id)
    if (identical(analysis_id, "a_lr")) {
      r$fragments$glm_note <- sub("209 events", "3 events", r$fragments$glm_note, fixed = TRUE)
    }
    r
  }, envir = globalenv())
  m <- sr_doc_model(p)
  tb <- sr_ph_table(m$reg)
  hit <- tb[startsWith(tb$source, "fragment:a_lr:glm_note:") & tb$kind == "count" & tb$suppressed, ]
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$text, "<5")
  expect_true(is.na(hit$value))
  expect_match(.md_say(m, m$main, "methods_a_lr"), "(<5 events)", fixed = TRUE)
  expect_true(all(sr_ph_verify(m$reg, p)$ok))
  # the other counts of the fragment are shown
  expect_true(any(tb$kind == "count" & !tb$suppressed & tb$text == "400"))
})

test_that("an analysis that did not run is listed, and a family it belongs to is not adjusted", {
  p <- .fx_writer_build(environment(), drop = "x_age", unrun = c("a_rmst", "a_cox"))
  m <- sr_doc_model(p)
  expect_identical(m$not_reported$id, c("a_cox", "a_rmst"))
  expect_identical(m$not_reported$label, c("secondary", "secondary"))
  expect_identical(m$not_reported$reason, c("not run", "not run"))
  who <- stats::na.omit(vapply(.md_paras(c(m$main, m$supplement)), function(b) b$analysis, NA_character_))
  expect_false(any(c("a_cox", "a_rmst") %in% who))
  nr <- .md_say(m, m$main, "results_not_reported")
  expect_match(nr, "^Not reported: ")
  expect_match(nr, "a_cox", fixed = TRUE)
  expect_match(nr, "a_rmst", fixed = TRUE)
  expect_match(nr, "fam1", fixed = TRUE)          # its adjusted p values cannot be computed
  expect_match(nr, "not yet run successfully: a_cox", fixed = TRUE)   # the real reason
  expect_identical(names(m$dropped_families), "fam1")
  expect_match(m$dropped_families[["fam1"]], "not yet run successfully", fixed = TRUE)
  expect_length(Filter(function(b) identical(b$id, "methods_family_fam1"), m$main), 0L)
  expect_false("families" %in% .md_ids(m$supplement, "table"))
  expect_false("Adjusted p" %in% names(Filter(function(b) identical(b$id, "main_results"), m$main)[[1L]]$df))
  # no x_age and no unplanned analysis: no deviation
  expect_identical(.md_say(m, m$main, "methods_deviations"), "There were no deviations from the locked plan.")
  expect_false("deviations" %in% .md_ids(m$supplement, "table"))
  expect_true(all(sr_ph_verify(m$reg, p)$ok))
  # a latest run that failed is told apart
  q <- .fx_writer_project(environment())
  q$state$runs$a_rmst <- list(analysis = "a_rmst", ok = FALSE, ok_key = NA_character_, error = "boom")
  q$state$runs$x_age <- NULL
  mq <- sr_doc_model(q)
  expect_identical(mq$not_reported$reason[mq$not_reported$id == "a_rmst"], "latest run failed")
  expect_identical(mq$not_reported$reason[mq$not_reported$id == "x_age"], "not run")
})

test_that("building the model twice gives the same registry and the same blocks", {
  p <- .fx_writer_project(environment())
  m1 <- sr_doc_model(p)
  m2 <- sr_doc_model(p)
  expect_identical(sr_ph_table(m1$reg), sr_ph_table(m2$reg))
  expect_identical(sr_canonical_json(c(m1$main, m1$supplement)), sr_canonical_json(c(m2$main, m2$supplement)))
  expect_identical(sr_canonical_json(m1$meta), sr_canonical_json(m2$meta))
})

test_that("the model leaves the result cache as it found it", {
  p <- .fx_writer_project(environment())
  expect_null(.sr_ph_ctx$cache)
  sr_doc_model(p)
  expect_null(.sr_ph_ctx$cache)
  expect_error(sr_doc_model(structure(list(state = list(sap = list(locked = FALSE))), class = "x")), "not locked")
  expect_null(.sr_ph_ctx$cache)
})
test_that("a family dropped for another reason is reported with that reason, with or without unrun analyses", {
  p <- .fx_writer_project(environment())
  real <- sr_family_adjust
  withr::defer(assign("sr_family_adjust", real, envir = globalenv()))
  assign("sr_family_adjust", function(project, family_id) {
    stop("family 'fam1': analysis 'a_cox' has no primary-role effect row", call. = FALSE)
  }, envir = globalenv())
  m <- sr_doc_model(p)
  expect_identical(nrow(m$not_reported), 0L)                 # nothing was unrun: the notice stands alone
  expect_identical(names(m$dropped_families), "fam1")
  nr <- .md_say(m, m$main, "results_not_reported")
  expect_match(nr, "^Adjusted p values of family fam1 are not reported \\(")
  expect_match(nr, "has no primary-role effect row", fixed = TRUE)
  expect_false(grepl("not run", nr, fixed = TRUE))
  expect_false(grepl("Not reported", nr, fixed = TRUE))
  expect_length(Filter(function(b) identical(b$id, "methods_family_fam1"), m$main), 0L)
  expect_true(all(sr_ph_verify(m$reg, p)$ok))
})

test_that("an exploratory decision is stated even when the precision sentence is omitted", {
  p <- .fx_writer_build(environment(), decision = "exploratory")
  real <- sr_feasibility_fragment
  withr::defer(assign("sr_feasibility_fragment", real, envir = globalenv()))
  assign("sr_feasibility_fragment", function(project, id) {
    r <- real(project, id)
    r$values$tier <- "not_applicable"
    r
  }, envir = globalenv())
  m <- sr_doc_model(p)
  ft <- .md_say(m, m$main, "methods_feas_a_lr")
  expect_false(grepl("expected precision", ft, fixed = TRUE))
  expect_identical(ft, "Because of the feasibility assessment, the analysis is reported as exploratory and hypothesis-generating (test fixture).")
  expect_true(all(sr_ph_verify(m$reg, p)$ok))
  # no assessment to read at all: still stated
  assign("sr_feasibility_fragment", function(project, id) stop("no assessment"), envir = globalenv())
  m2 <- sr_doc_model(p)
  expect_match(.md_say(m2, m2$main, "methods_feas_a_lr"), "exploratory and hypothesis-generating", fixed = TRUE)
})

test_that("one small-cell threshold governs prose, tables and the registry of a document (ruling R15)", {
  expect_identical(sr_ph_registry()$k, 5L)
  expect_error(sr_ph_registry(k = 1), "k")
  reg <- sr_ph_registry(k = 10)
  expect_identical(reg$k, 10L)
  tk <- sr_ph_lift("7 events were seen", reg, "fragment:a_lr:glm_note", "a_lr")
  expect_identical(sr_ph_resolve(tk, reg, cite = .md_cite), "<10 events were seen")
  expect_identical(sr_ph_table(reg)$suppressed, TRUE)
  # the default registry shows 7, as before
  reg5 <- sr_ph_registry()
  tk5 <- sr_ph_lift("7 events were seen", reg5, "fragment:a_lr:glm_note", "a_lr")
  expect_identical(sr_ph_resolve(tk5, reg5, cite = .md_cite), "7 events were seen")
  # a document built with suppress_k = 10 masks a count of 7 in prose
  p <- .fx_writer_project(environment())
  real <- sr_result
  withr::defer(assign("sr_result", real, envir = globalenv()))
  assign("sr_result", function(project, analysis_id) {
    r <- real(project, analysis_id)
    if (identical(analysis_id, "a_lr")) {
      r$fragments$glm_note <- sub("209 events", "7 events", r$fragments$glm_note, fixed = TRUE)
    }
    r
  }, envir = globalenv())
  m10 <- sr_doc_model(p, sr_writer_opts(suppress_k = 10))
  expect_identical(m10$reg$k, 10L)
  expect_match(.md_say(m10, m10$main, "methods_a_lr"), "(<10 events)", fixed = TRUE)
  expect_true(all(sr_ph_verify(m10$reg, p)$ok))
  m5 <- sr_doc_model(p)
  expect_match(.md_say(m5, m5$main, "methods_a_lr"), "(7 events)", fixed = TRUE)
  expect_true(all(sr_ph_verify(m5$reg, p)$ok))
})

