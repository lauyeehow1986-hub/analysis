# Tests for ui/analyses_view.R: the pure builders behind the Analyses panel. Synthetic data only.

# A stand-in project (state only) for the builders that read no store: a1 primary with the sensitivity
# analysis s1, a2 secondary, u1 an unplanned sensitivity analysis of a1.
.an_fake <- function(runs = list(), diag = list(), sap = list()) {
  an <- function(id, label, parent = NA_character_, pre = TRUE) {
    list(id = id, route = "r", label = label, parent = parent, prespecified = pre)
  }
  list(state = list(analyses = list(a1 = an("a1", "primary"), s1 = an("s1", "sensitivity", "a1"),
                                    a2 = an("a2", "secondary"),
                                    u1 = an("u1", "sensitivity", "a1", FALSE)),
                    runs = runs, diag = diag, sap = sap))
}

.an_eff <- function(est = 1.5, role = c("primary", "covariate"), measure = "HR") {
  data.frame(term = c("x", "z"), measure = measure, est = est * c(1, 0.5), lo = 1, hi = 2, p = c(0.01, 0.5),
             scale = "ratio", level = NA_character_, mcse = NA_real_, role = role, conditional = FALSE,
             ci_method = "wald", n = 100, events = 40, stringsAsFactors = FALSE)
}

# Replace sr_result in the global env for the calling test: `f(id)` gives the effects frame.
.an_mock_result <- function(f, env = parent.frame()) {
  old <- get("sr_result", envir = globalenv())
  assign("sr_result", function(project, analysis_id) list(effects = f(analysis_id)), envir = globalenv())
  withr::defer(assign("sr_result", old, envir = globalenv()), envir = env)
}

test_that("status: running with no live job is interrupted; a live job is running", {
  p <- .fx_an_project()
  p <- sr_project_append(p, "run.started", list(analysis = "a1", key = "k", extract_sha = "s", job = "j9"))
  expect_identical(.sr_an_status(p)[["a1"]], "interrupted")
  expect_identical(.sr_an_status(p, c(a1 = "j9"))[["a1"]], "running")
  expect_identical(.sr_an_status(p)[["a2"]], "not run")
  expect_identical(.sr_an_history(p)$outcome, "interrupted")
  expect_identical(.sr_an_history(p)$ended, "interrupted")
  expect_identical(nrow(.sr_an_history(p, c(a1 = "j9"))), 0L)
})

test_that("status: an old state without a status falls back to ok / failed", {
  p <- .an_fake(runs = list(a1 = list(ok = TRUE), a2 = list(ok = FALSE), s1 = list(status = "cancelled")))
  s <- .sr_an_status(p)
  expect_identical(s[["a1"]], "ok")
  expect_identical(s[["a2"]], "failed")
  expect_identical(s[["s1"]], "cancelled")
})

test_that("order: primary, its sensitivity children indented, then secondary", {
  p <- .fx_an_project()
  o <- .sr_an_order(p)
  expect_identical(o$id, c("a1", "s1", "a2"))
  expect_identical(o$depth, c(0L, 1L, 0L))
  expect_identical(.sr_an_runs_table(p)$id[2], "  - s1")
  expect_identical(names(.sr_an_runs_table(p)),
                   c("id", "label", "route", "status", "last_run", "elapsed", "cached", "diagnostics"))
})

test_that("order: pre-specified children before unplanned; strays at depth 0 at the end", {
  p <- .an_fake()
  p$state$analyses$orph <- list(id = "orph", route = "r", label = "sensitivity", parent = "gone",
                                prespecified = FALSE)
  o <- .sr_an_order(p)
  expect_identical(o$id, c("a1", "s1", "u1", "a2", "orph"))
  expect_identical(o$depth, c(0L, 1L, 1L, 0L, 0L))
})

test_that("runs table: last run, elapsed, cached and the diagnostics summary", {
  p <- .an_fake(runs = list(a1 = list(status = "ok", cached = TRUE, elapsed = 1.23456,
                                      history = list(list(job = "j", started = "t1", ended = "t2",
                                                          outcome = "ok", reason = NA)))),
                diag = list(a1 = list(ph = list(verdict = "ok"), linearity = list(verdict = "fail"),
                                      influence = list(verdict = "ok"))))
  t <- .sr_an_runs_table(p)
  r <- t[t$id == "a1", ]
  expect_identical(r$last_run, "t2")
  expect_identical(r$diagnostics, "2 ok, 1 fail")
  expect_identical(r$status, "ok")
  expect_equal(r$elapsed, 1.2)
  expect_identical(t$last_run[t$id == "a2"], "")
  expect_identical(t$diagnostics[t$id == "a2"], "")
})

test_that("queue: Run all skips ok analyses and keeps SAP order", {
  p <- sr_run(.fx_an_project(), "s1")
  expect_identical(.sr_an_queue(p, "all"), c("a1", "a2"))
  expect_identical(.sr_an_queue(p, "selected", "s1"), "s1")
  expect_error(.sr_an_queue(p, "selected", "nope"), "unknown analysis")
  expect_error(.sr_an_queue(p, "selected", NULL), "select")
})

test_that("can_run: each reason in order", {
  p <- .fx_an_project()
  expect_null(.sr_an_can_run(p))
  expect_identical(.sr_an_can_run(NULL), "open a project first")
  expect_identical(.sr_an_can_run(p, busy = TRUE), .sr_ui_busy_text)
  ro <- p
  ro$readonly <- TRUE
  expect_identical(.sr_an_can_run(ro), "the project is read-only (locked by another session)")
  expect_identical(.sr_an_can_run(NULL, busy = TRUE), "open a project first")
  expect_identical(.sr_an_can_run(ro, busy = TRUE), .sr_ui_busy_text)
})

test_that("can_run: an unlocked SAP", {
  expect_identical(.sr_an_can_run(.run_drafted()), "lock the SAP first")
})

test_that("can_run: a stopped project", {
  p <- .fx_an_project()
  # a locked SAP cannot be decided any more (and the lock refuses a stop): stand in for a stop in the state
  p$state$feasibility$a1$decision <- list(decision = "stop", reason = "too few events")
  expect_identical(.sr_an_can_run(p), "the project is stopped: too few events")
})

test_that("effects: primary rows first, flags inline, numbers rounded", {
  e <- data.frame(term = c("age", "x", "w"), measure = "HR", est = c(1.123456, 2.123456, 3.1), lo = 1.00001,
                  hi = 2.99999, p = c(0.123456, 0.000012345, NA), scale = "ratio", level = NA_character_,
                  mcse = NA_real_, role = c("covariate", "primary", "descriptive"), conditional = FALSE,
                  ci_method = "wald", n = 100, events = 40, stringsAsFactors = FALSE)
  pr <- list(effects = e, flags = data.frame(term = c("age", "age", "x"), flag = c("table2", "other", "non_collapsible"),
                                             detail = "d", stringsAsFactors = FALSE))
  out <- .sr_an_effects(pr)
  expect_identical(out$term, c("x", "age", "w"))
  expect_identical(out$flags, c("non_collapsible", "table2; other", ""))
  expect_equal(out$est, c(2.123, 1.123, 3.1))
  expect_equal(out$lo[1], 1)
  expect_equal(out$p[1], 1.23e-05)
  expect_true("n" %in% names(out))
  expect_identical(.sr_an_effects(list(effects = e))$flags, c("", "", ""))
})

test_that("figures: km carries km_risk; unknown names are dropped", {
  f <- .sr_an_figures(list(figures = list(km = data.frame(a = 1), km_risk = data.frame(b = 1),
                                          forest = data.frame(c = 1), junk = 1)))
  expect_identical(names(f), c("km", "forest"))
  expect_identical(f$km$kind, "km")
  expect_identical(names(f$km$data), c("km", "km_risk"))
  expect_identical(f$forest$kind, "forest")
  g <- .sr_an_figures(list(figures = list(km = data.frame(a = 1))))
  expect_identical(names(g$km$data), "km")
  expect_identical(length(.sr_an_figures(list(figures = list(km_risk = data.frame(b = 1))))), 0L)
  expect_identical(length(.sr_an_figures(list(figures = list()))), 0L)
})

test_that("diag rows follow SR_DIAG_IDS order; the plot is read from the fits cache", {
  dir <- withr::local_tempdir()
  pd <- data.frame(term = "x", time = 1:3, resid = c(0.1, 0.2, 0.3))
  saveRDS(list(ok = TRUE, value = list(plot_data = pd)), file.path(dir, "k1.rds"))
  saveRDS(list(ok = TRUE, value = list(plot_data = pd[0, ])), file.path(dir, "k2.rds"))
  p <- .an_fake(diag = list(a1 = list(
    linearity = list(check = "linearity", verdict = "warn", value = 0.1, warn = 0.05, fail = 0.2, detail = "d", key = "k2"),
    ph = list(check = "ph", verdict = "ok", value = 0.5, warn = 0.1, fail = 0.05, detail = NA, key = "k1"))))
  p$paths <- list(fits = dir)
  r <- .sr_an_diag_rows(p, "a1")
  expect_identical(names(r), c("check", "verdict", "value", "warn", "fail", "detail"))
  expect_identical(r$check, c("ph", "linearity"))
  expect_identical(nrow(.sr_an_diag_rows(p, "a2")), 0L)
  pl <- .sr_an_diag_plot(p, "a1", "ph")
  expect_identical(pl$kind, "schoenfeld")
  expect_identical(nrow(pl$data), 3L)
  expect_null(.sr_an_diag_plot(p, "a1", "linearity"))   # empty frame
  expect_null(.sr_an_diag_plot(p, "a1", "influence"))   # no such check
  p$state$diag$a1$ph$key <- "missing"
  expect_null(.sr_an_diag_plot(p, "a1", "ph"))
})

test_that("plan words: rule, add and limitation", {
  p <- .an_fake(sap = list(rules = list(r1 = list(id = "r1", analysis = "a1", check = "ph",
                                                 action = "replace_primary", alternative = "s1"))))
  plan <- data.frame(type = c("rule", "add", "limitation"), check = c("ph", "linearity", "inf_censoring"),
                     target = c("r1", "a1_linearity", NA), route = c(NA, "cox", NA),
                     reason = c("why", "why2", "no sensitivity analysis"), stringsAsFactors = FALSE)
  w <- .sr_an_plan_words(p, plan)
  expect_identical(w, c("fire rule `r1` (replace_primary -> `s1`)", "add sensitivity `a1_linearity` (cox)",
                        "record limitation: no sensitivity analysis"))
  expect_identical(.sr_an_plan_words(p, plan[0, ]), character())
})

test_that("compare: main rows, sensitivity rows, failed and not-run analyses never blank", {
  p <- .an_fake(runs = list(a1 = list(status = "ok", ok_key = "k1"), s1 = list(status = "ok", ok_key = "k2"),
                            a2 = list(status = "failed")))
  .an_mock_result(function(id) .an_eff(if (id == "a1") 1.5 else 1.2))
  cp <- .sr_an_compare(p)
  expect_identical(names(cp), c("block", "analysis", "role", "status", "measure", "term", "est", "lo", "hi"))
  b1 <- cp[cp$block == "a1", ]
  expect_identical(b1$analysis, c("a1", "s1", "u1"))
  expect_identical(b1$role, c("main", "sensitivity", "sensitivity"))
  expect_identical(b1$status, c("ok", "ok", "not run"))
  expect_equal(b1$est[1:2], c(1.5, 1.2))
  expect_true(is.na(b1$est[3]))
  b2 <- cp[cp$block == "a2", ]
  expect_identical(nrow(b2), 1L)
  expect_identical(b2$status, "failed")
  expect_true(is.na(b2$est))
})

test_that("compare: a fired replace_primary marks the new primary and the replaced one", {
  p <- .an_fake(runs = list(a1 = list(status = "ok", ok_key = "k1"), s1 = list(status = "ok", ok_key = "k2")),
                sap = list(fired = list(r1 = list(rule = "r1", analysis = "a1", check = "ph",
                                                  action = "replace_primary", alternative = "s1"))))
  p$state$analyses$s1$label <- "primary"
  p$state$analyses$a1$label <- "sensitivity"
  .an_mock_result(function(id) .an_eff(if (id == "a1") 1.5 else 1.2))
  cp <- .sr_an_compare(p)
  b <- cp[cp$block == "s1", ]
  expect_identical(b$analysis[1], "s1")
  expect_identical(b$status[1], "now primary")
  expect_true("a1" %in% b$analysis)
  expect_identical(b$status[b$analysis == "a1"], "replaced")
  expect_false("a1" %in% cp$block)
})

test_that("families: a family with an unrun member says what it waits for", {
  fam <- list(f1 = list(id = "f1", members = c("a1", "a2"), method = "holm"))
  p <- .an_fake(runs = list(a1 = list(status = "ok", ok_key = "k1")), sap = list(families = fam))
  f <- .sr_an_families(p)
  expect_identical(length(f), 1L)
  expect_identical(f[[1]]$id, "f1")
  expect_identical(f[[1]]$method, "holm")
  expect_null(f[[1]]$table)
  expect_identical(f[[1]]$waiting, "a2")
  .an_mock_result(function(id) .an_eff(1.5))
  p$state$runs$a2 <- list(status = "ok", ok_key = "k2")
  g <- .sr_an_families(p)[[1]]
  expect_identical(g$waiting, character())
  expect_identical(names(g$table), c("analysis", "term", "p", "p_adj", "method"))
  expect_equal(g$table$p_adj, c(0.02, 0.02))
  expect_identical(.sr_an_families(.an_fake()), list())
})

test_that("history: cancelled and interrupted attempts only", {
  h <- list(list(job = "j1", started = "t1", ended = "t2", outcome = "ok", reason = NA_character_),
            list(job = "j2", started = "t3", ended = "t4", outcome = "cancelled", reason = "cancelled by the user"),
            list(job = "j3", started = "t5", ended = NA_character_, outcome = "interrupted", reason = NA_character_))
  p <- .an_fake(runs = list(a1 = list(status = "running", history = c(h, list(
    list(job = "j4", started = "t6", ended = NA_character_, outcome = "running", reason = NA_character_))))))
  x <- .sr_an_history(p)
  expect_identical(names(x), c("analysis", "job", "started", "ended", "outcome", "reason"))
  expect_identical(x$job, c("j2", "j3", "j4"))
  expect_identical(x$outcome, c("cancelled", "interrupted", "interrupted"))
  expect_identical(x$ended, c("t4", "interrupted", "interrupted"))
  expect_identical(.sr_an_history(p, c(a1 = "j4"))$job, c("j2", "j3"))
})

test_that("compare: the replaced analysis keeps its other children; a stray is never dropped", {
  p <- .an_fake(runs = list(a1 = list(status = "ok", ok_key = "k1"), s1 = list(status = "ok", ok_key = "k2")),
                sap = list(fired = list(r1 = list(rule = "r1", analysis = "a1", check = "ph",
                                                  action = "replace_primary", alternative = "s1"))))
  p$state$analyses$s1$label <- "primary"
  p$state$analyses$a1$label <- "sensitivity"
  p$state$analyses$orph <- list(id = "orph", route = "r", label = "sensitivity", parent = "gone",
                                prespecified = FALSE)
  .an_mock_result(function(id) .an_eff(1.5))
  cp <- .sr_an_compare(p)
  b <- cp[cp$block == "s1", ]
  expect_true(all(c("s1", "a1", "u1") %in% b$analysis))
  expect_identical(b$status[b$analysis == "u1"], "not run")
  o <- cp[cp$block == "orph", ]
  expect_identical(o$analysis, "orph")
  expect_identical(o$status, "not run")
  expect_identical(sum(cp$analysis == "orph"), 1L)
})

test_that("an empty project gives typed, zero-row results", {
  p <- list(state = list(analyses = list()))
  t <- .sr_an_runs_table(p)
  expect_identical(nrow(t), 0L)
  expect_type(t$id, "character")
  expect_type(t$elapsed, "double")
  expect_identical(nrow(.sr_an_compare(p)), 0L)
  expect_identical(.sr_an_queue(p, "all"), character())
  s <- .sr_an_status(p)
  expect_type(s, "character")
  expect_length(s, 0L)
  expect_null(.sr_an_diag_plot(p, "a1", character()))
  expect_null(.sr_an_diag_plot(p, "a1", NA_character_))
})

test_that("compare: a sensitivity of another measure shows its own measure, not the parent's with NA", {
  p <- .an_fake(runs = list(a1 = list(status = "ok", ok_key = "k1"), s1 = list(status = "ok", ok_key = "k2")))
  .an_mock_result(function(id) {
    if (id == "a1") .an_eff(0.7, measure = "HR") else .an_eff(2.5, measure = "RMST_diff")
  })
  cp <- .sr_an_compare(p)
  s <- cp[cp$analysis == "s1", ]
  expect_identical(s$status, "ok")
  expect_identical(s$measure, "RMST_diff")
  expect_equal(s$est, 2.5)
  expect_false(anyNA(s$est))
  # the same measure still lines up with the parent's
  .an_mock_result(function(id) .an_eff(if (id == "a1") 1.5 else 1.2))
  cp2 <- .sr_an_compare(p)
  expect_identical(cp2$measure[cp2$analysis == "s1"], "HR")
})

test_that("runs table: a status that is not ok / failed shows no stale elapsed or cached", {
  old <- list(elapsed = 3.2, cached = TRUE, status = "cancelled")
  p <- .an_fake(runs = list(a1 = old, s1 = utils::modifyList(old, list(status = "running")),
                            a2 = utils::modifyList(old, list(status = "ok")),
                            u1 = utils::modifyList(old, list(status = "failed"))))
  t <- .sr_an_runs_table(p)
  expect_true(is.na(t$elapsed[t$id == "a1"]))
  expect_identical(t$cached[t$id == "a1"], "")
  expect_true(is.na(t$elapsed[t$id == "  - s1"]))
  expect_identical(t$cached[t$id == "  - s1"], "")
  expect_equal(t$elapsed[t$id == "a2"], 3.2)
  expect_identical(t$cached[t$id == "a2"], "yes")
  expect_equal(t$elapsed[t$id == "  - u1"], 3.2)
})

test_that("can_run: busy is reported before read-only", {
  p <- .fx_an_project()
  ro <- p
  ro$readonly <- TRUE
  expect_identical(.sr_an_can_run(ro, busy = TRUE), .sr_ui_busy_text)
  expect_identical(.sr_an_can_run(ro), "the project is read-only (locked by another session)")
})

test_that("families: a sr_family_adjust error is kept and shown", {
  fam <- list(f1 = list(id = "f1", members = c("a1", "a2"), method = "holm"))
  p <- .an_fake(runs = list(a1 = list(status = "ok", ok_key = "k1"), a2 = list(status = "ok", ok_key = "k2")),
                sap = list(families = fam))
  old <- get("sr_family_adjust", envir = globalenv())
  assign("sr_family_adjust", function(project, id) stop("members share no term", call. = FALSE), envir = globalenv())
  withr::defer(assign("sr_family_adjust", old, envir = globalenv()))
  f <- .sr_an_families(p)[[1]]
  expect_null(f$table)
  expect_identical(f$error, "members share no term")
})

test_that("a running entry is live only when the live job matches the entry's job", {
  p <- .an_fake(runs = list(a1 = list(status = "running", job = "j1",
                                      history = list(list(job = "j1", started = "t", ended = NA_character_,
                                                          outcome = "running", reason = NA_character_)))))
  expect_identical(.sr_an_status(p, c(a1 = "j1"))[["a1"]], "running")
  expect_identical(.sr_an_status(p, c(a1 = "other"))[["a1"]], "interrupted")
  expect_identical(nrow(.sr_an_history(p, c(a1 = "j1"))), 0L)
  expect_identical(.sr_an_history(p, c(a1 = "other"))$outcome, "interrupted")
})
