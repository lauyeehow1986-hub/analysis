# Tests for writer/tables.R and writer/figures.R: the main-results table, the route tables with
# the Table 2 handling, the family and deviation tables, and the figure specs. Synthetic data only
# (the writer fixture of helper-writer.R).

.tb_opts <- function(table2 = "label") list(table2 = table2, suppress_k = 5L)
.tb_preset <- function() list(fig_width_mm = 120, fig_height_mm = 90, dpi = 100)
.tb_ids <- c("t1", "a_lr", "a_cox", "a_rmst")
.tb_cite <- function(key) paste0("[", key, "]")

# The effects rows of the primary role of analyses `ids`, in order.
.tb_primary <- function(p, ids) {
  do.call(rbind, lapply(ids, function(id) {
    e <- sr_result(p, id)$effects
    e <- e[e$role == "primary", , drop = FALSE]
    if (nrow(e)) cbind(analysis = rep(id, nrow(e)), e, stringsAsFactors = FALSE) else NULL
  }))
}

test_that("the main table has one row per primary effects row, each number a token", {
  p <- .fx_writer_project(environment())
  reg <- sr_ph_registry()
  tb <- .sr_wr_main_table(p, .tb_ids, reg, .tb_opts())
  expect_identical(tb$type, "table")
  expect_identical(tb$where, "main")
  expect_identical(tb$n_suppressed, 0L)
  expect_false(is.null(tb$id))
  df <- tb$df
  prim <- .tb_primary(p, .tb_ids)
  expect_identical(nrow(df), nrow(prim))
  expect_identical(nrow(df), 5L)
  expect_identical(df$Analysis, prim$analysis)
  expect_identical(df$Comparison, prim$term)
  expect_identical(df$Measure[df$Analysis == "a_cox"], "Hazard ratio")
  expect_identical(df$Measure[df$Analysis == "a_rmst"], "Difference in restricted mean survival time")
  expect_true(all(c("Analysis", "Comparison", "Measure", "p") %in% names(df)))
  est_col <- grep("^Estimate", names(df), value = TRUE)
  expect_length(est_col, 1L)
  expect_match(est_col, "^Estimate \\(\\{\\{L[0-9]+\\}\\} CI\\)$")
  expect_identical(sr_ph_resolve(est_col, reg), "Estimate (95% CI)")
  expect_true(all(grepl("^\\{\\{E[0-9]+\\}\\} \\(\\{\\{E[0-9]+\\}\\}\\)$", df[[est_col]])))
  # a_lr's first row resolves to what sr_fmt_ci prints for the effects row
  i <- which(df$Analysis == "a_lr")[1L]
  e <- sr_result(p, "a_lr")$effects
  expect_identical(sr_ph_resolve(df[[est_col]][i], reg),
                   sr_fmt_ci(e$est[1L], e$lo[1L], e$hi[1L], e$mcse[1L]))
  expect_identical(sr_ph_resolve(df$p[i], reg), sr_fmt_p(e$p[1L]))
  expect_identical(df$Measure[i], "Odds ratio")
  # the adjusted p column: a_lr and a_cox are in family fam1, a_rmst is not
  expect_true("Adjusted p" %in% names(df))
  fa <- sr_family_adjust(p, "fam1")
  adj <- df[["Adjusted p"]]
  expect_identical(sr_ph_resolve(adj[i], reg), sr_fmt_p(fa$p_adj[fa$analysis == "a_lr"]))
  j <- which(df$Analysis == "a_cox")
  expect_identical(sr_ph_resolve(adj[j], reg), sr_fmt_p(fa$p_adj[fa$analysis == "a_cox"]))
  expect_identical(adj[df$Analysis == "a_rmst"], "-")
  # the marginal rows of a_lr are not adjusted
  expect_true(all(adj[df$Analysis == "a_lr"][-1L] == "-"))
  expect_true(all(sr_ph_verify(reg, p)$ok))
})

test_that("main table footnotes and templates carry no literal digit", {
  p <- .fx_writer_project(environment())
  reg <- sr_ph_registry()
  tb <- .sr_wr_main_table(p, .tb_ids, reg, .tb_opts())
  fn <- sr_ph_resolve(tb$footnotes, reg)
  expect_true("Odds and hazard ratios are conditional on the covariates of their model and are not collapsible; marginal estimates are shown where reported." %in% fn)
  expect_true("Adjusted p: holm over family fam1." %in% fn)
  expect_false(any(grepl("Bootstrap interval", fn)))
  expect_identical(sr_ph_digits(c(tb$caption, tb$footnotes, names(tb$df))), character(0))
  expect_identical(sr_ph_digits(unlist(tb$df, use.names = FALSE)), character(0))
  # a table without a conditional ratio or a family has neither footnote
  tb2 <- .sr_wr_main_table(p, "a_rmst", sr_ph_registry(), .tb_opts())
  expect_identical(tb2$footnotes, character(0))
  expect_false("Adjusted p" %in% names(tb2$df))
})

test_that("a bootstrap row carries the interval footnote and an NA p prints as a dash", {
  skip_on_cran()
  env <- environment()
  p <- .fx_writer_build(env, boot = TRUE, drop = c("t1", "a_cox", "a_rmst", "x_age"))
  on.exit({
    sr_duck_close(p)
    sr_project_close(p)
  }, add = TRUE)
  reg <- sr_ph_registry()
  tb <- .sr_wr_main_table(p, c("a_lr", "a_mp"), reg, .tb_opts())
  e <- sr_result(p, "a_mp")$effects
  prim <- e[e$role == "primary", , drop = FALSE]
  expect_true(all(startsWith(prim$ci_method, "boot_")))
  fn <- sr_ph_resolve(tb$footnotes, reg)
  expect_true(any(grepl("^Bootstrap interval \\(boot_[a-z_]+\\)\\.$", fn)))
  na_p <- is.na(prim$p)
  pcell <- tb$df$p[tb$df$Analysis == "a_mp"]
  expect_true(all(sr_ph_verify(reg, p)$ok))
  expect_length(pcell, nrow(prim))
  expect_identical(pcell == "-", na_p)
  expect_identical(grepl("^[{][{]E[0-9]+[}][}]$", pcell), !na_p)
  # a constructed NA p prints as a dash, whatever the bootstrap yields
  old <- sr_result
  assign("sr_result", function(project, analysis_id) {
    res <- old(project, analysis_id)
    res$effects$p[res$effects$role == "primary"] <- NA_real_
    res
  }, envir = globalenv())
  withr::defer(assign("sr_result", old, envir = globalenv()))
  tb_na <- .sr_wr_main_table(p, "a_mp", sr_ph_registry(), .tb_opts())
  expect_gt(nrow(tb_na$df), 0L)
  expect_true(all(tb_na$df$p == "-"))
})

test_that("a table1 route goes to the main document with small cells hidden; the rest is supplement", {
  p <- .fx_writer_project(environment())
  reg <- sr_ph_registry()
  blocks <- .sr_wr_route_tables(p, "t1", reg, .tb_opts())
  expect_true(all(vapply(blocks, function(b) identical(b$type, "table"), NA)))
  where <- vapply(blocks, function(b) b$where, "")
  ids <- vapply(blocks, function(b) b$id, "")
  expect_length(blocks, 2L)
  main <- blocks[[which(where == "main")]]
  expect_identical(sum(where == "main"), 1L)
  expect_identical(main$analysis, "t1")
  expect_gte(main$n_suppressed, 1L)
  expect_true(any(unlist(main$df, use.names = FALSE) == "<5"))
  # stage III is 3 rows: its Overall cell is hidden, and the Missing column is left alone
  expect_true(all(main$df[["Missing, n"]] %in% c("0", "")))
  expect_match(main$caption, "^Table \\{\\{L[0-9]+\\}\\} \\{\\{L[0-9]+\\}\\}: table1$")
  expect_identical(sr_ph_resolve(main$caption, reg), "Table 1 t1: table1")
  expect_identical(sr_ph_digits(c(main$caption, main$footnotes)), character(0))
  sup <- blocks[[which(where == "supplement")]]
  expect_identical(sup$analysis, "t1")
  expect_identical(sr_ph_resolve(sup$caption, reg), "Table 1 t1: missing")
  expect_identical(sup$n_suppressed, 0L)
  expect_length(unique(ids), 2L)
})

test_that("the sdc wrapper leaves the Overall, Missing and SMD columns out of the row rule", {
  df <- data.frame(Characteristic = c("stage I", "stage II"),
                   `Overall (n=40)` = c("30 (75.0%)", "10 (25.0%)"),
                   `A (n=20)` = c("18 (90.0%)", "2 (10.0%)"),
                   `B (n=3)` = c("12 (60.0%)", "8 (40.0%)"),
                   `Missing, n` = c("2", "0"),
                   SMD = c("0.31", "0.12"), check.names = FALSE, stringsAsFactors = FALSE)
  expect_identical(.sr_wr_skip_cols(names(df)), c("Overall (n=40)", "Missing, n", "SMD"))
  out <- .sr_wr_sdc(df, 5L)
  tb <- out$table
  # the 2 of the Missing column is a small cell of its own; it does not drag a group cell of its row
  expect_identical(tb[["Missing, n"]], c("<5", "0"))
  expect_identical(tb[["A (n=20)"]], c("18 (90.0%)", "<5"))
  # the row rule hides the smallest other group cell of the second row, never the Overall cell
  expect_identical(tb[["B (n=<5)"]], c("12 (60.0%)", "<5"))
  expect_identical(tb[["Overall (n=40)"]], df[["Overall (n=40)"]])
  expect_identical(tb$SMD, df$SMD)
  expect_identical(out$n_suppressed, 3L)
  # a group size of a header is a small cell too
  expect_identical(names(tb), c("Characteristic", "Overall (n=40)", "A (n=20)", "B (n=<5)", "Missing, n", "SMD"))
  # without skip_cols the row rule would also have hidden a group cell of the first row
  naive <- sr_sdc_table(df, 5L)
  expect_gt(naive$n_suppressed, out$n_suppressed)
})

test_that("route tables: the Table 2 footnote on the coefficients, or the covariate rows dropped", {
  p <- .fx_writer_project(environment())
  reg <- sr_ph_registry()
  blocks <- .sr_wr_route_tables(p, "a_lr", reg, .tb_opts("label"))
  ids <- vapply(blocks, function(b) b$id, "")
  expect_true(all(vapply(blocks, function(b) identical(b$where, "supplement"), NA)))
  expect_length(blocks, 4L)
  co <- blocks[[grep("coefficients", ids)]]
  expect_identical(co$analysis, "a_lr")
  expect_identical(sr_ph_resolve(co$caption, reg), "Logistic regression a_lr: coefficients")
  expect_true("age" %in% co$df$Term)
  fn <- sr_ph_resolve(co$footnotes, reg, cite = .tb_cite)
  expect_true("Covariate coefficients are adjusted for different sets of variables and are not estimates of their effects (Table 2 fallacy) [westreich2013]." %in% fn)
  expect_match(co$footnotes, "\\{\\{C:westreich2013\\}\\}")
  expect_identical(sr_ph_digits(c(co$caption, co$footnotes)), character(0))
  # the other tables of the route have no covariate rows and no footnote
  other <- blocks[-grep("coefficients", ids)]
  expect_true(all(vapply(other, function(b) length(b$footnotes) == 0L, NA)))

  reg_s <- sr_ph_registry()
  sb <- .sr_wr_route_tables(p, "a_lr", reg_s, .tb_opts("suppress"))
  sco <- sb[[grep("coefficients", vapply(sb, function(b) b$id, ""))]]
  expect_false(any(c("age", "z1") %in% sco$df$Term))
  expect_true("grp: B vs A" %in% sco$df$Term)
  sfn <- sr_ph_resolve(sco$footnotes, reg_s, cite = .tb_cite)
  expect_identical(sfn, "Covariate coefficients are not shown (Table 2 fallacy) [westreich2013].")
})

test_that("route tables of the survival routes and the flags of the rmst route", {
  p <- .fx_writer_project(environment())
  reg <- sr_ph_registry()
  cx <- .sr_wr_route_tables(p, "a_cox", reg, .tb_opts("suppress"))
  co <- cx[[grep("coefficients", vapply(cx, function(b) b$id, ""))]]
  expect_identical(co$df$Term, "grp: B vs A")
  expect_identical(sr_ph_resolve(co$caption, reg), "Cox regression a_cox: coefficients")
  rm <- .sr_wr_route_tables(p, "a_rmst", reg, .tb_opts())
  expect_length(rm, 2L)
  expect_true(all(vapply(rm, function(b) length(b$footnotes) == 0L, NA)))
  nm <- vapply(rm, function(b) sr_ph_resolve(b$caption, reg), "")
  expect_true("Restricted mean survival time a_rmst: rmst difference" %in% nm)
  expect_error(.sr_wr_route_tables(p, "a_cox", reg, list(table2 = "other")), "table2")
})

test_that("the family table and the deviation table", {
  p <- .fx_writer_project(environment())
  reg <- sr_ph_registry()
  ft <- .sr_wr_family_table(p, reg)
  expect_identical(ft$type, "table")
  expect_identical(ft$where, "supplement")
  fa <- sr_family_adjust(p, "fam1")
  expect_identical(nrow(ft$df), nrow(fa))
  expect_identical(sr_ph_resolve(ft$df[["Adjusted p"]], reg), sr_fmt_p(fa$p_adj))
  expect_identical(sr_ph_resolve(ft$df$p, reg), sr_fmt_p(fa$p))
  expect_identical(ft$df$Analysis, fa$analysis)
  expect_identical(sr_ph_digits(c(ft$caption, ft$footnotes, names(ft$df))), character(0))

  dv <- .sr_wr_deviation_table(p, reg)
  expect_identical(dv$where, "supplement")
  expect_identical(nrow(dv$df), 1L)
  expect_identical(dv$df$Analysis, "x_age")
  expect_identical(dv$df$Reason, "test fixture")

  # digits of a reason are lifted into tokens that re-derive from the project
  p2 <- p
  p2$state$analyses$x_age$reason <- "added after 3 reviewers asked, 12 weeks late"
  reg2 <- sr_ph_registry()
  dv2 <- .sr_wr_deviation_table(p2, reg2)
  expect_match(dv2$df$Reason, "^added after \\{\\{E[0-9]+\\}\\} reviewers asked, \\{\\{E[0-9]+\\}\\} weeks late$")
  expect_identical(sr_ph_resolve(dv2$df$Reason, reg2), "added after 3 reviewers asked, 12 weeks late")
  expect_identical(sr_ph_digits(dv2$df$Reason), character(0))
  expect_true(all(sr_ph_verify(reg2, p2)$ok))
  expect_true(all(sr_ph_verify(reg, p)$ok))
})

test_that("a project without family or deviation has no such tables", {
  skip_on_cran()
  env <- environment()
  p <- .fx_writer_build(env, drop = c("a_lr", "a_cox", "x_age"))
  on.exit({
    sr_duck_close(p)
    sr_project_close(p)
  }, add = TRUE)
  reg <- sr_ph_registry()
  expect_null(.sr_wr_family_table(p, reg))
  expect_null(.sr_wr_deviation_table(p, reg))
})

test_that("the resolvers for family and deviation sources reject a bad source", {
  p <- .fx_writer_project(environment())
  expect_identical(.sr_ph_derive(p, "family:fam1:1:p_adj", "p"),
                   sr_fmt_p(sr_family_adjust(p, "fam1")$p_adj[1L]))
  expect_error(.sr_ph_derive(p, "family:fam1:9:p_adj", "p"), "row")
  expect_error(.sr_ph_derive(p, "family:fam1:1:zzz", "p"), "field")
  expect_error(.sr_ph_derive(p, "family:fam1:1", "p"), "family")
  expect_error(.sr_ph_derive(p, "deviation:1:1", "num"), "number")
  expect_error(.sr_ph_derive(p, "deviation:5:1", "num"), "deviation")
})

test_that("figure blocks: km per survival analysis, forest per scale, flow, small cells hidden", {
  p <- .fx_writer_project(environment())
  reg <- sr_ph_registry()
  fb <- .sr_wr_figures(p, .tb_ids, reg, .tb_opts())
  kinds <- vapply(fb, function(b) b$kind, "")
  ids <- vapply(fb, function(b) b$id, "")
  expect_true(all(vapply(fb, function(b) identical(b$type, "figure"), NA)))
  expect_identical(sum(kinds == "km"), 2L)
  expect_setequal(ids[kinds == "km"], c("km_a_cox", "km_a_rmst"))
  expect_identical(sort(ids[kinds == "forest"]), c("forest_difference", "forest_ratio"))
  expect_identical(sum(kinds == "flow"), 1L)
  expect_true(all(vapply(fb, function(b) identical(b$where, "main"), NA)))

  km <- fb[[which(ids == "km_a_cox")]]
  expect_identical(sr_ph_resolve(km$caption, reg), "Kaplan-Meier curves by grp (a_cox)")
  expect_true(all(c("km", "km_risk") %in% names(km$data)))
  rk <- km$data$km_risk
  expect_true("label" %in% names(rk))
  small <- rk$n.risk >= 1 & rk$n.risk <= 4
  expect_true(any(small))
  expect_true(all(rk$label[small] == "<5"))
  expect_true(all(rk$label[!small & rk$n.risk >= 5] == sprintf("%d", as.integer(rk$n.risk[!small & rk$n.risk >= 5]))))
  expect_identical(rk$label[rk$n.risk == 0], rep("0", sum(rk$n.risk == 0)))

  fr <- fb[[which(ids == "forest_ratio")]]
  expect_identical(sr_ph_resolve(fr$caption, reg), "Primary and secondary estimates (ratio scale)")
  expect_true(all(fr$data$scale == "ratio"))
  expect_true(all(grepl("^a_(lr|cox): ", fr$data$term)))
  expect_true(all(is.finite(fr$data$lo) & is.finite(fr$data$hi)))
  fd <- fb[[which(ids == "forest_difference")]]
  expect_identical(sr_ph_resolve(fd$caption, reg), "Primary and secondary estimates (difference scale)")
  expect_true(all(fd$data$scale == "difference"))
  prim <- .tb_primary(p, .tb_ids)
  expect_identical(nrow(fr$data) + nrow(fd$data), nrow(prim))

  fl <- fb[[which(kinds == "flow")]]
  expect_identical(fl$caption, "Flow of rows from import to analysis")
  expect_gte(nrow(fl$data), 1L)
  expect_identical(sr_ph_digits(vapply(fb, function(b) b$caption, "")), character(0))
  expect_true(all(sr_ph_verify(reg, p)$ok))
})

test_that("sr_wr_figure_render writes a PNG per block and returns the file's sha", {
  p <- .fx_writer_project(environment())
  reg <- sr_ph_registry()
  fb <- .sr_wr_figures(p, .tb_ids, reg, .tb_opts())
  dir <- withr::local_tempdir()
  for (b in fb) {
    r <- sr_wr_figure_render(b, dir, .tb_preset())
    expect_identical(basename(r$path), paste0(b$id, ".png"))
    expect_true(file.exists(r$path))
    expect_identical(r$sha, .sr_sha256(.sr_read_bytes(r$path)))
    expect_identical(.sr_read_bytes(r$path)[1:4], as.raw(c(0x89, 0x50, 0x4E, 0x47)))
  }
  expect_identical(length(list.files(dir, pattern = "[.]png$")), length(fb))
  # the same block rendered twice gives the same bytes
  km <- fb[[which(vapply(fb, function(b) b$id, "") == "km_a_cox")]]
  expect_identical(sr_wr_figure_render(km, dir, .tb_preset())$sha, sr_wr_figure_render(km, dir, .tb_preset())$sha)
  expect_error(sr_wr_figure_render(km, dir, list(fig_width_mm = 100)), "preset")
})

test_that("a flow without a step gives no flow figure", {
  p <- .fx_writer_project(environment())
  old <- sr_flow
  assign("sr_flow", function(project, table) .sr_fl_empty(), envir = globalenv())
  withr::defer(assign("sr_flow", old, envir = globalenv()))
  fb <- .sr_wr_figures(p, .tb_ids, sr_ph_registry(), .tb_opts())
  expect_false(any(vapply(fb, function(b) identical(b$kind, "flow"), NA)))
  expect_true(any(vapply(fb, function(b) identical(b$kind, "km"), NA)))
})

test_that("every token of every table and figure caption verifies", {
  p <- .fx_writer_project(environment())
  reg <- sr_ph_registry()
  opts <- .tb_opts()
  .sr_wr_main_table(p, .tb_ids, reg, opts)
  for (id in .tb_ids) .sr_wr_route_tables(p, id, reg, opts)
  .sr_wr_family_table(p, reg)
  .sr_wr_deviation_table(p, reg)
  .sr_wr_figures(p, .tb_ids, reg, opts)
  v <- sr_ph_verify(reg, p)
  expect_gt(nrow(v), 20L)
  expect_true(all(v$ok), info = paste(v$source[!v$ok], collapse = "; "))
})

test_that("a numeric-level term in a main or family cell is tokenised and resolves back to the term", {
  skip_on_cran()
  p <- .fx_writer_project(environment())
  old <- sr_result
  assign("sr_result", function(project, analysis_id) {
    res <- old(project, analysis_id)
    ok <- !is.null(res$effects) && nrow(res$effects) > 0L
    if (ok) res$effects$term[res$effects$role == "primary"] <- "stage 2 vs 1"
    res
  }, envir = globalenv())
  withr::defer(assign("sr_result", old, envir = globalenv()))

  reg <- sr_ph_registry()
  tb <- .sr_wr_main_table(p, c("a_lr", "a_cox"), reg, .tb_opts())
  expect_gt(nrow(tb$df), 0L)
  expect_identical(sr_ph_digits(unlist(tb$df[c("Analysis", "Comparison", "Measure")], use.names = FALSE)),
                   character(0))
  expect_match(tb$df$Comparison[1L], "^stage [{][{]L[0-9]+[}][}] vs [{][{]L[0-9]+[}][}]$")
  expect_true(all(sr_ph_resolve(tb$df$Comparison, reg) == "stage 2 vs 1"))

  ft <- .sr_wr_family_table(p, reg)
  expect_identical(sr_ph_digits(unlist(ft$df[c("Family", "Analysis", "Comparison", "Method")], use.names = FALSE)),
                   character(0))
  expect_true(all(sr_ph_resolve(ft$df$Comparison, reg) == "stage 2 vs 1"))
  expect_identical(sr_ph_resolve(ft$df$Analysis, reg), sr_family_adjust(p, "fam1")$analysis)
  expect_true(all(sr_ph_verify(reg, p)$ok))
})

test_that("the flow block holds no raw small count: 1 to k - 1 is <k, zero stays 0", {
  skip_on_cran()
  p <- .fx_writer_project(environment())
  fl <- data.frame(step = c("s1", "s2"), label = c("After age filter", "After sentinel filter"),
                   n_before = c(120, 117), n_after = c(117, 117), n_excluded = c(3, 0),
                   reason = c("age out of range", "none"), stringsAsFactors = FALSE)
  attr(fl, "n_total") <- 120
  attr(fl, "label") <- "All records"
  old <- sr_flow
  assign("sr_flow", function(project, table) fl, envir = globalenv())
  withr::defer(assign("sr_flow", old, envir = globalenv()))

  reg <- sr_ph_registry()
  fb <- .sr_wr_figures(p, .tb_ids, reg, .tb_opts())
  b <- fb[[which(vapply(fb, function(x) identical(x$kind, "flow"), NA))]]
  d <- b$data
  expect_identical(d$n_excluded, c("<5", "0"))
  expect_identical(d$n_before, c("120", "117"))
  expect_identical(d$n_after, c("117", "117"))
  expect_identical(attr(d, "n_total"), "120")
  expect_identical(attr(d, "label"), "All records")
  expect_false(any(vapply(d[c("n_before", "n_after", "n_excluded")], is.numeric, NA)))
  # suppress_k reaches the mask: k = 2 shows an exclusion of 3
  b2 <- .sr_wr_figures(p, .tb_ids, sr_ph_registry(), list(table2 = "label", suppress_k = 2L))
  d2 <- b2[[which(vapply(b2, function(x) identical(x$kind, "flow"), NA))]]$data
  expect_identical(d2$n_excluded, c("3", "0"))
  # the masked block renders
  r <- sr_wr_figure_render(b, withr::local_tempdir(), .tb_preset())
  expect_true(file.exists(r$path))
  expect_match(r$sha, "^[0-9a-f]{64}$")
})
