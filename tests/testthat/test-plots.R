# Tests for diagnostics/plots.R: sr_plot() builds a ggplot from the data frames the routes and the
# diagnostics emit; sr_figure_save() writes a deterministic PNG. Synthetic data only; every file is
# written under withr::local_tempdir().

# ---- fixtures: the shapes the producers emit ------------------------------------------------------

.pl_km <- function(by = TRUE, n = 120L) {
  set.seed(11L)
  t <- stats::rexp(n, 0.1)
  c <- stats::rexp(n, 0.05)
  g <- factor(rep(c("control", "treated"), length.out = n))
  .sr_h_km(pmin(t, c), as.integer(t <= c), if (by) g else NULL)
}

.pl_effects_ratio <- function() {
  data.frame(term = c("treated", "age", "sex"), measure = "OR", est = c(0.62, 1.04, 1.31),
             lo = c(0.4, 1.01, 0.8), hi = c(0.96, 1.07, 2.1), scale = "ratio",
             role = c("primary", "covariate", "covariate"), stringsAsFactors = FALSE)
}

.pl_effects_diff <- function() {
  data.frame(term = c("treated", "age"), measure = "MD", est = c(-1.2, 0.3), lo = c(-2.1, 0.1),
             hi = c(-0.3, 0.5), scale = "difference", role = c("primary", "covariate"),
             stringsAsFactors = FALSE)
}

.pl_resid <- function(n = 60L) {
  set.seed(5L)
  x <- stats::rnorm(n)
  y <- 1 + 2 * x + stats::rnorm(n)
  fit <- stats::lm(y ~ x)
  data.frame(fitted = unname(stats::fitted(fit)), resid = unname(stats::residuals(fit)),
             std_resid = unname(stats::rstandard(fit)), leverage = unname(stats::hatvalues(fit)),
             cooks = unname(stats::cooks.distance(fit)))
}

.pl_schoenfeld <- function() {
  set.seed(3L)
  data.frame(term = rep(c("treated", "age"), each = 25L), time = rep(sort(stats::runif(25L, 1, 20)), 2L),
             resid = stats::rnorm(50L), stringsAsFactors = FALSE)
}

.pl_smd <- function() {
  data.frame(term = c("age", "sex", "bmi", "smoker"), measure = "SMD", est = c(0.05, -0.31, 0.12, NA),
             lo = c(-0.1, -0.5, -0.05, NA), hi = c(0.2, -0.1, 0.29, NA), scale = "difference",
             stringsAsFactors = FALSE)
}

.pl_linearity <- function() {
  x <- seq(20, 80, length.out = 30L)
  data.frame(var = rep(c("age", "bmi"), each = 30L), x = c(x, x / 3),
             linear = c(0.02 * (x - 50), 0.03 * (x / 3 - 17)),
             rcs = c(0.001 * (x - 50)^2 / 10 + 0.01 * (x - 50), 0.02 * (x / 3 - 17)))
}

.pl_png_dim <- function(path) {
  b <- readBin(path, "raw", n = 24L)
  list(sig = b[1:4], w = sum(as.integer(b[17:20]) * 256^(3:0)), h = sum(as.integer(b[21:24]) * 256^(3:0)))
}

# The 4-letter types of the chunks of a PNG, in order.
.pl_png_chunks <- function(path) {
  b <- readBin(path, "raw", n = file.size(path))
  pos <- 9L
  out <- character()
  while (pos < length(b)) {
    len <- sum(as.integer(b[pos:(pos + 3L)]) * 256^(3:0))
    out <- c(out, rawToChar(b[(pos + 4L):(pos + 7L)]))
    pos <- pos + 12L + len
  }
  out
}

.pl_built <- function(p) ggplot2::ggplot_build(p)

.pl_png_sig <- as.raw(c(0x89, 0x50, 0x4E, 0x47))

# ---- sr_plot --------------------------------------------------------------------------------------

test_that("km: a list of km and km_risk draws the curves and the number-at-risk strip", {
  k <- .pl_km()
  p <- sr_plot(list(km = k$km, km_risk = k$risk), "km")
  expect_s3_class(p, "ggplot")
  b <- .pl_built(p)
  expect_equal(nrow(b$layout$layout), 2L)
  texts <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  expect_length(texts, 1L)
  expect_setequal(texts[[1]]$data$label, as.character(k$risk$n.risk))
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomStep"), TRUE)))
})

test_that("km: a bare km frame gets the strip from its own n.risk; one stratum works", {
  k <- .pl_km()
  p <- sr_plot(k$km, "km")
  expect_s3_class(p, "ggplot")
  .pl_built(p)
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), TRUE)))
  k1 <- .pl_km(by = FALSE)
  p1 <- sr_plot(list(km = k1$km, km_risk = k1$risk), "km")
  expect_s3_class(p1, "ggplot")
  .pl_built(p1)
})

test_that("forest: ratio rows get a log10 x axis and a reference line at 1", {
  p <- sr_plot(.pl_effects_ratio(), "forest")
  expect_s3_class(p, "ggplot")
  b <- .pl_built(p)
  expect_identical(b$layout$panel_scales_x[[1]]$trans$name, "log-10")
  ref <- Filter(function(l) inherits(l$geom, "GeomVline"), p$layers)
  expect_length(ref, 1L)
  expect_equal(ref[[1]]$data$xintercept, 1)
})

test_that("forest: difference rows get a linear axis and a reference line at 0", {
  p <- sr_plot(.pl_effects_diff(), "forest")
  b <- .pl_built(p)
  expect_identical(b$layout$panel_scales_x[[1]]$trans$name, "identity")
  ref <- Filter(function(l) inherits(l$geom, "GeomVline"), p$layers)
  expect_equal(ref[[1]]$data$xintercept, 0)
})

test_that("forest: mixed scales, a non-positive ratio and no usable row are refused", {
  mixed <- rbind(.pl_effects_ratio(), .pl_effects_diff())
  expect_error(sr_plot(mixed, "forest"), "mixed")
  bad <- .pl_effects_ratio()
  bad$lo[1] <- 0
  expect_error(sr_plot(bad, "forest"), "positive")
  none <- .pl_effects_ratio()
  none$est <- NA_real_
  expect_error(sr_plot(none, "forest"), "no row")
})

test_that("forest: a row without an interval is drawn as a point", {
  e <- .pl_effects_ratio()
  e$lo[2] <- NA
  e$hi[2] <- NA
  expect_s3_class(.pl_built(sr_plot(e, "forest"))$plot, "ggplot")
})

test_that("resid_panel: four panels from the one long frame", {
  p <- sr_plot(.pl_resid(), "resid_panel")
  expect_s3_class(p, "ggplot")
  b <- .pl_built(p)
  expect_equal(nrow(b$layout$layout), 4L)
  expect_equal(length(levels(p$data$panel)), 4L)
})

test_that("schoenfeld: one panel per term", {
  p <- sr_plot(.pl_schoenfeld(), "schoenfeld")
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(.pl_built(p)$layout$layout), 2L)
})

test_that("loglog: log time against log(-log S) by stratum", {
  k <- .pl_km()
  p <- sr_plot(k$km, "loglog")
  expect_s3_class(p, "ggplot")
  b <- .pl_built(p)
  expect_true(all(is.finite(p$data$x)))
  expect_true(all(is.finite(p$data$y)))
  expect_true(nrow(b$data[[1]]) > 0L)
})

test_that("smd: dots with the +-0.1 reference lines", {
  p <- sr_plot(.pl_smd(), "smd")
  expect_s3_class(p, "ggplot")
  .pl_built(p)
  v <- Filter(function(l) inherits(l$geom, "GeomVline"), p$layers)
  xs <- sort(unlist(lapply(v, function(l) l$data$xintercept)))
  expect_true(all(c(-0.1, 0.1) %in% xs))
  expect_false("smoker" %in% as.character(p$data$term))   # an undefined SMD is not plotted
})

test_that("smd: non-SMD effect rows are left out", {
  cols <- c("term", "measure", "est", "lo", "hi")
  e <- rbind(.pl_smd()[, cols], .pl_effects_diff()[, cols])
  p <- sr_plot(e, "smd")
  expect_false("treated" %in% as.character(p$data$term))
})

test_that("linearity: linear and RCS curves per variable", {
  p <- sr_plot(.pl_linearity(), "linearity")
  expect_s3_class(p, "ggplot")
  b <- .pl_built(p)
  expect_equal(nrow(b$layout$layout), 2L)
  expect_setequal(as.character(unique(p$data$curve)), c("Linear", "RCS"))
})

test_that("sr_plot refuses an unknown kind, a non-frame and a frame missing columns", {
  expect_error(sr_plot(.pl_smd(), "pie"), "kind")
  expect_error(sr_plot(stats::lm(dist ~ speed, datasets::cars), "forest"), "data frame")
  expect_error(sr_plot(NULL, "forest"), "data frame")
  expect_error(sr_plot(data.frame(term = "a", est = 1), "forest"), "lo, hi, scale")
  expect_error(sr_plot(data.frame(fitted = 1:3), "resid_panel"), "resid, std_resid, leverage, cooks")
  expect_error(sr_plot(data.frame(x = 1), "linearity"), "var")
  expect_error(sr_plot(list(km = data.frame(time = 1)), "km"), "strata")
})

# ---- sr_figure_save -------------------------------------------------------------------------------

test_that("sr_figure_save writes a PNG of exactly the requested pixel size", {
  d <- withr::local_tempdir()
  p <- sr_plot(.pl_effects_ratio(), "forest")
  r <- sr_figure_save(p, file.path(d, "fig", "forest.png"))
  expect_named(r, c("path", "sha"))
  expect_true(file.exists(r$path))
  dm <- .pl_png_dim(r$path)
  expect_identical(dm$sig, .pl_png_sig)
  expect_equal(dm$w, round(170 / 25.4 * 300))
  expect_equal(dm$h, round(120 / 25.4 * 300))
  r2 <- sr_figure_save(p, file.path(d, "small.png"), width_mm = 85, height_mm = 60, dpi = 200)
  dm2 <- .pl_png_dim(r2$path)
  expect_equal(dm2$w, round(85 / 25.4 * 200))
  expect_equal(dm2$h, round(60 / 25.4 * 200))
  r3 <- sr_figure_save(p, file.path(d, "odd.png"), width_mm = 100.3, height_mm = 77.7, dpi = 150)
  dm3 <- .pl_png_dim(r3$path)
  expect_equal(dm3$w, round(100.3 / 25.4 * 150))
  expect_equal(dm3$h, round(77.7 / 25.4 * 150))
})

test_that("sr_figure_save is deterministic and reports the sha of the written bytes", {
  d <- withr::local_tempdir()
  p <- sr_plot(.pl_resid(), "resid_panel")
  a <- sr_figure_save(p, file.path(d, "a.png"))
  b <- sr_figure_save(p, file.path(d, "b.png"))
  expect_identical(a$sha, b$sha)
  expect_identical(unname(tools::md5sum(a$path)), unname(tools::md5sum(b$path)))
  expect_identical(a$sha, .sr_sha256(readBin(a$path, "raw", n = file.size(a$path))))
  expect_match(a$sha, "^[0-9a-f]{64}$")
  a2 <- sr_figure_save(p, file.path(d, "a.png"))                   # overwrite in place
  expect_identical(a2$sha, a$sha)
  expect_false(any(grepl("\\.tmp$", list.files(d, recursive = TRUE, all.files = TRUE))))
})

test_that("sr_figure_save writes no timestamp or text chunk", {
  d <- withr::local_tempdir()
  r <- sr_figure_save(sr_plot(.pl_smd(), "smd"), file.path(d, "s.png"))
  ch <- .pl_png_chunks(r$path)
  expect_identical(ch[1], "IHDR")
  expect_identical(ch[length(ch)], "IEND")
  expect_false(any(ch %in% c("tIME", "tEXt", "zTXt", "iTXt")))
})

test_that("every kind renders to a PNG", {
  d <- withr::local_tempdir()
  k <- .pl_km()
  plots <- list(
    km = sr_plot(list(km = k$km, km_risk = k$risk), "km"), forest = sr_plot(.pl_effects_ratio(), "forest"),
    resid_panel = sr_plot(.pl_resid(), "resid_panel"), schoenfeld = sr_plot(.pl_schoenfeld(), "schoenfeld"),
    loglog = sr_plot(k$km, "loglog"), smd = sr_plot(.pl_smd(), "smd"),
    linearity = sr_plot(.pl_linearity(), "linearity"))
  for (nm in names(plots)) {
    r <- sr_figure_save(plots[[nm]], file.path(d, paste0(nm, ".png")))
    expect_gt(file.size(r$path), 2000, label = nm)
    expect_identical(.pl_png_dim(r$path)$sig, .pl_png_sig)
  }
})

test_that("sr_figure_save validates its arguments", {
  d <- withr::local_tempdir()
  p <- sr_plot(.pl_smd(), "smd")
  expect_error(sr_figure_save(list(), file.path(d, "x.png")), "ggplot")
  expect_error(sr_figure_save(p, file.path(d, "x.txt")), "png")
  expect_error(sr_figure_save(p, c(file.path(d, "x.png"), file.path(d, "y.png"))), "path")
  expect_error(sr_figure_save(p, file.path(d, "x.png"), width_mm = 0), "width_mm")
  expect_error(sr_figure_save(p, file.path(d, "x.png"), height_mm = NA), "height_mm")
  expect_error(sr_figure_save(p, file.path(d, "x.png"), dpi = -1), "dpi")
  expect_false(file.exists(file.path(d, "x.png")))
})
