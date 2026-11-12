# Memory vs stream equivalence at scale (manual tool; the testthat matrix is test-scale-equiv.R):
#   Rscript tools/scale_equiv.R [--rows 1000000] [--chunk 200000] [--out <dir>]
#
# It builds one synthetic table of `rows` rows (the shape of .sc_frame in tests/testthat/helper-scale.R:
# clusters of 1-9 rows, a 0/1 outcome y, a continuous yc, x1, a tie-free x2, a text factor grp with a
# non-ASCII level, the exposure sex), writes it once as the rds extract (memory engine) and once as the
# Parquet extract (stream engine) of a scratch project folder, and for each cell (route x SE x terms) it
# evaluates the run prologue plus the compiled route on both, exactly as a run does. Then it compares:
#   estimates, model SE, CIs, marginal effects : 1e-8 (relative, all.equal)
#   robust and cluster SE                      : 1e-7
#   counts, levels, aliased, flags             : identical
# One CSV row per cell and field and a markdown summary go to --out (default <repo>/tools/out, git-ignored).
# Prints SCALE EQUIV PASS when every comparison is within tolerance, SCALE EQUIV FAIL otherwise (status 1).
# Synthetic data only; no network. Run from anywhere: the script finds the repo from its own location.

.se_arg <- function(args, name, default) {
  i <- match(paste0("--", name), args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1L]
}

.se_frame <- function(n, seed = 1L) {
  set.seed(seed)
  sizes <- sample.int(9L, n, replace = TRUE)
  pid <- rep(seq_along(sizes), sizes)[seq_len(n)]
  x1 <- stats::rnorm(n)
  x2 <- stats::rgamma(n, shape = 2, rate = 1)
  sex <- sample(c("M", "F"), n, replace = TRUE)
  y <- stats::rbinom(n, 1L, stats::plogis(-0.5 + 0.4 * x1 + 0.3 * (sex == "M")))
  yc <- 1 + 0.5 * x1 + 0.3 * (sex == "M") + stats::rnorm(n)
  df <- data.frame(id = seq_len(n), pid = pid, y = y, yc = yc, x1 = x1, x2 = x2,
                   grp = sample(c("A", "b", "C", "\u00e9"), n, replace = TRUE), sex = sex,
                   stringsAsFactors = FALSE)
  df$y[seq(5L, n, by = 37L)] <- NA
  df$yc[seq(5L, n, by = 37L)] <- NA
  df$x1[seq(11L, n, by = 53L)] <- NA
  df
}

.se_route <- function(id) {
  reg <- function(key, make) {
    if (!(.sr_route_key(key, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make())
  }
  reg("logistic", .sr_logistic_route)
  reg("modpois", .sr_modpois_route)
  reg("lm", .sr_lm_route)
  sr_route_get(id, 1L)
}

.se_terms <- list(
  rcs = list(list(var = "x1"), list(var = "grp", type = "factor"), list(var = "x2", type = "rcs")),
  `quad+grp:x1` = list(list(var = "x1"), list(var = "grp", type = "factor"), list(var = "x2", type = "quad"))
)

.se_spec <- function(route, se, terms, exclude = NULL) {
  s <- list(purpose = "explain-causal", outcome = if (identical(route, "lm")) "yc" else "y", exposure = "sex",
            exposure_ref = "F", terms = .se_terms[[terms]], seed = 42L, se = se, standardize_over = "analysis",
            layers = list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                          framework = "frequentist", clustered = identical(se, "cluster")))
  if (identical(terms, "quad+grp:x1")) s$interactions <- list(c("grp", "x1"))
  if (identical(se, "cluster")) s$cluster <- "pid"
  if (identical(route, "lm")) s$marginal <- TRUE
  if (!is.null(exclude)) s$exclude_rows <- exclude
  s
}

# One comparison row: field, kind (num / exact), max abs diff, pass.
.se_cmp <- function(field, st, mem, tol = NULL) {
  if (is.null(tol)) {
    ok <- identical(st, mem)
    d <- NA_real_
  } else {
    a <- as.numeric(unlist(st))
    b <- as.numeric(unlist(mem))
    ok <- length(a) == length(b) && isTRUE(all.equal(a, b, tolerance = tol))
    d <- if (length(a) == length(b) && length(a)) max(abs(a - b), na.rm = TRUE) else NA_real_
  }
  data.frame(field = field, tol = if (is.null(tol)) "identical" else format(tol), max_abs_diff = d, pass = ok,
             stringsAsFactors = FALSE)
}

.se_compare <- function(st, mem, route, se) {
  se_tol <- if (identical(se, "model")) 1e-8 else 1e-7
  r <- list(.se_cmp("names", names(st), c(names(mem), "engine", "chunk", "passes", "iter")),
            .se_cmp("term", st$coef$term, mem$coef$term),
            .se_cmp("est", st$coef$est, mem$coef$est, 1e-8),
            .se_cmp("se", st$coef$se, mem$coef$se, se_tol),
            .se_cmp("lo", st$coef$lo, mem$coef$lo, se_tol),
            .se_cmp("hi", st$coef$hi, mem$coef$hi, se_tol))
  for (f in c("aliased", "n", "n_total", "levels", "n_clusters", "se_type")) r <- c(r, list(.se_cmp(f, st[[f]], mem[[f]])))
  if (identical(route, "lm")) {
    for (f in c("sigma", "r2", "adj_r2")) r <- c(r, list(.se_cmp(f, st[[f]], mem[[f]], 1e-8)))
  } else {
    for (f in c("events", "converged", "near_separation")) r <- c(r, list(.se_cmp(f, st[[f]], mem[[f]])))
    if (nrow(mem$obs)) r <- c(r, list(.se_cmp("obs", st$obs[c("est", "lo", "hi")], mem$obs[c("est", "lo", "hi")], 1e-12)))
  }
  mr <- mem$marginal$rows
  if (!is.null(mr)) {
    for (f in intersect(c("p1", "p0", "rd", "log_rr"), names(mr))) r <- c(r, list(.se_cmp(paste0("marginal_", f), st$marginal$rows[[f]], mr[[f]], 1e-8)))
    for (f in intersect(c("rd_se", "log_rr_se"), names(mr))) r <- c(r, list(.se_cmp(paste0("marginal_", f), st$marginal$rows[[f]], mr[[f]], se_tol)))
  }
  do.call(rbind, r)
}

.se_main <- function(args) {
  rows <- as.numeric(.se_arg(args, "rows", "1000000"))
  chunk <- as.numeric(.se_arg(args, "chunk", "200000"))
  if (!is.finite(rows) || rows < 1000 || !is.finite(chunk) || chunk < 1) stop("--rows >= 1000 and --chunk >= 1")
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
  root <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))
  out <- .se_arg(args, "out", file.path(root, "tools", "out"))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  setwd(root)
  source(file.path(root, "app", "global.R"), encoding = "UTF-8")

  t0 <- proc.time()[["elapsed"]]
  df <- .se_frame(as.integer(rows))
  proj <- tempfile("sr_scale_equiv_")
  dir.create(file.path(proj, "data", "extracts"), recursive = TRUE)
  on.exit(unlink(proj, recursive = TRUE, force = TRUE), add = TRUE)
  bytes <- serialize(df, NULL, version = 3)
  rds_sha <- .sr_sha256(bytes)
  rds_rel <- paste0("data/extracts/", rds_sha, ".rds")
  writeBin(bytes, file.path(proj, rds_rel))
  rm(bytes)
  df$.sr_row <- seq_len(nrow(df))
  tmp_pq <- file.path(proj, "data", "x.parquet")
  con <- DBI::dbConnect(duckdb::duckdb())
  duckdb::duckdb_register(con, "se_df", df)
  DBI::dbExecute(con, sprintf("COPY se_df TO '%s' (FORMAT PARQUET)", gsub("'", "''", tmp_pq)))
  DBI::dbDisconnect(con, shutdown = TRUE)
  pq_sha <- paste(as.character(unclass(openssl::sha256(file(tmp_pq)))), collapse = "")
  pq_rel <- paste0("data/extracts/", pq_sha, ".parquet")
  file.rename(tmp_pq, file.path(proj, pq_rel))
  rm(df)
  invisible(gc())
  cat(sprintf("scale equiv: %s rows, chunk %s, data ready in %.1f s\n", format(rows, big.mark = ",", scientific = FALSE),
              format(chunk, scientific = FALSE), proc.time()[["elapsed"]] - t0))

  mem_pro <- .sr_run_prologue(list(rel = rds_rel, sha = rds_sha))
  st_pro <- .sr_run_prologue_stream(list(rel = pq_rel, sha = pq_sha))
  ev <- function(code) {
    e <- new.env(parent = globalenv())
    e$sr_project_dir <- proj
    e$sr_duck_memory <- "2GB"
    e$sr_duck_threads <- 2L
    e$sr_duck_temp <- tempdir()
    eval(parse(text = code), e)
  }
  cells <- list()
  for (route in c("lm", "logistic", "modpois")) {
    for (se in c("model", "robust", "cluster")) {
      if (identical(route, "modpois") && identical(se, "model")) next
      for (terms in names(.se_terms)) cells[[length(cells) + 1L]] <- list(route = route, se = se, terms = terms, ex = NULL)
    }
    cells[[length(cells) + 1L]] <- list(route = route, se = "robust", terms = "rcs", ex = c(3, 500, rows - 1))
  }
  res <- list()
  for (cl in cells) {
    spec <- .se_spec(cl$route, cl$se, cl$terms, cl$ex)
    label <- paste(cl$route, cl$se, cl$terms, if (!is.null(cl$ex)) "exclude_rows")
    rt <- .se_route(cl$route)
    t1 <- proc.time()[["elapsed"]]
    mem <- ev(paste(mem_pro, sr_compile(rt, spec), sep = "\n"))
    t_mem <- proc.time()[["elapsed"]] - t1
    t1 <- proc.time()[["elapsed"]]
    sspec <- utils::modifyList(spec, list(engine = "stream", stream_chunk = chunk))
    st <- ev(paste(st_pro, sr_compile(rt, sspec), sep = "\n"))
    t_st <- proc.time()[["elapsed"]] - t1
    cmp <- .se_compare(st, mem, cl$route, cl$se)
    cmp <- cbind(data.frame(cell = label, mem_s = round(t_mem, 1), stream_s = round(t_st, 1),
                            passes = as.integer(st$passes %||% NA), stringsAsFactors = FALSE), cmp)
    res[[length(res) + 1L]] <- cmp
    cat(sprintf("%-40s memory %6.1f s  stream %6.1f s  %s\n", label, t_mem, t_st, if (all(cmp$pass)) "ok" else "FAIL"))
  }
  all_res <- do.call(rbind, res)
  ok <- all(all_res$pass)
  stem <- file.path(out, sprintf("scale_equiv_%s", format(rows, scientific = FALSE)))
  utils::write.csv(all_res, paste0(stem, ".csv"), row.names = FALSE)
  by_cell <- split(all_res, all_res$cell)
  md <- c(sprintf("# Scale equivalence: %s rows, chunk %s", format(rows, big.mark = ",", scientific = FALSE),
                  format(chunk, scientific = FALSE)), "",
          "Tolerances: 1e-8 estimates, model SE, CIs, marginal effects; 1e-7 robust and cluster SE; identical counts.", "",
          "| cell | memory s | stream s | passes | max abs diff (num) | result |", "|---|---|---|---|---|---|")
  for (nm in names(by_cell)) {
    b <- by_cell[[nm]]
    d <- suppressWarnings(max(b$max_abs_diff, na.rm = TRUE))
    md <- c(md, sprintf("| %s | %.1f | %.1f | %s | %.3g | %s |", nm, b$mem_s[1], b$stream_s[1], b$passes[1], d,
                        if (all(b$pass)) "PASS" else paste("FAIL:", paste(b$field[!b$pass], collapse = ", "))))
  }
  md <- c(md, "", sprintf("Total wall time: %.1f s. Result: %s.", proc.time()[["elapsed"]] - t0, if (ok) "PASS" else "FAIL"))
  writeLines(md, paste0(stem, ".md"))
  cat(sprintf("wrote %s.csv and %s.md\n", stem, stem))
  cat(if (ok) "SCALE EQUIV PASS\n" else "SCALE EQUIV FAIL\n")
  ok
}

if (sys.nframe() == 0L) {
  ok <- .se_main(commandArgs(trailingOnly = TRUE))
  quit(status = if (isTRUE(ok)) 0L else 1L)
}
