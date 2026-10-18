# Tests for routes/table1.R: the Table 1 route (summaries by group, SMD with CI, no p-values).
# Synthetic data only; everything lives under withr::local_tempdir().

.t1_frame <- function(groups = c("A", "B"), n = 150L) {
  set.seed(20261011)
  g <- sample(groups, n, replace = TRUE)
  shift <- match(g, groups) - 1
  age <- round(rnorm(n, 60 + 4 * shift, 9 + shift), 1)
  bmi <- round(rlnorm(n, 3.2 + 0.1 * shift, 0.25), 1)
  sex <- sample(c("F", "M"), n, replace = TRUE, prob = c(0.5, 0.5))
  sex[g == groups[length(groups)]] <- sample(c("F", "M"), sum(g == groups[length(groups)]),
                                             replace = TRUE, prob = c(0.35, 0.65))
  smoke <- sample(c("never", "former", "current"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2))
  smoke[shift == 1] <- sample(c("never", "former", "current"), sum(shift == 1), replace = TRUE,
                              prob = c(0.2, 0.3, 0.5))
  stage <- as.integer(runif(n) < 0.3 + 0.2 * (shift > 0))
  age[sample(n, 9)] <- NA
  sex[sample(n, 6)] <- NA
  smoke[sample(n, 11)] <- NA
  bmi[sample(n, 4)] <- NA
  data.frame(id = seq_len(n), grp = g, age = age, bmi = bmi, sex = sex, smoke = smoke,
             stage = stage, stringsAsFactors = FALSE)
}

.t1_layers <- function() {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = FALSE)
}

.t1_spec <- function(by = "grp", vars = c("age", "bmi", "sex", "smoke", "stage"),
                     nonnormal = "bmi", ...) {
  s <- list(purpose = "describe", by = by, vars = vars, layers = .t1_layers(), seed = 42L)
  if (length(nonnormal)) s$nonnormal <- nonnormal
  c(s, list(...))
}

# The route, registered again when an earlier test file emptied the registry.
.t1_route <- function() {
  if (!(.sr_route_key("table1", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
    sr_route_register(.sr_table1_route())
  }
  sr_route_get("table1", 1L)
}

# Compile the spec, vet the code, run it on `df` and return the value.
.t1_value <- function(df, spec = .t1_spec()) {
  route <- .t1_route()
  code <- sr_compile(route, spec)
  expect_silent(.sr_emit_check(code, "analysis"))
  env <- new.env(parent = globalenv())
  env$sr_data <- df
  eval(parse(text = code), envir = env)
}

.t1_var <- function(val, name) {
  for (v in val$vars) if (identical(v$var, name)) return(v)
  stop("no such var in the value: ", name)
}

# ---- the route ---------------------------------------------------------------------------------

test_that("table1@1 is a registered describe route with the documented schema and estimand", {
  r <- .t1_route()
  expect_identical(r$id, "table1")
  expect_identical(r$version, 1L)
  expect_identical(r$family, "describe")
  expect_identical(r$purpose, "describe")
  expect_identical(r$estimand$population, "analysed sample")
  expect_identical(r$estimand$summary, "baseline characteristics by group")
  sc <- r$spec_schema
  expect_identical(sc$by$type, "chr");        expect_true(sc$by$required)
  expect_identical(sc$vars$type, "chr");      expect_true(sc$vars$required)
  expect_identical(sc$nonnormal$type, "chr"); expect_false(sc$nonnormal$required)
  expect_true("seed" %in% names(sc))
  expect_length(formals(r$compile), 1L)                 # compile takes the spec and no data
  expect_true(sr_spec_validate(.t1_spec(), r)$ok)
  expect_false(sr_spec_validate(.t1_spec(vars = NULL), r)$ok)
  expect_false(sr_spec_validate(c(list(purpose = "describe", vars = "age", seed = 1L)), r)$ok)
  expect_false(sr_spec_validate(modifyList(.t1_spec(), list(purpose = "predict")), r)$ok)
})

test_that("sr_spec_cols reads by, vars and nonnormal", {
  expect_identical(sr_spec_cols(.t1_spec()), c("grp", "age", "bmi", "sex", "smoke", "stage"))
  expect_identical(sr_spec_cols(.t1_spec(vars = c("age", "grp"), nonnormal = NULL)), c("grp", "age"))
})

test_that("compile refuses a spec that cannot make a table", {
  r <- .t1_route()
  expect_error(sr_compile(r, .t1_spec(by = c("grp", "sex"))), "by")
  expect_error(sr_compile(r, .t1_spec(vars = c("age", "grp"))), "by")
  expect_error(sr_compile(r, .t1_spec(vars = c("age", "age"))), "twice")
  expect_error(sr_compile(r, .t1_spec(nonnormal = "weight")), "nonnormal")
})

test_that("compile is deterministic and passes the emit check", {
  r <- .t1_route()
  a <- sr_compile(r, .t1_spec())
  expect_identical(a, sr_compile(r, .t1_spec()))
  expect_silent(.sr_emit_check(a, "analysis"))
})

# ---- the numbers -------------------------------------------------------------------------------

test_that("the continuous SMD and its CI equal the documented formulas", {
  df <- .t1_frame()
  val <- .t1_value(df)
  v <- .t1_var(val, "age")
  expect_identical(v$kind, "continuous")
  x1 <- df$age[df$grp == "B" & !is.na(df$age)]
  x0 <- df$age[df$grp == "A" & !is.na(df$age)]
  d <- (mean(x1) - mean(x0)) / sqrt((stats::var(x1) + stats::var(x0)) / 2)
  n1 <- length(x1)
  n0 <- length(x0)
  se <- sqrt((n1 + n0) / (n1 * n0) + d^2 / (2 * (n1 + n0)))
  expect_equal(v$smd, d, tolerance = 1e-12)
  expect_equal(v$smd_lo, d - 1.96 * se, tolerance = 1e-12)
  expect_equal(v$smd_hi, d + 1.96 * se, tolerance = 1e-12)
  expect_equal(v$mean[1:2], c(mean(x0), mean(x1)), tolerance = 1e-12)
  expect_equal(v$sd[1:2], c(stats::sd(x0), stats::sd(x1)), tolerance = 1e-12)
  expect_equal(v$n[1:2], c(n0, n1))
  expect_equal(v$n_miss[1:2], c(sum(is.na(df$age[df$grp == "A"])), sum(is.na(df$age[df$grp == "B"]))))
  # overall is the last entry
  allx <- df$age[!is.na(df$age)]
  expect_equal(v$mean[3], mean(allx), tolerance = 1e-12)
  expect_equal(v$n[3], length(allx))
  expect_equal(v$n_miss[3], sum(is.na(df$age)))
  # the median / quartiles are kept for a nonnormal variable
  b <- .t1_var(val, "bmi")
  expect_true(b$nonnormal)
  expect_false(v$nonnormal)
  bq <- stats::quantile(df$bmi[!is.na(df$bmi)], c(0.25, 0.5, 0.75), names = FALSE)
  expect_equal(c(b$q1[3], b$median[3], b$q3[3]), bq[c(1, 2, 3)], tolerance = 1e-12)
})

test_that("the binary SMD equals the hand formula (second level against the first)", {
  df <- .t1_frame()
  val <- .t1_value(df)
  v <- .t1_var(val, "sex")
  expect_identical(v$kind, "categorical")
  expect_identical(v$levels, c("F", "M"))
  s1 <- df$sex[df$grp == "B" & !is.na(df$sex)]
  s0 <- df$sex[df$grp == "A" & !is.na(df$sex)]
  p1 <- mean(s1 == "M")
  p0 <- mean(s0 == "M")
  d <- (p1 - p0) / sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
  n1 <- length(s1)
  n0 <- length(s0)
  se <- sqrt((n1 + n0) / (n1 * n0) + d^2 / (2 * (n1 + n0)))
  expect_equal(v$smd, d, tolerance = 1e-12)
  expect_equal(v$smd_lo, d - 1.96 * se, tolerance = 1e-12)
  expect_equal(v$smd_hi, d + 1.96 * se, tolerance = 1e-12)
  expect_equal(unname(v$count[, 1]), c(sum(s0 == "F"), sum(s0 == "M")))
  expect_equal(unname(v$count[, 3]), c(sum(df$sex == "F", na.rm = TRUE), sum(df$sex == "M", na.rm = TRUE)))
  # a 0/1 numeric column is a binary categorical, not a continuous variable
  st <- .t1_var(val, "stage")
  expect_identical(st$kind, "categorical")
  expect_identical(st$levels, c("0", "1"))
})

# Yang-Dalton from the matrix formula, drop the first level.
.t1_yd <- function(c1, c0) {
  p1 <- (c1 / sum(c1))[-1]
  p0 <- (c0 / sum(c0))[-1]
  mk <- function(p) {
    S <- -outer(p, p)
    diag(S) <- p * (1 - p)
    S
  }
  S <- (mk(p1) + mk(p0)) / 2
  sqrt(drop(t(p1 - p0) %*% solve(S) %*% (p1 - p0)))
}

test_that("the multi-level SMD is the Yang-Dalton multivariate distance, with no CI", {
  df <- .t1_frame()
  val <- .t1_value(df)
  v <- .t1_var(val, "smoke")
  expect_identical(v$levels, c("current", "former", "never"))
  c0 <- as.numeric(table(factor(df$smoke[df$grp == "A"], v$levels)))
  c1 <- as.numeric(table(factor(df$smoke[df$grp == "B"], v$levels)))
  expect_equal(v$smd, .t1_yd(c1, c0), tolerance = 1e-12)
  expect_true(is.na(v$smd_lo))
  expect_true(is.na(v$smd_hi))
  expect_identical(v$smd_method, "yang_dalton")
})

test_that("a level empty in both compared groups does not break the multivariate SMD", {
  df <- .t1_frame(groups = c("A", "B", "C"))
  # level "former" is absent from A and B (present only in C)
  df$smoke[df$grp %in% c("A", "B") & df$smoke == "former" & !is.na(df$smoke)] <- "never"
  val <- .t1_value(df)
  v <- .t1_var(val, "smoke")
  expect_true(all(is.finite(v$smd)))
  c_a <- as.numeric(table(factor(df$smoke[df$grp == "A"], v$levels)))
  c_b <- as.numeric(table(factor(df$smoke[df$grp == "B"], v$levels)))
  keep <- c_a + c_b > 0
  # the first level (the reference) is dropped, so compare on the levels both groups use
  expect_equal(v$smd[1], .t1_yd(c_b[keep], c_a[keep]), tolerance = 1e-10)
})

test_that("three groups give SMD of each group against the first level", {
  df <- .t1_frame(groups = c("A", "B", "C"))
  val <- .t1_value(df)
  expect_identical(val$groups, c("A", "B", "C"))
  v <- .t1_var(val, "age")
  expect_length(v$smd, 2L)
  x0 <- df$age[df$grp == "A" & !is.na(df$age)]
  for (k in 1:2) {
    x1 <- df$age[df$grp == c("B", "C")[k] & !is.na(df$age)]
    d <- (mean(x1) - mean(x0)) / sqrt((stats::var(x1) + stats::var(x0)) / 2)
    expect_equal(v$smd[k], d, tolerance = 1e-12)
  }
  expect_length(.t1_var(val, "smoke")$smd, 2L)
  pr <- .t1_route()$present(val, .t1_spec())
  tb <- pr$tables$table1
  expect_identical(sum(grepl("^SMD", names(tb))), 2L)
  expect_true(all(c("SMD: B vs A", "SMD: C vs A") %in% names(tb)))
  e <- pr$effects
  expect_identical(sum(e$term %in% c("age: B vs A", "age: C vs A")), 2L)
})

# ---- the presented result ----------------------------------------------------------------------

test_that("present passes the result contract; every effect is descriptive with p = NA", {
  df <- .t1_frame()
  val <- .t1_value(df)
  r <- .t1_route()
  pr <- sr_result_check(r$present(val, .t1_spec()), r)
  e <- pr$effects
  expect_true(all(e$role == "descriptive"))
  expect_true(all(e$measure == "SMD"))
  expect_true(all(is.na(e$p)))
  expect_identical(e$term, c("age", "bmi", "sex", "smoke", "stage"))
  expect_identical(e$ci_method[e$term == "smoke"], "none")
  expect_true(all(is.na(e$lo[e$term == "smoke"])))
  expect_false(anyNA(e$lo[e$term != "smoke"]))
  expect_identical(e$level[e$term == "sex"], "M")
  expect_equal(e$est[e$term == "age"], .t1_var(val, "age")$smd, tolerance = 1e-12)
  expect_equal(e$n[e$term == "age"], sum(.t1_var(val, "age")$n[1:2]))
  expect_identical(nrow(pr$flags), 0L)
})

test_that("no table of the result has a column named like a p-value", {
  val <- .t1_value(.t1_frame(groups = c("A", "B", "C")))
  r <- .t1_route()
  pr <- r$present(val, .t1_spec())
  expect_gte(length(pr$tables), 2L)
  for (nm in names(pr$tables)) {
    cn <- names(pr$tables[[nm]])
    expect_false(any(grepl("^p(\\.|_)?val", cn, ignore.case = TRUE)), info = nm)
    expect_false(any(cn == "p"), info = nm)
  }
  expect_false(any(grepl("^p(\\.|_)?val", names(pr$effects), ignore.case = TRUE)))
  expect_true(all(is.na(pr$effects$p)))
})

test_that("the display table formats mean (SD), median [IQR], n (%) and the missing count", {
  df <- .t1_frame()
  val <- .t1_value(df)
  r <- .t1_route()
  tb <- r$present(val, .t1_spec())$tables$table1
  expect_identical(names(tb)[1], "Characteristic")
  expect_true("Missing, n" %in% names(tb))
  expect_identical(sum(grepl("^SMD", names(tb))), 1L)
  expect_true("SMD" %in% names(tb))
  expect_match(names(tb)[2], "^Overall \\(n=150\\)$")
  expect_true(any(grepl("^A \\(n=[0-9]+\\)$", names(tb))))
  expect_true(any(tb$Characteristic == "age, mean (SD)"))
  expect_true(any(tb$Characteristic == "bmi, median [IQR]"))
  expect_true(any(tb$Characteristic == "sex, n (%)"))
  row_age <- tb[tb$Characteristic == "age, mean (SD)", ]
  expect_identical(row_age[["Overall (n=150)"]],
                   sprintf("%s (%s)", sr_fmt_num(mean(df$age, na.rm = TRUE)),
                           sr_fmt_num(stats::sd(df$age, na.rm = TRUE))))
  expect_identical(row_age[["Missing, n"]], as.character(sum(is.na(df$age))))
  row_bmi <- tb[tb$Characteristic == "bmi, median [IQR]", ]
  qs <- stats::quantile(df$bmi, c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
  expect_identical(row_bmi[["Overall (n=150)"]],
                   sprintf("%s [%s, %s]", sr_fmt_num(qs[2]), sr_fmt_num(qs[1]), sr_fmt_num(qs[3])))
  expect_match(row_age$SMD, "^-?[0-9.]+ \\(-?[0-9.]+ to -?[0-9.]+\\)$")
  # a categorical variable gets one row per level, indented under its header row
  i <- which(tb$Characteristic == "sex, n (%)")
  expect_identical(trimws(tb$Characteristic[i + 1:2]), c("F", "M"))
  nF <- sum(df$sex == "F", na.rm = TRUE)
  expect_identical(tb[["Overall (n=150)"]][i + 1L],
                   sprintf("%d (%s%%)", nF, sr_fmt_num(100 * nF / sum(!is.na(df$sex)), digits = 1L)))
  # multi-level SMD has no CI in the display
  j <- which(tb$Characteristic == "smoke, n (%)")
  expect_false(grepl("to", tb$SMD[j], fixed = TRUE))
  expect_identical(tb$SMD[j], sr_fmt_num(.t1_var(val, "smoke")$smd))
  mt <- r$present(val, .t1_spec())$tables$missing
  expect_identical(mt$Variable, c("age", "bmi", "sex", "smoke", "stage", "Excluded: missing grp"))
  expect_identical(mt$Overall, c(sum(is.na(df$age)), sum(is.na(df$bmi)), sum(is.na(df$sex)),
                                 sum(is.na(df$smoke)), 0L, 0L))
})

test_that("rows with a missing grouping value are excluded and counted", {
  df <- .t1_frame()
  df$grp[c(3, 9, 40)] <- NA
  val <- .t1_value(df)
  expect_identical(val$n_excluded, 3L)
  expect_equal(val$n[3], 147)
  expect_error(.t1_value(transform(df, grp = "A")), "two")
})

test_that("a hostile column name travels through compile as data, not code", {
  df <- .t1_frame()
  hostile <- "a b`); system('calc'); (\n\\"
  names(df)[names(df) == "age"] <- hostile
  names(df)[names(df) == "grp"] <- "g r`p"
  spec <- .t1_spec(by = "g r`p", vars = c(hostile, "sex"), nonnormal = hostile)
  val <- .t1_value(df, spec)
  expect_identical(val$vars[[1L]]$var, hostile)
  expect_true(val$vars[[1L]]$nonnormal)
  r <- .t1_route()
  pr <- sr_result_check(r$present(val, spec), r)
  expect_identical(pr$effects$term[1L], hostile)
  # a date column is refused with a message, not summarised as thousands of levels
  df2 <- .t1_frame()
  df2$when <- as.Date("2020-01-01") + seq_len(nrow(df2))
  expect_error(.t1_value(df2, .t1_spec(vars = c("age", "when"), nonnormal = NULL)), "date")
})

# ---- end to end --------------------------------------------------------------------------------

test_that("Table 1 runs end to end through sr_run on a locked project", {
  df <- .t1_frame()
  p <- .fx_ready_project(df, "t", c(id = "id", stage = "outcome"))
  .t1_route()
  p <- sr_sap_add(p, "t1", "table1", .t1_spec(), "t", label = "primary")
  p <- sr_sap_lock(.fx_feasible(p))
  p <- sr_run(p, "t1")
  expect_true(p$state$runs$t1$ok, info = p$state$runs$t1$error)
  res <- sr_result(p, "t1")
  expect_identical(res$effects$term, c("age", "bmi", "sex", "smoke", "stage"))
  expect_true(all(is.na(res$effects$p)))
  expect_true(all(res$effects$role == "descriptive"))
  x1 <- df$age[df$grp == "B" & !is.na(df$age)]
  x0 <- df$age[df$grp == "A" & !is.na(df$age)]
  d <- (mean(x1) - mean(x0)) / sqrt((stats::var(x1) + stats::var(x0)) / 2)
  expect_equal(res$effects$est[1L], d, tolerance = 1e-12)
  expect_true("table1" %in% names(res$tables))
  expect_identical(nrow(res$flags), 0L)
  # the second run is a cache hit
  p2 <- sr_run(p, "t1")
  expect_true(p2$state$runs$t1$cached)
})

# ---- fix round 1 ---------------------------------------------------------------------------------

test_that("rows excluded for a missing grouping value are shown in the presented output", {
  df <- .t1_frame()
  df$grp[c(3, 9, 40)] <- NA
  r <- .t1_route()
  pr <- r$present(.t1_value(df), .t1_spec())
  mt <- pr$tables$missing
  i <- which(mt$Variable == "Excluded: missing grp")
  expect_length(i, 1L)
  expect_identical(mt$Overall[i], sum(is.na(df$grp)))
  expect_true(all(is.na(mt[i, c("A", "B")])))
  expect_match(paste(unlist(pr$fragments), collapse = " "), "3 row(s) excluded", fixed = TRUE)
  expect_match(paste(unlist(pr$fragments), collapse = " "), "grp", fixed = TRUE)
  # nothing excluded: the row is still there, with 0
  pr0 <- r$present(.t1_value(.t1_frame()), .t1_spec())
  expect_identical(pr0$tables$missing$Overall[pr0$tables$missing$Variable == "Excluded: missing grp"], 0L)
  expect_silent(sr_result_check(pr, r))
})

test_that("group and level order is the C-locale (radix) order, not the session locale's", {
  df <- data.frame(g = rep(c("b", "B", "a"), 10), v = rep(c("x", "X", "w", "W", "x", "w"), 5),
                   m = as.numeric(seq_len(30)), stringsAsFactors = FALSE)
  spec <- list(purpose = "describe", by = "g", vars = c("v", "m"), layers = .t1_layers(), seed = 1L)
  val <- .t1_value(df, spec)
  expect_identical(val$groups, c("B", "a", "b"))
  expect_identical(.t1_var(val, "v")$levels, c("W", "X", "w", "x"))
  expect_identical(sort(c("b", "B", "a"), method = "radix"), c("B", "a", "b"))
  # a factor keeps its declared level order
  df$g <- factor(df$g, levels = c("b", "a", "B"))
  expect_identical(.t1_value(df, spec)$groups, c("b", "a", "B"))
})

test_that("a group with one observation gives an NA SMD, not an error", {
  df <- data.frame(g = c("A", rep("B", 9)), age = c(5, 1:9 + 0.5), stringsAsFactors = FALSE)
  spec <- list(purpose = "describe", by = "g", vars = "age", layers = .t1_layers(), seed = 1L)
  val <- .t1_value(df, spec)
  v <- .t1_var(val, "age")
  expect_equal(v$n[1:2], c(1, 9))
  expect_true(is.na(v$sd[1]))
  expect_true(is.na(v$smd) && is.na(v$smd_lo) && is.na(v$smd_hi))
  r <- .t1_route()
  pr <- sr_result_check(r$present(val, spec), r)
  expect_identical(pr$effects$ci_method, "none")
  expect_identical(pr$tables$table1$SMD, "NA")
})

test_that("zero pooled SD, a binary level absent from both groups and all-NA variables give NA", {
  df <- data.frame(g = rep(c("A", "B"), each = 6), k = rep(c(5, 7), each = 6),
                   one = "u", allna = NA_real_, chrna = NA_character_,
                   flag = rep(c(0, 1), each = 6), stringsAsFactors = FALSE)
  spec <- list(purpose = "describe", by = "g", vars = c("k", "one", "allna", "chrna", "flag"),
               layers = .t1_layers(), seed = 1L)
  val <- .t1_value(df, spec)
  # both groups constant: the pooled SD is 0, the SMD is undefined
  expect_true(is.na(.t1_var(val, "k")$smd))
  # one level only: identical groups, a multivariate distance of 0 and no CI
  one <- .t1_var(val, "one")
  expect_equal(one$smd, 0)
  expect_true(is.na(one$smd_lo))
  # all-NA numeric: continuous, no observations; all-NA text: categorical with no levels
  an <- .t1_var(val, "allna")
  expect_identical(an$kind, "continuous")
  expect_equal(an$n, c(0, 0, 0))
  expect_equal(an$n_miss, c(6, 6, 12))
  expect_true(is.na(an$smd))
  cn <- .t1_var(val, "chrna")
  expect_identical(cn$kind, "categorical")
  expect_length(cn$levels, 0L)
  expect_true(is.na(cn$smd))
  # 0/1 flag perfectly separated: p = 0 and 1, both variances 0, the SMD is undefined
  expect_true(is.na(.t1_var(val, "flag")$smd))
  r <- .t1_route()
  pr <- sr_result_check(r$present(val, spec), r)
  expect_true(all(is.na(pr$effects$est[pr$effects$term %in% c("k", "allna", "chrna", "flag")])))
  expect_identical(pr$effects$ci_method[pr$effects$term == "flag"], "none")
  expect_true(any(grepl("allna", pr$tables$table1$Characteristic)))
})

test_that("a fully separated multi-level variable has an undefined (NA) multivariate SMD", {
  # two groups: A all "a", B "b" and "c" mixed (S is singular, the difference is outside its range)
  df <- data.frame(g = rep(c("A", "B"), each = 8),
                   v = c(rep("a", 8), rep(c("b", "c"), 4)), stringsAsFactors = FALSE)
  spec <- list(purpose = "describe", by = "g", vars = "v", layers = .t1_layers(), seed = 1L)
  v <- .t1_var(.t1_value(df, spec), "v")
  expect_true(is.na(v$smd))
  # three groups, each with its own single level: no within-group variance at all
  df3 <- data.frame(g = rep(c("A", "B", "C"), each = 5), v = rep(c("a", "b", "c"), each = 5),
                    stringsAsFactors = FALSE)
  val3 <- .t1_value(df3, spec)
  v3 <- .t1_var(val3, "v")
  expect_length(v3$smd, 2L)
  expect_true(all(is.na(v3$smd)))
  r <- .t1_route()
  pr <- sr_result_check(r$present(val3, spec), r)
  expect_true(all(pr$effects$ci_method == "none"))
  expect_true(all(is.na(pr$effects$est)))
})

test_that("a reference level empty in both compared groups still gives the right multivariate SMD", {
  set.seed(7)
  g <- rep(c("A", "B", "C"), each = 40)
  v <- c(sample(c("b", "c", "d"), 40, TRUE, prob = c(0.5, 0.3, 0.2)),
         sample(c("b", "c", "d"), 40, TRUE, prob = c(0.2, 0.3, 0.5)),
         sample(c("a", "b", "c", "d"), 40, TRUE))
  df <- data.frame(g = g, v = v, stringsAsFactors = FALSE)
  spec <- list(purpose = "describe", by = "g", vars = "v", layers = .t1_layers(), seed = 1L)
  out <- .t1_var(.t1_value(df, spec), "v")
  expect_identical(out$levels, c("a", "b", "c", "d"))
  cb <- as.numeric(table(factor(df$v[df$g == "B"], c("b", "c", "d"))))
  ca <- as.numeric(table(factor(df$v[df$g == "A"], c("b", "c", "d"))))
  # the distance does not depend on which level is dropped; here the first present level is
  expect_equal(out$smd[1], .t1_yd(cb, ca), tolerance = 1e-8)
  expect_true(is.finite(out$smd[2]))
})