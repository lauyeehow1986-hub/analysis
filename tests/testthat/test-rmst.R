# Tests for routes/rmst.R: rmst@1, the restricted mean survival time by exposure level up to tau. Public package
# data only (survival::lung); projects live under withr::local_tempdir().

.rm_layers <- function(clustered = FALSE) {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = clustered)
}

.rm_spec <- function(tau = 365, ...) {
  s <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 2, exposure = "sex",
            tau = tau, layers = .rm_layers(), seed = 3L)
  mod <- list(...)
  for (nm in names(mod)) s[nm] <- list(mod[[nm]])
  s
}

.rm_route <- function() {
  if (!(.sr_route_key("rmst", 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(.sr_rmst_route())
  sr_route_get("rmst", 1L)
}

.rm_value <- function(df, spec = .rm_spec()) {
  code <- sr_compile(.rm_route(), spec)
  expect_silent(.sr_emit_check(code, "analysis"))
  env <- new.env(parent = globalenv())
  env$sr_data <- df
  eval(parse(text = code), envir = env)
}

.rm_present <- function(value, spec = .rm_spec()) {
  r <- .rm_route()
  sr_result_check(r$present(value, spec), r)
}

# ---- registration and validation ----------------------------------------------------------------

test_that("rmst is registered with the documented schema", {
  r <- .rm_route()
  expect_identical(r$family, "survival")
  expect_identical(r$purpose, "explain-causal")
  expect_setequal(names(r$spec_schema), c("time", "status", "status_event", "exposure", "tau", "layers", "seed"))
  expect_identical(r$spec_schema$tau$type, "num")
  expect_true(isTRUE(r$spec_schema$tau$required))
  expect_s3_class(r$estimand(.rm_spec()), "sr_estimand")
  expect_true(sr_spec_validate(.rm_spec(), r)$ok)
})

test_that("tau must be one finite number above zero; clustered RMST is refused", {
  r <- .rm_route()
  for (bad in list(0, -1, Inf, -Inf, NA_real_, NaN, "365", c(100, 200), numeric())) {
    v <- sr_spec_validate(.rm_spec(tau = bad), r)
    expect_false(v$ok, info = deparse(bad))
  }
  expect_match(paste(sr_spec_validate(.rm_spec(tau = Inf), r)$errors, collapse = " "), "non-finite", fixed = TRUE)
  expect_match(paste(sr_spec_validate(.rm_spec(tau = 0), r)$errors, collapse = " "), "finite", fixed = TRUE)
  v <- sr_spec_validate(.rm_spec(layers = .rm_layers(TRUE)), r)
  expect_false(v$ok)
  expect_match(paste(v$errors, collapse = " "), "clustered RMST is not supported in P1c", fixed = TRUE)
  L <- .rm_layers()
  L$weights <- "ipw"
  expect_error(sr_compile(r, .rm_spec(layers = L)), "does not implement")
})

# ---- the numbers --------------------------------------------------------------------------------

test_that("lung by sex at tau = 365: group RMST, SE and difference", {
  d <- survival::lung
  val <- .rm_value(d)
  g <- val$groups
  expect_equal(g$level, c("1", "2"))
  expect_equal(g$rmst, c(241.4951, 297.4654), tolerance = 1e-4)
  expect_equal(g$se, c(10.35823, 10.79132), tolerance = 1e-4)
  tab <- summary(survival::survfit(survival::Surv(time, status == 2) ~ sex, d), rmean = 365)$table
  expect_equal(g$rmst, unname(tab[, "rmean"]), tolerance = 1e-8)
  expect_equal(g$se, unname(tab[, "se(rmean)"]), tolerance = 1e-8)
  res <- .rm_present(val)
  e <- res$effects
  dif <- e[e$measure == "RMST_diff", ]
  expect_equal(nrow(dif), 1L)
  se <- sqrt(10.35823^2 + 10.79132^2)
  expect_equal(dif$est, 55.9703, tolerance = 1e-4)
  expect_equal(se, 14.9581, tolerance = 1e-4)
  expect_equal(dif$lo, 55.9703 - 1.959964 * 14.9581, tolerance = 1e-3)
  expect_equal(dif$hi, 55.9703 + 1.959964 * 14.9581, tolerance = 1e-3)
  expect_equal(dif$est, g$rmst[2] - g$rmst[1], tolerance = 1e-10)
  expect_equal(dif$lo, dif$est - stats::qnorm(0.975) * sqrt(g$se[1]^2 + g$se[2]^2), tolerance = 1e-10)
  expect_equal(dif$p, 2 * stats::pnorm(-abs(dif$est / sqrt(g$se[1]^2 + g$se[2]^2))), tolerance = 1e-10)
  expect_identical(dif$role, "primary")
  expect_identical(dif$scale, "difference")
  expect_identical(dif$level, "2")
  expect_identical(dif$term, "sex: 2 vs 1")
  expect_equal(dif$n, 228)
  expect_equal(dif$events, 165)
  grp <- e[e$measure == "RMST", ]
  expect_equal(nrow(grp), 2L)
  expect_true(all(grp$role == "descriptive"))
  expect_equal(grp$est, g$rmst, tolerance = 1e-10)
  expect_equal(grp$n, c(138, 90))
  expect_equal(grp$events, c(112, 53))
  expect_true(is.data.frame(res$tables$rmst))
  expect_true(is.character(res$fragments$rmst_note))
  expect_false("rmst_few_at_risk" %in% names(res$fragments))
})

test_that("the KM figure data is provided by exposure level", {
  d <- survival::lung
  res <- .rm_present(.rm_value(d))
  fit <- survival::survfit(survival::Surv(time, status == 2) ~ sex, data = d)
  expect_equal(res$figures$km$surv, fit$surv, tolerance = 1e-10)
  expect_identical(unique(res$figures$km$strata), c("1", "2"))
  expect_identical(names(res$figures$km_risk), c("strata", "time", "n.risk"))
})

test_that("more than two groups: each level against the reference, independent groups", {
  d <- survival::lung
  d$grp <- as.character(cut(d$age, c(0, 60, 70, 100), labels = c("young", "mid", "old")))
  s <- .rm_spec(tau = 300, exposure = "grp", exposure_ref = "mid")
  val <- .rm_value(d, s)
  expect_equal(val$groups$level, c("mid", "old", "young"))
  tab <- summary(survival::survfit(survival::Surv(time, status == 2) ~ grp, d), rmean = 300)$table
  tab <- tab[c("grp=mid", "grp=old", "grp=young"), ]
  expect_equal(val$groups$rmst, unname(tab[, "rmean"]), tolerance = 1e-8)
  e <- .rm_present(val, s)$effects
  dif <- e[e$measure == "RMST_diff", ]
  expect_equal(dif$term, c("grp: old vs mid", "grp: young vs mid"))
  expect_equal(dif$est, unname(tab[c(2, 3), "rmean"] - tab[1, "rmean"]), tolerance = 1e-8)
})

test_that("rows with a missing time, status or exposure are dropped and counted", {
  d <- survival::lung
  d$sex[1:5] <- NA
  d$time[6:8] <- NA
  val <- .rm_value(d)
  expect_equal(val$n, 228 - 8)
  expect_equal(val$n_total, 228)
  dd <- d[stats::complete.cases(d[c("time", "status", "sex")]), ]
  tab <- summary(survival::survfit(survival::Surv(time, status == 2) ~ sex, dd), rmean = 365)$table
  expect_equal(val$groups$rmst, unname(tab[, "rmean"]), tolerance = 1e-8)
})

# ---- the horizon check --------------------------------------------------------------------------

test_that("tau beyond a group follow-up stops the code and names tau, the group and its maximum", {
  d <- survival::lung
  q <- "\x27"
  e <- tryCatch(.rm_value(d, .rm_spec(tau = 5000)), error = function(e) e)
  expect_s3_class(e, "error")
  expect_match(conditionMessage(e), paste0("tau = 5000 exceeds the follow-up of group ", q, "1", q, " (max time 1022)"),
               fixed = TRUE)
  # a tau that only the second group cannot reach names that group
  e2 <- tryCatch(.rm_value(d, .rm_spec(tau = 1000)), error = function(e) e)
  expect_match(conditionMessage(e2), paste0("group ", q, "2", q, " (max time 965)"), fixed = TRUE)
  # tau equal to the shortest longest follow-up is allowed
  expect_type(.rm_value(d, .rm_spec(tau = 965)), "list")
})

test_that("fewer than 10 at risk at tau in any group adds the thin-tail note", {
  d <- survival::lung
  val <- .rm_value(d, .rm_spec(tau = 700))
  expect_equal(val$groups$at_risk, c(sum(d$time[d$sex == 1] >= 700), sum(d$time[d$sex == 2] >= 700)))
  res <- .rm_present(val, .rm_spec(tau = 700))
  expect_true("rmst_few_at_risk" %in% names(res$fragments))
  expect_match(res$fragments$rmst_few_at_risk, "700", fixed = TRUE)
  expect_match(res$fragments$rmst_few_at_risk, "2", fixed = TRUE)
})

# ---- end to end ---------------------------------------------------------------------------------

test_that("rmst runs through sr_sap_add, lock and sr_run; a bad tau is refused or ends the run readably", {
  bt <- intToUtf8(96)
  hostile <- paste0("v", bt, "); system(", intToUtf8(39), "calc", intToUtf8(39), "); (")
  d <- survival::lung
  d$id <- seq_len(nrow(d))
  names(d)[names(d) == "time"] <- hostile
  p <- .fx_ready_project(d, "t", c(id = "id", status = "outcome"))
  .rm_route()
  for (bad in list(0, -1, Inf, NA_real_)) {
    expect_error(sr_sap_add(p, "bad", "rmst", .rm_spec(tau = bad, time = hostile), "t", label = "primary"),
                 "tau|required|non-finite")
  }
  expect_error(sr_sap_add(p, "bad", "rmst", .rm_spec(layers = .rm_layers(TRUE), time = hostile), "t",
                          label = "primary"), "clustered RMST")
  p <- sr_sap_add(p, "r1", "rmst", .rm_spec(time = hostile), "t", label = "primary")
  p <- sr_sap_add(p, "r2", "rmst", .rm_spec(tau = 5000, time = hostile), "t", label = "secondary")
  p <- sr_sap_lock(.fx_feasible(p))
  p <- sr_run(p, "r1")
  p <- sr_run(p, "r2")
  expect_true(p$state$runs$r1$ok, info = p$state$runs$r1$error)
  expect_false(p$state$runs$r2$ok)
  expect_match(p$state$runs$r2$error, "tau = 5000 exceeds the follow-up of group", fixed = TRUE)
  res <- sr_result(p, "r1")
  dif <- res$effects[res$effects$measure == "RMST_diff", ]
  expect_equal(dif$est, 55.9703, tolerance = 1e-4)
  expect_true(is.data.frame(res$figures$km) && nrow(res$figures$km) > 0L)
  expect_error(sr_result(p, "r2"), "has not run successfully")
})

# ---- fix round 1 --------------------------------------------------------------------------------

test_that("a numeric exposure is ordered as numbers: the reference is the smallest value", {
  d <- survival::lung
  d$g <- c(5, 10, 20)[as.integer(cut(d$age, c(0, 60, 70, 100)))]
  s <- .rm_spec(tau = 300, exposure = "g")
  val <- .rm_value(d, s)
  expect_equal(val$groups$level, c("5", "10", "20"))
  tab <- summary(survival::survfit(survival::Surv(time, status == 2) ~ g, d), rmean = 300)$table
  expect_equal(val$groups$rmst, unname(tab[, "rmean"]), tolerance = 1e-8)
  e <- .rm_present(val, s)$effects
  dif <- e[e$measure == "RMST_diff", ]
  expect_equal(dif$term, c("g: 10 vs 5", "g: 20 vs 5"))
  expect_equal(dif$est, unname(tab[c(2, 3), "rmean"] - tab[1, "rmean"]), tolerance = 1e-8)
  # an explicit reference still goes first
  val2 <- .rm_value(d, .rm_spec(tau = 300, exposure = "g", exposure_ref = "20"))
  expect_equal(val2$groups$level, c("20", "5", "10"))
})

test_that("an exposure with more than 8 distinct values is refused by the code", {
  d <- survival::lung
  e <- tryCatch(.rm_value(d, .rm_spec(exposure = "age")), error = function(e) e)
  expect_s3_class(e, "error")
  expect_match(conditionMessage(e), "RMST compares groups: exposure 'age' has", fixed = TRUE)
  expect_match(conditionMessage(e), paste(length(unique(d$age)), "distinct values; categorise it first"),
               fixed = TRUE)
  # 8 distinct values are allowed
  d8 <- d
  d8$g8 <- as.integer(cut(d8$age, c(0, 45, 50, 55, 60, 65, 70, 75, 100)))
  expect_equal(length(unique(d8$g8)), 8L)
  expect_type(.rm_value(d8, .rm_spec(tau = 300, exposure = "g8")), "list")
  # the guard counts the rows analysed, not the ones lost to a missing value
  d9 <- d8
  d9$g8[d9$g8 == 8L] <- NA
  expect_type(.rm_value(d9, .rm_spec(tau = 300, exposure = "g8")), "list")
})

test_that("the many-level guard ends the run with a readable error", {
  d <- survival::lung
  d$id <- seq_len(nrow(d))
  p <- .fx_ready_project(d, "t", c(id = "id", status = "outcome"))
  .rm_route()
  p <- sr_sap_add(p, "agey", "rmst", .rm_spec(exposure = "age"), "t", label = "primary")
  p <- sr_sap_lock(.fx_feasible(p))
  p <- sr_run(p, "agey")
  expect_false(p$state$runs$agey$ok)
  expect_match(p$state$runs$agey$error, "RMST compares groups: exposure 'age' has", fixed = TRUE)
  expect_match(p$state$runs$agey$error, "categorise it first", fixed = TRUE)
})

test_that("a non-finite RMST difference stops present with a plain message", {
  val <- .rm_value(survival::lung)
  val$groups$se[1L] <- NaN
  r <- .rm_route()
  expect_error(r$present(val, .rm_spec()), "not finite", fixed = TRUE)
  expect_error(r$present(val, .rm_spec()), "sex", fixed = TRUE)
  val2 <- .rm_value(survival::lung)
  val2$groups$rmst[2L] <- Inf
  expect_error(r$present(val2, .rm_spec()), "not finite", fixed = TRUE)
})

test_that("the thin-tail note names the groups and their counts", {
  d <- survival::lung
  val <- .rm_value(d, .rm_spec(tau = 700))
  g <- val$groups
  thin <- g$at_risk < 10
  expect_true(any(thin))
  frag <- .rm_present(val, .rm_spec(tau = 700))$fragments$rmst_few_at_risk
  expect_match(frag, "at tau = 700 in group(s) ", fixed = TRUE)
  expect_match(frag, paste0("group(s) ", paste(g$level[thin], collapse = ", "), " (",
                            paste(g$at_risk[thin], collapse = ", "), ")"), fixed = TRUE)
})
