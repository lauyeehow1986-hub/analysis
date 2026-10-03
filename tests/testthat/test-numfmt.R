test_that("sr_fmt_num limits decimals by MCSE (pinned values)", {
  expect_identical(sr_fmt_num(0.4237, mcse = 0.004), "0.42")
  expect_identical(sr_fmt_num(0.4237, mcse = 0.0004), "0.424")
  expect_identical(sr_fmt_num(0.4237, mcse = 0.04), "0.4")
  expect_identical(sr_fmt_num(12.3456), "12.35")
  expect_identical(sr_fmt_num(-0.001), "0.00")
})

test_that("sr_fmt_num uses `digits` when mcse is NA or 0 and honours the option", {
  expect_identical(sr_fmt_num(1.23456, mcse = NA), "1.23")
  expect_identical(sr_fmt_num(1.23456, mcse = 0), "1.23")
  expect_identical(sr_fmt_num(1.23456, digits = 4L), "1.2346")
  withr::with_options(list(sr.digits = 3L), expect_identical(sr_fmt_num(1.23456), "1.235"))
  withr::with_envvar(c(SR_DIGITS = "1"), expect_identical(sr_fmt_num(1.26), "1.3"))
})

test_that("sr_fmt_num is vectorised over x and mcse", {
  expect_identical(sr_fmt_num(c(1.234, 2.345, 3.456)), c("1.23", "2.35", "3.46"))
  expect_identical(sr_fmt_num(c(1.234, 2.345), mcse = 0.04), c("1.2", "2.3"))
  expect_identical(sr_fmt_num(c(1.234, NA, 5), mcse = c(0.004, 0.004, 0.04)),
                   c("1.23", "NA", "5.0"))
  expect_identical(sr_fmt_num(numeric(0)), character(0))
})

test_that("NA formats as NA (including bare logical NA)", {
  expect_identical(sr_fmt_num(NA), "NA")
  expect_identical(sr_fmt_num(NA_real_, mcse = 0.01), "NA")
  expect_identical(sr_fmt_p(NA), "NA")
  expect_identical(sr_fmt_p(c(0.5, NA_real_)), c("0.50", "NA"))
  expect_identical(sr_fmt_ci(NA, NA, NA), "NA (NA to NA)")
})

test_that("negative zero never prints a minus sign", {
  expect_identical(sr_fmt_num(-0.001), "0.00")
  expect_identical(sr_fmt_num(-0.0004, mcse = 0.0004), "0.000")
  expect_identical(sr_fmt_num(-0.4, mcse = 5), "0")
  expect_identical(sr_fmt_num(-0.04, mcse = 0.04), "0.0")
  expect_identical(sr_fmt_num(-0.01), "-0.01")
  expect_identical(sr_fmt_num(-10.2, mcse = 5), "-10")
  expect_identical(sr_fmt_ci(-0.001, -0.004, 0.002), "0.00 (0.00 to 0.00)")
})

test_that("sr_mcse_digits returns the final d with the digits + 2 cap", {
  expect_identical(sr_mcse_digits(0.004), 2L)
  expect_identical(sr_mcse_digits(0.0004), 3L)
  expect_identical(sr_mcse_digits(0.04), 1L)
  expect_identical(sr_mcse_digits(NA), 2L)
  expect_identical(sr_mcse_digits(0), 2L)
  expect_identical(sr_mcse_digits(NA, digits = 3L), 3L)
  expect_identical(sr_mcse_digits(1e-9), 4L)
  expect_identical(sr_mcse_digits(1e-9, digits = 3L), 5L)
  expect_identical(sr_mcse_digits(5), 0L)
  expect_identical(sr_mcse_digits(c(0.004, NA, 0.04)), c(2L, 2L, 1L))
  withr::with_options(list(sr.digits = 1L), expect_identical(sr_mcse_digits(NA), 1L))
})

test_that("sr_fmt_ci shares one d and uses ' to ' (pinned value)", {
  expect_identical(sr_fmt_ci(0.4237, 0.301, 0.5799), "0.42 (0.30 to 0.58)")
  expect_identical(sr_fmt_ci(0.4237, 0.301, 0.5799, mcse = 0.0004), "0.424 (0.301 to 0.580)")
  expect_identical(sr_fmt_ci(0.4237, 0.301, 0.5799, mcse = 0.04), "0.4 (0.3 to 0.6)")
  expect_identical(sr_fmt_ci(-0.5, -0.9, -0.1), "-0.50 (-0.90 to -0.10)")
  expect_identical(sr_fmt_ci(1.2345, 0.5, 2.5, digits = 3L), "1.234 (0.500 to 2.500)")
})

test_that("sr_fmt_ci is vectorised and prints NA per slot", {
  expect_identical(sr_fmt_ci(c(0.1, 0.2), c(0, 0.1), c(0.2, 0.3)),
                   c("0.10 (0.00 to 0.20)", "0.20 (0.10 to 0.30)"))
  expect_identical(sr_fmt_ci(0.5, NA, 0.9), "0.50 (NA to 0.90)")
  expect_identical(sr_fmt_ci(NA, 0.1, 0.9), "NA (0.10 to 0.90)")
  expect_identical(sr_fmt_ci(c(0.1, 0.2), c(0, 0.1), c(0.2, 0.3), mcse = c(0.04, 0.004)),
                   c("0.1 (0.0 to 0.2)", "0.20 (0.10 to 0.30)"))
  expect_error(sr_fmt_ci(c(1, 2), c(0, 1, 2), c(2, 3)), "length")
})

test_that("sr_fmt_p pinned values", {
  expect_identical(sr_fmt_p(0.0004), "<0.001")
  expect_identical(sr_fmt_p(0.0042), "0.004")
  expect_identical(sr_fmt_p(0.0496), "0.0496")
  expect_identical(sr_fmt_p(0.0451), "0.045")
  expect_identical(sr_fmt_p(0.0449), "0.04")
  expect_identical(sr_fmt_p(0.0501), "0.05")
  expect_identical(sr_fmt_p(0.32), "0.32")
  expect_identical(sr_fmt_p(0.995), ">0.99")
  expect_identical(sr_fmt_p(NA), "NA")
})

test_that("sr_fmt_p boundaries and the 0.05 guard", {
  expect_identical(sr_fmt_p(0), "<0.001")
  expect_identical(sr_fmt_p(0.001), "0.001")
  expect_identical(sr_fmt_p(0.01), "0.01")
  expect_identical(sr_fmt_p(0.05), "0.05")
  expect_identical(sr_fmt_p(0.99), "0.99")
  expect_identical(sr_fmt_p(0.9901), ">0.99")
  expect_identical(sr_fmt_p(1), ">0.99")
  expect_identical(sr_fmt_p(0.04996), "0.04996")
  expect_identical(sr_fmt_p(0.0499994), "0.049999")
  expect_identical(sr_fmt_p(0.0499996), "<0.05")
  # still rounds to 0.05 at 6 dp -> never print "0.05" for a significant p
  expect_identical(sr_fmt_p(0.04999999), "<0.05")
  expect_identical(sr_fmt_p(0.5), "0.50")
})

test_that("sr_fmt_p is vectorised", {
  expect_identical(sr_fmt_p(c(0.0004, 0.0449, 0.0496, 0.32, 0.995, NA)),
                   c("<0.001", "0.04", "0.0496", "0.32", ">0.99", "NA"))
  expect_identical(sr_fmt_p(numeric(0)), character(0))
})

test_that("invalid inputs stop with a clear message", {
  expect_error(sr_fmt_num("a"), "numeric")
  expect_error(sr_fmt_num(1, mcse = -0.1), "mcse")
  expect_error(sr_fmt_num(1, mcse = Inf), "mcse")
  expect_error(sr_fmt_num(1, mcse = "a"), "mcse")
  expect_error(sr_fmt_num(c(1, 2, 3), mcse = c(0.1, 0.2)), "length")
  expect_error(sr_fmt_num(1, digits = -1L), "digits")
  expect_error(sr_mcse_digits(-1), "mcse")
  expect_error(sr_fmt_p(-0.1), "`p`")
  expect_error(sr_fmt_p(1.1), "`p`")
  expect_error(sr_fmt_p(Inf), "`p`")
  expect_error(sr_fmt_p("0.5"), "numeric")
  expect_error(sr_fmt_ci(1, 0, "x"), "numeric")
  expect_error(sr_fmt_ci(1, 0, 2, mcse = -1), "mcse")
})

test_that("zero-length input gives character(0)", {
  expect_identical(sr_fmt_num(numeric(0)), character(0))
  expect_identical(sr_fmt_p(numeric(0)), character(0))
  expect_identical(sr_fmt_ci(numeric(0), numeric(0), numeric(0)), character(0))
})

test_that("output uses '.' whatever options(OutDec) says", {
  old <- options(OutDec = ",")
  on.exit(options(old), add = TRUE)
  expect_identical(sr_fmt_num(1.5), "1.50")
  expect_identical(sr_fmt_num(-0.001), "0.00")
  expect_identical(sr_fmt_num(-0.4, mcse = 5), "0")
  expect_identical(sr_fmt_p(0.32), "0.32")
  expect_identical(sr_fmt_p(0.0496), "0.0496")
  expect_identical(sr_fmt_ci(0.4237, 0.301, 0.5799), "0.42 (0.30 to 0.58)")
  expect_identical(sr_fmt_ci(-0.001, -0.004, 0.002), "0.00 (0.00 to 0.00)")
})
