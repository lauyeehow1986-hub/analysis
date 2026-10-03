# Source the app core once per test run, from the repo root.
local({
  root <- normalizePath(testthat::test_path("../.."), winslash = "/")
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  source("app/global.R", local = FALSE)
})
