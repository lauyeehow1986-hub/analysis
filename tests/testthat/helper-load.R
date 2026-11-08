# Source the app core once per test run, from the repo root.
local({
  root <- normalizePath(testthat::test_path("../.."), winslash = "/")
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  source("app/global.R", local = FALSE)
})

# Record a role, not the OS account name, as the actor of every test event (sr_srproj_export warns when
# the exported history holds the OS user name). A test that needs the OS name sets sr.user locally.
options(sr.user = "sr-test")
