# Tests for hardware detection and profile caps (core/hardware.R).

no_override <- function(.env = parent.frame()) {
  withr::local_envvar(SR_PROFILE = NA, .local_envir = .env)
  withr::local_options(sr.profile = NULL, .local_envir = .env)
}

test_that("sr_profile picks locked16 for a CPU-only 16 GB box", {
  no_override()
  expect_identical(sr_profile(list(ram_gb = 15.7, gpu_vram_mb = NA)), "locked16")
})

test_that("sr_profile picks laptop for an 8 GB GPU with 24 GB RAM", {
  no_override()
  expect_identical(sr_profile(list(ram_gb = 23.7, gpu_vram_mb = 8188)), "laptop")
})

test_that("sr_profile thresholds: GPU alone or RAM alone is not enough", {
  no_override()
  expect_identical(sr_profile(list(ram_gb = 32, gpu_vram_mb = 4096)), "locked16")
  expect_identical(sr_profile(list(ram_gb = 16, gpu_vram_mb = 8188)), "locked16")
  expect_identical(sr_profile(list(ram_gb = 20, gpu_vram_mb = 6000)), "laptop")
  expect_identical(sr_profile(list(ram_gb = 32)), "locked16")
  expect_identical(sr_profile(list(ram_gb = 32, gpu_vram_mb = NULL)), "locked16")
})

test_that("SR_PROFILE overrides detection", {
  withr::local_options(sr.profile = NULL)
  withr::local_envvar(SR_PROFILE = "locked16")
  expect_identical(sr_profile(list(ram_gb = 23.7, gpu_vram_mb = 8188)), "locked16")
  withr::local_envvar(SR_PROFILE = "laptop")
  expect_identical(sr_profile(list(ram_gb = 15.7, gpu_vram_mb = NA)), "laptop")
})

test_that("an invalid override errors and names the valid values", {
  withr::local_options(sr.profile = NULL)
  withr::local_envvar(SR_PROFILE = "x")
  expect_error(sr_profile(list(ram_gb = 15.7, gpu_vram_mb = NA)), "locked16")
  expect_error(sr_profile(list(ram_gb = 15.7, gpu_vram_mb = NA)), "laptop")
})

test_that("an empty override means no override", {
  withr::local_options(sr.profile = NULL)
  withr::local_envvar(SR_PROFILE = "")
  expect_identical(sr_profile(list(ram_gb = 23.7, gpu_vram_mb = 8188)), "laptop")
})

test_that("caps table holds exactly", {
  l <- sr_profile_caps("locked16")
  expect_identical(names(l), c("workers", "ram_budget_gb", "duckdb_memory_gb", "duckdb_threads",
                               "model_tier", "gpu"))
  expect_equal(l$workers, 3)
  expect_equal(l$ram_budget_gb, 10)
  expect_equal(l$duckdb_memory_gb, 6)
  expect_equal(l$duckdb_threads, 4)
  expect_identical(l$model_tier, "von")
  expect_identical(l$gpu, FALSE)
  expect_true(is.integer(l$workers) && is.integer(l$duckdb_threads))

  p <- sr_profile_caps("laptop")
  expect_identical(names(p), names(l))
  expect_equal(p$workers, 8)
  expect_equal(p$ram_budget_gb, 18)
  expect_equal(p$duckdb_memory_gb, 14)
  expect_equal(p$duckdb_threads, 12)
  expect_identical(p$model_tier, "clm")
  expect_identical(p$gpu, TRUE)
})

test_that("an unknown profile errors in sr_profile_caps", {
  expect_error(sr_profile_caps("x"), "locked16")
  expect_error(sr_profile_caps(NA_character_))
  expect_error(sr_profile_caps(c("laptop", "locked16")))
})

test_that("sr_hw_detect returns the six fields", {
  hw <- sr_hw_detect()
  expect_named(hw, c("ram_gb", "cores", "gpu_name", "gpu_vram_mb", "blas", "os"))
  expect_true(is.numeric(hw$ram_gb) && hw$ram_gb > 0)
  expect_true(is.numeric(hw$cores) && length(hw$cores) == 1L)
  expect_true(is.character(hw$gpu_name) && length(hw$gpu_name) == 1L)
  expect_true(is.numeric(hw$gpu_vram_mb) && length(hw$gpu_vram_mb) == 1L)
  expect_identical(is.na(hw$gpu_name), is.na(hw$gpu_vram_mb))
  expect_true(is.character(hw$blas) && length(hw$blas) == 1L)
  expect_true(is.character(hw$os) && length(hw$os) == 1L)
})

test_that("sr_hw_detect is cached for the session", {
  a <- sr_hw_detect()
  b <- sr_hw_detect()
  expect_identical(a, b)
  expect_identical(sr_hw_detect(refresh = TRUE), a)
})

test_that("the fingerprint differs between profiles for the same hardware", {
  hw <- list(ram_gb = 23.7, cores = 12, gpu_name = "X", gpu_vram_mb = 8188,
             blas = "C:/R/bin/x64/Rblas.dll", os = "Windows")
  a <- sr_hw_fingerprint("locked16", hw)
  b <- sr_hw_fingerprint("laptop", hw)
  expect_match(a, "^[0-9a-f]{64}$")
  expect_false(identical(a, b))
  expect_identical(a, sr_hw_fingerprint("locked16", hw))
})

test_that("the fingerprint uses the BLAS basename and GPU presence only", {
  h1 <- list(blas = "C:/one/Rblas.dll", gpu_name = "Card A", ram_gb = 8)
  h2 <- list(blas = "D:/two/Rblas.dll", gpu_name = "Card B", ram_gb = 64)
  expect_identical(sr_hw_fingerprint("laptop", h1), sr_hw_fingerprint("laptop", h2))
  h3 <- list(blas = "C:/one/Rblas.dll", gpu_name = NA_character_, ram_gb = 8)
  expect_false(identical(sr_hw_fingerprint("laptop", h1), sr_hw_fingerprint("laptop", h3)))
})

test_that("sr_project_create works without a profile argument", {
  no_override()
  d <- file.path(withr::local_tempdir(), "proj")
  p <- sr_project_create(d, "x")
  on.exit(sr_project_close(p), add = TRUE)
  expect_true(p$meta$profile %in% c("locked16", "laptop"))
})
