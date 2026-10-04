# Tests for io/preflight.R: the space, time and network-share check that runs before an ingest.

GB <- 1024^3
MB <- 1024^2

# A sniff result stub: only the fields preflight reads. `size` fakes the on-disk size.
.pf_sniff <- function(path = "x.csv", size = 1e9, format = "csv", compression = "none", member = NA_character_) {
  list(path = path, format = format, compression = compression, zip_members = character(),
       member = member, size_bytes = size, warnings = character())
}

# drive_info stub: `free` for the project's data dir, `remote` for the source file.
.pf_is_data <- function(path) grepl("/data$", gsub(intToUtf8(92L), "/", path, fixed = TRUE))
.pf_drive <- function(free = 1e15, remote = FALSE, warnings = character()) {
  function(path) list(free_bytes = if (.pf_is_data(path)) free else 1e15,
                      remote = remote && !.pf_is_data(path), warnings = warnings)
}

.pf_project <- function(profile = "locked16", env = parent.frame()) {
  dir <- file.path(withr::local_tempdir(.local_envir = env), "proj")
  p <- sr_project_create(dir, "fixture", profile = profile)
  withr::defer(sr_project_close(p), envir = env)
  p
}

test_that("too little free space refuses with the need and the free GB", {
  p <- .pf_project()
  r <- sr_preflight(p, "x.csv", sniff = .pf_sniff(size = 5 * GB), drive_info = .pf_drive(free = 1 * GB))
  expect_false(r$ok)
  expect_match(r$refuse_reason, "insufficient space")
  expect_match(r$refuse_reason, "need [0-9.]+ GB, have 1 GB")
  expect_identical(r$free_bytes, 1 * GB)
  expect_true(r$need_bytes > r$free_bytes)
})

test_that("enough free space is ok with no refuse reason", {
  p <- .pf_project()
  r <- sr_preflight(p, "x.csv", sniff = .pf_sniff(size = 1 * GB), drive_info = .pf_drive(free = 100 * GB))
  expect_true(r$ok)
  expect_null(r$refuse_reason)
  expect_named(r, c("ok", "size_bytes", "uncompressed_bytes", "need_bytes", "free_bytes", "est_seconds",
                    "network_share", "external_suggested", "warnings", "refuse_reason"))
})

test_that("a remote source flags network_share and warns to copy it locally first", {
  p <- .pf_project()
  r <- sr_preflight(p, "x.csv", sniff = .pf_sniff(), drive_info = .pf_drive(remote = TRUE))
  expect_true(r$network_share)
  expect_true(any(grepl("copy", r$warnings, ignore.case = TRUE) & grepl("local", r$warnings, ignore.case = TRUE)))
  r2 <- sr_preflight(p, "x.csv", sniff = .pf_sniff(), drive_info = .pf_drive(remote = FALSE))
  expect_false(r2$network_share)
  expect_false(any(grepl("network", r2$warnings, ignore.case = TRUE)))
})

test_that("free space is read from the project data dir and the share flag from the source path", {
  p <- .pf_project()
  seen <- character()
  spy <- function(path) {
    seen <<- c(seen, path)
    list(free_bytes = 1e15, remote = FALSE)
  }
  sr_preflight(p, "some/src.csv", sniff = .pf_sniff(), drive_info = spy)
  expect_true(sr_project_paths(p$paths$dir)$data %in% seen)
  expect_true("some/src.csv" %in% seen)
})

test_that("external_suggested is TRUE above 2 GB and FALSE below", {
  p <- .pf_project()
  big <- sr_preflight(p, "x.csv", sniff = .pf_sniff(size = 3e9), drive_info = .pf_drive())
  small <- sr_preflight(p, "x.csv", sniff = .pf_sniff(size = 1 * GB), drive_info = .pf_drive())
  expect_true(big$external_suggested)
  expect_false(small$external_suggested)
})

test_that("need_bytes = parquet + spill + headroom (external) and adds the source copy otherwise", {
  p <- .pf_project("locked16")
  s <- .pf_sniff(size = 1e10)
  ext <- sr_preflight(p, "x.csv", sniff = s, drive_info = .pf_drive(), external = TRUE)
  expect_equal(ext$need_bytes, 3e9 + 6 * GB + 2 * GB)
  loc <- sr_preflight(p, "x.csv", sniff = s, drive_info = .pf_drive())
  expect_equal(loc$need_bytes, 3e9 + 6 * GB + 2 * GB + 1e10)
  # headroom is 5% of the size once that exceeds 2 GB; the laptop spill is 14 GB
  pl <- .pf_project("laptop")
  r <- sr_preflight(pl, "x.csv", sniff = .pf_sniff(size = 1e11), drive_info = .pf_drive(), external = TRUE)
  expect_equal(r$need_bytes, 0.30 * 1e11 + 14 * GB + 0.05 * 1e11)
})

test_that("est_seconds uses 60 MB/s on locked16 and 150 MB/s on laptop", {
  s <- .pf_sniff(size = 600 * MB)
  r1 <- sr_preflight(.pf_project("locked16"), "x.csv", sniff = s, drive_info = .pf_drive())
  r2 <- sr_preflight(.pf_project("laptop"), "x.csv", sniff = s, drive_info = .pf_drive())
  expect_equal(r1$est_seconds, 10)
  expect_equal(r2$est_seconds, 4)
})

test_that(".sr_drive_info on a real directory reports positive free bytes and is not remote", {
  d <- .sr_drive_info(tempdir())
  expect_true(is.numeric(d$free_bytes) && d$free_bytes > 0)
  expect_false(d$remote)
  # a path that does not exist yet falls back to its nearest existing parent
  d2 <- .sr_drive_info(file.path(tempdir(), "no", "such", "file.csv"))
  expect_true(d2$free_bytes > 0)
})

test_that(".sr_drive_info flags a UNC path as remote without probing it", {
  probed <- FALSE
  usage <- function(path) {
    probed <<- TRUE
    list(available = 1e12)
  }
  bs <- intToUtf8(92L)
  for (u in c(paste0(bs, bs, "server", bs, "share", bs, "a.csv"), "//server/share/a.csv")) {
    d <- .sr_drive_info(u, usage = usage)
    expect_true(d$remote)
  }
  expect_false(probed)
})

test_that(".sr_drive_info flags a partition whose options say remote (longest mountpoint wins)", {
  usage <- function(path) list(available = 5e9)
  dir <- normalizePath(tempdir(), winslash = "/")
  mk <- function(mounts, options) {
    function(all = TRUE) data.frame(device = mounts, mountpoint = mounts, fstype = "x",
                                    options = options, stringsAsFactors = FALSE)
  }
  # the temp dir sits on a local root and, in two cases, on a nested mount: the nested one wins
  root <- sub("/.*$", "/", dir)
  nested <- paste0(dir, "/")
  expect_false(.sr_drive_info(dir, usage = usage, partitions = mk(root, "rw,fixed"))$remote)
  expect_true(.sr_drive_info(dir, usage = usage, partitions = mk(c(root, nested), c("rw,fixed", "rw,remote")))$remote)
  expect_false(.sr_drive_info(dir, usage = usage, partitions = mk(c(root, nested), c("rw,remote", "rw,fixed")))$remote)
})

test_that("when ps fails free_bytes is NA with a warning and preflight refuses (fail closed)", {
  d <- .sr_drive_info(tempdir(), usage = function(path) stop("no ps"))
  expect_true(is.na(d$free_bytes))
  expect_true(length(d$warnings) >= 1L)
  p <- .pf_project()
  broken <- function(path) list(free_bytes = NA_real_, remote = FALSE, warnings = "ps failed")
  r <- sr_preflight(p, "x.csv", sniff = .pf_sniff(), drive_info = broken)
  expect_false(r$ok)
  expect_identical(r$refuse_reason, "cannot determine free space")
  expect_true(is.na(r$free_bytes))
  expect_true("ps failed" %in% r$warnings)
})

test_that("a zip source is sized by the exact uncompressed length of its member", {
  skip_if_not_installed("zip")
  d <- withr::local_tempdir()
  csv <- file.path(d, "data.csv")
  writeLines(c("a,b", rep("1,2", 1000)), csv)
  zp <- file.path(d, "data.zip")
  zip::zipr(zp, csv, mode = "cherry-pick")
  p <- .pf_project()
  s <- sr_sniff(zp)
  r <- sr_preflight(p, zp, sniff = s, drive_info = .pf_drive())
  expect_equal(r$uncompressed_bytes, as.numeric(file.size(csv)))
  expect_equal(r$size_bytes, as.numeric(file.size(zp)))
  expect_true(r$uncompressed_bytes > r$size_bytes)
})

test_that("gz, bz2 and xz sources are estimated from the sample ratio, floored at 3", {
  d <- withr::local_tempdir()
  p <- .pf_project()
  open_con <- function(path, cmp) switch(cmp, gz = gzfile(path, "wb"), bz2 = bzfile(path, "wb"), xz = xzfile(path, "wb"))
  set.seed(1)
  for (cmp in c("gz", "bz2", "xz")) {
    # highly compressible text: the ratio is well above 3, so the estimate exceeds 3 x the size
    rep_path <- file.path(d, paste0("rep.csv.", cmp))
    con <- open_con(rep_path, cmp)
    writeLines(c("a,b", rep("1,2", 5000)), con)
    close(con)
    r <- sr_preflight(p, rep_path, sniff = .pf_sniff(path = rep_path, size = 1e6, compression = cmp),
                      drive_info = .pf_drive())
    expect_true(r$uncompressed_bytes > 3 * 1e6)
    expect_equal(r$size_bytes, 1e6)
    # incompressible bytes: the raw ratio is about 1, so the floor of 3 applies
    noise_path <- file.path(d, paste0("noise.csv.", cmp))
    con <- open_con(noise_path, cmp)
    writeBin(as.raw(sample(0:255, 20000, replace = TRUE)), con)
    close(con)
    r <- sr_preflight(p, noise_path, sniff = .pf_sniff(path = noise_path, size = 1e6, compression = cmp),
                      drive_info = .pf_drive())
    expect_equal(r$uncompressed_bytes, 3e6)
  }
})

test_that("an XLSX over 200 MB is refused; one under is not", {
  p <- .pf_project()
  big <- sr_preflight(p, "x.xlsx", sniff = .pf_sniff(format = "xlsx", size = 201 * MB), drive_info = .pf_drive())
  expect_false(big$ok)
  expect_identical(big$refuse_reason, "XLSX too large: export to CSV")
  ok <- sr_preflight(p, "x.xlsx", sniff = .pf_sniff(format = "xlsx", size = 199 * MB), drive_info = .pf_drive())
  expect_true(ok$ok)
  xls <- sr_preflight(p, "x.xls", sniff = .pf_sniff(format = "xls", size = 300 * MB), drive_info = .pf_drive())
  expect_identical(xls$refuse_reason, "XLSX too large: export to CSV")
})

test_that("a JSON document over 512 MB is refused, NDJSON of that size is not", {
  p <- .pf_project()
  big <- sr_preflight(p, "x.json", sniff = .pf_sniff(format = "json", size = 513 * MB), drive_info = .pf_drive())
  expect_false(big$ok)
  expect_identical(big$refuse_reason, "convert to NDJSON")
  small <- sr_preflight(p, "x.json", sniff = .pf_sniff(format = "json", size = 100 * MB), drive_info = .pf_drive())
  expect_true(small$ok)
  nd <- sr_preflight(p, "x.ndjson", sniff = .pf_sniff(format = "ndjson", size = 600 * MB), drive_info = .pf_drive())
  expect_true(nd$ok)
})