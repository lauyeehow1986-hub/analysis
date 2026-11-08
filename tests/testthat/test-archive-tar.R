test_that(".sr_tar_entries lists files, folders and long names written by R's internal tar", {
  d <- withr::local_tempdir()
  long <- paste0("a/", strrep("b", 120), "/f.txt")              # > 100 bytes: prefix or GNU L
  withr::with_dir(d, {
    dir.create(dirname(long), recursive = TRUE); writeLines("x", long); writeBin(charToRaw("y
"), "top.txt")
    utils::tar("t.tar.gz", c("top.txt", long), tar = "internal", compression = "gzip")
  })
  e <- .sr_tar_entries(file.path(d, "t.tar.gz"))
  expect_true(all(c("top.txt", long) %in% e$name))
  expect_identical(e$size[e$name == "top.txt"], 2)              # "y\n"
  expect_true(all(e$type %in% c("0", "5")))
  expect_silent(.sr_tar_check(e))
})

test_that(".sr_archive_safe_path rejects traversal, absolute, drive, backslash and control names", {
  ok  <- c("events.jsonl", "data/store/t/part-0001.parquet", "out/exports/export-001/")
  bad <- c("", "../x", "a/../x", "/x", "C:/x", "a\b", "a//b", "./a", paste0("a", "\001"), strrep("a", 256))
  expect_true(all(.sr_archive_safe_path(ok)))
  expect_false(any(.sr_archive_safe_path(bad)))
})

test_that(".sr_tar_check refuses links, unsafe names and duplicates; a corrupt header errors", {
  e <- data.frame(name = c("a", "b"), type = c("0", "2"), size = c(1, 0))
  expect_error(.sr_tar_check(e), "link or special file")
  expect_error(.sr_tar_check(data.frame(name = "../a", type = "0", size = 1)), "unsafe archive member")
  expect_error(.sr_tar_check(data.frame(name = c("a", "a"), type = "0", size = 1)), "duplicate")
  f <- withr::local_tempfile(fileext = ".tar")
  writeBin(as.raw(c(rep(65L, 512), rep(0L, 1024))), f)
  expect_error(.sr_tar_entries(f), "not a tar archive")
})

test_that("a symlink member written by a hand-built header is listed with type 2", {
  f <- withr::local_tempfile(fileext = ".tar")
  writeBin(c(.fx_tar_header("evil", type = "2", linkname = "/etc"), raw(1024)), f)
  e <- .sr_tar_entries(f)
  expect_identical(e$type, "2")
  expect_error(.sr_tar_check(e), "link or special file")
})
