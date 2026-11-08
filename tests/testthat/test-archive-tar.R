.tar_pad <- function(n) raw((512 - n %% 512) %% 512)
.tar_file <- function(..., end = TRUE) {
  f <- withr::local_tempfile(fileext = ".tar", .local_envir = parent.frame())
  writeBin(c(do.call(c, list(...)), if (end) raw(1024)), f)
  f
}

test_that(".sr_tar_entries lists files, folders and long names written by R's internal tar", {
  d <- withr::local_tempdir()
  long <- paste0("a/", strrep("b", 120), "/f.txt")              # > 100 bytes: prefix or GNU L
  withr::with_dir(d, {
    dir.create(dirname(long), recursive = TRUE); writeLines("x", long)
    writeBin(as.raw(c(0x79, 0x0a)), "top.txt")                  # "y\n", no CRLF translation
    utils::tar("t.tar.gz", c("top.txt", long), tar = "internal", compression = "gzip")
  })
  e <- .sr_tar_entries(file.path(d, "t.tar.gz"))
  expect_true(all(c("top.txt", long) %in% e$name))
  expect_identical(e$size[e$name == "top.txt"], 2)              # "y\n"
  expect_true(all(e$type %in% c("0", "5")))
  expect_silent(.sr_tar_check(e))
  # parity with the reference reader
  ex <- withr::local_tempdir()
  expect_setequal(e$name, utils::untar(file.path(d, "t.tar.gz"), list = TRUE))
})

test_that(".sr_archive_safe_path rejects traversal, absolute, drive, backslash and control names", {
  ok  <- c("events.jsonl", "data/store/t/part-0001.parquet", "out/exports/export-001/")
  bad <- c("", "../x", "a/../x", "/x", "C:/x", "a\134b", "a//b", "./a", paste0("a", "\001"), strrep("a", 256))
  expect_true(all(.sr_archive_safe_path(ok)))
  expect_false(any(.sr_archive_safe_path(bad)))
  expect_false(.sr_archive_safe_path("a\134b"))                   # a real backslash, not "\b"
  expect_identical(nchar("a\134b"), 3L)
})

test_that(".sr_archive_safe_path refuses Windows-hostile names and invalid UTF-8", {
  bad <- c("a./b", "a /b", "a.", "x/b ", "CON", "con.txt", "x/NUL", "x/nul.tar", "aux", "COM1", "x/lpt9.log",
           "PRN.x", "a:b", "x/f:stream", rawToChar(as.raw(c(0x61, 0xff, 0x62))))
  expect_warning(r <- .sr_archive_safe_path(bad), NA)
  expect_false(any(r))
  expect_true(all(.sr_archive_safe_path(c("console", "com10", "a.b/c.d", "lpt", "nullable.txt"))))
})

test_that(".sr_tar_check refuses links, unsafe names and duplicates; a corrupt header errors", {
  e <- data.frame(name = c("a", "b"), type = c("0", "2"), size = c(1, 0))
  expect_error(.sr_tar_check(e), "link or special file")
  expect_error(.sr_tar_check(data.frame(name = "../a", type = "0", size = 1)), "unsafe archive member")
  expect_error(.sr_tar_check(data.frame(name = c("a", "a"), type = "0", size = 1)), "duplicate")
  f <- withr::local_tempfile(fileext = ".tar")
  writeBin(as.raw(c(rep(65L, 512), rep(0L, 1024))), f)
  expect_error(.sr_tar_entries(f), "not a tar archive [(]bad header checksum at member 1[)]")
})

test_that(".sr_tar_check duplicates are case-, slash- and trailing-dot-insensitive and hashed", {
  dup <- function(n) data.frame(name = n, type = "0", size = 1)
  expect_error(.sr_tar_check(dup(c("a", "A"))), "duplicate")
  expect_error(.sr_tar_check(data.frame(name = c("a", "a/"), type = c("0", "5"), size = 0)), "duplicate")
  expect_error(.sr_tar_check(data.frame(name = c("d/x", "D/X"), type = "0", size = 1)), "duplicate")
  big <- data.frame(name = sprintf("f/%06d", 1:50000), type = "0", size = 1)
  expect_lt(system.time(.sr_tar_check(big))[["elapsed"]], 5)
})

test_that(".sr_tar_check refuses a file whose name ends in a slash and over-size members", {
  expect_error(.sr_tar_check(data.frame(name = "d/", type = "0", size = 0)), "unsafe archive member")
  expect_error(.sr_tar_check(data.frame(name = "a", type = "0", size = 2^33 + 1)), "size limit")
  expect_silent(.sr_tar_check(data.frame(name = "a", type = "0", size = 2^33)))
})

test_that("a symlink member written by a hand-built header is listed with type 2", {
  f <- withr::local_tempfile(fileext = ".tar")
  writeBin(c(.fx_tar_header("evil", type = "2", linkname = "/etc"), raw(1024)), f)
  e <- .sr_tar_entries(f)
  expect_identical(e$type, "2")
  expect_error(.sr_tar_check(e), "link or special file")
})

test_that("a GNU L entry is unlisted and names the next member", {
  nm <- paste0("dir/", strrep("n", 150), ".txt")
  body <- c(charToRaw(nm), as.raw(0L))
  f <- .tar_file(.fx_tar_header("././@LongLink", type = "L", size = length(body)), body, .tar_pad(length(body)),
                 .fx_tar_header("short", size = 3), charToRaw("abc"), .tar_pad(3),
                 .fx_tar_header("after"))
  e <- .sr_tar_entries(f)
  expect_identical(e$name, c(nm, "after"))
  expect_identical(e$size, c(3, 0))
})

test_that("a dangling GNU L entry at the end is truncated", {
  body <- c(charToRaw("zzz"), as.raw(0L))
  f <- .tar_file(.fx_tar_header("././@LongLink", type = "L", size = length(body)), body, .tar_pad(length(body)))
  expect_error(.sr_tar_entries(f), "truncated tar archive")
})

test_that("an oversized GNU long name is refused", {
  f <- .tar_file(.fx_tar_header("././@LongLink", type = "L", size = 100000))
  expect_error(.sr_tar_entries(f), "oversized GNU long name")
})

test_that("ustar prefix joins with / only for the ustar magic and 00 version", {
  f <- .tar_file(.fx_tar_header("leaf.txt", prefix = "some/dir"))
  expect_identical(.sr_tar_entries(f)$name, "some/dir/leaf.txt")
  f2 <- .tar_file(.fx_tar_header("leaf.txt", prefix = "some/dir", magic = "ustar ", version = " \001"))
  expect_identical(.sr_tar_entries(f2)$name, "leaf.txt")
  f3 <- .tar_file(.fx_tar_header("leaf.txt", prefix = "some/dir", version = "01"))
  expect_identical(.sr_tar_entries(f3)$name, "leaf.txt")
})

test_that("base-256 sizes decode; negative or huge ones are refused", {
  sz <- c(as.raw(0x80), raw(9), as.raw(c(0x01, 0x00)))          # 256
  f <- .tar_file(.fx_tar_header("big", size_raw = sz), raw(256), .tar_pad(256))
  expect_identical(.sr_tar_entries(f)$size, 256)
  neg <- c(as.raw(0xff), rep(as.raw(0xff), 11))
  expect_error(.sr_tar_entries(.tar_file(.fx_tar_header("n", size_raw = neg))), "bad size")
  huge <- c(as.raw(0xc0), raw(11))
  expect_error(.sr_tar_entries(.tar_file(.fx_tar_header("n", size_raw = huge))), "bad size")
})

test_that("truncated archives and truncated data regions error", {
  f <- .tar_file(.fx_tar_header("a", size = 10), raw(100), end = FALSE)
  expect_error(.sr_tar_entries(f), "truncated tar archive")
  f2 <- withr::local_tempfile(fileext = ".tar")
  writeBin(c(.fx_tar_header("a"), raw(100)), f2)                # short header block
  expect_error(.sr_tar_entries(f2), "truncated tar archive")
})

test_that("a checksum mismatch names the member index", {
  h <- .fx_tar_header("a"); h2 <- .fx_tar_header("b"); h2[1] <- as.raw(0x7a)
  expect_error(.sr_tar_entries(.tar_file(h, h2)), "bad header checksum at member 2")
})

test_that("a multi-MiB member is skipped and the next member is found", {
  n <- 3 * 1048576 + 17
  f <- .tar_file(.fx_tar_header("big.bin", size = n), raw(n), .tar_pad(n), .fx_tar_header("next"))
  e <- .sr_tar_entries(f)
  expect_identical(e$name, c("big.bin", "next"))
  expect_identical(e$size, c(n, 0))
})

test_that("more than 1e6 members is refused", {
  expect_error(.sr_tar_check(data.frame(name = rep("a", 1e6 + 1), type = "0", size = 0)), "too many")
})
