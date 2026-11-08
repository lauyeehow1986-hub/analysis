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
    # suppressWarnings: utils::tar notes that names over 100 bytes are not portable
    suppressWarnings(utils::tar("t.tar.gz", c("top.txt", long), tar = "internal", compression = "gzip"))
  })
  e <- .sr_tar_entries(file.path(d, "t.tar.gz"))
  expect_true(all(c("top.txt", long) %in% e$name))
  expect_identical(e$size[e$name == "top.txt"], 2)              # "y\n"
  expect_true(all(e$type %in% c("0", "5")))
  expect_silent(.sr_tar_check(e))
  # parity with the reference reader
  ex <- withr::local_tempdir()
  expect_setequal(e$name, utils::untar(file.path(d, "t.tar.gz"), list = TRUE, tar = "internal"))
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
  # a scaling check, not an absolute time bound (which flakes under load): linear cost gives a ratio near
  # 4; the floor on the denominator keeps a fast machine from dividing by timer noise
  tm <- function(n) {
    big <- data.frame(name = sprintf("f/%06d", seq_len(n)), type = "0", size = 1)
    min(vapply(1:3, function(i) system.time(.sr_tar_check(big))[["elapsed"]], 0))
  }
  t10 <- tm(10000); t40 <- tm(40000)
  expect_lt(t40 / max(t10, 0.05), 8)
})

test_that(".sr_tar_check reports the first offending row, whatever the check, as the row loop did", {
  e <- data.frame(name = c("ok", "../bad", "lnk", "big"), type = c("0", "0", "2", "0"), size = c(1, 1, 0, 2^34))
  expect_error(.sr_tar_check(e), "unsafe archive member name '../bad'", fixed = TRUE)
  expect_error(.sr_tar_check(e[c(1, 4, 3, 2), ]), "archive member 'big' exceeds the size limit", fixed = TRUE)
  expect_error(.sr_tar_check(e[c(3, 2), ]), "archive member 'lnk' is a link or special file (type 2)", fixed = TRUE)
  # one row bad on every count: the type is reported first, then the name, then the size
  expect_error(.sr_tar_check(data.frame(name = "../x", type = "2", size = 2^34)), "link or special file")
  expect_error(.sr_tar_check(data.frame(name = "../x", type = "0", size = 2^34)), "unsafe archive member")
  expect_error(.sr_tar_check(data.frame(name = "a", type = "0", size = NA_real_)), "exceeds the size limit")
})

# ---- divergence from utils:::untar2 (the extractor): names must be read exactly as it reads them ----

.sentinel_path <- function() {
  s <- withr::local_tempdir(.local_envir = parent.frame())
  normalizePath(s, winslash = "/")
}

.gnu_l <- function(body, sz = length(body)) {
  c(.fx_tar_header("././@LongLink", type = "L", size = sz), body, .tar_pad(length(body)))
}

# Crafted variants that each made the scanner and utils:::untar2 disagree on a member name.
.divergent_tars <- function(sentinel, env = parent.frame()) {
  .tar_file <- function(...) {
    f <- withr::local_tempfile(fileext = ".tar", .local_envir = env)
    writeBin(c(do.call(c, list(...)), raw(1024)), f)
    f
  }
  target <- paste0(sentinel, "/pwn.txt")
  list(
    # L with size 1: the scanner read "C", untar2 the whole block up to its last non-zero byte
    l_short_size = .tar_file(.gnu_l(charToRaw(target), sz = 1), .fx_tar_header("x", size = 3), charToRaw("abc"),
                             .tar_pad(3)),
    # L body with an embedded NUL then more bytes
    l_embedded_nul = .tar_file(.gnu_l(c(charToRaw("ok.txt"), as.raw(0L), charToRaw(target))),
                               .fx_tar_header("x", size = 3), charToRaw("abc"), .tar_pad(3)),
    # L naming a folder entry: untar2 keeps the long name for the next regular file
    l_then_dir = .tar_file(.gnu_l(c(charToRaw("evil.txt"), as.raw(0L))), .fx_tar_header("d/", type = "5"),
                           .fx_tar_header("f", size = 3), charToRaw("abc"), .tar_pad(3)),
    # an empty L body
    l_empty = .tar_file(.gnu_l(raw(0), sz = 0), .fx_tar_header("f")),
    # name field: bytes after the first NUL
    name_garbage = .tar_file(local({
      h <- .fx_tar_header("a")
      h[3:12] <- charToRaw("../../evil")
      h[149:156] <- as.raw(32L)
      ck <- sprintf("%06o", sum(as.integer(h)))
      h[149:156] <- c(charToRaw(ck), as.raw(0L), as.raw(32L))
      h
    })),
    # prefix field: bytes after the first NUL
    prefix_garbage = .tar_file(local({
      h <- .fx_tar_header("leaf", prefix = "p")
      h[348:357] <- charToRaw("../../evil")
      h[149:156] <- as.raw(32L)
      ck <- sprintf("%06o", sum(as.integer(h)))
      h[149:156] <- c(charToRaw(ck), as.raw(0L), as.raw(32L))
      h
    }))
  )
}

test_that("GNU L names, name and prefix fields are read as untar2 reads them, else refused", {
  s <- .sentinel_path()
  v <- .divergent_tars(s)
  msg <- c(l_short_size = "GNU long name", l_embedded_nul = "GNU long name", l_then_dir = "GNU long name",
           l_empty = "GNU long name", name_garbage = "name field", prefix_garbage = "prefix field")
  for (k in names(v)) expect_error(.sr_tar_entries(v[[k]]), msg[[k]], info = k)
  expect_length(list.files(s, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("scanner and utils::untar(list = TRUE, tar = \"internal\") agree on every member name, or the scanner refuses", {
  s <- .sentinel_path()
  nm <- paste0("dir/", strrep("n", 150), ".txt")
  ok <- list(
    long = .tar_file(.gnu_l(c(charToRaw(nm), as.raw(0L))), .fx_tar_header("short", size = 3), charToRaw("abc"),
                     .tar_pad(3)),
    long_exact = .tar_file(.gnu_l(charToRaw(nm)), .fx_tar_header("short")),
    prefix = .tar_file(.fx_tar_header("leaf.txt", prefix = "some/dir"), .fx_tar_header("d/", type = "5")))
  for (k in names(ok)) {
    expect_identical(.sr_tar_entries(ok[[k]])$name, suppressWarnings(utils::untar(ok[[k]], list = TRUE, tar = "internal")), info = k)
  }
  for (f in c(ok, .divergent_tars(s))) {
    sc <- tryCatch(.sr_tar_entries(f)$name, error = function(e) NULL)
    if (is.null(sc)) next
    ref <- tryCatch(suppressWarnings(utils::untar(f, list = TRUE, tar = "internal")), error = function(e) NULL)
    expect_identical(sc, ref)
  }
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
  # no prefix bytes: old GNU magic and odd versions are read as plain names
  f2 <- .tar_file(.fx_tar_header("leaf.txt", magic = "ustar "))
  expect_identical(.sr_tar_entries(f2)$name, "leaf.txt")
  f3 <- .tar_file(.fx_tar_header("leaf.txt", version = "01"))
  expect_identical(.sr_tar_entries(f3)$name, "leaf.txt")
})

test_that("a non-POSIX magic with a non-empty prefix field is ambiguous and refused", {
  f <- .tar_file(.fx_tar_header("leaf.txt", prefix = "../x", magic = "ustar "))
  expect_error(.sr_tar_entries(f), "ambiguous ustar prefix in member")
  f2 <- .tar_file(.fx_tar_header("leaf.txt", prefix = "../x", magic = ""))
  expect_error(.sr_tar_entries(f2), "ambiguous ustar prefix in member")
  f3 <- .tar_file(.fx_tar_header("leaf.txt", prefix = "../x", version = "01"))
  expect_error(.sr_tar_entries(f3), "ambiguous ustar prefix in member")
})

test_that("error messages are single-line and complete", {
  e <- data.frame(name = "x", type = "2", size = 0)
  expect_error(.sr_tar_check(e), "archive member 'x' is a link or special file (type 2)", fixed = TRUE)
  expect_error(.sr_tar_check(data.frame(name = "../x", type = "0", size = 1)),
               "unsafe archive member name '../x'", fixed = TRUE)
  expect_error(.sr_tar_check(data.frame(name = "x", type = "0", size = 2^34)),
               "archive member 'x' exceeds the size limit", fixed = TRUE)
  expect_error(.sr_tar_check(data.frame(name = c("x", "X"), type = "0", size = 1)),
               "duplicate archive member 'X'", fixed = TRUE)
})

test_that("console handles and superscript COM/LPT aliases are refused", {
  sup <- function(p, cp) intToUtf8(c(utf8ToInt(p), cp))
  bad <- c("CONIN$", "conout$", "x/CONIN$.txt", sup("COM", 0xb9), sup("com", 0xb2), sup("x/LPT", 0xb3), sup("LPT", 0xb2))
  expect_false(any(.sr_archive_safe_path(bad)))
  expect_true(all(.sr_archive_safe_path(c("conin", "CONIN2", "COM0", "comx"))))
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

test_that("device stems are matched after stripping trailing spaces, and CLOCK$ is refused", {
  bad <- c("CON .txt", "con  .x", "NUL .", "x/aux  .log", "CLOCK$", "clock$.txt")
  expect_false(any(.sr_archive_safe_path(bad)))
  expect_true(all(.sr_archive_safe_path(c("console.txt", "connect", "clock", "clocks.txt"))))
})

test_that("a non-file member with a non-zero size is refused before its data is skipped", {
  hid <- .fx_tar_header("evil.txt", type = "1", linkname = "x")
  for (t in c("5", "1", "2", "3", "4", "6", "x", "g", "K")) {
    f <- .tar_file(.fx_tar_header("d/", type = t, size = 512), hid)
    expect_error(.sr_tar_entries(f), "with data", info = t)
  }
  expect_identical(.sr_tar_entries(.tar_file(.fx_tar_header("d/", type = "5")))$name, "d/")
})

test_that("mode, mtime and magic fields untar2 cannot parse are refused", {
  fix <- function(h) {
    h[149:156] <- as.raw(32L)
    h[149:156] <- c(charToRaw(sprintf("%06o", sum(as.integer(h)))), as.raw(0L), as.raw(32L))
    h
  }
  h <- .fx_tar_header("a"); h[101] <- charToRaw("9")
  expect_error(.sr_tar_entries(.tar_file(fix(h))), "mode field")
  h <- .fx_tar_header("a"); h[137] <- charToRaw("z")
  expect_error(.sr_tar_entries(.tar_file(fix(h))), "mtime field")
  h <- .fx_tar_header("a"); h[260] <- as.raw(0L)                  # "us" NUL "ar": untar2 cannot read it
  expect_error(.sr_tar_entries(.tar_file(fix(h))), "magic field")
})

test_that("randomized parity: whenever the scan and check pass, untar2 lists exactly the scanner names", {
  set.seed(20261108)
  pool <- c("a.txt", "d/", "d/b.txt", "x/y/z.csv", paste0(strrep("p", 60), "/", strrep("q", 60), ".txt"))
  uid <- 0L
  hdr <- function() {
    t <- sample(c("0", "0", "0", "0", "5", "5", "1", "2", "3", "6", "x", "g", "K"), 1)
    sz <- if (t %in% c("0", "L") || runif(1) < 0.3) sample(c(0, 1, 3, 511, 512, 700), 1) else 0
    uid <<- uid + 1L
    nm <- paste0("m", uid, "/", sample(pool, 1))   # unique, so duplicates do not mask the comparison
    pre <- ""
    if (nchar(nm) > 100) { pre <- dirname(nm); nm <- basename(nm) }
    h <- .fx_tar_header(nm, type = t, size = sz, prefix = pre, linkname = if (t %in% c("1", "2")) "a.txt" else "")
    data <- if (sz > 0) {
      if (runif(1) < 0.3) c(.fx_tar_header("hidden.txt", type = sample(c("0", "1"), 1)), raw(0))[seq_len(min(sz, 512))]
      else as.raw(sample(65:90, sz, replace = TRUE))
    } else raw(0)
    if (length(data) < sz) data <- c(data, raw(sz - length(data)))
    c(h, data, .tar_pad(sz))
  }
  lhdr <- function() {
    nm <- paste0("long/", strrep("n", sample(90:160, 1)), ".txt")
    body <- charToRaw(nm)
    if (runif(1) < 0.3) body <- c(body, as.raw(0L), charToRaw("tail"))
    sz <- if (runif(1) < 0.3) sample(seq_along(body), 1) else length(body)
    c(.fx_tar_header("././@LongLink", type = "L", size = sz), body, .tar_pad(length(body)))
  }
  checked <- 0L
  for (k in 1:400) {
    parts <- lapply(seq_len(sample(1:4, 1)), function(i) if (runif(1) < 0.2) c(lhdr(), hdr()) else hdr())
    f <- tempfile(fileext = ".tar")
    writeBin(c(do.call(c, parts), raw(1024)), f)
    sc <- tryCatch({ e <- .sr_tar_entries(f); .sr_tar_check(e); e$name }, error = function(e) NULL)
    if (!is.null(sc)) {
      checked <- checked + 1L
      ref <- tryCatch(suppressWarnings(utils::untar(f, list = TRUE, tar = "internal")), error = function(e) NULL)
      expect_identical(sc, ref, info = paste("variant", k))
    }
    unlink(f)
  }
  expect_gt(checked, 40L)                                 # 49 with this seed
})
