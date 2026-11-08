test_that("full round trip at a different path: identical hashes and results", {
  s <- .fx_srproj("full")
  dest <- file.path(withr::local_tempdir(), "elsewhere", "imported")
  dir.create(dirname(dest))
  q <- sr_srproj_import(s$path, dest)
  withr::defer(sr_project_close(q))
  expect_false(identical(normalizePath(q$paths$dir), normalizePath(s$p$paths$dir)))
  expect_true(q$import$verify$ok); expect_length(q$import$verify$absent, 0L)
  expect_identical(sr_freezes(q)$manifest_sha, sr_freezes(s$p)$manifest_sha)
  ms <- sr_freeze_manifest(s$p, "submission-v1"); mq <- sr_freeze_manifest(q, "submission-v1")
  expect_identical(mq$files, ms$files)
  for (f in mq$files) expect_identical(sr_sha256_file(file.path(q$paths$dir, f$path)), f$sha256)
  expect_identical(sr_result(q, "a_lr"), sr_result(s$p, "a_lr"))
  expect_identical(q$meta$id, s$p$meta$id)
  expect_identical(q$state$srproj_imports[[1]]$file, basename(s$path))
  expect_true(file.exists(file.path(dest, "out", "freezes", "submission-v1", "srproj.json")))
  expect_false(file.exists(file.path(dest, "srproj.json")))
})

test_that("lean import: parts listed missing, results still readable, verify reports absent not problems", {
  s <- .fx_srproj("lean")
  q <- sr_srproj_import(s$path, file.path(withr::local_tempdir(), "lean"))
  withr::defer(sr_project_close(q))
  expect_identical(q$import$parts_missing, "t")
  expect_true(q$import$verify$ok); expect_gt(length(q$import$verify$absent), 0L)
  expect_identical(sr_result(q, "a_cox"), sr_result(s$p, "a_cox"))
})

test_that("tampered or hostile archives are refused and leave nothing behind", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  # 1. a frozen file changed inside the archive (index no longer matches)
  x <- file.path(root, "x"); dir.create(x); utils::untar(s$path, exdir = x, tar = "internal")
  fit <- list.files(file.path(x, "fits"), full.names = TRUE)[1]; cat("x", file = fit, append = TRUE)
  bad1 <- file.path(root, "bad1.srproj")
  withr::with_dir(x, utils::tar(bad1, list.files(".", recursive = TRUE, all.files = TRUE), tar = "internal", compression = "gzip"))
  expect_error(sr_srproj_import(bad1, file.path(root, "d1")), "does not match its index")
  # 2. a member the index does not list
  writeLines("x", file.path(x, "extra.txt"))
  bad2 <- file.path(root, "bad2.srproj")
  withr::with_dir(x, utils::tar(bad2, list.files(".", recursive = TRUE, all.files = TRUE), tar = "internal", compression = "gzip"))
  expect_error(sr_srproj_import(bad2, file.path(root, "d2")), "not listed in the index")
  # 3. a symlink member: refused before extraction
  bad3 <- file.path(root, "bad3.srproj")
  writeBin(c(.fx_tar_header("evil", type = "2", linkname = "/etc"), raw(1024)), bad3)
  expect_error(sr_srproj_import(bad3, file.path(root, "d3")), "link or special file")
  # 4. events.jsonl edited and the index re-hashed to match: the freeze verification catches it
  bad4 <- .fx_srproj_reindexed(s$path, root, edit = "events.jsonl")
  expect_error(sr_srproj_import(bad4, file.path(root, "d4")), "does not verify")
  for (d in paste0("d", 1:4)) {
    expect_false(dir.exists(file.path(root, d)))
    expect_false(dir.exists(file.path(root, paste0(d, ".partial"))))
  }
})

test_that("the target folder must be new or empty; the archive must be a .srproj of a known version", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  busy <- file.path(root, "busy"); dir.create(busy); writeLines("keep", file.path(busy, "a.txt"))
  expect_error(sr_srproj_import(s$path, busy), "not empty")
  expect_identical(readLines(file.path(busy, "a.txt")), "keep")
  expect_error(sr_srproj_import(s$path, s$p$paths$dir), "not empty")
  expect_error(sr_srproj_import(file.path(root, "none.srproj"), file.path(root, "n")), "not found")
  newer <- .fx_srproj_reindexed(s$path, root, index = list(format_version = 99L))
  expect_error(sr_srproj_import(newer, file.path(root, "v")), "newer StatRouter")
  empty <- file.path(root, "empty"); dir.create(empty)
  q <- sr_srproj_import(s$path, empty); sr_project_close(q)
  expect_true(file.exists(file.path(empty, "events.jsonl")))
})

.no_trace <- function(root, names) {
  for (d in names) {
    expect_false(dir.exists(file.path(root, d)))
    expect_false(dir.exists(file.path(root, paste0(d, ".partial"))))
    expect_false(dir.exists(file.path(root, paste0(d, ".partial.src"))))
  }
}

test_that("a broken audit chain or events past the freeze point are refused, leaving nothing", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  a1 <- .fx_srproj_reindexed(s$path, root, fn = function(x) {
    f <- file.path(x, "audit.log"); ln <- readLines(f, warn = FALSE)
    ln[2L] <- sub("\"ts\"[[:space:]]*:[[:space:]]*\"[^\"]*\"", "\"ts\":\"2000-01-01T00:00:00Z\"", ln[2L])
    writeBin(charToRaw(paste0(paste(ln, collapse = "\n"), "\n")), f)
  })
  expect_error(sr_srproj_import(a1, file.path(root, "d1")), "project history does not verify")
  a2 <- .fx_srproj_reindexed(s$path, root, fn = function(x) {
    sr_event_append(file.path(x, "events.jsonl"), file.path(x, "audit.log"), "srproj.imported", list())
  })
  expect_error(sr_srproj_import(a2, file.path(root, "d2")), "after the freeze point")
  a3 <- .fx_srproj_reindexed(s$path, root, fn = function(x) {
    ab <- readBin(file.path(x, "audit.log"), "raw", file.size(file.path(x, "audit.log")))
    sr_event_append(file.path(x, "events.jsonl"), file.path(x, "audit.log"), "srproj.imported", list())
    writeBin(ab, file.path(x, "audit.log"))
  })
  expect_error(sr_srproj_import(a3, file.path(root, "d3")), "after the freeze point")
  .no_trace(root, c("d1", "d2", "d3"))
})

test_that("lock, temp and pre-placed index members are refused", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  a1 <- .fx_srproj_reindexed(s$path, root, fn = function(x) writeLines("{}", file.path(x, ".lock")))
  expect_error(sr_srproj_import(a1, file.path(root, "d1")), "lock or temporary")
  a2 <- .fx_srproj_reindexed(s$path, root, fn = function(x) {
    writeLines("{}", file.path(x, "out", "freezes", "submission-v1", "srproj.json"))
  })
  expect_error(sr_srproj_import(a2, file.path(root, "d2")), "srproj.json")
  .no_trace(root, c("d1", "d2"))
})

test_that("the total size cap is enforced before extraction", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  withr::local_options(sr.srproj_max_total = 10)
  expect_error(sr_srproj_import(s$path, file.path(root, "d1")), "total size")
  .no_trace(root, "d1")
})

test_that("an archive that changes between the scan and the extraction is refused", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  other <- .fx_srproj_reindexed(s$path, root, edit = "events.jsonl")
  copy <- file.path(root, "copy.srproj"); file.copy(s$path, copy)
  real <- .sr_tar_check
  withr::defer(assign(".sr_tar_check", real, envir = globalenv()))
  own <- file.path(root, "d1.partial.src", "archive.srproj")   # the private copy is what gets extracted
  assign(".sr_tar_check", function(e) {
    r <- real(e); file.copy(other, own, overwrite = TRUE); r
  }, envir = globalenv())
  expect_error(sr_srproj_import(copy, file.path(root, "d1")), "changed while")
  .no_trace(root, "d1")
})

test_that("verify_data = FALSE, trailing slash, pre-existing empty folder, malformed index, missing member", {
  s <- .fx_srproj("full")
  root <- withr::local_tempdir()
  q <- sr_srproj_import(s$path, file.path(root, "a"), verify_data = FALSE)
  expect_false(q$readonly); sr_project_close(q)
  q <- sr_srproj_import(s$path, paste0(file.path(root, "b"), "/"))
  expect_true(dir.exists(file.path(root, "b"))); sr_project_close(q)
  expect_false(dir.exists(file.path(root, "b.partial")))
  m1 <- .fx_srproj_reindexed(s$path, root, index = list(files = "oops"))
  expect_error(sr_srproj_import(m1, file.path(root, "d1")), "malformed")
  m2 <- .fx_srproj_reindexed(s$path, root, index = function(idx) {
    idx$files[[length(idx$files) + 1L]] <- list(path = "ghost.txt", sha256 = strrep("0", 64), bytes = 1)
    idx
  })
  expect_error(sr_srproj_import(m2, file.path(root, "d2")), "does not match its index: ghost.txt")
  .no_trace(root, c("d1", "d2"))
})

test_that("hostile member names and a pre-existing .partial are refused before anything is made", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  mk <- function(path, ...) {
    con <- gzfile(path, "wb"); on.exit(close(con))
    writeBin(c(do.call(c, lapply(list(...), function(n) .fx_tar_header(n))), raw(1024)), con)
    path
  }
  expect_error(sr_srproj_import(mk(file.path(root, "h1.srproj"), "../x"), file.path(root, "d1")), "unsafe")
  expect_error(sr_srproj_import(mk(file.path(root, "h2.srproj"), "/abs"), file.path(root, "d2")), "unsafe")
  expect_error(sr_srproj_import(mk(file.path(root, "h3.srproj"), "a.txt", "A.txt"), file.path(root, "d3")), "duplicate")
  dir.create(file.path(root, "d4.partial"))
  expect_error(sr_srproj_import(s$path, file.path(root, "d4")), "already exists")
  expect_true(dir.exists(file.path(root, "d4.partial")))
  expect_false(dir.exists(file.path(root, "d4")))
  .no_trace(root, c("d1", "d2", "d3"))
})

test_that("a read-only open is refused, the project is closed and nothing is left", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  real_open <- sr_project_open; real_close <- sr_project_close
  closed <- 0L
  withr::defer({
    assign("sr_project_open", real_open, envir = globalenv())
    assign("sr_project_close", real_close, envir = globalenv())
  })
  assign("sr_project_open", function(...) { q <- real_open(...); q$readonly <- TRUE; q }, envir = globalenv())
  assign("sr_project_close", function(p) { closed <<- closed + 1L; real_close(p) }, envir = globalenv())
  expect_error(sr_srproj_import(s$path, file.path(root, "d1")), "could not be opened for writing")
  expect_identical(closed, 1L)
  .no_trace(root, "d1")
})

test_that("member-name refusals happen before the extraction folder is created", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  made <- function(label, fn) .fx_srproj_reindexed(s$path, root, fn = fn)
  a1 <- made("p", function(x) writeLines("x", file.path(x, "stray.partial")))
  a2 <- made("t", function(x) writeLines("x", file.path(x, "stray.tmp")))
  a3 <- made("o", function(x) {
    writeLines("{}", file.path(x, "out", "freezes", "submission-v1", "srproj.json"))
  })
  real_dc <- dir.create
  withr::defer(rm("dir.create", envir = globalenv()))
  assign("dir.create", function(path, ...) {
    # the private copy folder (<dir>.partial.src) is made first by design; the extraction folder is not
    if (grepl("[.]partial$", path)) stop("a .partial folder was created") else real_dc(path, ...)
  }, envir = globalenv())
  expect_error(sr_srproj_import(a1, file.path(root, "d1")), "lock or temporary")
  expect_error(sr_srproj_import(a2, file.path(root, "d2")), "lock or temporary")
  expect_error(sr_srproj_import(a3, file.path(root, "d3")), "already holds|srproj.json")
  expect_identical(list.files(root, pattern = "^d[0-9]"), character())
  .no_trace(root, c("d1", "d2", "d3"))
})

test_that("a failed rename leaves a pre-existing empty target folder in place", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  empty <- file.path(root, "empty"); dir.create(empty)
  real <- file.rename
  withr::defer(rm("file.rename", envir = globalenv()))
  assign("file.rename", function(from, to) {
    if (grepl("[.]partial$", from)) FALSE else real(from, to)
  }, envir = globalenv())
  expect_error(sr_srproj_import(s$path, empty), "could not move")
  expect_true(dir.exists(empty))
  expect_length(list.files(empty, all.files = TRUE, no.. = TRUE), 0L)
  expect_false(dir.exists(paste0(empty, ".partial")))
})

test_that("an archive swapped after the scan, or after the check, is refused", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  other <- .fx_srproj_reindexed(s$path, root, edit = "events.jsonl")
  for (fn in c(".sr_tar_entries", ".sr_tar_check")) {
    copy <- file.path(root, paste0(fn, ".srproj")); file.copy(s$path, copy)
    real <- get(fn)
    own <- file.path(root, paste0("d", fn, ".partial.src"), "archive.srproj")
    assign(fn, function(...) { r <- real(...); file.copy(other, own, overwrite = TRUE); r }, envir = globalenv())
    expect_error(sr_srproj_import(copy, file.path(root, paste0("d", fn))), "changed while")
    assign(fn, real, envir = globalenv())
    .no_trace(root, paste0("d", fn))
  }
})

test_that("verify_data = FALSE skips data files that verify_data = TRUE flags", {
  s <- .fx_srproj("full")
  root <- withr::local_tempdir()
  m <- sr_freeze_manifest(s$p, "submission-v1")
  part <- Filter(function(f) f$kind == "part", m$files)[[1]]$path
  bad <- .fx_srproj_reindexed(s$path, root, edit = part)
  expect_error(sr_srproj_import(bad, file.path(root, "d1")), "does not verify")
  .no_trace(root, "d1")
  q <- sr_srproj_import(bad, file.path(root, "d2"), verify_data = FALSE)
  withr::defer(sr_project_close(q))
  expect_true(q$import$verify$ok)
})

test_that("a member under a .lock folder is refused before any folder is made", {
  root <- withr::local_tempdir()
  f <- file.path(root, "l.srproj")
  con <- gzfile(f, "wb")
  writeBin(c(.fx_tar_header("srproj.json"), .fx_tar_header(".lock/x"), raw(1024)), con)
  close(con)
  expect_error(sr_srproj_import(f, file.path(root, "d1")), "lock or temporary")
  .no_trace(root, "d1")
})

# A gzip tar with a srproj.json member and a GNU L entry declaring size 1 whose padded block names `target`.
.fx_l_trick <- function(path, target) {
  body <- charToRaw(target)
  con <- gzfile(path, "wb"); on.exit(close(con))
  writeBin(c(.fx_tar_header("srproj.json", size = 2), charToRaw("{}"), raw(510),
             .fx_tar_header("././@LongLink", type = "L", size = 1), body, raw((512 - length(body) %% 512) %% 512),
             .fx_tar_header("x", size = 3), charToRaw("abc"), raw(509), raw(1024)), con)
  path
}

test_that("a GNU L name that untar2 reads differently is refused and nothing is written anywhere", {
  root <- withr::local_tempdir()
  # an absolute path without a drive letter: untar2 opens it as given (only its folder is sanitised)
  sentinel <- sub("^[A-Za-z]:", "", normalizePath(withr::local_tempdir(), winslash = "/"))
  a <- .fx_l_trick(file.path(root, "l.srproj"), paste0(sentinel, "/pwn.txt"))
  expect_error(sr_srproj_import(a, file.path(root, "d1")), "GNU long name")
  expect_length(list.files(sentinel, all.files = TRUE, no.. = TRUE), 0L)
  .no_trace(root, "d1")
})

test_that("extraction writes only the member names the scanner approved (defence in depth)", {
  root <- withr::local_tempdir()
  # an absolute path without a drive letter: untar2 opens it as given (only its folder is sanitised)
  sentinel <- sub("^[A-Za-z]:", "", normalizePath(withr::local_tempdir(), winslash = "/"))
  a <- .fx_l_trick(file.path(root, "l.srproj"), paste0(sentinel, "/pwn.txt"))
  # stand in for a scanner that misreads the L entry as "C", as the old one did
  real <- .sr_tar_entries
  withr::defer(assign(".sr_tar_entries", real, envir = globalenv()))
  assign(".sr_tar_entries", function(path) {
    data.frame(name = c("srproj.json", "C"), type = "0", size = c(2, 3), stringsAsFactors = FALSE)
  }, envir = globalenv())
  expect_error(sr_srproj_import(a, file.path(root, "d1")))
  expect_false(file.exists(file.path(sentinel, "pwn.txt")))
  expect_length(list.files(sentinel, all.files = TRUE, no.. = TRUE), 0L)
  .no_trace(root, "d1")
})

test_that("folder members named like a lock, partial or temp folder are refused", {
  root <- withr::local_tempdir()
  for (nm in c("a.partial/", "b.tmp/", "x/.lock/")) {
    f <- file.path(root, paste0(make.names(nm), ".srproj"))
    con <- gzfile(f, "wb")
    writeBin(c(.fx_tar_header("srproj.json"), .fx_tar_header(nm, type = "5"), raw(1024)), con)
    close(con)
    expect_error(sr_srproj_import(f, file.path(root, "d1")), "lock or temporary", info = nm)
  }
  .no_trace(root, "d1")
})

# srproj.json, then a folder member "d/" declaring size 512 whose "data" block is a whole link header
# (type `ltype`) naming `target_dir`/evil.txt: untar2 reads no data for a folder, so it reads that block as
# the next header.
.fx_hidden_link <- function(path, target_dir, ltype = "1") {
  hidden <- .fx_tar_header("evil.txt", type = ltype, linkname = "srproj.json", prefix = target_dir)
  con <- gzfile(path, "wb"); on.exit(close(con))
  writeBin(c(.fx_tar_header("srproj.json", size = 2), charToRaw("{}"), raw(510),
             .fx_tar_header("d/", type = "5", size = 512), hidden, raw(1024)), con)
  path
}

test_that("a folder member with data (a link header hidden in it) is refused and nothing is written", {
  root <- withr::local_tempdir()
  for (lt in c("1", "2")) {
    sentinel <- sub("^[A-Za-z]:", "", normalizePath(withr::local_tempdir(), winslash = "/"))
    a <- .fx_hidden_link(file.path(root, paste0("h", lt, ".srproj")), sentinel, lt)
    expect_error(suppressWarnings(sr_srproj_import(a, file.path(root, "d1"))), "with data", info = lt)
    expect_length(list.files(sentinel, all.files = TRUE, no.. = TRUE), 0L)
    .no_trace(root, "d1")
  }
})

test_that("an archive swapped for an evil one while untar runs only touches the caller's path", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  arch <- file.path(root, "a.srproj"); file.copy(s$path, arch)
  sentinel <- file.path(sub("^[A-Za-z]:", "", normalizePath(withr::local_tempdir(), winslash = "/")),
                        "sentinel.txt")
  evil <- file.path(root, "evil.bin")
  writeBin(c(.fx_tar_header("srproj.json", size = 2), charToRaw("{}"), raw(510),
             .fx_tar_header(sentinel, type = "1", linkname = "srproj.json"), raw(1024)), evil)
  good <- file.path(root, "good.bin"); file.copy(s$path, good)
  suppressMessages(trace("untar", where = asNamespace("utils"), print = FALSE,
                         tracer = bquote(file.copy(.(evil), .(arch), overwrite = TRUE)),
                         exit = bquote(file.copy(.(good), .(arch), overwrite = TRUE))))
  withr::defer(suppressMessages(untrace("untar", where = asNamespace("utils"))))
  q <- sr_srproj_import(arch, file.path(root, "d1"))
  sr_project_close(q)
  expect_false(file.exists(sentinel))
  expect_true(file.exists(file.path(root, "d1", "events.jsonl")))
  expect_false(dir.exists(file.path(root, "d1.partial")))
  expect_false(dir.exists(file.path(root, "d1.partial.src")))
  expect_false(any(grepl("srproj", list.files(file.path(root, "d1")))))   # the copy is not in the project
})

test_that("the archive changing before its private copy is made is refused; the copy goes on every path", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  other <- .fx_srproj_reindexed(s$path, root, edit = "events.jsonl")
  copy <- file.path(root, "copy.srproj"); file.copy(s$path, copy)
  real <- .sr_srproj_private_copy
  withr::defer(assign(".sr_srproj_private_copy", real, envir = globalenv()))
  assign(".sr_srproj_private_copy", function(archive, src) {
    file.copy(other, archive, overwrite = TRUE); real(archive, src)
  }, envir = globalenv())
  expect_error(sr_srproj_import(copy, file.path(root, "d1")), "changed while")
  .no_trace(root, "d1")
  assign(".sr_srproj_private_copy", real, envir = globalenv())
  q <- sr_srproj_import(s$path, file.path(root, "d2")); sr_project_close(q)
  expect_false(dir.exists(file.path(root, "d2.partial.src")))
  expect_error(sr_srproj_import(other, file.path(root, "d3")), "does not match|verify")
  .no_trace(root, "d3")
  dir.create(file.path(root, "d4.partial.src"))
  expect_error(sr_srproj_import(s$path, file.path(root, "d4")), "already exists")
  expect_false(dir.exists(file.path(root, "d4")))
})

test_that("an archive file larger than the cap plus the header allowance is refused before any copy", {
  root <- withr::local_tempdir()
  withr::local_options(sr.srproj_max_total = 0)
  big <- file.path(root, "big.srproj")
  writeBin(raw(SR_SRPROJ_HEADER_ALLOWANCE + 1), big)
  withr::defer(rm("dir.create", envir = globalenv()))
  assign("dir.create", function(path, ...) stop("a folder was created"), envir = globalenv())
  expect_error(sr_srproj_import(big, file.path(root, "d1")), "archive file is larger than the limit")
  .no_trace(root, "d1")
})

test_that("a staging folder another process made first is not removed", {
  s <- .fx_srproj("lean")
  root <- withr::local_tempdir()
  real_dc <- dir.create
  withr::defer(rm("dir.create", envir = globalenv()))
  for (sfx in c(".partial.src", ".partial")) {
    d <- file.path(root, paste0("r", nchar(sfx)))
    target <- paste0(d, sfx)
    assign("dir.create", function(path, ...) {
      if (identical(path, target)) {                     # someone else wins the race to this name
        real_dc(path); writeLines("theirs", file.path(path, "keep.txt")); return(FALSE)
      }
      real_dc(path, ...)
    }, envir = globalenv())
    expect_error(sr_srproj_import(s$path, d), paste0("could not create ", basename(target)), fixed = TRUE)
    expect_true(file.exists(file.path(target, "keep.txt")), info = sfx)
    expect_false(dir.exists(d))
  }
})
