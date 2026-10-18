# Tests for writer/docx.R: the Word rendering of the document model (officer + flextable, no pandoc).
# Synthetic data only (the writer fixture of helper-writer.R). The model is built once per file; the
# renders go to a folder under tempdir().

.dx_cache <- new.env(parent = emptyenv())

.dx_props <- function(tip = "a1b2c3d4e5f60718293a4b5c6d7e8f90") {
  list(audit_tip = tip, sap_hash = "f5bdde8442476060aabbccddeeff0011", export_id = "export-001",
       created = "2026-10-18T09:30:00Z")
}

# The fixture model, built once (the project is released when the builder returns; the model is plain data).
.dx_model <- function() {
  if (is.null(.dx_cache$model)) {
    build <- function() {
      p <- .fx_writer_project(environment())
      sr_doc_model(p)
    }
    .dx_cache$model <- build()
    .dx_cache$dir <- tempfile("dx-render-")
    dir.create(.dx_cache$dir)
  }
  .dx_cache$model
}

# The full fixture model rendered once per part.
.dx_render <- function(part) {
  m <- .dx_model()
  key <- paste0("path_", part)
  if (is.null(.dx_cache[[key]])) {
    .dx_cache[[key]] <- sr_docx_render(m, part, file.path(.dx_cache$dir, paste0(part, ".docx")),
                                       file.path(.dx_cache$dir, paste0("fig-", part)), .dx_props())
  }
  .dx_cache[[key]]
}

# A copy of a registry (the registry is an environment).
.dx_reg_copy <- function(reg) {
  r <- sr_ph_registry(reg$k)
  r$rows <- reg$rows
  r
}

# A small model over the fixture's blocks: a heading, a paragraph, the main-results table, one Kaplan-Meier
# figure and the reference list; the supplement keeps only the given blocks. The registry is a copy.
.dx_mini <- function(supp = list(), preset = NULL) {
  m <- .dx_model()
  pick <- function(type, id = NULL) {
    Filter(function(b) identical(b$type, type) && (is.null(id) || identical(b$id, id)), m$main)
  }
  m$reg <- .dx_reg_copy(m$reg)
  m$main <- c(pick("heading")[1L], pick("para", "methods_lock"), pick("table", "main_results"),
              pick("figure", "km_a_cox"), pick("refs"))
  m$supplement <- supp
  if (!is.null(preset)) m$meta$preset <- preset
  m
}

.dx_unzip <- function(path) {
  d <- tempfile("dx-unz-")
  dir.create(d)
  zip::unzip(path, exdir = d)
  d
}

.dx_xml <- function(path, part) xml2::read_xml(file.path(.dx_unzip(path), part))

.dx_text <- function(path) {
  s <- officer::docx_summary(officer::read_docx(path), remove_fields = TRUE)
  s$text[!is.na(s$text)]
}

test_that("journal presets hold the plan's values and an unknown name lists the known ones", {
  d <- sr_journal_preset("default")
  expect_identical(names(d), c("name", "font", "size_pt", "fig_width_mm", "fig_height_mm", "dpi", "verified"))
  expect_identical(d$font, "Calibri")
  expect_equal(c(d$size_pt, d$fig_width_mm, d$fig_height_mm, d$dpi), c(11, 170, 120, 300))
  b <- sr_journal_preset("bmj")
  expect_equal(c(b$fig_width_mm, b$dpi), c(171, 300))
  expect_identical(b$font, "Arial")
  l <- sr_journal_preset("lancet")
  expect_identical(l$font, "Times New Roman")
  expect_equal(l$size_pt, 12)
  j <- sr_journal_preset("jama")
  expect_equal(c(j$size_pt, j$fig_width_mm, j$dpi), c(12, 178, 600))
  for (nm in c("default", "bmj", "lancet", "jama")) expect_false(sr_journal_preset(nm)$verified)
  expect_error(sr_journal_preset("nope"), "default, bmj, lancet, jama")
  expect_error(sr_journal_preset(c("a", "b")), "single")
  # the shape sr_wr_figure_render() reads
  expect_true(all(c("fig_width_mm", "fig_height_mm", "dpi") %in% names(sr_journal_preset("jama"))))
  # the file is ASCII
  expect_false(any(grepl("[^ -~]", readLines(.sr_journal_path(), warn = FALSE))))
})

test_that("a rendered document is read back by officer, with unique bookmarks that hold the registry text", {
  m <- .dx_model()
  for (part in c("main", "supplement")) {
    f <- .dx_render(part)
    expect_true(file.exists(f))
    expect_s3_class(officer::read_docx(f), "rdocx")
    bk <- sr_docx_bookmarks(f)
    expect_s3_class(bk, "data.frame")
    expect_identical(names(bk), c("name", "text"))
    expect_gt(nrow(bk), 0L)
    expect_false(anyDuplicated(bk$name) > 0L)
    expect_true(all(grepl("^sr_E[0-9]+$", bk$name)))
    expect_true(all(nchar(bk$name) <= 40L))
    tab <- sr_ph_table(m$reg)
    id <- sub("^sr_", "", bk$name)
    expect_true(all(id %in% tab$id))
    expect_identical(bk$text, tab$text[match(id, tab$id)])
  }
  # the main document bookmarks the numbers of the main-results table (a cell holds E tokens)
  bm <- sr_docx_bookmarks(.dx_render("main"))
  mr <- Filter(function(b) identical(b$id, "main_results"), m$main)[[1L]]
  toks <- unlist(regmatches(mr$df[[5L]], gregexpr("E[0-9]+", mr$df[[5L]])))
  expect_true(all(paste0("sr_", toks) %in% bm$name))
})

test_that("a suppressed count is bookmarked with the registry text, not a hard-coded one", {
  m <- .dx_mini()
  k <- 7L
  reg <- sr_ph_registry(k)
  tok <- sr_ph_put(reg, "<7", "count", "label:suppressed-demo", value = NA_real_, suppressed = TRUE)
  other <- sr_ph_put(reg, "12", "count", "label:plain-demo", value = 12)
  m$reg <- reg
  m$main <- list(.sr_wr_para("p1", paste0("Rows: ", tok, " and ", other, " and ", tok, " again"), "a", "results"))
  f <- sr_docx_render(m, "main", tempfile(fileext = ".docx"), tempfile("fig-"), .dx_props())
  bk <- sr_docx_bookmarks(f)
  # the repeat of a token is a plain run: the bookmark name stays unique
  expect_identical(bk$name, sub("\\{\\{", "sr_", sub("\\}\\}", "", c(tok, other))))
  expect_identical(bk$text, c("<7", "12"))
  expect_true(any(grepl("Rows: <7 and 12 and <7 again", .dx_text(f), fixed = TRUE)))
})

test_that("captions read Table 1. / Figure 1. / Table S1. and carry SEQ fields with the number cached", {
  tx <- .dx_text(.dx_render("main"))
  expect_true(any(startsWith(tx, "Table 1. ")))
  expect_true(any(startsWith(tx, "Table 2. ")))
  expect_true(any(startsWith(tx, "Figure 1. ")))
  expect_true(any(startsWith(tx, "Figure 5. ")))
  sx <- .dx_text(.dx_render("supplement"))
  expect_true(any(startsWith(sx, "Table S1. ")))
  expect_true(any(startsWith(sx, "Table S2. ")))
  expect_false(any(startsWith(sx, "Table 1. ")))
  seq_of <- function(path, name) {
    doc <- .dx_xml(path, "word/document.xml")
    ns <- xml2::xml_ns(doc)
    ins <- xml2::xml_find_all(doc, "//w:r[w:instrText]", ns)
    txt <- xml2::xml_text(ins)
    mine <- ins[trimws(txt) == paste0("SEQ ", name, " \\* ARABIC")]
    list(n = length(mine),
         cached = vapply(mine, function(r) {
           xml2::xml_text(xml2::xml_find_first(r, "following-sibling::w:r[2]/w:t", ns))
         }, ""),
         sep = vapply(mine, function(r) {
           xml2::xml_attr(xml2::xml_find_first(r, "following-sibling::w:r[1]/w:fldChar", ns), "w:fldCharType", ns)
         }, ""),
         begin = vapply(mine, function(r) {
           xml2::xml_attr(xml2::xml_find_first(r, "preceding-sibling::w:r[1]/w:fldChar", ns), "w:fldCharType", ns)
         }, ""),
         end = vapply(mine, function(r) {
           xml2::xml_attr(xml2::xml_find_first(r, "following-sibling::w:r[3]/w:fldChar", ns), "w:fldCharType", ns)
         }, ""))
  }
  m <- .dx_model()
  n_tab <- sum(vapply(m$main, function(b) identical(b$type, "table"), NA))
  n_fig <- sum(vapply(m$main, function(b) identical(b$type, "figure"), NA))
  t1 <- seq_of(.dx_render("main"), "Table")
  expect_identical(t1$cached, as.character(seq_len(n_tab)))
  expect_true(all(t1$begin == "begin") && all(t1$sep == "separate") && all(t1$end == "end"))
  f1 <- seq_of(.dx_render("main"), "Figure")
  expect_identical(f1$cached, as.character(seq_len(n_fig)))
  n_stab <- sum(vapply(m$supplement, function(b) identical(b$type, "table"), NA))
  ts <- seq_of(.dx_render("supplement"), "TableS")
  expect_identical(ts$cached, as.character(seq_len(n_stab)))
  expect_identical(seq_of(.dx_render("supplement"), "Table")$n, 0L)
  # the instruction is the plain SEQ field
  xml <- paste(readLines(file.path(.dx_unzip(.dx_render("main")), "word", "document.xml"), warn = FALSE), collapse = "")
  expect_true(grepl("SEQ Table \\* ARABIC", xml, fixed = TRUE))
  expect_true(grepl("SEQ Figure \\* ARABIC", xml, fixed = TRUE))
  # no officer-only attribute is left in the package part
  expect_false(grepl("w:officer", xml, fixed = TRUE))
  # [Content_Types].xml comes first in the archive
  expect_identical(zip::zip_list(.dx_render("main"))$filename[1L], "[Content_Types].xml")
})

test_that("the audit properties, the footer and the creator are written, and no user name leaks", {
  f <- .dx_render("main")
  pr <- .dx_props()
  d <- .dx_unzip(f)
  custom <- paste(readLines(file.path(d, "docProps", "custom.xml"), warn = FALSE), collapse = "")
  expect_match(custom, sprintf('name="sr_audit_tip"><vt:lpwstr>%s<', pr$audit_tip))
  expect_match(custom, sprintf('name="sr_sap_hash"><vt:lpwstr>%s<', pr$sap_hash))
  expect_match(custom, 'name="sr_export_id"><vt:lpwstr>export-001<')
  ft <- list.files(file.path(d, "word"), pattern = "^footer[0-9]*[.]xml$", full.names = TRUE)
  expect_gt(length(ft), 0L)
  foot <- paste(unlist(lapply(ft, readLines, warn = FALSE)), collapse = "")
  expect_true(grepl(substr(pr$audit_tip, 1L, 16L), foot, fixed = TRUE))
  expect_false(grepl(substr(pr$audit_tip, 1L, 17L), foot, fixed = TRUE))
  expect_true(grepl(sprintf("StatRouter export export-001 | SAP %s | audit %s", substr(pr$sap_hash, 1L, 12L),
                            substr(pr$audit_tip, 1L, 16L)), foot, fixed = TRUE))
  dp <- officer::doc_properties(officer::read_docx(f))
  expect_identical(dp$value[dp$tag == "creator"], "StatRouter")
  expect_identical(dp$value[dp$tag == "title"], "Manuscript")
  expect_identical(dp$value[dp$tag == "created"], "2026-10-18T09:30:00Z")
  dps <- officer::doc_properties(officer::read_docx(.dx_render("supplement")))
  expect_identical(dps$value[dps$tag == "title"], "Supplement")
  # the OS user name is in no part of the package
  user <- Sys.getenv("USERNAME", Sys.getenv("USER", ""))
  if (nzchar(user) && nchar(user) > 3L) {
    for (p in list.files(d, pattern = "[.](xml|rels)$", recursive = TRUE, full.names = TRUE, all.files = TRUE)) {
      expect_false(grepl(tolower(user), tolower(paste(readLines(p, warn = FALSE), collapse = "")), fixed = TRUE), info = p)
    }
  }
})

test_that("a label with & < and an accented letter renders and reads back unchanged", {
  m <- .dx_mini()
  hostile <- "R&D <b>level</b> café"
  tok <- sr_ph_put(m$reg, hostile, "label", "label:hostile-demo")
  m$main <- c(list(.sr_wr_head(paste0("Heading ", tok), 1L)),
              list(.sr_wr_para("p_h", paste0("Level ", tok, " ends."), "a", "results")),
              list(list(type = "table", id = "t_h", caption = paste0("Caption of ", tok), df = data.frame(
                A = c(paste0("Cell ", tok), "plain"), B = c("x", "y"), check.names = FALSE, stringsAsFactors = FALSE),
                footnotes = paste0("Note on ", tok), analysis = "a", where = "main", n_suppressed = 0L)),
              m$main[vapply(m$main, function(b) identical(b$type, "refs"), NA)])
  names(m$main[[3L]]$df)[1L] <- paste0("Col ", tok)
  f <- sr_docx_render(m, "main", tempfile(fileext = ".docx"), tempfile("fig-"), .dx_props())
  tx <- .dx_text(f)
  expect_true(paste0("Heading ", hostile) %in% tx)
  expect_true(paste0("Level ", hostile, " ends.") %in% tx)
  expect_true(paste0("Table 1. Caption of ", hostile) %in% tx)
  expect_true(paste0("Cell ", hostile) %in% tx)
  expect_true(paste0("Col ", hostile) %in% tx)
  expect_true(paste0("Note on ", hostile) %in% tx)
  # the characters are in the XML as entities, not as markup
  xml <- paste(readLines(file.path(.dx_unzip(f), "word", "document.xml"), warn = FALSE, encoding = "UTF-8"), collapse = "")
  expect_false(grepl("<b>level</b>", xml, fixed = TRUE))
  # a text that would pass for a marker is refused
  tok2 <- sr_ph_put(m$reg, "@@SR:SEQ:Table@@", "label", "label:marker-demo")
  m$main[[2L]]$text <- tok2
  expect_error(sr_docx_render(m, "main", tempfile(fileext = ".docx"), tempfile("fig-"), .dx_props()), "reserved marker")
})

test_that("the digest is stable across renders and changes with the content", {
  m <- .dx_mini()
  render <- function(model) {
    sr_docx_render(model, "main", tempfile(fileext = ".docx"), tempfile("fig-"), .dx_props())
  }
  a <- render(m)
  Sys.sleep(1.1)
  b <- render(m)
  expect_match(sr_docx_digest(a), "^[0-9a-f]{64}$")
  expect_identical(sr_docx_digest(a), sr_docx_digest(b))
  # the media part is named by its bytes: the same figure gives the same name
  media <- function(p) sort(grep("^word/media/", zip::zip_list(p)$filename, value = TRUE))
  expect_length(media(a), 1L)
  # one placeholder's text changes the digest
  bk <- sr_docx_bookmarks(a)
  id <- sub("^sr_", "", bk$name[1L])
  m2 <- m
  m2$reg <- .dx_reg_copy(m$reg)
  m2$reg$rows$text[m2$reg$rows$id == id] <- paste0(m2$reg$rows$text[m2$reg$rows$id == id], "x")
  expect_false(identical(sr_docx_digest(a), sr_docx_digest(render(m2))))
  # a change of the export id (footer, property) changes it too
  pr <- .dx_props()
  pr$export_id <- "export-002"
  c2 <- sr_docx_render(m, "main", tempfile(fileext = ".docx"), tempfile("fig-"), pr)
  expect_false(identical(sr_docx_digest(a), sr_docx_digest(c2)))
  # but the creation time does not
  pr2 <- .dx_props()
  pr2$created <- "2027-01-01T00:00:00Z"
  d2 <- sr_docx_render(m, "main", tempfile(fileext = ".docx"), tempfile("fig-"), pr2)
  expect_identical(sr_docx_digest(a), sr_docx_digest(d2))
  expect_error(sr_docx_digest(tempfile(fileext = ".docx")), "does not exist")
})

test_that("the preset sets the figure resolution: jama gives round(178 / 25.4 * 600) pixels", {
  m <- .dx_mini(preset = "jama")
  fd <- tempfile("fig-")
  f <- sr_docx_render(m, "main", tempfile(fileext = ".docx"), fd, .dx_props())
  expect_true(file.exists(f))
  px <- .sr_dx_png_size(file.path(fd, "km_a_cox.png"))
  expect_identical(unname(px[["w"]]), round(178 / 25.4 * 600))
  # the same figure at the default preset
  fd2 <- tempfile("fig-")
  sr_docx_render(.dx_mini(), "main", tempfile(fileext = ".docx"), fd2, .dx_props())
  expect_identical(unname(.sr_dx_png_size(file.path(fd2, "km_a_cox.png"))[["w"]]), round(170 / 25.4 * 300))
  # an unknown preset in the model is the preset error
  bad <- .dx_mini(preset = "nope")
  expect_error(sr_docx_render(bad, "main", tempfile(fileext = ".docx"), tempfile("fig-"), .dx_props()), "known presets")
})

test_that("the references section lists one line per citation key", {
  m <- .dx_model()
  tx <- officer::docx_summary(officer::read_docx(.dx_render("main")))
  tx <- tx[tx$content_type == "paragraph" & !is.na(tx$text), ]
  at <- which(tx$text == "References")
  expect_length(at, 1L)
  expect_match(tx$style_name[at], "heading 1", fixed = TRUE)
  lines <- tx$text[(at + 1L):nrow(tx)]
  expect_length(lines, length(sr_cite_order(m)))
  expect_identical(sub("^([0-9]+)\\. .*$", "\\1", lines), as.character(seq_along(lines)))
  # the citations of the text use the same numbers
  expect_true(any(grepl("\\[[0-9]+\\]", tx$text[seq_len(at - 1L)])))
})

test_that("the checklist is a coloured table under a STROBE checklist heading", {
  ck <- data.frame(item = c("1a", "1b", "2"), section = "S", label = c("a", "b", "c"),
                   status = c("green", "amber", "red"), detail = c("found", "for the authors to write", "missing"),
                   stringsAsFactors = FALSE)
  m <- .dx_mini(supp = list(list(type = "checklist", id = "strobe", df = ck)))
  f <- sr_docx_render(m, "supplement", tempfile(fileext = ".docx"), tempfile("fig-"), .dx_props())
  s <- officer::docx_summary(officer::read_docx(f))
  hd <- s[s$content_type == "paragraph" & s$text %in% "STROBE checklist", ]
  expect_identical(nrow(hd), 1L)
  expect_match(hd$style_name, "heading", fixed = TRUE)
  expect_true(all(c("found", "for the authors to write", "missing") %in% s$text))
  xml <- paste(readLines(file.path(.dx_unzip(f), "word", "document.xml"), warn = FALSE), collapse = "")
  for (fill in c("C6EFCE", "FFEB9C", "FFC7CE")) expect_true(grepl(sprintf('w:fill="%s"', fill), xml, fixed = TRUE), info = fill)
  # the status stays readable as text
  expect_true(all(c("green", "amber", "red") %in% s$text))
  # a block without the columns is refused
  m$supplement[[1L]]$df <- ck[, c("item", "status")]
  expect_error(sr_docx_render(m, "supplement", tempfile(fileext = ".docx"), tempfile("fig-"), .dx_props()), "columns")
  # the fixture's own checklist is in the supplement
  sx <- officer::docx_summary(officer::read_docx(.dx_render("supplement")))
  expect_true("STROBE checklist" %in% sx$text)
})

test_that("the supplement numbers its figures and tables with the S sequences", {
  m <- .dx_mini()
  fig <- m$main[vapply(m$main, function(b) identical(b$type, "figure"), NA)]
  tab <- m$main[vapply(m$main, function(b) identical(b$type, "table"), NA)]
  m$supplement <- c(fig, tab)
  f <- sr_docx_render(m, "supplement", tempfile(fileext = ".docx"), tempfile("fig-"), .dx_props())
  tx <- .dx_text(f)
  expect_true(any(startsWith(tx, "Figure S1. ")))
  expect_true(any(startsWith(tx, "Table S1. ")))
  xml <- paste(readLines(file.path(.dx_unzip(f), "word", "document.xml"), warn = FALSE), collapse = "")
  expect_true(grepl("SEQ FigureS \\* ARABIC", xml, fixed = TRUE))
  expect_true(grepl("SEQ TableS \\* ARABIC", xml, fixed = TRUE))
})

test_that("sr_docx_render refuses bad arguments", {
  m <- .dx_mini()
  p <- tempfile(fileext = ".docx")
  fd <- tempfile("fig-")
  expect_error(sr_docx_render(list(), "main", p, fd, .dx_props()), "document model")
  expect_error(sr_docx_render(m, "appendix", p, fd, .dx_props()), "should be one of")
  expect_error(sr_docx_render(m, "main", NA_character_, fd, .dx_props()), "path")
  expect_error(sr_docx_render(m, "main", p, "", .dx_props()), "fig_dir")
  pr <- .dx_props()
  pr$audit_tip <- NULL
  expect_error(sr_docx_render(m, "main", p, fd, pr), "audit_tip")
  pr <- .dx_props()
  pr$created <- "yesterday"
  expect_error(sr_docx_render(m, "main", p, fd, pr), "created")
  expect_error(sr_docx_render(m, "main", p, fd, "x"), "props")
  m$main[[1L]]$type <- "banner"
  expect_error(sr_docx_render(m, "main", p, fd, .dx_props()), "unknown block type")
  expect_error(.sr_docx_patch(tempfile(fileext = ".docx"), identity), "does not exist")
})
