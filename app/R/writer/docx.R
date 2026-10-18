# writer/docx.R - the Word rendering of the document model, with officer and flextable (no pandoc).
#
#   sr_journal_preset(name)            list(name, font, size_pt, fig_width_mm, fig_height_mm, dpi, verified)
#                                      from inst/templates/journals.json; every preset is verified = FALSE
#   sr_docx_render(model, part, path, fig_dir, props)   one .docx of the main text or of the supplement
#   sr_docx_bookmarks(path)            data.frame(name, text) of the bookmarks of word/document.xml
#   sr_docx_digest(path)               the SHA-256 that stands for the content of a .docx (ruling: the digest
#                                      is the determinism contract, not the bytes of the file)
#
# Markers. officer cannot write a complex field with a cached result, nor a bookmark inside a flextable
# cell. Both are written as a run whose whole text is a marker (@@SR:<kind>:<arg>@@) and swapped for the
# real XML by .sr_docx_patch(): the caption number is a SEQ field whose cached result is the number, and
# a number placed in a cell is wrapped in a bookmark. A text that would collide with a marker is refused.
#
# Bookmarks. The first use of an {{E}} token in a document is bookmarked as sr_<id> (Word needs a bookmark
# name to be unique); later uses of the same token are plain runs. The bookmark text is the registry text,
# so a suppressed count stays "<k" (ruling R29).

SR_DX_MARK <- "@@SR:"
SR_DX_NS_W <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
SR_DX_STATUS_FILL <- c(green = "#C6EFCE", amber = "#FFEB9C", red = "#FFC7CE")

# ---- journal presets ------------------------------------------------------------------------------

# inst/templates/journals.json: next to app/ in the repo and in the portable bundle.
.sr_journal_path <- function() .sr_wr_template("journals.json", what = "journal presets file")

# The presets in file order: a list of validated plain lists.
.sr_journal_all <- function() {
  path <- .sr_journal_path()
  items <- .sr_parse_json(readChar(path, file.size(path), useBytes = TRUE))
  num <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0
  lapply(items, function(it) {
    ok <- is.character(it$name) && length(it$name) == 1L && nzchar(it$name) &&
      is.character(it$font) && length(it$font) == 1L && nzchar(it$font) &&
      num(it$size_pt) && num(it$fig_width_mm) && num(it$fig_height_mm) && num(it$dpi) &&
      is.logical(it$verified) && length(it$verified) == 1L && !is.na(it$verified)
    if (!ok) stop("journals.json holds an invalid preset: ", it$name %||% "?", call. = FALSE)
    list(name = it$name, font = it$font, size_pt = as.numeric(it$size_pt),
         fig_width_mm = as.numeric(it$fig_width_mm), fig_height_mm = as.numeric(it$fig_height_mm),
         dpi = as.numeric(it$dpi), verified = it$verified)
  })
}

#' A journal preset: the font and size of the text and the size and resolution of the figures. The list
#' also has the shape sr_wr_figure_render() reads (fig_width_mm, fig_height_mm, dpi). Every preset is
#' `verified = FALSE`: check it against the author guidelines before submission.
#' @param name a preset name; an unknown name is an error listing the known ones.
sr_journal_preset <- function(name = "default") {
  if (!(is.character(name) && length(name) == 1L && !is.na(name))) {
    stop("`name` must be a single preset name", call. = FALSE)
  }
  all <- .sr_journal_all()
  known <- vapply(all, function(p) p$name, "")
  hit <- match(name, known)
  if (is.na(hit)) {
    stop("unknown journal preset '", name, "'; known presets: ", paste(known, collapse = ", "), call. = FALSE)
  }
  all[[hit]]
}

# ---- text pieces ------------------------------------------------------------------------------------

# A text with the characters XML 1.0 cannot hold removed, and no marker inside.
.sr_dx_clean <- function(x) {
  x <- gsub("[\001-\010\013\014\016-\037]", "", as.character(x), perl = TRUE)
  x[is.na(x)] <- ""
  if (any(grepl(SR_DX_MARK, x, fixed = TRUE))) {
    stop("a text holds the reserved marker '", SR_DX_MARK, "'; it cannot be written to the document", call. = FALSE)
  }
  x
}

# A template cut into pieces, in order: data.frame(kind = text | E | L | C, id, text). `id` is the token id
# of an E piece (E23) and "" otherwise; `text` is what the piece shows (a citation is [k]).
.sr_dx_pieces <- function(template, reg, cite) {
  s <- as.character(template)[1L]
  if (is.na(s)) s <- ""
  m <- gregexpr(SR_PH_RE, s, perl = TRUE)[[1L]]
  starts <- if (m[1L] == -1L) integer() else as.integer(m)
  lens <- if (m[1L] == -1L) integer() else attr(m, "match.length")
  kind <- character()
  id <- character()
  text <- character()
  pos <- 1L
  add <- function(k, i, t) {
    kind <<- c(kind, k)
    id <<- c(id, i)
    text <<- c(text, t)
  }
  for (j in seq_along(starts)) {
    if (starts[j] > pos) add("text", "", substr(s, pos, starts[j] - 1L))
    tok <- substr(s, starts[j], starts[j] + lens[j] - 1L)
    inner <- sub("\\}\\}$", "", sub("^\\{\\{", "", tok))
    k <- if (startsWith(inner, "C:")) "C" else substr(inner, 1L, 1L)
    add(k, if (identical(k, "E")) inner else "", .sr_dx_clean(sr_ph_resolve(tok, reg, cite = cite)))
    pos <- starts[j] + lens[j]
  }
  if (pos <= nchar(s)) add("text", "", substr(s, pos, nchar(s)))
  if (!length(kind)) return(data.frame(kind = character(), id = character(), text = character(), stringsAsFactors = FALSE))
  text[kind == "text"] <- .sr_dx_clean(text[kind == "text"])
  data.frame(kind = kind, id = id, text = text, stringsAsFactors = FALSE)
}

# The ids of the E pieces that this call is the first to use in the document. `seen` is an environment.
.sr_dx_claim <- function(pieces, seen) {
  hit <- logical(nrow(pieces))
  for (i in which(pieces$kind == "E")) {
    nm <- pieces$id[i]
    if (is.null(seen[[nm]])) {
      seen[[nm]] <- TRUE
      hit[i] <- TRUE
    }
  }
  hit
}

# The officer runs of a template: a text, label or citation is a plain run, the first use of a number a
# bookmarked run.
.sr_dx_runs <- function(template, ctx, fp = ctx$fp) {
  pieces <- .sr_dx_pieces(template, ctx$reg, ctx$cite)
  claim <- .sr_dx_claim(pieces, ctx$seen)
  lapply(seq_len(nrow(pieces)), function(i) {
    run <- officer::ftext(pieces$text[i], fp)
    if (claim[i]) officer::run_bookmark(paste0("sr_", pieces$id[i]), run) else run
  })
}

# ---- blocks -----------------------------------------------------------------------------------------

.sr_dx_heading <- function(x, b, ctx) {
  lvl <- min(max(as.integer(b$level), 1L), 3L)
  pieces <- .sr_dx_pieces(b$text, ctx$reg, ctx$cite)
  runs <- list(officer::ftext(paste(pieces$text, collapse = ""), officer::fp_text_lite(font.family = ctx$preset$font)))
  officer::body_add_fpar(x, do.call(officer::fpar, runs), style = paste("heading", lvl))
}

.sr_dx_para <- function(x, b, ctx) {
  officer::body_add_fpar(x, do.call(officer::fpar, .sr_dx_runs(b$text, ctx)), style = "Normal")
}

# "Table 1. <caption>" / "Table S1. <caption>": the number is a marker, patched into a SEQ field.
.sr_dx_caption <- function(template, word, ctx) {
  seq_name <- if (ctx$supp) paste0(word, "S") else word
  lead <- if (ctx$supp) paste0(word, " S") else paste0(word, " ")
  runs <- c(list(officer::ftext(lead, ctx$fp), officer::ftext(paste0(SR_DX_MARK, "SEQ:", seq_name, "@@"), ctx$fp),
                 officer::ftext(". ", ctx$fp)),
            .sr_dx_runs(template, ctx))
  runs
}

.sr_dx_chunks <- function(pieces, claim) {
  out <- list()
  for (i in seq_len(nrow(pieces))) {
    if (claim[i]) {
      nm <- paste0("sr_", pieces$id[i])
      out <- c(out, list(flextable::as_chunk(paste0(SR_DX_MARK, "BS:", nm, "@@")), flextable::as_chunk(pieces$text[i]),
                         flextable::as_chunk(paste0(SR_DX_MARK, "BE:", nm, "@@"))))
    } else {
      out <- c(out, list(flextable::as_chunk(pieces$text[i])))
    }
  }
  out
}

# The flextable of a text data.frame. `df` cells and names are templates; a cell that holds the first use of
# a number gets the bookmark markers. Cells are read row by row (the document order).
.sr_dx_flextable <- function(df, ctx, header = names(df)) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  nr <- nrow(df)
  nc <- ncol(df)
  head_txt <- vapply(header, function(h) paste(.sr_dx_pieces(h, ctx$reg, ctx$cite)$text, collapse = ""), "", USE.NAMES = FALSE)
  plain <- matrix("", nr, nc)
  rich <- list()
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      pieces <- .sr_dx_pieces(df[[j]][i], ctx$reg, ctx$cite)
      plain[i, j] <- paste(pieces$text, collapse = "")
      claim <- .sr_dx_claim(pieces, ctx$seen)
      if (any(claim)) rich[[length(rich) + 1L]] <- list(i = i, j = j, chunks = .sr_dx_chunks(pieces, claim))
    }
  }
  key <- paste0("c", seq_len(nc))
  body <- stats::setNames(as.data.frame(plain, stringsAsFactors = FALSE), key)
  ft <- flextable::flextable(body)
  ft <- flextable::set_header_labels(ft, values = stats::setNames(as.list(head_txt), key))
  for (r in rich) {
    ft <- flextable::compose(ft, i = r$i, j = r$j, value = flextable::as_paragraph(list_values = r$chunks), part = "body")
  }
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::font(ft, fontname = ctx$preset$font, part = "all")
  ft <- flextable::fontsize(ft, size = max(ctx$preset$size_pt - 2, 6), part = "all")
  flextable::set_table_properties(ft, layout = "autofit", width = 1)
}

.sr_dx_small_paras <- function(x, texts, ctx) {
  fp <- officer::fp_text_lite(font.family = ctx$preset$font, font.size = max(ctx$preset$size_pt - 2, 6))
  for (t in texts[!is.na(texts) & nzchar(texts)]) {
    x <- officer::body_add_fpar(x, do.call(officer::fpar, .sr_dx_runs(t, ctx, fp)), style = "Normal")
  }
  x
}

.sr_dx_table <- function(x, b, ctx) {
  cap <- do.call(officer::fpar, c(.sr_dx_caption(b$caption, "Table", ctx), list(fp_p = officer::fp_par(keep_with_next = TRUE))))
  x <- officer::body_add_fpar(x, cap, style = "Table Caption")
  x <- flextable::body_add_flextable(x, .sr_dx_flextable(b$df, ctx), align = "center")
  .sr_dx_small_paras(x, b$footnotes, ctx)
}

# The size of a PNG in pixels, from its IHDR chunk.
.sr_dx_png_size <- function(path) {
  b <- readBin(path, "raw", 24L)
  if (length(b) < 24L || !identical(b[1:8], as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))) {
    stop("'", path, "' is not a PNG file", call. = FALSE)
  }
  be <- function(v) sum(as.numeric(as.integer(v)) * 256^(3:0))
  c(w = be(b[17:20]), h = be(b[21:24]))
}

.sr_dx_figure <- function(x, b, ctx) {
  img <- sr_wr_figure_render(b, ctx$fig_dir, ctx$preset)
  px <- .sr_dx_png_size(img$path)
  w_mm <- if (is.numeric(b$width_mm) && length(b$width_mm) == 1L && is.finite(b$width_mm) && b$width_mm > 0) b$width_mm else ctx$preset$fig_width_mm
  w_in <- min(w_mm / 25.4, ctx$text_w_in)
  alt <- paste(.sr_dx_pieces(b$caption, ctx$reg, ctx$cite)$text, collapse = "")
  pic <- officer::fpar(officer::external_img(img$path, width = w_in, height = w_in * px[["h"]] / px[["w"]], unit = "in", alt = alt),
                       fp_p = officer::fp_par(text.align = "center", keep_with_next = TRUE))
  x <- officer::body_add_fpar(x, pic, style = "Normal")
  officer::body_add_fpar(x, do.call(officer::fpar, .sr_dx_caption(b$caption, "Figure", ctx)), style = "Image Caption")
}

.sr_dx_refs <- function(x, b, ctx) {
  x <- .sr_dx_heading(x, list(text = "References", level = 1L), ctx)
  lines <- sr_cite_vancouver(sr_cite_csl(b$keys))
  for (ln in .sr_dx_clean(lines)) {
    x <- officer::body_add_fpar(x, officer::fpar(officer::ftext(ln, ctx$fp)), style = "Normal")
  }
  x
}

.sr_dx_checklist <- function(x, b, ctx) {
  x <- .sr_dx_heading(x, list(text = "STROBE checklist", level = 1L), ctx)
  df <- b$df
  need <- c("item", "section", "label", "status", "detail")
  if (!all(need %in% names(df))) stop("the checklist block needs the columns ", paste(need, collapse = ", "), call. = FALSE)
  show <- df[, need, drop = FALSE]
  show[] <- lapply(show, function(col) .sr_dx_clean(col))
  ft <- .sr_dx_flextable(show, ctx, header = c("Item", "Section", "Reporting item", "Status", "Detail"))
  for (st in names(SR_DX_STATUS_FILL)) {
    i <- which(show$status == st)
    if (length(i)) ft <- flextable::bg(ft, i = i, j = 4L, bg = SR_DX_STATUS_FILL[[st]], part = "body")
  }
  flextable::body_add_flextable(x, ft, align = "center")
}

# ---- the document -----------------------------------------------------------------------------------

.sr_dx_time <- function(x) {
  if (inherits(x, "POSIXt")) {
    # officer writes the clock reading of the object with a trailing "Z": express it in UTC first
    x <- as.POSIXct(x)
    attr(x, "tzone") <- "UTC"
    return(x)
  }
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    t <- tryCatch(as.POSIXct(sub("Z$", "", sub("T", " ", x)), tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d")),
                  error = function(e) as.POSIXct(NA))
    if (!is.na(t)) return(t)
  }
  stop("`props$created` must be a POSIXct or an ISO-8601 time string", call. = FALSE)
}

.sr_dx_props <- function(props) {
  if (!is.list(props)) stop("`props` must be a list(audit_tip, sap_hash, export_id, created)", call. = FALSE)
  one <- function(nm) {
    v <- props[[nm]]
    if (!(is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v))) {
      stop("`props$", nm, "` must be a single non-empty string", call. = FALSE)
    }
    .sr_dx_clean(v)
  }
  list(audit_tip = one("audit_tip"), sap_hash = one("sap_hash"), export_id = one("export_id"),
       created = .sr_dx_time(props$created))
}

#' Render one part of the document model to a .docx.
#'
#' Headings use the Word heading styles; paragraphs are runs in the preset font; every first use of a number
#' is a bookmark sr_<id> holding the registry text; table and figure captions carry SEQ fields (cached result
#' = the number; "Table S1" / "Figure S1" in the supplement); the references and the STROBE checklist are
#' rendered where the model has them. The core properties name the creator "StatRouter"; the custom
#' properties sr_audit_tip, sr_sap_hash and sr_export_id and the footer carry the audit state.
#' @param model from sr_doc_model().
#' @param part "main" or "supplement".
#' @param path the .docx to write.
#' @param fig_dir the folder of the figure PNGs (created).
#' @param props list(audit_tip, sap_hash, export_id, created): single strings and a time.
#' @return path.
sr_docx_render <- function(model, part = c("main", "supplement"), path, fig_dir, props) {
  part <- match.arg(part)
  if (!(is.list(model) && all(c("meta", "reg", "main", "supplement") %in% names(model)))) {
    stop("`model` must be a document model (sr_doc_model)", call. = FALSE)
  }
  if (!(is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path))) stop("`path` must be one file name", call. = FALSE)
  if (!(is.character(fig_dir) && length(fig_dir) == 1L && !is.na(fig_dir) && nzchar(fig_dir))) {
    stop("`fig_dir` must be one folder name", call. = FALSE)
  }
  pr <- .sr_dx_props(props)
  preset <- sr_journal_preset(model$meta$preset %||% "default")
  keys <- sr_cite_order(model)
  # the text is as wide as the figures (the page is A4): the margins give way, never the figure
  text_w_in <- max(6.3, preset$fig_width_mm / 25.4)
  ctx <- list(reg = model$reg, cite = sr_cite_fn(keys), preset = preset, supp = identical(part, "supplement"),
              fig_dir = fig_dir, text_w_in = text_w_in, seen = new.env(parent = emptyenv()),
              fp = officer::fp_text_lite(font.family = preset$font, font.size = preset$size_pt))
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  x <- officer::read_docx()
  x <- officer::set_doc_properties(x, title = if (identical(part, "main")) "Manuscript" else "Supplement",
                                   creator = "StatRouter", created = pr$created,
                                   values = list(sr_audit_tip = pr$audit_tip, sr_sap_hash = pr$sap_hash,
                                                 sr_export_id = pr$export_id))
  for (b in model[[part]]) {
    x <- switch(b$type,
      heading = .sr_dx_heading(x, b, ctx),
      para = .sr_dx_para(x, b, ctx),
      table = .sr_dx_table(x, b, ctx),
      figure = .sr_dx_figure(x, b, ctx),
      refs = .sr_dx_refs(x, b, ctx),
      checklist = .sr_dx_checklist(x, b, ctx),
      stop("unknown block type '", b$type, "'", call. = FALSE))
  }
  foot <- sprintf("StatRouter export %s | SAP %s | audit %s", pr$export_id, substr(pr$sap_hash, 1L, 12L),
                  substr(pr$audit_tip, 1L, 16L))
  margin <- (8.27 - text_w_in) / 2
  sec <- officer::prop_section(
    page_size = officer::page_size(orient = "portrait", width = 8.27, height = 11.69),
    page_margins = officer::page_mar(top = 0.98, bottom = 0.98, left = margin, right = margin, header = 0.49, footer = 0.49, gutter = 0),
    type = "continuous",
    footer_default = officer::block_list(officer::fpar(officer::ftext(foot, officer::fp_text_lite(font.family = preset$font, font.size = 8)))))
  x <- officer::body_set_default_section(x, sec)
  print(x, target = path)
  .sr_docx_patch(path, .sr_dx_patch_document)
  path
}

# ---- the markers ------------------------------------------------------------------------------------

# Swap the markers of word/document.xml for XML, renumber the bookmarks and drop officer's own attribute.
.sr_dx_patch_document <- function(doc) {
  ns <- xml2::xml_ns(doc)
  frag <- function(inner) xml2::read_xml(sprintf('<w:x xmlns:w="%s">%s</w:x>', SR_DX_NS_W, inner))
  swap <- function(run, inner) {
    for (ch in xml2::xml_children(frag(inner))) xml2::xml_add_sibling(run, ch, .where = "before", .copy = TRUE)
    xml2::xml_remove(run)
  }
  fld <- function(type, rpr) sprintf('<w:r>%s<w:fldChar w:fldCharType="%s"/></w:r>', rpr, type)
  runs <- xml2::xml_find_all(doc, sprintf("//w:r[w:t[starts-with(., '%s')]]", SR_DX_MARK), ns)
  n <- list()
  for (run in runs) {
    mk <- regmatches(xml2::xml_text(run), regexec("^@@SR:(BS|BE|SEQ):(.+)@@$", xml2::xml_text(run)))[[1L]]
    if (length(mk) != 3L) next
    arg <- mk[3L]
    rpr_node <- xml2::xml_find_first(run, "w:rPr", ns)
    rpr <- if (inherits(rpr_node, "xml_missing")) "" else as.character(rpr_node)
    if (identical(mk[2L], "BS")) {
      swap(run, sprintf('<w:bookmarkStart w:id="%s" w:name="%s"/>', arg, arg))
    } else if (identical(mk[2L], "BE")) {
      swap(run, sprintf('<w:bookmarkEnd w:id="%s"/>', arg))
    } else {
      n[[arg]] <- (n[[arg]] %||% 0L) + 1L
      swap(run, paste0(fld("begin", rpr),
                       sprintf('<w:r>%s<w:instrText xml:space="preserve"> SEQ %s \\* ARABIC </w:instrText></w:r>', rpr, arg),
                       fld("separate", rpr),
                       sprintf("<w:r>%s<w:t>%d</w:t></w:r>", rpr, n[[arg]]),
                       fld("end", rpr)))
    }
  }
  # sequential integer ids (officer writes UUIDs), the same for the same document
  starts <- xml2::xml_find_all(doc, "//w:bookmarkStart", ns)
  ends <- xml2::xml_find_all(doc, "//w:bookmarkEnd", ns)
  old <- xml2::xml_attr(starts, "w:id", ns)
  for (i in seq_along(starts)) xml2::xml_set_attr(starts[[i]], "w:id", as.character(i), ns)
  end_old <- xml2::xml_attr(ends, "w:id", ns)
  for (i in seq_along(ends)) xml2::xml_set_attr(ends[[i]], "w:id", as.character(match(end_old[i], old)), ns)
  for (r in xml2::xml_find_all(doc, "//w:footerReference | //w:headerReference", ns)) xml2::xml_set_attr(r, "w:officer", NULL, ns)
  invisible(doc)
}

#' Rewrite word/document.xml of a .docx in place: `fun(doc)` edits the xml2 document, then the package is
#' zipped again with [Content_Types].xml first.
.sr_docx_patch <- function(path, fun) {
  if (!file.exists(path)) stop("'", path, "' does not exist", call. = FALSE)
  dir <- tempfile("sr-docx-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  zip::unzip(path, exdir = dir)
  doc_path <- file.path(dir, "word", "document.xml")
  doc <- xml2::read_xml(doc_path)
  fun(doc)
  xml2::write_xml(doc, doc_path)
  files <- list.files(dir, recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
  files <- c(intersect("[Content_Types].xml", files), sort(setdiff(files, "[Content_Types].xml"), method = "radix"))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  zip::zip(out, files, root = dir, mode = "mirror", include_directories = FALSE)
  if (!file.copy(out, path, overwrite = TRUE)) stop("could not write '", path, "'", call. = FALSE)
  invisible(path)
}

# ---- reading back -------------------------------------------------------------------------------------

#' The bookmarks of a .docx: data.frame(name, text), the text between each bookmarkStart and its
#' bookmarkEnd, in document order.
sr_docx_bookmarks <- function(path) {
  if (!file.exists(path)) stop("'", path, "' does not exist", call. = FALSE)
  dir <- tempfile("sr-docx-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  zip::unzip(path, files = "word/document.xml", exdir = dir)
  doc <- xml2::read_xml(file.path(dir, "word", "document.xml"))
  ns <- xml2::xml_ns(doc)
  starts <- xml2::xml_find_all(doc, "//w:bookmarkStart", ns)
  name <- character(length(starts))
  text <- character(length(starts))
  for (i in seq_along(starts)) {
    id <- xml2::xml_attr(starts[[i]], "w:id", ns)
    name[i] <- xml2::xml_attr(starts[[i]], "w:name", ns)
    sib <- xml2::xml_find_all(starts[[i]], "following-sibling::*", ns)
    buf <- character()
    for (s in sib) {
      if (identical(xml2::xml_name(s), "bookmarkEnd") && identical(xml2::xml_attr(s, "w:id", ns), id)) break
      buf <- c(buf, xml2::xml_text(xml2::xml_find_all(s, ".//w:t", ns)))
    }
    text[i] <- paste(buf, collapse = "")
  }
  data.frame(name = name, text = text, stringsAsFactors = FALSE)
}

# The values of the export-specific custom properties (sr_audit_tip, sr_export_id) of a .docx unpacked in
# `dir`: a named character vector, empty when docProps/custom.xml is absent.
.sr_dx_meta_values <- function(dir) {
  f <- file.path(dir, "docProps", "custom.xml")
  if (!file.exists(f)) return(character())
  doc <- xml2::read_xml(f)
  props <- xml2::xml_find_all(doc, "//*[local-name()='property']")
  nm <- xml2::xml_attr(props, "name")
  val <- xml2::xml_text(props)
  keep <- nm %in% c("sr_audit_tip", "sr_export_id") & nzchar(val)
  stats::setNames(val[keep], nm[keep])
}

#' The SHA-256 that stands for a .docx: over the sorted (part name, SHA-256 of the part bytes) lines. The
#' creation and modification dates of docProps/core.xml are replaced by a fixed string, and the media parts
#' are named by the SHA-256 of their bytes (the relationship targets follow), so two renders of the same
#' model give the same digest although officer stamps the time and names the images freshly. The digest
#' stands for the content, not for the export it sits in (ruling R33): the values of the sr_audit_tip and
#' sr_export_id custom properties are replaced by fixed strings wherever they appear (docProps/custom.xml
#' and the footer parts; the footer shows the first 16 characters of the audit tip), so the same model
#' exported twice has one digest. The SAP hash stays in.
sr_docx_digest <- function(path) {
  if (!file.exists(path)) stop("'", path, "' does not exist", call. = FALSE)
  dir <- tempfile("sr-docx-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  zip::unzip(path, exdir = dir)
  parts <- list.files(dir, recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
  bytes <- lapply(parts, function(p) .sr_read_bytes(file.path(dir, p)))
  names(bytes) <- parts
  newname <- parts
  media <- startsWith(parts, "word/media/")
  rename <- character()
  for (i in which(media)) {
    base <- basename(parts[i])
    ext <- tools::file_ext(base)
    nn <- paste0(.sr_sha256(bytes[[i]]), if (nzchar(ext)) paste0(".", ext) else "")
    newname[i] <- paste0("word/media/", nn)
    rename[base] <- nn
  }
  meta <- .sr_dx_meta_values(dir)
  tip <- unname(meta["sr_audit_tip"])
  eid <- unname(meta["sr_export_id"])
  text_part <- grepl("\\.(xml|rels)$", parts)
  for (i in which(text_part)) {
    s <- rawToChar(bytes[[i]])
    Encoding(s) <- "UTF-8"
    if (identical(parts[i], "docProps/core.xml")) {
      s <- gsub("(<dcterms:(created|modified)[^>]*>)[^<]*", "\\1FIXED", s)
    }
    if (identical(parts[i], "docProps/custom.xml") || grepl("^word/(footer|header)[0-9]*\\.xml$", parts[i])) {
      if (!is.na(tip)) {
        s <- gsub(tip, "SR-AUDIT-TIP", s, fixed = TRUE)
        s <- gsub(substr(tip, 1L, 16L), "SR-AUDIT-TIP", s, fixed = TRUE)
      }
      if (!is.na(eid)) s <- gsub(eid, "SR-EXPORT-ID", s, fixed = TRUE)
    }
    for (old in names(rename)) s <- gsub(old, rename[[old]], s, fixed = TRUE)
    bytes[[i]] <- charToRaw(s)
  }
  ord <- order(newname, method = "radix")
  lines <- vapply(ord, function(i) paste0(newname[i], "\t", .sr_sha256(bytes[[i]]), "\n"), "")
  .sr_sha256(paste(lines, collapse = ""))
}
