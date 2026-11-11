# ui/write_view.R - the pure view builders behind the Write panel (no Shiny reactivity, no side effects).
#
# Prefix .sr_wp_ (the writer core owns .sr_wr_). Models never compute numbers: a view only formats what the
# core frames hold. The preview renders every text through .sr_dx_pieces, the way the docx builder does, so a
# suppressed count reads "<5" here exactly as in the document.

# ---- presets and options ---------------------------------------------------------------------------

# The journal presets in file order: data.frame(name, label); an unverified preset is marked in its label.
.sr_wp_presets <- function() {
  all <- .sr_journal_all()
  name <- vapply(all, function(x) x$name, "")
  ver <- vapply(all, function(x) isTRUE(x$verified), NA)
  data.frame(name = name, label = ifelse(ver, name, paste0(name, " (not verified)")), stringsAsFactors = FALSE)
}

# The writer options of the form: list(opts, error). Never throws.
.sr_wp_opts <- function(preset, suppress_k, table2_on, alpha) {
  tryCatch(list(opts = sr_writer_opts(preset, suppress_k, if (isTRUE(table2_on)) "label" else "suppress", alpha),
                error = NULL),
           error = function(e) list(opts = NULL, error = conditionMessage(e)))
}

# ---- gates -----------------------------------------------------------------------------------------

# The export gates: data.frame(gate, ok, reason); `reason` is "" when ok.
.sr_wp_gates <- function(p, busy) {
  mk <- function(gate, ok, reason) {
    data.frame(gate = gate, ok = ok, reason = if (ok) "" else reason, stringsAsFactors = FALSE)
  }
  if (is.null(p)) return(mk("Project open", FALSE, "open a project first"))
  an <- .sr_sap_analyses(p)
  runs <- p$state$runs %||% list()
  unrun <- names(an)[vapply(names(an), function(id) {
    k <- runs[[id]]$ok_key
    is.null(k) || !length(k) || is.na(k[[1L]])
  }, NA)]
  rbind(
    mk("Project writable", !isTRUE(p$readonly), "the project is read-only (locked by another session)"),
    mk("SAP locked", !sr_blind(p), "lock the SAP first"),
    mk("Pre-specified analyses run", !length(unrun), paste0("no ok run: ", paste(unrun, collapse = ", "))),
    mk("App not busy", !isTRUE(busy), .sr_ui_busy_text)
  )
}

# The first failing reason, or NULL.
.sr_wp_gate_reason <- function(gates) {
  bad <- which(!gates$ok)
  if (length(bad)) gates$reason[bad[1L]] else NULL
}

# ---- claim guard -----------------------------------------------------------------------------------

# The guard findings, block severity first and then by block; columns severity, rule, block, analysis, detail.
.sr_wp_guard_rows <- function(findings) {
  out <- findings[order(findings$severity != "block", findings$block),
                  c("severity", "rule", "block", "analysis", "detail"), drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---- preview ---------------------------------------------------------------------------------------

# A template resolved to one string, the way the docx shows it (a suppressed count reads "<k").
.sr_wp_text <- function(template, model) {
  paste(.sr_dx_pieces(template, model$reg, .sr_dx_cite(model))$text, collapse = "")
}

.sr_wp_block <- function(b, model, fig_dir, preset) {
  txt <- function(x) .sr_wp_text(x, model)
  switch(b$type,
    heading = if (identical(as.integer(b$level), 1L)) htmltools::h3(txt(b$text)) else htmltools::h4(txt(b$text)),
    para = htmltools::p(txt(b$text)),
    table = {
      df <- as.data.frame(b$df, stringsAsFactors = FALSE)
      head <- vapply(b$header %||% names(df), txt, "", USE.NAMES = FALSE)
      rows <- lapply(seq_len(nrow(df)), function(i) {
        htmltools::tags$tr(lapply(seq_len(ncol(df)), function(j) htmltools::tags$td(txt(df[[j]][i]))))
      })
      notes <- b$footnotes[!is.na(b$footnotes) & nzchar(b$footnotes)]
      htmltools::tagList(
        htmltools::p(htmltools::strong(txt(b$caption))),
        htmltools::tags$table(class = "table table-sm",
                              htmltools::tags$thead(htmltools::tags$tr(lapply(head, htmltools::tags$th))),
                              htmltools::tags$tbody(rows)),
        lapply(notes, function(f) htmltools::tagList(htmltools::tags$small(txt(f)), htmltools::br())))
    },
    figure = {
      img <- sr_wr_figure_render(b, fig_dir, preset)$path
      uri <- paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(img, "raw", file.size(img))))
      htmltools::tagList(htmltools::tags$img(src = uri, style = "max-width:320px;width:100%;", alt = txt(b$caption)),
                         htmltools::p(htmltools::tags$small(txt(b$caption))))
    },
    NULL)
}

# The main part then the supplement as one tagList; refs and the checklist are left out (the checklist has a card).
.sr_wp_preview <- function(model, fig_dir, preset) {
  part <- function(blocks) lapply(blocks, function(b) {
    if (!is.list(b) || b$type %in% c("refs", "checklist")) NULL else .sr_wp_block(b, model, fig_dir, preset)
  })
  htmltools::tagList(part(model$main), htmltools::h3("Supplement"), part(model$supplement))
}

# ---- history and verification ----------------------------------------------------------------------

.sr_wp_hcols <- c("kind", "id", "created", "preset", "n_files", "on_disk", "status")

# One row per export, in event order: data.frame(kind, id, created, preset, n_files, on_disk, status).
.sr_wp_history <- function(p) {
  ex <- p$state$exports %||% list()
  if (!length(ex)) {
    return(data.frame(kind = character(), id = character(), created = character(), preset = character(),
                      n_files = integer(), on_disk = logical(), status = character(), stringsAsFactors = FALSE))
  }
  evs <- tryCatch(sr_events_read(p$paths$events), error = function(e) list())
  ts <- list()
  for (e in evs) if (identical(e$type, "doc.exported")) ts[[as.character(e$payload$id)]] <- as.character(e$ts)
  rows <- lapply(ex, function(e) {
    id <- as.character(e$id)
    paths <- vapply(e$files %||% list(), function(f) as.character(f$path), "")
    disk <- all(file.exists(file.path(p$paths$dir, paths)))
    created <- ts[[id]] %||% as.character(e$created %||% NA_character_)
    data.frame(kind = as.character(e$kind), id = id, created = created[1L],
               preset = as.character(e$preset %||% NA_character_)[1L], n_files = length(paths),
               on_disk = disk, status = if (disk) "ok" else "missing on disk", stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[, .sr_wp_hcols, drop = FALSE]
}

# The lines of a verification: list(problems, numbers, note); note says why numbers were not checked. Numeric columns of the differing rows go to 4 digits.
.sr_wp_verify_lines <- function(v) {
  f <- v$files
  bad <- f[f$status != "ok", , drop = FALSE]
  problems <- if (nrow(bad)) paste0(bad$status, ": ", bad$path) else character()
  nums <- NULL
  if (!isTRUE(v$numbers_ok) && !is.null(v$numbers)) {
    nums <- v$numbers[!v$numbers$ok, , drop = FALSE]
    for (j in names(nums)) if (is.numeric(nums[[j]])) nums[[j]] <- signif(nums[[j]], 4L)
    rownames(nums) <- NULL
  }
  list(problems = problems, numbers = nums, note = v$numbers_note)
}

# ---- checklist -------------------------------------------------------------------------------------

# list(df, note): the STROBE frame, or for a CONSORT project the note text.
.sr_wp_checklist <- function(model) {
  blocks <- model$supplement
  for (b in blocks) {
    if (is.list(b) && identical(b$type, "checklist")) {
      df <- as.data.frame(b$df, stringsAsFactors = FALSE)
      return(list(df = df[, c("item", "section", "label", "status", "detail"), drop = FALSE], note = NULL))
    }
  }
  for (b in blocks) {
    if (is.list(b) && identical(b$type, "para") && identical(b$id, "checklist_note")) {
      return(list(df = NULL, note = as.character(b$text)))
    }
  }
  list(df = NULL, note = NULL)
}
