# io/redcap_dict.R - dictionaries from REDCap (data dictionary CSV and ODM XML metadata) and the ODM
# data reader (format `odm`).
#
# Both dictionary readers map a REDCap field to dictionary rows through ONE function
# (.sr_redcap_field), so the CSV and the ODM give the same dictionary: radio / dropdown / yesno /
# truefalse -> categorical with levels; text with a validation -> integer / numeric / date / datetime /
# text; calc and slider -> numeric; notes and file -> text; descriptive -> no row; a checkbox field
# `v` with choices c1, c2 -> logical variables v___c1, v___c2. Labels lose their HTML and extra white
# space; date bounds become ISO text.
#
# ODM XML is read with xml2::read_xml(options = "NONET") only: no NOENT (entities are not expanded),
# no DTDLOAD (an external DTD or entity is not loaded), no network. The ODM data reader builds one
# row per subject, event and repeat instance (all the forms of an event share a row).

# ---- shared ---------------------------------------------------------------------------------------

# HTML tags and entities out, white space collapsed.
.sr_redcap_strip <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("<[^>]*>", "", x)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  trimws(gsub("[[:space:]]+", " ", x))
}

#' "1, Male | 2, Female" -> data.frame(code, label), or NULL when nothing parses. Items split on "|"
#' (or a line break); each item splits on its first comma.
.sr_redcap_choices <- function(s) {
  if (is.null(s) || length(s) != 1L || is.na(s) || !nzchar(trimws(s))) return(NULL)
  parts <- trimws(unlist(strsplit(s, "\\||\r?\n"), use.names = FALSE))
  parts <- parts[nzchar(parts)]
  comma <- regexpr(",", parts, fixed = TRUE)
  parts <- parts[comma > 0L]
  comma <- comma[comma > 0L]
  if (!length(parts)) return(NULL)
  code <- trimws(substr(parts, 1L, comma - 1L))
  label <- .sr_redcap_strip(substring(parts, comma + 1L))
  keep <- nzchar(code)
  if (!any(keep)) return(NULL)
  data.frame(code = code[keep], label = label[keep], stringsAsFactors = FALSE)
}

# The variable name REDCap gives a checkbox choice: v___<code>, the code lower-cased, other characters _.
.sr_redcap_cb_name <- function(variable, code) {
  paste0(variable, "___", gsub("[^a-z0-9_]", "_", tolower(code)))
}

# A min / max as ISO text for a date or datetime field: the field's own format (ymd, dmy, mdy) first,
# then ISO; text that does not parse is kept as it was.
.sr_redcap_iso_bound <- function(x, validation, type) {
  if (is.na(x) || !nzchar(x) || !(type %in% c("date", "datetime"))) return(if (is.na(x)) "" else x)
  ord <- sub(".*_", "", validation)
  dpart <- switch(ord, dmy = "%d-%m-%Y", mdy = "%m-%d-%Y", "%Y-%m-%d")
  dparts <- unique(c(dpart, "%Y-%m-%d"))
  tparts <- if (type == "datetime") c(" %H:%M:%S", " %H:%M", "T%H:%M:%S", "T%H:%M") else ""
  y <- gsub("/", "-", trimws(x), fixed = TRUE)
  for (tp in tparts) for (dp in dparts) {
    p <- strptime(y, paste0(dp, tp), tz = "UTC")
    if (!is.na(p)) return(format(p, if (type == "date") "%Y-%m-%d" else "%Y-%m-%dT%H:%M:%S"))
  }
  x
}

# The dictionary rows of one REDCap field. `choices` is a data.frame(code, label) or NULL.
.sr_redcap_field <- function(variable, label, field_type, validation, choices, min, max) {
  ft <- tolower(trimws(field_type))
  val <- tolower(trimws(validation))
  label <- .sr_redcap_strip(label)
  if (!nzchar(label)) label <- variable
  if (ft == "descriptive") return(NULL)
  one <- function(v, lab, type, lv = NULL, mn = "", mx = "") {
    .sr_dict_build(v, lab, type, "", mn, mx, list(lv %||% .sr_dict_levels0()), FALSE, list(character()), "redcap")
  }
  if (ft == "checkbox") {
    if (is.null(choices) || !nrow(choices)) stop("checkbox field '", variable, "' has no choices", call. = FALSE)
    return(.sr_dict_build(.sr_redcap_cb_name(variable, choices$code),
                          paste0(label, " (choice=", choices$label, ")"), "logical", "", "", "",
                          rep(list(.sr_dict_levels0()), nrow(choices)), FALSE, rep(list(character()), nrow(choices)),
                          "redcap"))
  }
  if (ft %in% c("radio", "dropdown", "yesno", "truefalse")) {
    if (is.null(choices) || !nrow(choices)) {
      choices <- switch(ft,
                        yesno = data.frame(code = c("1", "0"), label = c("Yes", "No"), stringsAsFactors = FALSE),
                        truefalse = data.frame(code = c("1", "0"), label = c("True", "False"), stringsAsFactors = FALSE),
                        NULL)
    }
    return(one(variable, label, "categorical", choices))
  }
  type <- if (ft %in% c("calc", "slider")) {
    "numeric"
  } else if (ft == "text") {
    if (val == "integer") "integer"
    else if (grepl("^number", val)) "numeric"
    else if (grepl("^datetime", val)) "datetime"
    else if (grepl("^date", val)) "date"
    else "text"
  } else if (ft == "boolean") {
    "logical"
  } else {
    "text"
  }
  bounds <- type %in% c("integer", "numeric", "date", "datetime")
  mn <- if (bounds) .sr_redcap_iso_bound(trimws(min), val, type) else ""
  mx <- if (bounds) .sr_redcap_iso_bound(trimws(max), val, type) else ""
  one(variable, label, type, NULL, mn, mx)
}

# ---- REDCap data dictionary CSV -------------------------------------------------------------------

#' Read a REDCap data dictionary CSV (the export of Project Setup > Data Dictionary). Columns are found
#' by name, not position: Variable / Field Name, Field Type, Field Label, Choices, Calculations, OR
#' Slider Labels, Text Validation Type OR Show Slider Number, Text Validation Min / Max.
sr_dict_read_redcap <- function(path) {
  dd <- .sr_dict_read_table(path)
  nm <- gsub("^_+|_+$", "", gsub("[^a-z]+", "_", tolower(names(dd))))
  find <- function(pattern) {
    i <- which(grepl(pattern, nm))
    if (length(i)) i[1L] else NA_integer_
  }
  cols <- list(variable = find("^variable_field_name$|^variable$|^field_name$"),
               type = find("^field_type$"), label = find("^field_label$"),
               choices = find("^choices"), validation = find("^text_validation_type"),
               min = find("^text_validation_min"), max = find("^text_validation_max"))
  if (is.na(cols$variable)) stop("the REDCap data dictionary has no variable column ('Variable / Field Name')", call. = FALSE)
  get <- function(k, i) if (is.na(cols[[k]])) "" else dd[[cols[[k]]]][i]
  parts <- lapply(seq_len(nrow(dd)), function(i) {
    v <- trimws(get("variable", i))
    if (!nzchar(v)) return(NULL)
    ft <- tolower(trimws(get("type", i)))
    if (!nzchar(ft)) ft <- "text"
    ch <- if (ft %in% c("radio", "dropdown", "checkbox", "yesno", "truefalse")) .sr_redcap_choices(get("choices", i)) else NULL
    .sr_redcap_field(v, get("label", i), ft, get("validation", i), ch, get("min", i), get("max", i))
  })
  sr_dict_validate(.sr_dict_bind(parts))
}

# ---- ODM XML --------------------------------------------------------------------------------------

# Parse an ODM file: local path only, no network, no entity expansion, no DTD loading; namespaces
# stripped so XPath stays plain. Errors if the root element is not ODM.
.sr_odm_read <- function(path) {
  .sr_dict_check_path(path, "the ODM file")
  # an entity declared in a DTD is not defined (and so not expanded): libxml only warns, mute that one
  doc <- tryCatch(withCallingHandlers(xml2::read_xml(path, options = "NONET"),
                                      warning = function(w) {
                                        if (grepl("^Entity '.*' not defined", conditionMessage(w))) invokeRestart("muffleWarning")
                                      }),
                  error = function(e) stop("cannot read the ODM file ", basename(path), ": ", conditionMessage(e),
                                           call. = FALSE))
  xml2::xml_ns_strip(doc)
  if (!identical(xml2::xml_name(doc), "ODM")) stop("not an ODM file: the root element is '", xml2::xml_name(doc), "'", call. = FALSE)
  doc
}

.sr_odm_attr <- function(node, name) {
  a <- xml2::xml_attr(node, name)
  if (is.na(a)) "" else a
}

# CodeList OID -> data.frame(code, label)
.sr_odm_codelists <- function(doc) {
  out <- list()
  for (cl in xml2::xml_find_all(doc, "//MetaDataVersion/CodeList")) {
    items <- xml2::xml_find_all(cl, "./CodeListItem | ./EnumeratedItem")
    code <- xml2::xml_attr(items, "CodedValue")
    lab <- vapply(items, function(n) {
      tt <- xml2::xml_find_first(n, "./Decode/TranslatedText")
      if (inherits(tt, "xml_missing")) NA_character_ else xml2::xml_text(tt)
    }, character(1))
    lab <- ifelse(is.na(lab), code, .sr_redcap_strip(lab))
    keep <- !is.na(code)
    out[[xml2::xml_attr(cl, "OID")]] <- data.frame(code = code[keep], label = lab[keep], stringsAsFactors = FALSE)
  }
  out
}

# ODM DataType -> a REDCap-style text validation (for ODM files that do not carry one)
.sr_odm_validation <- function(dtype) {
  switch(tolower(dtype), integer = "integer", float = , double = , decimal = "number", date = "date_ymd",
         datetime = "datetime_ymd", "")
}

#' Read the metadata of an ODM XML file (REDCap "CDISC ODM" export, or any ODM): ItemDef, CodeList and
#' RangeCheck map to variable, label, type, levels, min and max exactly as the REDCap data dictionary
#' does. Comparators GE / GT give the minimum, LE / LT the maximum (inclusive or not is not kept).
sr_dict_read_odm <- function(path) {
  doc <- .sr_odm_read(path)
  items <- xml2::xml_find_all(doc, "//MetaDataVersion/ItemDef")
  if (!length(items)) stop("the ODM file has no ItemDef: it carries no metadata", call. = FALSE)
  cls <- .sr_odm_codelists(doc)
  redcap <- any(!is.na(xml2::xml_attr(items, "FieldType")))
  parts <- lapply(items, function(it) {
    oid <- .sr_odm_attr(it, "OID")
    nmx <- .sr_odm_attr(it, "Name")
    v <- if (nzchar(oid)) oid else nmx
    if (!nzchar(v)) return(NULL)
    q <- xml2::xml_find_first(it, "./Question/TranslatedText")
    if (inherits(q, "xml_missing") || !nzchar(xml2::xml_text(q))) q <- xml2::xml_find_first(it, "./Description/TranslatedText")
    lab <- if (!inherits(q, "xml_missing") && nzchar(xml2::xml_text(q))) xml2::xml_text(q) else if (nzchar(nmx)) nmx else v
    dtype <- .sr_odm_attr(it, "DataType")
    ft <- .sr_odm_attr(it, "FieldType")
    val <- .sr_odm_attr(it, "TextValidationType")
    ref <- xml2::xml_find_first(it, "./CodeListRef")
    ch <- if (inherits(ref, "xml_missing")) NULL else cls[[.sr_odm_attr(ref, "CodeListOID")]]
    if (!is.null(ch) && !nrow(ch)) ch <- NULL
    mn <- .sr_odm_attr(it, "TextValidationMin")
    mx <- .sr_odm_attr(it, "TextValidationMax")
    for (rc in xml2::xml_find_all(it, "./RangeCheck")) {
      cv <- xml2::xml_text(xml2::xml_find_first(rc, "./CheckValue"))
      cmp <- toupper(.sr_odm_attr(rc, "Comparator"))
      if (cmp %in% c("GE", "GT") && !is.na(cv)) mn <- trimws(cv)
      if (cmp %in% c("LE", "LT") && !is.na(cv)) mx <- trimws(cv)
    }
    if (!nzchar(ft)) {
      ft <- if (!is.null(ch)) "radio" else if (tolower(dtype) == "boolean") "boolean" else "text"
      if (!nzchar(val)) val <- .sr_odm_validation(dtype)
    }
    if (tolower(ft) == "checkbox" && grepl("___", v, fixed = TRUE)) {
      # REDCap lists each checkbox choice as its own ItemDef (<var>___<code>)
      return(.sr_dict_build(v, .sr_redcap_strip(lab), "logical", "", "", "", list(.sr_dict_levels0()), FALSE,
                            list(character()), "redcap"))
    }
    d <- .sr_redcap_field(v, lab, ft, val, ch, mn, mx)
    if (!is.null(d) && !redcap) d$source <- "odm"
    d
  })
  sr_dict_validate(.sr_dict_bind(parts))
}

# ---- ODM data reader ------------------------------------------------------------------------------

.sr_odm_num_re <- "^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?$"

# A column of ODM values (character, NA for missing) in the type its ItemDef DataType asks for, when
# every value fits; otherwise it stays text. A value is never turned into NA.
.sr_odm_coerce <- function(x, dtype) {
  dtype <- tolower(dtype)
  ok <- x[!is.na(x)]
  if (!length(ok)) return(x)
  num <- function() all(grepl(.sr_odm_num_re, ok)) && !any(grepl("^[-+]?0[0-9]", ok))
  if (dtype == "integer" && num()) {
    if (all(grepl("^[-+]?[0-9]+$", ok)) && all(abs(as.numeric(ok)) < 2^31)) return(as.integer(x))
    return(as.numeric(x))
  }
  if (dtype %in% c("float", "double", "decimal") && num()) return(as.numeric(x))
  if (dtype == "date" && all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", ok)) && !anyNA(as.Date(ok, format = "%Y-%m-%d"))) {
    return(as.Date(x, format = "%Y-%m-%d"))
  }
  if (dtype == "boolean" && all(tolower(ok) %in% c("0", "1", "true", "false"))) return(tolower(x) %in% c("1", "true") & !is.na(x) | ifelse(is.na(x), NA, FALSE))
  x
}

# The clinical data of an ODM document as a data frame: subject_key, event_oid, event_repeat,
# repeat_instance, then one column per item that has data (metadata order first).
.sr_odm_records <- function(doc) {
  subjects <- xml2::xml_find_all(doc, "//ClinicalData/SubjectData")
  if (!length(subjects)) stop("the ODM file has no clinical data (no SubjectData)", call. = FALSE)
  keys <- character()
  meta <- list()
  r_idx <- integer()
  r_item <- character()
  r_val <- character()
  rep_attr <- function(node, nm) {
    a <- xml2::xml_attr(node, nm)
    if (is.na(a) || !nzchar(a)) NA_character_ else a
  }
  for (sd in subjects) {
    subj <- xml2::xml_attr(sd, "SubjectKey")
    events <- xml2::xml_find_all(sd, "./StudyEventData")
    if (!length(events)) events <- list(sd)
    for (ev in events) {
      is_ev <- inherits(ev, "xml_node") && identical(xml2::xml_name(ev), "StudyEventData")
      ev_oid <- if (is_ev) rep_attr(ev, "StudyEventOID") else NA_character_
      ev_rep <- if (is_ev) rep_attr(ev, "StudyEventRepeatKey") else NA_character_
      forms <- xml2::xml_find_all(ev, "./FormData")
      if (!length(forms)) forms <- xml2::xml_find_all(ev, ".//FormData")
      for (fd in forms) {
        f_rep <- rep_attr(fd, "FormRepeatKey")
        groups <- xml2::xml_find_all(fd, "./ItemGroupData")
        if (!length(groups)) groups <- list(fd)
        for (ig in groups) {
          g_rep <- if (identical(xml2::xml_name(ig), "ItemGroupData")) rep_attr(ig, "ItemGroupRepeatKey") else NA_character_
          nodes <- xml2::xml_find_all(ig, "./ItemData")
          if (!length(nodes)) next
          key <- paste(subj, ev_oid, ev_rep, f_rep, g_rep, sep = "\r")
          idx <- match(key, keys)
          if (is.na(idx)) {
            keys <- c(keys, key)
            idx <- length(keys)
            inst <- if (!is.na(f_rep)) f_rep else g_rep
            meta[[idx]] <- c(subj, ev_oid, ev_rep, inst)
          }
          val <- xml2::xml_attr(nodes, "Value")
          val[!is.na(val) & !nzchar(val)] <- NA_character_
          r_idx <- c(r_idx, rep(idx, length(nodes)))
          r_item <- c(r_item, xml2::xml_attr(nodes, "ItemOID"))
          r_val <- c(r_val, val)
        }
      }
    }
  }
  if (!length(keys)) stop("the ODM file has no clinical data (no ItemData)", call. = FALSE)
  ok <- !is.na(r_item)
  r_idx <- r_idx[ok]
  r_item <- r_item[ok]
  r_val <- r_val[ok]
  mdef <- xml2::xml_find_all(doc, "//MetaDataVersion/ItemDef")
  moid <- xml2::xml_attr(mdef, "OID")
  mtype <- stats::setNames(xml2::xml_attr(mdef, "DataType"), moid)
  seen <- unique(r_item)
  items <- c(intersect(moid, seen), setdiff(seen, moid))
  m <- matrix(NA_character_, nrow = length(keys), ncol = length(items))
  m[cbind(r_idx, match(r_item, items))] <- r_val
  mm <- do.call(rbind, meta)
  mnames <- c("subject_key", "event_oid", "event_repeat", "repeat_instance")
  while (any(mnames %in% items)) mnames[mnames %in% items] <- paste0("odm_", mnames[mnames %in% items])
  out <- list()
  for (k in 1:4) out[[mnames[k]]] <- mm[, k]
  for (j in seq_along(items)) {
    dt <- mtype[items[j]]
    out[[items[j]]] <- .sr_odm_coerce(m[, j], if (is.na(dt)) "" else dt)
  }
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

# The reader behind `.sr_reader_register("odm", ...)`: one part, not resumable. The whole document is
# parsed in memory.
.sr_read_odm <- function(project, sniff, table, ck, options, progress, cancel) {
  .sr_col_nocomp(sniff, "ODM XML")
  .sr_col_single(project, sniff, ck, progress, cancel, function(con, to) {
    doc <- .sr_odm_read(sniff$path)
    .sr_col_frame_part(con, .sr_odm_records(doc), to, "the ODM data")
  })
}

.sr_reader_register("odm", .sr_read_odm)
