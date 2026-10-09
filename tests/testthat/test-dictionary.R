# Tests for io/dictionary.R and io/redcap_dict.R: the dictionary shape, generic CSV, REDCap data
# dictionary CSV, REDCap ODM XML metadata, labels carried by the source file, the ODM data reader, and
# storage (data/dict/<table>.json + the `dict.set` event). Synthetic data only; everything lives under
# withr::local_tempdir(). Non-ASCII text is built with intToUtf8() (source files are ASCII).

DC_DICT_COLS <- c("variable", "label", "type", "units", "min", "max", "levels", "ordered",
                  "missing_codes", "source")
DC_TYPES <- c("numeric", "integer", "categorical", "date", "datetime", "text", "logical")

.dc_fx <- function(name) normalizePath(testthat::test_path("fixtures", "redcap", name), winslash = "/")

.dc_project <- function(env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  p
}

.dc_ing <- function(p, path, ..., options = list()) {
  options$drive_info <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())
  sr_ingest(p, path, ..., options = options)
}

.dc_all <- function(p, table) {
  m <- sr_store_manifest(p, table)
  cols <- vapply(m$schema, function(s) s$name, "")
  sr_table_page(p, table, 0, 1000, order_by = cols)
}

.dc_schema <- function(p, table) {
  t <- p$state$tables[[table]]$store$schema
  stats::setNames(vapply(t, function(x) x$type, ""), vapply(t, function(x) x$name, ""))
}

# A valid dictionary with one text row per name (every optional piece empty).
.dc_mk <- function(variable, type = "text") {
  n <- length(variable)
  d <- data.frame(variable = variable, label = variable, type = type, units = "", min = "", max = "",
                  stringsAsFactors = FALSE)
  d$levels <- rep(list(data.frame(code = character(), label = character(), stringsAsFactors = FALSE)), n)
  d$missing_codes <- rep(list(character()), n)
  d$source <- "test"
  d
}

.dc_lv <- function(code, label = code) data.frame(code = code, label = label, stringsAsFactors = FALSE)

.dc_write <- function(lines, name, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  f <- file.path(dir, name)
  writeLines(lines, f)
  f
}

.dc_by_var <- function(d) {
  d <- d[order(d$variable), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# ---- shape ----------------------------------------------------------------------------------------

test_that("sr_dict_empty has the dictionary columns and no rows", {
  d <- sr_dict_empty()
  expect_identical(names(d), DC_DICT_COLS)
  expect_equal(nrow(d), 0L)
  expect_true(is.list(d$levels) && is.list(d$missing_codes))
  expect_identical(sr_dict_validate(d), d)
})

test_that("sr_dict_validate errors on a missing column, a duplicate variable and a bad type", {
  ok <- .dc_mk(c("a", "b"))
  expect_error(sr_dict_validate(ok[, setdiff(names(ok), "units")]), "units")
  expect_error(sr_dict_validate(ok[, setdiff(names(ok), "levels")]), "levels")
  dup <- .dc_mk(c("a", "a"))
  expect_error(sr_dict_validate(dup), "duplicate")
  bad <- .dc_mk(c("a", "b"), type = c("numeric", "weird"))
  expect_error(sr_dict_validate(bad), "type")
  blank <- .dc_mk(c("a", ""))
  expect_error(sr_dict_validate(blank), "variable")
  expect_error(sr_dict_validate(list(a = 1)), "data frame")
  worse <- ok
  worse$levels[[1]] <- data.frame(code = "1", stringsAsFactors = FALSE)
  expect_error(sr_dict_validate(worse), "levels")
})

test_that("sr_dict_validate fills the optional ordered column and keeps min and max as text", {
  ok <- .dc_mk(c("a", "b"))
  v <- sr_dict_validate(ok)
  expect_identical(v$ordered, c(FALSE, FALSE))
  expect_identical(names(v), DC_DICT_COLS)
  ok$min <- c(NA, "3")
  v2 <- sr_dict_validate(ok)
  expect_identical(v2$min, c("", "3"))
  ok$ordered <- c(TRUE, FALSE)
  expect_identical(sr_dict_validate(ok)$ordered, c(TRUE, FALSE))
  ok$min <- c(1, 2)
  expect_identical(sr_dict_validate(ok)$min, c("1", "2"))
})

# ---- generic CSV ----------------------------------------------------------------------------------

test_that("generic CSV: levels given as 'code=label|code=label' parse, order is kept", {
  f <- .dc_write(c("variable,label,type,units,min,max,levels",
                   "sex,Sex,categorical,,,,1=Male|2=Female",
                   "age,Age,numeric,years,0,120,",
                   "grade,Grade,categorical,,,,3=High|1=Low|2=Mid"), "d.csv")
  d <- sr_dict_read_csv(f)
  expect_silent(sr_dict_validate(d))
  expect_identical(d$variable, c("sex", "age", "grade"))
  expect_identical(d$levels[[1]], .dc_lv(c("1", "2"), c("Male", "Female")))
  expect_equal(nrow(d$levels[[2]]), 0L)
  expect_identical(d$levels[[3]]$code, c("3", "1", "2"))
  expect_identical(d$min[2], "0")
  expect_identical(d$max[2], "120")
  expect_identical(d$units[2], "years")
  expect_false(any(d$ordered))
  expect_identical(d$source, rep("csv", 3L))
})

test_that("generic CSV: missing_codes and ordered columns are read when present", {
  f <- .dc_write(c("variable,type,levels,ordered,missing_codes",
                   "stage,categorical,A|B|C,yes,-9|-8",
                   "x,numeric,,FALSE,-1"), "d.csv")
  d <- sr_dict_read_csv(f)
  expect_identical(d$ordered, c(TRUE, FALSE))
  expect_identical(d$missing_codes[[1]], c("-9", "-8"))
  expect_identical(d$missing_codes[[2]], "-1")
  expect_identical(d$label, c("stage", "x"))               # no label column: the variable name
})

test_that("generic CSV: a missing variable column or an unknown type is refused", {
  f <- .dc_write(c("name,type", "a,numeric"), "d.csv")
  expect_error(sr_dict_read_csv(f), "variable")
  g <- .dc_write(c("variable,type", "a,quantum"), "g.csv")
  expect_error(sr_dict_read_csv(g), "type")
})

test_that("the sample dictionary (id / ordinal / binary / pipe levels) reads and validates", {
  s <- .sr_sample_dict()
  csv <- file.path(withr::local_tempdir(), "sample_dict.csv")
  .fx_write_csv(s, csv)
  d <- sr_dict_read_csv(csv)
  expect_silent(sr_dict_validate(d))
  expect_equal(nrow(d), nrow(s))
  expect_identical(d$variable, s$variable)
  expect_true(all(d$type %in% DC_TYPES))
  ny <- d[d$variable == "nyha", ]
  expect_equal(ny$type, "categorical")
  expect_true(ny$ordered)
  expect_identical(ny$levels[[1]], .dc_lv(c("I", "II", "III", "IV")))
  expect_equal(d$type[d$variable == "patient_id"], "text")                  # id -> text
  db <- d[d$variable == "diabetes", ]
  expect_equal(db$type, "categorical")                                      # binary -> categorical
  expect_false(db$ordered)
  expect_identical(db$levels[[1]], .dc_lv(c("0", "1")))
  expect_identical(d$levels[[which(d$variable == "sex")]], .dc_lv(c("F", "M")))   # code = label = value
  expect_identical(d$levels[[which(d$variable == "site")]]$code, SR_SAMPLE_SITES)
  expect_identical(d$min[d$variable == "admit_date"], "2015-01-01")         # date bounds stay ISO text
  expect_identical(d$max[d$variable == "age"], "110")
  expect_identical(d$ordered[d$variable != "nyha"], rep(FALSE, nrow(d) - 1L))
})

# ---- REDCap data dictionary CSV -------------------------------------------------------------------

test_that(".sr_redcap_choices parses 'code, label | code, label'", {
  x <- .sr_redcap_choices("1, Male | 2, Female")
  expect_identical(x, .dc_lv(c("1", "2"), c("Male", "Female")))
  y <- .sr_redcap_choices("1, Yes, definitely | 0, No")
  expect_identical(y$label, c("Yes, definitely", "No"))
  expect_identical(.sr_redcap_choices("a,<i>Alpha</i>|b, Beta ")$label, c("Alpha", "Beta"))
  expect_null(.sr_redcap_choices(""))
  expect_null(.sr_redcap_choices("[weight]/2"))
})

test_that("REDCap CSV: radio, checkbox expansion, integer with bounds, HTML label", {
  d <- sr_dict_read_redcap(.dc_fx("dd.csv"))
  expect_silent(sr_dict_validate(d))
  expect_identical(d$variable, c("record_id", "age", "sex", "comorb___1", "comorb___2", "comorb___3",
                                 "visit_date", "weight", "smoker", "notes", "bmi"))
  g <- function(v, col) d[[col]][d$variable == v]
  expect_equal(g("age", "label"), "Age")                                     # <b>Age</b> stripped
  expect_equal(g("age", "type"), "integer")
  expect_equal(g("age", "min"), "18")
  expect_equal(g("age", "max"), "100")
  expect_equal(g("sex", "type"), "categorical")
  expect_identical(d$levels[[which(d$variable == "sex")]], .dc_lv(c("1", "2"), c("Male", "Female")))
  for (k in 1:3) {
    v <- paste0("comorb___", k)
    expect_equal(g(v, "type"), "logical")
    expect_equal(nrow(d$levels[[which(d$variable == v)]]), 0L)
  }
  expect_equal(g("comorb___3", "label"), "Comorbidities (choice=CKD)")
  expect_false("comorb" %in% d$variable)
  expect_equal(g("record_id", "type"), "text")
  expect_equal(g("weight", "type"), "numeric")
  expect_equal(g("weight", "min"), "30")
  expect_equal(g("visit_date", "type"), "date")
  expect_equal(g("visit_date", "min"), "2020-01-01")                         # dmy bound -> ISO text
  expect_equal(g("visit_date", "max"), "2025-12-31")
  expect_equal(g("visit_date", "label"), "Visit date")                       # whitespace collapsed
  expect_equal(g("smoker", "type"), "categorical")
  expect_identical(d$levels[[which(d$variable == "smoker")]], .dc_lv(c("1", "0"), c("Yes", "No")))
  expect_equal(g("notes", "type"), "text")
  expect_equal(g("bmi", "type"), "numeric")
  expect_equal(nrow(d$levels[[which(d$variable == "bmi")]]), 0L)             # a calc formula is not a choice list
  expect_true(all(d$source == "redcap"))
  expect_false(any(d$ordered))
})

test_that("REDCap CSV: validation types map to dictionary types and a BOM header reads", {
  hdr <- paste0(intToUtf8(65279L), '"Variable / Field Name","Form Name","Field Type","Field Label",',
                '"Choices, Calculations, OR Slider Labels","Text Validation Type OR Show Slider Number",',
                '"Text Validation Min","Text Validation Max"')
  rows <- c('"a","f","text","A","","number_1dp","",""',
            '"b","f","text","B","","datetime_seconds_ymd","2020-01-01 00:00:00","2020-12-31 23:59:59"',
            '"c","f","text","C","","date_mdy","12-31-2020",""',
            '"d","f","text","D","","email","",""',
            '"e","f","dropdown","E","y, Yes | n, No","","",""',
            '"f","f","truefalse","F","","","",""',
            '"g","f","descriptive","Just text","","","",""',
            '"h","f","slider","H","","","",""',
            '"i","f","text","I","","time","",""',
            '"j","f","file","J","","","",""')
  f <- file.path(withr::local_tempdir(), "dd.csv")
  con <- file(f, "wb")
  writeBin(charToRaw(enc2utf8(paste(c(hdr, rows), collapse = "\n"))), con)
  close(con)
  d <- sr_dict_read_redcap(f)
  g <- function(v, col) d[[col]][d$variable == v]
  expect_equal(g("a", "type"), "numeric")
  expect_equal(g("b", "type"), "datetime")
  expect_equal(g("b", "min"), "2020-01-01T00:00:00")
  expect_equal(g("c", "type"), "date")
  expect_equal(g("c", "min"), "2020-12-31")
  expect_equal(g("d", "type"), "text")
  expect_equal(g("e", "type"), "categorical")
  expect_identical(d$levels[[which(d$variable == "e")]], .dc_lv(c("y", "n"), c("Yes", "No")))
  expect_identical(d$levels[[which(d$variable == "f")]], .dc_lv(c("1", "0"), c("True", "False")))
  expect_false("g" %in% d$variable)                                          # descriptive: no data
  expect_equal(g("h", "type"), "numeric")
  expect_equal(g("i", "type"), "text")
  expect_equal(g("j", "type"), "text")
})

test_that("REDCap CSV: a file without the variable column is refused", {
  f <- .dc_write(c("Field Label,Field Type", "A,text"), "dd.csv")
  expect_error(sr_dict_read_redcap(f), "variable")
})

# ---- REDCap ODM XML metadata ----------------------------------------------------------------------

test_that("ODM metadata gives the same dictionary as the REDCap CSV (identical after sorting)", {
  a <- .dc_by_var(sr_dict_read_redcap(.dc_fx("dd.csv")))
  b <- .dc_by_var(sr_dict_read_odm(.dc_fx("odm.xml")))
  expect_silent(sr_dict_validate(b))
  expect_identical(b, a)
  expect_identical(b$label[b$variable == "age"], "Age")
  expect_identical(b$label[b$variable == "visit_date"], "Visit date")
})

test_that("ODM metadata: a generic (non-REDCap) ODM maps DataType, code lists and range checks", {
  f <- .dc_write(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<ODM ODMVersion="1.3.2" FileOID="g1" FileType="Snapshot">',
    '<Study OID="s"><MetaDataVersion OID="m" Name="m">',
    '<ItemDef OID="n" Name="n" DataType="integer"><Description><TranslatedText>Count</TranslatedText></Description>',
    '<RangeCheck Comparator="GT" SoftHard="Hard"><CheckValue>0</CheckValue></RangeCheck></ItemDef>',
    '<ItemDef OID="x" Name="x Label" DataType="float"/>',
    '<ItemDef OID="d" Name="d" DataType="date"/>',
    '<ItemDef OID="dt" Name="dt" DataType="datetime"/>',
    '<ItemDef OID="b" Name="b" DataType="boolean"/>',
    '<ItemDef OID="s1" Name="s1" DataType="text"><CodeListRef CodeListOID="cl1"/></ItemDef>',
    '<CodeList OID="cl1" Name="cl1" DataType="text">',
    '<CodeListItem CodedValue="a"><Decode><TranslatedText>Alpha</TranslatedText></Decode></CodeListItem>',
    '<CodeListItem CodedValue="b"><Decode><TranslatedText>Beta</TranslatedText></Decode></CodeListItem>',
    '</CodeList></MetaDataVersion></Study></ODM>'), "g.xml")
  d <- sr_dict_read_odm(f)
  expect_silent(sr_dict_validate(d))
  g <- function(v, col) d[[col]][d$variable == v]
  expect_equal(g("n", "type"), "integer")
  expect_equal(g("n", "min"), "0")
  expect_equal(g("x", "type"), "numeric")
  expect_equal(g("x", "label"), "x Label")
  expect_equal(g("d", "type"), "date")
  expect_equal(g("dt", "type"), "datetime")
  expect_equal(g("b", "type"), "logical")
  expect_equal(g("s1", "type"), "categorical")
  expect_identical(d$levels[[which(d$variable == "s1")]], .dc_lv(c("a", "b"), c("Alpha", "Beta")))
  expect_true(all(d$source == "odm"))
})

test_that("ODM metadata: a checkbox ItemDef with a code list expands to <var>___<code> logicals", {
  f <- .dc_write(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3" xmlns:redcap="https://projectredcap.org" ODMVersion="1.3.1">',
    '<Study OID="s"><MetaDataVersion OID="m" Name="m">',
    '<ItemDef OID="comorb" Name="comorb" DataType="text" redcap:FieldType="checkbox">',
    '<Question><TranslatedText>Comorbidities</TranslatedText></Question><CodeListRef CodeListOID="cl"/></ItemDef>',
    '<CodeList OID="cl" Name="cl" DataType="text">',
    '<CodeListItem CodedValue="1"><Decode><TranslatedText>Diabetes</TranslatedText></Decode></CodeListItem>',
    '<CodeListItem CodedValue="2"><Decode><TranslatedText>CKD</TranslatedText></Decode></CodeListItem>',
    '</CodeList></MetaDataVersion></Study></ODM>'), "c.xml")
  d <- sr_dict_read_odm(f)
  expect_identical(d$variable, c("comorb___1", "comorb___2"))
  expect_identical(d$type, c("logical", "logical"))
  expect_identical(d$label, c("Comorbidities (choice=Diabetes)", "Comorbidities (choice=CKD)"))
})

test_that("ODM metadata: a file with no ItemDef, or that is not ODM, is refused", {
  f <- .dc_write(c('<?xml version="1.0"?>', '<ODM><Study OID="s"><MetaDataVersion OID="m" Name="m"/></Study></ODM>'), "e.xml")
  expect_error(sr_dict_read_odm(f), "ItemDef")
  g <- .dc_write(c('<?xml version="1.0"?>', '<other><a/></other>'), "o.xml")
  expect_error(sr_dict_read_odm(g), "ODM")
})

test_that("ODM parsing never loads an external entity or DTD (no NOENT, no DTDLOAD, NONET)", {
  td <- withr::local_tempdir()
  secret <- file.path(td, "secret.txt")
  writeLines("TOP-SECRET-MARKER-XYZ", secret)
  url <- paste0("file:///", sub("^/", "", normalizePath(secret, winslash = "/")))
  body <- c('<ODM ODMVersion="1.3.2" FileOID="x" FileType="Snapshot"><Study OID="s">',
            '<MetaDataVersion OID="m" Name="m">',
            '<ItemDef OID="v1" Name="v1" DataType="text"><Question><TranslatedText>A &xxe; B</TranslatedText></Question></ItemDef>',
            '</MetaDataVersion></Study></ODM>')
  f1 <- .dc_write(c('<?xml version="1.0"?>',
                    sprintf('<!DOCTYPE ODM [<!ENTITY xxe SYSTEM "%s">]>', url), body), "xxe.xml", td)
  f2 <- .dc_write(c('<?xml version="1.0"?>', '<!DOCTYPE ODM SYSTEM "http://127.0.0.1:9/odm.dtd">', body), "dtd.xml", td)
  for (f in c(f1, f2)) {
    t0 <- Sys.time()
    res <- tryCatch(sr_dict_read_odm(f), error = function(e) e)
    expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 20)
    if (inherits(res, "error")) {
      expect_true(nzchar(conditionMessage(res)))
      expect_false(grepl("TOP-SECRET", conditionMessage(res), fixed = TRUE))
    } else {
      expect_false(any(grepl("TOP-SECRET", unlist(res[c("variable", "label")]), fixed = TRUE)))
    }
  }
})

# ---- ODM data reader ------------------------------------------------------------------------------

test_that("the ODM data reader ingests 3 subjects x 2 events as 6 rows, one per subject and event", {
  p <- .dc_project()
  expect_equal(sr_sniff(.dc_fx("odm.xml"))$format, "odm")
  p <- .dc_ing(p, .dc_fx("odm.xml"), table = "trial")
  expect_equal(p$state$tables$trial$store$rows, 6)
  expect_false(isTRUE(p$state$tables$trial$resumable))
  d <- .dc_all(p, "trial")
  d <- d[order(d$subject_key, d$event_oid), ]
  expect_equal(nrow(d), 6L)
  expect_identical(names(d)[1:4], c("subject_key", "event_oid", "event_repeat", "repeat_instance"))
  expect_equal(as.character(d$subject_key), rep(c("S01", "S02", "S03"), each = 2))
  expect_equal(as.character(d$event_oid), rep(c("baseline_arm_1", "followup_arm_1"), 3))
  sc <- .dc_schema(p, "trial")
  expect_equal(sc[["age"]], "INTEGER")
  expect_equal(sc[["weight"]], "DOUBLE")
  expect_equal(sc[["visit_date"]], "DATE")
  expect_equal(sc[["comorb___1"]], "BOOLEAN")
  expect_equal(sc[["sex"]], "VARCHAR")
  expect_true(all(c("record_id", "age", "sex", "comorb___1", "comorb___2", "comorb___3", "visit_date", "weight") %in% names(d)))
  expect_false(any(c("smoker", "notes", "bmi") %in% names(d)))               # no data, no column
  r1 <- d[1, ]                                                              # S01 baseline: both forms in one row
  expect_equal(as.character(r1$record_id), "S01")
  expect_equal(r1$age, 54L)
  expect_equal(as.character(r1$sex), "1")
  expect_true(r1$comorb___1)
  expect_false(r1$comorb___2)
  expect_equal(r1$weight, 82.5)
  expect_equal(as.character(r1$visit_date), "2024-03-05")
  expect_true(is.na(d$record_id[2]))                                        # follow-up has no demographics
  expect_true(is.na(d$age[5]))                                              # S03 blank -> NA, not ""
  expect_true(all(is.na(d$event_repeat)))
  # the dictionary read from the same file matches the table's columns
  p <- sr_dict_set(p, "trial", sr_dict_read_odm(.dc_fx("odm.xml")), source = "odm")
  m <- sr_dict_match(p, "trial")
  expect_setequal(m$variable[m$in_data & !m$in_dict], c("subject_key", "event_oid", "event_repeat", "repeat_instance"))
  expect_setequal(m$variable[!m$in_data & m$in_dict], c("smoker", "notes", "bmi"))
})

test_that("the ODM data reader keeps repeat instances apart and never turns a bad number into NA", {
  f <- .dc_write(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<ODM ODMVersion="1.3.2" FileOID="r1" FileType="Snapshot"><Study OID="s"><MetaDataVersion OID="m" Name="m">',
    '<ItemDef OID="lab" Name="lab" DataType="float"/><ItemDef OID="k" Name="k" DataType="integer"/>',
    '</MetaDataVersion></Study>',
    '<ClinicalData StudyOID="s" MetaDataVersion="m">',
    '<SubjectData SubjectKey="P1"><StudyEventData StudyEventOID="ev1">',
    '<FormData FormOID="Form.lab" FormRepeatKey="1"><ItemGroupData ItemGroupOID="g"><ItemData ItemOID="lab" Value="1.5"/><ItemData ItemOID="k" Value="1"/></ItemGroupData></FormData>',
    '<FormData FormOID="Form.lab" FormRepeatKey="2"><ItemGroupData ItemGroupOID="g"><ItemData ItemOID="lab" Value="pending"/><ItemData ItemOID="k" Value="2"/></ItemGroupData></FormData>',
    '</StudyEventData></SubjectData></ClinicalData></ODM>'), "r.xml")
  p <- .dc_project()
  p <- .dc_ing(p, f, table = "rep")
  d <- .dc_all(p, "rep")
  d <- d[order(d$repeat_instance), ]
  expect_equal(nrow(d), 2L)
  expect_equal(as.character(d$repeat_instance), c("1", "2"))
  expect_equal(.dc_schema(p, "rep")[["lab"]], "VARCHAR")                    # one non-number: the column stays text
  expect_equal(as.character(d$lab), c("1.5", "pending"))
  expect_equal(.dc_schema(p, "rep")[["k"]], "INTEGER")
})

test_that("the ODM data reader refuses a file with no clinical data", {
  f <- .dc_write(c('<?xml version="1.0"?>', '<ODM><Study OID="s"><MetaDataVersion OID="m" Name="m"/></Study></ODM>'), "n.xml")
  p <- .dc_project()
  expect_error(.dc_ing(p, f, table = "nodata"), "clinical data")
})

# ---- storage --------------------------------------------------------------------------------------

test_that("sr_dict_set writes data/dict/<table>.json and a dict.set event; reopening gives the same dict", {
  p <- .dc_project()
  d <- sr_dict_read_redcap(.dc_fx("dd.csv"))
  p <- sr_dict_set(p, "trial", d, source = "redcap")
  f <- file.path(p$paths$dir, "data", "dict", "trial.json")
  expect_true(file.exists(f))
  expect_length(list.files(dirname(f), pattern = "tmp$"), 0L)
  evs <- sr_events_read(p$paths$events)
  ev <- evs[[length(evs)]]
  expect_equal(ev$type, "dict.set")
  expect_equal(ev$payload$table, "trial")
  expect_equal(ev$payload$file, "data/dict/trial.json")
  expect_equal(ev$payload$sha256, sr_sha256_file(f))
  expect_equal(ev$payload$n, nrow(d))
  expect_equal(ev$payload$source, "redcap")
  expect_equal(p$state$dict$trial$sha256, sr_sha256_file(f))
  expect_equal(p$state$dict$trial$n, nrow(d))
  expect_identical(sr_dict_get(p, "trial"), sr_dict_validate(d))
  dir <- p$paths$dir
  sr_project_close(p)
  p2 <- sr_project_open(dir)
  withr::defer(sr_project_close(p2))
  expect_identical(sr_dict_get(p2, "trial"), sr_dict_validate(d))
  expect_equal(p2$state$dict$trial$file, "data/dict/trial.json")
  expect_null(sr_dict_get(p2, "other"))
})

test_that("the stored JSON round-trips levels, missing_codes, ordered, character min/max and non-ASCII text", {
  p <- .dc_project()
  d <- .dc_mk(c("a", "b", "c", "d"), type = c("categorical", "numeric", "date", "categorical"))
  d$label[1] <- paste0("Sexe ", intToUtf8(c(233L, 8364L)))
  d$levels[[1]] <- .dc_lv(c("2", "1", "0"), c(paste0("F", intToUtf8(233L)), "M", "Unknown"))
  d$levels[[4]] <- .dc_lv("only")                                  # one level: still a data frame
  d$ordered <- c(TRUE, FALSE, FALSE, FALSE)
  d$missing_codes[[2]] <- "-9"                                      # one code: still a list element of length 1
  d$missing_codes[[3]] <- c("-9", "-8")
  d$min <- c("", "0.10", "2015-01-01", "")
  d$max <- c("", "1e3", "2025-12-31", "")
  d$units <- c("", "kg", "", "")
  p <- sr_dict_set(p, "t", d, source = "manual")
  g <- sr_dict_get(p, "t")
  expect_identical(g, sr_dict_validate(d))
  expect_identical(g$levels[[1]]$code, c("2", "1", "0"))
  expect_identical(g$levels[[4]], .dc_lv("only"))
  expect_identical(g$missing_codes[[2]], "-9")
  expect_identical(g$missing_codes[[4]], character())
  expect_identical(g$min[2], "0.10")
  expect_identical(g$max[2], "1e3")
  expect_identical(g$label[1], paste0("Sexe ", intToUtf8(c(233L, 8364L))))
  expect_true(all(validUTF8(g$label)))
})

test_that("sr_dict_set replaces a table's dictionary (the event reducer merges its entry) and keeps other tables", {
  p <- .dc_project()
  p <- sr_dict_set(p, "one", .dc_mk(c("a", "b")), source = "csv")
  p <- sr_dict_set(p, "two", .dc_mk("z"), source = "csv")
  p <- sr_dict_set(p, "one", .dc_mk(c("a", "b", "c")), source = "manual")
  expect_equal(p$state$dict$one$n, 3L)
  expect_equal(p$state$dict$one$source, "manual")
  expect_equal(p$state$dict$two$n, 1L)
  expect_identical(sr_dict_get(p, "one")$variable, c("a", "b", "c"))
  ev <- sr_project_append(p, "dict.set", list(table = "one", note = "extra"))
  expect_equal(ev$state$dict$one$note, "extra")
  expect_equal(ev$state$dict$one$n, 3L)                              # merged, not replaced
})

test_that("sr_dict_get verifies the file's SHA-256 against state and names the table on a mismatch", {
  p <- .dc_project()
  p <- sr_dict_set(p, "trial", .dc_mk(c("a", "b")), source = "csv")
  f <- file.path(p$paths$dir, "data", "dict", "trial.json")
  cat(" ", file = f, append = TRUE)
  expect_error(sr_dict_get(p, "trial"), "trial")
  expect_error(sr_dict_get(p, "trial"), "sha|hash|changed", ignore.case = TRUE)
  unlink(f)
  expect_error(sr_dict_get(p, "trial"), "trial")
})

test_that("sr_dict_set refuses a bad table name, and a bad dictionary", {
  p <- .dc_project()
  expect_error(sr_dict_set(p, "Bad Name", .dc_mk("a"), source = "csv"), "table")
  expect_error(sr_dict_set(p, "t", .dc_mk(c("a", "a")), source = "csv"), "duplicate")
  expect_error(sr_dict_set(p, "t", .dc_mk("a"), source = NA_character_), "source")
  expect_false(file.exists(file.path(p$paths$dir, "data", "dict", "t.json")))
})

# ---- labels carried by the source -----------------------------------------------------------------

test_that("sr_dict_from_labels turns haven labels into levels (in label order) and missing_codes", {
  dl <- list(
    sex = list(label = "Sex of patient",
               values = data.frame(code = c("2", "1"), label = c("Female", "Male"), order = c(2L, 1L),
                                   stringsAsFactors = FALSE),
               missing_codes = NULL, kind = "labelled"),
    score = list(label = "Score", values = NULL, missing_codes = "-9", kind = "double"),
    n = list(label = "Count", values = NULL, missing_codes = c("-1", "-2"), kind = "integer"),
    when = list(label = "When", values = NULL, missing_codes = NULL, kind = "date"),
    anon = list(label = NA_character_,
                values = data.frame(code = c("a", "b"), label = c("A", "B"), stringsAsFactors = FALSE),
                missing_codes = NULL))
  d <- sr_dict_from_labels(dl)
  expect_silent(sr_dict_validate(d))
  expect_identical(d$variable, names(dl))
  g <- function(v, col) d[[col]][d$variable == v]
  expect_equal(g("sex", "label"), "Sex of patient")
  expect_equal(g("sex", "type"), "categorical")
  expect_identical(d$levels[[1]], .dc_lv(c("1", "2"), c("Male", "Female")))
  expect_equal(g("score", "type"), "numeric")
  expect_identical(d$missing_codes[[2]], "-9")
  expect_equal(g("n", "type"), "integer")
  expect_identical(d$missing_codes[[3]], c("-1", "-2"))
  expect_equal(g("when", "type"), "date")
  expect_equal(g("anon", "label"), "anon")                           # no variable label: the name
  expect_equal(g("anon", "type"), "categorical")
  expect_identical(d$levels[[5]]$code, c("a", "b"))
  expect_true(all(d$source == "source"))
  expect_equal(nrow(sr_dict_from_labels(list())), 0L)
})

test_that("ingesting a .sav with value labels stores a dictionary with its levels and missing_codes", {
  p <- .dc_project()
  d <- data.frame(id = 1:30)
  d$sex <- haven::labelled(rep(c(1, 2), 15), c(Male = 1, Female = 2), label = "Sex of patient")
  sc <- seq_len(30) * 0.5
  sc[c(4, 9)] <- -9
  d$score <- haven::labelled_spss(sc, na_values = -9, label = "Score")
  d$age <- 20L + 1:30
  f <- file.path(dirname(p$paths$dir), "src", "fx.sav")
  dir.create(dirname(f))
  haven::write_sav(d, f)
  p <- .dc_ing(p, f, table = "fx")
  expect_true(p$state$tables$fx$dict_from_source)
  expect_equal(p$state$dict$fx$source, "source")
  expect_true(file.exists(file.path(p$paths$dir, "data", "dict", "fx.json")))
  g <- sr_dict_get(p, "fx")
  expect_silent(sr_dict_validate(g))
  expect_setequal(g$variable, c("sex", "score"))                     # a column with no labels has no entry
  sx <- g[g$variable == "sex", ]
  expect_equal(sx$type, "categorical")
  expect_equal(sx$label, "Sex of patient")
  expect_identical(sx$levels[[1]], .dc_lv(c("1", "2"), c("Male", "Female")))
  sco <- g[g$variable == "score", ]
  expect_identical(sco$missing_codes[[1]], "-9")
  expect_equal(sco$type, "numeric")
  expect_equal(sco$label, "Score")
  expect_identical(g$source, c("source", "source"))
})

# ---- matching -------------------------------------------------------------------------------------

test_that("sr_dict_match on the cardiac sample with its dictionary lists no unmatched names", {
  p <- .fx_cardiac_project(n = 200L)
  withr::defer(sr_duck_close(p))
  csv <- file.path(withr::local_tempdir(), "dict.csv")
  .fx_write_csv(.sr_sample_dict(), csv)
  p <- sr_dict_set(p, "cardiac", sr_dict_read_csv(csv), source = "csv")
  m <- sr_dict_match(p, "cardiac")
  expect_identical(names(m), c("variable", "in_data", "in_dict"))
  expect_equal(nrow(m), nrow(.sr_sample_dict()))
  expect_true(all(m$in_data))
  expect_true(all(m$in_dict))
  expect_identical(m$variable, unique(m$variable))
})

test_that("sr_dict_match shows names that are unmatched in either direction, and a table with no dictionary", {
  p <- .fx_cardiac_project(n = 200L)
  withr::defer(sr_duck_close(p))
  none <- sr_dict_match(p, "cardiac")
  expect_true(all(none$in_data))
  expect_false(any(none$in_dict))
  d <- .dc_mk(c("age", "sex", "ghost_var"))
  p <- sr_dict_set(p, "cardiac", d, source = "manual")
  m <- sr_dict_match(p, "cardiac")
  expect_identical(m$variable[!m$in_dict], setdiff(m$variable[m$in_data], c("age", "sex")))
  expect_identical(m$variable[!m$in_data], "ghost_var")
  expect_true(all(m$in_dict[m$variable %in% c("age", "sex")]))
  expect_error(sr_dict_match(p, "no_such_table"), "no_such_table")
})

# ---- fix round 1 ----------------------------------------------------------------------------------

# Put `value` in the global environment under `name` (NULL removes it) until `env` ends.
.dc_swap <- function(name, value, env = parent.frame()) {
  had <- exists(name, envir = globalenv(), inherits = FALSE)
  orig <- if (had) get(name, envir = globalenv(), inherits = FALSE) else NULL
  if (is.null(value)) suppressWarnings(rm(list = name, envir = globalenv())) else assign(name, value, envir = globalenv())
  withr::defer({
    if (had) assign(name, orig, envir = globalenv()) else suppressWarnings(rm(list = name, envir = globalenv()))
  }, envir = env)
  invisible(NULL)
}

test_that("sr_dict_from_labels drops labels with an empty or missing code and keeps the first of a duplicate code", {
  dl <- list(
    s = list(label = "S",
             values = data.frame(code = c("a", "", "b", "a", NA), label = c("A", "Blank", "B", "A again", "NA code"),
                                 order = 1:5, stringsAsFactors = FALSE),
             missing_codes = NULL, kind = "character"),
    only_blank = list(label = "OB",
                      values = data.frame(code = "", label = "Blank", order = 1L, stringsAsFactors = FALSE),
                      missing_codes = NULL, kind = "character"))
  d <- sr_dict_from_labels(dl)
  expect_silent(sr_dict_validate(d))
  expect_identical(d$levels[[1]], .dc_lv(c("a", "b"), c("A", "B")))
  expect_equal(nrow(d$levels[[2]]), 0L)                              # nothing usable left: no levels
  expect_equal(d$type[2], "text")
})

test_that("a dictionary error during ingest is a warning: the table stays imported and sr_ingest returns", {
  p <- .dc_project()
  d <- data.frame(id = 1:30)
  d$sex <- haven::labelled(rep(c(1, 2), 15), c(Male = 1, Female = 2), label = "Sex of patient")
  f <- file.path(dirname(p$paths$dir), "src", "fx.sav")
  dir.create(dirname(f))
  haven::write_sav(d, f)
  .dc_swap("sr_dict_set", function(project, table, dict, source = NULL) stop("disk is read-only"))
  expect_warning(p <- .dc_ing(p, f, table = "fx"), "dictionary for table 'fx' not stored: disk is read-only")
  expect_true("fx" %in% names(p$state$tables))
  expect_equal(p$state$tables$fx$store$rows, 30)
  expect_null(p$state$dict$fx)
  # the data are readable and a later sr_dict_set (real one restored) works
  expect_equal(nrow(.dc_all(p, "fx")), 30L)
})

test_that("a dictionary-conversion error (sr_dict_from_labels) during ingest is also only a warning", {
  p <- .dc_project()
  d <- data.frame(id = 1:10)
  d$sex <- haven::labelled(rep(c(1, 2), 5), c(Male = 1, Female = 2))
  f <- file.path(dirname(p$paths$dir), "src", "fy.sav")
  dir.create(dirname(f))
  haven::write_sav(d, f)
  .dc_swap("sr_dict_from_labels", function(dict_labels) stop("cannot convert"))
  expect_warning(p <- .dc_ing(p, f, table = "fy"), "dictionary for table 'fy' not stored: cannot convert")
  expect_equal(p$state$tables$fy$store$rows, 10)
})

test_that(".sr_redcap_strip removes real tags and comments only", {
  expect_identical(.sr_redcap_strip("Age (<18 or >65)"), "Age (<18 or >65)")
  expect_identical(.sr_redcap_strip("<b>Age</b>"), "Age")
  expect_identical(.sr_redcap_strip("<span style='x'>BP</span> &lt;140"), "BP <140")
  expect_identical(.sr_redcap_strip("a <!-- hidden --> b"), "a b")
  expect_identical(.sr_redcap_strip("x<br/>y"), "xy")
  expect_identical(.sr_redcap_strip("1 < 2 and 3 > 2"), "1 < 2 and 3 > 2")
  expect_identical(.sr_redcap_strip("Fish &amp; chips&nbsp;&lt;b&gt;"), "Fish & chips <b>")
})

test_that("a REDCap CSV and the same ODM keep a '<18 or >65' label and still agree", {
  csv <- .dc_write(c('"Variable / Field Name","Form Name","Section Header","Field Type","Field Label","Choices, Calculations, OR Slider Labels","Field Note","Text Validation Type OR Show Slider Number","Text Validation Min","Text Validation Max","Identifier?"',
                     '"agegrp","f","","text","Age (<18 or >65) <b>years</b>","","","","","",""'), "lt.csv")
  odm <- .dc_write(c('<?xml version="1.0"?>',
                     '<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3" xmlns:redcap="https://projectredcap.org" ODMVersion="1.3.1" FileOID="x" FileType="Snapshot"><Study OID="s"><MetaDataVersion OID="m" Name="m">',
                     '<ItemDef OID="agegrp" Name="agegrp" DataType="text" redcap:Variable="agegrp" redcap:FieldType="text"><Question><TranslatedText>Age (&lt;18 or &gt;65) &lt;b&gt;years&lt;/b&gt;</TranslatedText></Question></ItemDef>',
                     '</MetaDataVersion></Study></ODM>'), "lt.xml")
  a <- sr_dict_read_redcap(csv)
  b <- sr_dict_read_odm(odm)
  expect_equal(a$label, "Age (<18 or >65) years")
  expect_identical(.dc_by_var(a), .dc_by_var(b))
})

.dc_big_odm <- function(n_subj, dup = FALSE) {
  head <- c('<?xml version="1.0" encoding="UTF-8"?>',
            '<ODM ODMVersion="1.3.2" FileOID="big" FileType="Snapshot"><Study OID="s"><MetaDataVersion OID="m" Name="m">',
            '<ItemDef OID="a" Name="a" DataType="integer"/><ItemDef OID="b" Name="b" DataType="float"/>',
            '</MetaDataVersion></Study><ClinicalData StudyOID="s" MetaDataVersion="m">')
  ids <- sprintf("S%05d", seq_len(n_subj))
  ev <- function(e) sprintf(paste0('<StudyEventData StudyEventOID="%s"><FormData FormOID="F"><ItemGroupData ItemGroupOID="g">',
                                   '<ItemData ItemOID="a" Value="%d"/><ItemData ItemOID="b" Value="%s"/>',
                                   if (dup) '<ItemData ItemOID="a" Value="999"/>' else "",
                                   '</ItemGroupData></FormData></StudyEventData>'), e, seq_len(n_subj) %% 90L, format(seq_len(n_subj) / 8, trim = TRUE))
  body <- sprintf('<SubjectData SubjectKey="%s">%s%s</SubjectData>', ids, ev("ev1"), ev("ev2"))
  c(head, body, "</ClinicalData></ODM>")
}

test_that("the ODM data reader scales: 2000 subjects x 2 events parse to 4000 rows quickly", {
  f <- .dc_write(.dc_big_odm(2000L), "big.xml")
  t0 <- Sys.time()
  r <- .sr_odm_records(.sr_odm_read(f))
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 20)
  expect_equal(nrow(r), 4000L)
  expect_identical(names(r), c("subject_key", "event_oid", "event_repeat", "repeat_instance", "a", "b"))
  expect_identical(r$subject_key[c(1, 2, 3999, 4000)], c("S00001", "S00001", "S02000", "S02000"))
  expect_identical(r$event_oid[1:2], c("ev1", "ev2"))
  expect_identical(r$a[1:3], c(1L, 1L, 2L))
  expect_equal(r$b[4000], 250)
})

test_that("an item that appears twice for one subject/event/instance warns, naming the item", {
  f <- .dc_write(.dc_big_odm(3L, dup = TRUE), "dup.xml")
  expect_warning(r <- .sr_odm_records(.sr_odm_read(f)), "'a'")
  expect_equal(nrow(r), 6L)
  expect_identical(r$a[1], 999L)                                      # the last value is kept
  # no duplicate, no warning
  g <- .dc_write(.dc_big_odm(3L), "nodup.xml")
  expect_silent(.sr_odm_records(.sr_odm_read(g)))
})
