# Tests for writer/cite.R: the method references, the package citations, citation order and the
# CSL-JSON / RIS / Vancouver outputs, plus the refs block of the document model. Offline: package
# items come from installed metadata only.

.ct_all_blocks <- function(m) c(m$main, m$supplement)
.ct_block_text <- function(b) {
  switch(b$type, para = b$text, table = , figure = c(b$caption, b$footnotes), character())
}
.ct_used <- function(m) {
  txt <- unlist(lapply(.ct_all_blocks(m), .ct_block_text), use.names = FALSE)
  .sr_cite_tokens(txt)
}
.ct_eff <- function(ci_method = character(), measure = character()) {
  data.frame(ci_method = ci_method, measure = measure, stringsAsFactors = FALSE)
}
.ct_spec <- function(...) {
  utils::modifyList(list(layers = list(missing = "cc")), list(...))
}

test_that("the route keys follow the plan", {
  k <- .sr_cite_keys("modpois", .ct_spec(), sr_effects_empty())
  expect_true("zou2004" %in% k$methods)
  expect_true("sandwich" %in% k$packages)  # the modified Poisson route reports robust standard errors
  expect_named(k, c("methods", "packages"))

  fk <- .sr_cite_keys("logistic", .ct_spec(penalty = "firth"), sr_effects_empty())
  expect_true(all(c("firth1993", "heinze2002") %in% fk$methods))
  expect_true("logistf" %in% fk$packages)
  expect_false("zou2004" %in% fk$methods)

  mi <- .sr_cite_keys("logistic", .ct_spec(layers = list(missing = "mi")), sr_effects_empty())
  expect_true(all(c("rubin1987", "vonhippel2020") %in% mi$methods))
  expect_true("mice" %in% mi$packages)

  cx <- .sr_cite_keys("cox", .ct_spec(), sr_effects_empty())
  expect_identical(cx$methods, "cox1972")
  expect_identical(cx$packages, "survival")
  expect_identical(.sr_cite_keys("cox_describe", .ct_spec(), sr_effects_empty())$methods, "cox1972")
  rm_ <- .sr_cite_keys("rmst", .ct_spec(), sr_effects_empty())
  expect_identical(rm_$methods, "royston2013")
  expect_identical(rm_$packages, "survival")
  expect_identical(.sr_cite_keys("table1", .ct_spec(), sr_effects_empty())$methods, c("austin2009", "yang2012"))

  # the default SE of a logistic route is the model SE: no sandwich unless the spec asks for it
  expect_identical(.sr_cite_keys("logistic", .ct_spec(), sr_effects_empty())$packages, character())
  expect_true("sandwich" %in% .sr_cite_keys("logistic", .ct_spec(se = "robust"), sr_effects_empty())$packages)
  expect_true("sandwich" %in% .sr_cite_keys("lm", .ct_spec(se = "cluster"), sr_effects_empty())$packages)
  expect_false("sandwich" %in% .sr_cite_keys("lm", .ct_spec(se = "model"), sr_effects_empty())$packages)

  # the effects table brings the interval and measure references
  e <- .ct_eff(c("boot_bca", "wilson", "z_model"), c("RD", "risk", "NNT"))
  ek <- .sr_cite_keys("logistic", .ct_spec(), e)
  expect_true(all(c("efron1987", "altman1998", "wilson1927") %in% ek$methods))
  expect_true("boot" %in% ek$packages)
  expect_false("efron1987" %in% .sr_cite_keys("logistic", .ct_spec(), .ct_eff("boot_perc", "RD"))$methods)

  # a restricted cubic spline term
  rcs <- .ct_spec(terms = list(list(var = "age", type = "rcs"), list(var = "z1")))
  rk <- .sr_cite_keys("logistic", rcs, sr_effects_empty())
  expect_true("harrell2015" %in% rk$methods)
  expect_true("Hmisc" %in% rk$packages)
  lin <- .ct_spec(terms = list(list(var = "age"), list(var = "z1", type = "linear")))
  expect_false("harrell2015" %in% .sr_cite_keys("logistic", lin, sr_effects_empty())$methods)

  # no repeats, and a route with nothing to cite gives empty vectors
  none <- .sr_cite_keys("lm", .ct_spec(), sr_effects_empty())
  expect_identical(none$methods, character())
  expect_identical(none$packages, character())
  expect_identical(anyDuplicated(.sr_cite_keys("cox", .ct_spec(), .ct_eff(rep("wilson", 3), rep("HR", 3)))$methods), 0L)
})

test_that("the method reference database holds the 17 items, ASCII on disk", {
  db <- sr_cite_db()
  want <- c("vonelm2007", "zou2004", "rubin1987", "vonhippel2020", "austin2009", "yang2012", "altman1998",
            "firth1993", "heinze2002", "cox1972", "royston2013", "efron1987", "eldridge2006", "wilson1927",
            "westreich2013", "harrell2015", "schulz2010")
  expect_setequal(names(db), want)
  expect_length(db, 17L)
  expect_identical(sr_cite_db(), db)  # read once
  for (id in want) {
    it <- db[[id]]
    expect_identical(it$id, id)
    expect_true(is.character(it$title) && nzchar(it$title), info = id)
    expect_false(is.na(.sr_cite_year(it)), info = id)
    expect_true(length(it$author) >= 1L, info = id)
  }
  art <- Filter(function(it) identical(it$type, "article-journal"), db)
  expect_length(art, 14L)
  for (it in art) expect_match(it$DOI, "^10\\.", info = it$id)
  expect_identical(vapply(db[c("rubin1987", "harrell2015")], function(it) it$type, ""),
                   c(rubin1987 = "book", harrell2015 = "book"))
  expect_identical(db$yang2012$type, "paper-conference")
  # the file is ASCII: a non-ASCII name is a \u escape that the parser turns back into the character
  path <- .sr_cite_db_path()
  expect_false(any(utf8ToInt(readChar(path, file.size(path), useBytes = TRUE)) > 127L))
  expect_identical(db$vonelm2007$author[[5L]]$family, intToUtf8(c(71L, 248L, 116L, 122L, 115L, 99L, 104L, 101L)))
})

test_that("package items come from installed metadata, with no network", {
  pk <- "survival"
  urls <- c(utils::packageDescription(pk)$URL, "https://CRAN.R-project.org/package=survival",
            "https://cran.r-project.org/web/packages/survival/index.html", "https://cran.r-project.org/CRAN_mirrors.csv")
  urls <- urls[!is.na(urls)]
  spy <- local_fetch_spy(urls)
  withr::local_options(repos = NULL)
  it <- .sr_cite_pkg(pk)
  expect_identical(it$id, "pkg_survival")
  expect_identical(it$type, "software")
  expect_true(nzchar(it$title))
  expect_identical(it$version, as.character(utils::packageVersion(pk)))
  expect_true(length(it$author) >= 1L)
  expect_false(is.na(.sr_cite_year(it)))
  rc <- .sr_cite_r_core()
  expect_identical(rc$id, "r_core")
  expect_identical(rc$version, paste(R.version$major, R.version$minor, sep = "."))
  expect_identical(rc$author[[1L]]$literal, "R Core Team")
  expect_match(rc$title, "^R: A Language and Environment")
  # every package the writer can cite is installed here and gets a complete item
  for (p in SR_CITE_PKGS) {
    ip <- .sr_cite_pkg(p)
    expect_identical(ip$id, .sr_cite_pkg_key(p))
    expect_match(ip$id, "^pkg_[a-z][a-z0-9_]*$")
    expect_true(nzchar(ip$title) && nzchar(ip$version), info = p)
  }
  expect_identical(.sr_cite_pkg_key("Hmisc"), "pkg_hmisc")
  expect_identical(.sr_cite_item("pkg_hmisc")$id, "pkg_hmisc")
  expect_length(spy$calls, 0L)
})

test_that("package entries are written from the installed DESCRIPTION, not from citation() (R23)", {
  desc_year <- function(pk) {
    d <- utils::packageDescription(pk)
    for (f in c("Date/Publication", "Packaged", "Date")) {
      v <- d[[f]]
      if (is.character(v) && length(v) == 1L && grepl("^[0-9]{4}", v)) return(as.integer(substr(v, 1L, 4L)))
    }
    NA_integer_
  }
  for (pk in intersect(c("survival", "sandwich", "mice"), rownames(utils::installed.packages()))) {
    it <- .sr_cite_pkg(pk)
    d <- utils::packageDescription(pk)
    expect_identical(it[["type"]], "software", info = pk)
    expect_match(it[["title"]], paste0("^", pk, ": "), info = pk)
    expect_identical(it[["title"]], paste0(pk, ": ", gsub("[[:space:]]+", " ", trimws(d[["Title"]]))), info = pk)
    expect_identical(it[["issued"]][["date-parts"]][[1L]][[1L]], desc_year(pk), info = pk)
    expect_identical(it[["version"]], as.character(utils::packageVersion(pk)), info = pk)
    # the CRAN form of the DOI, or none: never the authors' article
    doi <- it[["DOI"]]
    if (identical(d[["Repository"]], "CRAN")) {
      expect_identical(doi, paste0("10.32614/CRAN.package.", pk), info = pk)
    } else {
      expect_null(doi)
    }
    # nothing of citation(pkg) leaks in: neither its article DOI, nor its title
    cit <- suppressWarnings(utils::citation(pk))
    art_dois <- unlist(lapply(seq_along(cit), function(i) cit[i]$doi))
    expect_false(!is.null(doi) && doi %in% art_dois, info = pk)
    expect_false(it[["title"]] %in% vapply(seq_along(cit), function(i) as.character(cit[i]$title), ""), info = pk)
    # and the Vancouver line prints the package, its version, the DESCRIPTION year and the DOI
    vl <- .sr_cite_vanc_one(it)
    expect_match(vl, paste0(" ", pk, ": "), fixed = TRUE, info = pk)
    expect_match(vl, paste0(" R package version ", it[["version"]], ". ", desc_year(pk), "."), fixed = TRUE, info = pk)
  }
  # the mice DESCRIPTION year is the package's, not that of the 2011 article
  skip_if_not_installed("mice")
  expect_identical(.sr_cite_pkg("mice")[["issued"]][["date-parts"]][[1L]][[1L]], desc_year("mice"))
  expect_false(identical(.sr_cite_pkg("mice")[["issued"]][["date-parts"]][[1L]][[1L]], 2011L))
  # a base package has no Repository, so no DOI is invented
  stat <- .sr_cite_pkg("stats")
  expect_null(stat[["DOI"]])
  expect_match(stat[["title"]], "^stats: ")
  expect_false(grepl("doi:", .sr_cite_vanc_one(stat), fixed = TRUE))
  # r_core still comes from citation()
  expect_identical(.sr_cite_r_core()[["author"]][[1L]][["literal"]], "R Core Team")
})

test_that("a trailing Jr, Sr, II, III or IV moves from the family name to the suffix", {
  a <- .sr_cite_author("Harrell Jr", "Frank E")
  expect_identical(a[["family"]], "Harrell")
  expect_identical(a[["suffix"]], "Jr")
  expect_identical(a[["given"]], "Frank E")
  for (s in c("Jr", "Jr.", "Sr", "Sr.", "II", "III", "IV")) {
    b <- .sr_cite_author(paste("Smith", s), "Pat Q")
    expect_identical(b[["family"]], "Smith", info = s)
    expect_identical(b[["suffix"]], sub("[.]$", "", s), info = s)
  }
  b <- .sr_cite_author("Smith, Jr.", "Pat Q")
  expect_identical(b[["family"]], "Smith")
  expect_identical(b[["suffix"]], "Jr")
  # no suffix on a plain name, nor on a name that merely ends like one
  expect_null(.sr_cite_author("Harrell", "Frank E")[["suffix"]])
  expect_null(.sr_cite_author("Ivy", "Al")[["suffix"]])
  expect_null(.sr_cite_author("Mii", "Al")[["suffix"]])
  # through a person object and to Vancouver
  p <- utils::person("Frank E", "Harrell Jr", role = c("aut", "cre"))
  au <- .sr_cite_authors(p)
  expect_identical(au[[1L]][["suffix"]], "Jr")
  it <- list(id = "x", type = "software", author = au, title = "Hmisc: Test", version = "1.0", genre = "R package")
  expect_match(.sr_cite_vanc_one(it), "^Harrell FE Jr\\. Hmisc: Test\\.")
  skip_if_not_installed("Hmisc")
  hm <- .sr_cite_vanc_one(.sr_cite_pkg("Hmisc"))
  expect_match(hm, "^Harrell FE Jr[,.]")
  expect_false(grepl("Harrell Jr", hm, fixed = TRUE))
})

test_that("a package that is not installed is a clear error, not a broken entry", {
  expect_error(.sr_cite_pkg("srNoSuchPackageXyz"), "srNoSuchPackageXyz.*not installed")
  expect_error(.sr_cite_item("zzz_unknown"), "no reference item for citation key 'zzz_unknown'")
  expect_error(.sr_cite_item("pkg_srnosuch"), "no reference item")
  m <- list(main = list(list(type = "para", id = "p", text = "x {{C:zzz_unknown}}", analysis = NA, section = "methods")),
            supplement = list())
  expect_error(sr_cite_order(m), "zzz_unknown")
})

test_that("Vancouver entries follow the plan", {
  db <- sr_cite_db()
  v <- sr_cite_vancouver(list(db$zou2004))
  expect_identical(v, paste0("1. ", "Zou G. A modified Poisson regression approach to prospective studies with binary data. ",
                             "Am J Epidemiol. 2004;159(7):702-706. doi:10.1093/aje/kwh090"))
  expect_identical(.sr_cite_vanc_one(db$zou2004),
                   "Zou G. A modified Poisson regression approach to prospective studies with binary data. Am J Epidemiol. 2004;159(7):702-706. doi:10.1093/aje/kwh090")
  # numbered by position
  v2 <- sr_cite_vancouver(db[c("zou2004", "cox1972")])
  expect_match(v2[2L], "^2\\. Cox DR\\. Regression models and life-tables\\. J R Stat Soc Series B\\. 1972;34\\(2\\):187-220\\. doi:10\\.1111/")
  # more than six authors: the first six and "et al."
  seven <- list(id = "s", type = "article-journal", title = "A paper", `container-title` = "J Test",
                issued = list(`date-parts` = list(list(2020L))), volume = "1", issue = "2", page = "3-4",
                author = lapply(c("Aa", "Bb", "Cc", "Dd", "Ee", "Ff", "Gg"), function(f) list(family = f, given = "X")))
  s7 <- .sr_cite_vanc_one(seven)
  expect_match(s7, "^Aa X, Bb X, Cc X, Dd X, Ee X, Ff X, et al\\. A paper\\. J Test\\. 2020;1\\(2\\):3-4\\.$")
  expect_false(grepl("Gg", s7))
  six <- seven
  six$author <- seven$author[1:6]
  expect_match(.sr_cite_vanc_one(six), "^Aa X, Bb X, Cc X, Dd X, Ee X, Ff X\\. A paper\\.")
  expect_false(grepl("et al", .sr_cite_vanc_one(six)))
  # a title ending in a question mark gets no extra stop; an article without an issue or a DOI
  expect_match(.sr_cite_vanc_one(db$vonhippel2020), "imputations do you need\\? A two-stage calculation using a quadratic rule\\. Sociol")
  expect_identical(.sr_cite_vanc_one(db$royston2013),
                   paste0("Royston P, Parmar MKB. Restricted mean survival time: an alternative to the hazard ratio for the ",
                          "design and analysis of randomized trials with a time-to-event outcome. BMC Med Res Methodol. ",
                          "2013;13:152. doi:10.1186/1471-2288-13-152"))
  noload <- db$zou2004
  noload$DOI <- NULL
  expect_match(.sr_cite_vanc_one(noload), "702-706\\.$")
  # suffix and initials
  expect_match(.sr_cite_vanc_one(db$harrell2015), "^Harrell FE Jr\\. Regression Modeling Strategies\\. 2nd ed\\. Cham: Springer; 2015\\. doi:10\\.1007/")
  expect_match(.sr_cite_vanc_one(db$vonelm2007), "^von Elm E, Altman DG, Egger M, Pocock SJ, G.tzsche PC, Vandenbroucke JP\\. The Strengthening")
  # a book: place, publisher, year
  expect_identical(.sr_cite_vanc_one(db$rubin1987), "Rubin DB. Multiple Imputation for Nonresponse in Surveys. New York: Wiley; 1987.")
  # a conference paper
  expect_identical(.sr_cite_vanc_one(db$yang2012),
                   paste0("Yang D, Dalton JE. A unified approach to measuring the effect size between two groups using SAS. ",
                          "SAS Global Forum 2012; paper 335-2012."))
  # an R package and R itself
  pkg <- .sr_cite_pkg("survival")
  pv <- .sr_cite_vanc_one(pkg)
  expect_match(pv, "^Therneau TM[^.]*\\. survival: .+\\. R package version ")
  expect_true(endsWith(pv, paste0(" R package version ", pkg$version, ". ", .sr_cite_year(pkg),
                                  ". doi:10.32614/CRAN.package.survival")))
  expect_match(.sr_cite_vanc_one(.sr_cite_r_core()), "^R Core Team\\. R: A Language and Environment for Statistical Computing\\. R version [0-9.]+\\. [0-9]{4}\\.$")
  expect_identical(sr_cite_vancouver(list()), character())
})

test_that("RIS records start with TY and end with ER", {
  keys <- c("zou2004", "rubin1987", "yang2012", "harrell2015", "vonelm2007", "royston2013", "pkg_survival", "r_core")
  ris <- sr_cite_ris(sr_cite_csl(keys))
  expect_length(ris, length(keys))
  for (r in ris) {
    expect_match(r, "^TY  - ")
    expect_match(r, "ER  - $")
    expect_identical(lengths(regmatches(r, gregexpr("\nER  - ", r))), 1L)
  }
  zou <- strsplit(ris[1L], "\n")[[1L]]
  expect_identical(zou, c("TY  - JOUR", "AU  - Zou, G", "TI  - A modified Poisson regression approach to prospective studies with binary data",
                          "JO  - Am J Epidemiol", "PY  - 2004", "VL  - 159", "IS  - 7", "SP  - 702", "EP  - 706",
                          "DO  - 10.1093/aje/kwh090", "ER  - "))
  expect_true("TY  - BOOK" %in% strsplit(ris[2L], "\n")[[1L]])
  expect_true("TY  - CPAPER" %in% strsplit(ris[3L], "\n")[[1L]])
  expect_true("T2  - SAS Global Forum 2012" %in% strsplit(ris[3L], "\n")[[1L]])
  expect_true("AU  - Harrell, FE, Jr" %in% strsplit(ris[4L], "\n")[[1L]])
  expect_identical(sum(startsWith(strsplit(ris[5L], "\n")[[1L]], "AU  - ")), 6L)
  expect_true("SP  - 152" %in% strsplit(ris[6L], "\n")[[1L]])
  expect_false(any(startsWith(strsplit(ris[6L], "\n")[[1L]], "EP  - ")))
  expect_true("TY  - COMP" %in% strsplit(ris[7L], "\n")[[1L]])
  expect_true("AU  - R Core Team" %in% strsplit(ris[8L], "\n")[[1L]])
})

test_that("CSL-JSON round trips and keeps the order of the keys", {
  items <- sr_cite_csl(c("cox1972", "zou2004", "cox1972"))
  expect_length(items, 2L)
  expect_identical(vapply(items, function(i) i$id, ""), c("cox1972", "zou2004"))
  js <- sr_cite_csl_json(items)
  expect_false(any(utf8ToInt(js) > 127L))
  back <- .sr_parse_json(js)
  expect_length(back, 2L)
  expect_identical(back[[2L]]$DOI, "10.1093/aje/kwh090")
  expect_identical(back[[2L]]$author[[1L]]$family, "Zou")
  expect_identical(back[[1L]]$issued[["date-parts"]][[1L]][[1L]], 1972L)
})

test_that("the fixture's document ends in a refs block whose keys all have an item", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  last <- m$main[[length(m$main)]]
  expect_identical(last$type, "refs")
  expect_named(last, c("type", "keys"))
  expect_identical(sum(vapply(m$main, function(b) identical(b$type, "refs"), NA)), 1L)
  expect_identical(last$keys, sr_cite_order(m))   # the refs block does not feed back into the order
  expect_identical(anyDuplicated(last$keys), 0L)
  used <- .ct_used(m)
  expect_true(length(used) > 0L)
  expect_true(all(used %in% last$keys))
  items <- sr_cite_csl(last$keys)
  expect_length(items, length(last$keys))
  expect_identical(vapply(items, function(i) i$id, ""), last$keys)
  expect_length(sr_cite_vancouver(items), length(last$keys))
  expect_length(sr_cite_ris(items), length(last$keys))
})

test_that("sr_cite_order is the order of first use: main text, captions and footnotes, supplement", {
  p <- .fx_writer_project(environment())
  m <- sr_doc_model(p)
  ord <- sr_cite_order(m)
  # the keys the main paragraphs use come first, in the order of first use
  para_txt <- unlist(lapply(Filter(function(b) identical(b$type, "para"), m$main), function(b) b$text))
  para_keys <- unique(.sr_cite_tokens(para_txt))
  expect_identical(ord[seq_along(para_keys)], para_keys)
  expect_true(all(c("austin2009", "cox1972", "r_core", "zou2004") %in% ord))
  expect_lt(match("austin2009", ord), match("cox1972", ord))     # t1 is before a_cox
  # the survival and sandwich packages are cited in the software sentence
  expect_true(all(c("pkg_survival", "pkg_sandwich") %in% ord))
  expect_lt(match("r_core", ord), match("pkg_survival", ord))
  # the Table 2 footnote is a table footnote: its key follows every paragraph key
  tabs <- Filter(function(b) identical(b$type, "table"), c(m$main, m$supplement))
  fn_keys <- unique(.sr_cite_tokens(unlist(lapply(tabs, function(b) b$footnotes))))
  expect_true("westreich2013" %in% fn_keys)
  expect_true("westreich2013" %in% ord)
  expect_false("westreich2013" %in% para_keys)   # only the footnote cites it, so it follows every paragraph key
  expect_gt(match("westreich2013", ord), length(para_keys))
  # STROBE is cited in place by the software sentence
  expect_true("vonelm2007" %in% ord)
  # the citation renders as the position in the list
  cite <- sr_cite_fn(ord)
  expect_identical(cite("austin2009"), paste0("[", match("austin2009", ord), "]"))
  expect_identical(sr_ph_resolve("see {{C:cox1972}}.", m$reg, cite = cite), paste0("see [", match("cox1972", ord), "]."))
  expect_error(cite("not_in_list"), "not in the reference list")
  # the software sentence, resolved, numbers its packages
  sw <- Filter(function(b) identical(b$id, "methods_software"), m$main)[[1L]]
  txt <- sr_ph_resolve(sw$text, m$reg, cite = cite)
  expect_false(grepl("{{", txt, fixed = TRUE))
  expect_match(txt, "with the packages [A-Za-z, ]+ \\[[0-9]+\\]( \\[[0-9]+\\])*, each cited in the references;")
})

test_that("vonelm2007 and r_core are appended once when nothing cites them", {
  empty <- list(main = list(list(type = "para", id = "p", text = "A sentence {{C:zou2004}}.", analysis = NA, section = "methods"),
                            list(type = "refs", keys = "ignored")),
                supplement = list())
  expect_identical(sr_cite_order(empty), c("zou2004", "vonelm2007", "r_core"))
  both <- empty
  both$main[[1L]]$text <- "{{C:r_core}} then {{C:vonelm2007}} then {{C:r_core}}"
  expect_identical(sr_cite_order(both), c("r_core", "vonelm2007"))
  # a table footnote and a figure caption count; the supplement follows the main text
  mixed <- list(
    main = list(list(type = "table", id = "t", caption = "cap {{C:cox1972}}", footnotes = c("a", "{{C:efron1987}}")),
                list(type = "para", id = "p", text = "{{C:zou2004}}", analysis = NA, section = "methods"),
                list(type = "figure", id = "f", caption = "{{C:wilson1927}}")),
    supplement = list(list(type = "para", id = "q", text = "{{C:austin2009}} {{C:zou2004}}", analysis = NA, section = "methods")))
  expect_identical(sr_cite_order(mixed),
                   c("zou2004", "cox1972", "efron1987", "wilson1927", "austin2009", "vonelm2007", "r_core"))
})

test_that("the feasibility sentence of a clustered analysis cites eldridge2006", {
  fake <- function(project, analysis_id) {
    list(values = list(n = 120, events = NA_real_, prevalence = 0.5, h = 0.3, mde = 0.4, delta = NA_real_,
                       delta_source = NA_character_, tier = "precision_only"))
  }
  real <- get("sr_feasibility_fragment", envir = globalenv())
  assign("sr_feasibility_fragment", fake, envir = globalenv())
  withr::defer(assign("sr_feasibility_fragment", real, envir = globalenv()))
  proj <- function(layers, cluster = NULL) {
    list(state = list(analyses = list(a = list(spec = list(layers = layers, cluster = cluster)))))
  }
  flat <- list(clustered = FALSE)
  reg <- sr_ph_registry()
  txt <- .sr_wr_m_feas_precision(proj(flat), "a", reg)
  expect_false(grepl("{{C:", txt, fixed = TRUE))
  expect_match(txt, "precision was rated [{}EL0-9]+\\.$")
  cl <- .sr_wr_m_feas_precision(proj(list(clustered = TRUE), "site"), "a", sr_ph_registry())
  expect_match(cl, " \\{\\{C:eldridge2006\\}\\}\\.$")
  expect_identical(.sr_cite_tokens(cl), "eldridge2006")
  # a cluster column alone also counts
  expect_true(.sr_wr_clustered(proj(flat, "site"), "a"))
  expect_false(.sr_wr_clustered(proj(flat), "a"))
  expect_false(.sr_wr_clustered(proj(flat, character()), "a"))
  # and the full feasibility paragraph keeps the citation
  expect_match(.sr_wr_m_feas(proj(list(clustered = TRUE), "site"), "a", sr_ph_registry()), "\\{\\{C:eldridge2006\\}\\}")
})

test_that("LaTeX markup in installed citation metadata is cleaned", {
  expect_identical(.sr_cite_clean("K\\\"oll"), paste0("K", intToUtf8(246L), "ll"))
  expect_identical(.sr_cite_clean("Andr\\'{e} {\\pkg{mice}}: _x_"), paste0("Andr", intToUtf8(233L), " mice: _x_"))
  expect_identical(.sr_cite_clean("_R: A Language_"), "R: A Language")
  expect_identical(.sr_cite_clean("G\\o tzsche"), paste0("G", intToUtf8(248L), " tzsche"))
  expect_true(is.na(.sr_cite_clean(NA_character_)))
  # the sandwich authors, a real case, carry no backslash
  au <- unlist(.sr_cite_pkg("sandwich")$author)
  expect_false(any(grepl("\\", au, fixed = TRUE)))
})
