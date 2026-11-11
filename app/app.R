options(shiny.autoload.r = FALSE)  # app/global.R is the only loader; never auto-source app/R/*.R

# StatRouter shell. The Project, Data, Design, SAP, Analyses, Write and Archive panels work (all but the first once a project is open); the rest are placeholders.
# With autoloading off, Shiny runs this file with the app directory as the working directory and does
# not source global.R for an app.R app, so load it here unless a caller already did.
if (!exists("sr_root", inherits = TRUE)) source("global.R", local = FALSE, encoding = "UTF-8")

.sr_panel_names <- c("Reviewer", "Settings")

.sr_placeholder_panel <- function(title) {
  bslib::nav_panel(title, bslib::card(bslib::card_body(shiny::p(class = "text-muted",
                                                             "Coming in P1b-P1d"))))
}

# "<profile> . <mode>" (the separator is a middle dot, built from its code point to keep the source
# ASCII), plus a "no network" badge in air-gap mode.
.sr_badges <- function() {
  profile <- tryCatch(sr_profile(), error = function(e) "unknown")
  mode <- tryCatch(sr_mode(), error = function(e) "unknown")
  bslib::nav_item(shiny::div(
    class = "d-flex align-items-center gap-2",
    shiny::span(class = "badge text-bg-secondary", paste0(profile, " ", intToUtf8(0xB7), " ", mode)),
    if (identical(mode, "airgap")) shiny::span(class = "badge text-bg-success", "no network")
  ))
}

ui <- do.call(bslib::page_navbar, c(
  list(
    title = "StatRouter",
    id = "main_nav",
    theme = bslib::bs_theme(version = 5),
    # the Data, Design, SAP and Analyses tabs are normal scrolling pages of cards (in a fill layout their cards are squashed and overlap); the others keep the fill layout
    fillable = c("Project", .sr_panel_names),
    bslib::nav_panel("Project", mod_project_ui("project"), mod_import_ui("import")),
    bslib::nav_panel("Data", mod_data_ui("data"), value = "Data"),
    bslib::nav_panel("Design", mod_design_ui("design"), value = "Design"),
    bslib::nav_panel("SAP", mod_sap_ui("sap"), value = "SAP"),
    bslib::nav_panel(shiny::span("Analyses ", shiny::uiOutput("analyses-badge", inline = TRUE)),
                     mod_analyses_ui("analyses"), value = "Analyses"),
    bslib::nav_panel("Write", mod_write_ui("write"), value = "Write"),
    bslib::nav_panel("Archive", mod_archive_ui("archive"), value = "Archive")
  ),
  lapply(.sr_panel_names, .sr_placeholder_panel),
  list(bslib::nav_spacer(), .sr_badges())
))

server <- function(input, output, session) {
  # The busy hub: one flag per job module (Tasks 5, 7, 8 and 9 add Write, Archive, Import and Settings).
  # Every module refuses edits while any job other than its own is running. A module that owns a job
  # receives busy_for(<its flag>), so its own running state never blocks itself.
  flags <- shiny::reactiveValues(data = FALSE, analyses = FALSE, archive = FALSE, import = FALSE)
  busy_for <- function(except = character()) {
    shiny::reactive(.sr_ui_busy_any(shiny::reactiveValuesToList(flags), except))
  }
  project <- mod_project_server("project", busy = busy_for())
  d <- mod_data_server("data", project, busy_for("data"))
  a <- mod_analyses_server("analyses", project, busy_for("analyses"))
  shiny::observe(flags$data <- isTRUE(d$busy()))
  shiny::observe(flags$analyses <- isTRUE(a$running()))
  mod_design_server("design", project, busy_for())
  mod_sap_server("sap", project, busy_for())
  mod_write_server("write", project, busy_for(), settings = function() 0L)
  ar <- mod_archive_server("archive", project, busy_for("archive"))
  shiny::observe(flags$archive <- isTRUE(ar$running()))
  im <- mod_import_server("import", project, busy_for("import"))
  shiny::observe(flags$import <- isTRUE(im$running()))
  # The Data, Design, SAP, Analyses, Write and Archive panels are hidden until a project is open.
  shiny::observe({
    for (p in c("Data", "Design", "SAP", "Analyses", "Write", "Archive")) {
      if (is.null(project())) bslib::nav_hide("main_nav", p) else bslib::nav_show("main_nav", p)
    }
  })
}

shiny::shinyApp(ui, server)
