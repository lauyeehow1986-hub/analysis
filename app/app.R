options(shiny.autoload.r = FALSE)  # app/global.R is the only loader; never auto-source app/R/*.R

# StatRouter shell. The Project, Data, Design and SAP panels work (the last three once a project is open); the rest are placeholders.
# With autoloading off, Shiny runs this file with the app directory as the working directory and does
# not source global.R for an app.R app, so load it here unless a caller already did.
if (!exists("sr_root", inherits = TRUE)) source("global.R", local = FALSE, encoding = "UTF-8")

.sr_panel_names <- c("Analyses", "Write", "Archive", "Reviewer", "Settings")

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
    # the Data, Design and SAP tabs are normal scrolling pages of cards (in a fill layout their cards are squashed and overlap); the others keep the fill layout
    fillable = c("Project", .sr_panel_names),
    bslib::nav_panel("Project", mod_project_ui("project")),
    bslib::nav_panel("Data", mod_data_ui("data"), value = "Data"),
    bslib::nav_panel("Design", mod_design_ui("design"), value = "Design"),
    bslib::nav_panel("SAP", mod_sap_ui("sap"), value = "SAP")
  ),
  lapply(.sr_panel_names, .sr_placeholder_panel),
  list(bslib::nav_spacer(), .sr_badges())
))

server <- function(input, output, session) {
  project <- mod_project_server("project")
  d <- mod_data_server("data", project)
  mod_design_server("design", project, d$busy)
  mod_sap_server("sap", project, d$busy)
  # The Data, Design and SAP panels are hidden until a project is open.
  shiny::observe({
    for (p in c("Data", "Design", "SAP")) {
      if (is.null(project())) bslib::nav_hide("main_nav", p) else bslib::nav_show("main_nav", p)
    }
  })
}

shiny::shinyApp(ui, server)
