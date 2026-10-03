options(shiny.autoload.r = FALSE)  # app/global.R is the only loader; never auto-source app/R/*.R

# StatRouter shell. In P1a only the Project panel works; the rest are placeholders for P1b-P1d.
# With autoloading off, Shiny runs this file with the app directory as the working directory and does
# not source global.R for an app.R app, so load it here unless a caller already did.
if (!exists("sr_root", inherits = TRUE)) source("global.R", local = FALSE, encoding = "UTF-8")

.sr_panel_names <- c("Data", "Profile", "Design", "SAP", "Analyses", "Write", "Archive", "Reviewer",
                     "Settings")

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
    bslib::nav_panel("Project", mod_project_ui("project"))
  ),
  lapply(.sr_panel_names, .sr_placeholder_panel),
  list(bslib::nav_spacer(), .sr_badges())
))

server <- function(input, output, session) {
  project <- mod_project_server("project")
}

shiny::shinyApp(ui, server)
