# The user interface: viridis navbar with the same menus as the module list.

app_theme <- function() {
  bslib::bs_theme(version = 5, bootswatch = "flatly",
                  primary = VIRIDIS_BLUE, secondary = VIRIDIS_TEAL,
                  success = VIRIDIS_GREEN, info = VIRIDIS_TEAL,
                  warning = VIRIDIS_YELLOW)
}

app_css <- function() {
  shiny::tags$style(shiny::HTML(sprintf("
    .navbar { --bslib-navbar-default-bg: %s; --bslib-navbar-inverse-bg: %s;
              background-color: %s !important; border-bottom: 4px solid transparent;
              border-image: linear-gradient(90deg, %s, %s, %s, %s, %s) 1; }
    .navbar .navbar-brand, .navbar .nav-link, .navbar .dropdown-toggle { color: #fff !important; }
    .navbar .nav-link.active, .navbar .nav-link:hover { color: %s !important; }
    .nav-tabs .nav-link, .nav-pills .nav-link { color: %s !important; }
    .nav-tabs .nav-link.active { color: %s !important; border-bottom: 3px solid %s; }
    .ethno-pill { border-radius: 999px; width: 100%%; font-weight: 600; padding: .55rem 1rem; margin-bottom: .6rem; }
    .ethno-export, .ethno-export:hover { background-color: %s; border-color: %s; color: #fff; }
    .ethno-basket { border: 2px solid %s; }
    .ethno-basket .card-header { background-color: %s; color: #fff; font-weight: 700; border-bottom: 0; }
    .ethno-add { margin: 0 0 .75rem 0; }
    .card-header { border-bottom: 2px solid %s; font-weight: 600; }
  ", VIRIDIS_PURPLE, VIRIDIS_PURPLE, VIRIDIS_PURPLE, VIRIDIS_PURPLE, VIRIDIS_BLUE, VIRIDIS_TEAL, VIRIDIS_GREEN, VIRIDIS_YELLOW,
  VIRIDIS_YELLOW, VIRIDIS_BLUE, VIRIDIS_PURPLE, VIRIDIS_TEAL, VIRIDIS_TEAL,
  VIRIDIS_TEAL, VIRIDIS_TEAL, VIRIDIS_BLUE, VIRIDIS_TEAL)))
}

app_ui <- function() {
  bslib::page_navbar(
    title = "EthnoBotani Explorer",
    theme = app_theme(),
    fillable = FALSE,
    bg = VIRIDIS_PURPLE, inverse = TRUE,
    header = app_css(),

    bslib::nav_panel("Home",
      bslib::card(
        bslib::card_header(paste("Welcome (ethnoshiny", as.character(utils::packageVersion("ethnoshiny")), ")")),
        shiny::p("EthnoBotani Explorer turns ethnobotanical interview data into quantitative indices, ",
                 "comparisons, correlations, networks, maps and ready-to-share reports without writing code."),
        shiny::tags$ol(
          shiny::tags$li(shiny::strong("Data: "), "upload your CSV/Excel file or start with the example data."),
          shiny::tags$li(shiny::strong("Explore: "), "informant profile, use categories, map, visual lab."),
          shiny::tags$li(shiny::strong("Analyse: "), "UV, RFC, CI, FL, ICF; group comparison; correlation; NMDS; networks."),
          shiny::tags$li(shiny::strong("Report: "), "press \"Add to report\" on any chart or table, then pick what to include and export an HTML/Word report.")
        ),
        shiny::p("Data format: one row = one use report by one informant. See the ",
                 shiny::strong("Data"), " tab for the template. Scientific names are shown in italics, ",
                 "and all charts use the viridis colour scale (colour-blind friendly).")
      ),
      bslib::card(
        bslib::card_header("Ethics and data sovereignty"),
        shiny::p("Traditional knowledge belongs to the communities that hold it. Obtain prior informed consent, ",
                 "identify informants with codes (never names), and agree with the community on what may be shared. ",
                 "Use the privacy options in the Map and Report tabs before publishing results.")
      )
    ),

    bslib::nav_menu("Data",
      bslib::nav_panel("Upload and validate", mod_data_ui("data")),
      bslib::nav_panel("Taxonomy check", mod_taxonomy_ui("tax"))
    ),

    bslib::nav_menu("Explore",
      bslib::nav_panel("Informant profile", mod_profile_ui("profile")),
      bslib::nav_panel("Use categories", mod_categories_ui("cats")),
      bslib::nav_panel("Visual lab", mod_visuals_ui("vis")),
      bslib::nav_panel("Map", mod_map_ui("map"))
    ),

    bslib::nav_menu("Analyse",
      bslib::nav_panel("Quantitative indices", mod_indices_ui("idx")),
      bslib::nav_panel("Group comparison", mod_compare_ui("cmp")),
      bslib::nav_panel("Correlation", mod_correlation_ui("cor")),
      bslib::nav_panel("Multivariate", mod_multivar_ui("mv")),
      bslib::nav_panel("Network", mod_network_ui("net"))
    ),

    bslib::nav_panel("Report", mod_report_ui("rep"))
  )
}
