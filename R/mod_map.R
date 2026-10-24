# =============================================================================
# mod_map.R  -  Map of study sites and where each species is reported (Version 3)
# Interactive leaflet map when installed, otherwise a simple scatter plot.
# Requires latitude and longitude columns.
# =============================================================================


mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectizeInput(ns("species"), "Show", choices = "All species",
                            options = selectize_italic("All species")),
      shiny::checkboxInput(ns("blur"), "Hide exact locations (round coordinates to 0.1 degree, about 11 km)",
                           value = TRUE),
      shiny::p(class = "text-muted small",
               "Locations of informants and sacred or sensitive plant sources can be culturally sensitive. ",
               "Keep this option on unless the community has agreed to share precise locations."),
      if (!has_leaflet()) shiny::p(class = "text-warning small",
                                 "Install 'leaflet' for a base map; showing a simple plot instead.")
    ),
    bslib::card(
      bslib::card_header("Map", report_button(ns, "add_map", "Add map to report")),
      if (has_leaflet()) leaflet::leafletOutput(ns("map"), height = 540)
      else plotly::plotlyOutput(ns("map_static"), height = 540)
    )
  )
}

mod_map_server <- function(id, data, report = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(data(), {
      sp <- sort(unique(data()$scientific_name))
      shiny::updateSelectizeInput(session, "species", choices = c("All species", sp))
    })

    sites <- shiny::reactive({
      shiny::req(data())
      d <- data()
      d <- d[!is.na(d$latitude) & !is.na(d$longitude), , drop = FALSE]
      shiny::validate(shiny::need(nrow(d) > 0,
        "No coordinates found. Add 'latitude' and 'longitude' columns (decimal degrees) to your data."))
      if (!is.null(input$species) && input$species != "All species") {
        d <- d[d$scientific_name == input$species, , drop = FALSE]
      }
      shiny::validate(shiny::need(nrow(d) > 0, "This species has no georeferenced reports."))
      d$place <- ifelse(is.na(d$village), paste(d$latitude, d$longitude), d$village)
      out <- d |>
        dplyr::group_by(place) |>
        dplyr::summarise(lat = mean(latitude), lon = mean(longitude),
                         informants = dplyr::n_distinct(informant_id),
                         species = dplyr::n_distinct(scientific_name),
                         reports = dplyr::n(), .groups = "drop")
      if (isTRUE(input$blur)) {
        out$lat <- round(out$lat, 1)
        out$lon <- round(out$lon, 1)
      }
      out
    })

    # Static version of the map (no base map): used for the report and as the
    # fallback when 'leaflet' is not installed
    p_map <- shiny::reactive({
      d <- sites()
      ggplot2::ggplot(d, ggplot2::aes(x = lon, y = lat, size = reports, fill = reports, text = place)) +
        ggplot2::geom_point(shape = 21, colour = "white", alpha = 0.9) +
        ggplot2::geom_text(ggplot2::aes(x = lon, y = lat, label = place), inherit.aes = FALSE,
                           size = 3, vjust = -1.6, check_overlap = TRUE) +
        scale_fill_ethno_c() +
        ggplot2::labs(x = "Longitude", y = "Latitude", size = "Use reports", fill = "Use reports") +
        theme_ethno()
    })

    if (has_leaflet()) {
      output$map <- leaflet::renderLeaflet({
        d <- sites()
        pal <- leaflet::colorNumeric("viridis", domain = d$reports)
        leaflet::leaflet(d) |>
          leaflet::addTiles() |>
          leaflet::addCircleMarkers(
            lng = ~lon, lat = ~lat, radius = ~pmax(6, sqrt(reports) * 2.5),
            color = "white", weight = 1.5, fillColor = ~pal(reports), fillOpacity = 0.85,
            popup = ~paste0("<b>", place, "</b><br>Informants: ", informants,
                            "<br>Species: ", species, "<br>Use reports: ", reports)
          ) |>
          leaflet::addLegend("bottomright", pal = pal, values = ~reports, title = "Use reports")
      })
    } else {
      output$map_static <- plotly::renderPlotly({
        suppressWarnings(plotly::ggplotly(p_map(), tooltip = c("text", "size")))
      })
    }

    bind_report(input, report, "add_map", "map_sites", "Map",
                function() {
                  sp <- input$species
                  if (is.null(sp) || identical(sp, "All species")) "Map of study sites (all species)"
                  else paste0("Map of study sites: ", sp)
                },
                plot = p_map, width = 7, height = 5.5)
  })
}
