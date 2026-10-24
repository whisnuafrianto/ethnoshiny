# =============================================================================
# mod_categories.R  -  Use categories, plant parts, preparation, habitus ...
# =============================================================================

ATTRIBUTE_CHOICES <- c(
  "Use category" = "use_category",
  "Plant part used" = "plant_part",
  "Preparation" = "preparation",
  "Administration" = "administration",
  "Ailment category" = "ailment",
  "Habitus" = "habitus",
  "Family" = "family"
)

mod_categories_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("attribute"), "Explore by", choices = ATTRIBUTE_CHOICES),
      shiny::radioButtons(ns("measure"), "Count",
                          choices = c("Number of species" = "species",
                                      "Number of use reports" = "reports")),
      shiny::sliderInput(ns("top_n"), "Show top", 5, 30, 12, step = 1),
      shiny::hr(),
      shiny::sliderInput(ns("top_species"), "Heatmap: number of species", 5, 40, 20, step = 1)
    ),
    bslib::card(bslib::card_header("Distribution", report_button(ns, "add_bars")),
                plotly::plotlyOutput(ns("bars"), height = 380)),
    bslib::card(bslib::card_header("Species x use category (informants citing)",
                                   report_button(ns, "add_heatmap")),
                plotly::plotlyOutput(ns("heatmap"), height = 560))
  )
}

mod_categories_server <- function(id, data, report = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    p_bars <- shiny::reactive({
      shiny::req(data())
      d <- data()
      d$value <- d[[input$attribute]]
      d <- d[!is.na(d$value), , drop = FALSE]
      shiny::validate(shiny::need(nrow(d) > 0, "This column has no data."))
      d <- d |>
        dplyr::group_by(value) |>
        dplyr::summarise(species = dplyr::n_distinct(scientific_name),
                         reports = dplyr::n(), .groups = "drop")
      d$n <- d[[input$measure]]
      d <- dplyr::slice_max(d, n, n = input$top_n, with_ties = FALSE)
      p <- ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(value, n), y = n, fill = n)) +
        ggplot2::geom_col(show.legend = FALSE, width = 0.75) +
        ggplot2::coord_flip() +
        scale_fill_ethno_c() +
        ggplot2::labs(x = NULL, y = NULL) +
        theme_ethno()
      p
    })
    output$bars <- plotly::renderPlotly(plotly::ggplotly(p_bars()))

    p_heat <- shiny::reactive({
      shiny::req(data())
      d <- data()
      top <- d |>
        dplyr::distinct(scientific_name, informant_id) |>
        dplyr::count(scientific_name) |>
        dplyr::slice_max(n, n = input$top_species, with_ties = FALSE)
      m <- d |>
        dplyr::filter(scientific_name %in% top$scientific_name) |>
        dplyr::distinct(scientific_name, use_category, informant_id) |>
        dplyr::count(scientific_name, use_category, name = "informants")
      m$sp_label <- factor(italicize_name(m$scientific_name), levels = rev(italicize_name(top$scientific_name)))
      p <- ggplot2::ggplot(m, ggplot2::aes(x = use_category, y = sp_label, fill = informants)) +
        ggplot2::geom_tile(colour = "white") +
        scale_fill_ethno_c() +
        ggplot2::labs(x = NULL, y = NULL, fill = "Informants") +
        theme_ethno() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
      p
    })
    output$heatmap <- plotly::renderPlotly(plotly::ggplotly(p_heat()))

    attr_name <- function() names(ATTRIBUTE_CHOICES)[ATTRIBUTE_CHOICES == input$attribute]
    bind_report(input, report, "add_bars", "cat_bars", "Categories",
                function() paste0("Distribution by ", tolower(attr_name())), plot = p_bars)
    bind_report(input, report, "add_heatmap", "cat_heatmap", "Categories",
                "Species by use category (informants citing)", plot = p_heat, height = 6)
  })
}
