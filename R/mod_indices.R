# =============================================================================
# mod_indices.R  -  UV, FC, RFC, CI, FL, ICF
# =============================================================================

mod_indices_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("metric"), "Rank species by",
                         choices = c("Use Value (UV)" = "UV",
                                     "Relative Frequency of Citation (RFC)" = "RFC",
                                     "Frequency of Citation (FC)" = "FC",
                                     "Cultural Importance (CI)" = "CI",
                                     "Number of use categories (NU)" = "NU",
                                     "Use Reports (UR)" = "UR")),
      shiny::sliderInput(ns("top_n"), "Number of species in the chart", 5, 40, 15, step = 1),
      shiny::radioButtons(ns("style"), "Chart style",
                          choices = c("Bars" = "bar",
                                      "Lollipop" = "lollipop",
                                      "Radial (flower)" = "radial")),
      shiny::hr(),
      shiny::numericInput(ns("min_iu"), "Fidelity Level: minimum informants citing the species (Iu)",
                          value = 3, min = 1, step = 1),
      shiny::selectInput(ns("icf_by"), "Informant Consensus Factor computed on",
                         choices = c("Use category" = "use_category",
                                     "Ailment category" = "ailment"))
    ),
    bslib::navset_card_tab(
      bslib::nav_panel("Species indices",
                       report_button(ns, "add_species_tbl", "Add table to report"),
                       DT::DTOutput(ns("tbl_species"))),
      bslib::nav_panel("Chart",
                       report_button(ns, "add_species_plot"),
                       plotly::plotlyOutput(ns("plot_species"), height = 520)),
      bslib::nav_panel("Fidelity Level (FL)",
                       report_button(ns, "add_fl_plot", "Add chart to report"),
                       report_button(ns, "add_fl_tbl", "Add table to report"),
                       plotly::plotlyOutput(ns("plot_fl"), height = 380),
                       DT::DTOutput(ns("tbl_fl"))),
      bslib::nav_panel("Informant Consensus (ICF)",
                       report_button(ns, "add_icf_plot", "Add chart to report"),
                       report_button(ns, "add_icf_tbl", "Add table to report"),
                       plotly::plotlyOutput(ns("plot_icf"), height = 380),
                       DT::DTOutput(ns("tbl_icf"))),
      bslib::nav_panel("Formulas",
                       shiny::tags$ul(
                         shiny::tags$li(shiny::strong("FC"), " - informants who cited the species."),
                         shiny::tags$li(shiny::strong("RFC"), " = FC / N  (N = total informants)."),
                         shiny::tags$li(shiny::strong("UV"), " = UR / N  (UR = use reports; Phillips & Gentry 1993)."),
                         shiny::tags$li(shiny::strong("CI"), " = sum over use categories of (informants citing the species in that category / N)  (Tardio & Pardo-de-Santayana 2008)."),
                         shiny::tags$li(shiny::strong("FL"), " = Ip / Iu x 100  (Ip = informants citing the species for the main use; Iu = informants citing the species for any use)."),
                         shiny::tags$li(shiny::strong("ICF"), " = (Nur - Nt) / (Nur - 1)  (Nur = use reports in a category; Nt = species used in that category; Trotter & Logan 1986). Values near 1 mean informants agree on which species to use.")
                       ),
                       shiny::p(class = "text-muted small",
                                "Tip: FL and ICF are unreliable for species/categories with very few citations. Increase the minimum Iu to filter them out.")
      )
    )
  )
}

mod_indices_server <- function(id, data, report = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    sp  <- shiny::reactive({ shiny::req(data()); species_indices(data()) })
    fl  <- shiny::reactive({ shiny::req(data()); fidelity_main_use(data(), input$min_iu) })
    icf <- shiny::reactive({ shiny::req(data()); informant_consensus(data(), input$icf_by) })

    output$tbl_species <- DT::renderDT({
      d <- sp() |> dplyr::mutate(dplyr::across(c(RFC, UV, CI), ~ round(.x, 3)))
      dt_table(d, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })

    # static ggplot (also used for the report); radial style is plotly-only
    p_species <- shiny::reactive({
      m <- input$metric
      d <- sp(); d$value <- d[[m]]
      d <- dplyr::slice_max(d, value, n = input$top_n, with_ties = FALSE)
      d$label <- italicize_name(d$scientific_name)      # <i>..</i>, rendered by plotly

      if (identical(input$style, "lollipop")) {
        return(plot_lollipop(d, "label", "value", y_label = m))
      }
      p <- ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(label, value),
                                           y = value, fill = value)) +
        ggplot2::geom_col(show.legend = FALSE, width = 0.75) +
        ggplot2::coord_flip() +
        scale_fill_ethno_c() +
        ggplot2::labs(x = NULL, y = m) +
        theme_ethno()
      p
    })
    output$plot_species <- plotly::renderPlotly({
      if (identical(input$style, "radial")) {
        m <- input$metric
        d <- sp(); d$value <- d[[m]]
        d <- dplyr::slice_max(d, value, n = input$top_n, with_ties = FALSE)
        d$label <- italicize_name(d$scientific_name)
        return(plot_radial(d, "label", "value", y_label = m))
      }
      plotly::ggplotly(p_species())
    })

    output$tbl_fl <- DT::renderDT({
      d <- fl() |> dplyr::mutate(FL = round(FL, 1))
      dt_table(d, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })

    p_fl <- shiny::reactive({
      d <- fl()
      shiny::validate(shiny::need(nrow(d) > 0, "No species reach the minimum Iu. Lower the threshold."))
      d$label <- italicize_name(d$scientific_name)
      p <- ggplot2::ggplot(d, ggplot2::aes(x = Iu, y = FL, colour = use_category,
                                           text = label)) +
        ggplot2::geom_point(size = 3.5, alpha = 0.85) +
        scale_colour_ethno_d() +
        ggplot2::labs(x = "Informants citing the species (Iu)", y = "Fidelity Level (%)",
                      colour = "Main use") +
        theme_ethno()
      p
    })
    output$plot_fl <- plotly::renderPlotly(
      suppressWarnings(plotly::ggplotly(p_fl(), tooltip = c("text", "x", "y", "colour"))))

    output$tbl_icf <- DT::renderDT({
      d <- icf() |> dplyr::mutate(ICF = round(ICF, 3))
      dt_table(d, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })

    p_icf <- shiny::reactive({
      d <- icf() |> dplyr::filter(!is.na(ICF))
      shiny::validate(shiny::need(nrow(d) > 0, "ICF could not be computed (column empty or too few reports)."))
      p <- ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(category, ICF), y = ICF, fill = ICF)) +
        ggplot2::geom_col(show.legend = FALSE, width = 0.75) +
        ggplot2::coord_flip() +
        scale_fill_ethno_c() +
        ggplot2::labs(x = NULL, y = "ICF") +
        theme_ethno()
      p
    })
    output$plot_icf <- plotly::renderPlotly(plotly::ggplotly(p_icf()))

    bind_report(input, report, "add_species_tbl", "idx_species_tbl", "Indices",
                "Species indices (UV, RFC, FC, CI)", table = function() {
                  sp() |> dplyr::mutate(dplyr::across(c(RFC, UV, CI), ~ round(.x, 3)))
                })
    bind_report(input, report, "add_species_plot", "idx_species_plot", "Indices",
                function() paste0("Top species by ", input$metric), height = 5.5,
                plot = function() {
                  if (identical(input$style, "radial")) {   # radial -> bars in the report
                    m <- input$metric; d <- sp(); d$value <- d[[m]]
                    d <- dplyr::slice_max(d, value, n = input$top_n, with_ties = FALSE)
                    d$label <- italicize_name(d$scientific_name)
                    plot_lollipop(d, "label", "value", y_label = m)
                  } else p_species()
                })
    bind_report(input, report, "add_fl_plot", "idx_fl_plot", "Indices",
                "Fidelity Level (FL) by species", plot = p_fl)
    bind_report(input, report, "add_fl_tbl", "idx_fl_tbl", "Indices",
                "Fidelity Level (FL) table", table = function() fl() |> dplyr::mutate(FL = round(FL, 1)))
    bind_report(input, report, "add_icf_plot", "idx_icf_plot", "Indices",
                function() paste0("Informant Consensus Factor by ", input$icf_by), plot = p_icf)
    bind_report(input, report, "add_icf_tbl", "idx_icf_tbl", "Indices",
                "Informant Consensus Factor table", table = function() icf() |> dplyr::mutate(ICF = round(ICF, 3)))
  })
}
