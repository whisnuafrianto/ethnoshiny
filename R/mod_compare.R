# =============================================================================
# mod_compare.R  -  Compare species knowledge between groups (Version 2)
# Venn diagram, Jaccard / Sorensen similarity, shared and unique species
# =============================================================================

mod_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("by"), "Compare groups defined by", choices = NULL),
      shiny::selectizeInput(ns("groups"), "Groups to include", choices = NULL, multiple = TRUE),
      shiny::radioButtons(ns("method"), "Similarity index",
                          choices = c("Jaccard" = "jaccard", "Sorensen" = "sorensen")),
      shiny::p(class = "text-muted small",
               "The Venn diagram supports 2 to 4 groups. The heatmap and the table support any number.")
    ),
    bslib::navset_card_tab(
      bslib::nav_panel("Venn diagram", report_button(ns, "add_venn"),
                       shiny::plotOutput(ns("venn"), height = 460)),
      bslib::nav_panel("Similarity",
                       report_button(ns, "add_sim", "Add heatmap to report"),
                       report_button(ns, "add_sim_tbl", "Add table to report"),
                       plotly::plotlyOutput(ns("heatmap"), height = 420),
                       DT::DTOutput(ns("sim_table"))),
      bslib::nav_panel("Shared and unique species",
                       report_button(ns, "add_shared", "Add table to report"),
                       DT::DTOutput(ns("shared")))
    )
  )
}

mod_compare_server <- function(id, data, report = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # Fill the "group by" selector with columns that have >= 2 groups
    shiny::observeEvent(data(), {
      ch <- group_choices(data())
      shiny::updateSelectInput(session, "by", choices = ch)
    })

    # Fill the group selector for the chosen column
    shiny::observeEvent(list(data(), input$by), {
      shiny::req(input$by, data())
      vals <- sort(unique(stats::na.omit(as.character(data()[[input$by]]))))
      shiny::updateSelectizeInput(session, "groups", choices = vals, selected = utils::head(vals, 4))
    })

    lists <- shiny::reactive({
      shiny::req(input$by, input$groups)
      d <- data()
      d <- d[as.character(d[[input$by]]) %in% input$groups, , drop = FALSE]
      species_by_group(d, input$by)
    })

    p_venn <- shiny::reactive({
      l <- lists()
      shiny::validate(
        shiny::need(length(l) >= 2, "Select at least 2 groups."),
        shiny::need(length(l) <= 4, "The Venn diagram supports at most 4 groups. Select fewer groups."),
        shiny::need(requireNamespace("ggVennDiagram", quietly = TRUE),
                    "Package 'ggVennDiagram' is not installed. Run install.packages('ggVennDiagram').")
      )
      # Soft pastel blue (white -> light blue): the black counts stay readable, as in many papers
      ggVennDiagram::ggVennDiagram(l, label_alpha = 0) +
        ggplot2::scale_fill_gradient(low = "#F4F8FB", high = "#8DB9DA") +
        ggplot2::theme(legend.position = "none")
    })
    output$venn <- shiny::renderPlot(p_venn())

    sim <- shiny::reactive({
      l <- lists()
      shiny::validate(shiny::need(length(l) >= 2, "Select at least 2 groups."))
      similarity_matrix(l, input$method)
    })

    output$heatmap <- plotly::renderPlotly({
      m <- sim()
      plotly::plot_ly(z = round(m, 3), x = colnames(m), y = rownames(m),
                      type = "heatmap", colorscale = "Viridis", zmin = 0, zmax = 1)
    })

    output$sim_table <- DT::renderDT({
      dt_table(round(sim(), 3), options = list(dom = "t", scrollX = TRUE))
    })

    shared_tbl <- shiny::reactive({
      shiny::req(input$by, input$groups)
      d <- data()
      d$grp <- as.character(d[[input$by]])
      d <- d[d$grp %in% input$groups, , drop = FALSE]
      tab <- d |>
        dplyr::group_by(scientific_name, grp) |>
        dplyr::summarise(FC = dplyr::n_distinct(informant_id), .groups = "drop") |>
        tidyr::pivot_wider(names_from = grp, values_from = FC, values_fill = 0L)
      grp_cols <- setdiff(names(tab), "scientific_name")
      n_groups <- rowSums(as.matrix(tab[, grp_cols, drop = FALSE]) > 0)
      tab$groups_using <- n_groups
      tab$status <- dplyr::case_when(
        n_groups == length(grp_cols) ~ "Used in all groups",
        n_groups == 1 ~ "Unique to one group",
        TRUE ~ "Used in some groups"
      )
      dplyr::arrange(tab, dplyr::desc(groups_using), scientific_name)
    })
    output$shared <- DT::renderDT(
      dt_table(shared_tbl(), rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)))

    p_sim <- shiny::reactive({
      m <- sim()
      df <- as.data.frame(as.table(round(m, 3)), stringsAsFactors = FALSE)
      names(df) <- c("a", "b", "value")
      ggplot2::ggplot(df, ggplot2::aes(a, b, fill = value)) +
        ggplot2::geom_tile(colour = "white") +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", value)), colour = "white", size = 3.5) +
        scale_fill_ethno_c(limits = c(0, 1)) +
        ggplot2::labs(x = NULL, y = NULL, fill = tools::toTitleCase(input$method)) +
        theme_ethno()
    })
    bind_report(input, report, "add_venn", "cmp_venn", "Comparison",
                function() paste0("Venn diagram of species by ", input$by), plot = p_venn)
    bind_report(input, report, "add_sim", "cmp_sim", "Comparison",
                function() paste0(tools::toTitleCase(input$method), " similarity between ", input$by, " groups"),
                plot = p_sim, height = 4)
    bind_report(input, report, "add_sim_tbl", "cmp_sim_tbl", "Comparison",
                "Similarity matrix", table = function() {
                  m <- round(sim(), 3); cbind(group = rownames(m), as.data.frame(m)) })
    bind_report(input, report, "add_shared", "cmp_shared", "Comparison",
                "Shared and unique species", table = shared_tbl)
  })
}
