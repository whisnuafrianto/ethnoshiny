# =============================================================================
# mod_multivar.R  -  NMDS, PERMANOVA and hierarchical clustering (Version 2)
# Uses the vegan package on the informant x species presence/absence matrix.
# =============================================================================

mod_multivar_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("by"), "Colour / group by", choices = NULL),
      shiny::numericInput(ns("perm"), "PERMANOVA permutations", value = 499, min = 99, max = 9999, step = 100),
      shiny::p(class = "text-muted small",
               "NMDS places informants with similar plant knowledge close together (Jaccard distance on presence/absence). ",
               "PERMANOVA tests whether groups differ in composition.")
    ),
    bslib::navset_card_tab(
      bslib::nav_panel("NMDS ordination",
                       report_button(ns, "add_nmds", "Add NMDS and PERMANOVA to report"),
                       plotly::plotlyOutput(ns("nmds_plot"), height = 480),
                       shiny::verbatimTextOutput(ns("stress")),
                       shiny::h6("PERMANOVA (adonis2)"),
                       shiny::verbatimTextOutput(ns("permanova"))),
      bslib::nav_panel("Group clustering",
                       report_button(ns, "add_dendro"),
                       shiny::plotOutput(ns("dendro"), height = 420))
    )
  )
}

mod_multivar_server <- function(id, data, report = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(data(), {
      shiny::updateSelectInput(session, "by", choices = group_choices(data()))
    })

    # Informant x species matrix and the group of each informant
    ord_data <- shiny::reactive({
      shiny::req(data(), input$by)
      mat <- informant_species_matrix(data())
      info <- data() |>
        dplyr::distinct(informant_id, .keep_all = TRUE) |>
        dplyr::transmute(informant_id = as.character(informant_id),
                         group = as.character(.data[[input$by]]))
      grp <- info$group[match(rownames(mat), info$informant_id)]
      grp[is.na(grp)] <- "(not recorded)"
      list(mat = mat, grp = grp)
    })

    nmds <- shiny::reactive({
      od <- ord_data()
      shiny::validate(
        shiny::need(nrow(od$mat) >= 5, "NMDS needs at least 5 informants."),
        shiny::need(ncol(od$mat) >= 3, "NMDS needs at least 3 species.")
      )
      set.seed(123)
      res <- tryCatch(
        vegan::metaMDS(od$mat, distance = "jaccard", binary = TRUE, k = 2,
                       trymax = 50, trace = 0, autotransform = FALSE),
        error = function(e) e
      )
      shiny::validate(shiny::need(!inherits(res, "error"),
                                  if (inherits(res, "error")) paste("NMDS failed:", conditionMessage(res)) else ""))
      res
    })

    p_nmds <- shiny::reactive({
      od <- ord_data()
      sc <- as.data.frame(vegan::scores(nmds(), display = "sites"))
      sc$informant <- rownames(od$mat)
      sc$group <- od$grp
      p <- ggplot2::ggplot(sc, ggplot2::aes(x = NMDS1, y = NMDS2, colour = group, text = informant)) +
        ggplot2::geom_point(size = 3.5, alpha = 0.85) +
        scale_colour_ethno_d() +
        ggplot2::labs(colour = NULL) +
        theme_ethno()
      p
    })
    output$nmds_plot <- plotly::renderPlotly(
      suppressWarnings(plotly::ggplotly(p_nmds(), tooltip = c("text", "colour"))))

    stress_text <- shiny::reactive({
      s <- nmds()$stress
      paste0("Stress = ", round(s, 3),
             if (s < 0.1) "  (excellent)" else if (s < 0.2) "  (acceptable)" else "  (poor: interpret with caution)")
    })
    output$stress <- shiny::renderText(stress_text())

    permanova <- shiny::reactive({
      od <- ord_data()
      shiny::validate(shiny::need(length(unique(od$grp)) >= 2, "Need at least 2 groups."))
      dist_mat <- vegan::vegdist(od$mat, method = "jaccard", binary = TRUE)
      meta <- data.frame(group = factor(od$grp))
      set.seed(123)
      vegan::adonis2(dist_mat ~ group, data = meta, permutations = input$perm)
    })
    output$permanova <- shiny::renderPrint(permanova())

    draw_dendro <- shiny::reactive({
      shiny::req(data(), input$by)
      l <- species_by_group(data(), input$by)
      shiny::validate(shiny::need(length(l) >= 3, "Clustering needs at least 3 groups."))
      m <- similarity_matrix(l, "jaccard")
      hc <- stats::hclust(stats::as.dist(1 - m), method = "average")
      function() graphics::plot(hc, main = "Groups clustered by species composition (Jaccard, average linkage)",
                                xlab = "", sub = "", ylab = "Distance (1 - Jaccard)")
    })
    output$dendro <- shiny::renderPlot(draw_dendro()())

    bind_report(input, report, "add_nmds", "mv_nmds", "Multivariate",
                function() paste0("NMDS of informants coloured by ", input$by), plot = p_nmds)
    bind_report(input, report, "add_nmds", "mv_permanova", "Multivariate",
                "NMDS stress and PERMANOVA", text = function() {
                  c(stress_text(), "", utils::capture.output(print(permanova()))) })
    bind_report(input, report, "add_dendro", "mv_dendro", "Multivariate",
                function() paste0("Clustering of ", input$by, " groups"), plot = draw_dendro)
  })
}
