# =============================================================================
# mod_network.R  -  Bipartite network: plants <-> uses / ailments / plant parts
# (Version 2).  Interactive with visNetwork when installed, otherwise a static
# igraph plot.  Hub statistics are always computed with igraph.
# =============================================================================


mod_network_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("relation"), "Link plants to",
                         choices = c("Use category" = "use_category",
                                     "Ailment category" = "ailment",
                                     "Plant part" = "plant_part")),
      shiny::sliderInput(ns("top_n"), "Number of most-cited species", 5, 40, 20, step = 1),
      shiny::p(class = "text-muted small",
               "Edge width = number of informants reporting the link. Plants with many links connect several uses and are the 'hubs' of the local knowledge system."),
      if (!has_visnetwork()) shiny::p(class = "text-warning small",
                                    "Install 'visNetwork' for an interactive network; showing a static plot instead.")
    ),
    bslib::card(
      bslib::card_header("Network", report_button(ns, "add_net", "Add network to report")),
      if (has_visnetwork()) visNetwork::visNetworkOutput(ns("vis"), height = "520px")
      else shiny::plotOutput(ns("static"), height = 520)
    ),
    bslib::card(
      bslib::card_header("Hub species", report_button(ns, "add_hubs", "Add table to report")),
      DT::DTOutput(ns("hubs"))
    )
  )
}

mod_network_server <- function(id, data, report = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    net <- shiny::reactive({
      shiny::req(data())
      d <- data()
      d$target <- d[[input$relation]]
      d <- d[!is.na(d$target), , drop = FALSE]
      shiny::validate(shiny::need(nrow(d) > 0, "This column has no data."))

      top <- d |>
        dplyr::distinct(scientific_name, informant_id) |>
        dplyr::count(scientific_name) |>
        dplyr::slice_max(n, n = input$top_n, with_ties = FALSE)

      edges <- d |>
        dplyr::filter(scientific_name %in% top$scientific_name) |>
        dplyr::distinct(scientific_name, target, informant_id) |>
        dplyr::count(scientific_name, target, name = "weight")

      edges$from <- edges$scientific_name
      edges$to   <- paste0("[", edges$target, "]")
      list(edges = as.data.frame(edges), species = unique(edges$scientific_name))
    })

    graph <- shiny::reactive({
      n <- net()
      igraph::graph_from_data_frame(n$edges[, c("from", "to", "weight")], directed = FALSE)
    })

    # Static igraph drawing: used for the plot when visNetwork is missing and for the report
    draw_static <- function() {
      g <- graph()
      sp <- net()$species
      igraph::V(g)$color <- ifelse(igraph::V(g)$name %in% sp, VIRIDIS_TEAL, VIRIDIS_YELLOW)
      igraph::V(g)$label <- gsub("^\\[|\\]$", "", igraph::V(g)$name)
      is_sp <- igraph::V(g)$name %in% sp
      set.seed(42)
      graphics::par(mar = c(1, 1, 1, 1))
      graphics::plot(g, layout = igraph::layout_with_fr(g),
                     vertex.size = 8, vertex.label.cex = 0.8, vertex.label.color = "black",
                     vertex.label.font = ifelse(is_sp, 3, 1),   # 3 = italic
                     edge.width = 1 + 4 * igraph::E(g)$weight / max(igraph::E(g)$weight))
    }

    if (has_visnetwork()) {
      output$vis <- visNetwork::renderVisNetwork({
        n <- net()
        ids <- unique(c(n$edges$from, n$edges$to))
        nodes <- data.frame(
          id = ids,
          label = ifelse(ids %in% n$species, italicize_name(ids, "vis"),
                         gsub("^\\[|\\]$", "", ids)),
          group = ifelse(ids %in% n$species, "Plant", "Use"),
          stringsAsFactors = FALSE
        )
        edges <- data.frame(from = n$edges$from, to = n$edges$to, value = n$edges$weight)
        visNetwork::visNetwork(nodes, edges) |>
          visNetwork::visNodes(font = list(multi = "md")) |>
          visNetwork::visGroups(groupname = "Plant", color = VIRIDIS_TEAL, shape = "dot") |>
          visNetwork::visGroups(groupname = "Use", color = list(background = VIRIDIS_YELLOW,
                                                                border = VIRIDIS_PURPLE),
                                shape = "box") |>
          visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) |>
          visNetwork::visLayout(randomSeed = 42) |>
          visNetwork::visLegend()
      })
    } else {
      output$static <- shiny::renderPlot(draw_static())
    }

    hubs_tbl <- shiny::reactive({
      g <- graph()
      sp <- net()$species
      v <- igraph::V(g)$name
      tab <- data.frame(
        species = v,
        links = as.integer(igraph::degree(g)),
        strength = as.integer(igraph::strength(g, weights = igraph::E(g)$weight)),
        betweenness = round(igraph::betweenness(g, weights = NA), 2),
        stringsAsFactors = FALSE
      )
      tab <- tab[tab$species %in% sp, ]
      tab[order(-tab$links, -tab$strength), ]
    })
    output$hubs <- DT::renderDT(
      dt_table(hubs_tbl(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)))

    bind_report(input, report, "add_net", "net_plot", "Network",
                function() paste0("Network of plants and ", gsub("_", " ", input$relation)),
                plot = function() { net(); draw_static }, width = 8, height = 6.5)
    bind_report(input, report, "add_hubs", "net_hubs", "Network", "Hub species", table = hubs_tbl)
  })
}
