# =============================================================================
# mod_visuals.R  -  "Visual lab": less common charts for telling the story
#   Flow (Sankey) | Hierarchy (treemap / sunburst) | Waffle | Species radar |
#   Raincloud (distribution of knowledge by group)
# All use the viridis palette.
# =============================================================================

FLOW_CHOICES <- c("Village" = "village", "Ethnic group" = "ethnic_group",
                  "Gender" = "gender", "Occupation" = "occupation",
                  "Use category" = "use_category", "Plant part" = "plant_part",
                  "Habitus" = "habitus", "Family" = "family",
                  "Ailment category" = "ailment")

HIERARCHY_PATHS <- list(
  "Family > Species" = c("family", "scientific_name"),
  "Use category > Plant part > Species" = c("use_category", "plant_part", "scientific_name"),
  "Village > Use category > Species" = c("village", "use_category", "scientific_name"),
  "Habitus > Family > Species" = c("habitus", "family", "scientific_name")
)

RAINCLOUD_VARS <- c("Species known" = "species_known",
                    "Use reports" = "use_reports",
                    "Use categories known" = "categories_known",
                    "Plant parts known" = "parts_known",
                    "Age" = "age",
                    "Years of experience" = "experience_years")

mod_visuals_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    bslib::nav_panel(
      "Flow (Sankey)",
      shiny::p(class = "text-muted",
               "Follow the use reports from one column to the next. The width of a ribbon is the number of reports."),
      bslib::layout_columns(
        col_widths = c(4, 4, 4), fill = FALSE,
        shiny::selectInput(ns("s1"), "Step 1", FLOW_CHOICES, selected = "village"),
        shiny::selectInput(ns("s2"), "Step 2", FLOW_CHOICES, selected = "use_category"),
        shiny::selectInput(ns("s3"), "Step 3 (optional)", c("(none)" = "none", FLOW_CHOICES),
                           selected = "plant_part")),
      report_button(ns, "add_sankey"),
      plotly::plotlyOutput(ns("sankey"), height = 560)
    ),
    bslib::nav_panel(
      "Hierarchy",
      shiny::p(class = "text-muted",
               "Area = number of use reports. Click a block to zoom in; click the top bar to zoom out."),
      bslib::layout_columns(
        col_widths = c(7, 5), fill = FALSE,
        shiny::selectInput(ns("path"), "Hierarchy", names(HIERARCHY_PATHS)),
        shiny::radioButtons(ns("hstyle"), "Style", inline = TRUE,
                            choices = c("Treemap" = "treemap", "Sunburst" = "sunburst"))),
      report_button(ns, "add_hierarchy"),
      plotly::plotlyOutput(ns("hierarchy"), height = 580)
    ),
    bslib::nav_panel(
      "Waffle",
      shiny::p(class = "text-muted",
               "100 squares share the use reports. Each square is 1% of all reports."),
      shiny::selectInput(ns("waffle_by"), "Split by", FLOW_CHOICES, selected = "use_category"),
      report_button(ns, "add_waffle"),
      shiny::plotOutput(ns("waffle"), height = 460)
    ),
    bslib::nav_panel(
      "Species radar",
      shiny::p(class = "text-muted",
               "The 'fingerprint' of each plant across use categories. Pick up to 5 species to compare."),
      bslib::layout_columns(
        col_widths = c(8, 4), fill = FALSE,
        shiny::selectizeInput(ns("radar_sp"), "Species", choices = NULL, multiple = TRUE,
                              options = c(list(maxItems = 5), selectize_italic())),
        shiny::radioButtons(ns("radar_measure"), "Axis shows",
                            choices = c("Fidelity level (%)" = "FL", "Informants" = "Ip"))),
      report_button(ns, "add_radar"),
      plotly::plotlyOutput(ns("radar"), height = 540)
    ),
    bslib::nav_panel(
      "Raincloud",
      shiny::p(class = "text-muted",
               "Every dot is an informant; the cloud shows how the values are spread within each group."),
      bslib::layout_columns(
        col_widths = c(6, 6), fill = FALSE,
        shiny::selectInput(ns("rain_var"), "Measure", RAINCLOUD_VARS),
        shiny::selectInput(ns("rain_by"), "Group by", choices = NULL)),
      report_button(ns, "add_rain"),
      shiny::plotOutput(ns("raincloud"), height = 480)
    )
  )
}

mod_visuals_server <- function(id, data, report = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- Sankey ---------------------------------------------------------------
    flow_cols <- shiny::reactive(c(input$s1, input$s2, if (!identical(input$s3, "none")) input$s3))

    sankey_res <- shiny::reactive({
      shiny::req(data())
      cols <- flow_cols()
      d <- data()
      shiny::validate(
        shiny::need(!anyDuplicated(cols), "Choose a different column for each step."),
        shiny::need(all(cols %in% names(d)), "A chosen column is not in your data."),
        shiny::need(any(stats::complete.cases(d[, cols, drop = FALSE])),
                    "No rows have values in all the chosen columns.")
      )
      sankey_data(d, cols, top = 8)
    })

    output$sankey <- plotly::renderPlotly({
      s <- sankey_res()
      node_cols <- viridis_cols(nrow(s$nodes))
      plotly::plot_ly(
        type = "sankey", orientation = "h",
        node = list(label = s$nodes$label, color = node_cols, pad = 14, thickness = 18,
                    line = list(color = "white", width = 0.5)),
        link = list(source = s$links$source, target = s$links$target, value = s$links$value,
                    color = to_rgba(node_cols[s$links$source + 1], 0.45))
      ) |>
        plotly::layout(font = list(size = 12))
    })

    flow_title <- function() {
      lab <- function(v) names(FLOW_CHOICES)[match(v, FLOW_CHOICES)]
      paste(lab(flow_cols()), collapse = " > ")
    }
    p_sankey <- shiny::reactive(plot_sankey_static(sankey_res(), title = flow_title()))
    bind_report(input, report, "add_sankey", "vis_sankey", "Visuals",
                function() paste0("Flow of use reports: ", flow_title()),
                plot = p_sankey, width = 9, height = 6)

    # ---- Treemap / sunburst -----------------------------------------------------
    hier_res <- shiny::reactive({
      shiny::req(data(), input$path)
      path <- HIERARCHY_PATHS[[input$path]]
      d <- data()
      shiny::validate(shiny::need(all(path %in% names(d)), "A column needed for this hierarchy is missing."))
      h <- hierarchy_data(d, path)
      shiny::validate(shiny::need(nrow(h) > 0, "No rows have values in all the columns of this hierarchy."))
      list(h = h, path = path)
    })

    output$hierarchy <- plotly::renderPlotly({
      res <- hier_res()
      h <- res$h; path <- res$path
      # species are the last level of every hierarchy: write their names in italics
      leaf_species <- h$level == length(path) & identical(utils::tail(path, 1), "scientific_name")
      h$labels <- ifelse(leaf_species, italicize_name(h$labels), h$labels)
      plotly::plot_ly(
        type = input$hstyle,
        ids = h$ids, labels = h$labels, parents = h$parents, values = h$values,
        branchvalues = "total",
        marker = list(colors = h$values, colorscale = "Viridis", showscale = TRUE,
                      colorbar = list(title = "Reports"), line = list(color = "white", width = 1)),
        hovertemplate = "%{label}<br>%{value} use reports<extra></extra>"
      )
    })

    p_hier <- shiny::reactive({
      res <- hier_res()
      plot_hierarchy_static(res$h, res$path, style = input$hstyle, title = input$path)
    })
    bind_report(input, report, "add_hierarchy", "vis_hierarchy", "Visuals",
                function() paste0(if (identical(input$hstyle, "sunburst")) "Sunburst: " else "Treemap: ", input$path),
                plot = p_hier, width = 9, height = 6.5)

    # ---- Waffle -----------------------------------------------------------------
    p_waffle <- shiny::reactive({
      shiny::req(data(), input$waffle_by)
      d <- data()
      shiny::validate(shiny::need(input$waffle_by %in% names(d) && any(!is.na(d[[input$waffle_by]])),
                                  "This column has no data."))
      g <- waffle_grid(d[[input$waffle_by]])
      pal <- categorical_palette(levels(g$category))
      ggplot2::ggplot(g, ggplot2::aes(x = x, y = y, fill = category)) +
        ggplot2::geom_tile(colour = "white", linewidth = 1.2, width = 0.94, height = 0.94) +
        ggplot2::scale_fill_manual(values = pal, name = NULL) +
        ggplot2::coord_equal() +
        ggplot2::labs(title = "Each square = 1% of all use reports") +
        ggplot2::theme_void(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                       plot.title.position = "plot",
                       legend.text = ggplot2::element_text(size = 12))
    })
    output$waffle <- shiny::renderPlot(p_waffle(), res = 96)
    bind_report(input, report, "add_waffle", "vis_waffle", "Visuals",
                function() paste0("Waffle chart of use reports by ", input$waffle_by),
                plot = p_waffle, width = 8, height = 5)

    # ---- Species radar ----------------------------------------------------------
    shiny::observeEvent(data(), {
      sp <- species_indices(data())
      shiny::updateSelectizeInput(session, "radar_sp", choices = sp$scientific_name,
                                  selected = utils::head(sp$scientific_name, 3))
    })

    radar_res <- shiny::reactive({
      shiny::req(data(), input$radar_sp)
      d <- data()
      cats <- sort(unique(d$use_category))
      shiny::validate(shiny::need(length(cats) >= 3, "The radar needs at least 3 use categories."))
      sel <- input$radar_sp
      fl <- fidelity_level(d[d$scientific_name %in% sel, , drop = FALSE]) |>
        tidyr::complete(scientific_name = sel, use_category = cats,
                        fill = list(Ip = 0L, FL = 0))
      list(fl = fl, cats = cats, sel = sel, m = input$radar_measure)
    })

    output$radar <- plotly::renderPlotly({
      res <- radar_res()
      fl <- res$fl; cats <- res$cats; sel <- res$sel; m <- res$m
      cols <- viridis_cols(length(sel))
      p <- plotly::plot_ly(type = "scatterpolar", mode = "lines+markers")
      for (i in seq_along(sel)) {
        sub <- fl[fl$scientific_name == sel[i], , drop = FALSE]
        sub <- sub[match(cats, sub$use_category), , drop = FALSE]
        p <- plotly::add_trace(
          p, r = c(sub[[m]], sub[[m]][1]), theta = c(cats, cats[1]),
          name = italicize_name(sel[i]), fill = "toself",
          line = list(color = cols[i], width = 2),
          marker = list(color = cols[i], size = 6),
          fillcolor = to_rgba(cols[i], 0.25)
        )
      }
      plotly::layout(p, polar = list(radialaxis = list(visible = TRUE, rangemode = "tozero")),
                     legend = list(orientation = "h", y = -0.1))
    })

    p_radar <- shiny::reactive({
      res <- radar_res()
      plot_radar_static(res$fl, res$cats, res$sel, res$m,
                        y_label = if (identical(res$m, "FL")) "Fidelity level (%)" else "Informants")
    })
    bind_report(input, report, "add_radar", "vis_radar", "Visuals",
                "Species radar (use-category fingerprint)",
                plot = p_radar, width = 8, height = 6.5)

    # ---- Raincloud --------------------------------------------------------------
    shiny::observeEvent(data(), {
      shiny::updateSelectInput(session, "rain_by", choices = group_choices(data()))
    })

    p_rain <- shiny::reactive({
      shiny::req(data(), input$rain_by, input$rain_var)
      d <- data()
      inf <- informant_summary(d)
      first_rows <- d[!duplicated(d$informant_id), , drop = FALSE]
      inf$group <- as.character(first_rows[[input$rain_by]][match(inf$informant_id, first_rows$informant_id)])
      inf$value <- inf[[input$rain_var]]
      inf <- inf[!is.na(inf$group) & !is.na(inf$value), , drop = FALSE]
      shiny::validate(shiny::need(nrow(inf) >= 5, "Too few informants with values for this measure and grouping."))

      kw <- tryCatch({
        if (dplyr::n_distinct(inf$group) >= 2) {
          p <- stats::kruskal.test(value ~ factor(group), data = inf)$p.value
          sprintf("Kruskal-Wallis test between groups: p = %.3g", p)
        } else NULL
      }, error = function(e) NULL)

      ggplot2::ggplot(inf, ggplot2::aes(x = group, y = value, fill = group, colour = group)) +
        ggplot2::geom_violin(alpha = 0.35, colour = NA, trim = TRUE, width = 0.9) +
        ggplot2::geom_boxplot(width = 0.12, fill = "white", colour = "grey25",
                              outlier.shape = NA, linewidth = 0.5) +
        ggplot2::geom_jitter(width = 0.07, height = 0, size = 2, alpha = 0.75) +
        ggplot2::coord_flip() +
        scale_fill_ethno_d() +
        scale_colour_ethno_d() +
        ggplot2::labs(x = NULL, y = names(RAINCLOUD_VARS)[match(input$rain_var, RAINCLOUD_VARS)],
                      subtitle = kw) +
        theme_ethno() +
        ggplot2::theme(legend.position = "none")
    })
    output$raincloud <- shiny::renderPlot(p_rain(), res = 96)
    bind_report(input, report, "add_rain", "vis_raincloud", "Visuals",
                function() paste0("Raincloud plot: ", names(RAINCLOUD_VARS)[match(input$rain_var, RAINCLOUD_VARS)],
                                  " by ", input$rain_by),
                plot = p_rain)
  })
}
