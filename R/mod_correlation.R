# =============================================================================
# mod_correlation.R  -  Correlation analysis
#  1. Informant knowledge: does age / experience go with how much a person knows?
#  2. Species traits: are widely known plants also the most versatile?
#  3. Plant co-knowledge: which plants are known by the same people? (phi)
# Matrices are drawn as "bubble matrices" (size = |r|, colour = r, viridis).
# =============================================================================

INFORMANT_VARS <- c("Age" = "age",
                    "Years of experience" = "experience_years",
                    "Species known" = "species_known",
                    "Use reports" = "use_reports",
                    "Use categories known" = "categories_known",
                    "Plant parts known" = "parts_known")

SPECIES_VARS <- c("Informants citing (FC)" = "informants",
                  "Use reports" = "use_reports",
                  "Use categories (NU)" = "use_categories",
                  "Plant parts" = "plant_parts",
                  "Ailment categories" = "ailments",
                  "Villages citing" = "villages")

method_symbol <- function(method) {
  switch(method, spearman = "Spearman rho", pearson = "Pearson r", kendall = "Kendall tau")
}

# One block = bubble matrix + scatter plot + sentence + table, for one data table
corr_block_ui <- function(ns, prefix, var_choices, default_x, default_y) {
  shiny::tagList(
    shiny::checkboxGroupInput(ns(paste0(prefix, "_vars")), "Variables",
                              choices = var_choices, selected = unname(var_choices), inline = TRUE),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(bslib::card_header("Correlation matrix",
                                     report_button(ns, paste0(prefix, "_add_matrix"))),
                  shiny::plotOutput(ns(paste0(prefix, "_matrix")), height = 440)),
      bslib::card(bslib::card_header("Look at one pair",
                                     report_button(ns, paste0(prefix, "_add_pair"))),
                  bslib::layout_columns(
                    col_widths = c(6, 6), fill = FALSE,
                    shiny::selectInput(ns(paste0(prefix, "_x")), "X axis", var_choices, selected = default_x),
                    shiny::selectInput(ns(paste0(prefix, "_y")), "Y axis", var_choices, selected = default_y)),
                  shiny::textOutput(ns(paste0(prefix, "_summary"))),
                  plotly::plotlyOutput(ns(paste0(prefix, "_scatter")), height = 340))
    ),
    bslib::card(bslib::card_header("All pairs",
                                   report_button(ns, paste0(prefix, "_add_table"), "Add table to report")),
                DT::DTOutput(ns(paste0(prefix, "_table"))))
  )
}

mod_correlation_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::radioButtons(ns("method"), "Correlation method",
                          choices = c("Spearman (ranks, recommended)" = "spearman",
                                      "Pearson (straight-line)" = "pearson",
                                      "Kendall (ranks, small samples)" = "kendall")),
      shiny::selectInput(ns("adjust"), "p-value adjustment (many pairs are tested)",
                         choices = c("Holm" = "holm",
                                     "Benjamini-Hochberg (FDR)" = "BH",
                                     "None" = "none")),
      shiny::hr(),
      shiny::p(class = "text-muted small",
               "Stars show significance after adjustment: * p < 0.05, ** p < 0.01, *** p < 0.001."),
      shiny::p(class = "text-muted small",
               "Rule of thumb for |r|: below 0.1 negligible, 0.1-0.3 weak, 0.3-0.5 moderate, 0.5-0.7 strong, above 0.7 very strong."),
      shiny::p(class = "text-muted small",
               "Correlation is not causation, and with few informants even large coefficients can be chance.")
    ),
    bslib::navset_card_tab(
      bslib::nav_panel("Informant knowledge",
                       shiny::p(class = "text-muted",
                                "Each point is one informant. Does a person who is older, or has more experience, know more plants and uses?"),
                       corr_block_ui(ns, "inf", INFORMANT_VARS, "experience_years", "species_known")),
      bslib::nav_panel("Species traits",
                       shiny::p(class = "text-muted",
                                "Each point is one species. Are widely known plants also used in more ways, by more plant parts, in more villages?"),
                       corr_block_ui(ns, "sp", SPECIES_VARS, "informants", "use_categories")),
      bslib::nav_panel("Plant co-knowledge",
                       shiny::p(class = "text-muted",
                                "phi between plants: +1 = the same people know both, 0 = unrelated, -1 = known by different people (specialisation)."),
                       shiny::sliderInput(ns("co_top"), "Number of most-cited species", 6, 35, 18, step = 1),
                       bslib::card(bslib::card_header("Which plants travel together?",
                                                      report_button(ns, "add_co")),
                                   shiny::plotOutput(ns("co_matrix"), height = 620)),
                       bslib::card(bslib::card_header("Strongest pairs",
                                                      report_button(ns, "add_co_tbl", "Add table to report")),
                                   shiny::radioButtons(ns("co_dir"), NULL, inline = TRUE,
                                                       choices = c("Often known together" = "high",
                                                                   "Rarely known together" = "low")),
                                   DT::DTOutput(ns("co_table"))))
    )
  )
}

mod_correlation_server <- function(id, data, report = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    inf_tbl <- shiny::reactive({ shiny::req(data()); informant_summary(data()) })
    sp_tbl  <- shiny::reactive({ shiny::req(data()); species_summary(data()) })

    # Builds the four outputs of one block (matrix, summary sentence, scatter, table)
    make_block <- function(prefix, tbl, labels) {
      cm <- shiny::reactive({
        vars <- input[[paste0(prefix, "_vars")]]
        shiny::validate(shiny::need(length(vars) >= 2, "Select at least two variables."))
        res <- correlation_matrix(tbl(), vars, input$method, input$adjust)
        shiny::validate(shiny::need(
          nrow(res$r) >= 2,
          "Fewer than two usable variables (each needs at least 5 values and some variation). Add the missing columns to your data."))
        res
      })

      p_matrix <- shiny::reactive({
        res <- cm()
        plot_bubble_matrix(res$r, res$p_adj, fill_label = method_symbol(res$method))
      })
      output[[paste0(prefix, "_matrix")]] <- shiny::renderPlot(p_matrix(), res = 96)

      pair <- shiny::reactive({
        x <- input[[paste0(prefix, "_x")]]
        y <- input[[paste0(prefix, "_y")]]
        shiny::validate(shiny::need(!identical(x, y), "Choose two different variables."))
        d <- tbl()
        shiny::validate(shiny::need(all(c(x, y) %in% names(d)), "Variable not available."))
        dd <- data.frame(x = d[[x]], y = d[[y]])
        dd <- dd[stats::complete.cases(dd), , drop = FALSE]
        shiny::validate(shiny::need(nrow(dd) >= 5 && length(unique(dd$x)) > 1 && length(unique(dd$y)) > 1,
                                    "This pair has too few complete values or no variation."))
        ct <- suppressWarnings(stats::cor.test(dd$x, dd$y, method = input$method, exact = FALSE))
        list(d = dd, x = x, y = y, r = unname(ct$estimate), p = ct$p.value)
      })

      output[[paste0(prefix, "_summary")]] <- shiny::renderText({
        pr <- pair()
        sprintf("%s = %.2f (%s), p = %.3g, n = %d",
                method_symbol(input$method), pr$r, describe_correlation(pr$r), pr$p, nrow(pr$d))
      })

      p_pair <- shiny::reactive({
        pr <- pair()
        lab <- function(v) names(labels)[match(v, labels)]
        base <- ggplot2::ggplot(pr$d, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_point(colour = VIRIDIS_TEAL, size = 3, alpha = 0.55) +
          ggplot2::labs(x = lab(pr$x), y = lab(pr$y)) +
          theme_ethno()
        smooth <- function(method) {
          ggplot2::geom_smooth(method = method, formula = y ~ x, colour = VIRIDIS_PURPLE,
                               fill = VIRIDIS_YELLOW, alpha = 0.25, linewidth = 1)
        }
        # Rank methods: a flexible monotone trend; fall back to a straight line if loess cannot be fitted
        want <- if (input$method == "pearson") "lm" else "loess"
        p <- base + smooth(want)
        built <- tryCatch({ suppressWarnings(ggplot2::ggplot_build(p)); TRUE }, error = function(e) FALSE)
        if (!built) p <- base + smooth("lm")
        p
      })
      output[[paste0(prefix, "_scatter")]] <- plotly::renderPlotly(suppressWarnings(plotly::ggplotly(p_pair())))

      pairs_tbl <- shiny::reactive({
        tab <- correlation_pairs(cm())
        tab$strength <- describe_correlation(tab$r)
        tab[order(-abs(tab$r)), , drop = FALSE]
      })
      what <- if (prefix == "inf") "informant knowledge" else "species traits"
      bind_report(input, report, paste0(prefix, "_add_matrix"), paste0("cor_", prefix, "_matrix"),
                  "Correlation", paste0("Correlation matrix (", what, ")"), plot = p_matrix, height = 5.5)
      bind_report(input, report, paste0(prefix, "_add_pair"), paste0("cor_", prefix, "_pair"),
                  "Correlation", function() {
                    pr <- pair(); lab <- function(v) names(labels)[match(v, labels)]
                    paste0(lab(pr$x), " vs ", lab(pr$y), " (", input$method, ")")
                  }, plot = p_pair,
                  text = NULL)
      bind_report(input, report, paste0(prefix, "_add_table"), paste0("cor_", prefix, "_table"),
                  "Correlation", paste0("All pairs (", what, ")"), table = pairs_tbl)

      output[[paste0(prefix, "_table")]] <- DT::renderDT({
        tab <- pairs_tbl()
        dt_table(tab, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) |>
          DT::formatRound(c("r", "p", "p_adjusted"), 3)
      })
    }

    make_block("inf", inf_tbl, INFORMANT_VARS)
    make_block("sp",  sp_tbl,  SPECIES_VARS)

    # ---- Plant co-knowledge ---------------------------------------------------
    ck <- shiny::reactive({
      shiny::req(data())
      res <- coknowledge(data(), input$co_top)
      shiny::validate(shiny::need(ncol(res$phi) >= 3, "Need at least 3 species known by some, but not all, informants."))
      res
    })

    p_co <- shiny::reactive({
      res <- ck()
      plot_bubble_matrix(res$phi, NULL, fill_label = "phi", italic = TRUE)
    })
    output$co_matrix <- shiny::renderPlot(p_co(), res = 96)
    bind_report(input, report, "add_co", "cor_coknowledge", "Correlation",
                "Plant co-knowledge (phi between species)", plot = p_co, width = 8, height = 7.5)

    co_pairs_tbl <- shiny::reactive({
      tab <- coknowledge_pairs(ck())
      tab <- if (identical(input$co_dir, "low")) tab[order(tab$phi), ] else tab[order(-tab$phi), ]
      utils::head(tab, 50)
    })

    output$co_table <- DT::renderDT({
      dt_table(co_pairs_tbl(), rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE)) |>
        DT::formatRound("phi", 3)
    })
    bind_report(input, report, "add_co_tbl", "cor_coknowledge_tbl", "Correlation",
                function() if (identical(input$co_dir, "low")) "Plant pairs rarely known together"
                           else "Plant pairs often known together",
                table = co_pairs_tbl)
  })
}
