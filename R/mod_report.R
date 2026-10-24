# =============================================================================
# mod_report.R  -  Report basket and downloads
# Every analysis tab has an "Add to report" button. This tab lists what was
# added, lets the user choose what to include, and exports the report
# (HTML or Word). The Excel workbook is also here.
# =============================================================================

mod_report_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h2("Report", class = "text-center my-3"),
    bslib::layout_columns(
      col_widths = c(8, 4), fill = FALSE,
      bslib::card(
        class = "ethno-basket",
        bslib::card_header("Select results to include in the Report"),
        shiny::uiOutput(ns("basket"))
      ),
      shiny::div(
        class = "ethno-actions",
        shiny::actionButton(ns("select_all"), "Select All", icon = shiny::icon("circle-check"),
                            class = "btn-primary ethno-pill"),
        shiny::actionButton(ns("deselect_all"), "Deselect All", icon = shiny::icon("circle-xmark"),
                            class = "btn-primary ethno-pill"),
        shiny::hr(),
        shiny::downloadButton(ns("report"), "Export Report", class = "ethno-pill ethno-export"),
        shiny::hr(),
        shiny::actionButton(ns("delete"), "Delete Report", icon = shiny::icon("triangle-exclamation"),
                            class = "btn-danger ethno-pill")
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6), fill = FALSE,
      bslib::card(
        bslib::card_header("Report settings"),
        shiny::textInput(ns("title"), "Report title", "Ethnobotanical survey report"),
        shiny::textAreaInput(ns("notes"), "Notes (study area, methods, consent statement)",
                             rows = 4, placeholder = "e.g. Interviews were conducted in 2026 with prior informed consent ..."),
        shiny::radioButtons(ns("format"), "Report format", inline = TRUE,
                            choices = c("HTML" = "html_document", "Word (.docx)" = "word_document")),
        shiny::p(class = "text-muted small",
                 "Add results with the 'Add to report' buttons in every tab. ",
                 "Interactive-only charts (Sankey, treemap/sunburst, radar, map) are added as static pictures; ",
                 "the map has no base map in the report.")
      ),
      bslib::card(
        bslib::card_header("Excel workbook and privacy"),
        shiny::checkboxInput(ns("drop_recipes"),
                             "Remove preparation and administration (detailed recipes) from downloaded data",
                             value = TRUE),
        shiny::checkboxInput(ns("round_coords"),
                             "Round coordinates to 0.1 degree in downloaded data",
                             value = TRUE),
        shiny::p("The Excel workbook contains the cleaned data, species indices, fidelity levels, ",
                 "informant consensus factors and a summary sheet."),
        shiny::downloadButton(ns("excel"), "Download Excel workbook", class = "btn-outline-primary"),
        shiny::hr(),
        shiny::p(class = "text-muted small",
                 "Ethnobotanical data often contains traditional knowledge that belongs to communities. ",
                 "Share it only with consent, anonymise informants (use codes, never names) and respect any benefit-sharing agreements.")
      )
    )
  )
}

# @param report the shared basket made by new_report_store()
mod_report_server <- function(id, data, report) {
  shiny::moduleServer(id, function(input, output, session) {

    summary_list <- function(df) {
      list(informants = dplyr::n_distinct(df$informant_id),
           species = dplyr::n_distinct(df$scientific_name),
           families = dplyr::n_distinct(df$family, na.rm = TRUE),
           reports = nrow(df),
           categories = dplyr::n_distinct(df$use_category))
    }

    output$excel <- shiny::downloadHandler(
      filename = function() paste0("ethnobotany_results_", Sys.Date(), ".xlsx"),
      content = function(file) {
        df <- data(); shiny::req(df)
        shared <- anonymise_data(df, input$drop_recipes, input$round_coords)
        s <- summary_list(df)
        wb <- openxlsx::createWorkbook()
        add_sheet <- function(name, x) {
          openxlsx::addWorksheet(wb, name)
          openxlsx::writeData(wb, name, as.data.frame(x))
          openxlsx::setColWidths(wb, name, cols = seq_len(ncol(x)), widths = "auto")
          openxlsx::freezePane(wb, name, firstRow = TRUE)
          # scientific names in italics
          sp_cols <- which(names(x) %in% SPECIES_COLS)
          if (length(sp_cols) && nrow(x) > 0) {
            openxlsx::addStyle(wb, name, openxlsx::createStyle(textDecoration = "italic"),
                               rows = 1 + seq_len(nrow(x)), cols = sp_cols, gridExpand = TRUE, stack = TRUE)
          }
        }
        add_sheet("Summary", data.frame(item = names(s), value = unlist(s)))
        add_sheet("Clean_data", shared)
        add_sheet("Species_indices", species_indices(df))
        add_sheet("Fidelity_level", fidelity_level(df))
        add_sheet("ICF_use_category", informant_consensus(df, "use_category"))
        add_sheet("ICF_ailment", informant_consensus(df, "ailment"))
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )


    ids <- shiny::reactive(names(report$items))

    # ---- the list of added results ------------------------------------------------
    output$basket <- shiny::renderUI({
      its <- report$items
      if (!length(its)) {
        return(shiny::div(class = "form-check m-2",
                          shiny::tags$input(type = "checkbox", class = "form-check-input",
                                            checked = NA, disabled = NA),
                          shiny::tags$label(class = "form-check-label", "Empty Report")))
      }
      its <- order_report_items(its)
      choices <- stats::setNames(names(its), vapply(its, report_label, character(1)))
      keep <- shiny::isolate(input$basket_sel)
      new  <- setdiff(names(its), shiny::isolate(known()))
      known(names(its))
      shiny::checkboxGroupInput(session$ns("basket_sel"), NULL, choices = choices,
                                selected = union(intersect(keep, names(its)), new), width = "100%")
    })
    known <- shiny::reactiveVal(character())

    shiny::observeEvent(input$select_all, {
      shiny::updateCheckboxGroupInput(session, "basket_sel", selected = ids())
    })
    shiny::observeEvent(input$deselect_all, {
      shiny::updateCheckboxGroupInput(session, "basket_sel", selected = character())
    })

    shiny::observeEvent(input$delete, {
      if (!length(ids())) return()
      shiny::showModal(shiny::modalDialog(
        title = "Delete the report?",
        paste("All", length(ids()), "results added to the report will be removed."),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
                                shiny::actionButton(session$ns("delete_yes"), "Delete", class = "btn-danger")),
        easyClose = TRUE))
    })
    shiny::observeEvent(input$delete_yes, {
      unlink(unlist(lapply(report$items, function(i) i$path)))
      report$items <- list()
      known(character())
      shiny::removeModal()
    })

    output$report <- shiny::downloadHandler(
      filename = function() {
        paste0("ethnobotany_report_", Sys.Date(),
               if (identical(input$format, "word_document")) ".docx" else ".html")
      },
      content = function(file) {
        chosen <- report$items[intersect(input$basket_sel, ids())]
        shiny::withProgress(message = "Building report...", value = 0.5, {
          build_report(chosen, input$title, input$notes, input$format, file)
        })
      }
    )
  })
}
