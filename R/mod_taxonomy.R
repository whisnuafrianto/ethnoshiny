# =============================================================================
# mod_taxonomy.R  -  Scientific name checks (Version 3)
#  1. Offline consistency checks (always available)
#  2. Optional online check against the GBIF Backbone Taxonomy (package rgbif)
# =============================================================================

mod_taxonomy_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header("1. Offline consistency checks",
                         report_button(ns, "add_offline", "Add table to report")),
      shiny::p(class = "text-muted small",
               "Flags names that do not look like 'Genus epithet' and families that are inconsistent within a genus."),
      DT::DTOutput(ns("offline"))
    ),
    bslib::card(
      bslib::card_header("2. Check names against GBIF (needs internet and the 'rgbif' package)",
                         report_button(ns, "add_gbif", "Add table to report")),
      shiny::actionButton(ns("run_gbif"), "Check names with GBIF", icon = shiny::icon("globe"),
                          class = "btn-primary"),
      shiny::downloadButton(ns("download"), "Download result (CSV)", class = "btn-outline-secondary"),
      shiny::p(class = "text-muted small mt-2",
               "Verdicts: 'OK' = accepted name, exact match; 'Synonym' = GBIF treats the name as a synonym, review it; ",
               "'Fuzzy match' = GBIF matched a similar spelling; 'Not found' = no match. Always confirm with a taxonomist or a database such as POWO."),
      DT::DTOutput(ns("gbif"))
    )
  )
}

mod_taxonomy_server <- function(id, data, report = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    offline_tbl <- shiny::reactive({
      shiny::req(data())
      taxonomy_offline_checks(data())
    })

    output$offline <- DT::renderDT({
      dt_table(offline_tbl(), rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })
    bind_report(input, report, "add_offline", "tax_offline", "Taxonomy",
                "Offline name checks", table = offline_tbl)

    gbif_result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run_gbif, {
      shiny::req(data())
      if (!requireNamespace("rgbif", quietly = TRUE)) {
        shiny::showNotification("Package 'rgbif' is not installed. Run install.packages('rgbif').",
                                type = "error", duration = 8)
        return()
      }
      nm <- utils::head(sort(unique(data()$scientific_name)), 200)
      res <- NULL
      shiny::withProgress(message = "Querying GBIF Backbone Taxonomy...", value = 0.5, {
        res <- tryCatch(rgbif::name_backbone_checklist(nm), error = function(e) e)
      })
      if (inherits(res, "error")) {
        shiny::showNotification(paste("GBIF request failed:", conditionMessage(res)),
                                type = "error", duration = 8)
        return()
      }
      for (cn in c("scientificName", "status", "matchType", "family", "confidence")) {
        if (!cn %in% names(res)) res[[cn]] <- NA
      }
      recorded <- data() |>
        dplyr::group_by(scientific_name) |>
        dplyr::summarise(recorded_family = first_non_na(family), .groups = "drop")

      out <- data.frame(
        input_name = nm,
        gbif_name = res$scientificName,
        status = res$status,
        match_type = res$matchType,
        gbif_family = res$family,
        confidence = res$confidence,
        stringsAsFactors = FALSE
      )
      out <- dplyr::left_join(out, recorded, by = c("input_name" = "scientific_name"))
      out$verdict <- dplyr::case_when(
        is.na(out$match_type) | out$match_type %in% c("NONE", "HIGHERRANK") ~ "Not found",
        out$match_type == "FUZZY" ~ "Fuzzy match",
        out$status == "SYNONYM" ~ "Synonym",
        !is.na(out$recorded_family) & !is.na(out$gbif_family) &
          out$recorded_family != out$gbif_family ~ "Family differs",
        TRUE ~ "OK"
      )
      gbif_result(out)
    })

    output$gbif <- DT::renderDT({
      shiny::req(gbif_result())
      dt_table(gbif_result(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })

    bind_report(input, report, "add_gbif", "tax_gbif", "Taxonomy",
                "GBIF name check", table = function() { shiny::req(gbif_result()); gbif_result() })

    output$download <- shiny::downloadHandler(
      filename = function() paste0("taxonomy_check_", Sys.Date(), ".csv"),
      content = function(file) {
        res <- gbif_result()
        if (is.null(res)) res <- data.frame(message = "Run the GBIF check first.")
        utils::write.csv(res, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
      }
    )
  })
}
