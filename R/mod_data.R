# =============================================================================
# mod_data.R  -  Upload, validation and preview
# Returns a list with `data` (reactive of the cleaned data frame).
# =============================================================================

mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,
      shiny::h5("1. Choose a data source"),
      shiny::fileInput(ns("file"), "Upload CSV or Excel",
                       accept = c(".csv", ".xlsx", ".xls"),
                       buttonLabel = "Browse...", placeholder = "No file selected"),
      report_button(ns, "add_overview", "Add study overview to report"),
      shiny::actionButton(ns("example"), "Use example data",
                          icon = shiny::icon("seedling"),
                          class = "btn-outline-primary w-100"),
      shiny::hr(),
      shiny::downloadButton(ns("template"), "Download CSV template",
                            class = "btn-outline-secondary w-100"),
      shiny::hr(),
      shiny::p(class = "text-muted small",
               "One row = one use report by one informant. Required columns: ",
               shiny::code("informant_id"), ", ", shiny::code("scientific_name"), ", ",
               shiny::code("use_category"), ". All other columns are optional but unlock more analyses.")
    ),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("boxes")),
    bslib::card(
      bslib::card_header("Cleaned data preview",
                         report_button(ns, "add_data", "Add table to report")),
      DT::DTOutput(ns("table"))
    )
  )
}

# @param initial NULL (start with the example data) or an already validated
#   result of validate_data() to start with the user's own data
mod_data_server <- function(id, initial = NULL, report = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    start <- if (is.null(initial)) load_example_data() else initial
    active   <- shiny::reactiveVal(start$data)   # last VALID data set
    vres     <- shiny::reactiveVal(start)        # last validation result
    source_nm <- shiny::reactiveVal(
      if (is.null(initial)) "Example data (synthetic, for demonstration only)"
      else "Data passed to ethnoshiny()")

    shiny::observeEvent(input$example, {
      res <- load_example_data()
      vres(res); active(res$data)
      source_nm("Example data (synthetic, for demonstration only)")
    })

    shiny::observeEvent(input$file, {
      raw <- tryCatch(read_input_file(input$file$datapath, input$file$name),
                      error = function(e) e)
      if (inherits(raw, "error")) {
        vres(list(ok = FALSE, message = conditionMessage(raw), data = NULL, log = character()))
        return()
      }
      res <- validate_data(raw)
      vres(res)
      if (res$ok) {
        active(res$data)
        source_nm(paste("File:", input$file$name))
      }
    })

    output$status <- shiny::renderUI({
      r <- vres()
      shiny::tagList(
        shiny::div(class = "alert alert-secondary",
                   shiny::strong("Active source: "), source_nm()),
        if (!r$ok) shiny::div(class = "alert alert-danger",
                              shiny::strong("File rejected: "), r$message,
                              " The previous data set is still in use."),
        if (r$ok && length(r$log)) shiny::div(
          class = "alert alert-warning",
          shiny::strong("Cleaning notes"),
          shiny::tags$ul(lapply(r$log, shiny::tags$li))
        )
      )
    })

    output$boxes <- shiny::renderUI({
      df <- active(); shiny::req(df)
      bslib::layout_columns(
        fill = FALSE, col_widths = c(3, 3, 3, 3),
        bslib::value_box("Informants", dplyr::n_distinct(df$informant_id),
                         theme = bslib::value_box_theme(bg = VIRIDIS_PURPLE, fg = "white")),
        bslib::value_box("Species", dplyr::n_distinct(df$scientific_name),
                         theme = bslib::value_box_theme(bg = VIRIDIS_BLUE, fg = "white")),
        bslib::value_box("Families", dplyr::n_distinct(df$family, na.rm = TRUE),
                         theme = bslib::value_box_theme(bg = VIRIDIS_TEAL, fg = "white")),
        bslib::value_box("Use reports", nrow(df),
                         theme = bslib::value_box_theme(bg = VIRIDIS_GREEN, fg = "#1a1a1a"))
      )
    })

    output$table <- DT::renderDT({
      dt_table(active(), rownames = FALSE, filter = "top",
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    output$template <- shiny::downloadHandler(
      filename = function() "template_ethnobotany.csv",
      content = function(file) {
        file.copy(ext_file("template_ethnobotany.csv"), file, overwrite = TRUE)
      }
    )

    overview <- shiny::reactive({
      df <- active(); shiny::req(df)
      data.frame(
        Item = c("Informants", "Species", "Families", "Use categories", "Use reports"),
        Count = c(dplyr::n_distinct(df$informant_id), dplyr::n_distinct(df$scientific_name),
                  dplyr::n_distinct(df$family, na.rm = TRUE), dplyr::n_distinct(df$use_category),
                  nrow(df)))
    })
    bind_report(input, report, "add_overview", "data_overview", "Data",
                "Study overview", table = overview)
    bind_report(input, report, "add_data", "data_table", "Data",
                "Cleaned data", table = function() active())

    list(data = shiny::reactive(active()))
  })
}
