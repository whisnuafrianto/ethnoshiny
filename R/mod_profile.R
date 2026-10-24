# =============================================================================
# mod_profile.R  -  Informant profile (one row per informant)
# Every chart has an "Add to report" button.
# =============================================================================

mod_profile_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(bslib::card_header("Gender", report_button(ns, "add_gender")),
                plotly::plotlyOutput(ns("gender"), height = 280)),
    bslib::card(bslib::card_header("Age", report_button(ns, "add_age")),
                plotly::plotlyOutput(ns("age"), height = 280)),
    bslib::card(bslib::card_header("Village", report_button(ns, "add_village")),
                plotly::plotlyOutput(ns("village"), height = 280)),
    bslib::card(bslib::card_header("Ethnic group", report_button(ns, "add_ethnic")),
                plotly::plotlyOutput(ns("ethnic"), height = 280)),
    bslib::card(bslib::card_header("Occupation", report_button(ns, "add_occupation")),
                plotly::plotlyOutput(ns("occupation"), height = 280)),
    bslib::card(bslib::card_header("Years of experience", report_button(ns, "add_experience")),
                plotly::plotlyOutput(ns("experience"), height = 280))
  )
}

mod_profile_server <- function(id, data, report = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    informants <- shiny::reactive({
      shiny::req(data())
      dplyr::distinct(data(), informant_id, .keep_all = TRUE)
    })

    draw <- function(p) plotly::ggplotly(p)

    # One chart = one reactive ggplot, drawn in the app and stored in the report
    profile_chart <- function(key, label, make_plot) {
      p <- shiny::reactive(make_plot())
      output[[key]] <- plotly::renderPlotly(draw(p()))
      bind_report(input, report, paste0("add_", key), paste0("prof_", key),
                  "Profile", label, plot = p)
    }

    profile_chart("gender", "Informants by gender",
                  function() plot_counts(informants(), "gender", NULL))
    profile_chart("village", "Informants by village",
                  function() plot_counts(informants(), "village", NULL))
    profile_chart("ethnic", "Informants by ethnic group",
                  function() plot_counts(informants(), "ethnic_group", NULL))
    profile_chart("occupation", "Informants by occupation",
                  function() plot_counts(informants(), "occupation", NULL))
    profile_chart("age", "Age of informants",
                  function() plot_histogram(informants(), "age", NULL, 5))
    profile_chart("experience", "Years of experience of informants",
                  function() plot_histogram(informants(), "experience_years", NULL, 5))
  })
}
