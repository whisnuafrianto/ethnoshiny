app_server <- function(initial = NULL) {
  force(initial)
  function(input, output, session) {
    rep <- new_report_store()                   # basket of results added to the report
    d   <- mod_data_server("data", initial, rep)  # d$data is a reactive: the cleaned data set

    mod_taxonomy_server("tax", d$data, rep)
    mod_profile_server("profile", d$data, rep)
    mod_categories_server("cats", d$data, rep)
    mod_visuals_server("vis", d$data, rep)
    mod_map_server("map", d$data, rep)
    mod_indices_server("idx", d$data, rep)
    mod_compare_server("cmp", d$data, rep)
    mod_correlation_server("cor", d$data, rep)
    mod_multivar_server("mv", d$data, rep)
    mod_network_server("net", d$data, rep)
    mod_report_server("rep", d$data, rep)
  }
}
