#' ethnoshiny: Quantitative Ethnobotany in a Shiny App
#'
#' Launch the app with [ethnoshiny()]. Scientific names are written in italics
#' everywhere; charts use the viridis palette.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data
NULL

# Columns used with bare names inside dplyr / ggplot2 calls
utils::globalVariables(c(
  "n", "a", "b", "value", "label", "xi", "yi", "r", "x", "y", "category", "NMDS1", "NMDS2",
  "group", "scientific_name", "use_category", "informant_id", "target", "place",
  "latitude", "longitude", "village", "lat", "lon", "reports", "informants",
  "species", "Iu", "FL", "UV", "ICF", "groups_using", "grp", "FC", "sp_label", "CI", "Ip", "NU", "Nt", "Nur", "RFC", "UR",
  "age", "ailment", "ci_n", "experience_years", "family", "genus", "informant", "issue",
  "local_name", "n_local_names", "plant_part", "present", "unit",
  "x0", "x1", "y0", "y1", "ymin", "ymax", "ym", "link", "col", "level", "labels", "top",
  "txt", "face", "share"
))

# Path of a file shipped in inst/extdata
ext_file <- function(name) system.file("extdata", name, package = "ethnoshiny")

has_visnetwork <- function() requireNamespace("visNetwork", quietly = TRUE)
has_leaflet    <- function() requireNamespace("leaflet", quietly = TRUE)
