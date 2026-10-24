#' Launch the EthnoBotani Explorer
#'
#' One command, like `biblioshiny()` in bibliometrix:
#' `library(ethnoshiny); ethnoshiny()`.
#'
#' @param data `NULL` (start with the built-in synthetic example) or your own
#'   data: a data frame, or the path to a CSV / Excel file. It is validated
#'   with the same rules as an upload in the app; if it is rejected the
#'   function stops with the reason. You can always upload another file in the
#'   app itself.
#' @param host Host passed to [shiny::runApp()].
#' @param port Port, or `NULL` for a random free port.
#' @param launch.browser Open the app in the browser? Defaults to
#'   `interactive()`.
#' @param max_upload_mb Largest file that can be uploaded in the app (MB).
#' @return Called for its side effect (a running app); returns `NULL`
#'   invisibly when the app is closed.
#' @examples
#' if (interactive()) {
#'   ethnoshiny()                                  # example data
#'   ethnoshiny(ethnoshiny_example())              # a data frame
#'   ethnoshiny("my_interviews.xlsx")              # your own file
#' }
#' @export
ethnoshiny <- function(data = NULL, host = "127.0.0.1", port = NULL,
                       launch.browser = interactive(), max_upload_mb = 30) {
  app <- ethnoshiny_app(data, max_upload_mb)
  report_missing_packages()
  shiny::runApp(app, host = host, port = port, launch.browser = launch.browser)
}

#' Build the Shiny app object without running it
#'
#' Useful for deployment (shinyapps.io, Posit Connect, Shiny Server):
#' `ethnoshiny::ethnoshiny_app()` returns a `shiny.appobj`.
#'
#' @inheritParams ethnoshiny
#' @return A Shiny app object.
#' @export
ethnoshiny_app <- function(data = NULL, max_upload_mb = 30) {
  initial <- NULL
  if (!is.null(data)) {
    raw <- if (is.character(data) && length(data) == 1) read_input_file(data, basename(data)) else data
    initial <- validate_data(raw)
    if (!isTRUE(initial$ok)) stop("The data cannot be used: ", initial$message, call. = FALSE)
  }
  options(shiny.maxRequestSize = max_upload_mb * 1024^2)
  shiny::shinyApp(ui = app_ui(), server = app_server(initial))
}

#' The built-in synthetic example data
#'
#' 66 informants, 31 species, 876 use reports in 3 villages. Invented for
#' demonstration; it does not describe real communities.
#' @return A data frame.
#' @export
ethnoshiny_example <- function() utils::read.csv(ext_file("example_ethnobotany.csv"),
                                                 stringsAsFactors = FALSE)

#' Write an empty data template (CSV)
#'
#' @param path Where to write the file.
#' @return The path, invisibly.
#' @export
ethnoshiny_template <- function(path = "template_ethnobotany.csv") {
  file.copy(ext_file("template_ethnobotany.csv"), path, overwrite = TRUE)
  message("Template written to ", normalizePath(path))
  invisible(path)
}

# Tell the user once which optional packages would unlock more features
report_missing_packages <- function() {
  opt <- c(visNetwork = "interactive network", leaflet = "base map",
           ggVennDiagram = "Venn diagram", rgbif = "online GBIF name check")
  miss <- opt[!vapply(names(opt), requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    message("Optional packages not installed (the app still works with fallbacks): ",
            paste0(names(miss), " (", miss, ")", collapse = ", "),
            "\nInstall with: install.packages(c(",
            paste0('"', names(miss), '"', collapse = ", "), "))")
  }
}
