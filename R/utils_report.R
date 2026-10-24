# =============================================================================
# utils_report.R
# The report basket: every analysis tab has an "Add to report" button that
# stores a static copy of its result (plot saved as PNG, table, or text).
# The Report tab lists the stored items, and exports the selected ones.
# =============================================================================

# Order of the sections in the exported report
REPORT_SECTIONS <- c("Data", "Taxonomy", "Profile", "Map", "Indices", "Categories",
                     "Comparison", "Correlation", "Multivariate", "Network", "Visuals")

#' The shared basket (one per app session)
#' @noRd
new_report_store <- function() shiny::reactiveValues(items = list())

#' Resolution (dpi) of the plots stored in the report basket.
#' Default 300 (journal quality); change with options(ethnoshiny.dpi = 600)
#' before running the app. Values are kept between 72 and 1200.
#' @noRd
report_dpi <- function() {
  d <- suppressWarnings(as.numeric(getOption("ethnoshiny.dpi", 300)))
  if (length(d) != 1L || is.na(d)) d <- 300
  min(max(d, 72), 1200)
}

report_dir <- function() {
  d <- file.path(tempdir(), "ethnoshiny_report")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

#' Add (or replace) one result in the report basket
#' @param report the store made by new_report_store(); NULL does nothing
#' @param id unique id of the result (adding it again replaces the old copy)
#' @param section one of REPORT_SECTIONS
#' @param plot a ggplot object, or a function that draws a base-R plot
#' @param table a data frame
#' @param text a character vector (verbatim output)
#' @noRd
add_to_report <- function(report, id, section, title, plot = NULL, table = NULL,
                          text = NULL, width = 7, height = 4.5, notify = TRUE) {
  if (is.null(report)) return(invisible(FALSE))
  item <- list(id = id, section = section, title = title, type = NA_character_,
               path = NULL, table = NULL, text = NULL)
  if (!is.null(plot)) {
    path <- file.path(report_dir(), paste0(gsub("[^A-Za-z0-9_-]", "_", id), ".png"))
    ok <- tryCatch({
      if (inherits(plot, "ggplot")) {
        plot <- italicize_static_plot(plot)
        ggplot2::ggsave(path, plot, width = width, height = height, dpi = report_dpi(), bg = "white")
      } else if (is.function(plot)) {
        grDevices::png(path, width = width, height = height, units = "in", res = report_dpi(), bg = "white")
        on.exit(grDevices::dev.off(), add = TRUE)
        plot()
      } else stop("plot must be a ggplot or a function")
      file.exists(path)
    }, error = function(e) FALSE)
    if (!isTRUE(ok)) {
      if (notify) tryCatch(shiny::showNotification("This chart could not be added to the report.",
                                                   type = "error"), error = function(e) NULL)
      return(invisible(FALSE))
    }
    item$type <- "plot"; item$path <- path
  } else if (!is.null(table)) {
    item$type <- "table"; item$table <- as.data.frame(table, stringsAsFactors = FALSE)
  } else if (!is.null(text)) {
    item$type <- "text"; item$text <- as.character(text)
  } else stop("Give a plot, a table or a text")

  items <- report$items
  items[[id]] <- item
  report$items <- items
  if (notify) tryCatch(shiny::showNotification(paste0("Added to report: ", title),
                                               type = "message", duration = 3),
                       error = function(e) NULL)
  invisible(TRUE)
}

#' Label shown next to the check box of an item
#' @noRd
report_label <- function(item) paste0(item$section, " - ", item$title)

#' Items ordered by section (the order of adding is kept inside a section)
#' @noRd
order_report_items <- function(items) {
  if (!length(items)) return(items)
  sec <- vapply(items, function(i) match(i$section, REPORT_SECTIONS, nomatch = 99L), integer(1))
  items[order(sec)]
}

#' Pipe table with scientific names in italics
#' @noRd
report_table_md <- function(df) {
  for (cn in intersect(SPECIES_COLS, names(df))) df[[cn]] <- italicize_name(df[[cn]], "markdown")
  num <- vapply(df, function(v) is.numeric(v) && !is.integer(v), logical(1))
  for (cn in names(df)[num]) df[[cn]] <- round(df[[cn]], 3)
  paste(knitr::kable(df, format = "pipe"), collapse = "\n")
}

#' Markdown body of the report for the chosen items
#' @noRd
report_markdown <- function(items, caption_plots = TRUE) {
  if (!length(items)) return("No results were selected for this report.")
  items <- order_report_items(items)
  out <- character()
  last <- ""
  n_fig <- 0L; n_tab <- 0L
  for (it in items) {
    if (!identical(it$section, last)) {
      out <- c(out, "", paste0("# ", it$section), "")
      last <- it$section
    }
    out <- c(out, paste0("## ", it$title), "")
    if (it$type == "plot") {
      n_fig <- n_fig + 1L
      out <- c(out, sprintf("![Figure %d. %s](%s){width=100%%}", n_fig, it$title, it$path), "")
    } else if (it$type == "table") {
      n_tab <- n_tab + 1L
      out <- c(out, sprintf("**Table %d.** %s", n_tab, it$title), "", report_table_md(it$table), "")
    } else {
      out <- c(out, "```", it$text, "```", "")
    }
  }
  out
}

#' Write and render the report
#' @param items list of items (already filtered to the selected ones)
#' @noRd
build_report <- function(items, title, notes, format = c("html_document", "word_document"),
                         file, summary_text = NULL) {
  format <- match.arg(format)
  tmp <- tempfile("report_"); dir.create(tmp)
  esc <- function(x) gsub("\"", "'", x, fixed = TRUE)
  yaml <- c("---",
            paste0("title: \"", esc(title), "\""),
            paste0("date: \"", format(Sys.Date(), "%d %B %Y"), "\""),
            "output:",
            "  html_document:", "    toc: true", "    toc_float: true", "    theme: flatly",
            "  word_document:", "    toc: true",
            "---", "")
  body <- c(if (!is.null(notes) && nzchar(trimws(notes))) c("## Notes", "", notes, ""),
            report_markdown(items),
            "", "---", "",
            "*Indices: Phillips and Gentry (1993) for UV, Trotter and Logan (1986) for ICF, Friedman et al. (1986) for FL, Tardio and Pardo-de-Santayana (2008) for CI. Results depend on sampling effort; interpret indices based on few informants with caution.*")
  rmd <- file.path(tmp, "report.Rmd")
  writeLines(c(yaml, body), rmd, useBytes = TRUE)
  ext <- if (format == "word_document") "docx" else "html"
  out <- rmarkdown::render(rmd, output_format = format, output_file = paste0("report.", ext),
                           envir = new.env(parent = globalenv()), quiet = TRUE)
  file.copy(out, file, overwrite = TRUE)
}

#' Small "Add to report" button placed above a result
#' @noRd
report_button <- function(ns, id, label = "Add to report") {
  shiny::actionButton(ns(id), label, icon = shiny::icon("plus"),
                      class = "btn-sm ethno-add")
}

#' Wire an "Add to report" button to a result
#' @param plot,table,text functions (usually reactives) that return the result
#' @param title a string, or a function returning one (e.g. depends on inputs)
#' @noRd
bind_report <- function(input, report, btn, id, section, title,
                        plot = NULL, table = NULL, text = NULL, width = 7, height = 4.5) {
  shiny::observeEvent(input[[btn]], {
    if (is.null(report)) return()
    ttl <- if (is.function(title)) title() else title
    res <- tryCatch(list(
      plot = if (!is.null(plot)) plot(),
      table = if (!is.null(table)) table(),
      text = if (!is.null(text)) text()),
      error = function(e) NULL)
    if (is.null(res) || (is.null(res$plot) && is.null(res$table) && is.null(res$text))) {
      shiny::showNotification("Nothing to add yet: this result is empty or cannot be drawn (see the message on the chart).", type = "warning")
      return()
    }
    add_to_report(report, id, section, ttl, plot = res$plot, table = res$table,
                  text = res$text, width = width, height = height)
  })
}

# "<i>Genus species</i> L." -> list of plotmath expressions (static plots cannot show HTML)
html_labels_to_expr <- function(x) {
  lapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(s)) return("")
    parts <- strsplit(gsub("(<i>|</i>)", "\u0001\\1\u0001", s), "\u0001", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    ital <- FALSE; items <- character()
    for (pt in parts) {
      if (pt == "<i>") ital <- TRUE
      else if (pt == "</i>") ital <- FALSE
      else {
        pt <- gsub("&lt;", "<", gsub("&gt;", ">", gsub("&amp;", "&", pt, fixed = TRUE), fixed = TRUE), fixed = TRUE)
        q <- encodeString(pt, quote = "\"")
        items <- c(items, if (ital) paste0("italic(", q, ")") else q)
      }
    }
    parse(text = paste(items, collapse = " ~ "))[[1]]
  })
}

# Make a ggplot built for plotly (with <i> tags in discrete labels) print italics in a PNG
italicize_static_plot <- function(p) {
  b <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
  if (is.null(b)) return(p)
  has_tags <- function(sc) {
    inherits(sc, "ScaleDiscrete") &&
      any(grepl("<i>", tryCatch(sc$get_labels(), error = function(e) character()), fixed = TRUE))
  }
  if (has_tags(b$layout$panel_scales_x[[1]]))
    p <- p + ggplot2::scale_x_discrete(labels = html_labels_to_expr)
  if (has_tags(b$layout$panel_scales_y[[1]]))
    p <- p + ggplot2::scale_y_discrete(labels = html_labels_to_expr)
  p
}
