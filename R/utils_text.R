# =============================================================================
# utils_text.R
# Botanical typography: scientific names are written in italics.
#
# Only the genus and the epithet are italic. Authorities (L., Roxb.), rank
# markers (subsp., var., f.), the hybrid sign and "sp." stay upright:
#   Curcuma longa L.            -> *Curcuma longa* L.
#   Oryza sativa subsp. japonica -> *Oryza sativa* subsp. *japonica*
#   Musa x paradisiaca          -> *Musa* x *paradisiaca*
#   Piper sp.                   -> *Piper* sp.
# =============================================================================

# Columns that hold scientific names (used to italicise whole table columns)
SPECIES_COLS <- c("scientific_name", "species", "species_1", "species_2",
                  "input_name", "gbif_name")

RANK_WORDS  <- c("subsp", "ssp", "var", "subvar", "f", "forma", "cv", "nothosubsp")
UNCERTAIN   <- c("sp", "spp", "aff", "cf")

# Split a name into runs of text, flagging which runs are italic
name_parts <- function(s) {
  tok <- strsplit(trimws(s), "\\s+")[[1]]
  n <- length(tok)
  if (n == 0) return(data.frame(text = character(), italic = logical()))

  italic <- rep(FALSE, n)
  italic[1] <- TRUE                       # the genus
  in_authority <- FALSE
  expect_epithet <- TRUE

  for (i in seq_len(n)[-1]) {
    t <- tok[i]
    base <- sub("\\.$", "", t)
    if (base %in% c("x", "\u00d7") && !in_authority) {          # hybrid sign
      expect_epithet <- TRUE
    } else if (tolower(base) %in% RANK_WORDS) {                  # subsp. var. f.
      expect_epithet <- TRUE
      in_authority <- FALSE
    } else if (tolower(base) %in% UNCERTAIN) {                   # sp. spp. aff. cf.
      expect_epithet <- FALSE
    } else if (!in_authority && expect_epithet && grepl("^[a-z][a-z-]*$", t)) {
      italic[i] <- TRUE                                          # the epithet
      expect_epithet <- FALSE
    } else {                                                     # authority text
      in_authority <- TRUE
      expect_epithet <- FALSE
    }
  }

  # Merge neighbouring tokens that share the same style into runs
  runs <- rle(italic)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  data.frame(
    text = mapply(function(a, b) paste(tok[a:b], collapse = " "), starts, ends),
    italic = runs$values,
    stringsAsFactors = FALSE
  )
}

#' Write scientific names with italic genus and epithet
#' @param x character vector of names (NA and "" are returned unchanged)
#' @param format "html" (<i>..</i>, for plotly and HTML), "markdown" (*..*, for
#'   Word/HTML reports) or "vis" (_.._, for visNetwork labels)
#' @return character vector
#' @examples
#' italicize_name("Curcuma longa L.")
#' italicize_name("Oryza sativa subsp. japonica", format = "markdown")
#' @export
italicize_name <- function(x, format = c("html", "markdown", "vis")) {
  format <- match.arg(format)
  wrap <- switch(format,
                 html     = function(t) paste0("<i>", t, "</i>"),
                 markdown = function(t) paste0("*", t, "*"),
                 vis      = function(t) paste0("_", t, "_"))
  esc <- if (format == "html") {
    function(t) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", t, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  } else identity

  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return(s)
    p <- name_parts(s)
    paste(ifelse(p$italic, wrap(esc(p$text)), esc(p$text)), collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

#' Axis labels for ggplot2: italic genus and epithet, upright authority.
#' Use as `scale_x_discrete(labels = species_axis_labels)`.
#' @param x character vector of names
#' @return list of plotmath expressions
#' @export
species_axis_labels <- function(x) {
  lapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return("")
    p <- name_parts(s)
    q <- encodeString(p$text, quote = "\"")
    items <- ifelse(p$italic, paste0("italic(", q, ")"), q)
    parse(text = paste(items, collapse = " ~ "))[[1]]
  })
}

# Italicise the scientific-name columns of a DT table
italicise_dt <- function(dt, df) {
  cols <- intersect(SPECIES_COLS, names(df))
  if (length(cols)) DT::formatStyle(dt, cols, fontStyle = "italic") else dt
}

# Options for selectizeInput that show choices in italics
# (values listed in `plain_values`, such as "All species", stay upright)
selectize_italic <- function(plain_values = character()) {
  plain <- paste0("[", paste0("'", plain_values, "'", collapse = ","), "]")
  fmt <- paste0(
    "function(item, escape) {",
    " var t = escape(item.label);",
    " return '<div>' + (", plain, ".indexOf(item.value) >= 0 ? t : '<em>' + t + '</em>') + '</div>';",
    " }")
  list(render = I(paste0("{ option: ", fmt, ", item: ", fmt, " }")))
}

# DT table whose scientific-name columns are shown in italics
dt_table <- function(data, ...) italicise_dt(DT::datatable(data, ...), data)
