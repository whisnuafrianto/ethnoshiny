# =============================================================================
# utils_indices.R
# Pure functions (no Shiny code): data validation, cleaning and ethnobotanical
# indices. Keeping them separate from the UI makes them easy to test.
#
# Data model ("long" format): ONE ROW = ONE USE REPORT by ONE informant.
# =============================================================================

# (APP_DIR removed: files are located with system.file())

REQUIRED_COLS <- c("informant_id", "scientific_name", "use_category")

OPTIONAL_COLS <- c(
  "gender", "age", "village", "ethnic_group", "occupation", "experience_years",
  "local_name", "family", "habitus", "plant_part", "preparation",
  "administration", "ailment", "latitude", "longitude"
)

NUMERIC_COLS <- c("age", "experience_years", "latitude", "longitude")

# ---- Reading files -----------------------------------------------------------

# Read a CSV (comma or semicolon separated) or Excel file
read_input_file <- function(path, name = basename(path)) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) {
    return(as.data.frame(readxl::read_excel(path)))
  }
  if (ext == "csv") {
    first_line <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
    n_semicolon <- lengths(regmatches(first_line, gregexpr(";", first_line)))
    n_comma     <- lengths(regmatches(first_line, gregexpr(",", first_line)))
    sep <- if (n_semicolon > n_comma) ";" else ","
    return(utils::read.csv(path, sep = sep, stringsAsFactors = FALSE,
                           fileEncoding = "UTF-8-BOM", check.names = FALSE))
  }
  stop("Unsupported file type. Please upload .csv, .xlsx or .xls")
}

# ---- Validation & cleaning ---------------------------------------------------

first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) x[1] else NA_character_
}

# Validate and clean a raw data frame
# @return list(ok, message, data, log)
validate_data <- function(df_raw) {
  log <- character()
  df <- as.data.frame(df_raw, stringsAsFactors = FALSE)

  # Standardise column names: "Informant ID" -> "informant_id"
  names(df) <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(names(df)))))

  missing_cols <- setdiff(REQUIRED_COLS, names(df))
  if (length(missing_cols)) {
    return(list(
      ok = FALSE,
      message = paste0("Missing required column(s): ",
                       paste(missing_cols, collapse = ", "), "."),
      data = NULL, log = log
    ))
  }

  # Add optional columns that are absent
  for (col in setdiff(OPTIONAL_COLS, names(df))) {
    df[[col]] <- if (col %in% NUMERIC_COLS) NA_real_ else NA_character_
  }

  # Trim text, turn empty strings into NA
  text_cols <- setdiff(names(df), NUMERIC_COLS)
  for (col in text_cols) {
    x <- trimws(as.character(df[[col]]))
    x[x %in% c("", "NA", "N/A", "-")] <- NA_character_
    df[[col]] <- x
  }
  for (col in NUMERIC_COLS) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }

  # Standardise scientific names: single spaces, capitalised genus
  df$scientific_name <- gsub("\\s+", " ", df$scientific_name)
  df$scientific_name <- ifelse(
    is.na(df$scientific_name), NA_character_,
    paste0(toupper(substring(df$scientific_name, 1, 1)),
           substring(df$scientific_name, 2))
  )

  # Remove rows lacking required values
  bad <- is.na(df$informant_id) | is.na(df$scientific_name) | is.na(df$use_category)
  if (any(bad)) {
    log <- c(log, sprintf("%d row(s) removed because informant_id, scientific_name or use_category was empty.", sum(bad)))
    df <- df[!bad, , drop = FALSE]
  }
  if (nrow(df) == 0) {
    return(list(ok = FALSE, message = "No valid rows remain after cleaning.",
                data = NULL, log = log))
  }

  # Remove exact duplicate reports
  key_cols <- c("informant_id", "scientific_name", "use_category",
                "plant_part", "preparation", "administration", "ailment")
  dup <- duplicated(df[, key_cols])
  if (any(dup)) {
    log <- c(log, sprintf("%d duplicate use report(s) removed.", sum(dup)))
    df <- df[!dup, , drop = FALSE]
  }

  # Soft warnings
  if (dplyr::n_distinct(df$informant_id) < 10) {
    log <- c(log, "Fewer than 10 informants: quantitative indices and multivariate analyses will be unstable.")
  }
  bad_names <- sum(!is_binomial(unique(df$scientific_name)))
  if (bad_names > 0) {
    log <- c(log, sprintf("%d scientific name(s) do not look like 'Genus epithet'. Check the 'Taxonomy check' tab.", bad_names))
  }

  rownames(df) <- NULL
  list(ok = TRUE, message = "OK", data = df, log = log)
}

# Does a name look like "Genus epithet ..."?
is_binomial <- function(x) {
  grepl("^[A-Z][a-z-]+ (x |\u00d7 )?[a-z][a-z-]+", x)
}

load_example_data <- function(path = ext_file("example_ethnobotany.csv")) {
  if (!file.exists(path)) stop("Example data file not found: ", path)
  validate_data(read_input_file(path))
}

# Remove sensitive columns before sharing data
anonymise_data <- function(df, drop_recipes = TRUE, round_coords = TRUE) {
  if (drop_recipes) {
    df$preparation <- NA_character_
    df$administration <- NA_character_
  }
  if (round_coords) {
    df$latitude  <- round(df$latitude, 1)    # ~11 km precision
    df$longitude <- round(df$longitude, 1)
  }
  df
}

# ---- Quantitative indices ----------------------------------------------------

# Species-level indices
#  FC  Frequency of Citation: informants who cited the species
#  RFC Relative FC = FC / N
#  UR  Use Reports (rows)
#  UV  Use Value = UR / N  (Phillips & Gentry 1993)
#  NU  Number of use categories
#  CI  Cultural Importance = sum over use categories of (informants / N)
#      (Tardio & Pardo-de-Santayana 2008)
species_indices <- function(df) {
  N <- dplyr::n_distinct(df$informant_id)

  ci <- df |>
    dplyr::distinct(scientific_name, use_category, informant_id) |>
    dplyr::count(scientific_name, name = "ci_n")

  df |>
    dplyr::group_by(scientific_name) |>
    dplyr::summarise(
      local_name = first_non_na(local_name),
      family     = first_non_na(family),
      FC = dplyr::n_distinct(informant_id),
      UR = dplyr::n(),
      NU = dplyr::n_distinct(use_category),
      .groups = "drop"
    ) |>
    dplyr::left_join(ci, by = "scientific_name") |>
    dplyr::mutate(
      RFC = FC / N,
      UV  = UR / N,
      CI  = ci_n / N
    ) |>
    dplyr::select(scientific_name, local_name, family, FC, RFC, UR, UV, NU, CI) |>
    dplyr::arrange(dplyr::desc(FC), dplyr::desc(UV))
}

# Fidelity Level for each species x use category
#  FL = Ip / Iu * 100
#  Ip: informants citing the species for that category
#  Iu: informants citing the species for any category
fidelity_level <- function(df) {
  iu <- df |>
    dplyr::distinct(scientific_name, informant_id) |>
    dplyr::count(scientific_name, name = "Iu")

  df |>
    dplyr::distinct(scientific_name, use_category, informant_id) |>
    dplyr::count(scientific_name, use_category, name = "Ip") |>
    dplyr::left_join(iu, by = "scientific_name") |>
    dplyr::mutate(FL = Ip / Iu * 100) |>
    dplyr::arrange(scientific_name, dplyr::desc(FL))
}

# Main use (highest FL) of every species
fidelity_main_use <- function(df, min_iu = 1) {
  fidelity_level(df) |>
    dplyr::filter(Iu >= min_iu) |>
    dplyr::group_by(scientific_name) |>
    dplyr::slice_max(FL, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(FL), dplyr::desc(Iu))
}

# Informant Consensus Factor (Trotter & Logan 1986)
#  ICF = (Nur - Nt) / (Nur - 1)
#  Nur: use reports in the category; Nt: species used in the category
# @param by "use_category" or "ailment"
informant_consensus <- function(df, by = c("use_category", "ailment")) {
  by <- match.arg(by)
  d <- df
  d$category <- d[[by]]
  d |>
    dplyr::filter(!is.na(category)) |>
    dplyr::group_by(category) |>
    dplyr::summarise(
      Nur = dplyr::n(),
      Nt  = dplyr::n_distinct(scientific_name),
      .groups = "drop"
    ) |>
    dplyr::mutate(ICF = ifelse(Nur > 1, (Nur - Nt) / (Nur - 1), NA_real_)) |>
    dplyr::arrange(dplyr::desc(ICF))
}

# ---- Group comparison --------------------------------------------------------

# Named list: group -> unique species
species_by_group <- function(df, column) {
  d <- df[!is.na(df[[column]]), , drop = FALSE]
  lapply(split(d$scientific_name, as.character(d[[column]])), unique)
}

# Pairwise similarity matrix between species lists
similarity_matrix <- function(lst, method = c("jaccard", "sorensen")) {
  method <- match.arg(method)
  nm <- names(lst)
  n <- length(nm)
  m <- matrix(NA_real_, n, n, dimnames = list(nm, nm))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      a <- lst[[i]]; b <- lst[[j]]
      inter <- length(intersect(a, b))
      if (method == "jaccard") {
        uni <- length(union(a, b))
        m[i, j] <- if (uni == 0) NA_real_ else inter / uni
      } else {
        tot <- length(a) + length(b)
        m[i, j] <- if (tot == 0) NA_real_ else 2 * inter / tot
      }
    }
  }
  m
}

# ---- Multivariate ------------------------------------------------------------

# Informant x species presence/absence matrix
informant_species_matrix <- function(df) {
  w <- df |>
    dplyr::distinct(informant_id, scientific_name) |>
    dplyr::mutate(present = 1L) |>
    tidyr::pivot_wider(names_from = scientific_name, values_from = present,
                       values_fill = 0L)
  m <- as.matrix(w[, setdiff(names(w), "informant_id"), drop = FALSE])
  rownames(m) <- as.character(w$informant_id)
  m
}

# ---- Taxonomy (offline checks) ----------------------------------------------

# Internal consistency checks that need no internet
taxonomy_offline_checks <- function(df) {
  sp <- df |>
    dplyr::group_by(scientific_name) |>
    dplyr::summarise(
      family = first_non_na(family),
      n_local_names = dplyr::n_distinct(local_name, na.rm = TRUE),
      n_families = dplyr::n_distinct(family, na.rm = TRUE),
      reports = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      genus = sub(" .*", "", scientific_name),
      format_ok = is_binomial(scientific_name)
    )

  genus_family <- sp |>
    dplyr::filter(!is.na(family)) |>
    dplyr::group_by(genus) |>
    dplyr::summarise(n_fam_genus = dplyr::n_distinct(family), .groups = "drop")

  sp |>
    dplyr::left_join(genus_family, by = "genus") |>
    dplyr::mutate(
      issue = dplyr::case_when(
        !format_ok ~ "Name does not look like 'Genus epithet'",
        n_families > 1 ~ "Several families recorded for this name",
        !is.na(n_fam_genus) & n_fam_genus > 1 ~ "Same genus recorded under different families",
        TRUE ~ "No issue found"
      )
    ) |>
    dplyr::select(scientific_name, family, n_local_names, reports, issue)
}
