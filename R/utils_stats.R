# =============================================================================
# utils_stats.R
# Pure functions (no Shiny): correlation analysis and data preparation for the
# special charts (Sankey, treemap, waffle).
# =============================================================================

# ---- Small helpers -----------------------------------------------------------

first_non_na_num <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) x[1] else NA_real_
}

# "***", "**", "*" or "" for p-values
sig_stars <- function(p) {
  out <- rep("", length(p))
  out[!is.na(p) & p < 0.05]  <- "*"
  out[!is.na(p) & p < 0.01]  <- "**"
  out[!is.na(p) & p < 0.001] <- "***"
  out
}

# Order the variables of a correlation matrix so related ones sit together
order_by_cluster <- function(r) {
  r2 <- r
  r2[is.na(r2)] <- 0
  if (nrow(r2) < 3) return(rownames(r))
  hc <- stats::hclust(stats::as.dist(1 - r2), method = "average")
  rownames(r)[hc$order]
}

# ---- Tables to correlate -----------------------------------------------------

# One row per informant: how much does each person know?
informant_summary <- function(df) {
  df |>
    dplyr::group_by(informant_id) |>
    dplyr::summarise(
      age              = first_non_na_num(age),
      experience_years = first_non_na_num(experience_years),
      species_known    = dplyr::n_distinct(scientific_name),
      use_reports      = dplyr::n(),
      categories_known = dplyr::n_distinct(use_category),
      parts_known      = dplyr::n_distinct(plant_part, na.rm = TRUE),
      .groups = "drop"
    )
}

# One row per species: how widely known and how versatile is it?
# (RFC and UV are left out on purpose: they are FC and UR divided by a constant,
#  so they correlate perfectly with them.)
species_summary <- function(df) {
  df |>
    dplyr::group_by(scientific_name) |>
    dplyr::summarise(
      informants     = dplyr::n_distinct(informant_id),
      use_reports    = dplyr::n(),
      use_categories = dplyr::n_distinct(use_category),
      plant_parts    = dplyr::n_distinct(plant_part, na.rm = TRUE),
      ailments       = dplyr::n_distinct(ailment, na.rm = TRUE),
      villages       = dplyr::n_distinct(village, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---- Correlation -------------------------------------------------------------

# Correlation matrix with p-values and sample sizes (pairwise complete cases)
# @param method "spearman" (rank based, robust, default), "pearson" or "kendall"
# @param adjust p-value adjustment for the many pairs ("holm", "BH", "none", ...)
# @return list(r, p, p_adj, n, method, adjust)
correlation_matrix <- function(df, vars, method = c("spearman", "pearson", "kendall"),
                               adjust = "holm") {
  method <- match.arg(method)
  vars <- vars[vars %in% names(df)]
  usable <- vapply(vars, function(v) {
    x <- df[[v]]
    sum(!is.na(x)) >= 5 && dplyr::n_distinct(x, na.rm = TRUE) >= 2
  }, logical(1))
  vars <- vars[usable]
  k <- length(vars)

  r <- matrix(NA_real_, k, k, dimnames = list(vars, vars))
  p <- r
  n <- r
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      x <- df[[vars[i]]]
      y <- df[[vars[j]]]
      keep <- !is.na(x) & !is.na(y)
      n[i, j] <- sum(keep)
      if (i == j) { r[i, j] <- 1; p[i, j] <- 0; next }
      if (n[i, j] >= 5 && dplyr::n_distinct(x[keep]) > 1 && dplyr::n_distinct(y[keep]) > 1) {
        ct <- suppressWarnings(stats::cor.test(x[keep], y[keep], method = method, exact = FALSE))
        r[i, j] <- unname(ct$estimate)
        p[i, j] <- ct$p.value
      }
    }
  }

  # Adjust over the unique pairs, then mirror so the matrix stays symmetric
  p_adj <- p
  if (k >= 2) {
    up <- upper.tri(p)
    p_adj[up] <- stats::p.adjust(p[up], method = adjust)
    p_adj[lower.tri(p_adj)] <- t(p_adj)[lower.tri(p_adj)]
  }
  list(r = r, p = p, p_adj = p_adj, n = n, method = method, adjust = adjust)
}

# Long table of the unique variable pairs
correlation_pairs <- function(cm) {
  vars <- rownames(cm$r)
  idx <- which(upper.tri(cm$r), arr.ind = TRUE)
  data.frame(
    variable_1 = vars[idx[, 1]],
    variable_2 = vars[idx[, 2]],
    r = cm$r[idx],
    p = cm$p[idx],
    p_adjusted = cm$p_adj[idx],
    n = cm$n[idx],
    stringsAsFactors = FALSE
  )
}

# Plain-language label for the strength of a correlation
describe_correlation <- function(r) {
  a <- abs(r)
  strength <- dplyr::case_when(
    is.na(r) ~ "undefined",
    a < 0.1 ~ "negligible",
    a < 0.3 ~ "weak",
    a < 0.5 ~ "moderate",
    a < 0.7 ~ "strong",
    TRUE ~ "very strong"
  )
  direction <- ifelse(is.na(r) | a < 0.1, "", ifelse(r > 0, " positive", " negative"))
  paste0(strength, direction)
}

# ---- Co-knowledge of plants --------------------------------------------------

# Which plants are known by the same people?
# phi coefficient (Pearson correlation of two 0/1 variables) between the
# presence/absence columns of the most cited species.
coknowledge <- function(df, top_n = 20) {
  m <- informant_species_matrix(df)
  top <- names(sort(colSums(m), decreasing = TRUE))
  top <- top[seq_len(min(top_n, length(top)))]
  m <- m[, top, drop = FALSE]
  varying <- apply(m, 2, function(x) length(unique(x)) > 1)
  m <- m[, varying, drop = FALSE]
  phi <- suppressWarnings(stats::cor(m))
  list(phi = phi, presence = m, shared = crossprod(m))
}

coknowledge_pairs <- function(ck) {
  idx <- which(upper.tri(ck$phi), arr.ind = TRUE)
  data.frame(
    species_1 = rownames(ck$phi)[idx[, 1]],
    species_2 = colnames(ck$phi)[idx[, 2]],
    phi = ck$phi[idx],
    shared_informants = ck$shared[idx],
    stringsAsFactors = FALSE
  )
}

# ---- Data preparation for special charts -------------------------------------

# Keep the n most frequent values, lump the rest into "Other"
lump_top <- function(x, n = 8) {
  x <- as.character(x)
  tb <- sort(table(x), decreasing = TRUE)
  keep <- names(tb)[seq_len(min(n, length(tb)))]
  ifelse(x %in% keep, x, "Other")
}

# Waffle chart: share 100 squares between categories (largest remainder method)
# so the squares always add up to exactly `total`.
waffle_allocate <- function(counts, total = 100) {
  p <- counts / sum(counts) * total
  base <- floor(p)
  rest <- total - sum(base)
  if (rest > 0) {
    idx <- order(p - base, decreasing = TRUE)[seq_len(rest)]
    base[idx] <- base[idx] + 1
  }
  base
}

# Grid of squares (10 x 10) for a waffle chart
waffle_grid <- function(values, n_cols = 10, total = 100, top = 8) {
  values <- values[!is.na(values) & values != ""]
  values <- lump_top(values, top)
  tb <- sort(table(values), decreasing = TRUE)
  tb <- tb[names(tb) != "Other"] |> c(tb[names(tb) == "Other"])   # "Other" last
  squares <- waffle_allocate(as.numeric(tb), total)
  pct <- round(as.numeric(tb) / sum(tb) * 100)
  labels <- paste0(names(tb), " (", pct, "%)")
  cat_vec <- rep(labels, times = squares)
  i <- seq_len(total) - 1
  data.frame(
    x = i %% n_cols,
    y = i %/% n_cols,
    category = factor(cat_vec, levels = labels)
  )
}

# Links and nodes for a Sankey diagram across 2 or 3 columns
# @param columns character vector of column names (levels, left to right)
sankey_data <- function(df, columns, top = 8) {
  d <- df[, columns, drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d[] <- lapply(d, lump_top, n = top)

  node_labels <- lapply(d, function(x) sort(unique(x)))
  nodes <- data.frame(
    level = rep(seq_along(columns), lengths(node_labels)),
    label = unlist(node_labels, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  nodes$id <- seq_len(nrow(nodes)) - 1L          # plotly counts from 0
  key <- paste(nodes$level, nodes$label)

  links <- lapply(seq_len(length(columns) - 1), function(k) {
    agg <- dplyr::count(d, from = .data[[columns[k]]], to = .data[[columns[k + 1]]], name = "value")
    data.frame(
      source = nodes$id[match(paste(k, agg$from), key)],
      target = nodes$id[match(paste(k + 1, agg$to), key)],
      value = agg$value
    )
  })
  list(nodes = nodes, links = do.call(rbind, links))
}

# Rows for a treemap / sunburst: every level of the path becomes nodes
# whose value is the number of use reports below them.
hierarchy_data <- function(df, path) {
  d <- df[, path, drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d[] <- lapply(d, as.character)
  rows <- lapply(seq_along(path), function(k) {
    cols <- path[seq_len(k)]
    agg <- dplyr::count(d, dplyr::across(dplyr::all_of(cols)), name = "value")
    ids <- do.call(paste, c(as.list(agg[cols]), sep = " / "))
    parents <- if (k == 1) rep("", nrow(agg)) else
      do.call(paste, c(as.list(agg[path[seq_len(k - 1)]]), sep = " / "))
    data.frame(ids = ids, labels = agg[[path[k]]], parents = parents,
               values = agg$value, level = k, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
