# =============================================================================
# utils_plots.R
# Colour system (viridis) and ggplot2 helpers shared by several modules.
#
# Viridis is perceptually uniform and colour-blind friendly. Dark purple =
# low, yellow = high. Every chart in the app uses it so the whole app looks
# like one family.
# =============================================================================

# Key viridis colours (option "D")
VIRIDIS_PURPLE <- "#440154"
VIRIDIS_BLUE   <- "#31688E"
VIRIDIS_TEAL   <- "#21908C"
VIRIDIS_GREEN  <- "#5DC863"
VIRIDIS_YELLOW <- "#FDE725"

ETHNO_MAIN <- VIRIDIS_TEAL      # default colour for single-colour charts

# n viridis colours (hex strings), slightly trimmed so the ends stay readable
viridis_cols <- function(n, begin = 0.05, end = 0.92) {
  viridisLite::viridis(n, begin = begin, end = end, option = "D")
}

# Named viridis palette for categories. With more than 2 categories the colours
# are interleaved so that neighbouring categories look clearly different.
categorical_palette <- function(lv) {
  n <- length(lv)
  if (n == 0) return(character())
  cols <- viridis_cols(n)
  if (n > 2) {
    perm <- c(seq(1, n, by = 2), seq(2, n, by = 2))
    cols <- cols[order(perm)]
  }
  stats::setNames(cols, lv)
}

# "#RRGGBB" -> "rgba(r,g,b,a)" (plotly does not accept 8-digit hex everywhere)
to_rgba <- function(hex, alpha = 0.5) {
  m <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", m[1, ], m[2, ], m[3, ], alpha)
}

# ---- ggplot2 scales and theme -------------------------------------------------

scale_fill_ethno_c   <- function(...) ggplot2::scale_fill_viridis_c(option = "D", end = 0.95, ...)
scale_colour_ethno_c <- function(...) ggplot2::scale_colour_viridis_c(option = "D", end = 0.95, ...)
scale_fill_ethno_d   <- function(...) ggplot2::scale_fill_viridis_d(option = "D", begin = 0.05, end = 0.92, ...)
scale_colour_ethno_d <- function(...) ggplot2::scale_colour_viridis_d(option = "D", begin = 0.05, end = 0.92, ...)

theme_ethno <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey90"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.title.position = "plot",
      legend.position = "right"
    )
}

# ---- Basic charts --------------------------------------------------------------

# A blank plot that only shows a message (used when a column is empty)
plot_message <- function(text) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = text, size = 5, colour = "grey40") +
    ggplot2::theme_void()
}

# Horizontal bar chart of the counts of a categorical column
# Bars are coloured by their own length (viridis), so the eye finds the leaders.
# @param unit_col if given, count distinct values of this column instead of rows
plot_counts <- function(df, column, title, top = 15, unit_col = NULL) {
  if (!column %in% names(df) || all(is.na(df[[column]]))) {
    return(plot_message(paste0("Column '", column, "' has no data")))
  }
  d <- df
  d$value <- d[[column]]
  d <- d[!is.na(d$value), , drop = FALSE]
  d <- if (is.null(unit_col)) {
    dplyr::count(d, value, name = "n")
  } else {
    d$unit <- d[[unit_col]]
    d |> dplyr::group_by(value) |> dplyr::summarise(n = dplyr::n_distinct(unit), .groups = "drop")
  }
  d <- dplyr::slice_max(d, n, n = top, with_ties = FALSE)

  ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(value, n), y = n, fill = n)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.75) +
    ggplot2::coord_flip() +
    scale_fill_ethno_c() +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    theme_ethno()
}

# Histogram of a numeric column
plot_histogram <- function(df, column, title, binwidth = 5) {
  if (!column %in% names(df) || all(is.na(df[[column]]))) {
    return(plot_message(paste0("Column '", column, "' has no data")))
  }
  ggplot2::ggplot(df[!is.na(df[[column]]), , drop = FALSE],
                  ggplot2::aes(x = .data[[column]])) +
    ggplot2::geom_histogram(binwidth = binwidth, fill = ETHNO_MAIN, colour = "white") +
    ggplot2::labs(x = NULL, y = "Informants", title = title) +
    theme_ethno()
}

# Lollipop chart: a thin stick and a coloured dot (colour = value)
plot_lollipop <- function(d, label_col, value_col, y_label = NULL) {
  d$label <- d[[label_col]]
  d$value <- d[[value_col]]
  ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(label, value), y = value)) +
    ggplot2::geom_segment(ggplot2::aes(xend = label, y = 0, yend = value),
                          colour = "grey75", linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(colour = value), size = 5) +
    ggplot2::coord_flip() +
    scale_colour_ethno_c() +
    ggplot2::labs(x = NULL, y = y_label, colour = y_label) +
    theme_ethno()
}

# Radial bar chart (plotly "barpolar"): a flower of bars, colour = value
plot_radial <- function(d, label_col, value_col, y_label = NULL) {
  d <- d[order(d[[value_col]], decreasing = TRUE), , drop = FALSE]
  plotly::plot_ly(
    type = "barpolar",
    r = d[[value_col]], theta = d[[label_col]],
    marker = list(color = d[[value_col]], colorscale = "Viridis", showscale = TRUE,
                  colorbar = list(title = y_label),
                  line = list(color = "white", width = 1)),
    hovertemplate = paste0("%{theta}<br>", y_label, ": %{r}<extra></extra>")
  ) |>
    plotly::layout(
      polar = list(
        radialaxis  = list(visible = TRUE, showline = FALSE),
        angularaxis = list(categoryorder = "array", categoryarray = d[[label_col]],
                           direction = "clockwise", tickfont = list(size = 10))
      ),
      showlegend = FALSE
    )
}

# Choices for "group by" selectors: only columns with at least 2 distinct values
group_choices <- function(df) {
  cols <- c("Village" = "village", "Ethnic group" = "ethnic_group",
            "Gender" = "gender", "Occupation" = "occupation")
  keep <- vapply(cols, function(cn) dplyr::n_distinct(df[[cn]], na.rm = TRUE) >= 2, logical(1))
  cols[keep]
}

# ---- Correlation bubble matrix -----------------------------------------------

# Mixed correlation matrix.
#  * few variables (<= 10): bubbles below the diagonal (size = |r|, colour = r),
#    numbers with significance stars above, variable names on the diagonal.
#  * many variables: bubbles in the whole matrix and names on the axes.
# Variables are ordered by hierarchical clustering so related ones sit together.
# @param r correlation matrix; @param p_adj matrix of adjusted p-values (or NULL)
plot_bubble_matrix <- function(r, p_adj = NULL, title = NULL, fill_label = "r", order = TRUE,
                               italic = FALSE) {
  vars <- if (order && nrow(r) >= 3) order_by_cluster(r) else rownames(r)
  k <- length(vars)
  r <- r[vars, vars, drop = FALSE]
  if (is.null(p_adj)) p_adj <- matrix(NA_real_, k, k, dimnames = list(vars, vars))
  p_adj <- p_adj[vars, vars, drop = FALSE]

  long <- as.data.frame(as.table(r), stringsAsFactors = FALSE)
  names(long) <- c("row", "col", "r")
  long$p  <- as.vector(p_adj)
  long$xi <- match(long$col, vars)
  long$yi <- match(long$row, vars)
  long$label <- ifelse(is.na(long$r), "", paste0(sprintf("%.2f", long$r), sig_stars(long$p)))
  mixed <- k <= 10

  wrap <- function(v) vapply(v, function(s) paste(strwrap(gsub("_", " ", s), 12), collapse = "\n"), "")

  lower <- if (mixed) long[long$yi > long$xi, ] else long[long$yi != long$xi, ]
  lower <- lower[!is.na(lower$r), ]

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = lower,
                        ggplot2::aes(x = xi, y = yi, size = abs(r), fill = r),
                        shape = 21, colour = "white", stroke = 0.6) +
    ggplot2::scale_fill_viridis_c(option = "D", limits = c(-1, 1), name = fill_label) +
    ggplot2::scale_size_continuous(range = c(1.5, if (mixed) 16 else 9), limits = c(0, 1), guide = "none") +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   plot.title.position = "plot")

  # scientific names: italic genus and epithet, upright authority
  axis_labels <- if (italic) as.expression(species_axis_labels(vars)) else vars

  if (mixed) {
    upper <- long[long$yi < long$xi & !is.na(long$r), ]
    diag_df <- data.frame(xi = seq_len(k), yi = seq_len(k), label = wrap(vars))
    p <- p +
      ggplot2::geom_text(data = upper, ggplot2::aes(x = xi, y = yi, label = label), size = 3.6) +
      ggplot2::geom_text(data = diag_df, ggplot2::aes(x = xi, y = yi, label = label),
                         fontface = if (italic) "bold.italic" else "bold", size = 3.4, lineheight = 0.9) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.7)) +
      ggplot2::scale_y_reverse(expand = ggplot2::expansion(add = 0.7))
  } else {
    p <- p +
      ggplot2::scale_x_continuous(breaks = seq_len(k), labels = axis_labels,
                                  expand = ggplot2::expansion(add = 0.7), position = "top") +
      ggplot2::scale_y_reverse(breaks = seq_len(k), labels = axis_labels,
                               expand = ggplot2::expansion(add = 0.7)) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5, size = 9),
        axis.text.y = ggplot2::element_text(size = 9))
  }
  p
}

# ---- Static versions of the interactive-only charts ------------------------------
# The Sankey, treemap/sunburst and radar charts of the "Visual lab" are drawn with
# plotly in the app. These ggplot2 versions are what the "Add to report" button
# stores, so the charts can be exported in the HTML/Word report.

# Sankey (alluvial) diagram.
# @param s result of sankey_data()
plot_sankey_static <- function(s, title = NULL) {
  nodes <- s$nodes
  links <- s$links
  links$value <- as.numeric(links$value)

  nodes$out  <- vapply(nodes$id, function(i) sum(links$value[links$source == i]), numeric(1))
  nodes$inn  <- vapply(nodes$id, function(i) sum(links$value[links$target == i]), numeric(1))
  nodes$size <- pmax(nodes$out, nodes$inn)
  nodes$col  <- viridis_cols(nrow(nodes))

  n_lv      <- max(nodes$level)
  per_level <- as.numeric(tapply(nodes$size, nodes$level, sum))
  n_nodes   <- as.numeric(table(nodes$level))
  gap       <- max(per_level) * 0.04
  heights   <- per_level + gap * (n_nodes - 1)
  H         <- max(heights)

  # stack the nodes of each level from the top, each level centred vertically
  nodes$ymax <- NA_real_
  for (lv in seq_len(n_lv)) {
    top <- H - (H - heights[lv]) / 2
    for (i in which(nodes$level == lv)) {
      nodes$ymax[i] <- top
      top <- top - nodes$size[i] - gap
    }
  }
  nodes$ymin <- nodes$ymax - nodes$size

  links$s   <- match(links$source, nodes$id)
  links$t   <- match(links$target, nodes$id)
  links$s_y <- nodes$ymax[links$s]
  links$t_y <- nodes$ymax[links$t]

  # where each ribbon leaves its source node and enters its target node
  out_top <- numeric(nrow(links)); cur <- nodes$ymax
  for (k in order(-links$s_y, -links$t_y)) {
    out_top[k] <- cur[links$s[k]]
    cur[links$s[k]] <- cur[links$s[k]] - links$value[k]
  }
  in_top <- numeric(nrow(links)); cur <- nodes$ymax
  for (k in order(-links$t_y, -links$s_y)) {
    in_top[k] <- cur[links$t[k]]
    cur[links$t[k]] <- cur[links$t[k]] - links$value[k]
  }

  w  <- 0.05                                   # half width of a node bar
  tt <- seq(0, 1, length.out = 40)
  sm <- tt * tt * (3 - 2 * tt)                 # smooth S-curve between the two ends
  polys <- do.call(rbind, lapply(seq_len(nrow(links)), function(k) {
    x   <- (nodes$level[links$s[k]] + w) + ((nodes$level[links$t[k]] - w) - (nodes$level[links$s[k]] + w)) * tt
    top <- out_top[k] + (in_top[k] - out_top[k]) * sm
    b0  <- out_top[k] - links$value[k]
    b1  <- in_top[k] - links$value[k]
    bot <- b0 + (b1 - b0) * sm
    data.frame(x = c(x, rev(x)), y = c(top, rev(bot)), link = k,
               col = nodes$col[links$s[k]], stringsAsFactors = FALSE)
  }))

  nodes$x0 <- nodes$level - w
  nodes$x1 <- nodes$level + w
  nodes$ym <- (nodes$ymin + nodes$ymax) / 2
  first <- nodes[nodes$level == 1, , drop = FALSE]
  last  <- nodes[nodes$level == n_lv, , drop = FALSE]
  mid   <- nodes[nodes$level > 1 & nodes$level < n_lv, , drop = FALSE]

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = polys, ggplot2::aes(x = x, y = y, group = link, fill = col),
                          alpha = 0.45) +
    ggplot2::geom_rect(data = nodes, ggplot2::aes(xmin = x0, xmax = x1, ymin = ymin, ymax = ymax,
                                                  fill = col)) +
    ggplot2::geom_text(data = first, ggplot2::aes(x = x0 - 0.03, y = ym, label = label),
                       hjust = 1, size = 3.4) +
    ggplot2::geom_text(data = last, ggplot2::aes(x = x1 + 0.03, y = ym, label = label),
                       hjust = 0, size = 3.4)
  if (nrow(mid)) {
    p <- p + ggplot2::geom_text(data = mid, ggplot2::aes(x = level, y = ym, label = label),
                                size = 3.2, fontface = "bold")
  }
  p +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = c(0.9, 0.9))) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   plot.title.position = "plot",
                   plot.margin = ggplot2::margin(8, 8, 8, 8))
}

# Splits the range lo..hi into pieces proportional to v
split_range <- function(lo, hi, v) {
  cs <- cumsum(v)
  cbind(lo = lo + (hi - lo) * (cs - v) / sum(v),
        hi = lo + (hi - lo) * cs / sum(v))
}

# Position of every node of hierarchy_data() in the unit square (x0, x1, y0, y1).
# Treemap: children are cut along x, then y, then x ... (slice and dice).
# Sunburst: x is the angle (0..1); the ring is given by the level.
hierarchy_layout <- function(h, style = c("treemap", "sunburst")) {
  style <- match.arg(style)
  h <- h[order(h$level, h$ids), , drop = FALSE]
  h$x0 <- NA_real_; h$x1 <- NA_real_; h$y0 <- NA_real_; h$y1 <- NA_real_
  top <- which(h$level == 1)
  r <- split_range(0, 1, h$values[top])
  h$x0[top] <- r[, 1]; h$x1[top] <- r[, 2]; h$y0[top] <- 0; h$y1[top] <- 1
  if (max(h$level) >= 2) {
    for (k in 2:max(h$level)) {
      for (pid in unique(h$parents[h$level == k])) {
        ch  <- which(h$level == k & h$parents == pid)
        par <- which(h$level == k - 1 & h$ids == pid)[1]
        if (style == "sunburst" || k %% 2 == 1) {
          r <- split_range(h$x0[par], h$x1[par], h$values[ch])
          h$x0[ch] <- r[, 1]; h$x1[ch] <- r[, 2]
          h$y0[ch] <- h$y0[par]; h$y1[ch] <- h$y1[par]
        } else {
          r <- split_range(h$y0[par], h$y1[par], h$values[ch])
          h$y0[ch] <- r[, 1]; h$y1[ch] <- r[, 2]
          h$x0[ch] <- h$x0[par]; h$x1[ch] <- h$x1[par]
        }
      }
    }
  }
  # the first-level block each node belongs to (used for the colours)
  h$top <- ifelse(h$level == 1, h$labels, NA_character_)
  if (max(h$level) >= 2) {
    key <- paste(h$level, h$ids)
    for (k in 2:max(h$level)) {
      idx <- which(h$level == k)
      h$top[idx] <- h$top[match(paste(k - 1, h$parents[idx]), key)]
    }
  }
  h
}

# Static treemap or sunburst
# @param h result of hierarchy_data(); path the column names used to make it
plot_hierarchy_static <- function(h, path, style = c("treemap", "sunburst"), title = NULL) {
  style <- match.arg(style)
  depth <- length(path)
  lay <- hierarchy_layout(h, style)
  species_leaf <- identical(utils::tail(path, 1), "scientific_name")
  lay$face <- ifelse(lay$level == depth & species_leaf, "italic", "plain")
  pal <- categorical_palette(sort(unique(lay$top)))
  lay$fillc <- unname(pal[lay$top])
  rgb <- grDevices::col2rgb(lay$fillc)
  lay$txt <- ifelse((0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]) / 255 > 0.6, "black", "white")

  if (style == "treemap") {
    leaf <- lay[lay$level == depth, , drop = FALSE]
    leaf$share <- (leaf$x1 - leaf$x0) * (leaf$y1 - leaf$y0)
    blocks <- lay[lay$level == 1, , drop = FALSE]
    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = leaf, ggplot2::aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1,
                                                   fill = top),
                         colour = "white", linewidth = 0.3) +
      ggplot2::geom_rect(data = blocks, ggplot2::aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
                         fill = NA, colour = "white", linewidth = 1.4) +
      ggplot2::geom_text(data = leaf[leaf$share >= 0.01, , drop = FALSE],
                         ggplot2::aes(x = (x0 + x1) / 2, y = (y0 + y1) / 2, label = labels,
                                      colour = txt, fontface = face),
                         size = 2.8, check_overlap = TRUE) +
      ggplot2::scale_colour_identity() +
      ggplot2::scale_fill_manual(values = pal, name = path[1]) +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)
  } else {
    lay$span <- lay$x1 - lay$x0
    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = lay, ggplot2::aes(xmin = x0, xmax = x1, ymin = level, ymax = level + 1,
                                                  fill = top, alpha = 1 - 0.12 * (level - 1)),
                         colour = "white", linewidth = 0.3) +
      ggplot2::geom_text(data = lay[lay$span >= 0.03, , drop = FALSE],
                         ggplot2::aes(x = (x0 + x1) / 2, y = level + 0.5, label = labels,
                                      fontface = face),
                         size = 2.6, check_overlap = TRUE) +
      ggplot2::scale_alpha_identity() +
      ggplot2::scale_fill_manual(values = pal, name = path[1]) +
      ggplot2::scale_x_continuous(limits = c(0, 1)) +
      ggplot2::scale_y_continuous(limits = c(0, depth + 1)) +
      ggplot2::coord_polar(theta = "x")
  }
  p +
    ggplot2::labs(title = title, caption = "Area = number of use reports") +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   plot.title.position = "plot",
                   plot.margin = ggplot2::margin(8, 8, 8, 8))
}

# Static radar chart of the use-category "fingerprint" of species
# @param fl fidelity_level() table of the selected species; cats the use categories
plot_radar_static <- function(fl, cats, sel, measure = "FL", y_label = NULL) {
  n <- length(cats)
  cols <- stats::setNames(viridis_cols(length(sel)), sel)
  d <- do.call(rbind, lapply(sel, function(sp) {
    sub <- fl[fl$scientific_name == sp, , drop = FALSE]
    sub <- sub[match(cats, sub$use_category), , drop = FALSE]
    v <- sub[[measure]]
    data.frame(species = sp, x = c(seq_len(n), n + 1), y = c(v, v[1]), stringsAsFactors = FALSE)
  }))
  d$species <- factor(d$species, levels = sel)
  lab_fun <- function(x) html_labels_to_expr(italicize_name(x))
  ggplot2::ggplot(d, ggplot2::aes(x = x, y = y, group = species, colour = species, fill = species)) +
    ggplot2::geom_polygon(alpha = 0.25, linewidth = 0.9) +
    ggplot2::geom_point(size = 2) +
    ggplot2::coord_polar() +
    ggplot2::scale_x_continuous(breaks = seq_len(n), labels = cats, limits = c(1, n + 1)) +
    ggplot2::scale_colour_manual(values = cols, labels = lab_fun, name = NULL) +
    ggplot2::scale_fill_manual(values = cols, labels = lab_fun, name = NULL) +
    ggplot2::labs(x = NULL, y = y_label) +
    theme_ethno() +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.major.x = ggplot2::element_line(colour = "grey90"))
}
