df  <- load_example_data()$data
dat <- shiny::reactive(df)

test_that("add_to_report stores plots, tables and text, and replaces by id", {
  rep <- new_report_store()
  shiny::isolate({
    p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + ggplot2::geom_point()
    expect_true(add_to_report(rep, "p1", "Indices", "A plot", plot = p, notify = FALSE))
    expect_true(file.exists(rep$items$p1$path))
    add_to_report(rep, "t1", "Data", "A table", table = data.frame(a = 1:2), notify = FALSE)
    add_to_report(rep, "x1", "Multivariate", "Text", text = c("a", "b"), notify = FALSE)
    add_to_report(rep, "p1", "Indices", "A plot again", plot = p, notify = FALSE)
    expect_length(rep$items, 3)
    expect_equal(rep$items$p1$title, "A plot again")
    ord <- names(order_report_items(rep$items))
    expect_equal(ord[1], "t1")                   # Data comes first
    expect_false(add_to_report(rep, "bad", "Indices", "Bad", plot = function() stop("x"), notify = FALSE))
  })
})

test_that("italic species labels survive in static plots", {
  d <- data.frame(l = italicize_name(c("Zingiber officinale Roscoe", "Curcuma longa L.")), v = 1:2)
  p <- italicize_static_plot(plot_lollipop(d, "l", "v", "UV"))
  expect_true(inherits(p$scales$get_scales("x"), "ScaleDiscretePosition"))
  expect_silent(ggplot2::ggsave(tempfile(fileext = ".png"), p, width = 4, height = 3))
})

test_that("report markdown has figure, table and text parts", {
  rep <- new_report_store()
  shiny::isolate({
    add_to_report(rep, "t1", "Indices", "Species table",
                  table = data.frame(scientific_name = "Zingiber officinale", UV = 1.2345), notify = FALSE)
    add_to_report(rep, "x1", "Multivariate", "PERMANOVA", text = "R2 = 0.1", notify = FALSE)
    md <- paste(report_markdown(rep$items), collapse = "\n")
  })
  expect_match(md, "# Indices"); expect_match(md, "\\*Zingiber officinale\\*"); expect_match(md, "R2 = 0.1")
  expect_match(paste(report_markdown(list()), collapse = ""), "No results")
})

test_that("every Add-to-report button fills the basket", {
  skip_if_not_installed("vegan")
  rep <- new_report_store()
  shiny::testServer(mod_categories_server, args = list(data = dat, report = rep), {
    session$setInputs(attribute = "use_category", measure = "species", top_n = 10, top_species = 15)
    session$setInputs(add_bars = 1, add_heatmap = 1)
  })
  shiny::testServer(mod_indices_server, args = list(data = dat, report = rep), {
    session$setInputs(metric = "UV", top_n = 10, min_iu = 3, icf_by = "use_category", style = "radial")
    session$setInputs(add_species_tbl = 1, add_species_plot = 1, add_fl_plot = 1, add_fl_tbl = 1,
                      add_icf_plot = 1, add_icf_tbl = 1)
  })
  shiny::testServer(mod_visuals_server, args = list(data = dat, report = rep), {
    session$setInputs(waffle_by = "use_category", rain_var = "species_known", rain_by = "village")
    session$setInputs(add_waffle = 1, add_rain = 1)
  })
  shiny::testServer(mod_compare_server, args = list(data = dat, report = rep), {
    session$setInputs(by = "village")
    session$setInputs(groups = unique(df$village)[1:3], method = "jaccard")
    session$setInputs(add_sim = 1, add_sim_tbl = 1, add_shared = 1)
  })
  shiny::testServer(mod_correlation_server, args = list(data = dat, report = rep), {
    session$setInputs(method = "spearman", adjust = "holm",
                      inf_vars = unname(INFORMANT_VARS), inf_x = "age", inf_y = "species_known",
                      sp_vars = unname(SPECIES_VARS), sp_x = "informants", sp_y = "use_categories",
                      co_top = 12, co_dir = "high")
    session$setInputs(inf_add_matrix = 1, inf_add_pair = 1, inf_add_table = 1,
                      sp_add_matrix = 1, sp_add_pair = 1, sp_add_table = 1, add_co = 1)
  })
  shiny::testServer(mod_multivar_server, args = list(data = dat, report = rep), {
    session$setInputs(by = "village", perm = 99)
    session$setInputs(add_nmds = 1, add_dendro = 1)
  })
  shiny::testServer(mod_network_server, args = list(data = dat, report = rep), {
    session$setInputs(relation = "use_category", top_n = 12)
    session$setInputs(add_net = 1, add_hubs = 1)
  })
  shiny::testServer(mod_data_server, args = list(report = rep), {
    session$setInputs(add_overview = 1)
  })
  ids <- shiny::isolate(names(rep$items))
  expect_true(all(c("cat_bars", "cat_heatmap", "idx_species_tbl", "idx_species_plot", "idx_fl_plot",
                    "idx_fl_tbl", "idx_icf_plot", "idx_icf_tbl", "vis_waffle", "vis_raincloud",
                    "cmp_sim", "cmp_sim_tbl", "cmp_shared", "cor_inf_matrix", "cor_inf_pair",
                    "cor_inf_table", "cor_sp_matrix", "cor_sp_pair", "cor_sp_table",
                    "cor_coknowledge", "mv_nmds", "mv_permanova", "mv_dendro", "net_plot",
                    "net_hubs", "data_overview") %in% ids))

  skip_if_not(rmarkdown::pandoc_available())
  for (fmt in c("html_document", "word_document")) {
    out <- tempfile(fileext = if (fmt == "html_document") ".html" else ".docx")
    shiny::isolate(build_report(rep$items, "Test report", "A note", fmt, out))
    expect_true(file.exists(out)); expect_gt(file.size(out), 5000)
  }
})
