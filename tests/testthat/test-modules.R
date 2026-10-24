# Runs the server logic of every module against the example data
# (shiny::testServer renders each output; an error in any plot or table fails the test).
df  <- load_example_data()$data
dat <- shiny::reactive(df)
rendered <- function(x) expect_false(is.null(x))

test_that("profile", {
  shiny::testServer(mod_profile_server, args = list(data = dat), {
    for (o in c("gender", "age", "village", "ethnic", "occupation", "experience")) rendered(output[[o]])
  })
})

test_that("indices, all three chart styles", {
  shiny::testServer(mod_indices_server, args = list(data = dat), {
    for (st in c("bar", "lollipop", "radial")) {
      session$setInputs(metric = "UV", top_n = 10, min_iu = 3, icf_by = "use_category", style = st)
      for (o in c("tbl_species", "plot_species", "tbl_fl", "plot_fl", "tbl_icf", "plot_icf")) rendered(output[[o]])
    }
    session$setInputs(icf_by = "ailment")
    rendered(output$plot_icf)
  })
})

test_that("categories", {
  shiny::testServer(mod_categories_server, args = list(data = dat), {
    for (a in c("use_category", "plant_part", "preparation", "administration", "ailment", "habitus", "family")) {
      session$setInputs(attribute = a, measure = "species", top_n = 10, top_species = 15)
      rendered(output$bars)
    }
    rendered(output$heatmap)
  })
})

test_that("compare", {
  shiny::testServer(mod_compare_server, args = list(data = dat), {
    session$setInputs(by = "village")
    session$setInputs(groups = unique(df$village)[1:3], method = "jaccard")
    rendered(output$heatmap); rendered(output$sim_table); rendered(output$shared)
    session$setInputs(method = "sorensen")
    rendered(output$heatmap)
  })
})

test_that("correlation", {
  shiny::testServer(mod_correlation_server, args = list(data = dat), {
    for (m in c("spearman", "pearson", "kendall")) {
      session$setInputs(method = m, adjust = "holm",
                        inf_vars = unname(INFORMANT_VARS), inf_x = "age", inf_y = "species_known",
                        sp_vars = unname(SPECIES_VARS), sp_x = "informants", sp_y = "use_categories",
                        co_top = 12, co_dir = "high")
      for (o in c("inf_matrix", "inf_summary", "inf_scatter", "inf_table",
                  "sp_matrix", "sp_summary", "sp_scatter", "sp_table", "co_matrix", "co_table")) rendered(output[[o]])
    }
    session$setInputs(co_top = 6, co_dir = "low")           # small matrix = mixed layout
    rendered(output$co_matrix)
  })
})

test_that("visual lab", {
  shiny::testServer(mod_visuals_server, args = list(data = dat), {
    session$setInputs(s1 = "village", s2 = "use_category", s3 = "plant_part",
                      path = names(HIERARCHY_PATHS)[1], hstyle = "treemap",
                      waffle_by = "use_category", radar_sp = utils::head(unique(df$scientific_name), 3),
                      radar_measure = "FL", rain_var = "species_known", rain_by = "village")
    for (o in c("sankey", "hierarchy", "waffle", "radar", "raincloud")) rendered(output[[o]])
    for (p in names(HIERARCHY_PATHS)) { session$setInputs(path = p, hstyle = "sunburst"); rendered(output$hierarchy) }
    session$setInputs(s3 = "none"); rendered(output$sankey)
  })
})

test_that("multivariate", {
  shiny::testServer(mod_multivar_server, args = list(data = dat), {
    session$setInputs(by = "village", perm = 99)
    rendered(output$nmds_plot); rendered(output$stress); rendered(output$permanova); rendered(output$dendro)
    session$setInputs(by = "ethnic_group")
    rendered(output$nmds_plot)
  })
})

test_that("network", {
  shiny::testServer(mod_network_server, args = list(data = dat), {
    for (r in c("use_category", "ailment", "plant_part")) {
      session$setInputs(relation = r, top_n = 15)
      rendered(output$hubs)
      if (!has_visnetwork()) rendered(output$static) else rendered(output$vis)
    }
  })
})

test_that("taxonomy (offline) and map", {
  shiny::testServer(mod_taxonomy_server, args = list(data = dat), { rendered(output$offline) })
  shiny::testServer(mod_map_server, args = list(data = dat), {
    session$setInputs(species = "All species", blur = TRUE)
    if (has_leaflet()) rendered(output$map) else rendered(output$map_static)
    session$setInputs(species = df$scientific_name[1], blur = FALSE)
    if (!has_leaflet()) rendered(output$map_static)
  })
})

test_that("bad input is rejected, semicolon CSV is cleaned", {
  expect_false(validate_data(data.frame(a = 1, b = 2))$ok)
  semi <- tempfile(fileext = ".csv")
  writeLines(c("Informant ID;Scientific Name;Use Category",
               "I1;curcuma  longa;Food", "I1;curcuma  longa;Food", "I2;;Food"), semi)
  r <- validate_data(read_input_file(semi))
  expect_true(r$ok); expect_equal(nrow(r$data), 1); expect_equal(r$data$scientific_name, "Curcuma longa")
})

test_that("ethnoshiny_app builds, accepts own data, rejects bad data", {
  expect_s3_class(ethnoshiny_app(), "shiny.appobj")
  expect_s3_class(ethnoshiny_app(ethnoshiny_example()), "shiny.appobj")
  expect_error(ethnoshiny_app(data.frame(a = 1)), "cannot be used")
  expect_true(nrow(ethnoshiny_example()) > 800)
  tf <- tempfile(fileext = ".csv"); ethnoshiny_template(tf)
  expect_true(file.exists(tf))
})

test_that("data module starts from supplied data", {
  init <- validate_data(ethnoshiny_example()[1:60, ])
  shiny::testServer(mod_data_server, args = list(initial = init), {
    expect_equal(nrow(session$returned$data()), nrow(init$data))
  })
})
