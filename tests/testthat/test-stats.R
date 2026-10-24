# Checks for correlation helpers and the data preparation of the special charts.
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })



test_that("correlation_matrix recovers perfect and absent relationships", {
  d <- data.frame(a = 1:20, b = (1:20)^2, c = 20:1, noise = rep(c(1, 2, 3, 4), 5))
  cm <- correlation_matrix(d, c("a", "b", "c", "noise"), "spearman")
  expect_equal(cm$r["a", "b"], 1)        # monotone increasing
  expect_equal(cm$r["a", "c"], -1)       # monotone decreasing
  expect_equal(cm$r["a", "a"], 1)
  expect_true(isSymmetric(cm$r))
  expect_true(isSymmetric(cm$p_adj))
  expect_true(all(cm$p_adj[upper.tri(cm$p_adj)] >= cm$p[upper.tri(cm$p)]))  # Holm never lowers p
  pr <- correlation_pairs(cm)
  expect_equal(nrow(pr), choose(4, 2))
})

test_that("pearson differs from spearman on a curved relationship", {
  d <- data.frame(x = 1:30, y = exp(1:30 / 5))
  expect_equal(correlation_matrix(d, c("x", "y"), "spearman")$r["x", "y"], 1)
  expect_lt(correlation_matrix(d, c("x", "y"), "pearson")$r["x", "y"], 1)
})

test_that("constant or nearly empty variables are dropped, not crashed on", {
  d <- data.frame(a = 1:10, const = 5, mostly_na = c(1, 2, rep(NA, 8)))
  cm <- correlation_matrix(d, c("a", "const", "mostly_na"))
  expect_equal(rownames(cm$r), "a")
})

test_that("significance stars and strength labels", {
  expect_equal(sig_stars(c(0.0004, 0.005, 0.03, 0.2, NA)), c("***", "**", "*", "", ""))
  expect_equal(describe_correlation(c(0.05, 0.2, 0.4, 0.6, 0.9, -0.6)),
               c("negligible", "weak positive", "moderate positive",
                 "strong positive", "very strong positive", "strong negative"))
})

test_that("informant and species summaries count correctly", {
  toy <- validate_data(data.frame(
    informant_id    = c("I1", "I1", "I2", "I3", "I4"),
    scientific_name = c("Aus a", "Aus a", "Aus a", "Aus a", "Bus b"),
    use_category    = c("Food", "Medicinal", "Medicinal", "Medicinal", "Food"),
    age             = c(30, 30, 40, 50, 60)
  ))$data
  inf <- informant_summary(toy)
  expect_equal(inf$use_reports[inf$informant_id == "I1"], 2)
  expect_equal(inf$categories_known[inf$informant_id == "I1"], 2)
  expect_equal(inf$age[inf$informant_id == "I4"], 60)
  sp <- species_summary(toy)
  expect_equal(sp$informants[sp$scientific_name == "Aus a"], 3)
  expect_equal(sp$use_categories[sp$scientific_name == "Aus a"], 2)
})

test_that("co-knowledge phi is +1 for identical and -1 for opposite knowledge", {
  df <- data.frame(
    informant_id    = c("I1", "I1", "I2", "I2", "I3", "I3", "I4", "I4", "I5", "I6"),
    scientific_name = c("A a", "B b", "A a", "B b", "C c", "C c", "C c", "C c", "D d", "D d"),
    use_category    = "Food"
  )
  ck <- coknowledge(df, top_n = 10)
  expect_equal(unname(ck$phi["A a", "B b"]), 1)      # I1 and I2 know both
  expect_lt(ck$phi["A a", "C c"], 0)                 # never known together
  pr <- coknowledge_pairs(ck)
  expect_equal(pr$shared_informants[pr$species_1 == "A a" & pr$species_2 == "B b"] , 2)
})

test_that("waffle squares always add up to the total", {
  for (counts in list(c(10, 20, 30), c(1, 1, 1), c(333, 333, 334), c(5, 1, 1, 1, 1, 1))) {
    expect_equal(sum(waffle_allocate(counts)), 100)
  }
  g <- waffle_grid(rep(c("x", "y", "z", NA), c(50, 30, 20, 5)))
  expect_equal(nrow(g), 100)
  expect_equal(sort(unique(g$x)), 0:9)
})

test_that("sankey links carry every row exactly once per step", {
  df <- data.frame(a = rep(c("p", "q"), c(6, 4)),
                   b = c(rep("u", 3), rep("v", 3), rep("u", 4)),
                   c = rep(c("m", "n"), 5))
  s <- sankey_data(df, c("a", "b", "c"))
  expect_equal(sum(s$links$value), 2 * nrow(df))     # 2 steps x 10 rows
  expect_equal(nrow(s$nodes), 2 + 2 + 2)
  expect_true(all(s$links$source %in% s$nodes$id & s$links$target %in% s$nodes$id))
})

test_that("treemap parents hold the sum of their children", {
  df <- data.frame(f = c("F1", "F1", "F1", "F2"), s = c("a", "a", "b", "c"))
  h <- hierarchy_data(df, c("f", "s"))
  parent_val <- h$values[h$level == 1]; names(parent_val) <- h$ids[h$level == 1]
  child_sum <- tapply(h$values[h$level == 2], h$parents[h$level == 2], sum)
  expect_equal(as.numeric(parent_val[names(child_sum)]), as.numeric(child_sum))
  expect_equal(sum(h$values[h$level == 1]), nrow(df))   # top level covers every row
})

test_that("categorical palette works for any number of categories and keeps names", {
  for (k in 0:12) {
    lv <- if (k == 0) character() else paste0("c", seq_len(k))
    pal <- categorical_palette(lv)
    expect_equal(length(pal), k)
    if (k > 0) expect_equal(names(pal), lv)
    expect_equal(length(unique(pal)), k)
  }
})
