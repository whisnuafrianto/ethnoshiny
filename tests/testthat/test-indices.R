# Hand-computed check of every index.
# Data: 4 informants, species A and B
#   I1: A (Food), A (Medicinal)   I2: A (Medicinal)
#   I3: A (Medicinal)             I4: B (Food)
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })


toy <- validate_data(data.frame(
  informant_id    = c("I1", "I1", "I2", "I3", "I4"),
  scientific_name = c("Aus a", "Aus a", "Aus a", "Aus a", "Bus b"),
  use_category    = c("Food", "Medicinal", "Medicinal", "Medicinal", "Food")
))$data

test_that("species indices match hand calculation", {
  a <- species_indices(toy) |> filter(scientific_name == "Aus a")
  expect_equal(a$FC, 3)
  expect_equal(a$UR, 4)
  expect_equal(a$NU, 2)
  expect_equal(a$RFC, 3 / 4)
  expect_equal(a$UV, 4 / 4)
  expect_equal(a$CI, 4 / 4)
})

test_that("fidelity level", {
  fl <- fidelity_level(toy) |> filter(scientific_name == "Aus a")
  expect_equal(fl$FL[fl$use_category == "Medicinal"], 100)
  expect_equal(fl$FL[fl$use_category == "Food"], 100 / 3)
})

test_that("informant consensus factor", {
  icf <- informant_consensus(toy, "use_category")
  expect_equal(icf$ICF[icf$category == "Medicinal"], 1)   # (3-1)/(3-1)
  expect_equal(icf$ICF[icf$category == "Food"], 0)        # (2-2)/(2-1)
})

test_that("similarity indices", {
  lst <- list(g1 = c("a", "b", "c"), g2 = c("b", "c", "d"))
  expect_equal(similarity_matrix(lst, "jaccard")["g1", "g2"], 2 / 4)
  expect_equal(similarity_matrix(lst, "sorensen")["g1", "g2"], 2 * 2 / 6)
})

test_that("validation catches missing columns and cleans names", {
  expect_false(validate_data(data.frame(x = 1))$ok)
  r <- validate_data(data.frame(`Informant ID` = "I1", `Scientific Name` = "  curcuma   longa ",
                                `Use Category` = "Food", check.names = FALSE))
  expect_true(r$ok)
  expect_equal(r$data$scientific_name, "Curcuma longa")
})
