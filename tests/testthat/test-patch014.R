test_that("fmt_p never shows a tiny p-value as 0", {
  expect_equal(fmt_p(c(0, 1e-10, 0.0004, 0.0234, 0.5, NA)),
               c("<0.001", "<0.001", "<0.001", "0.023", "0.500", NA))
})

test_that("report tables drop empty columns and format p-values", {
  df <- data.frame(a = 1:3, empty = NA_character_, p = c(0, 0.02, 0.5), p_adjusted = c(1e-9, 0.04, 0.5))
  md <- report_table_md(df)
  expect_false(grepl("empty", md))
  expect_true(grepl("<0.001", md))
  expect_false(grepl("\\| 0 ", md))
})

test_that("long report tables are cut and say so", {
  df <- data.frame(id = seq_len(500), x = rnorm(500))
  md <- report_table_md(df, max_rows = 50)
  expect_true(grepl("first 50 of 500 rows", md))
  expect_equal(length(gregexpr("\\n\\|", md)[[1]]), 51L + 0L)
})

test_that("a stress close to zero is flagged, not called excellent", {
  expect_match(nmds_stress_label(7e-05, 102, 5), "near zero")
  expect_match(nmds_stress_label(7e-05, 102, 5), "5 distinct positions")
  expect_false(grepl("excellent\\)", nmds_stress_label(7e-05)))
  expect_match(nmds_stress_label(0.05), "excellent")
  expect_match(nmds_stress_label(0.15), "acceptable")
  expect_match(nmds_stress_label(0.3), "poor")
})
