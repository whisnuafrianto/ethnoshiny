# Botanical italics


test_that("only genus and epithet are italic; authority and ranks stay upright", {
  h <- function(x) italicize_name(x, "html")
  expect_equal(h("Curcuma longa"), "<i>Curcuma longa</i>")
  expect_equal(h("Curcuma longa L."), "<i>Curcuma longa</i> L.")
  expect_equal(h("Curcuma longa (L.) Roxb."), "<i>Curcuma longa</i> (L.) Roxb.")
  expect_equal(h("Piper sp."), "<i>Piper</i> sp.")
  expect_equal(h("Piper"), "<i>Piper</i>")
  expect_equal(h("Musa x paradisiaca"), "<i>Musa</i> x <i>paradisiaca</i>")
  expect_equal(h("Oryza sativa subsp. japonica"), "<i>Oryza sativa</i> subsp. <i>japonica</i>")
  expect_equal(h("Curcuma longa L. var. alba"), "<i>Curcuma longa</i> L. var. <i>alba</i>")
})

test_that("other formats and edge cases", {
  expect_equal(italicize_name("Curcuma longa L.", "markdown"), "*Curcuma longa* L.")
  expect_equal(italicize_name("Curcuma longa", "vis"), "_Curcuma longa_")
  expect_equal(italicize_name(c("Aus a", NA, "")), c("<i>Aus a</i>", NA, ""))
  expect_equal(italicize_name("Aus a Smith & Jones"), "<i>Aus a</i> Smith &amp; Jones")
  expect_length(italicize_name(character()), 0)
})

test_that("axis labels are plotmath expressions that evaluate", {
  lab <- species_axis_labels(c("Curcuma longa L.", "Piper sp.", NA))
  expect_length(lab, 3)
  expect_true(is.call(lab[[1]]))
  expect_equal(deparse(lab[[1]]), "italic(\"Curcuma longa\") ~ \"L.\"")
  expect_equal(lab[[3]], "")
})

test_that("selectize render options mention italics and the plain exceptions", {
  o <- selectize_italic("All species")
  expect_s3_class(o$render, "AsIs")
  expect_match(o$render, "<em>")
  expect_match(o$render, "All species")
})

test_that("bubble matrix draws italic species names as axis labels", {
  skip_if_not_installed("ggplot2")
  r <- matrix(c(1, .2, -.1, .2, 1, .3, -.1, .3, 1), 3,
              dimnames = list(c("Aus a", "Bus b", "Cus c"), c("Aus a", "Bus b", "Cus c")))
  r2 <- rbind(r, "Dus d" = c(.1, .1, .1)); r2 <- cbind(r2, "Dus d" = c(.1, .1, .1, 1))
  big <- r2[rep(1:4, 3), rep(1:4, 3)]; dimnames(big) <- list(paste0("Gen", 1:12, " sp", letters[1:12]),
                                                              paste0("Gen", 1:12, " sp", letters[1:12]))
  p <- plot_bubble_matrix(big, NULL, italic = TRUE, order = FALSE)       # > 10 variables: names on the axes
  lab <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
  expect_true(is.expression(lab))
  expect_length(lab, 12)
})
