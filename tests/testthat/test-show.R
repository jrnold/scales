context("show")

test_that("show_col works", {
  # I don't know how to test plot output without vdiffr, so just check that it
  # runs successfully.
  colours <- hue_pal()(8)
  # invisibly returns its inputs
  expect_equal(show_col(colours), colours)
  expect_equal(show_col(colours, labels = FALSE), colours)
  expect_equal(show_col(colours, borders = NA), colours)
})

test_that("show_shape works", {
  shapes <- shape_pal()(5)
  # invisibly returns its inputs
  expect_equal(show_shape(shapes), shapes)
  expect_equal(show_shape(shapes, labels = FALSE), shapes)
})

test_that("show_linetype works", {
  linetypes <- linetype_pal()(5)
  # invisibly returns its inputs
  expect_equal(show_shape(linetypes), linetypes)
  expect_equal(show_shape(linetypes, labels = FALSE), linetypes)
})
