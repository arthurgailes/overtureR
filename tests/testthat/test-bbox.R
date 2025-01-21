test_that("bbox filter is reasonably fast", {
  testthat::skip_if_offline()
  skip_on_cran()

  bbox <- sf::st_bbox(c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0))
  timer <- bench::mark(iterations = 1, filter_gc = FALSE, {
    open_curtain("building", bbox)
  })

  expect_lt(timer$median, 30)
  expect_lt(timer$median, 1e6)
})
