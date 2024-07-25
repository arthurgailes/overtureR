test_that("Establishing a view is low-memory", {
  testthat::skip_if_offline()

  bbox <- sf::st_bbox(c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0))
  timer <- bench::mark(iterations = 1, filter_gc = FALSE, {
    open_curtain("building", bbox)
  })

  expect_lt(timer$mem_alloc, 1e6)
})
