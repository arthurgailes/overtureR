test_that("Establishing a view is low-memory", {
  testthat::skip_if_offline()
  testthat::skip_if_not_installed("bench")

  bbox <- sf::st_bbox(c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0))
  timer <- bench::mark(iterations = 2, filter_gc = FALSE, time_unit = "s", {
    open_curtain("building", bbox)
  })

  expect_lt(timer$mem_alloc, 1e6)

  # can speed be improved here?
  expect_lt(timer$median, 20)
})
