test_that("record examples work", {
  broadway <- c(xmin = -73.99011, ymin = 40.755488, xmax = -73.98, ymax = 40.76206)
  buildings <- open_curtain("building", spatial_filter = broadway)
  local_buildings <- record_overture(tempdir(), buildings, overwrite = TRUE)

  expect_gt(pull(count(local_buildings), n), 0)
})
