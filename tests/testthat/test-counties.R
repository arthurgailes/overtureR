test_that("Counties download works as expected", {
  testthat::skip_if_offline()
  library(dplyr, warn.conflicts = FALSE)

  counties <- open_curtain("division_area", bbox = NULL) %>%
    # in R, filtering on variables must come before removing them via select
    filter(subtype == "county" & country == "US" & region == "US-PA") %>%
    transmute(
      id,
      division_id,
      # STRUCT/MAP columns can be accessed as column[["subcolumn"]] in transmute
      primary = names[["primary"]],
      geometry
    )

  counties_sf <- collect_sf(counties)

  expect_true("tbl_lazy" %in% class(counties))
  expect_true("sf" %in% class(counties_sf))
  expect_equal(pull(count(counties), n), 67)

  expect_true(all(grepl("County", counties_sf$primary)))
  expect_true(all(sf::st_is_valid(counties_sf$geometry)))
  expect_true(sf::st_crs(counties_sf$geometry) == sf::st_crs(4326))
})
