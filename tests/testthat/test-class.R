test_that("Counties download works as expected", {
  testthat::skip_if_offline()
  library(dplyr, warn.conflicts = FALSE)

  counties <- open_curtain("division_area", bbox = NULL) %>%
    # in R, filtering on variables must come before removing them via select
    filter(subtype == "county" & region == "US-PA") %>%
    transmute(
      id,
      division_id,
      # STRUCT/MAP columns can be accessed as column[["subcolumn"]] in transmute
      primary = names[["primary"]],
      geometry
    )

  counties_collected <- collect(counties)
  expect_true(class(counties)[[1]] == "overture_call")
  playbill <- attr(counties, "overture_playbill")

  expect_equal(playbill[["type"]], "division_area")
  expect_equal(playbill[["theme"]], "divisions")

  expect_true("tbl_lazy" %in% class(counties))

  counties_sf <- collect(counties)

  expect_false("overture_call" %in% class(counties_sf))
  expect_true("sf" %in% class(counties_sf))
  expect_equal(pull(count(counties), n), 67)

  expect_true(all(grepl("County", counties_sf$primary)))
  expect_true(all(sf::st_is_valid(counties_sf$geometry)))
  expect_true(sf::st_crs(counties_sf$geometry) == sf::st_crs(4326))
})

test_that("class assignment works", {
  conn <- DBI::dbConnect(duckdb::duckdb())
  division <- open_curtain("division", conn = conn, tablename = "test")

  # convert arbitrary sql into `overture_call`
  division2 <- tbl(conn, "test")
  division2 <- as_overture(division2, "division")

  expect_true(class(counties)[[1]] == "overture_call")
  expect_error(as_overture(mtcars), "Input must be a tbl_sql object")

  DBI::dbDisconnect(conn, shutdown = TRUE)
})
