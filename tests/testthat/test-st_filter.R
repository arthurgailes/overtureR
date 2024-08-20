test_that("focus_spotlight handles all options correctly", {
  skip_on_cran()
  con <- duckdb::dbConnect(duckdb::duckdb())
  # Mock functions for testing

  # regex for what the where clause should look like
  result_template <- "AND ST_Intersects\\(master\\.geometry, \\(SELECT ST_Union_Agg\\(geometry\\) AS geometry FROM .*\\)\\)$"

  tbl <- data.frame(geometry = c("POINT(0 0)", "POINT(1 1)"))
  duckdb::duckdb_register(con, "test", tbl, overwrite = TRUE)

  dbplyr_tbl <- dplyr::tbl(con, "test")
  # Test with NULL input
  expect_equal(focus_spotlight(conn, NULL), "")

  # Test with sf object
  sf_obj <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
  )

  result_sf <- focus_spotlight(con, sf_obj)
  expect_true(grepl(result_template, result_sf))

  # Test with dbplyr tbl
  result_dbplyr <- focus_spotlight(con, dbplyr_tbl)
  expect_true(grepl(result_template, result_dbplyr))

  # Test with character input (table name)
  result_char <- focus_spotlight(con, "test")
  expect_true(grepl(result_template, result_dbplyr))

  # Test error cases
  expect_error(focus_spotlight(con, list()), "invalid `spatial_filter` object")

  # Test with dbplyr tbl without geometry column
  invalid_tbl <- data.frame(x = 1, y = 2)
  duckdb::duckdb_register(con, "invalid", invalid_tbl)
  invalid_dbplyr <- dplyr::tbl(con, "invalid")

  expect_error(focus_spotlight(con, invalid_dbplyr), "`spatial_filter` must have a column 'geometry' of class GEOMETRY")

  # reject if not a string
  expect_error(focus_spotlight(con, "not a table!"), "if a string, `spatial_filter` must be a table in the connection")

  DBI::dbDisconnect(con)
})

test_that("spatial_filter works correctly in open_curtain", {
  skip_if_offline()
  skip_on_cran()
  con <- stage_conn()

  # get Mecklenburg county from sf object
  return_hood <- function(spatial_filter) {
    tbl <- open_curtain("division", spatial_filter)
    tbl <- dplyr::filter(tbl, subtype == "neighborhood")
  }

  meck <- subset(
    sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE),
    NAME == "Mecklenburg"
  )
  meck <- sf::st_transform(meck, 4326)

  meck_hood_sf <- collect(return_hood(meck))
  expect_true(all(unlist(sf::st_intersects(meck_hood_sf, meck$geometry))))

  meck_dbplyr <- sf_as_dbplyr(con, "meck", meck, overwrite = TRUE)

  meck_dbplyr_sf <- collect(return_hood(meck_dbplyr))
  expect_true(all(unlist(sf::st_intersects(meck_dbplyr_sf, meck$geometry))))
  expect_equal(meck_dbplyr_sf, meck_hood_sf)

  # test with tablename
  DBI::dbExecute(con, glue::glue(
    "CREATE OR REPLACE VIEW meck_test AS ({dbplyr::sql_render(meck_dbplyr)})"
  ))
  meck_tablename_sf <- collect(return_hood("meck_test"))
  expect_true(all(unlist(sf::st_intersects(meck_dbplyr_sf, meck$geometry))))
  expect_equal(meck_tablename_sf, meck_hood_sf)

  meck_bbox <- sf::st_bbox(meck)
  meck_hood_bbox <- collect(return_hood(meck_bbox))
  expect_gt(nrow(meck_hood_bbox), nrow(meck_hood_sf))
})
