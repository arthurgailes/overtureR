test_that("sql_handle_spatial_filter handles all options correctly", {
  
  con <- duckdb::dbConnect(duckdb::duckdb())
  # Mock functions for testing

  # regex for what the where clause should look like
  result_template <- "AND ST_Intersects\\(master\\.geometry, \\(SELECT ST_Union_Agg\\(geometry\\) AS geometry FROM .*\\)\\)$"
  
  tbl <- data.frame(geometry = c("POINT(0 0)", "POINT(1 1)"))
  duckdb::duckdb_register(con, "test", tbl, overwrite = TRUE)
  
  dbplyr_tbl <- dplyr::tbl(con, "test")
  # Test with NULL input
  expect_equal(sql_handle_spatial_filter(conn, NULL), "")
  
  # Test with sf object
  sf_obj <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
  )

  result_sf <- sql_handle_spatial_filter(con, sf_obj)
  expect_true(grepl(result_template, result_sf))
  expect_true(duckdb::dbExistsTable(con, "internal_spatial_filter_init"))
  
  # Test with dbplyr tbl
  result_dbplyr <- sql_handle_spatial_filter(con, dbplyr_tbl)
  expect_true(grepl(result_template, result_dbplyr))
  
  # Test with character input (table name)
  result_char <- sql_handle_spatial_filter(con, "test")
  expect_equal(result_char, result_dbplyr)
  
  # Test error cases
  expect_error(sql_handle_spatial_filter(con, list()), "invalid `spatial_filter` object")
  
  # Test with dbplyr tbl without geometry column
  invalid_tbl <- data.frame(x = 1, y = 2)
  duckdb::duckdb_register(con, "invalid", invalid_tbl)
  invalid_dbplyr <- dplyr::tbl(con, "invalid")

  colnames(invalid_dbplyr)
  duckdb::dbExistsTable(con, "invalid")

  expect_error(sql_handle_spatial_filter(con, invalid_dbplyr), "`spatial_filter` must have a column named 'geometry'")
  expect_error(sql_handle_spatial_filter(con, "invalid"), "`spatial_filter` must have a column named 'geometry'")

  # reject if not a string
  expect_error(sql_handle_spatial_filter(con, "not a table!"), "if a string, `spatial_filter` must be a table in the connection")

  DBI::dbDisconnect(con)
})


test_that("spatial_filter works correctly in open_curtain", {
  
  con <- stage_conn()

  return_hood <- function(spatial_filter) {
    open_curtain("division", spatial_filter)
  }

})
