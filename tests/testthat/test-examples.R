test_that("record examples work", {
  skip_if_offline()
  broadway <- c(xmin = -73.99011, ymin = 40.755488, xmax = -73.98, ymax = 40.76206)
  buildings <- open_curtain("building", spatial_filter = broadway)
  local_buildings <- record_overture(tempdir(), buildings, overwrite = TRUE)

  expect_gt(pull(count(local_buildings), n), 0)
})


test_that("collect.overture_call example works", {
  skip_if_offline()

  bbox <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)
  lazy_tbl <- open_curtain("building", bbox)
  collected_tbl <- collect(lazy_tbl)

  expect_s3_class(collected_tbl, "sf")
  expect_gt(nrow(collected_tbl), 0)
})

test_that("stage_conn and strike_stage examples work", {
  con <- stage_conn()
  expect_s4_class(con, "duckdb_connection")

  strike_stage(con)
  expect_false(DBI::dbIsValid(con))
})

test_that("record_overture example works", {
  skip_if_offline()

  broadway <- c(xmin = -73.99011, ymin = 40.755488, xmax = -73.98, ymax = 40.76206)
  buildings <- open_curtain("building", spatial_filter = broadway)
  local_buildings <- record_overture(tempdir(), buildings, overwrite = TRUE)

  expect_s3_class(local_buildings, "overture_call")
  expect_gt(pull(count(local_buildings), n), 0)
})

test_that("snapshot_overture works", {
  skip_if_offline()

  broadway <- c(xmin = -73.99011, ymin = 40.755488, xmax = -73.98, ymax = 40.76206)
  buildings <- open_curtain("building", spatial_filter = broadway)
  snapshot <- snapshot_overture(buildings, overwrite = TRUE)

  expect_s3_class(snapshot, "overture_call")
  expect_gt(pull(count(snapshot), n), 0)
})

test_that("sf_as_dbplyr example works", {
  con <- duckdb::dbConnect(duckdb::duckdb())
  sf_obj <- sf::st_sf(a = 3, geometry = sf::st_sfc(st_point(1:2)))
  sf_tbl <- sf::st_sf(con, "test", sf_obj)

  expect_s3_class(sf_tbl, "tbl_duckdb")
  expect_equal(pull(sf_tbl, a), 3)

  duckdb::dbDisconnect(con)
})

test_that("as_overture example works", {
  skip_if_offline()
  conn <- stage_conn()
  division <- open_curtain("division", conn = conn, tablename = "test")

  division2 <- tbl(conn, "test")
  division2 <- as_overture(division2, "division")

  expect_s3_class(division2, "overture_call")

  strike_stage(conn)
})
