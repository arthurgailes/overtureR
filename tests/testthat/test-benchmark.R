# these tests are only for reference; time can vary by machine
test_that("Configuration is fast", {
  skip()

  conn <- duckdb::dbConnect(duckdb::duckdb())
  init_timer <- system.time(config_extensions(conn))
  redo_timer <- system.time(config_extensions(conn))

  duckdb::dbDisconnect(conn)

  expect_lt(init_timer[["elapsed"]], 5)
  expect_lt(redo_timer[["elapsed"]], 0.5)
})

test_that("stage_conn will pass CRAN", {
  # cran requires user time <= 2.5x elapsed time, but also won't accept a test
  skip()
  timer <- system.time({
    con <- stage_conn()
    strike_stage(con)
  })

  expect_lte(timer[["user.self"]] / timer[["elapsed"]], 2.5)
})


test_that("sf_as_dbplyr will pass CRAN", {
  skip()
    con <- duckdb::dbConnect(duckdb::duckdb())
    sf_obj <- sf::st_sf(a = 3, geometry = sf::st_sfc(sf::st_point(1:2)))
  timer <- system.time({
    sf_tbl <- sf_as_dbplyr(con, "test", sf_obj)
  })

    expect_s3_class(sf_tbl, "tbl_duckdb_connection")
    expect_equal(dplyr::pull(sf_tbl, a), 3)

    duckdb::dbDisconnect(con)

  expect_lte(timer[["user.self"]] / timer[["elapsed"]], 2.5)
})
