test_that("Configuration is fast", {

  conn <- duckdb::dbConnect(duckdb::duckdb())
  init_timer <- system.time(config_extensions(conn))
  redo_timer <- system.time(config_extensions(conn))

  duckdb::dbDisconnect(conn)

  expect_lt(init_timer[["elapsed"]], 5)
  expect_lt(redo_timer[["elapsed"]], 0.5)
})

