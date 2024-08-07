test_that("Configuration is fast", {
  conn <- duckdb::dbConnect(duckdb::duckdb())
  init_timer <- system.time(config_extensions(conn))
  redo_timer <- system.time(config_extensions(conn))

  duckdb::dbDisconnect(conn)

  expect_lt(init_timer[["elapsed"]], 5)
  expect_lt(redo_timer[["elapsed"]], 0.5)
})

test_that("stage_conn will pass CRAN", {
  # cran requires user time <= 2.5x elapsed time, but also won't accept a test
  skip_on_cran()
  skip_on_ci()
  timer <- system.time({
    con <- stage_conn()
    strike_stage(con)
  })

  skip_if(is.na(timer[["user.self"]]))
  expect_lte(timer[["user.self"]] / timer[["elapsed"]], 2.5)
})

