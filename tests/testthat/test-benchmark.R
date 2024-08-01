test_that("Configuration is fast", {
  conn <- duckdb::dbConnect(duckdb::duckdb())
  init_timer <- system.time(config_extensions(conn))
  redo_timer <- system.time(config_extensions(conn))

  duckdb::dbDisconnect(conn)

  expect_lt(init_timer[["elapsed"]], 5)
  expect_lt(redo_timer[["elapsed"]], 0.5)
})

test_that("stage_conn will pass CRAN", {
  # cran requires user time <= 2.5x elapsed time
  timer <- system.time({
    con <- stage_conn()
    strike_stage(con)
  })

  #sys.time tests differently on GH actions
  user <- ifelse(
    is.na(timer[["user.self"]]), timer[["user.child"]], timer[["user.self"]]
  )

  skip_if(is.na(user))

  expect_lte(user / timer[["elapsed"]], 2.5)
})

