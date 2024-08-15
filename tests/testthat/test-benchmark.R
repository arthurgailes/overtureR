test_that("Configuration is fast", {
  skip_on_cran()
  conn <- duckdb::dbConnect(duckdb::duckdb())
  init_timer <- system.time(config_extensions(conn))
  redo_timer <- system.time(config_extensions(conn))

  duckdb::dbDisconnect(conn)

  expect_lt(init_timer[["elapsed"]], 5)
  expect_lt(redo_timer[["elapsed"]], 0.5)
})

# test_that("stage_conn will pass CRAN", {
#   # cran requires user time <= 2.5x elapsed time, but also won't accept a test
#   skip_if_not(interactive())
#   skip_on_ci()
#   skip_on_cran()
#   timer <- system.time({
#     con <- stage_conn()
#     strike_stage(con)
#   })

#   expect_lte(timer[["user.self"]] / timer[["elapsed"]], 2.5)
# })
