# stage_conn() caches one connection in options("overturer_conn"). These tests
# leave that option the way they found it.
local_clean_stage <- function(env = parent.frame()) {
  withr::local_options(list(overturer_conn = NULL), .local_envir = env)
  withr::defer(
    {
      conn <- getOption("overturer_conn")
      if (inherits(conn, "duckdb_connection") && DBI::dbIsValid(conn)) {
        DBI::dbDisconnect(conn, shutdown = TRUE)
      }
    },
    envir = env
  )
}

test_that("stage_conn creates one connection and reuses it", {
  local_clean_stage()

  con <- stage_conn()
  expect_s4_class(con, "duckdb_connection")
  expect_true(DBI::dbIsValid(con))
  expect_identical(stage_conn(), con)
  expect_identical(getOption("overturer_conn"), con)

  strike_stage(con)
  expect_false(DBI::dbIsValid(con))
  expect_null(getOption("overturer_conn"))
})

test_that("stage_conn passes dbdir and friends to duckdb", {
  local_clean_stage()
  dbdir <- withr::local_tempfile(fileext = ".duckdb")

  con <- stage_conn(dbdir = dbdir)
  DBI::dbExecute(con, "CREATE TABLE t AS SELECT 1 AS x")
  expect_true(file.exists(dbdir))
  info <- DBI::dbGetQuery(con, "PRAGMA database_list")
  expect_true(any(grepl(basename(dbdir), info$file, fixed = TRUE)))
  strike_stage(con)

  # and the file is a real database that can be reopened
  con2 <- stage_conn(dbdir = dbdir, read_only = TRUE)
  expect_equal(DBI::dbGetQuery(con2, "SELECT x FROM t")$x, 1)
  expect_error(
    DBI::dbExecute(con2, "CREATE TABLE u AS SELECT 2 AS y"), "read-only"
  )
  strike_stage(con2)
})

test_that("stage_conn replaces a cached connection that was closed elsewhere", {
  local_clean_stage()

  con <- stage_conn()
  DBI::dbDisconnect(con, shutdown = TRUE)
  con2 <- stage_conn()
  expect_true(DBI::dbIsValid(con2))
  expect_false(identical(con, con2))
})

test_that("stage_conn registers its finalizer once", {
  local_clean_stage()
  withr::defer(.stage_state$finalizer_set <- TRUE)
  .stage_state$finalizer_set <- FALSE

  registrations <- 0
  local_mocked_bindings(
    register_stage_finalizer = function() registrations <<- registrations + 1
  )
  stage_conn()
  stage_conn()
  strike_stage()
  stage_conn()
  expect_equal(registrations, 1)
  expect_true(.stage_state$finalizer_set)
})

test_that("strike_stage is safe to call without a connection", {
  local_clean_stage()

  expect_null(getOption("overturer_conn"))
  expect_no_error(strike_stage())
  # nothing was created just to be closed
  expect_null(getOption("overturer_conn"))

  # closing a connection that is not the cached one leaves the cache alone
  cached <- stage_conn()
  other <- DBI::dbConnect(duckdb::duckdb())
  strike_stage(other)
  expect_false(DBI::dbIsValid(other))
  expect_identical(getOption("overturer_conn"), cached)

  # closing the cached one twice is fine
  strike_stage(cached)
  expect_no_error(strike_stage(cached))
  expect_null(getOption("overturer_conn"))
})

test_that("config_extensions loads httpfs and spatial and sets the caches", {
  conn <- local_conn()
  config_extensions(conn)

  loaded <- DBI::dbGetQuery(
    conn, "SELECT extension_name FROM duckdb_extensions() WHERE loaded"
  )$extension_name
  expect_true(all(c("httpfs", "spatial") %in% loaded))

  settings <- DBI::dbGetQuery(
    conn,
    "SELECT name, value FROM duckdb_settings()
     WHERE name IN ('parquet_metadata_cache', 'enable_http_metadata_cache')"
  )
  expect_equal(nrow(settings), 2)
  expect_true(all(settings$value == "true"))

  # a second call is a no-op
  expect_no_error(config_extensions(conn))
})
