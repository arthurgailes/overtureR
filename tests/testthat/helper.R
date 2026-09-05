# Shared test setup. Most tests run offline against the fixtures in
# tests/testthat/fixtures: a few hundred Overture features around Times Square
# in Overture's partition layout under a made-up release, and a miniature of
# Overture's STAC catalog describing them. See fixtures/make-fixtures.R.

fixture_release <- "2024-01-01.0"

fixture_path <- function(...) {
  testthat::test_path("fixtures", ...)
}

# A base_url that looks like an Overture release, so open_curtain() prunes
# files through the (fixture) catalog. The Parquet files are stored flat in
# fixtures/data (CRAN limits path lengths) and laid out in Overture's
# `release/<version>/theme=<theme>/type=<type>/` structure in a temporary
# directory the first time a test asks for it.
fixture_base_url <- function() {
  root <- file.path(tempdir(), "overtureR_fixtures", "release", fixture_release)
  themes <- c(
    building = "buildings", building_part = "buildings", place = "places"
  )
  for (type in names(themes)) {
    partition <- paste0("theme=", themes[[type]], "/type=", type)
    dir <- file.path(root, partition)
    file <- file.path(dir, sprintf("part-00000-fixture-%s.zstd.parquet", type))
    if (!file.exists(file)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      file.copy(fixture_path("data", paste0(type, ".parquet")), file)
    }
  }
  normalizePath(root, winslash = "/")
}

# A bbox inside the fixture data: a few blocks around Times Square.
fixture_bbox <- c(xmin = -73.988, ymin = 40.757, xmax = -73.983, ymax = 40.760)

fixture_stac_url <- function() {
  normalizePath(fixture_path("stac"), winslash = "/")
}

# Point the package at the fixture catalog and a throwaway cache for the
# duration of the calling test. Also forgets any release cached in the session.
local_fixture_stac <- function(env = parent.frame()) {
  cache_dir <- withr::local_tempdir(.local_envir = env)
  withr::local_options(
    list(
      overturer_stac_url = fixture_stac_url(),
      overturer_cache_dir = cache_dir,
      overturer_cache = TRUE,
      overturer_latest_release = NULL
    ),
    .local_envir = env
  )
  withr::defer(forget_stac_cache(), envir = env)
  forget_stac_cache()
  invisible(cache_dir)
}

# Empty the in-memory catalog cache, as a fresh R session would have it.
forget_stac_cache <- function() {
  cache <- overtureR:::.stac_cache
  rm(list = ls(envir = cache), envir = cache)
}

# A fresh duckdb connection, closed when the calling test ends. Skips the
# test when DuckDB's extensions can't be installed, as on a machine without
# network access.
local_conn <- function(env = parent.frame()) {
  conn <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(conn, shutdown = TRUE), envir = env)
  tryCatch(
    config_extensions(conn),
    error = function(e) {
      testthat::skip(paste("duckdb extensions unavailable:", e$message))
    }
  )
  conn
}

# An overture_call over the fixture data.
local_fixture_curtain <- function(
  type = "building",
  spatial_filter = fixture_bbox,
  conn = local_conn(env),
  env = parent.frame(),
  ...
) {
  local_fixture_stac(env)
  open_curtain(
    type, spatial_filter, conn = conn, base_url = fixture_base_url(), ...
  )
}

# The SQL behind a view open_curtain() created.
view_sql <- function(conn, tablename) {
  DBI::dbGetQuery(
    conn,
    glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{tablename}'")
  )$sql
}

# Live tests read Overture's real S3 release. They run locally (devtools::test
# sets NOT_CRAN) and on the weekly CI schedule, which sets OVERTURER_LIVE_TESTS,
# but not on CRAN or on every CI push, where a new Overture release would fail
# unrelated work.
skip_if_not_live <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  on_ci <- isTRUE(as.logical(Sys.getenv("CI", "false")))
  if (on_ci && !identical(Sys.getenv("OVERTURER_LIVE_TESTS"), "true")) {
    testthat::skip("live Overture tests run on the weekly schedule")
  }
}
