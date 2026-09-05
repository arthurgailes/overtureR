no_such_catalog <- function() file.path(tempdir(), "no-such-catalog")

test_that("stac_releases lists releases newest first", {
  local_fixture_stac()
  conn <- local_conn()

  expect_equal(stac_releases(conn), c("2024-01-01.0", "2023-12-01.0"))
})

test_that("latest_overture_release reads and caches the catalog's latest", {
  local_fixture_stac()
  conn <- local_conn()

  expect_equal(latest_overture_release(conn), fixture_release)
  expect_equal(getOption("overturer_latest_release"), fixture_release)

  # served from the session cache, so a broken catalog isn't touched
  withr::local_options(overturer_stac_url = "nowhere")
  expect_equal(latest_overture_release(conn), fixture_release)
  expect_error(latest_overture_release(conn, refresh = TRUE))
})

test_that("latest_overture_release errors clearly with no catalog or cache", {
  local_fixture_stac()
  conn <- local_conn()
  withr::local_options(overturer_stac_url = no_such_catalog())

  expect_error(
    latest_overture_release(conn),
    "Could not reach Overture's release catalog.*pass `base_url`"
  )
  expect_null(getOption("overturer_latest_release"))
})

test_that("latest_overture_release falls back to the newest cached release", {
  cache <- local_fixture_stac()
  conn <- local_conn()

  # warm the disk cache, then take the catalog away
  overture_types(fixture_release, conn = conn)
  expect_true(dir.exists(file.path(cache, fixture_release)))
  withr::local_options(overturer_stac_url = no_such_catalog())

  expect_warning(
    release <- latest_overture_release(conn),
    "Using the newest cached release, 2024-01-01.0"
  )
  expect_equal(release, fixture_release)
})

test_that("overture_types reads the type table from the catalog", {
  local_fixture_stac()
  conn <- local_conn()

  types <- overture_types(fixture_release, conn = conn)
  expect_equal(
    types,
    data.frame(
      type = c("building", "building_part", "place"),
      theme = c("buildings", "buildings", "places"),
      stringsAsFactors = FALSE
    )
  )

  # default release is the latest
  expect_equal(overture_types(conn = conn), types)
})

test_that("overture_types falls back to the built-in table with a warning", {
  local_fixture_stac()
  conn <- local_conn()

  expect_warning(
    types <- overture_types("2000-01-01.0", conn = conn),
    "Using the package's built-in list"
  )
  expect_equal(types, static_overture_types())
  expect_true("bathymetry" %in% types$type)
  expect_equal(types$theme[types$type == "bathymetry"], "base")
})

test_that("get_theme_from_type knows the built-in types offline", {
  withr::local_options(overturer_stac_url = "nowhere")

  expect_equal(get_theme_from_type("building"), "buildings")
  expect_equal(get_theme_from_type("bathymetry"), "base")
  expect_equal(get_theme_from_type("segment"), "transportation")
  expect_error(get_theme_from_type("*"), "`theme` must be set")
  expect_error(get_theme_from_type(NULL), "`theme` must be set")
  expect_error(get_theme_from_type(c("a", "b")), "single string")
})

test_that("get_theme_from_type lists valid types for an unknown type", {
  local_fixture_stac()
  local_conn()

  expect_error(
    get_theme_from_type("skyscraper", release = fixture_release),
    "Unknown Overture type \"skyscraper\". Valid types are: address, bathymetry"
  )
})

test_that("stac_manifest returns one row per file with its bbox", {
  local_fixture_stac()
  conn <- local_conn()

  manifest <- stac_manifest(fixture_release, "buildings", "building", conn)
  expect_named(manifest, c("file", "xmin", "ymin", "xmax", "ymax"))
  expect_equal(nrow(manifest), 3)
  expect_equal(manifest$file[[1]], "part-00000-fixture-building.zstd.parquet")
  expect_equal(manifest$xmin[[2]], -10)
  expect_equal(manifest$ymax[[3]], -10)

  # an unknown type warns and returns NULL so callers can fall back
  expect_warning(
    missing <- stac_manifest(fixture_release, "buildings", "castle", conn),
    "Reading every file instead"
  )
  expect_null(missing)
})

test_that("prune_manifest keeps only files whose bbox touches the filter", {
  manifest <- data.frame(
    file = c("world", "europe", "australia"),
    xmin = c(-180, -10, 112), ymin = c(-90, 35, -44),
    xmax = c(180, 30, 154), ymax = c(90, 60, -10)
  )

  new_york <- c(xmin = -74, ymin = 40.7, xmax = -73.9, ymax = 40.8)
  expect_equal(prune_manifest(manifest, new_york), "world")

  paris <- c(xmin = 2.2, ymin = 48.8, xmax = 2.4, ymax = 48.9)
  expect_equal(prune_manifest(manifest, paris), c("world", "europe"))

  # a shared edge counts as touching, like the SQL bbox test
  edge <- c(xmin = 30, ymin = 40, xmax = 40, ymax = 50)
  expect_equal(prune_manifest(manifest, edge), c("world", "europe"))

  nothing <- data.frame(file = "x", xmin = 0, ymin = 0, xmax = 1, ymax = 1)
  expect_equal(prune_manifest(nothing, paris), character())
})

test_that("prune_files builds full paths and falls back on missing files", {
  local_fixture_stac()
  conn <- local_conn()
  base_url <- fixture_base_url()
  buildings <- function(base_url, bbox, type = "building") {
    prune_files(conn, base_url, fixture_release, "buildings", type, bbox)
  }

  expect_equal(
    buildings(base_url, fixture_bbox),
    file.path(
      base_url,
      "theme=buildings/type=building/part-00000-fixture-building.zstd.parquet"
    )
  )

  # nothing touches Antarctica: read one file, whose bbox filter yields no rows
  antarctica <- c(xmin = 0, ymin = -89, xmax = 1, ymax = -88)
  expect_length(buildings(base_url, antarctica), 1)

  # a Paris filter wants the (missing) europe file, so a local copy can't prune
  paris <- c(xmin = 2.2, ymin = 48.8, xmax = 2.4, ymax = 48.9)
  expect_null(buildings(base_url, paris))

  # remote paths are never checked for existence
  remote <- "s3://overturemaps-us-west-2/release/2024-01-01.0"
  paths <- buildings(remote, paris)
  expect_length(paths, 2)
  expect_match(paths, paste0("^", remote, "/theme=buildings/type=building/"))

  # no manifest, no pruning
  expect_warning(expect_null(buildings(remote, paris, type = "castle")))
})

test_that("release_from_url and is_remote_url recognise release URLs", {
  s3 <- "s3://overturemaps-us-west-2/release/2026-08-19.0"
  azure <- "https://x.blob.core.windows.net/release/2026-08-19.0/"
  expect_equal(release_from_url(s3), "2026-08-19.0")
  expect_equal(release_from_url(azure), "2026-08-19.0")
  expect_equal(release_from_url("C:/data/release/2024-01-01.0"), "2024-01-01.0")
  expect_null(release_from_url(tempdir()))
  expect_null(release_from_url("s3://bucket/release/latest"))

  expect_true(is_remote_url("s3://bucket/x"))
  expect_true(is_remote_url("https://host/x"))
  expect_false(is_remote_url("C:/data/x"))
  expect_false(is_remote_url("/home/x"))
})

test_that("the catalog cache works in memory and on disk", {
  cache <- local_fixture_stac()
  conn <- local_conn()

  places <- function() stac_manifest(fixture_release, "places", "place", conn)

  manifest <- places()
  path <- file.path(cache, fixture_release, "manifest-places-place.rds")
  expect_true(file.exists(path))
  expect_equal(readRDS(path), manifest)

  # memory hit: even with the catalog gone, the answer is the same
  withr::local_options(overturer_stac_url = "nowhere")
  expect_equal(places(), manifest)

  # disk hit: a fresh session (empty memory cache) reads the file back
  forget_stac_cache()
  expect_equal(places(), manifest)

  # cache off: memory only, nothing new written
  withr::local_options(overturer_cache = FALSE)
  forget_stac_cache()
  expect_warning(places())
  withr::local_options(overturer_stac_url = fixture_stac_url())
  overture_types(fixture_release, conn = conn)
  expect_false(file.exists(file.path(cache, fixture_release, "types.rds")))
})

test_that("the disk cache keeps only the newest releases", {
  cache <- local_fixture_stac()
  releases <- c("2024-01-01.0", "2024-02-01.0", "2024-03-01.0", "2024-04-01.0")
  for (release in releases) {
    stac_cache_set(release, "types", data.frame(type = "x", theme = "y"))
  }
  expect_equal(
    list.dirs(cache, full.names = FALSE, recursive = FALSE),
    c("2024-02-01.0", "2024-03-01.0", "2024-04-01.0")
  )
  expect_equal(stac_cached_releases()[[1]], "2024-04-01.0")

  clear_overture_cache()
  expect_false(dir.exists(cache))
  expect_length(ls(envir = overtureR:::.stac_cache), 0)
})
