# The few tests that read Overture's real release. See skip_if_not_live() in
# helper.R for when they run. Everything else in the suite is offline.

test_that("latest_overture_release finds a current release", {
  skip_if_not_live()
  withr::local_options(overturer_latest_release = NULL)

  release <- latest_overture_release()
  expect_match(release, "^\\d{4}-\\d{2}-\\d{2}\\.\\d+$")
  expect_equal(latest_overture_release(), release)
  expect_true(release %in% stac_releases(stage_conn()))

  # the live type table covers everything the package knows about
  types <- overture_types(release)
  expect_true(all(names(type_theme_map) %in% types$type))
  for (type in names(type_theme_map)) {
    expect_equal(
      types$theme[types$type == type], type_theme_map[[type]],
      info = type
    )
  }
})

test_that("open_curtain reads, prunes, and collects from the live release", {
  skip_if_not_live()
  conn <- local_conn()
  bbox <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)

  buildings <- open_curtain("building", bbox, conn = conn, tablename = "live")
  sql <- view_sql(conn, "live")
  expect_no_match(sql, "type=building/*", fixed = TRUE)
  expect_match(sql, "\\.parquet'")
  # far fewer than the hundreds of files in the partition
  n_files <- lengths(regmatches(sql, gregexpr(".parquet'", sql, fixed = TRUE)))
  expect_lt(n_files, 10)

  collected <- collect(head(buildings, 5))
  expect_s3_class(collected, "sf")
  expect_equal(nrow(collected), 5)
  expect_equal(sf::st_crs(collected), sf::st_crs(4326))

  # the pruned query returns exactly the rows the wildcard query does
  n_pruned <- dplyr::pull(dplyr::count(buildings), n)
  withr::local_options(overturer_prune = FALSE)
  unpruned <- open_curtain(
    "building", bbox, conn = conn, tablename = "live_all"
  )
  expect_match(view_sql(conn, "live_all"), "type=building/*", fixed = TRUE)
  expect_equal(dplyr::pull(dplyr::count(unpruned), n), n_pruned)
  expect_gt(n_pruned, 0)
})

test_that("an sf filter and record_overture work against the live release", {
  skip_if_not_live()
  conn <- local_conn()

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  meck <- subset(nc, NAME == "Mecklenburg") # NAD27, not 4326, on purpose
  counties <- open_curtain("division_area", meck, conn = conn) |>
    dplyr::filter(subtype == "county")
  counties_sf <- collect(counties)
  expect_true(any(grepl("Mecklenburg", counties_sf$names$primary)))
  meck_4326 <- sf::st_transform(meck, 4326)
  expect_true(all(lengths(sf::st_intersects(counties_sf, meck_4326)) > 0))

  dir <- withr::local_tempdir()
  local <- record_overture(counties, dir, overwrite = TRUE)
  copied <- collect(local)
  expect_equal(sort(copied$id), sort(counties_sf$id))
  expect_equal(colnames(copied), colnames(counties_sf))
})
