test_that("record_overture writes a local copy that reads back the same", {
  conn <- local_conn()
  places <- local_fixture_curtain("place", conn = conn)
  dir <- withr::local_tempdir()

  local <- record_overture(places, dir, overwrite = TRUE)

  expect_s3_class(local, "overture_call")
  expect_equal(
    attr(local, "overture_playbill"),
    list(type = "place", theme = "places", release = fixture_release)
  )
  expect_true(dir.exists(file.path(dir, "theme=places", "type=place")))
  sql <- view_sql(conn, dbplyr::remote_name(local))
  expect_match(sql, basename(dir), fixed = TRUE)
  expect_match(sql, "theme=places/type=place/", fixed = TRUE)

  remote <- collect(places)
  copied <- collect(local)
  expect_equal(colnames(remote), colnames(copied))
  expect_equal(dim(remote), dim(copied))
  expect_equal(class(remote), class(copied))
  expect_equal(sort(remote$id), sort(copied$id))
  expect_equal(sf::st_crs(copied), sf::st_crs(4326))
})

test_that("record_overture keeps a filtered query, not the whole partition", {
  conn <- local_conn()
  buildings <- local_fixture_curtain("building", conn = conn)
  tall <- dplyr::filter(buildings, !is.na(height), height > 100)
  dir <- withr::local_tempdir()

  local <- record_overture(tall, dir, overwrite = TRUE)
  expect_equal(
    dplyr::pull(dplyr::count(local), n),
    dplyr::pull(dplyr::count(tall), n)
  )
  expect_true(all(collect(local)$height > 100))
})

test_that("record_overture respects overwrite", {
  conn <- local_conn()
  places <- local_fixture_curtain("place", conn = conn)
  dir <- file.path(withr::local_tempdir(), "fresh")

  # creates a missing directory
  expect_no_error(record_overture(places, dir))
  # refuses to write into a non-empty one
  expect_error(record_overture(places, dir), "'overwrite' must be set to TRUE")
  expect_no_error(record_overture(places, dir, overwrite = TRUE))
})

test_that("record_overture passes write_opts through and rejects overrides", {
  conn <- local_conn()
  places <- local_fixture_curtain("place", conn = conn)
  dir <- withr::local_tempdir()

  expect_error(
    record_overture(places, dir, write_opts = "OVERWRITE", overwrite = TRUE)
  )
  expect_error(
    record_overture(
      places, dir, write_opts = "PARTITION_BY(thing)", overwrite = TRUE
    )
  )
  expect_error(record_overture(mtcars, dir), "must be a overture_call")

  result <- record_overture(
    places, dir, write_opts = "ROW_GROUP_SIZE 100000", overwrite = TRUE
  )
  expect_s3_class(result, "overture_call")
})

test_that("snapshot_overture defaults to a temporary directory", {
  conn <- local_conn()
  places <- local_fixture_curtain("place", conn = conn)

  snapshot <- snapshot_overture(places)
  expect_s3_class(snapshot, "overture_call")
  sql <- view_sql(conn, dbplyr::remote_name(snapshot))
  expect_match(sql, basename(tempdir()), fixed = TRUE)
  expect_equal(
    dplyr::pull(dplyr::count(snapshot), n),
    dplyr::pull(dplyr::count(places), n)
  )
})

test_that("record_overture writes a manifest that open_curtain prunes with", {
  conn <- local_conn()
  buildings <- local_fixture_curtain("building", conn = conn)
  dir <- withr::local_tempdir()

  local <- record_overture(buildings, dir, overwrite = TRUE, grid = 0.002)
  type_dir <- file.path(dir, "theme=buildings", "type=building")
  expect_true(file.exists(file.path(type_dir, "_overture.json")))
  cells <- list.dirs(type_dir, full.names = FALSE, recursive = FALSE)
  expect_gt(length(cells), 1)
  expect_match(cells, "^x_cell=")

  manifest <- recording_manifest(conn, dir, "buildings", "building")
  expect_equal(manifest$release, fixture_release)
  expect_gt(nrow(manifest$files), 1)
  expect_true(all(file.exists(manifest$files$file)))
  expect_true(all(manifest$files$xmin <= manifest$files$xmax))
  parquet <- Sys.glob(file.path(type_dir, "*", "*", "*.parquet"))
  expect_equal(nrow(manifest$files), length(parquet))

  # the copy answers a small query from a subset of its files
  corner <- c(xmin = -73.988, ymin = 40.757, xmax = -73.987, ymax = 40.758)
  pruned <- open_curtain(
    "building", corner, conn = conn, base_url = dir, tablename = "corner"
  )
  sql <- view_sql(conn, "corner")
  expect_no_match(sql, "**", fixed = TRUE)
  n_files <- lengths(regmatches(sql, gregexpr(".parquet'", sql, fixed = TRUE)))
  expect_lt(n_files, nrow(manifest$files))
  expect_equal(playbill(pruned)$release, fixture_release)

  withr::local_options(overturer_prune = FALSE)
  unpruned <- open_curtain("building", corner, conn = conn, base_url = dir)
  expect_equal(
    sort(dplyr::pull(pruned, id)), sort(dplyr::pull(unpruned, id))
  )
  expect_gt(dplyr::pull(dplyr::count(pruned), n), 0)
})

test_that("record_overture accepts extra partition columns", {
  conn <- local_conn()
  buildings <- local_fixture_curtain("building", conn = conn)
  dir <- withr::local_tempdir()

  local <- record_overture(
    buildings, dir, overwrite = TRUE, partition_by = "subtype"
  )
  type_dir <- file.path(dir, "theme=buildings", "type=building")
  subdirs <- list.dirs(type_dir, full.names = FALSE, recursive = FALSE)
  expect_match(subdirs, "^subtype=")
  expect_equal(
    dplyr::pull(dplyr::count(local), n),
    dplyr::pull(dplyr::count(buildings), n)
  )

  expect_error(
    record_overture(buildings, dir, overwrite = TRUE, partition_by = "nope"),
    "not in the data: nope"
  )
  expect_error(record_overture(buildings, dir, overwrite = TRUE, grid = -1))
  expect_error(
    record_overture(
      dplyr::select(buildings, id, geometry), dir, overwrite = TRUE, grid = 1
    ),
    "`bbox` column"
  )
})

test_that("record_overture can open the data itself", {
  conn <- local_conn()
  local_fixture_stac()
  dir <- withr::local_tempdir()

  local <- record_overture(
    "place", dir,
    spatial_filter = fixture_bbox, conn = conn, base_url = fixture_base_url()
  )
  expect_s3_class(local, "overture_call")
  expect_equal(playbill(local)$type, "place")
  direct <- local_fixture_curtain("place", conn = conn)
  expect_equal(
    dplyr::pull(dplyr::count(local), n), dplyr::pull(dplyr::count(direct), n)
  )

  expect_error(
    record_overture(direct, dir, spatial_filter = fixture_bbox),
    "only apply when `curtain_call` is a type name"
  )
})

test_that("record_overture quotes the output path", {
  conn <- local_conn()
  places <- local_fixture_curtain("place", conn = conn)
  dir <- file.path(withr::local_tempdir(), "o'brien's data")

  local <- record_overture(places, dir)
  expect_true(dir.exists(file.path(dir, "theme=places", "type=place")))
  expect_equal(
    dplyr::pull(dplyr::count(local), n), dplyr::pull(dplyr::count(places), n)
  )
})

test_that("overwrite = TRUE replaces the partition, not the directory", {
  conn <- local_conn()
  places <- local_fixture_curtain("place", conn = conn)
  buildings <- local_fixture_curtain("building", conn = conn)
  dir <- withr::local_tempdir()
  writeLines("keep me", file.path(dir, "notes.txt"))

  record_overture(buildings, dir, overwrite = TRUE)
  record_overture(places, dir, overwrite = TRUE)
  few <- dplyr::filter(places, !is.na(confidence), confidence > 0.9)
  local <- record_overture(few, dir, overwrite = TRUE)

  expect_equal(
    dplyr::pull(dplyr::count(local), n), dplyr::pull(dplyr::count(few), n)
  )
  expect_true(file.exists(file.path(dir, "notes.txt")))
  expect_true(dir.exists(file.path(dir, "theme=buildings", "type=building")))
  still_buildings <- open_curtain("building", conn = conn, base_url = dir)
  expect_equal(
    dplyr::pull(dplyr::count(still_buildings), n),
    dplyr::pull(dplyr::count(buildings), n)
  )
})
