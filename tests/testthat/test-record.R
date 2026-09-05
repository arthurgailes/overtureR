test_that("record_overture writes a local copy that reads back the same", {
  conn <- local_conn()
  places <- local_fixture_curtain("place", conn = conn)
  dir <- withr::local_tempdir()

  local <- record_overture(places, dir, overwrite = TRUE)

  expect_s3_class(local, "overture_call")
  expect_equal(
    attr(local, "overture_playbill"), c(type = "place", theme = "places")
  )
  expect_true(dir.exists(file.path(dir, "theme=places", "type=place")))
  sql <- view_sql(conn, dbplyr::remote_name(local))
  expect_match(sql, basename(dir), fixed = TRUE)
  expect_match(sql, "theme=places/type=place/*", fixed = TRUE)

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
