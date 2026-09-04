broadway <- c(xmin = -73.99, ymin = 40.76, xmax = -73.98, ymax = 40.76)

test_that("downloading works by directory", {
  skip_if_offline()
  skip_on_cran()

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  counties <- open_curtain("division_area", bbox = NULL, conn = con) |>
    dplyr::filter(subtype == "county" & country == "US")

  # use a fresh dir that doesn't exist
  dir <- file.path(tempdir(), "overtureR_record_dir")
  unlink(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  counties_dl <- record_overture(counties, dir, overwrite = TRUE)

  default <- dplyr::collect(counties)
  dl <- dplyr::collect(counties_dl)

  # the local round-trip returns the same data as the remote query
  expect_equal(colnames(default), colnames(dl))
  expect_equal(dim(default), dim(dl))
  expect_equal(class(default), class(dl))
  expect_equal(sum(sf::st_area(default)), sum(sf::st_area(dl)))
})


test_that("record_overture respects overwrite parameter", {
  skip_if_offline()
  skip_on_cran()

  # a fresh, dedicated dir (not tempdir() itself, which is rarely empty)
  dir <- file.path(tempdir(), "overtureR_overwrite_test")
  unlink(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  place <- open_curtain("place", broadway)

  # First write into the empty dir
  record_overture(place, dir)

  # Second write without overwrite errors because the dir is not empty
  expect_error(record_overture(place, dir))

  # Second write with overwrite succeeds
  expect_no_error(record_overture(place, dir, overwrite = TRUE))
})

test_that("record_overture handles custom write_opts", {
  skip_if_offline()
  skip_on_cran()

  dir <- file.path(tempdir(), "overtureR_writeopts_test")
  unlink(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  place <- open_curtain("place", broadway)

  expect_error(record_overture(place, dir, write_opts = "OVERWRITE", overwrite = TRUE))
  expect_error(record_overture(place, dir, write_opts = "PARTITION_BY(thing)", overwrite = TRUE))

  custom_opts <- c("ROW_GROUP_SIZE 100000")
  result <- record_overture(place, dir, write_opts = custom_opts, overwrite = TRUE)

  expect_s3_class(result, "overture_call")

  # Check if custom partitioning was applied (this might require inspecting the file structure)
  expect_true(dir.exists(file.path(dir, "theme=places")))
})

test_that("snapshot_overture works correctly", {
  skip_if_offline()
  skip_on_cran()

  dir <- file.path(tempdir(), "overtureR_snapshot_test")
  unlink(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  result <- snapshot_overture(open_curtain("place", spatial_filter = broadway), output_dir = dir)

  expect_s3_class(result, "overture_call")
  expect_true(dir.exists(file.path(dir, "theme=places")))
})
