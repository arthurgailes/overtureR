broadway <- c(xmin = -73.99, ymin = 40.76, xmax = -73.98, ymax = 40.76)

test_that("downloading works by directory", {
  skip_if_offline()

  con <- DBI::dbConnect(duckdb::duckdb())
  counties <- open_curtain("division_area", bbox = NULL, conn = con) |>
    dplyr::filter(subtype == "county" & country == "US")

  dir <- tempdir()

  timer <- bench::mark(
    exec = DBI::dbExecute(con, dbplyr::sql_render(counties)),
    func = {
      counties_dl <- record_overture(dir, counties, conn = con, overwrite = TRUE)
    },
    check = FALSE, filter_gc = FALSE
  )

  exec_mem <- dplyr::filter(timer, as.character(expression) == "exec")$mem_alloc
  func_mem <- dplyr::filter(timer, as.character(expression) == "func")$mem_alloc

  expect_lt(func_mem, exec_mem / 10)

  collect_timer <- bench::mark(
    default = {default <- dplyr::collect(counties)},
    dl = {dl <- dplyr::collect(counties_dl)},
    check = FALSE, max_iterations = 5, filter_gc = FALSE
  )

  m_def <- dplyr::filter(collect_timer, as.character(expression) == "default")$median
  m_dl <- dplyr::filter(collect_timer, as.character(expression) == "dl")$median

  expect_lt(m_dl, m_def / 10)

  expect_equal(colnames(default), colnames(dl))
  expect_equal(dim(default), dim(dl))
  expect_equal(class(default), class(dl))
  expect_equal(sum(sf::st_area(default)), sum(sf::st_area(dl)))

  unlink(dir)
  DBI::dbDisconnect(con)
})

test_that("record_overture handles NULL curtain_call correctly", {
  skip_if_offline()

  dir <- tempdir()

  # Test with NULL curtain_call

  result <- record_overture(dir, type = "place", spatial_filter = broadway, overwrite = TRUE)

  expect_s3_class(result, "overture_call")
  expect_true("place" %in% attr(result, "overture_playbill"))

  unlink(dir, recursive = TRUE)
})

test_that("record_overture respects overwrite parameter", {
  skip_if_offline()

  dir <- tempdir()


  # First write
  record_overture(dir, type = "place", spatial_filter = broadway)

  # Second write without overwrite
  expect_error(record_overture(dir, type = "place", spatial_filter = broadway))

  # Second write with overwrite
  expect_no_error(record_overture(dir, type = "place", spatial_filter = broadway, overwrite = TRUE))

  unlink(dir, recursive = TRUE)
})

test_that("record_overture handles custom write_opts", {
  skip_if_offline()

  dir <- tempdir()


  expect_error(record_overture(dir, type = "place", write_opts = "OVERWRITE"))
  expect_error(record_overture(dir, type = "place", write_opts = "PARTITION_BY(thing)"))

  custom_opts <- c("ROW_GROUP_SIZE 100000")
  result <- record_overture(dir, type = "place", spatial_filter = broadway, write_opts = custom_opts, overwrite = TRUE)

  expect_s3_class(result, "overture_call")

  # Check if custom partitioning was applied (this might require inspecting the file structure)
  expect_true(dir.exists(file.path(dir, "theme=places")))

  unlink(dir, recursive = TRUE)
})

test_that("snapshot_overture works correctly", {
  skip_if_offline()

  result <- snapshot_overture(type = "place", spatial_filter = broadway)

  expect_s3_class(result, "overture_call")
  expect_true(dir.exists(file.path(tempdir(), "theme=places")))

  # Clean up
  unlink(list.files(tempdir(), pattern = "theme=", full.names = TRUE), recursive = TRUE)
})
