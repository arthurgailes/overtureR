test_that("downloading works by directory", {
  library(dplyr, warn.conflicts = FALSE)

  con <- stage_conn()
  counties <- open_curtain("division_area", bbox = NULL) %>%
    # in R, filtering on variables must come before removing them via select
    filter(subtype == "county" & country == "US")

  sql <- dbplyr::sql_render(counties)

  dir <- tempdir()
  timer <- bench::mark(
    copy = DBI::dbExecute(con, glue::glue(
      "COPY  ({sql}) TO '{dir}' (FORMAT PARQUET, PARTITION_BY (theme, type), OVERWRITE_OR_IGNORE)")
    ),
    load = DBI::dbGetQuery(con, sql),
    exec = DBI::dbExecute(con, sql),
    check = FALSE
  )

  timer

  expect_true(class(counties)[[1]] == "tbl_overture")
  expect_true("tbl_lazy" %in% class(counties))

  counties_sf <- collect(counties)

  expect_false("tbl_overture" %in% class(counties_sf))
  expect_true("sf" %in% class(counties_sf))
  expect_equal(pull(count(counties), n), 67)

  expect_true(all(grepl("County", counties_sf$primary)))
  expect_true(all(sf::st_is_valid(counties_sf$geometry)))
  expect_true(sf::st_crs(counties_sf$geometry) == sf::st_crs(4326))

})
