test_that("downloading works by directory", {
  skip_if_offline()
  library(dplyr, warn.conflicts = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb())
  counties <- open_curtain("division_area", bbox = NULL, conn = con) %>%
    # in R, filtering on variables must come before removing them via select
    filter(subtype == "county" & country == "US")


  dir <- tempdir()

  timer <- bench::mark(
    exec = DBI::dbExecute(con, dbplyr::sql_render(counties)),
    # copy = {
    #   # test some pieces of the function for performance
    #   playbill <- attr(counties, "overture_playbill")
    #
    #   type <- playbill[["type"]]
    #   theme <- playbill[["theme"]]
    #
    #   cols <- colnames(counties)
    #   county_copy <- dplyr::mutate(counties, geometry = ST_AsWKB(geometry))
    #
    #   sql <- dbplyr::sql_render(county_copy)
    #   DBI::dbExecute(con, glue::glue(
    #   "COPY  ({sql}) TO '{dir}'
    #   (FORMAT PARQUET, PARTITION_BY (theme, type), OVERWRITE_OR_IGNORE)")
    # )},
    func = {
      counties_dl <- record_overture(dir, counties, conn = con, overwrite = TRUE)
    },
    check = FALSE, filter_gc = FALSE
  )

  exec_mem <- filter(timer, as.character(expression) == "exec")$mem_alloc
  func_mem <- filter(timer, as.character(expression) == "func")$mem_alloc
  # copy_mem <- filter(timer, expression == "copy") |> pull(mem_alloc)

  # check that function uses < 10% of actual memory. TODO: make stricter (1%?)
  expect_lt(func_mem, exec_mem / 10)

  collect_timer <- bench::mark(
    default = {default <- collect(counties)},
    dl = {dl <- collect(counties_dl)},
    check = FALSE, max_iterations = 5, filter_gc = FALSE
  )

  m_def <- filter(collect_timer, as.character(expression) == "default")$median
  m_dl <- filter(collect_timer, as.character(expression) == "dl")$median

  expect_lt(m_dl, m_def / 10)

  expect_equal(colnames(default), colnames(dl))
  expect_equal(dim(default), dim(dl))
  expect_equal(class(default), class(dl))
  expect_equal(sum(sf::st_area(default)), sum(sf::st_area(dl)))

  unlink(dir)
  DBI::dbDisconnect(con)
})

