test_that("downloading works by directory", {
  library(dplyr, warn.conflicts = FALSE)

  con <- stage_conn()
  counties <- open_curtain("division_area", bbox = NULL) %>%
    # in R, filtering on variables must come before removing them via select
    filter(subtype == "county" & country == "US")

  sql <- dbplyr::sql_render(counties)

  dir <- tempdir()


  timer <- bench::mark(
    method = download_overture(dir, counties),
    copy = {
      playbill <- attr(counties, "overture_playbill")

      type <- playbill[["type"]]
      theme <- playbill[["theme"]]
      DBI::dbExecute(con, glue::glue(
      "COPY  ({sql}) TO '{dir}'
      (FORMAT PARQUET, PARTITION_BY (theme, type), OVERWRITE_OR_IGNORE)")
    )},
    # exec = DBI::dbExecute(con, sql),
    check = FALSE
  )
  View(timer$memory[[1]])

  sizes <- file.size(list.files(dir, ".parquet", recursive = TRUE))

  expect_lt(timer$mem_alloc, 1024^2)

  unlink(dir)

})

