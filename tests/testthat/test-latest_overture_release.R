test_that("latest_overture_release discovers a current release", {
  skip_if_offline()
  skip_on_cran()

  options(overturer_latest_release = NULL)
  release <- latest_overture_release()

  expect_type(release, "character")
  expect_match(release, "^\\d{4}-\\d{2}-\\d{2}\\.\\d+$")

  # cached on second call, without re-querying
  expect_equal(latest_overture_release(), release)
})

test_that("open_curtain's default base_url uses the latest release", {
  skip_if_offline()
  skip_on_cran()

  options(overturer_latest_release = NULL)
  release <- latest_overture_release()

  conn <- stage_conn()
  open_curtain("division", tablename = "test_latest_release")
  view_sql <- DBI::dbGetQuery(
    conn,
    "SELECT sql FROM duckdb_views() WHERE view_name = 'test_latest_release'"
  )$sql

  expect_true(grepl(release, view_sql, fixed = TRUE))
})
