test_that("st_crs reads the CRS without collecting", {
  conn <- local_conn()
  buildings <- local_fixture_curtain("building", conn = conn)

  expect_equal(sf::st_crs(buildings), sf::st_crs(4326))
})

test_that("st_bbox matches the collected extent and carries the CRS", {
  conn <- local_conn()
  buildings <- local_fixture_curtain("building", conn = conn)

  bbox <- sf::st_bbox(buildings)
  expect_s3_class(bbox, "bbox")
  expect_equal(sf::st_crs(bbox), sf::st_crs(4326))
  expect_equal(
    as.numeric(bbox),
    as.numeric(sf::st_bbox(collect(buildings)))
  )
})

test_that("overture_crs falls back to EPSG:4326", {
  expect_equal(overture_crs("GEOMETRY('OGC:CRS84')"), sf::st_crs(4326))
  expect_equal(overture_crs("GEOMETRY"), sf::st_crs(4326))
  expect_equal(overture_crs(NULL), sf::st_crs(4326))
  expect_equal(overture_crs("GEOMETRY('EPSG:3857')"), sf::st_crs(3857))
})
