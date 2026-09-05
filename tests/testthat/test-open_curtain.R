test_that("open_curtain returns a lazy overture_call over a local release", {
  conn <- local_conn()
  buildings <- local_fixture_curtain("building", conn = conn)

  expect_s3_class(buildings, "overture_call")
  expect_s3_class(buildings, "tbl_lazy")
  expect_equal(class(buildings)[[1]], "overture_call")
  expect_equal(
    attr(buildings, "overture_playbill"),
    list(type = "building", theme = "buildings", release = fixture_release)
  )
  expect_true(
    all(c("id", "geometry", "bbox", "theme", "type") %in% colnames(buildings))
  )

  n <- dplyr::pull(dplyr::count(buildings), n)
  expect_gt(n, 0)
  expect_lt(n, 561) # the bbox is smaller than the fixture's
})

test_that("open_curtain reads only the files whose bbox touches the filter", {
  conn <- local_conn()
  local_fixture_curtain("building", conn = conn, tablename = "pruned")

  sql <- view_sql(conn, "pruned")
  expect_match(sql, "part-00000-fixture-building.zstd.parquet", fixed = TRUE)
  expect_no_match(sql, "fake-europe")
  expect_no_match(sql, "type=building/*", fixed = TRUE)
  expect_match(sql, "hive_partitioning")
})

test_that("options(overturer_prune = FALSE) reads the whole partition", {
  conn <- local_conn()
  withr::local_options(overturer_prune = FALSE)
  local_fixture_curtain("building", conn = conn, tablename = "wildcard")

  expect_match(view_sql(conn, "wildcard"), "type=building/*", fixed = TRUE)
})

test_that("no spatial filter means no pruning and no bbox clause", {
  conn <- local_conn()
  local_fixture_stac()
  all_buildings <- open_curtain(
    "building",
    conn = conn, base_url = fixture_base_url(), tablename = "everything"
  )

  sql <- view_sql(conn, "everything")
  expect_match(sql, "type=building/*", fixed = TRUE)
  expect_no_match(sql, "bbox.xmax")
  expect_equal(dplyr::pull(dplyr::count(all_buildings), n), 561)
})

test_that("pruned and unpruned queries return the same rows", {
  conn <- local_conn()
  pruned <- local_fixture_curtain("place", conn = conn)
  withr::local_options(overturer_prune = FALSE)
  unpruned <- local_fixture_curtain("place", conn = conn)

  expect_equal(
    sort(dplyr::pull(pruned, id)),
    sort(dplyr::pull(unpruned, id))
  )
})

test_that("a filter touching no file returns zero rows quickly", {
  conn <- local_conn()
  antarctica <- c(xmin = 0, ymin = -89, xmax = 1, ymax = -88)
  buildings <- local_fixture_curtain(
    "building", antarctica, conn = conn, tablename = "empty"
  )

  expect_equal(dplyr::pull(dplyr::count(buildings), n), 0)
  expect_no_match(view_sql(conn, "empty"), "type=building/*", fixed = TRUE)
})

test_that("type = '*' reads every type in the theme", {
  conn <- local_conn()
  local_fixture_stac()
  base_url <- fixture_base_url()

  all_types <- open_curtain(
    "*", fixture_bbox, theme = "buildings", conn = conn, base_url = base_url,
    tablename = "star"
  )
  expect_equal(attr(all_types, "overture_playbill")[["type"]], "*")
  counts <- dplyr::collect(dplyr::count(all_types, type))
  expect_setequal(counts$type, c("building", "building_part"))

  sql <- view_sql(conn, "star")
  expect_match(sql, "fixture-building.zstd", fixed = TRUE)
  expect_match(sql, "fixture-building_part.zstd", fixed = TRUE)
  expect_match(sql, "union_by_name = CAST('t' AS BOOLEAN)", fixed = TRUE)
  # the two types have different columns; printing reads all of them
  expect_output(print(all_types), "theme buildings", fixed = TRUE)
  expect_s3_class(collect(head(all_types, 3)), "sf")

  # NULL behaves like "*"
  null_type <- open_curtain(
    NULL, fixture_bbox, theme = "buildings", conn = conn, base_url = base_url
  )
  expect_equal(
    dplyr::pull(dplyr::count(null_type), n), sum(counts$n)
  )

  # but needs a theme
  expect_error(
    open_curtain("*", fixture_bbox, conn = conn, base_url = base_url),
    "`theme` must be set"
  )
})

test_that("as_sf = TRUE collects straight to sf", {
  conn <- local_conn()
  buildings <- local_fixture_curtain("building", conn = conn, as_sf = TRUE)

  expect_s3_class(buildings, "sf")
  expect_false(inherits(buildings, "overture_call"))
  expect_equal(sf::st_crs(buildings), sf::st_crs(4326))
  expect_true(all(sf::st_is_valid(buildings)))
})

test_that("mode = 'table' materializes the data in duckdb", {
  conn <- local_conn()
  buildings <- local_fixture_curtain(
    "building", conn = conn, mode = "table", tablename = "materialized"
  )

  tables <- DBI::dbGetQuery(conn, "SELECT table_name FROM duckdb_tables()")
  expect_true("materialized" %in% tables$table_name)
  expect_gt(dplyr::pull(dplyr::count(buildings), n), 0)
})

test_that("read_opts are passed to read_parquet", {
  conn <- local_conn()
  buildings <- local_fixture_curtain(
    "building",
    conn = conn, read_opts = list(filename = TRUE), tablename = "opts"
  )

  expect_match(
    view_sql(conn, "opts"), "filename = CAST('t' AS BOOLEAN)",
    fixed = TRUE
  )
  expect_true("filename" %in% colnames(buildings))
})

test_that("the deprecated bbox argument still works, with a warning", {
  conn <- local_conn()
  local_fixture_stac()

  expect_warning(
    buildings <- open_curtain(
      "building",
      bbox = fixture_bbox, conn = conn, base_url = fixture_base_url()
    ),
    "`bbox` is deprecated"
  )
  expect_gt(dplyr::pull(dplyr::count(buildings), n), 0)
})

test_that("tablename defaults avoid clobbering an existing view", {
  conn <- local_conn()
  first <- local_fixture_curtain("building", conn = conn)
  second <- local_fixture_curtain("building", conn = conn)

  expect_equal(dbplyr::remote_name(first), "overtureR_building")
  expect_equal(dbplyr::remote_name(second), "overtureR_building1")
  expect_equal(cast_extra(conn, "buildings", "building"), "overtureR_building2")
  expect_equal(cast_extra(conn, "buildings", "*"), "overtureR_buildings")
})

test_that("an unnamed numeric bbox gives a clear error", {
  conn <- local_conn()
  local_fixture_stac()

  expect_error(
    open_curtain(
      "building", c(-74, 40.7, -73.9, 40.8),
      conn = conn, base_url = fixture_base_url()
    ),
    "names xmin, ymin, xmax, ymax"
  )
  expect_error(
    stage_bbox(conn, c(xmin = 1, ymin = 2, xmax = 3)),
    "names xmin, ymin, xmax, ymax"
  )
  expect_error(stage_bbox(conn, list()), "invalid `spatial_filter` object")
})

test_that("release is taken from the argument, the option, or the catalog", {
  conn <- local_conn()
  local_fixture_stac()
  # point the S3 path builder at the fixture tree
  releases_dir <- dirname(fixture_base_url())
  testthat::local_mocked_bindings(
    release_url = function(release) file.path(releases_dir, release)
  )

  by_default <- open_curtain("building", fixture_bbox, conn = conn)
  expect_equal(playbill(by_default)$release, fixture_release)
  expect_gt(dplyr::pull(dplyr::count(by_default), n), 0)

  by_arg <- open_curtain(
    "building", fixture_bbox, conn = conn, release = fixture_release
  )
  expect_equal(playbill(by_arg)$release, fixture_release)

  withr::local_options(overturer_release = fixture_release)
  by_option <- open_curtain("building", fixture_bbox, conn = conn)
  expect_equal(playbill(by_option)$release, fixture_release)

  # the option loses to an explicit argument
  expect_warning(
    expect_error(
      open_curtain(
        "building", fixture_bbox, conn = conn, release = "1999-01-01.0"
      )
    ),
    "Reading every file instead"
  )
})

test_that("release_url builds Overture's S3 path", {
  expect_equal(
    release_url("2026-08-19.0"),
    "s3://overturemaps-us-west-2/release/2026-08-19.0"
  )
})

test_that("print shows the release and type before the table", {
  conn <- local_conn()
  buildings <- local_fixture_curtain("building", conn = conn)

  expect_output(
    print(buildings), "# Overture release 2024-01-01.0, type building",
    fixed = TRUE
  )
  expect_output(print(buildings), "# Database:")
  capture.output(returned <- print(buildings))
  expect_identical(returned, buildings)

  all_types <- local_fixture_curtain("*", conn = conn, theme = "buildings")
  expect_output(print(all_types), "theme buildings", fixed = TRUE)

  plain <- dplyr::tbl(conn, dbplyr::remote_name(buildings))
  unknown <- as_overture(plain, "building")
  expect_output(print(unknown), "release unknown", fixed = TRUE)
})

# ---- spatial filters ---------------------------------------------------------

times_square <- function(crs = 4326) {
  poly <- sf::st_polygon(list(rbind(
    c(-73.9875, 40.7575), c(-73.9835, 40.7575), c(-73.9835, 40.7595),
    c(-73.9875, 40.7595), c(-73.9875, 40.7575)
  )))
  geometry <- sf::st_sfc(poly, crs = 4326)
  shape <- sf::st_sf(name = "times square", geometry = geometry)
  sf::st_transform(shape, crs)
}

# collect() the fixture buildings that intersect a spatial filter
fixture_buildings <- function(spatial_filter, conn, env = parent.frame()) {
  collect(
    local_fixture_curtain("building", spatial_filter, conn = conn, env = env)
  )
}

test_that("sf filters return the features that intersect the shape", {
  conn <- local_conn()
  shape <- times_square()
  buildings <- collect(local_fixture_curtain("building", shape, conn = conn))

  expect_s3_class(buildings, "sf")
  expect_gt(nrow(buildings), 0)
  expect_true(all(lengths(sf::st_intersects(buildings, shape)) > 0))

  # fewer than the bbox alone would return
  by_bbox <- local_fixture_curtain("building", sf::st_bbox(shape), conn = conn)
  expect_lt(nrow(buildings), dplyr::pull(dplyr::count(by_bbox), n))
})

test_that("a projected sf filter is transformed to EPSG:4326 first", {
  conn <- local_conn()
  utm <- times_square(32618)
  in_4326 <- fixture_buildings(times_square(), conn)
  in_utm <- fixture_buildings(utm, conn)
  as_sfc <- fixture_buildings(sf::st_geometry(utm), conn)
  as_bbox <- fixture_buildings(sf::st_bbox(utm), conn)

  expect_equal(sort(in_utm$id), sort(in_4326$id))
  expect_equal(sort(as_sfc$id), sort(in_4326$id))

  # a projected box is not axis-aligned in EPSG:4326, so its transformed box
  # is slightly larger than the original: it must cover the same rows and
  # not many more
  ids_4326 <- fixture_buildings(sf::st_bbox(times_square()), conn)$id
  ids_utm <- as_bbox$id
  expect_true(all(ids_4326 %in% ids_utm))
  expect_lt(length(ids_utm), length(ids_4326) * 1.5)
})

test_that("an sf filter without a CRS is assumed to be EPSG:4326, and warns", {
  conn <- local_conn()
  no_crs <- sf::st_set_crs(times_square(), NA)

  expect_warning(
    buildings <- local_fixture_curtain("building", no_crs, conn = conn),
    "assuming EPSG:4326"
  )
  expect_equal(
    sort(dplyr::pull(buildings, id)),
    sort(fixture_buildings(times_square(), conn)$id)
  )
})

test_that("sf filters leave one small table, not a registered data frame", {
  conn <- local_conn()
  local_fixture_curtain("building", times_square(), conn = conn)

  tables <- DBI::dbGetQuery(
    conn, "SELECT table_name, temporary FROM duckdb_tables()"
  )
  expect_equal(tables$table_name, "overtureR_spotlight")
  expect_true(tables$temporary)
  views <- DBI::dbGetQuery(
    conn, "SELECT view_name FROM duckdb_views() WHERE NOT internal"
  )
  expect_false(any(grepl("upload", views$view_name)))
  expect_equal(
    DBI::dbGetQuery(conn, "SELECT count(*) AS n FROM overtureR_spotlight")$n, 1
  )
})

test_that("a table name or a dbplyr table can be the spatial filter", {
  conn <- local_conn()
  shape <- times_square()
  by_sf <- fixture_buildings(shape, conn)

  shape_tbl <- sf_as_dbplyr(conn, "shape", shape)
  by_dbplyr <- fixture_buildings(shape_tbl, conn)
  by_name <- fixture_buildings("shape", conn)

  expect_equal(sort(by_dbplyr$id), sort(by_sf$id))
  expect_equal(sort(by_name$id), sort(by_sf$id))

  # one overture_call can filter another
  places <- local_fixture_curtain("place", shape, conn = conn)
  near_places <- local_fixture_curtain("building", places, conn = conn)
  expect_gt(dplyr::pull(dplyr::count(near_places), n), 0)
})

test_that("predicate = 'within' keeps only features inside the filter", {
  conn <- local_conn()
  shape <- times_square()
  intersecting <- fixture_buildings(shape, conn)
  within <- collect(
    local_fixture_curtain("building", shape, conn = conn, predicate = "within")
  )

  expect_gt(nrow(within), 0)
  expect_lt(nrow(within), nrow(intersecting))
  expect_true(all(within$id %in% intersecting$id))
  expect_true(all(lengths(sf::st_within(within, shape)) > 0))

  # a bbox filter gets the same treatment
  box <- sf::st_bbox(shape)
  within_box <- collect(
    local_fixture_curtain("building", box, conn = conn, predicate = "within")
  )
  expect_equal(sort(within_box$id), sort(within$id))
  local_fixture_curtain(
    "building", fixture_bbox, conn = conn, predicate = "within",
    tablename = "within_vec"
  )
  expect_match(view_sql(conn, "within_vec"), "st_within", ignore.case = TRUE)
  expect_match(
    view_sql(conn, "within_vec"), "st_makeenvelope", ignore.case = TRUE
  )
})

test_that("predicate = 'contains' finds the feature around a point", {
  conn <- local_conn()
  buildings <- collect(local_fixture_curtain("building", conn = conn))
  target <- buildings[which.max(sf::st_area(buildings)), ]
  point <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(target)))

  around <- collect(local_fixture_curtain(
    "building", point, conn = conn, predicate = "contains"
  ))
  expect_true(target$id %in% around$id)
  expect_true(all(lengths(sf::st_contains(around, point)) > 0))
  expect_lt(nrow(around), nrow(buildings))

  expect_error(
    local_fixture_curtain("building", point, conn = conn, predicate = "near"),
    "`predicate` must be one of"
  )
})

# ---- collect -----------------------------------------------------------------

test_that("collect converts geometry to sf without going through GDAL", {
  conn <- local_conn()
  places <- local_fixture_curtain("place", conn = conn)

  expect_silent(collected <- collect(head(places, 5)))
  expect_s3_class(collected, "sf")
  expect_equal(nrow(collected), 5)
  expect_equal(sf::st_crs(collected), sf::st_crs(4326))
  expect_equal(as.character(unique(sf::st_geometry_type(collected))), "POINT")

  in_3857 <- collect(head(places, 5), crs = 3857)
  expect_equal(sf::st_crs(in_3857), sf::st_crs(3857))
})

test_that("collect handles a geometry column that is already WKB", {
  conn <- local_conn()
  buildings <- local_fixture_curtain("building", conn = conn)

  converted <- dplyr::mutate(head(buildings, 3), geometry = ST_AsWKB(geometry))
  collected <- collect(converted)
  expect_s3_class(collected, "sf")
  expect_equal(nrow(collected), 3)
})

test_that("collect leaves a non-geometry column named geometry alone", {
  conn <- local_conn()
  buildings <- local_fixture_curtain("building", conn = conn)

  plain <- collect(dplyr::transmute(head(buildings, 3), id, geometry = id))
  expect_false(inherits(plain, "sf"))
  expect_type(plain$geometry, "character")

  no_geom <- collect(dplyr::select(head(buildings, 3), id))
  expect_false(inherits(no_geom, "sf"))
})

test_that("collect of zero rows is still an sf object", {
  conn <- local_conn()
  buildings <- local_fixture_curtain("building", conn = conn)

  empty <- collect(dplyr::filter(buildings, id == "no such id"))
  expect_s3_class(empty, "sf")
  expect_equal(nrow(empty), 0)
})

test_that("collect_sf is deprecated but works", {
  conn <- local_conn()
  buildings <- local_fixture_curtain("building", conn = conn)

  expect_warning(collected <- collect_sf(head(buildings, 2)), "deprecated")
  expect_s3_class(collected, "sf")
})

# ---- as_overture -------------------------------------------------------------

test_that("as_overture wraps a tbl_sql and rejects anything else", {
  conn <- local_conn()
  local_fixture_curtain("building", conn = conn, tablename = "plain")
  plain <- dplyr::tbl(conn, "plain")
  expect_false(inherits(plain, "overture_call"))

  wrapped <- as_overture(plain, "building")
  expect_equal(class(wrapped)[[1]], "overture_call")
  expect_equal(attr(wrapped, "overture_playbill")[["theme"]], "buildings")
  expect_identical(as_overture(wrapped, "building"), wrapped)
  expect_s3_class(collect(head(wrapped, 2)), "sf")
  expect_error(as_overture(mtcars), "Input must be a tbl_sql object")
})
