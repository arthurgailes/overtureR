# The pieces of SQL that open_curtain() assembles, tested one at a time
# without a network. Snapshots make any change to the query shape visible.

bbox_vector <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)

test_that("audition_data recognises each kind of spatial filter", {
  conn <- local_conn()
  sf_obj <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326))
  duckdb::duckdb_register(conn, "some_table", data.frame(x = 1))

  expect_null(audition_data(NULL))
  expect_equal(audition_data(bbox_vector), "bbox_vector")
  expect_equal(audition_data(sf::st_bbox(sf_obj)), "bbox")
  expect_equal(audition_data(sf_obj), "sf")
  expect_equal(audition_data(sf::st_geometry(sf_obj)), "sf")
  expect_equal(audition_data(sf::st_geometry(sf_obj)[[1]]), "sf")
  expect_equal(audition_data("some_table"), "tablename")
  expect_equal(audition_data(dplyr::tbl(conn, "some_table")), "dbplyr")
  expect_error(audition_data(list()), "invalid `spatial_filter` object")
  expect_error(audition_data(mtcars), "invalid `spatial_filter` object")
})

test_that("stage_bbox returns the same EPSG:4326 box for every filter kind", {
  conn <- local_conn()
  box <- sf::st_bbox(bbox_vector, crs = sf::st_crs(4326))
  sf_obj <- sf::st_as_sf(sf::st_as_sfc(box))

  expect_null(stage_bbox(conn, NULL))
  expect_equal(stage_bbox(conn, bbox_vector), bbox_vector)
  shuffled <- bbox_vector[c("ymax", "xmin", "xmax", "ymin")]
  expect_equal(stage_bbox(conn, shuffled), bbox_vector)
  expect_equal(stage_bbox(conn, sf::st_bbox(sf_obj)), bbox_vector)
  expect_equal(stage_bbox(conn, sf_obj), bbox_vector)
  expect_equal(stage_bbox(conn, sf::st_geometry(sf_obj)), bbox_vector)

  projected <- sf::st_transform(sf_obj, 3857)
  expect_equal(stage_bbox(conn, projected), bbox_vector, tolerance = 1e-6)
  expect_equal(
    stage_bbox(conn, sf::st_bbox(projected)), bbox_vector,
    tolerance = 1e-6
  )

  sf_as_dbplyr(conn, "box", sf_obj)
  expect_equal(stage_bbox(conn, "box"), bbox_vector)
  expect_equal(stage_bbox(conn, dplyr::tbl(conn, "box")), bbox_vector)
})

test_that("set_stage_boundary renders the bbox clause", {
  conn <- local_conn()

  expect_equal(set_stage_boundary(conn, NULL), "")
  expect_snapshot(cat(set_stage_boundary(conn, bbox_vector)))
  # values are rounded to 10 decimals so the SQL stays short and stable
  expect_snapshot(cat(set_stage_boundary(conn, bbox_vector + 1e-12)))
})

test_that("focus_spotlight renders the geometry clause for each filter kind", {
  conn <- local_conn()
  points <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
  sf_obj <- sf::st_sf(geometry = points)

  expect_equal(focus_spotlight(conn, NULL), "")
  expect_equal(focus_spotlight(conn, bbox_vector), "")
  expect_equal(focus_spotlight(conn, sf::st_bbox(sf_obj)), "")

  expect_snapshot(cat(focus_spotlight(conn, sf_obj)))
  # a second sf filter gets its own table
  expect_snapshot(cat(focus_spotlight(conn, sf::st_geometry(sf_obj))))

  pts <- data.frame(geometry = c("POINT(0 0)", "POINT(1 1)"))
  duckdb::duckdb_register(conn, "pts", pts)
  expect_snapshot(cat(focus_spotlight(conn, "pts")))
  expect_snapshot(cat(focus_spotlight(conn, dplyr::tbl(conn, "pts"))))
})

test_that("focus_spotlight switches the predicate function", {
  conn <- local_conn()
  points <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
  sf_obj <- sf::st_sf(geometry = points)

  # a bbox needs a geometry test for anything but intersects
  expect_snapshot(cat(focus_spotlight(conn, bbox_vector, "within")))
  expect_snapshot(cat(focus_spotlight(conn, sf::st_bbox(sf_obj), "contains")))
  expect_snapshot(cat(focus_spotlight(conn, sf_obj, "within")))
  expect_snapshot(cat(focus_spotlight(conn, sf_obj, "CONTAINS")))

  expect_error(focus_spotlight(conn, sf_obj, "touches"), "must be one of")
  expect_error(match_predicate(c("within", "contains")), "single string")
  expect_error(match_predicate(1), "single string")
})

test_that("focus_spotlight rejects filters it can't use", {
  conn <- local_conn()
  duckdb::duckdb_register(conn, "no_geom", data.frame(x = 1, y = 2))

  expect_error(focus_spotlight(conn, list()), "invalid `spatial_filter` object")
  expect_error(
    focus_spotlight(conn, dplyr::tbl(conn, "no_geom")),
    "`spatial_filter` must have a column 'geometry' of class GEOMETRY"
  )
  expect_error(
    focus_spotlight(conn, "not a table!"),
    "if a string, `spatial_filter` must be a table in the connection"
  )
  expect_error(
    focus_spotlight(conn, c("a", "b")),
    "if a string, `spatial_filter` must be a table in the connection"
  )
})

test_that("spotlight_files chooses between a file list and the wildcard", {
  local_fixture_stac()
  conn <- local_conn()
  release <- "s3://bucket/release/2024-01-01.0"
  files <- function(...) cat(spotlight_files(conn, ...))

  # no filter, or not a release: wildcard
  expect_snapshot(files(release, "buildings", "building", NULL))
  expect_snapshot(files("C:/local/copy", "buildings", "building", fixture_bbox))
  withr::with_options(list(overturer_prune = FALSE), {
    expect_snapshot(files(release, "buildings", "building", fixture_bbox))
  })

  # a release and a filter: only the touching files
  expect_snapshot(files(release, "buildings", "building", fixture_bbox))
  paris <- c(xmin = 2.2, ymin = 48.8, xmax = 2.4, ymax = 48.9)
  expect_snapshot(files(release, "buildings", "building", paris))
  expect_snapshot(files(release, "buildings", "*", fixture_bbox))

  # a theme the catalog doesn't know, or a type without a manifest: wildcard
  expect_snapshot(files(release, "castles", "*", fixture_bbox))
  expect_warning(
    castle <- spotlight_files(
      conn, release, "buildings", "castle", fixture_bbox
    )
  )
  expect_equal(castle, paste0("'", release, "/theme=buildings/type=castle/*'"))
})

test_that("process_parquet_read_opts merges user options over the defaults", {
  expect_equal(
    process_parquet_read_opts(list()),
    "filename=FALSE, hive_partitioning=TRUE, union_by_name=FALSE"
  )
  expect_equal(
    process_parquet_read_opts(list(), union_by_name = TRUE),
    "filename=FALSE, hive_partitioning=TRUE, union_by_name=TRUE"
  )
  custom <- list(hive_partitioning = FALSE, binary_as_string = TRUE)
  expect_equal(
    process_parquet_read_opts(custom),
    paste0(
      "filename=FALSE, hive_partitioning=FALSE, union_by_name=FALSE, ",
      "binary_as_string=TRUE"
    )
  )
})

test_that("process_write_opts adds the partition and rejects overrides", {
  expect_equal(
    process_write_opts(NULL, FALSE),
    "PARTITION_BY (theme, type), FILENAME_PATTERN 'data_{uuid}'"
  )
  expect_equal(
    process_write_opts(NULL, TRUE),
    paste0(
      "PARTITION_BY (theme, type), FILENAME_PATTERN 'data_{uuid}', ",
      "OVERWRITE_OR_IGNORE"
    )
  )
  expect_equal(
    process_write_opts("ROW_GROUP_SIZE 100000", TRUE),
    paste0(
      "ROW_GROUP_SIZE 100000, PARTITION_BY (theme, type), ",
      "FILENAME_PATTERN 'data_{uuid}', OVERWRITE_OR_IGNORE"
    )
  )
  expect_equal(
    process_write_opts("FILENAME_PATTERN 'part_{i}'", FALSE),
    "FILENAME_PATTERN 'part_{i}', PARTITION_BY (theme, type)"
  )
  expect_equal(
    process_write_opts(NULL, FALSE, c("theme", "type", "x_cell")),
    "PARTITION_BY (theme, type, x_cell), FILENAME_PATTERN 'data_{uuid}'"
  )
  expect_error(process_write_opts("OVERWRITE", TRUE), "`overwrite` argument")
  expect_error(
    process_write_opts("partition_by (x)", TRUE), "`partition_by` argument"
  )
})

test_that("cast_extra numbers table names until one is free", {
  conn <- local_conn()

  expect_equal(cast_extra(conn, "buildings", "building"), "overtureR_building")
  expect_equal(cast_extra(conn, "buildings", "*"), "overtureR_buildings")
  expect_equal(cast_extra(conn, "buildings", NULL), "overtureR_buildings")

  duckdb::duckdb_register(conn, "overtureR_building", data.frame(x = 1))
  expect_equal(cast_extra(conn, "buildings", "building"), "overtureR_building1")
})

test_that("sf_as_dbplyr registers geometry as WKB and casts it to GEOMETRY", {
  conn <- local_conn()
  points <- sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)), crs = 4326)
  sf_obj <- sf::st_sf(a = c(3, 4), geometry = points)

  full <- sf_as_dbplyr(conn, "full_tbl", sf_obj)
  expect_s3_class(full, "tbl_duckdb_connection")
  expect_equal(sort(dplyr::pull(full, a)), c(3, 4))
  expect_equal(colnames(full), c("a", "geometry"))
  types <- DBI::dbGetQuery(conn, "DESCRIBE full_tbl")
  expect_match(types$column_type[types$column_name == "geometry"], "^GEOMETRY")
  init <- DBI::dbGetQuery(conn, "DESCRIBE full_tbl_init")
  expect_equal(init$column_type[init$column_name == "geometry"], "BLOB")

  # geometry only, from an sf or an sfc
  geom <- sf_as_dbplyr(conn, "geom", sf_obj, geom_only = TRUE)
  expect_equal(colnames(geom), "geometry")
  sfc <- sf_as_dbplyr(conn, "sfc", sf::st_geometry(sf_obj), geom_only = FALSE)
  expect_equal(colnames(sfc), "geometry")

  # the geometry survives the round trip exactly
  back <- DBI::dbGetQuery(
    conn, "SELECT ST_AsText(geometry) AS wkt FROM full_tbl ORDER BY a"
  )
  expect_equal(back$wkt, c("POINT (1 2)", "POINT (3 4)"))

  sf_as_dbplyr(conn, "dup", sf_obj)
  expect_error(sf_as_dbplyr(conn, "dup", sf_obj))
  expect_no_error(sf_as_dbplyr(conn, "dup", sf_obj, overwrite = TRUE))
  expect_error(sf_as_dbplyr("not a connection", "x", sf_obj), "only supports")
})
