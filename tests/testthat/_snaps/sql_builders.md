# set_stage_boundary renders the bbox clause

    Code
      cat(set_stage_boundary(conn, bbox_vector))
    Output
      AND bbox.xmax >= -120.5
      AND bbox.xmin <= -120
      AND bbox.ymax >= 35.5
      AND bbox.ymin <= 36

---

    Code
      cat(set_stage_boundary(conn, bbox_vector + 1e-12))
    Output
      AND bbox.xmax >= -120.5
      AND bbox.xmin <= -120
      AND bbox.ymax >= 35.5
      AND bbox.ymin <= 36

# focus_spotlight renders the geometry clause for each filter kind

    Code
      cat(focus_spotlight(conn, sf_obj))
    Output
      AND ST_Intersects(master.geometry, (SELECT geometry FROM overtureR_spotlight))

---

    Code
      cat(focus_spotlight(conn, sf::st_geometry(sf_obj)))
    Output
      AND ST_Intersects(master.geometry, (SELECT geometry FROM overtureR_spotlight1))

---

    Code
      cat(focus_spotlight(conn, "pts"))
    Output
      AND ST_Intersects(master.geometry, (SELECT ST_Union_Agg(geometry) AS geometry FROM pts))

---

    Code
      cat(focus_spotlight(conn, dplyr::tbl(conn, "pts")))
    Output
      AND ST_Intersects(master.geometry, (SELECT ST_Union_Agg(geometry) AS geometry FROM (SELECT *
      FROM pts)))

# spotlight_files chooses between a file list and the wildcard

    Code
      files(release, "buildings", "building", NULL)
    Output
      's3://bucket/release/2024-01-01.0/theme=buildings/type=building/*'

---

    Code
      files("C:/local/copy", "buildings", "building", fixture_bbox)
    Output
      'C:/local/copy/theme=buildings/type=building/*'

---

    Code
      files(release, "buildings", "building", fixture_bbox)
    Output
      's3://bucket/release/2024-01-01.0/theme=buildings/type=building/*'

---

    Code
      files(release, "buildings", "building", fixture_bbox)
    Output
      ['s3://bucket/release/2024-01-01.0/theme=buildings/type=building/part-00000-fixture-building.zstd.parquet']

---

    Code
      files(release, "buildings", "building", paris)
    Output
      ['s3://bucket/release/2024-01-01.0/theme=buildings/type=building/part-00000-fixture-building.zstd.parquet', 's3://bucket/release/2024-01-01.0/theme=buildings/type=building/part-00001-fake-europe.zstd.parquet']

---

    Code
      files(release, "buildings", "*", fixture_bbox)
    Output
      ['s3://bucket/release/2024-01-01.0/theme=buildings/type=building/part-00000-fixture-building.zstd.parquet', 's3://bucket/release/2024-01-01.0/theme=buildings/type=building_part/part-00000-fixture-building_part.zstd.parquet']

---

    Code
      files(release, "castles", "*", fixture_bbox)
    Output
      's3://bucket/release/2024-01-01.0/theme=castles/type=*/*'

