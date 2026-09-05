# overtureR work plan

Written 2026-09-05 for overtureR 0.2.6. Measured with duckdb R 1.5.2, dbplyr 2.5.2,
sf 1.1.0, and Overture release 2026-08-19.0.

## Summary

1. ~~Read only the files that touch the filter area, using Overture's STAC catalog. Cold query 87 s to 2 s.~~ Done, branch `0.3`.
2. ~~Upload `sf` filters as binary (14 s to 0.05 s) and transform them to EPSG:4326.~~ Done.
3. ~~Fix the known bugs.~~ Done.
4. Let users pin a release.
5. ~~Let the type list update itself; export `overture_types()`.~~ Done.
6. ~~Cut the 5.6 s fixed cost of `open_curtain()`.~~ Done.
7. ~~Rebuild the test suite: offline fixtures, SQL builder tests, a regression test per bug.~~ Done.
8. Improve `record_overture()`: custom partitions, one-call download, local manifest.
9. Write the performance and mapping articles; refresh docs and the skill.
10. Add a `predicate` argument.
11. Raise the minimum duckdb version to 1.1.0.
12. ~~Drop PMTiles and alpha or beta datasets from the roadmap.~~ Done.
13. Feature ideas to consider: boundary lookup, category taxonomy, in-database simplify, spatial join, transparent cache, GDAL export, attribution helper.

Items 1, 2, 3, 5, 6, 7, and 12 shipped together on branch `0.3` (2026-09-05) as version 0.3.0,
rather than as a 0.2.7 patch plus 0.3.0. Next: 4, then 8, 9, 10, 11.

## Terms used in this plan

- **Release.** One monthly snapshot of Overture data. Each release is a fixed set of
  Parquet files on S3 that never changes after publication.
- **Footer.** The metadata block at the end of a Parquet file. DuckDB must read it to learn
  what rows a file holds.
- **Cold query.** The first query of an R session. Nothing is cached, so DuckDB reads every
  footer. A **warm query** repeats a cold one in the same session and reuses the cache.
- **Manifest.** A table with one row per Parquet file, giving the file's URL and the
  bounding box of the geometry it contains.
- **STAC.** Overture's public catalog at `stac.overturemaps.org`. It lists every release,
  every theme and type in each release, and one entry per Parquet file with that file's
  bounding box.

## What did the tests show?

| Test                                                                     | Result               |
| ------------------------------------------------------------------------ | -------------------- |
| Cold `count(*)` on `building` for a small bbox (reads 512 files)         | 87 s                 |
| Same query, warm                                                         | 2.5 to 4 s           |
| Same cold query limited to the 1 file whose bbox intersects              | 2.2 s                |
| Building a manifest by reading every footer                              | 54 s                 |
| Building the same manifest from STAC in one `read_json` call             | 3.3 s                |
| `sf_as_dbplyr()` uploading 5,000 polygons as WKT text (current code)     | 13.9 s               |
| Same upload as WKB binary                                                | 0.05 s               |
| Fixed cost of one `open_curtain()` call before any data is read          | 5.6 s                |
| Of that, `dplyr::tbl()` re-reading the remote schema                     | 2.5 s                |
| Spatial filter with 6 polygons, current query versus an `EXISTS` rewrite | 1.35 s versus 1.07 s |
| Types in the live release that `type_theme_map` does not know            | `bathymetry`         |

The cold query is the package's biggest performance problem. You can fix it with data that
Overture already publishes. Everything else is smaller.

## What should you build, in order?

### 1. Read only the files that touch the filter area (done)

Done 2026-09-05 in `R/stac.R` and `open_curtain()`; cold building query 87 s to 6.5 s end to end.

### 2. Upload `sf` filters as binary, and fix the coordinate system (done)

Done 2026-09-05; a filter with no CRS warns and assumes EPSG:4326 rather than erroring.

### 3. Fix the known bugs (done)

Done 2026-09-05; every bug fixed with a regression test, pull request 3 closed.

### 4. Let users pin a release

**What to do.** Add a `release` argument to `open_curtain()`, defaulting to
`latest_overture_release()`, and build `base_url` from it. Add an option
`overturer_release` to set it once per session. Export `overture_releases()` to list the
releases Overture still hosts.

**Why.** Version 0.2.6 made "latest" the default. That keeps data fresh, but the same script
now returns different rows from month to month, and nothing records which release produced a
result. Reproducible work needs a way to pin one. The STAC root already lists every live
release, so listing them is one JSON read.

**How.** Store `release` in the `overture_playbill` attribute so `record_overture()` and
printed output can report it. Add a `print.overture_call()` method that starts with a line
such as "Overture release 2026-08-19.0, type building".

**Effort.** Small to medium, about one day. Needs the STAC code from item 1.

### 5. Let the type list update itself (done)

Done 2026-09-05; `overture_types()` exported, unknown types list the valid ones.

### 6. Cut the fixed cost of `open_curtain()` (done)

Done 2026-09-05; cache settings on, and `vars =` was unneeded once pruning landed (`tbl()` 0.02 s).

### 7. Rebuild the test suite (done)

Done 2026-09-05; 7 files, 292 expectations, 3 live tests on a weekly schedule, fixtures 381 KB.

### 8. Improve `record_overture()`

- **Custom partitions.** Accept `partition_by =` in addition to the fixed `(theme, type)`.
  Offer a grid option, such as partitioning on `floor(bbox.xmin)` and `floor(bbox.ymin)`, so
  large local copies also skip files by location. Remove the `PARTITION_BY` rejection in
  `process_write_opts()`.
- **Download in one call.** Accept `record_overture(type, spatial_filter, output_dir)` so
  users need not call `open_curtain()` first.
- **Write a local manifest** next to the Parquet files, in the same shape as item 1, so the
  returned `overture_call` can skip files locally too.
- **Quote the path** in `COPY ... TO '<dir>'`. A `'` in `output_dir` breaks the statement.
- **Record the source release** in a small `_overture.json` file so users know where the
  data came from.

**Effort.** Medium. Needs items 1 and 4.

### 9. Write the articles and refresh the docs

- A **performance article** built from the appendix numbers. Cover: filter before you
  collect, why `spatial_filter` matters, warm versus cold queries, when to use
  `record_overture()`, and how to pin a release.
- A **mapping article** using `mapgl` or `ggplot2` on a saved local copy, so it knits offline.
- Update the README roadmap, the getting-started article, and `inst/skills/overturer/` to
  cover `release =`, `overture_types()`, `overture_releases()`, and file skipping.
- Add `bathymetry` to `references/data-model.md`.

**Effort.** Small to medium. Do it after items 1 to 5 so the docs describe the new API.

### 10. Add a `predicate` argument

**What to do.** Add `open_curtain(..., predicate = c("intersects", "within", "contains"))`.

**Why.** Intersects is the only option today. Many users mean "within" when they filter by a
polygon, such as buildings fully inside a boundary. This is a switch on the `ST_*` function
name.

**Do not** rewrite the filter as an `EXISTS` semi-join or an inner spatial join. With 6
polygons the semi-join saved only 0.3 s and the inner join was slower. The work is bound by
network reads, not by the geometry test. Revisit only if a user reports a slow filter with
many polygons.

**Effort.** Small.

### 11. Raise the minimum duckdb version

Set `duckdb (>= 1.1.0)` in DESCRIPTION. Delete the pre-1.1 WKB conversion branches and the
duckdb 1.1.3 `core_nightly` workaround in `config_extensions()`. Fewer code paths means fewer
tests. Do this with the next minor release, after you confirm CRAN has duckdb binaries for
every platform.

### 12. Drop or defer these roadmap items (done)

Done 2026-09-05; README roadmap rewritten.

### 13. Feature ideas to consider

These stay within the package's job: get Overture data into R through DuckDB as lazy tables
or `sf`. None is measured or scoped. The starred ones look most useful relative to effort.

**Finding an area to filter by**

- * `division_area("Philadelphia", country = "US")` looks up a boundary by name and subtype
  and returns it as an `sf` or lazy table you can pass straight to `spatial_filter`. Most
  users start by hunting for a boundary; today that takes a manual filter on `division_area`.
- `division_hierarchy(id)` walks `parent_division_id` up to the country, or lists children.
- `spatial_filter` as a point plus a radius in meters, using `ST_DWithin_Spheroid` in
  DuckDB.

**Working with nested columns**

- `flatten_overture(x)` expands the common structs (`names$primary`, `bbox$*`,
  `categories$primary`, first `sources` record) into flat columns before `collect()`.
- `pluck_name(x, lang = "es")` pulls a localized name from `names$common`.
- * `overture_categories()` ships the places category taxonomy, and
  `filter_category(x, "food_and_drink")` matches a parent category and all its children.
  Overture's categories are a tree, and users filter on the wrong level constantly.
- `overture_schema(type)` returns column names and DuckDB types for a type, cached per
  release. Helps users and AI agents without a `collect()`.

**Doing more in the database before download**

- * `collect(x, simplify = 0.0001)` runs `ST_Simplify` in DuckDB, and `collect(x, crs = )`
  runs `ST_Transform` there. Land, water, and division polygons are large; simplifying before
  transfer could cut download time several-fold. Measure first.
- `collect(x, geometry = "centroid")` or `"bbox"` returns points instead of full geometry.
  Faster for plotting places or counting buildings.
- * `spotlight_join(x, y, predicate = "intersects")` does a lazy spatial join between two
  lazy tables, such as buildings to places or segments to divisions, inside DuckDB. Today
  users must write the `ST_Intersects` SQL themselves.
- Document which `sf` verbs pass through to DuckDB inside `mutate()` (`ST_Area_Spheroid`,
  `ST_Length_Spheroid`, `ST_Centroid`), and add a small translation table so
  `sf::st_area()` inside `mutate()` becomes the DuckDB call.
- Warn, or ask, before a `collect()` with no spatial filter and no row limit.

**Caching and local copies**

- * Transparent local cache. `open_curtain(..., cache = TRUE)` records the query result to
  `tools::R_user_dir("overtureR", "cache")` on first `collect()` and reads from disk after
  that, keyed on release, type, and filter bbox. `record_overture()` already does the hard
  part; this removes the manual step.
- `list_recordings()` and `open_recording(name)` to manage named local copies with their
  release and bbox recorded in a small index.
- Tiled downloads. `record_overture()` splits a large area into a grid and downloads tile by
  tile with a progress bar, so one failed request does not lose an hour of work. This is the
  roadmap's "chunking" item.

**Getting data out**

- * `export_overture(x, "out.gpkg")` writes GeoPackage, FlatGeobuf, or GeoJSON straight
  from DuckDB with `COPY ... TO (FORMAT GDAL)`. No R memory, no `sf` round trip.
- `collect(x, as = "tibble")` skips the `sf` conversion for users who only want attributes.
- Return `arrow` or `duckplyr` frames for users staying in the DuckDB ecosystem, and accept
  `duckplyr` frames as `spatial_filter`.

**Places, addresses, and transportation**

- `search_places("Kennedy Center", spatial_filter)` does fuzzy name matching in DuckDB
  with `jaro_winkler_similarity` or `ILIKE`.
- `geocode_overture(addresses)` matches free-text addresses against the addresses theme;
  `reverse_geocode(points)` finds the nearest address or place.
- `road_network(spatial_filter)` joins segments to connectors and returns an object
  `sfnetworks` can use for routing.

**Tracking features across releases**

- `find_by_gers(id)` looks one feature up by its stable id across all types.
- `compare_releases(type, spatial_filter, releases)` reports added, removed, and changed
  features by id between two releases. Check first whether Overture publishes a changelog
  dataset that does the diff for you.

**Publishing**

- * `overture_attribution(x)` returns the license and attribution text for the themes in a
  query. Users publishing maps need it and rarely know where to look.
- `print.overture_call()` shows release, type, filter bbox, and an estimated row count.
- `plot()` or `mapview()` method that samples a few hundred rows for a quick look.

## In what order should you ship?

1. **Version 0.2.7, a patch, one to two days.** Items 3 and 2, plus the saved test data
   from item 7. All are small and low risk, and several fix silent wrong answers.
2. **Version 0.3.0, a minor release, one to two weeks.** Items 1, 4, 5, and 6 on one branch,
   because they share the same catalog reads and cache. Then item 8, then item 9, then items
   10 and 11.

## Appendix: how the tests were run

All times are wall-clock `system.time()` on Windows 11, R 4.5.0, duckdb R 1.5.2 (engine
v1.5.2), against `s3://overturemaps-us-west-2/release/2026-08-19.0`. Each test ran in a
fresh R process unless marked warm. The test bbox was `xmin=-120.5, ymin=35.5,
xmax=-120.0, ymax=36.0`, which holds 3,310 buildings. The scripts lived in the session
scratchpad. They are short enough to recreate from the descriptions above.

- **Cold versus warm.** The warm speedup comes from DuckDB's in-process external file cache,
  on by default since 1.3 and keyed on the exact file path. It does not survive the R session.
  That is why an on-disk manifest matters.
- **Manifest from footers.** `parquet_kv_metadata()` on the wildcard path, decoding the
  `geo` key's `columns.geometry.bbox`.
- **Manifest from STAC.** `read_json` on the collection's item URLs, selecting `bbox[1..4]`
  and `assets.aws.href`. Both methods found exactly 1 file for the test bbox.
- **Text versus binary upload.** 5,000 polygons, made by repeating the `nc` shapefile 50
  times.
- **`open_curtain()` breakdown.** `glob()` listing 0.5 to 0.7 s, `DESCRIBE read_parquet`
  2.1 s, `CREATE VIEW` 0.5 s, `dplyr::tbl()` 2.5 s (1.0 s with `vars =`), `as_overture()`
  0.02 s.
