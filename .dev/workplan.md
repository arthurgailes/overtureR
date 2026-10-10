# overtureR work plan

Written 2026-09-05 for overtureR 0.2.6. Measured with duckdb R 1.5.2, dbplyr 2.5.2,
sf 1.1.0, and Overture release 2026-08-19.0.

## Summary

Items 1 to 8 and 10 to 12 of the original plan shipped on 2026-09-05: STAC file pruning,
binary `sf` uploads, bug fixes, release pinning, a self-updating type list, the lower fixed
cost of `open_curtain()`, the offline test suite, the `record_overture()` improvements, the
`predicate` argument, duckdb 1.1.0 as the floor, the `st_bbox()` and `st_crs()` methods
copied from duckspatial, and the trimmed roadmap. Versions 0.2.6 and 0.3.0 never reached
CRAN. All of it, plus the AI-assisted queries article, reached CRAN as 0.3.1 on 2026-10-08.

What remains:

1. Write the performance and mapping articles; refresh docs and the skill.
2. Feature ideas to consider, including duckspatial interoperability.
3. One deferred duckspatial idea: benchmark `wk` for WKB parsing before switching to it.

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

### 1. Write the articles and refresh the docs

- A **performance article** built from the appendix numbers. Cover: filter before you
  collect, why `spatial_filter` matters, warm versus cold queries, when to use
  `record_overture()`, and how to pin a release.
- A **mapping article** using `mapgl` or `ggplot2` on a saved local copy, so it knits offline.
- Update the getting-started article to cover `release =`, `predicate =`,
  `overture_types()`, `overture_releases()`, file skipping, and the `record_overture()`
  manifest and `grid`. The README roadmap and the skill were updated on 2026-09-05.
- Add `bathymetry` to `references/data-model.md`.

**Effort.** Small to medium. The API it documents is now in place.

### 2. Feature ideas to consider

These stay within the package's job: get Overture data into R through DuckDB as lazy tables
or `sf`. None is measured or scoped. The starred ones look most useful relative to effort.

**Finding an area to filter by**

- * `call_place("San Francisco", country = "US")` looks up an Overture division by name and
  returns its boundary as a lazy `overture_call` you pass straight to `spatial_filter` (or
  `collect()` to an `sf` polygon). This is the missing step behind natural-language queries:
  `open_curtain()` takes only a spatial filter, never a name, so "buildings in San Francisco"
  needs a name-to-geometry resolver. `call_place()` resolves the name eagerly against the
  `division` type (`names$primary ILIKE name`, narrowed by `country` and `subtype`), prints
  "Found N candidates; returning the best: ...", and hands back the matching `division_area`
  polygon. It returns one best match by default and all matches with `multiple = TRUE`
  (default from `getOption("overturer_division_multiple", FALSE)`). The return type is always
  a lazy `overture_call`, whether 1 row or N. Name resolution is a non-spatial scan of the
  global `division` partition, so STAC bbox pruning cannot help it; a spike must confirm the
  cold cost (cache like the catalog if slow) and which fields exist to rank "best". Pair it
  with a vignette wiring ellmer + btw + the bundled skill into a natural-language loop over
  `call_place()`; ellmer and btw go in Suggests, not Imports. Named 2026-09-14; supersedes the
  earlier `division_area()` working name, which collided with the Overture type string.
  - *Spike, 2026-09-14 (release 2026-08-19.0):* a cold exact-name scan of `division` took
    about 19 seconds, so `call_place()` must cache the name-to-id result on disk like the
    STAC catalog. `"San Francisco"` matched 766 divisions worldwide (541 localities, 160
    neighborhoods, 9 counties; countries led by MX, PH, EC), so narrowing and ranking are
    required, not optional. `population` is the one strong prominence signal: San Francisco,
    US-CA carries 873,965 while most of the 766 have none. So the best-match rule is: apply
    the `country`, `subtype` and `region` filters, order by population descending with nulls
    last, then break ties by a subtype priority (locality and county above neighborhood).
    `population` lives on `division`, not `division_area`, so resolve and rank on `division`,
    take the winner's id and bbox, then fetch the `division_area` polygon where
    `division_id` matches, passing that bbox as the spatial filter so STAC pruning speeds the
    second fetch. Cache both steps.
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

- * **Interoperate with duckspatial** (GitHub `Cidree/duckspatial`, 1.2.1 on 2026-09-05).
  Its `as_duckspatial_df()` accepts any lazy dbplyr table, detects a `geometry` column, and
  reads the CRS from DuckDB 1.5's typed `GEOMETRY`. An `open_curtain()` result should convert
  in one call and gain its lazy verbs: simplify, transform, centroid, buffer, spatial join,
  predicate matrices, areal-weighted interpolation, GDAL export, MBTiles output. Add it to
  Suggests and write a short article, such as buildings per tract with
  `ddbs_interpolate_aw()`. Verify the round trip first; it needs duckdb >= 1.5.4.2. Warn in
  the article that converting a table with no remote name, such as after `filter()`, runs
  `CREATE TEMP TABLE` on the query, so convert after the spatial filter, never on a whole
  theme. Do not import it: it pulls in arrow, geoarrow, nanoarrow, wk, units, uuid, cli,
  lifecycle, and tibble.
- Warn, or ask, before a `collect()` with no spatial filter and no row limit.

*Pruned 2026-09-05, because duckspatial already does them on a lazy table:* simplify and
transform inside `collect()`, centroid or bbox geometry output, `spotlight_join()`, and the
`sf`-verb-to-`ST_*` translation table.

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

- `collect(x, as = "tibble")` skips the `sf` conversion for users who only want attributes.
- Accept `duckplyr` frames as `spatial_filter`.

*Pruned 2026-09-05, because duckspatial already does them:* `export_overture()` (its
`ddbs_write_vector()` runs `COPY ... TO (FORMAT GDAL)`) and arrow or geoarrow output (its
`collect(as = "geoarrow")`).

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

### 3. Copy small ideas from duckspatial

Each is a few lines, adds no dependency, and makes the lazy object behave more like `sf`.
Source: duckspatial 1.2.1, `R/duckspatial_df_sf_methods.R` and
`R/duckspatial_df_dplyr_methods.R`.

Shipped in 0.3.0:

- **`st_bbox.overture_call()`** runs `ST_Extent_Agg(geometry)` in DuckDB and returns an `sf`
  bbox in the table's CRS, so users get an extent without a `collect()`.
- **`st_crs.overture_call()`** reads the CRS from the typed `GEOMETRY('OGC:CRS84')` column
  on duckdb >= 1.5, and falls back to EPSG:4326 on older versions, so `sf` generics work on
  the lazy object.

Dropped:

- **Cache the geometry column type** in the `overture_playbill` attribute. Not done, because
  the attribute survives `dplyr::mutate()`: after `mutate(geometry = ST_AsWKB(geometry))` the
  column is `BLOB` but a cached type would still read `GEOMETRY`, so `collect()` would inject
  `ST_AsWKB()` a second time and fail. `collect()` keeps its `DESCRIBE`, which is negligible
  next to the data it then pulls. `st_bbox()` and `st_crs()` each run one cheap query only
  when called, which is the real win over `sf`'s collect-based default.

Deferred:

- **Measure `wk` for WKB parsing.** duckspatial converts WKB with `wk::new_wk_wkb()` before
  `sf::st_as_sfc()`. wk is already in sf's dependency tree through s2, so the cost is nil.
  Switch only if a benchmark on a large `collect()` shows a real gain; the offline fixtures
  are too small to measure it.

**Effort.** Small.

## In what order should you ship?

0.3.1 is on CRAN (2026-10-08). Next is item 1, the articles, which need no code. Items 2
and 3 are unscheduled.

Lessons from the 0.3.1 release:

- **Overture changes its schema between releases.** Release 2026-09-23.0 removed
  `categories` from places in favor of `taxonomy` and `basic_category`, which broke the
  getting-started article and the skill. Re-render the articles against the latest
  release before each submission.
- **CRAN limits tests to two cores.** Loading DuckDB's extensions checks their signatures
  on every core, whatever `threads` says, so tests share one connection
  (`tests/testthat/helper.R`). Don't open a new connection per test.

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
