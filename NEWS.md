# overtureR 0.3.0

## Faster queries

* `open_curtain()` now reads only the Parquet files whose bounding box touches
`spatial_filter`, using the per-file bounding boxes in Overture's STAC catalog,
instead of every file in the partition. A cold city-sized `building` query drops
from about 87 seconds to a few seconds, because DuckDB reads 1 to 3 Parquet
footers rather than 512. The catalog is read once per release, type and session
and cached on disk under `tools::R_user_dir("overtureR", "cache")` (releases
never change, so the cache never goes stale). Turn pruning off with
`options(overturer_prune = FALSE)`, move or disable the cache with
`options(overturer_cache_dir = )` and `options(overturer_cache = FALSE)`, or
clear it with the new `clear_overture_cache()`.

* `sf_as_dbplyr()` (and so `sf` and `sfc` spatial filters) sends geometry to
DuckDB as well-known binary instead of well-known text. Uploading 5,000
polygons drops from about 14 seconds to under 0.1 seconds, and coordinates keep
full precision.

* An `sf` spatial filter is now uploaded once and its union kept in a small
temporary table, instead of leaving a registered data frame and two views
behind and re-running `ST_Union_Agg()` on every query.

* Connections created by `stage_conn()` (and any connection passed to
`open_curtain()`) enable DuckDB's Parquet metadata and HTTP metadata caches, so
repeated queries against the same files skip re-reading their footers.

## New

* `open_curtain()` gains a `release` argument to pin a query to one Overture
release, such as `"2026-08-19.0"`, instead of whichever release is latest when
the script runs. Set `options(overturer_release = )` to pin a whole session. The
release is recorded on the result, and the new `print()` method for
`overture_call` objects shows it. `base_url` now defaults to `NULL` and is built
from `release`.

* `open_curtain()` gains a `predicate` argument: `"intersects"` (the default),
`"within"` or `"contains"`, so a polygon filter can keep only the features fully
inside it, or a point can find the feature around it.

* `overture_releases()` lists the releases Overture currently hosts.

* `overture_types()` lists the `type` and `theme` pairs in a release, read from
Overture's catalog, so new types appear without a package update. The built-in
table (now including `bathymetry`) is the offline fallback, and `open_curtain()`
names the valid types when given one it doesn't know.

* `clear_overture_cache()` removes the catalog cache.

## Local copies

* `record_overture()` writes an `_overture.json` manifest into each `type=`
directory it creates, recording the source release and the bounding box of every
file. `open_curtain()` on such a copy reads only the files that touch
`spatial_filter`, and labels the result with the release it came from.

* `record_overture()` gains `partition_by`, for extra partition columns below
`theme` and `type`, and `grid`, which partitions the copy into cells of a given
size in degrees so large local copies also skip files by location.

* `record_overture()` can download in one call: pass a type name and a
`spatial_filter` (plus any other `open_curtain()` arguments) instead of an
`overture_call`.

* `record_overture()` with `overwrite = TRUE` replaces the `theme=/type=`
directories it is about to write instead of writing new files next to the old
ones, so a re-recorded copy holds only the new rows. Other files in `output_dir`
are untouched. Written files get unique names (`data_<uuid>.parquet`), so
DuckDB's Parquet metadata cache never serves a stale footer for a rewritten
copy.

* `record_overture()` quotes `output_dir`, so a path containing `'` works.

## Fixes

* `sf`, `sfc` and `bbox` spatial filters in a coordinate reference system other
than EPSG:4326 are transformed before filtering. Previously their raw
coordinates were compared with Overture's longitude and latitude, which
silently returned no rows or the wrong rows. A filter with no coordinate
reference system is assumed to be EPSG:4326, with a warning.

* `stage_conn()` now passes `dbdir`, `read_only`, `bigint` and `config` on to
duckdb. Before, only `...` reached `DBI::dbConnect()`, so
`stage_conn(dbdir = "x.duckdb")` silently opened an in-memory database.

* `stage_conn()` registers its shutdown finalizer once rather than on every
call, and `strike_stage()` no longer opens a new connection just to close it
when none is cached.

* `collect()` checks the geometry column's type before converting it. A
column that is already well-known binary (for example after
`mutate(geometry = ST_AsWKB(geometry))`) is used as is, and a non-spatial
column named `geometry` is left alone, where both used to fail with a binder
error. Geometry is converted with `sf::st_as_sfc()` rather than through GDAL,
which removes the `OGR: Unsupported geometry type` message on places data.
Extra arguments such as `collect(x, crs = 3857)` now work.

* `latest_overture_release()` no longer falls back to a hardcoded release
when the catalog is unreachable, because Overture removes releases after a
few months and the hardcoded one would itself fail. It now uses the newest
release in the local catalog cache with a warning, or fails with an error
that points to `base_url`.

* An unnamed numeric `spatial_filter` gives a clear error instead of
`subscript out of bounds`. Passing a single `sfg` point works.

* `record_overture()` checks its input before touching the connection.

* Fixed examples that called a non-existent `exit_stage()` and passed an
undefined `bbox`, and the getting-started article's references to
`collect_sf`.

## Dependencies

* overtureR now requires duckdb 1.1.0 or later, and drops the code paths for
older versions (manual WKB casts and the duckdb 1.1.3 extension workaround).

## Tests

* The test suite now runs offline against a few hundred Overture features
saved under `tests/testthat/fixtures/`, plus a miniature of Overture's STAC
catalog. It covers each SQL builder, every filter kind, the catalog cache, and
a regression test for each fix above. Three tests still read the live release;
they run locally and on a weekly schedule, not on every push.

# overtureR 0.2.6

* Bundle an agent skill at `inst/skills/overturer/` teaching AI coding agents
the package's API and idioms - chiefly accessing Overture's nested struct
columns with `$` (`names$primary`), lazy-then-`collect()` evaluation, and the
`type`->`theme` mapping. Ships `SKILL.md` plus `references/data-model.md` (schema
and type/theme table) and `references/overture-context.md` (Overture themes,
GERS stable ids, licensing/attribution, release cadence, and links).
Discoverable by tools following the `inst/skills/` convention (e.g. Posit's
`btw`).

* `open_curtain()`'s `base_url` no longer hardcodes a specific Overture
release. It now defaults to the latest release, discovered dynamically via
the new exported `latest_overture_release()` (queries Overture's STAC
catalog and caches the result for the session), so the package doesn't need
a release-bump update every time Overture cuts a new release.

* Fix `record_overture()` producing local Parquet files that couldn't be
`collect()`-ed back on duckdb >= 1.1: geometry was being pre-cast to WKB
before writing, which loses the GeoParquet metadata that tells DuckDB to
read the column back as native `GEOMETRY`, so the later `ST_AsWKB()` call
in `collect()` failed on what was now a plain `BLOB` column. The WKB cast
is now skipped on duckdb >= 1.1, matching how `open_curtain()` already
treats those versions.

# overtureR 0.2.3

* Fix for bug in `duckdb` 1.1.3.
* Update for post 1.1.0 native duckdb geometry reading
* Update URL to November 2024 Overture release.

# overtureR 0.2.0

* Add support for downloading Overture Maps data via `record_overture` and 
`snapshot_overture`. These functions return a lazy 'overture_call' dataframe
linked to the new local dataset.
  
* The second parameter to `open_curtain` has changed from 'bbox' to 
'spatial_filter', which allows both bounding boxes (named vector or class 
'bbox'), 'sf' objects, or another `dbplyr` dataframe (e.g. a different 
`overtureR` dataset/table. In the latter two cases, the data will first be 
filtered by the bounding box of `spatial_filter`, then geographically if 
necessary. Filtering is currently by intersection.

* `open_curtain` parameter 'bbox' is deprecated, and will likely be removed in a
future release.

* Class 'overture_call' has been added to facilitate `collect` calls directly as
`sf` objects.

* `collect_sf` is now deprecated, use `collect`.

# overtureR 0.1.0

* Initial CRAN submission
