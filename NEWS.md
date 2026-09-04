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
