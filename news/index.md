# Changelog

## overtureR 0.2.6

- [`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)’s
  `base_url` no longer hardcodes a specific Overture release. It now
  defaults to the latest release, discovered dynamically via the new
  exported
  [`latest_overture_release()`](https://arthurgailes.github.io/overtureR/reference/latest_overture_release.md)
  (queries Overture’s STAC catalog and caches the result for the
  session), so the package doesn’t need a release-bump update every time
  Overture cuts a new release.

- Fix
  [`record_overture()`](https://arthurgailes.github.io/overtureR/reference/record_overture.md)
  producing local Parquet files that couldn’t be
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html)-ed
  back on duckdb \>= 1.1: geometry was being pre-cast to WKB before
  writing, which loses the GeoParquet metadata that tells DuckDB to read
  the column back as native `GEOMETRY`, so the later `ST_AsWKB()` call
  in [`collect()`](https://dplyr.tidyverse.org/reference/compute.html)
  failed on what was now a plain `BLOB` column. The WKB cast is now
  skipped on duckdb \>= 1.1, matching how
  [`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)
  already treats those versions.

## overtureR 0.2.3

CRAN release: 2024-09-04

- Fix for bug in `duckdb` 1.1.3.
- Update for post 1.1.0 native duckdb geometry reading
- Update URL to November 2024 Overture release.

## overtureR 0.2.0

- Add support for downloading Overture Maps data via `record_overture`
  and `snapshot_overture`. These functions return a lazy ‘overture_call’
  dataframe linked to the new local dataset.

- The second parameter to `open_curtain` has changed from ‘bbox’ to
  ‘spatial_filter’, which allows both bounding boxes (named vector or
  class ‘bbox’), ‘sf’ objects, or another `dbplyr` dataframe (e.g. a
  different `overtureR` dataset/table. In the latter two cases, the data
  will first be filtered by the bounding box of `spatial_filter`, then
  geographically if necessary. Filtering is currently by intersection.

- `open_curtain` parameter ‘bbox’ is deprecated, and will likely be
  removed in a future release.

- Class ‘overture_call’ has been added to facilitate `collect` calls
  directly as `sf` objects.

- `collect_sf` is now deprecated, use `collect`.

## overtureR 0.1.0

CRAN release: 2024-08-01

- Initial CRAN submission
