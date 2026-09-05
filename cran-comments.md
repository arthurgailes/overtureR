## overtureR 0.3.0

This release makes `open_curtain()` read only the Parquet files whose
bounding box touches the spatial filter, using the per-file bounding boxes
in Overture's public STAC catalog. The test suite now runs offline against
small bundled fixtures (about 380 KB).

New arguments: `release` pins a query to one Overture release, and
`predicate` selects "within" or "contains" in place of "intersects".
`record_overture()` gains custom partitions and writes a manifest next to
the data. New functions: `overture_types()`, `overture_releases()` and
`clear_overture_cache()`. Several bugs in `collect()`, `strike_stage()`, and
`record_overture()` are fixed. See NEWS.md for the full list.

The minimum duckdb version rises to 1.1.0 (released September 2024), which
removes the compatibility code for older versions.

The package caches the release catalog on disk under
`tools::R_user_dir("overtureR", "cache")`. Nothing is written there during
checks or examples: tests redirect the cache to `tempdir()`, and every
example that reaches the network runs only in interactive sessions
(`@examplesIf interactive()`).

The three tests that read Overture's live S3 release are skipped on CRAN.

## R CMD check results

0 errors | 0 warnings | 0 notes
