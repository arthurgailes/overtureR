## overtureR 0.3.0

This release makes `open_curtain()` read only the Parquet files whose
bounding box touches the spatial filter, using the per-file bounding boxes
in Overture's public STAC catalog, and rebuilds the test suite so that it
runs offline against small bundled fixtures (about 380 KB). It also fixes
several bugs in `collect()`, `strike_stage()`, and `record_overture()`, and
adds `overture_types()` and `clear_overture_cache()`. See NEWS.md for the
full list.

The package caches the release catalog on disk under
`tools::R_user_dir("overtureR", "cache")`. Nothing is written there during
checks or examples: tests redirect the cache to `tempdir()`, and every
example that reaches the network runs only in interactive sessions
(`@examplesIf interactive()`).

The three tests that read Overture's live S3 release are skipped on CRAN.

## R CMD check results

0 errors | 0 warnings | 0 notes
