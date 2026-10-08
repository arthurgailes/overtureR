## Resubmission

The first upload of 0.3.1 gave a NOTE on Debian: the tests used 5.1 times
as much CPU time as elapsed time. The cause was DuckDB's extension loading,
which checks each extension's signature on every core. The tests opened a
new connection, and so loaded the extensions again, for each test. They now
share one connection that runs queries on one thread, and their CPU time is
about equal to their elapsed time.

## overtureR 0.3.1

This release replaces 0.2.5, the current CRAN version. Versions 0.2.6 and
0.3.0 were never published on CRAN, so NEWS.md lists their changes under
their own headings.

`open_curtain()` now reads only the Parquet files whose bounding box touches
the spatial filter, using the per-file bounding boxes in Overture's public
STAC catalog. The test suite now runs offline against small bundled fixtures
(about 380 KB).

New arguments: `release` pins a query to one Overture release, and
`predicate` selects "within" or "contains" in place of "intersects".
`record_overture()` gains custom partitions and writes a manifest next to
the data. New functions: `overture_types()`, `overture_releases()` and
`clear_overture_cache()`. This release fixes bugs in `collect()`,
`strike_stage()` and `record_overture()`. See NEWS.md for the full list.

The minimum duckdb version rises to 1.1.0 (released September 2024), which
removes the compatibility code for older versions.

The package ships a Markdown file at `inst/skills/overturer/SKILL.md`, with
two reference files beside it. These are plain-text usage notes for AI
coding assistants, read by tools such as Posit's btw package. The package
code never reads or runs them.

The package caches the release catalog on disk under
`tools::R_user_dir("overtureR", "cache")`. It writes nothing there during
checks or examples: tests redirect the cache to `tempdir()`, and every
example that reaches the network runs only in interactive sessions
(`@examplesIf interactive()`).

The three tests that read Overture's live S3 release skip on CRAN.

## Test environments

- Local Windows 11, R 4.5.0
- win-builder, R-devel
- R-hub: Linux (R-devel), macOS (R-release), Windows (R-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes
