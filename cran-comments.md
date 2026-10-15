## overtureR 0.3.2

This release fixes two tests that fail with the upcoming duckdb 2.0.0, at
the request of the duckdb maintainers
(https://github.com/arthurgailes/overtureR/issues/5). DuckDB 2.0 writes a
`TRUE` option back into view SQL as `true`, where 1.x wrote
`CAST('t' AS BOOLEAN)`. The tests now accept either spelling. The package
code is unchanged.

0.3.1 was published on 2026-10-08. We are sorry for the quick follow-up. It
keeps the package passing its checks when duckdb 2.0.0 reaches CRAN.

## Test environments

- Local Windows 11, R 4.5.0, duckdb 1.5.2
- win-builder, R-devel
- R-hub: Linux (R-devel), macOS (R-devel), Windows (R-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes
