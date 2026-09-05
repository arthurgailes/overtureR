# List the dataset types in an Overture release

Reads the type-to-theme table for a release from Overture's STAC
catalog, so new types (such as `bathymetry`) appear without a package
update. The answer is cached per release. If the catalog can't be
reached, the package's built-in table is returned with a warning.

## Usage

``` r
overture_types(release = latest_overture_release(conn), conn = NULL)
```

## Arguments

- release:

  An Overture release, such as `"2026-08-19.0"`. Defaults to the latest
  release.

- conn:

  A duckdb connection. Uses the cached session connection by default.

## Value

A data frame with columns `type` and `theme`.

## Examples

``` r
if (FALSE) { # interactive()
overture_types()
}
```
