# List the Overture releases still online

Overture publishes a release about once a month and removes old ones
after a few months. This reads the releases its STAC catalog currently
lists, so you can pick one for `open_curtain(release = )` or check that
a pinned release is still available.

## Usage

``` r
overture_releases(conn = NULL)
```

## Arguments

- conn:

  A duckdb connection. Uses the cached session connection by default.

## Value

A character vector of releases, newest first, such as
`c("2026-08-19.0", "2026-07-22.0")`.

## Examples

``` r
if (FALSE) { # interactive()
overture_releases()
}
```
