# Discover the latest available Overture Maps release

[`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)
needs a release date/version to build its S3 path (e.g.
`"2026-08-19.0"`). Overture cuts a new release roughly monthly, so
hardcoding one quickly goes stale. This queries Overture's STAC catalog
(<https://stac.overturemaps.org/catalog.json>) for its current `latest`
release, so callers don't have to track releases themselves or wait on a
package update. The result is cached for the session.

## Usage

``` r
latest_overture_release(conn = NULL, refresh = FALSE)
```

## Arguments

- conn:

  A duckdb connection. Uses the cached session connection by default.

- refresh:

  If `TRUE`, bypass the session cache and re-query the catalog.

## Value

A string identifying the latest release, e.g. `"2026-08-19.0"`.

## Details

If the catalog can't be reached, the newest release in the package's
local catalog cache is used with a warning (Overture removes releases
after a few months, so it may itself be gone). With no cache, the call
fails with an error; pass `base_url` to
[`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)
to work offline or from a local copy.

## Examples

``` r
if (FALSE) { # interactive()
latest_overture_release()
}
```
