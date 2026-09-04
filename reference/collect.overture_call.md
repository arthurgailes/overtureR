# Convert dbplyr table to sf Object

Collects a lazy dbplyr view and materializes it as an in-memory `sf`
table. `collect_sf` is a deprecated alias.

## Usage

``` r
# S3 method for class 'overture_call'
collect(x, ..., geom_col = "geometry", crs = 4326)

collect_sf(...)
```

## Arguments

- x:

  A lazy data frame backed by a database query.

- ...:

  Further arguments passed to
  [`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html).

- geom_col:

  The name of the geometry column. Will auto-detect names matching
  'geom'.

- crs:

  The coordinate reference system to use for the geometries, specified
  by its EPSG code. The default is 4326 (WGS 84).

## Value

An 'sf' object with the dataset converted to spatial features.

## Examples

``` r
if (FALSE) { # interactive()

bbox <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)
lazy_tbl <- open_curtain("building", bbox)
collect(lazy_tbl)
}
```
