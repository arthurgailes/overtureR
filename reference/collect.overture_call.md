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

## Details

The geometry column is read back as well-known binary and converted with
[`sf::st_as_sfc()`](https://r-spatial.github.io/sf/reference/st_as_sfc.html).
If the column is already binary (for example, after a
`mutate(geometry = ST_AsWKB(geometry))`), it is used as is. If it is
neither DuckDB `GEOMETRY` nor binary, the result is returned as a plain
data frame.

## Examples

``` r
if (FALSE) { # interactive()

bbox <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)
lazy_tbl <- open_curtain("building", bbox)
collect(lazy_tbl)
}
```
