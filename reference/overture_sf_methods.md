# Extent and coordinate reference system without collecting

[`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)
and
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
methods for lazy `overture_call` tables, so you can read a query's
extent and coordinate reference system straight from DuckDB, without
pulling the rows into R with
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html).

## Usage

``` r
# S3 method for class 'overture_call'
st_crs(x, ...)

# S3 method for class 'overture_call'
st_bbox(obj, ...)
```

## Arguments

- ...:

  Unused, for compatibility with the generics.

- obj, x:

  An `overture_call` object, as returned by
  [`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md).

## Value

`st_bbox()` returns an
[`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)
object; `st_crs()` returns an
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
object.

## Details

`st_bbox()` runs one `ST_Extent_Agg()` query over the geometry column.
`st_crs()` reads the coordinate reference system from DuckDB's typed
`GEOMETRY` column (for example `GEOMETRY('OGC:CRS84')` on duckdb \>=
1.5), falling back to EPSG:4326, which is what Overture stores.

## Examples

``` r
if (FALSE) { # interactive()
bbox <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)
buildings <- open_curtain("building", bbox)
sf::st_crs(buildings)
sf::st_bbox(buildings)
}
```
