# Registeran sf object as a DuckDB virtual table

A thin wrapper around
[`duckdb::duckdb_register()`](https://r.duckdb.org/reference/duckdb_register.html)
that creates a virtual table, then selects the geometry column to
DuckDB.'s GEOMETRY type in the returned `dbplyr` representation. Mostly
useful for join and spatial operations within DuckDB. No data is copied.

## Usage

``` r
sf_as_dbplyr(
  conn,
  name,
  sf_obj,
  geom_only = isFALSE(inherits(sf_obj, "sf")),
  overwrite = FALSE,
  ...
)
```

## Arguments

- conn:

  A DuckDB connection, created by `dbConnect()`.

- name:

  The name for the virtual table that is registered or unregistered

- sf_obj:

  sf object to be registered to duckdb

- geom_only:

  if TRUE, only the geometry column is registered. Always FALSE for sfc
  or sfg objects

- overwrite:

  Should an existing registration be overwritten?

- ...:

  additional arguments passed to duckdb_register

## Value

a `dbplyr` lazy table

## Details

Behind the scenes, this function creates an initial view (`name`\_init)
with the geometry stored as text via
[`sf::st_as_text`](https://r-spatial.github.io/sf/reference/st_as_text.html).
It then creates the view `name` which replaces the geometry column with
DuckDB's internal geometry type.

## Examples

``` r
if (FALSE) { # interactive()
library(sf)

con <- stage_conn()
sf_obj <- st_sf(a = 3, geometry = st_sfc(st_point(1:2)))
sf_as_dbplyr(con, "test", sf_obj)

DBI::dbDisconnect(con)
}
```
