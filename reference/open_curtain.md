# Retrieve (Spatially Filtered) Overture Datasets

Fetches overture data from AWS. If a bounding box is provided, it
applies spatial filtering to only include records within that area. The
core code is copied from `duckdbfs`, which deserves all credit for the
implementation

## Usage

``` r
open_curtain(
  type,
  spatial_filter = NULL,
  theme = get_theme_from_type(type),
  conn = NULL,
  as_sf = FALSE,
  mode = "view",
  tablename = NULL,
  read_opts = list(),
  base_url = paste0("s3://overturemaps-us-west-2/release/",
    latest_overture_release(conn)),
  bbox = NULL
)
```

## Arguments

- type:

  A string specifying the type of overture dataset to read. Setting to
  "\*" or `NULL` will read all types for a given theme.

- spatial_filter:

  An object to spatially filter the result.

- theme:

  Inferred from type by default. Must be set if type is "\*" or NULL

- conn:

  A connection to a duckdb database.

- as_sf:

  If TRUE, return an sf dataframe

- mode:

  Either "view" (default) or "table". If "table", will download the
  dataset into memory.

- tablename:

  The name of the table to create in the database.

- read_opts:

  A named list of key-value pairs passed to [DuckDB's
  read_parquet](https://duckdb.org/docs/data/parquet/overview.html#parameters)

- base_url:

  Allows user to download data from a different mirror, such as a local
  directory, or a alternative release. Defaults to the latest Overture
  release, discovered via
  [`latest_overture_release()`](https://arthurgailes.github.io/overtureR/reference/latest_overture_release.md).

- bbox:

  alias for `spatial_filter`. may be deprecated in the future.

## Value

An dbplyr lazy dataframe, or an sf dataframe if as_sf is TRUE

## Examples

``` r
if (FALSE) { # interactive()
bbox <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)
open_curtain("building", bbox)
}
```
