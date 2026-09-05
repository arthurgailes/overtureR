# Retrieve (Spatially Filtered) Overture Datasets

Fetches overture data from AWS. If a spatial filter is provided, it
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
  predicate = "intersects",
  release = NULL,
  base_url = NULL,
  bbox = NULL
)
```

## Arguments

- type:

  A string specifying the type of overture dataset to read. Setting to
  "\*" or `NULL` will read all types for a given theme. See
  [`overture_types()`](https://arthurgailes.github.io/overtureR/reference/overture_types.md)
  for the valid values.

- spatial_filter:

  An object to spatially filter the result: a named numeric vector or
  [`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)
  bounding box, an `sf` or `sfc` object, the name of a table in `conn`,
  or another `dbplyr` lazy table with a `geometry` column. `sf` filters
  in another coordinate reference system are transformed to EPSG:4326
  (Overture's) before filtering.

- theme:

  Inferred from type by default. Must be set if type is "\*" or `NULL`.

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
  read_parquet](https://duckdb.org/docs/data/parquet/overview#parameters)

- predicate:

  How a feature must relate to `spatial_filter` to be kept:
  `"intersects"` (default), `"within"` (the feature lies entirely inside
  the filter) or `"contains"` (the feature contains the whole filter).

- release:

  An Overture release, such as `"2026-08-19.0"`. Defaults to
  `getOption("overturer_release")`, then to the latest release found by
  [`latest_overture_release()`](https://arthurgailes.github.io/overtureR/reference/latest_overture_release.md).
  See
  [`overture_releases()`](https://arthurgailes.github.io/overtureR/reference/overture_releases.md)
  for the releases Overture still hosts. When `base_url` is set,
  `release` only labels the result.

- base_url:

  Read from a different mirror, such as a local directory from
  [`record_overture()`](https://arthurgailes.github.io/overtureR/reference/record_overture.md).
  Defaults to the S3 path of `release`.

- bbox:

  alias for `spatial_filter`. may be deprecated in the future.

## Value

An dbplyr lazy dataframe, or an sf dataframe if as_sf is TRUE

## Details

When `spatial_filter` is set and `base_url` points at an Overture
release or at a directory written by
[`record_overture()`](https://arthurgailes.github.io/overtureR/reference/record_overture.md),
`open_curtain()` reads only the Parquet files whose bounding box touches
the filter, using the file list in Overture's STAC catalog or the local
copy's manifest (see
[`overture_types()`](https://arthurgailes.github.io/overtureR/reference/overture_types.md)
and
[`clear_overture_cache()`](https://arthurgailes.github.io/overtureR/reference/clear_overture_cache.md)).
This turns a cold query over hundreds of files into one over a handful.
Set `options(overturer_prune = FALSE)` to always read the whole
partition.

To pin every query in a session to one release, set
`options(overturer_release = "2026-08-19.0")`.

## Examples

``` r
if (FALSE) { # interactive()
bbox <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)
open_curtain("building", bbox)

# pin a release so the script returns the same rows next month
open_curtain("building", bbox, release = "2026-08-19.0")

# only buildings entirely inside the box
open_curtain("building", bbox, predicate = "within")
}
```
