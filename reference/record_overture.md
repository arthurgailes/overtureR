# Download Overture Maps data to a local directory

Writes the rows of an `overture_call` to Parquet files under
`output_dir`, in Overture's own `theme=<theme>/type=<type>/` layout, and
returns a new `overture_call` that reads from the copy. Each `type=`
directory also gets an `_overture.json` manifest recording the source
release and the bounding box of every file, so
[`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)
on the copy skips files by location just as it does on S3.
`snapshot_overture()` defaults `output_dir` to
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) and `overwrite` to
`TRUE`.

## Usage

``` r
record_overture(
  curtain_call,
  output_dir,
  overwrite = FALSE,
  write_opts = NULL,
  partition_by = NULL,
  grid = NULL,
  spatial_filter = NULL,
  ...
)

snapshot_overture(
  curtain_call,
  output_dir = tempdir(),
  overwrite = TRUE,
  write_opts = NULL,
  partition_by = NULL,
  grid = NULL,
  spatial_filter = NULL,
  ...
)
```

## Arguments

- curtain_call:

  An `overture_call` object, or the name of an Overture type (such as
  `"building"`) to open with
  [`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)
  first.

- output_dir:

  The directory where the data will be saved.

- overwrite:

  If `FALSE` (default), `output_dir` must be empty. If `TRUE`, the
  `theme=/type=` directories being written are replaced; other files in
  `output_dir` are left alone.

- write_opts:

  A character vector of extra options for DuckDB's `COPY` command, such
  as `"ROW_GROUP_SIZE 100000"`. Use `partition_by`, not `PARTITION_BY`,
  to change the layout.

- partition_by:

  Names of columns to partition by, below `theme` and `type`. Add
  columns with
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  first if needed.

- grid:

  Cell size in degrees. If set, the copy is further partitioned into a
  grid of that size (columns `x_cell` and `y_cell`, the cell's
  south-west corner), so each file covers a compact area and a later
  `spatial_filter` skips most of them.

- spatial_filter, ...:

  Passed to
  [`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)
  when `curtain_call` is a type name.

## Value

An `overture_call` reading from the downloaded data. Use
[`dplyr::show_query()`](https://dplyr.tidyverse.org/reference/explain.html)
to see its query and
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
to bring the rows into R.

## See also

[DuckDB documentation on partitioned
writes](https://duckdb.org/docs/data/partitioning/partitioned_writes)

## Examples

``` r
if (FALSE) { # interactive()
broadway <- c(xmin = -73.99, ymin = 40.755, xmax = -73.98, ymax = 40.762)
buildings <- open_curtain("building", spatial_filter = broadway)
local_buildings <- record_overture(buildings, tempdir(), overwrite = TRUE)

# or in one call
local_buildings <- record_overture(
  "building", tempdir(), overwrite = TRUE, spatial_filter = broadway
)
}
```
