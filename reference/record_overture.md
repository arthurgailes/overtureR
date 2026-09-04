# Download Overture Maps Data to Local Directory

This function downloads Overture Maps data to a local directory,
maintaining the same partition structure as in S3. `snapshot_overture`
defaults 'output_dir' to
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) and overwrite to
TRUE.

## Usage

``` r
record_overture(curtain_call, output_dir, overwrite = FALSE, write_opts = NULL)

snapshot_overture(
  curtain_call,
  output_dir = tempdir(),
  overwrite = TRUE,
  write_opts = NULL
)
```

## Arguments

- curtain_call:

  A overture_call object.

- output_dir:

  The directory where the data will be saved.

- overwrite:

  Logical, if FALSE (default), existing directories will not be
  overwritten.

- write_opts:

  a character vector passed to DuckDB's COPY command.

## Value

Another tbl_lazy. Use
[`dplyr::show_query()`](https://dplyr.tidyverse.org/reference/explain.html)
to see the generated query, and use
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
to execute the query and return data to R.

An 'overture_call' for the downloaded data

## See also

[DuckDB documentation on partitioned
writes](https://duckdb.org/docs/data/partitioning/partitioned_writes)

## Examples

``` r
if (FALSE) { # interactive()
broadway <- c(xmin = -73.99, ymin = 40.76, xmax = -73.98, ymax = 40.76)
buildings <- open_curtain("building", spatial_filter = bbox)
local_buildings <- record_overture(buildings, tempdir(), overwrite = TRUE)
}
```
