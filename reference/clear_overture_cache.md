# Clear overtureR's catalog cache

overtureR caches what it reads from Overture's STAC catalog (the list of
types in a release, and the bounding box of every Parquet file) in
memory and on disk under `tools::R_user_dir("overtureR", "cache")`.
Releases never change, so the cache never goes stale, but you can clear
it here. Set `options(overturer_cache = FALSE)` to keep the cache in
memory only, or `options(overturer_cache_dir = )` to move it.

## Usage

``` r
clear_overture_cache()
```

## Value

The cache directory, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
clear_overture_cache()
} # }
```
