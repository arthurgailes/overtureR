
<!-- README.md is generated from README.Rmd. Please edit that file -->

# overtureR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/overtureR)](https://CRAN.R-project.org/package=overtureR)
[![R-CMD-check](https://github.com/arthurgailes/overtureR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/arthurgailes/overtureR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/arthurgailes/overtureR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/arthurgailes/overtureR?branch=master)
<!-- badges: end -->

## Installation

``` r
devtools::install_github("arthurgailes/overtureR")
```

## Usage

Replicating `duckdb` examples fromm the [Overture
docs](https://docs.overturemaps.org/getting-data/duckdb/)

``` r
library(overtureR)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.2.3
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.2.3

counties <- open_curtain("division_area") |> 
  # in R, filtering on variables must come before removing them via select
  filter(subtype == 'county' & country == 'US' & region == 'US-PA') |> 
  transmute(
    id,
    division_id,
    # STRUCT/MAP columns can be accessed as column[["subcolumn"]] in transmute
    primary = names[["primary"]],
    geometry
  ) 

# Plot the results
counties |>
  collect_sf() |> 
  ggplot() +
    geom_sf(aes(fill = as.numeric(sf::st_area(geometry))))
```

<img src="man/figures/README-counties-1.png" width="100%" />

``` r
library(overtureR)
library(dplyr)

# lazily load the full `mountains` dataset
mountains <- open_curtain(type = "*", theme = "places") |> 
  transmute(
    id, 
    primary_name = names[["primary"]],
    x = bbox[["xmin"]],
    y = bbox[["ymin"]],
    main_category = categories[["primary"]],
    primary_source = sources[[1]][["dataset"]],
    confidence,
    geometry # currently no duckdb spatial implementation
  ) |> 
  filter(main_category == 'mountain' & confidence > .90) 

head(mountains)
#> # Source:   SQL [6 x 8]
#> # Database: DuckDB v1.0.0 [Arthur.Gailes@Windows 10 x64:R 4.2.1/:memory:]
#>   id            primary_name     x     y main_category primary_source confidence
#>   <chr>         <chr>        <dbl> <dbl> <chr>         <chr>               <dbl>
#> 1 08f464e0e312… Kawaikini    -159.  22.1 mountain      meta                0.954
#> 2 08f464e3b1a2… Kalepa       -159.  22.0 mountain      meta                0.938
#> 3 08f464e05984… Sleeping Gi… -159.  22.1 mountain      meta                0.945
#> 4 08f464e3a4d0… Nounou-East… -159.  22.1 mountain      meta                0.945
#> 5 08f464e05514… Makaleha Mo… -159.  22.1 mountain      meta                0.965
#> 6 08f464e03538… Makana       -160.  22.2 mountain      meta                0.938
#> # ℹ 1 more variable: geometry <list>
```

## Roadmap

-   add to_sf param/function?
-   Load all possible views into conn
-   Add beta/alpha datasets
-   load/install duckdb spatial
-   allow mirror/local reads via `base_url`
-   rm duckdbfs dependency
-   allow duckdb to write to file
-   improve s3 handling
-   add mapping vignettes
-   use pmtiles
-   rm d\[b\]plyr dependencies?
-   function to
