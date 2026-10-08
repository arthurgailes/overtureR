# Raising the Curtain: Getting Started with overtureR

``` r

# install if needed:
install.packages("overtureR")
```

``` r

library(overtureR)
library(ggplot2)
library(dplyr)
library(sf)
```

This vignette demonstrates how to use overtureR to access and visualize
Overture Maps data, focusing on a practical example in Washington, DC:
finding the theater.

Overture Maps is an open-source mapping initiative aimed at developers
who build map services or use geospatial data. It provides a
collaborative, globally-referenced, and quality-assured dataset with a
structured schema. This makes it an excellent resource for creating
reliable and interoperable map products. Using overtureR, we can easily
tap into this rich dataset. In this guide, we’ll walk through the
process of:

1.  Fetching the boundary of Washington, DC
2.  Locating Ronald Reagan National Airport
3.  Finding the Kennedy Center theater
4.  Getting to the Kennedy Center with public transit

[`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)
function is our primary tool for accessing Overture Maps data. We’ll
start by using
[`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)
to retrieve the DC boundary and pinpoint the airport:

``` r

# Washington, DC boundary
dc <- open_curtain("division_area") |>
  filter(subtype == "region", region == "US-DC") |>
  collect()

# adding a bounding box makes the query faster:
dc_catchment <- st_geometry(dc) |>
  # 10 miles from DC
  st_buffer(10 * 1609.34) |>
  st_bbox()

reagan_airport <- open_curtain("place", spatial_filter = dc_catchment) |>
  filter(
    basic_category == "airport",
    grepl("^Ronald Reagan Washington National Airport", names$primary)
  ) |>
  # several places share the name; keep the most reliable one
  slice_max(confidence, n = 1) |>
  collect()

print(reagan_airport)
#> Simple feature collection with 1 feature and 17 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -77.04273 ymin: 38.84835 xmax: -77.04273 ymax: 38.84835
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 18
#>   id                     geometry confidence websites  emails socials   phones
#>   <chr>               <POINT [°]>      <dbl> <list>    <list> <list>    <list>
#> 1 9bddff21-… (-77.04273 38.84835)      0.976 <chr [1]> <NULL> <chr [1]> <NULL>
#> # ℹ 11 more variables: brand <df[,2]>, addresses <list>, names <df[,3]>,
#> #   sources <list>, operating_status <chr>, basic_category <chr>,
#> #   taxonomy <df[,3]>, version <int>, bbox <df[,4]>, theme <chr>, type <chr>
```

By default, `open_curtain` would search through every “place” (aka point
of interest) in the world - an enormous dataset. Obviously, that’s too
much to load into most computers’ memory, so `open_curtain` does this
lazily. Only after calling `collect` does it load data onto your
computer. So we filter the data first, spatially and by name, like so:

1.  fetch the boundary of Washington, DC from the “division_area”
    dataset;
2.  filter for the specific region we wanted;
3.  create a spatial buffer around DC to define our area of interest for
    subsequent queries; and
4.  locate Ronald Reagan National Airport using the “place” dataset,
    filtering by name and category.

Afterwards, `collect` brings only the data you need into memory. For
more on lazy programming, see the [dbplyr
documentation](https://dbplyr.tidyverse.org/).

Now that we’ve set the stage with our starting point, let’s spotlight
our destination. In the next code block, we’ll locate the Kennedy
Center:

``` r

reagan_plot <- ggplot() +
  geom_sf(data = dc, fill = "purple", alpha = 0.05) +
  geom_sf(data = reagan_airport, color = "red", size = 4) +
  geom_sf_label(
    data = reagan_airport, nudge_y = 0.01, aes(label = names$primary)
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

reagan_plot
```

![](getting_started_files/figure-html/reagan_plot-1.png)

In this code, we’ve queried the “building” dataset within our defined DC
area. We used a text filter to find buildings with “The Kennedy Center”
in their name. This demonstrates overtureR’s ability to perform
text-based searches within the Overture Maps dataset.

To get to the theater, we’ll need to know our transit options. The
following code showcases overtureR’s capacity to handle more complex
spatial and attribute queries:

``` r


kennedy_center <- open_curtain("building", st_bbox(dc)) |>
  filter(grepl("Kennedy Center", names$primary)) |>
  collect()


kennedy_plot <- reagan_plot +
  geom_sf(data = kennedy_center, fill = "green") +
  geom_sf_label(data = kennedy_center, nudge_y = 0.01, aes(label = names$primary))
kennedy_plot
```

![](getting_started_files/figure-html/kennedy-1.png)

In the code above, we’ve created a bounding box that encompasses both
the airport and the Kennedy Center, plus a one-mile buffer. We then used
this to filter the “segment” dataset for rail transit, specifically the
Blue Line of the DC Metro.

For the grand finale, we’ll create a map that displays all the elements
we’ve gathered:

``` r

# filter town to areas that are within ~1 mile of our two points
kennedy_reagan_bbox <- bind_rows(kennedy_center, reagan_airport) |>
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(1 * 1609.34) |>
  st_bbox()

dc_transit <- open_curtain("segment", kennedy_reagan_bbox) |>
  filter(
    subtype == "rail",
    # filter to the Blue Line of the DC Metro
    grepl("Metro", names$primary),
    grepl("Blue", names$primary)
  ) |>
  select(id, names, geometry) |>
  collect()

print(dc_transit)
#> Simple feature collection with 20 features and 2 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -77.07099 ymin: 38.81561 xmax: -76.96203 ymax: 38.90138
#> Geodetic CRS:  WGS 84
#> # A tibble: 20 × 3
#>    id                                   names$primary                   geometry
#>    <chr>                                <chr>                   <LINESTRING [°]>
#>  1 2f677d3b-22eb-4da0-9190-006a8ba0f170 Washington Me… (-77.06389 38.88592, -77…
#>  2 f3be69a6-4842-4960-876d-974f53fd1b0a Washington Me… (-77.07089 38.89466, -77…
#>  3 1992200a-1872-471b-ae55-7063c6922625 Washington Me… (-77.06394 38.8859, -77.…
#>  4 5905e0a3-5de7-4bc7-b03e-c9e4575beb18 Washington Me… (-77.06363 38.8855, -77.…
#>  5 ea9a4d22-30fa-4643-b073-3079c178bd53 Washington Me… (-77.06174 38.88253, -77…
#>  6 991babf5-2254-41c0-815f-249582189435 Washington Me… (-77.06174 38.88253, -77…
#>  7 29d8560d-34fe-47f5-8b2c-c5c276158888 Washington Me… (-77.06368 38.88548, -77…
#>  8 19851ca0-83f2-4e75-9bfd-62b2db763176 Washington Me… (-77.05259 38.87016, -77…
#>  9 42842d38-b263-4d0a-857f-022bff20cea5 Washington Me… (-77.05904 38.86444, -77…
#> 10 44644aa4-ab84-46ef-9916-72fcdc14aa00 Washington Me… (-77.05269 38.87024, -77…
#> 11 e9e87c62-1ddb-493c-8b13-381f023e58c5 Washington Me… (-77.05915 38.86413, -77…
#> 12 10a7eab7-6610-47f3-9658-34dd22b77c67 Washington Me… (-77.05907 38.86444, -77…
#> 13 8bdc65a2-fbeb-4f2d-83e8-06cbd88ff8a1 Washington Me… (-77.05918 38.86414, -77…
#> 14 0d9f5fd8-3893-439c-ac61-0fafe2231737 Washington Me… (-77.04461 38.85512, -77…
#> 15 92b822b6-1d5a-4eee-9ac4-255072259954 Washington Me… (-77.05255 38.81561, -77…
#> 16 f93c1c51-85b9-49c9-a5b2-7f4769586ca7 Washington Me… (-77.04351 38.8519, -77.…
#> 17 3442c5c9-07e9-47f9-924a-bf91f726720c Washington Me… (-77.0433 38.85196, -77.…
#> 18 6467debb-6376-42da-b19a-89e22707b65e Washington Me… (-77.0448 38.85507, -77.…
#> 19 516b8b21-6fe1-4836-94ed-daed052376e6 Washington Me… (-76.96203 38.8974, -76.…
#> 20 22be2fe4-155b-4fc6-8ea7-bb40ee6e9def Washington Me… (-77.07085 38.89466, -77…
#> # ℹ 2 more variables: names$common <list>, $rules <list>
```

This final step uses ggplot2 to create a map that displays the airport,
the Kennedy Center, and the Metro Blue Line connecting them. This
visualizes the route from our arrival point to our theatrical
destination.

``` r

kennedy_plot +
  geom_sf(data = dc_transit, color = "blue") +
  coord_sf(
    xlim = c(kennedy_reagan_bbox[["xmin"]], kennedy_reagan_bbox[["xmax"]]),
    ylim = c(kennedy_reagan_bbox[["ymin"]], kennedy_reagan_bbox[["ymax"]]),
  )
```

![](getting_started_files/figure-html/kennedy_plot-1.png)

Perfect, it looks like we can take the blue line straight there. Break a
leg!
