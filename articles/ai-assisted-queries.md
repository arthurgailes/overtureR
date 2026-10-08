# Ask Overture in Plain Language

You can point a language model at overtureR and ask it questions in
plain English. This article shows how. You type a request like
“buildings taller than five stories in downtown Chicago”; a local R
agent writes the matching
[`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)
pipeline, runs it, and hands you back an `sf` object you can map. Every
step below reports what it cost you in data and in time, so you can see
where the work happens.

The agent does not guess at the API. overtureR ships a
[skill](https://github.com/arthurgailes/overtureR/tree/master/inst/skills/overturer)
that teaches a model the package: the
lazy-then-[`collect()`](https://dplyr.tidyverse.org/reference/compute.html)
workflow, how to reach nested struct columns, and the mistakes to avoid.
The [btw](https://posit-dev.github.io/btw/) package discovers that skill
and feeds it to the model, so the code the model writes is code that
works.

The pieces fit together like this:

      your question
          |
          v
      ellmer chat  <--- btw feeds it the overtureR skill + your R session
          |
          v
      open_curtain(...) |> filter(...) |> collect()   (a real overtureR pipeline)
          |
          v
      sf object  --->  interactive map

Three packages do the work. [ellmer](https://ellmer.tidyverse.org/)
talks to the model, [btw](https://posit-dev.github.io/btw/) supplies
context, and [mapgl](https://walker-data.com/mapgl/) draws the result.

``` r

install.packages(c("overtureR", "ellmer", "btw", "mapgl"))
```

``` r

library(overtureR)
library(dplyr)
library(sf)
library(mapgl)
```

## Why the skill matters

Ask a model to query Overture Maps without the skill and it writes
plausible code that fails. It does not know Overture’s nested columns,
and it forgets that
[`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)
is lazy, so it tries to pull the planet into memory.

Here is the kind of answer you get **without** the skill:

``` r

# Without the skill: three common failures in four lines
open_curtain("building") |>                 # no spatial_filter: scans the globe
  collect() |>                               # collect() first: downloads everything
  filter(sql("names.primary") == "x") |>     # raw SQL for a struct field
  mutate(floors = height / 3)
```

Here is what the model writes **with** the skill loaded:

``` r

# With the skill: filter in the database, then collect
bbox <- c(xmin = -87.65, ymin = 41.87, xmax = -87.61, ymax = 41.89)

open_curtain("building", spatial_filter = bbox) |>  # prune to the area first
  filter(height > 15) |>                             # filter in DuckDB
  transmute(id, height, name = names$primary, geometry) |>  # keep geometry
  collect()                                          # only now pull to R
```

The skill turns confident-but-wrong code into a pipeline that runs. That
is the whole point of wiring it in.

## Wire up the agent

[`btw_client()`](https://posit-dev.github.io/btw/reference/btw_client.html)
builds an ellmer chat that already carries your project context,
including any skills it can find. Add one tool that runs a pipeline and
returns a short summary, and the agent can both write queries and see
their results.

``` r

library(ellmer)
library(btw)

# A tool the model can call to run one overtureR pipeline
run_overture <- tool(
  function(code) {
    result <- eval(parse(text = code))
    paste(utils::capture.output(print(result)), collapse = "\n")
  },
  name = "run_overture_query",
  description = paste(
    "Run one overtureR pipeline built with open_curtain() and dplyr verbs,",
    "ending in collect(). Returns a text summary of the resulting sf object."
  ),
  arguments = list(
    code = type_string("R code for a single overtureR pipeline.")
  )
)

# btw_client() discovers the overtureR skill and your session;
# then we add the query-runner tool.
chat <- btw_client()
chat$set_tools(c(chat$get_tools(), list(run_overture)))
```

## One question, end to end

Now ask a question:

``` r

chat$chat("Show me buildings taller than five stories in downtown Chicago.")
```

A well-primed agent answers in two steps, and you can watch each one’s
cost.

### Step 1: resolve the place

Overture carries administrative areas in the `division_area` type, so
the agent looks Chicago up by name rather than guessing coordinates.
Timing the call shows how long a name lookup takes:

``` r

place_time <- system.time(
  chicago <- open_curtain("division_area") |>
    filter(
      subtype == "locality",
      names$primary == "Chicago",
      region == "US-IL"
    ) |>
    transmute(name = names$primary, geometry) |>  # keep the geometry column
    collect()
)

sprintf(
  "Resolved %d boundary in %.1f seconds.",
  nrow(chicago),
  place_time[["elapsed"]]
)
#> [1] "Resolved 1 boundary in 7.6 seconds."
```

Place-name lookup is a manual step today: you filter `division_area` on
`names$primary`. A future `call_place()` helper will fold this into one
call, but the pattern above already works and is what the agent uses
now. Here is the boundary it found:

``` r

maplibre(
  style = carto_style("positron"),
  center = c(-87.73, 41.83),
  zoom = 9,
  height = "360px"
) |>
  add_fill_layer(
    id = "boundary",
    source = chicago,
    fill_color = "#3182bd",
    fill_opacity = 0.3
  )
```

### Step 2: fetch the buildings

The agent then queries buildings inside a tight downtown box and keeps
the tall ones. Five stories is roughly 15 meters. The spatial filter is
what keeps this fast: DuckDB prunes to the area before it reads any
geometry.

``` r

loop <- c(xmin = -87.645, ymin = 41.875, xmax = -87.615, ymax = 41.895)

building_time <- system.time(
  tall_buildings <- open_curtain("building", spatial_filter = loop) |>
    filter(height > 15) |>
    transmute(id, height, name = names$primary, geometry) |>
    collect()
)

sprintf(
  "Fetched %d buildings over 15 m in %.1f seconds; tallest is %.0f m.",
  nrow(tall_buildings),
  building_time[["elapsed"]],
  max(tall_buildings$height)
)
#> [1] "Fetched 1023 buildings over 15 m in 7.8 seconds; tallest is 340 m."
```

The tallest few give a sense of the data:

``` r

tall_buildings |>
  st_drop_geometry() |>
  arrange(desc(height)) |>
  slice_head(n = 5) |>
  knitr::kable(col.names = c("id", "height (m)", "name"), digits = 0)
```

| id | height (m) | name |
|:---|---:|:---|
| aef295ec-4ca7-4a15-9e0a-1b3a980d34f1 | 340 | Aon Center |
| c27c351c-332b-4e0e-9ecf-d904eb25024e | 307 | Franklin Center North Tower |
| e366709e-ed23-4ad3-9eb6-3e5edba512ea | 303 | Two Prudential Plaza |
| e2f8889a-572a-4259-a61c-514aab39839f | 278 | One Prudential Plaza |
| 5fa211b4-b64a-48a3-a7d3-42dbce854be8 | 267 | 400 Lake Shore - North Tower |

The result is an ordinary `sf` object, so you can map it. mapgl draws
the footprints as 3D blocks, extruded and colored by height:

``` r

maplibre(
  style = carto_style("positron"),
  center = c(-87.63, 41.885),
  zoom = 14,
  pitch = 55,
  bearing = -20,
  height = "520px"
) |>
  add_fill_extrusion_layer(
    id = "buildings",
    source = tall_buildings,
    fill_extrusion_height = get_column("height"),
    fill_extrusion_color = interpolate(
      column = "height",
      values = c(15, 100, 300),
      stops = c("#fde0dd", "#fa9fb5", "#7a0177")
    ),
    fill_extrusion_opacity = 0.9
  )
```

You asked in English and got a map. The model wrote the pipeline;
overtureR and DuckDB did the fetching, in the few seconds the timings
above report.

## Fan out across cities with subagents

One chat answers one question at a time. To compare several places at
once, run a subagent per place. ellmer’s
[`parallel_chat_structured()`](https://ellmer.tidyverse.org/reference/parallel_chat.html)
sends many prompts concurrently and returns tidy, typed results, so each
subagent turns a plain-language request into a structured query
specification.

``` r

library(ellmer)

# The shape we want back from every subagent
query_spec <- type_object(
  city = type_string("City name."),
  xmin = type_number("Western longitude of a downtown box."),
  ymin = type_number("Southern latitude of a downtown box."),
  xmax = type_number("Eastern longitude of a downtown box."),
  ymax = type_number("Northern latitude of a downtown box.")
)

requests <- list(
  "downtown Chicago",
  "downtown Denver",
  "downtown Austin"
)

# One subagent per city, all at once
specs <- parallel_chat_structured(
  chat = btw_client(),
  prompts = requests,
  type = query_spec
)
```

`specs` comes back as a data frame, one row per city. From there the
fetching is deterministic overtureR: map the specification over
[`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md),
combine, and quantify. To keep this article reproducible, we run the
same shape with fixed boxes rather than a live model:

``` r

downtowns <- tibble::tribble(
  ~city,     ~xmin,     ~ymin,    ~xmax,     ~ymax,
  "Chicago", -87.645,   41.875,   -87.615,   41.895,
  "Denver",  -105.005,  39.735,   -104.985,  39.755,
  "Austin",  -97.755,   30.260,   -97.735,   30.280
)

fanout_time <- system.time(
  tall_by_city <- downtowns |>
    purrr::pmap(function(city, xmin, ymin, xmax, ymax) {
      bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
      open_curtain("building", spatial_filter = bbox) |>
        filter(height > 15) |>
        transmute(height, city = !!city, geometry) |>
        collect()
    }) |>
    bind_rows()
)

sprintf(
  "Fetched %d tall buildings across %d cities in %.1f seconds.",
  nrow(tall_by_city),
  n_distinct(tall_by_city$city),
  fanout_time[["elapsed"]]
)
#> [1] "Fetched 1676 tall buildings across 3 cities in 3.3 seconds."
```

The count per city is the comparison you asked for:

``` r

tall_by_city |>
  st_drop_geometry() |>
  count(city, name = "tall_buildings") |>
  arrange(desc(tall_buildings)) |>
  knitr::kable()
```

| city    | tall_buildings |
|:--------|---------------:|
| Chicago |           1023 |
| Austin  |            327 |
| Denver  |            326 |

The subagents handle the ambiguous part, reading each request and naming
a box. overtureR handles the exact part, fetching the same clean query
for every city. Splitting the work this way keeps the language model
where it is strong and the database where it is precise.

## Practical notes

- **Pin a release for reproducible work.**
  [`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)
  defaults to the latest Overture release, and releases expire after
  about 60 days. Set `options(overturer_release = )` so a script returns
  the same data next month.
- **Attribute Overture.** The data is open, but many themes are ODbL and
  require crediting OpenStreetMap contributors. Credit Overture in
  anything you publish.
- **Watch the cost.** Every `chat$chat()` call spends tokens; run
  [`ellmer::token_usage()`](https://ellmer.tidyverse.org/reference/token_usage.html)
  to see the running total. Let the model draft the pipeline, then rerun
  the query yourself when you need it often.
- **Keep a human in the loop.** The model writes code that runs, not
  code that is guaranteed correct. Read the pipeline before you trust
  the map. \`\`\`
