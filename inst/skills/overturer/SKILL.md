---
name: overturer
description: Use when querying Overture Maps data in R - buildings, places (POIs), roads/transportation, administrative divisions, addresses, land/water - or when you see open_curtain(), record_overture(), or an "overture_call" object, or need Overture data as a dbplyr lazy table or an sf data frame.
---

# overtureR

## Overview

overtureR queries [Overture Maps](https://overturemaps.org/) Parquet releases (from S3 or a local mirror) through DuckDB and exposes them as **lazy `dplyr` tables** you refine with normal dplyr verbs, then materialize as `sf`.

**Core mental model:** `open_curtain()` returns a *lazy* query, not data. You chain `filter()`/`mutate()`/`select()`, then `collect()` to pull results into R. Filtering happens in DuckDB against Parquet on S3 - so filter **before** collecting, or you try to download the planet.

## The one thing agents get wrong: nested struct columns

Overture columns like `names`, `bbox`, `categories`, and `sources` are **nested structs**. Access their fields with R's `$` **inside dplyr verbs** - dbplyr translates it to a DuckDB struct field access. Do NOT reach for `sql("names.primary")` or `struct_extract()`.

```r
# CORRECT - $ inside dplyr verbs
open_curtain("place", bbox) |>
  filter(categories$primary == "airport") |>       # struct field in filter
  transmute(id, name = names$primary, x = bbox$xmin) |>   # and in transmute
  collect()

# WRONG - do not do this
mutate(name = sql("names.primary"))          # unnecessary raw SQL
mutate(name = struct_extract(names, "primary"))
```

Common struct fields: `names$primary`, `bbox$xmin`/`$ymin`/`$xmax`/`$ymax`, `categories$primary`, `sources[[1]]$dataset`. See `references/data-model.md` for the schema and full `type`->`theme` table.

## Quick reference

| Function | Purpose |
|----------|---------|
| `open_curtain(type, spatial_filter, ...)` | Entry point. Returns a lazy `overture_call` (or sf if `as_sf = TRUE`). |
| `collect(x)` | Execute the query; returns an **sf** object (geometry auto-detected, CRS 4326). |
| `record_overture(x, output_dir, overwrite)` | Download the current query to a local Parquet mirror; returns a new `overture_call` pointed at it. |
| `snapshot_overture(x)` | `record_overture()` into `tempdir()` with `overwrite = TRUE`. |
| `stage_conn()` / `strike_stage()` | Get / close the cached session DuckDB connection. |
| `sf_as_dbplyr(conn, name, sf_obj)` | Register a local `sf` object as a DuckDB view (for in-DB joins) without copying. |
| `latest_overture_release()` | The release string `open_curtain()` defaults to. |

## Core workflow

```r
library(overtureR)
library(dplyr)

bbox <- c(xmin = -87.65, ymin = 41.87, xmax = -87.61, ymax = 41.89)  # named vector

open_curtain("building", spatial_filter = bbox) |>  # lazy; theme "buildings" inferred
  filter(!is.na(height)) |>                          # filter BEFORE dropping columns
  transmute(id, height, name = names$primary) |>     # geometry is kept automatically
  collect()                                          # -> sf, CRS 4326
```

Aggregate in the database when you don't need geometry - `summarise(mean(height, na.rm = TRUE)) |> pull()` runs in DuckDB, no download.

## `type` and `theme`

`open_curtain(type, ...)` infers `theme` from `type` (e.g. `"building"` -> `"buildings"`). To read a **whole theme**, pass `type = "*"` (or `NULL`) **and** set `theme` explicitly:

```r
open_curtain(type = "*", theme = "places")
```

Valid types: `building`, `building_part`, `place`, `segment`, `connector`, `division`, `division_area`, `division_boundary`, `address`, `infrastructure`, `land`, `land_cover`, `land_use`, `water`. Full mapping in `references/data-model.md`.

## `spatial_filter` accepts several types

A named numeric bbox vector `c(xmin=, ymin=, xmax=, ymax=)`, an `sf`/`sfc` object, an `st_bbox` object, a table name in the connection (string), or another dbplyr `tbl`. An `sf` filter is uploaded to DuckDB (not pulled into R) and applied as `ST_Intersects`. The bbox layer additionally prunes partitions cheaply, so **always pass a spatial_filter** for anything short of a global query.

## Local caching for repeat/offline use

```r
local <- open_curtain("building", bbox) |>
  record_overture(output_dir = "data/overture", overwrite = TRUE)
# `local` is a fresh overture_call reading local Parquet - same API, much faster
local |> filter(!is.na(height)) |> collect()
```

## Common mistakes

| Symptom | Cause / fix |
|---------|-------------|
| Query hangs / tries to download everything | No `spatial_filter`, or `collect()` before filtering. Filter in-DB first. |
| `filter()` on a column errors after `select`/`transmute` | You dropped it. Filter before selecting. |
| Reaching for `sql("names.primary")` | Use `names$primary` directly inside the dplyr verb. |
| "Could not find theme for the provided type" | `type = "*"`/`NULL` without a `theme`, or an unknown `type`. Set `theme`. |
| Result is a tibble, not sf | Geometry column not named `geometry`, or you called `dplyr::collect()` on a non-`overture_call`. |
| Tests/queries fail offline | Every query hits live Overture S3 unless you've `record_overture()`'d a local copy. |

## Overture background and links

overtureR reads [Overture Maps](https://overturemaps.org/), an open global map dataset on a
fixed schema. A few facts change how you query it:

- **GERS `id`.** Every feature has a stable id. Use it to join your own data to Overture and
  to track a feature across releases.
- **Releases expire.** Overture keeps only about 60 days of releases online, so an old
  release URL stops working. `open_curtain()` defaults to the latest via
  `latest_overture_release()`; download a local copy with `record_overture()` to keep data.
- **Attribution.** The data is open but the license differs by theme (many are ODbL and
  require crediting "OpenStreetMap contributors"). Attribute Overture in any published product.

Key links: [docs home](https://docs.overturemaps.org/) |
[schema reference](https://docs.overturemaps.org/schema/) |
[GERS](https://docs.overturemaps.org/gers/) |
[attribution](https://docs.overturemaps.org/attribution/) |
[explore map viewer](https://explore.overturemaps.org/). Full background - themes, GERS,
licensing, release cadence - is in `references/overture-context.md`.

## Conventions when editing this package

Public API uses a "theater" metaphor (`open_curtain`, `stage_conn`, `strike_stage`, `record_overture`, `focus_spotlight`, `cast_extra`, `audition_data`). Roxygen with Markdown; run `devtools::document()` after doc changes. User-facing changes get a `NEWS.md` bullet. See the repo's `CLAUDE.md` for the full development workflow.
