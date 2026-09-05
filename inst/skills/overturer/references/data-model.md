# overtureR data model

## `type` -> `theme` mapping

`open_curtain(type)` infers `theme` from this table. When `type` is `"*"` or `NULL`, you must pass `theme` yourself.

| `type` | `theme` |
|--------|---------|
| `address` | `addresses` |
| `building` | `buildings` |
| `building_part` | `buildings` |
| `division` | `divisions` |
| `division_area` | `divisions` |
| `division_boundary` | `divisions` |
| `place` | `places` |
| `segment` | `transportation` |
| `connector` | `transportation` |
| `infrastructure` | `base` |
| `land` | `base` |
| `land_cover` | `base` |
| `land_use` | `base` |
| `water` | `base` |

Read a whole theme with e.g. `open_curtain(type = "*", theme = "base")`.

## Nested struct columns

Overture's schema nests many attributes in structs. Inside dplyr verbs, reach fields
with `$`; dbplyr translates this to DuckDB struct access. `sources` is a list of
structs, so index with `[[1]]` first.

| Expression | Meaning |
|------------|---------|
| `names$primary` | Primary display name (most common lookup) |
| `names$common` | Localized/common names (nested further) |
| `bbox$xmin`, `bbox$ymin`, `bbox$xmax`, `bbox$ymax` | Per-row bounding box (cheap coordinates without touching geometry) |
| `categories$primary` | Primary category (places; e.g. `"airport"`, `"restaurant"`) |
| `categories$alternate` | Alternate categories (list) |
| `sources[[1]]$dataset` | Source dataset of the first provenance record |
| `sources[[1]]$record_id` | Source record id |

Text search on names uses base R string functions inside `filter()`, which dbplyr
pushes down to SQL:

```r
filter(grepl("Kennedy Center", names$primary))
filter(names$primary == "Ronald Reagan Washington National Airport")
```

## Geometry

- Every dataset has a `geometry` column. `collect()` on an `overture_call` converts
  DuckDB's native `GEOMETRY` to `sf` (CRS **4326** by default; override with `crs =`).
- `geometry` is retained through `transmute()`/`select()` only if you keep it, but
  `open_curtain()` output already carries it - name any geometry column `geometry` so
  `collect()` auto-detects it.
- Coordinates live in `bbox$*`; use those for quick numeric filtering or plotting
  centroids without materializing full geometry.

## Selected common attribute columns

Top-level (non-struct) columns worth knowing:

- **buildings**: `height` (m, often `NA`), `num_floors`, `class`, `subtype`.
- **places**: `confidence` (0-1; filter e.g. `confidence > 0.9`), `websites`, `phones`.
- **divisions / division_area**: `subtype` (`"country"`, `"region"`, `"county"`, ...),
  `country` (ISO), `region` (e.g. `"US-PA"`), `division_id`.
- **transportation (segment)**: `subtype` (`"road"`, `"rail"`, ...), `class`, `connectors`.

Column availability changes across Overture releases - when unsure, `collect()` a small
spatially-filtered sample and inspect `names()`/`glimpse()`.
