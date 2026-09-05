# Overture Maps: background for overtureR users

overtureR reads [Overture Maps](https://overturemaps.org/) data. Overture is an open
map dataset built by the Overture Maps Foundation. It publishes global, quality-checked
geographic data on a fixed schema, so the same columns appear release after release. This
file covers the Overture concepts an agent needs to query the data well. For how to call
the package, see `../SKILL.md`.

## Themes

Overture groups its data into six themes. `open_curtain(type)` reads one `type` inside a
theme, and infers the theme from the type (see the `type`->`theme` table in
`data-model.md`). Pick the theme that holds what you need:

| Theme | Holds | Use it when you need |
|-------|-------|----------------------|
| `buildings` | Building footprints and parts (`height`, `num_floors` where known) | Building shapes or heights |
| `places` | Points of interest - businesses, landmarks - with `categories`, `names`, `confidence` | Named locations or POIs by category |
| `transportation` | Road and rail `segment`s plus `connector`s | A street network or routing graph |
| `divisions` | Administrative areas and boundaries (country, region, county) | Boundaries or a place to clip other data to |
| `addresses` | Postal address points | Address-level points |
| `base` | Land, water, land use, land cover, infrastructure | Natural features or a generic base map layer |

## GERS: the stable `id`

Every Overture feature carries an `id` from the Global Entity Reference System (GERS). A
GERS id names one real-world entity - a specific building or road. Two things make it
useful:

1. **Join your own data to Overture.** Store the GERS `id` alongside your records, then
   match on it later.
2. **Track a feature across releases.** The same entity keeps its id from one release to
   the next, so you can compare a feature over time.

Re-fetch a known set of features by their ids:

```r
# ids you saved from an earlier query
my_ids <- c("08b2a100d2b1dfff...", "08b2a100d2b1c7ff...")

open_curtain("building", spatial_filter = bbox) |>
  filter(id %in% my_ids) |>
  collect()
```

For a large id set, register your table as a DuckDB view with `sf_as_dbplyr()` (or a plain
`dplyr::tbl`) and join on `id` inside the database. See the
[GERS documentation](https://docs.overturemaps.org/gers/) for how ids are assigned and
matched.

## Releases: cadence and the 60-day window

- **overtureR picks the release for you.** `open_curtain()` defaults to the latest release,
  found at call time by `latest_overture_release()`. You rarely set it yourself.
- **Overture publishes monthly.** Major schema changes land only in March, June, September,
  and December.
- **Old releases disappear after about 60 days.** Overture keeps only the two most recent
  monthly releases online. A release URL older than that stops working. To keep data for
  longer, download a local copy with `record_overture()` (see `../SKILL.md`).

To check the current state, do not rely on a number written here. Instead:

- Call `latest_overture_release()` for the release overtureR will use.
- Read the [release calendar](https://docs.overturemaps.org/release-calendar/) for dates
  and the current schema version.
- Read the [schema reference](https://docs.overturemaps.org/schema/) for the columns in the
  current release.

## Licensing and attribution

Overture data is open, but the license differs by theme. Most themes derived from
OpenStreetMap (`base`, `buildings`, `divisions`, `transportation`) use the Open Database
License (ODbL) and require crediting "OpenStreetMap contributors". `places` is mostly
CDLA Permissive 2.0. `addresses` varies by source country.

If you publish a map or product built on Overture data, you must attribute it. A general
credit is "Overture Maps Foundation, overturemaps.org", plus any source credit the theme
requires. The per-theme terms are the source of record, so read the
[attribution and licensing page](https://docs.overturemaps.org/attribution/) before you
publish.

## Developer tenets (context, not consumption)

Overture published six [developer tenets](https://docs.overturemaps.org/blog/2026/05/29/developer-tenets/)
that guide how it *builds* the data. They describe Overture's own engineering, not how you
query the data, but they explain why the dataset behaves as it does:

1. **Follow the footsteps of those before you** - respect existing patterns and conventions.
2. **Documentation beyond code** - write requirements, designs, and comments alongside code.
3. **Invest in stability** - identical inputs must produce identical outputs.
4. **Design for operations** - clear errors, logging, and documented failure modes.
5. **Our product is data; software is how we build it** - judge changes by their effect on the data.
6. **Balance cost and performance** - work within resource limits.

## Links and shortcuts

| Destination | Link |
|-------------|------|
| Overture docs home | https://docs.overturemaps.org/ |
| Getting data with DuckDB (overtureR's approach) | https://docs.overturemaps.org/getting-data/duckdb/ |
| Schema reference (columns per theme) | https://docs.overturemaps.org/schema/ |
| GERS (stable ids) | https://docs.overturemaps.org/gers/ |
| Attribution and licensing | https://docs.overturemaps.org/attribution/ |
| Release calendar | https://docs.overturemaps.org/release-calendar/ |
| Explore map viewer (browse features visually) | https://explore.overturemaps.org/ |
| Docs source on GitHub | https://github.com/OvertureMaps/docs |
