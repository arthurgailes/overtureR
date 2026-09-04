# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## What this package does

overtureR is an R package that provides a `dbplyr`/`sf` interface to
[Overture Maps](https://overturemaps.org/) data. It reads Overture’s
partitioned Parquet releases (from S3 or a local mirror) into DuckDB,
exposes them as lazy `dplyr` tables, and can materialize results as `sf`
spatial data frames. It also supports downloading (partitioning) subsets
to a local directory for offline/repeat use.

## Development commands

There is no Makefile; development is driven from R via `devtools`. See
`.dev/build.R` for the canonical sequence:

``` r

devtools::document()                          # regenerate NAMESPACE/man from roxygen comments
devtools::install(upgrade = FALSE)            # install local package
devtools::test()                              # run testthat suite
devtools::check(remote = TRUE, manual = FALSE)
devtools::check_win_devel()
revdepcheck::revdep_check(num_workers = 2)
devtools::build(manual = TRUE)
```

Run a single test file/case with testthat directly, e.g.
`devtools::load_all(); testthat::test_file("tests/testthat/test-class.R")`.

Lint with `lintr::lint_package()` (config in `.lintr`, uses
`linters_with_defaults()`).

**Tests hit the live Overture S3 dataset over the network.** Most tests
call `skip_if_offline()` and `skip_on_cran()` — expect network access to
be required locally, and don’t be surprised if CI-only failures relate
to the upstream Overture release rather than the code. Some tests
(`test-benchmark.R`) are timing references only and always `skip()`.

`README.md` is generated from `README.Rmd` (`devtools::build_readme()`
or knit it directly) — edit the `.Rmd`, never the `.md`, and re-knit
after changes (this regenerates `man/figures/README-*.png`).

## Architecture

The whole package is a thin orchestration layer over a single cached
DuckDB connection, using DuckDB’s `httpfs` + `spatial` extensions to
query Overture’s Parquet files in place.

**Connection lifecycle** (`R/stage_conn.R`):
[`stage_conn()`](https://arthurgailes.github.io/overtureR/reference/stage_conn.md)
lazily creates a DuckDB connection and caches it in
`options("overturer_conn")`, so all `overtureR` calls in a session share
one connection unless the user passes their own `conn`.
[`strike_stage()`](https://arthurgailes.github.io/overtureR/reference/stage_conn.md)
closes it. `utils.R::config_extensions()` ensures `httpfs`/`spatial` are
installed and loaded on that connection (with a workaround for a known
duckdb 1.1.3 bug).

**Query construction** (`R/open_curtain.R` — the main entry point): 1.
Builds a `read_parquet(...)` glob URL against `base_url` (an S3 release
by default, or a local directory — see `record_overture`), partitioned
by `theme=.../type=...`. 2. Spatial filtering has two layers, both built
as SQL string fragments: `set_stage_boundary()` narrows on the
partition-level `bbox` columns (cheap), and `focus_spotlight()` adds an
`ST_Intersects` predicate against the actual geometry, accepting a bbox,
an `sf`/`sfc` object, a table name, or another dbplyr `tbl_sql` as
`spatial_filter` (dispatched via `audition_data()`’s class sniffing). An
`sf` filter is uploaded via
[`sf_as_dbplyr()`](https://arthurgailes.github.io/overtureR/reference/sf_as_dbplyr.md)
rather than pulled into R. 3. Executes a `CREATE OR REPLACE VIEW|TABLE`
and returns a
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) wrapped
by
[`as_overture()`](https://arthurgailes.github.io/overtureR/reference/as_overture.md).
4. `get_theme_from_type()` (`R/get_theme_from_type.R`) maps a dataset
`type` (e.g. `"building"`) to its Overture `theme` (e.g. `"buildings"`)
via the static `type_theme_map` — required whenever `type` is
`"*"`/`NULL`.

**The `overture_call` class** (`R/as_overture.R`): a
`tbl_sql`/`tbl_lazy` subclass carrying an `overture_playbill` attribute
(`type`, `theme`). This is what lets
[`record_overture()`](https://arthurgailes.github.io/overtureR/reference/record_overture.md)
reconstruct a fresh
[`open_curtain()`](https://arthurgailes.github.io/overtureR/reference/open_curtain.md)
call against a new `base_url` after downloading, and what
`collect.overture_call` dispatches on.

**Collecting to sf** (`R/collect.R`):
[`collect.overture_call()`](https://arthurgailes.github.io/overtureR/reference/collect.overture_call.md)
converts a DuckDB-native `GEOMETRY` column to WKB before calling the
parent `dbplyr`/`dplyr` `collect()` method, then wraps the result with
[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html).
The result drops the `overture_call` class (it’s now an in-memory `sf`
object, not a lazy query).

**Local materialization** (`R/record_overture.R`): re-serializes the
current lazy query’s geometry to WKB, renders it to SQL
([`dbplyr::sql_render`](https://dbplyr.tidyverse.org/reference/sql_build.html)),
and runs a DuckDB
`COPY ... TO ... (FORMAT PARQUET, PARTITION_BY (theme, type))`,
preserving Overture’s own partition layout. Returns a new
`overture_call` pointed at the downloaded directory via
`open_curtain(base_url = output_dir)`.
[`snapshot_overture()`](https://arthurgailes.github.io/overtureR/reference/record_overture.md)
is a convenience wrapper defaulting to
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) +
`overwrite = TRUE`.

**sf ↔︎ DuckDB bridge** (`R/sf_as_dbplyr.R`): registers an in-memory `sf`
object as a DuckDB view without copying data twice — geometry goes over
as WKT text first (`*_init` view), then a second view casts it to
DuckDB’s `GEOMETRY` type. Used internally by `focus_spotlight()`, and
usable directly for ad hoc spatial joins between a local `sf` object and
Overture data.

## Conventions

- Public functions use the package’s “theater” naming metaphor
  (`open_curtain`, `stage_conn`, `strike_stage`, `record_overture`,
  `focus_spotlight`, `cast_extra`, `audition_data`) — keep new internal
  helpers consistent with that theme where it fits naturally, but don’t
  force it onto something it doesn’t suit.
- Roxygen with Markdown (`Roxygen: list(markdown = TRUE)`); document
  exported functions, run `devtools::document()` before committing
  generated `man/*.Rd`/`NAMESPACE` changes.
- Follow the tidyverse style guide (see `.github/CONTRIBUTING.md`);
  don’t run `styler` over unrelated code in the same PR.
- User-facing changes get a bullet at the top of `NEWS.md`.
