# Create or reuse a cached DuckDB connection

`stage_conn` is primarily intended for internal use by other `overtureR`
functions. However, it can be called directly by the user whenever it is
desirable to have direct access to the connection object. The core code
is copied from `duckdbfs`, which deserves all credit for the
implementation

## Usage

``` r
stage_conn(
  dbdir = ":memory:",
  read_only = FALSE,
  bigint = "numeric",
  config = list(),
  ...
)

strike_stage(conn = getOption("overturer_conn", NULL))
```

## Arguments

- dbdir:

  Location for database files. Should be a path to an existing directory
  in the file system. With the default (or `""`), all data is kept in
  RAM.

- read_only:

  Set to `TRUE` for read-only operation. For file-based databases, this
  is only applied when the database file is opened for the first time.
  Subsequent connections (via the same `drv` object or a `drv` object
  pointing to the same path) will silently ignore this flag.

- bigint:

  How 64-bit integers should be returned. There are two options:
  `"numeric"` and `"integer64"`. If `"numeric"` is selected, bigint
  integers will be treated as double/numeric. If `"integer64"` is
  selected, bigint integers will be set to bit64 encoding.

- config:

  Named list with DuckDB configuration flags, see
  <https://duckdb.org/docs/configuration/overview#configuration-reference>
  for the possible options. These flags are only applied when the
  database object is instantiated. Subsequent connections will silently
  ignore these flags.

- ...:

  Further arguments passed to
  [DBI::dbConnect](https://dbi.r-dbi.org/reference/dbConnect.html)

- conn:

  A duckdb connection. Defaults to the cached session connection, if
  there is one.

## Value

a
[`duckdb::duckdb()`](https://r.duckdb.org/reference/duckdb.html)connection
object

## Details

When first called (by a user or internal function), this function both
creates a duckdb connection and places that connection into a cache
(`overturer_conn` option). On subsequent calls, this function returns
the cached connection, rather than recreating a fresh connection. The
`dbdir`, `read_only`, `bigint`, and `config` arguments only take effect
when a connection is created.

This frees the user from the responsibility of managing a connection
object, because functions needing access to the connection can use this
to create or access the existing connection. At the close of the global
environment, this function's finalizer should gracefully shutdown the
connection before removing the cache.

`strike_stage` closes the connection.

## Examples

``` r

con <- stage_conn()
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpLueSXg/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
strike_stage(con)
```
