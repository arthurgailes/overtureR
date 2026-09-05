#' Register an sf object as a DuckDB virtual table
#'
#' A thin wrapper around `duckdb::duckdb_register()` that creates a virtual
#' table, then casts the geometry column to DuckDB's GEOMETRY type in the
#' returned `dbplyr` representation. Mostly useful for join and spatial
#' operations within DuckDB. No data is copied.
#'
#'
#' @inheritParams duckdb::duckdb_register
#' @param sf_obj sf object to be registered to duckdb
#' @param geom_only if TRUE, only the geometry column is registered. Always
#' TRUE for sfc or sfg objects
#' @param ... additional arguments passed to duckdb_register
#'
#' @details
#' Behind the scenes, this function creates an initial view (`name`_init) with
#' the geometry stored as well-known binary via `sf::st_as_binary`. It then
#' creates the view `name` which replaces the geometry column with DuckDB's
#' internal geometry type. DuckDB geometries carry no coordinate reference
#' system; the geometry is registered in whatever system `sf_obj` uses.
#'
#' @return a `dbplyr` lazy table
#'
#' @examplesIf interactive()
#' library(sf)
#'
#' con <- stage_conn()
#' sf_obj <- st_sf(a = 3, geometry = st_sfc(st_point(1:2)))
#' sf_as_dbplyr(con, "test", sf_obj)
#'
#' DBI::dbDisconnect(con)
#'
#' @export
sf_as_dbplyr <- function(
    conn,
    name,
    sf_obj,
    geom_only = isFALSE(inherits(sf_obj, "sf")),
    overwrite = FALSE,
    ...) {
  if (!inherits(conn, "duckdb_connection")) {
    stop("only supports duckdb connections")
  }
  config_extensions(conn)

  wkb <- sf::st_as_binary(sf::st_geometry(sf_obj))
  geom <- structure(unclass(wkb), class = c("blob", "vctrs_vctr", "list"))

  if (isFALSE(inherits(sf_obj, "sf"))) geom_only <- TRUE

  if (isFALSE(geom_only)) {
    df <- sf::st_drop_geometry(sf_obj)
    df$geometry <- geom
  } else {
    df <- data.frame(geometry = seq_along(geom))
    df$geometry <- geom
  }

  duckdb::duckdb_register(
    conn,
    name = paste0(name, "_init"),
    df = df,
    overwrite = overwrite,
    ...
  )

  replace <- ifelse(isTRUE(overwrite), "OR REPLACE", "")
  DBI::dbExecute(conn, glue::glue(
    "CREATE {replace} VIEW {name} AS
    (SELECT * REPLACE ST_GeomFromWKB(geometry) AS geometry FROM {name}_init)"
  ))

  dplyr::tbl(conn, name)
}

utils::globalVariables(c("ST_GeomFromWKB"), package = "overtureR")
