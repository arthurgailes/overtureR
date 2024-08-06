#' Registeran sf object as a DuckDB virtual table
#' 
#' A thin wrapper around `duckdb::duckdb_register()` that creates a virtual 
#' table, then selects the geometry column to DuckDB.'s GEOMETRY type in the 
#' returned `dbplyr` representation. Mostly useful for join and spatial 
#' operations within DuckDB. No data is copied.
#' 
#' 
#' @inheritParams duckdb::duckdb_register
#' @param sf_obj sf object to be registered to duckdb
#' @param geom_only if TRUE, only the geometry column is registered. Always 
#' FALSE for sfc or sfg objects
#' @param... additional arguments passed to duckdb_register
#' 
#' 
#' @return a dbplyr tbl
#' 
#' @examples
#' library(sf)
#' 
#' con <- duckdb::dbConnect(duckdb::duckdb())
#' sf_obj = st_sf(a = 3, geometry = st_sfc(st_point(1:2)))
#' sf_as_duckdb_dbplyr(con, "test", sf_obj)
#' 
#' dbDisconnect(con)
#' 
#' @export 
sf_as_dbplyr <- function(
  conn, 
  name, 
  sf_obj, 
  geom_only = isFALSE("sf" %in% class(sf_obj)), 
  ...) {
  
  if(class(conn) != "duckdb_connection") stop("only supports duckdb connections")
  geom <- sf::st_as_text(sf::st_geometry(sf_obj), EWKT = TRUE)

  if (isFALSE("sf" %in% class(sf_obj))) geom_only <- TRUE

  if (isFALSE(geom_only)) {
    df <- sf::st_drop_geometry(sf_obj)
    df$geometry <- geom
  } else df <- data.frame(geometry = geom)

  duckdb::duckdb_register(conn, name = name, df = df, ...)

  res <- dplyr::tbl(conn, name)
  res <- dplyr::mutate(res, geometry = ST_GeomFromText(geometry))
  return(res)
}

utils::globalVariables(c("ST_GeomFromText"), package = "overtureR")