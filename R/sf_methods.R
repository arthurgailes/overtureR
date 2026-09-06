#' Extent and coordinate reference system without collecting
#'
#' `sf::st_bbox()` and `sf::st_crs()` methods for lazy `overture_call` tables,
#' so you can read a query's extent and coordinate reference system straight
#' from DuckDB, without pulling the rows into R with [collect()].
#'
#' `st_bbox()` runs one `ST_Extent_Agg()` query over the geometry column.
#' `st_crs()` reads the coordinate reference system from DuckDB's typed
#' `GEOMETRY` column (for example `GEOMETRY('OGC:CRS84')` on duckdb >= 1.5),
#' falling back to EPSG:4326, which is what Overture stores.
#'
#' @param obj,x An `overture_call` object, as returned by [open_curtain()].
#' @param ... Unused, for compatibility with the generics.
#'
#' @return `st_bbox()` returns an `sf::st_bbox()` object; `st_crs()` returns an
#'   `sf::st_crs()` object.
#'
#' @examplesIf interactive()
#' bbox <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)
#' buildings <- open_curtain("building", bbox)
#' sf::st_crs(buildings)
#' sf::st_bbox(buildings)
#' @name overture_sf_methods
NULL

#' @rdname overture_sf_methods
#' @exportS3Method sf::st_crs
st_crs.overture_call <- function(x, ...) {
  overture_crs(describe_columns(x)[["geometry"]])
}

#' @rdname overture_sf_methods
#' @exportS3Method sf::st_bbox
st_bbox.overture_call <- function(obj, ...) {
  conn <- dbplyr::remote_con(obj)
  sql <- dbplyr::remote_query(obj)
  extent <- DBI::dbGetQuery(conn, glue::glue(
    "SELECT ST_AsWKB(ST_Extent_Agg(geometry)) AS geometry FROM ({sql}) AS q"
  ))
  bbox <- sf::st_bbox(sf::st_as_sfc(extent$geometry))
  sf::st_crs(bbox) <- sf::st_crs(obj)
  bbox
}

# The coordinate reference system named in a DuckDB GEOMETRY type string, such
# as the "OGC:CRS84" in "GEOMETRY('OGC:CRS84')". OGC:CRS84 is EPSG:4326 in
# longitude, latitude order. Older duckdb reports a bare "GEOMETRY"; Overture
# is always EPSG:4326, so that is the fallback.
overture_crs <- function(geom_type) {
  token <- sub("^GEOMETRY\\('(.*)'\\)$", "\\1", geom_type %||% "")
  unmatched <- identical(token, geom_type) || !nzchar(token)
  if (unmatched || identical(token, "OGC:CRS84")) {
    return(sf::st_crs(4326))
  }
  crs <- tryCatch(sf::st_crs(token), error = function(e) sf::st_crs(NA))
  if (is.na(crs)) sf::st_crs(4326) else crs
}
