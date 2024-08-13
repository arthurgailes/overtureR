#' Convert dbplyr table to sf Object
#'
#' Collects a lazy dbplyr view and materializes it as an
#' in-memory `sf` table. `collect_sf` is a deprecated alias.
#'
#'
#' @param x A lazy data frame backed by a database query.
#' @param geom_col The name of the geometry column. Will auto-detect names
#' matching 'geom'.
#' @param crs The coordinate reference system to use for the geometries, specified
#'        by its EPSG code. The default is 4326 (WGS 84).
#' @param ... Further arguments passed to [dplyr::collect()].
#' @importFrom dplyr collect
#' @importFrom rlang .data :=
#'
#' @return An 'sf' object with the dataset converted to spatial features.
#' @examplesIf interactive()
#'
#' bbox <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)
#' lazy_tbl <- open_curtain("building", bbox)
#' collect(lazy_tbl)
#' @export
collect.overture_call <- function(x, ..., geom_col = "geometry", crs = 4326) {
  has_geom <- geom_col %in% colnames(x)

  # DuckDB Geoemtry class conversion. TODO: test for internal geometry type?
  if ("tbl_duckdb_connection" %in% class(x) & isTRUE(has_geom)) {
    x <- dplyr::mutate(x, {{ geom_col }} := ST_AsWKB(.data[[geom_col]]))
  }

  # Call the parent method (dbplyr::collect.tbl_sql)
  result <- NextMethod(x, ...)

  if (isTRUE(has_geom)) {
    result <- sf::st_as_sf(result, sf_column_name = geom_col, crs = crs)
  }

  return(result)
}

#' @rdname collect.overture_call
collect_sf <- function(...) {
  warning("collect_sf is deprecated, use collect")
  dplyr::collect(...)
}

utils::globalVariables(c("ST_AsWKB"), package = "overtureR")
