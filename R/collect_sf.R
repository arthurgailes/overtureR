#' Convert dbplyr table to sf Object
#'
#' Collects a lazy dbplyr view and materializes it as an
#' in-memory `sf` table.
#'
#' @param tbl A dbplyr view object containing a 'geometry' column.
#' @param geom_col The name of the geometry column. Will auto-detect names
#' matching 'geom'.
#' @param crs The coordinate reference system to use for the geometries, specified
#'        by its EPSG code. The default is 4326 (WGS 84).
#' @return An 'sf' object with the dataset converted to spatial features.
#' @examplesIf interactive()
#'
#' bbox <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)
#' lazy_tbl <- open_curtain("building", bbox)
#' collect_sf(lazy_tbl)
#' @export
collect_sf <- function(tbl, geom_col = "geometry", crs = 4326) {
  if (!geom_col %in% colnames(tbl)) stop("Could not find `geom_col`")

  # DuckDB Geoemtry class conversion. TODO: test for internal geometry type?
  if ("tbl_duckdb_connection" %in% class(tbl)) {
    tbl <- dplyr::mutate(tbl, {{ geom_col }} := ST_AsWKB(.data[[geom_col]]))
  }
  new_tbl <- dplyr::collect(tbl)

  new_tbl <- sf::st_as_sf(new_tbl, sf_column_name = geom_col)

  if (!is.na(crs)) sf::st_crs(new_tbl) <- crs

  return(new_tbl)
}


utils::globalVariables(c("ST_AsWKB"), package = "overtureR")
