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
collect_sf <- function(tbl, geom_col = NULL, crs = 4326) {
  if(is.null(geom_col)) {
    geom_col <- grep("geom", colnames(tbl), ignore.case = TRUE, value = TRUE)
  }

  if(length(geom_col) != 1) stop("could not determine geometry column")

  tbl <- dplyr::collect(tbl)

  tbl[[geom_col]] <- sf::st_as_sfc(tbl[[geom_col]], crs = crs)
  tbl <- sf::st_as_sf(tbl)
  return(tbl)
}

