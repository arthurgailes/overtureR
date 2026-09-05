#' Convert dbplyr table to sf Object
#'
#' Collects a lazy dbplyr view and materializes it as an
#' in-memory `sf` table. `collect_sf` is a deprecated alias.
#'
#' The geometry column is read back as well-known binary and converted with
#' `sf::st_as_sfc()`. If the column is already binary (for example, after a
#' `mutate(geometry = ST_AsWKB(geometry))`), it is used as is. If it is
#' neither DuckDB `GEOMETRY` nor binary, the result is returned as a plain
#' data frame.
#'
#' @param x A lazy data frame backed by a database query.
#' @param geom_col The name of the geometry column. Will auto-detect names
#' matching 'geom'.
#' @param crs The coordinate reference system to use for the geometries,
#'   specified by its EPSG code. The default is 4326 (WGS 84).
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

  geom_type <- NULL
  if (isTRUE(has_geom)) {
    geom_type <- describe_columns(x)[[geom_col]]
  }

  # DuckDB GEOMETRY (reported as e.g. "GEOMETRY('OGC:CRS84')" from duckdb
  # 1.5) has to travel to R as WKB; a BLOB already is WKB
  is_geometry <- isTRUE(grepl("^GEOMETRY", geom_type))
  if (is_geometry) {
    x <- dplyr::mutate(x, {{ geom_col }} := ST_AsWKB(.data[[geom_col]]))
  }
  to_sf <- is_geometry || identical(geom_type, "BLOB")

  # hand the plain lazy table to dbplyr's collect with only the user's `...`
  class(x) <- setdiff(class(x), "overture_call")
  result <- dplyr::collect(x, ...)

  if (to_sf) {
    wkb <- structure(unclass(result[[geom_col]]), class = "WKB")
    result[[geom_col]] <- sf::st_as_sfc(wkb, crs = crs)
    result <- sf::st_as_sf(result, sf_column_name = geom_col)
  }

  result
}

#' @rdname collect.overture_call
collect_sf <- function(...) {
  warning("collect_sf is deprecated, use collect")
  dplyr::collect(...)
}

utils::globalVariables(c("ST_AsWKB"), package = "overtureR")
