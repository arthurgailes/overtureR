#' Retrieve (Spatially Filtered) Overture Datasets
#'
#' Fetches an Arrow dataset from S3 based on the specified `overture_type`.
#' If a bounding box is provided, it applies spatial filtering to only include records within that area.
#'
#' @param type A string specifying the type of overture dataset to read. Setting to `*` or `NULL` will read all types for a given theme.
#' @param bbox Optional bounding box to filter the records, expected as a vector
#'        of four numbers (xmin, ymin, xmax, ymax). Set to NULL to omit from query
#' @param theme Inferred from type by default. Must be set if
#' @return An object of class Arrow Datasets containing the filtered records.
#' @examplesIf interactive()
#' bbox <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)
#' open_curtain("building", bbox)
#' @export
open_curtain <- function(
  type,
  bbox,
  theme = get_theme_from_type(type),
  conn = NULL,
  as_sf = FALSE,
  mode = "view",
  tablename = ifelse(is.null(type) | type == "*", theme, type),
  union_by_name = FALSE,
  base_url = "s3://overturemaps-us-west-2/release/2024-07-22.0"
) {
  # use duckdbfs if no conn provided
  if(is.null(conn)) conn <- duckdbfs::cached_connection()

  # TODO: improve this.
  duckdbfs::duckdb_s3_config(conn, s3_region='us-west-2')

  bbox <- set_bbox_sql(bbox)


  url <- glue::glue('{base_url}/theme={theme}/type={type}/*')
  # TODO: improve select, handle geometry internally
  interior_query <- glue::glue(
    "SELECT *
     FROM read_parquet('{url}', filename=true, hive_partitioning=true, union_by_name = {union_by_name})"
  )

  query_suffix <- glue::glue("WHERE 1=1 {bbox} ")

  query <- glue::glue(
    "CREATE OR REPLACE {toupper(mode)} {tablename} AS
    ({interior_query} {query_suffix})"
  )

  DBI::dbExecute(conn, query)

  dataset <- dplyr::tbl(conn, tablename)
  if(isTRUE(as_sf)) dataset <- collect_sf(dataset)

  return(dataset)
}


#' mapping specific overture dataset types to their corresponding thematic categories.
get_theme_from_type <- function(type) {
  theme <- type_theme_map[[type]]
  if(length(theme) != 1) {
    stop("Could not find theme for the provided type, please enter manually")
  }

  return(theme)
}

type_theme_map <- list(
  address = "addresses",
  building = "buildings",
  building_part = "buildings",
  division = "divisions",
  division_area = "divisions",
  division_boundary = "divisions",
  place = "places",
  segment = "transportation",
  connector = "transportation",
  infrastructure = "base",
  land = "base",
  land_cover = "base",
  land_use = "base",
  water = "base"
)

#' translate sf/list bounding box to SQL syntax
set_bbox_sql <- function(bbox) {
  if (is.null(bbox)) bbox = ""
  else {
    xmin <- bbox[["xmin"]]
    ymin <- bbox[["ymin"]]
    xmax <- bbox[["xmax"]]
    ymax <- bbox[["ymax"]]

    bbox <- glue::glue(
      "AND bbox.xmin > {xmin}
      AND bbox.xmax < {xmax}
      AND bbox.ymin > {ymin}
      AND bbox.ymax < {ymax}"
    )
  }
  return (bbox)
}
