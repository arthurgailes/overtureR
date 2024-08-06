#' Retrieve (Spatially Filtered) Overture Datasets
#'
#' Fetches overture data from AWS.
#' If a bounding box is provided, it applies spatial filtering to only include
#' records within that area. The core code is copied from `duckdbfs`, which
#' deserves all credit for the implementation
#'
#' @param type A string specifying the type of overture dataset to read.
#' Setting to "*" or `NULL` will read all types for a given theme.
#' @param st_filter An object to spatially filter the result. 
#' @param theme Inferred from type by default. Must be set if type is "*" or NULL
#' @param conn A connection to a duckdb database.
#' @param as_sf If TRUE, return an sf dataframe
#' @param mode Either "view" (default) or "table". If "table", will download the
#' dataset into memory.
#' @param tablename The name of the table to create in the database.
#' @param union_by_name If TRUE, will execute a UNION by
#'  column name across all files (NOTE: this can add considerably to
#'  the initial execution time)
#' @param base_url Allows user to download data from a different mirror, such
#' as a beta or alpha release.
#' @param bbox alias for `st_filter`. may be deprecated in the future.
#'
#' @return An dbplyr lazy dataframe, or an sf dataframe if as_sf is TRUE
#'
#' @examplesIf interactive()
#' bbox <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)
#' open_curtain("building", bbox)
#' @export
open_curtain <- function(
    type,
    st_filter = NULL,
    theme = get_theme_from_type(type),
    conn = NULL,
    as_sf = FALSE,
    mode = "view",
    tablename = ifelse(is.null(type) | type == "*", theme, type),
    union_by_name = FALSE,
    base_url = "s3://overturemaps-us-west-2/release/2024-07-22.0",
    bbox = NULL) {
  # use cached connection if no conn provided
  if (is.null(conn)) conn <- stage_conn()
  config_extensions(conn)

  # should I expose this? Should it be set in cache_connection?
  duckdb::dbSendQuery(conn, "SET s3_region='us-west-2'")

  if(!is.null(bbox)) {
    warning("param `bbox` is deprecated. Use `st_filter`")
  }
  bbox <- set_bbox_sql(st_filter, mode)

  url <- glue::glue("{base_url}/theme={theme}/type={type}/*")
  # TODO: improve select, handle geometry internally
  interior_query <- glue::glue(
    "SELECT * REPLACE (ST_GeomFromWKB(geometry) as geometry)
     FROM read_parquet('{url}', filename=true, hive_partitioning=true, union_by_name = {union_by_name})"
  )

  st_filter <- sql_hanlde_st_filter(st_filter)

  query_suffix <- glue::glue("WHERE 1=1 {bbox} ")

  query <- glue::glue(
    "CREATE OR REPLACE {toupper(mode)} {tablename} AS
    ({interior_query} {query_suffix})"
  )

  DBI::dbExecute(conn, query)

  dataset <- dplyr::tbl(conn, tablename)
  if (isTRUE(as_sf)) dataset <- collect_sf(dataset)

  return(dataset)
}


# mapping specific overture dataset types to their corresponding thematic categories.
get_theme_from_type <- function(type) {
  theme <- type_theme_map[[type]]
  if (length(theme) != 1) {
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

# translate sf/list bounding box to SQL syntax
set_bbox_sql <- function(bbox, mode) {
  if (is.null(bbox)) {
    bbox <- ""
    if (mode == "table") warning("No bounding box set. Loading the full (possibly very large!) dataset into memory.")
    return(bbox)
  } 

  # sf objects
  if (any(grepl("^sf[c]$", class(bbox)))) bbox <- sf::st_bbox(bbox)

  # bbox or list objects
  xmin <- bbox[["xmin"]]
  ymin <- bbox[["ymin"]]
  xmax <- bbox[["xmax"]]
  ymax <- bbox[["ymax"]]

  if(any(is.null(c(xmin, ymin, xmax, ymax)))) stop("invalid `st_filter` object")

  bbox <- glue::glue(
    "AND bbox.xmin > {xmin}
    AND bbox.xmax < {xmax}
    AND bbox.ymin > {ymin}
    AND bbox.ymax < {ymax}"
  )

  return(bbox)
}

sql_handle_st_filter <- function(st_filter, conn) {
  if (is.null(st_filter)) return("")
  
  # for sf, upload geom only to duckdb
  if (any(grepl("^sf[c]$", class(bbox)))) {
    geom <- sf::st_as_sfc(st_filter)
    data <- as.data.frame(geometry = st_as_text(geom, EKWT = TRUE))

    duckdb::duckdb_register(
      conn, "internal_st_filter_init", st_filter, overwrite = TRUE
    )

    sql_init <- "(SELECT ST_GeomFromText(geometry) AS geometry FROM internal_st_filter_init)"
  }
  # if dbplyr, use sql subquery directly
  if("tbl_duckdb_connection" %in% class(tbl)) {
    if(!"geometry" %in% colnames(st_filter)) stop("`st_filter` must have a column named 'geometry'")
      
      sql_init <- dbplyr::sql_render(within)
      
  }
  # if char, use table/view name
  if (is.character(st_filter)) {
    is_valid <- length(st_filter) != 1
    existing <- duckdb::dbExistsTable(conn, st_filter)
    if(!is_valid | !existing) stop("if a string, `st_filter` must be a table in the connection")
      
    sql_init <- st_filter
    
  }
    
  agg_query <- "(SELECT ST_Union_Agg(geometry) AS geometry FROM {sql_init})"

  where_clause <- glue::glue("WHERE ST_Intersects(master.geometry, {agg_query})")
  return(where_clause)
  }