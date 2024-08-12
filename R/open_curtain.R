#' Retrieve (Spatially Filtered) Overture Datasets
#'
#' Fetches overture data from AWS.
#' If a bounding box is provided, it applies spatial filtering to only include
#' records within that area. The core code is copied from `duckdbfs`, which
#' deserves all credit for the implementation
#'
#' @param type A string specifying the type of overture dataset to read.
#' Setting to "*" or `NULL` will read all types for a given theme.
#' @param spatial_filter An object to spatially filter the result.
#' @param theme Inferred from type by default. Must be set if type is "*" or NULL
#' @param conn A connection to a duckdb database.
#' @param as_sf If TRUE, return an sf dataframe
#' @param mode Either "view" (default) or "table". If "table", will download the
#' dataset into memory.
#' @param tablename The name of the table to create in the database.
#' @param read_opts A named list of key-value pairs passed to
#' \href{https://duckdb.org/docs/data/parquet/overview.html#parameters}{DuckDB's read_parquet}
#' @param base_url Allows user to download data from a different mirror, such
#' as a beta or alpha release.
#' @param bbox alias for `spatial_filter`. may be deprecated in the future.
#'
#' @return An dbplyr lazy dataframe, or an sf dataframe if as_sf is TRUE
#'
#' @examplesIf interactive()
#' bbox <- c(xmin = -120.5, ymin = 35.5, xmax = -120.0, ymax = 36.0)
#' open_curtain("building", bbox)
#' @export
open_curtain <- function(
    type,
    spatial_filter = NULL,
    theme = get_theme_from_type(type),
    conn = NULL,
    as_sf = FALSE,
    mode = "view",
    tablename = NULL,
    read_opts = list(),
    base_url = "s3://overturemaps-us-west-2/release/2024-07-22.0",
    bbox = NULL) {
  # use cached connection if no conn provided
  if (is.null(conn)) conn <- stage_conn()
  config_extensions(conn)

  # should I expose this? Should it be set in cache_connection?
  DBI::dbExecute(conn, "SET s3_region='us-west-2'")

  if (!is.null(bbox)) {
    warning("param `bbox` is deprecated. Use `spatial_filter`")
    if (is.null(spatial_filter)) spatial_filter <- bbox
  }
  bbox <- set_stage_boundary(conn, spatial_filter)
  spatial_query <- focus_spotlight(conn, spatial_filter)

  if (is.null(tablename)) tablename <- cast_extra(conn, theme, type)

  url <- glue::glue("{base_url}/theme={theme}/type={type}/*")
  # TODO: improve select, handle geometry internally

  read_opts <- process_parquet_read_opts(read_opts)

  interior_query <- glue::glue(
    "SELECT * REPLACE (ST_GeomFromWKB(geometry) as geometry)
     FROM read_parquet('{url}', {read_opts})"
  )

  query_suffix <- glue::glue("WHERE 1=1 {bbox} {spatial_query} ")

  query <- glue::glue(
    "CREATE OR REPLACE {toupper(mode)} {tablename} AS
    (FROM ({interior_query}) AS master {query_suffix})"
  )

  DBI::dbExecute(conn, query)

  dataset <- dplyr::tbl(conn, tablename)
  dataset <- as_overture(dataset, type = type, theme = theme)

  if (isTRUE(as_sf)) dataset <- collect(dataset)

  return(dataset)
}

process_parquet_read_opts <- function(opts){
  default_read_opts <- list(
    filename = FALSE,
    hive_partitioning = TRUE,
    union_by_name = FALSE
  )

  parquet_opts <- modifyList(default_read_opts, opts)

  parquet_opts_str <- paste(
    names(parquet_opts), parquet_opts, sep = "=", collapse = ", "
  )

  return(parquet_opts_str)
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


focus_spotlight <- function(conn, spatial_filter) {
  if (is.null(spatial_filter)) {
    return("")
  }

  # class test. Put in own function?
  spatial_class <- audition_data(spatial_filter)

  if (grepl("bbox", spatial_class)) {
    return("")
  } # processed as bbox directly

  # for sf/sfc, upload geom only to duckdb
  if (spatial_class == "sf") {
    # give view a random suffix to prevent overwriting
    rnum <- round(abs(runif(1, max = 1e5)))
    sf_dbplyr <- sf_as_dbplyr(
      conn, paste0("overtureR_spatial_filter_", rnum),
      sf_obj = spatial_filter, overwrite = TRUE, geom_only = TRUE
    )

    sql_init <- paste0("(", dbplyr::sql_render(sf_dbplyr), ")")
  }

  # if char, convert to dbplyr
  if (spatial_class == "tablename") {
    is_valid <- length(spatial_filter) == 1
    existing <- duckdb::dbExistsTable(conn, spatial_filter)
    if (!is_valid | !existing) stop("if a string, `spatial_filter` must be a table in the connection")

    sql_init <- spatial_filter
  }
  # if dbplyr, use sql subquery directly
  if (spatial_class == "dbplyr") {
    if (!"geometry" %in% colnames(spatial_filter)) stop("`spatial_filter` must have a column 'geometry' of class GEOMETRY")

    sql_init <- paste0("(", dbplyr::sql_render(spatial_filter), ")")
  }

  agg_query <- glue::glue("(SELECT ST_Union_Agg(geometry) AS geometry FROM {sql_init})")

  where_clause <- glue::glue("AND ST_Intersects(master.geometry, {agg_query})")
  return(where_clause)
}


# translate bounding box to SQL syntax
set_stage_boundary <- function(conn, spatial_filter) {
  if (is.null(spatial_filter)) {
    return("")
  }

  spatial_class <- audition_data(spatial_filter)

  if (spatial_class == "bbox") bbox <- spatial_filter
  if (spatial_class %in% c("sf", "bbox_vector")) bbox <- sf::st_bbox(spatial_filter)

  # dbplyr
  if (spatial_class %in% c("tablename", "dbplyr")) {
    if (spatial_class == "dbplyr") {
      spatial_filter <- paste0("(", dbplyr::sql_render(spatial_filter), ")")
    }
    bbox_raw <- DBI::dbGetQuery(conn, glue::glue(
      "SELECT ST_AsWKB(ST_Envelope_AGG(geometry)) AS geometry
      FROM {spatial_filter}"
    ))
    bbox <- sf::st_bbox(sf::st_as_sfc(bbox_raw$geometry))
  }

  # bbox or list objects
  xmin <- round(bbox[["xmin"]], 10)
  ymin <- round(bbox[["ymin"]], 10)
  xmax <- round(bbox[["xmax"]], 10)
  ymax <- round(bbox[["ymax"]], 10)

  if (any(is.null(c(xmin, ymin, xmax, ymax)))) stop("invalid `spatial_filter` object")

  bbox <- glue::glue(
    "AND bbox.xmax >= {xmin}
    AND bbox.xmin <= {xmax}
    AND bbox.ymax >= {ymin}
    AND bbox.ymin <= {ymax}"
  )

  return(bbox)
}

audition_data <- function(spatial_filter) {
  if (is.null(spatial_filter)) {
    return(NULL)
  } else if (is.numeric(spatial_filter)) {
    return("bbox_vector")
  } else if ("bbox" %in% class(spatial_filter)) {
    return("bbox")
  } else if (any(grepl("^sf[cg]?$", class(spatial_filter)))) {
    return("sf")
  } else if (is.character(spatial_filter)) {
    return("tablename")
  } else if ("tbl_sql" %in% class(spatial_filter)) {
    return("dbplyr")
  } else {
    stop("invalid `spatial_filter` object")
  }
}

cast_extra <- function(conn, theme, type) {
  use_theme <- is.null(type) | type == "*"
  tablename <- paste0("overtureR_", ifelse(use_theme, theme, type))

  view_exists <- duckdb::dbExistsTable(conn, tablename)

  i <- 0
  while (isTRUE(view_exists)) {
    i <- i + 1
    if (i > 1e3) stop("Over 1,000 iterations of this table in duckdb. If this is intentional please supply `tablename` to continue")

    tablename <- paste0("overtureR_", ifelse(use_theme, theme, type), i)
    view_exists <- duckdb::dbExistsTable(conn, tablename)
  }
  return(tablename)
}
