#' Retrieve (Spatially Filtered) Overture Datasets
#'
#' Fetches overture data from AWS.
#' If a spatial filter is provided, it applies spatial filtering to only
#' include records within that area. The core code is copied from `duckdbfs`,
#' which deserves all credit for the implementation
#'
#' @param type A string specifying the type of overture dataset to read.
#' Setting to "*" or `NULL` will read all types for a given theme. See
#' [overture_types()] for the valid values.
#' @param spatial_filter An object to spatially filter the result: a named
#' numeric vector or `sf::st_bbox()` bounding box, an `sf` or `sfc` object, the
#' name of a table in `conn`, or another `dbplyr` lazy table with a `geometry`
#' column. `sf` filters in another coordinate reference system are transformed
#' to EPSG:4326 (Overture's) before filtering.
#' @param theme Inferred from type by default. Must be set if type is "*" or
#' `NULL`.
#' @param conn A connection to a duckdb database.
#' @param as_sf If TRUE, return an sf dataframe
#' @param mode Either "view" (default) or "table". If "table", will download the
#' dataset into memory.
#' @param tablename The name of the table to create in the database.
#' @param read_opts A named list of key-value pairs passed to
#' \href{https://duckdb.org/docs/data/parquet/overview.html#parameters}{DuckDB's read_parquet}
#' @param base_url Allows user to download data from a different mirror, such
#' as a local directory, or a alternative release. Defaults to the latest
#' Overture release, discovered via [latest_overture_release()].
#' @param bbox alias for `spatial_filter`. may be deprecated in the future.
#'
#' @details
#' When `spatial_filter` is set and `base_url` points at an Overture release,
#' `open_curtain()` reads only the Parquet files whose bounding box touches
#' the filter, using the file list in Overture's STAC catalog (see
#' [overture_types()] and [clear_overture_cache()]). This turns a cold query
#' over hundreds of files into one over a handful. Set
#' `options(overturer_prune = FALSE)` to always read the whole partition.
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
    base_url = paste0(
      "s3://overturemaps-us-west-2/release/", latest_overture_release(conn)
    ),
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
  if (is.null(type)) type <- "*"

  # bring sf-style filters into Overture's coordinate system once, up front
  has_crs <- audition_data(spatial_filter) %in% c("sf", "bbox")
  if (!is.null(spatial_filter) && has_crs) {
    spatial_filter <- stage_crs(spatial_filter)
  }

  filter_bbox <- stage_bbox(conn, spatial_filter)
  bbox <- set_stage_boundary(conn, spatial_filter, bbox = filter_bbox)
  spatial_query <- focus_spotlight(conn, spatial_filter)

  if (is.null(tablename)) tablename <- cast_extra(conn, theme, type)

  url <- spotlight_files(conn, base_url, theme, type, filter_bbox)
  # TODO: improve select, handle geometry internally

  read_opts <- process_parquet_read_opts(read_opts)

  geometry <- if (duckdb_native_geometry()) {
    ""
  } else {
    "REPLACE (ST_GeomFromWKB(geometry) as geometry)"
  }

  interior_query <- glue::glue(
    "SELECT * {geometry}
     FROM read_parquet({url}, {read_opts})"
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

  dataset
}

# The read_parquet() source: a list of the files that touch the filter's bbox
# when the catalog can tell us, else the wildcard path for the partition.
spotlight_files <- function(conn, base_url, theme, type, filter_bbox) {
  wildcard <- glue::glue("'{base_url}/theme={theme}/type={type}/*'")

  release <- release_from_url(base_url)
  prune <- isTRUE(getOption("overturer_prune", TRUE)) &&
    !is.null(filter_bbox) && !is.null(release)
  if (!prune) {
    return(wildcard)
  }

  types <- if (identical(type, "*")) {
    all_types <- overture_types(release, conn = conn)
    all_types$type[all_types$theme == theme]
  } else {
    type
  }
  if (length(types) == 0) {
    return(wildcard)
  }

  files <- lapply(types, function(ty) {
    prune_files(conn, base_url, release, theme, ty, filter_bbox)
  })
  # any type we can't prune means we can't build a complete list
  if (any(vapply(files, is.null, logical(1)))) {
    return(wildcard)
  }

  files <- unlist(files)
  paste0("[", paste0("'", files, "'", collapse = ", "), "]")
}

process_parquet_read_opts <- function(opts) {
  default_read_opts <- list(
    filename = FALSE,
    hive_partitioning = TRUE,
    union_by_name = FALSE
  )

  parquet_opts <- utils::modifyList(default_read_opts, opts)

  paste(
    names(parquet_opts), parquet_opts,
    sep = "=", collapse = ", "
  )
}

focus_spotlight <- function(conn, spatial_filter) {
  if (is.null(spatial_filter)) {
    return("")
  }

  # class test. Put in own function?
  spatial_class <- audition_data(spatial_filter)

  if (grepl("bbox", spatial_class)) {
    return("")
  } # processed as bbox directly

  # for sf/sfc, upload the geometry to duckdb and keep only its union in a
  # small table, so the upload isn't held in R memory or re-aggregated on
  # every query
  if (spatial_class == "sf") {
    spatial_filter <- stage_crs(spatial_filter)
    name <- cast_extra(conn, "spotlight", "spotlight")
    sf_as_dbplyr(
      conn, paste0(name, "_upload"),
      sf_obj = spatial_filter, overwrite = TRUE, geom_only = TRUE
    )
    DBI::dbExecute(conn, glue::glue(
      "CREATE TEMP TABLE {name} AS
       (SELECT ST_Union_Agg(geometry) AS geometry FROM {name}_upload)"
    ))
    DBI::dbExecute(conn, glue::glue("DROP VIEW {name}_upload"))
    duckdb::duckdb_unregister(conn, paste0(name, "_upload_init"))

    return(glue::glue(
      "AND ST_Intersects(master.geometry, (SELECT geometry FROM {name}))"
    ))
  }

  # if char, convert to dbplyr
  if (spatial_class == "tablename") {
    is_valid <- length(spatial_filter) == 1
    existing <- is_valid && duckdb::dbExistsTable(conn, spatial_filter)
    if (!is_valid || !existing) {
      stop("if a string, `spatial_filter` must be a table in the connection")
    }

    sql_init <- spatial_filter
  }
  # if dbplyr, use sql subquery directly
  if (spatial_class == "dbplyr") {
    if (!"geometry" %in% colnames(spatial_filter)) {
      stop("`spatial_filter` must have a column 'geometry' of class GEOMETRY")
    }

    sql_init <- paste0("(", dbplyr::sql_render(spatial_filter), ")")
  }

  agg_query <- glue::glue(
    "(SELECT ST_Union_Agg(geometry) AS geometry FROM {sql_init})"
  )

  glue::glue("AND ST_Intersects(master.geometry, {agg_query})")
}

# Overture data is in EPSG:4326. Bring an sf/sfc/bbox filter into the same
# system so the bbox and geometry tests compare like with like.
stage_crs <- function(x) {
  crs <- sf::st_crs(x)
  if (is.na(crs)) {
    warning(
      "`spatial_filter` has no coordinate reference system; ",
      "assuming EPSG:4326 (longitude, latitude)",
      call. = FALSE
    )
    return(sf::st_set_crs(x, 4326))
  }
  if (crs == sf::st_crs(4326)) {
    return(x)
  }
  if (inherits(x, "bbox")) {
    return(sf::st_bbox(sf::st_transform(sf::st_as_sfc(x), 4326)))
  }
  sf::st_transform(x, 4326)
}

# The filter's bounding box as a named numeric vector in EPSG:4326, or NULL
# when there is no filter.
stage_bbox <- function(conn, spatial_filter) {
  if (is.null(spatial_filter)) {
    return(NULL)
  }

  spatial_class <- audition_data(spatial_filter)
  corners <- c("xmin", "ymin", "xmax", "ymax")

  if (spatial_class == "bbox_vector") {
    named <- all(corners %in% names(spatial_filter))
    if (length(spatial_filter) != 4 || !named) {
      stop(
        "a numeric `spatial_filter` must be a bounding box with names ",
        "xmin, ymin, xmax, ymax, e.g. c(xmin = -120.5, ymin = 35.5, ",
        "xmax = -120.0, ymax = 36.0)"
      )
    }
    bbox <- spatial_filter[corners]
  }
  if (spatial_class %in% c("bbox", "sf")) {
    bbox <- sf::st_bbox(stage_crs(spatial_filter))
  }

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

  bbox <- round(as.numeric(bbox[corners]), 10)
  names(bbox) <- corners

  if (anyNA(bbox)) stop("invalid `spatial_filter` object")

  bbox
}

# translate bounding box to SQL syntax
set_stage_boundary <- function(
  conn,
  spatial_filter,
  bbox = stage_bbox(conn, spatial_filter)
) {
  if (is.null(bbox)) {
    return("")
  }

  glue::glue(
    "AND bbox.xmax >= {bbox[['xmin']]}
    AND bbox.xmin <= {bbox[['xmax']]}
    AND bbox.ymax >= {bbox[['ymin']]}
    AND bbox.ymin <= {bbox[['ymax']]}"
  )
}

audition_data <- function(spatial_filter) {
  if (is.null(spatial_filter)) {
    NULL
  } else if (inherits(spatial_filter, "bbox")) {
    "bbox"
  } else if (any(grepl("^sf[cg]?$", class(spatial_filter)))) {
    "sf"
  } else if (is.numeric(spatial_filter)) {
    "bbox_vector"
  } else if (is.character(spatial_filter)) {
    "tablename"
  } else if (inherits(spatial_filter, "tbl_sql")) {
    "dbplyr"
  } else {
    stop("invalid `spatial_filter` object")
  }
}

cast_extra <- function(conn, theme, type) {
  use_theme <- is.null(type) || type == "*"
  tablename <- paste0("overtureR_", ifelse(use_theme, theme, type))

  view_exists <- duckdb::dbExistsTable(conn, tablename)

  i <- 0
  while (isTRUE(view_exists)) {
    i <- i + 1
    if (i > 1e3) {
      stop(
        "Over 1,000 iterations of this table in duckdb. ",
        "If this is intentional please supply `tablename` to continue"
      )
    }

    tablename <- paste0("overtureR_", ifelse(use_theme, theme, type), i)
    view_exists <- duckdb::dbExistsTable(conn, tablename)
  }
  tablename
}
