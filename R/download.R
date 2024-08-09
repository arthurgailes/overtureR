
#' Write Overture Data to File or Directory
#'
#' Writes Overture data to a file or directory, using DuckDB for efficient, low-memory operations.
#' If a directory is specified, it maintains the Overture Maps partitioning structure and uses parquet format.
#'
#' @param data A dbplyr lazy query, an sf object, or NULL if using open_curtain parameters.
#' @param conn A DuckDB connection. If NULL, uses the cached connection from stage_conn().
#' @param output A string specifying the output file path or directory.
#' @param format A string specifying the output format. One of "geojson", "geojsonseq", or "parquet".
#'               If NULL (default), it's inferred from the output path for files, or set to "parquet" for directories.
#' @param ... Additional arguments passed to open_curtain if data is NULL.
#'
#' @return Invisibly returns NULL.
#' @export
write_overture <- function(data = NULL, conn = NULL, output, format = NULL, ...) {
  if (is.null(conn)) conn <- stage_conn()

  is_directory <- dir.exists(output) || (!file.exists(output) && !grepl("\\.[^.]+$", output))
  format <- infer_format(output, format, is_directory)

  prepared_data <- prepare_data(data, conn, ...)

  if (is_directory) {
    write_to_directory(conn, prepared_data$temp_objects$temp_view_name, output)
  } else {
    write_to_file(conn, prepared_data$temp_objects$temp_view_name, output, format)
  }

  # Clean up
  DBI::dbExecute(conn, glue::glue("DROP VIEW IF EXISTS {prepared_data$temp_objects$temp_view_name}"))
  if (!is.null(prepared_data$temp_objects$temp_table_name)) {
    DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {prepared_data$temp_objects$temp_table_name}"))
  }

  invisible(NULL)
}

#' Infer or set format based on output
#'
#' @param output A string specifying the output file path or directory.
#' @param format A string specifying the output format, or NULL.
#' @param is_directory A boolean indicating if the output is a directory.
#' @return A string specifying the format.
infer_format <- function(output, format, is_directory) {
  if (is.null(format)) {
    if (is_directory) {
      return("parquet")
    } else {
      ext <- tools::file_ext(output)
      format <- switch(ext,
        geojson = "geojson",
        geojsonseq = "geojsonseq",
        parquet = "parquet",
        stop("Could not infer format from output. Please specify format explicitly.")
      )
    }
  }

  if (!format %in% c("geojson", "geojsonseq", "parquet")) {
    stop("Invalid format. Choose 'geojson', 'geojsonseq', or 'parquet'.")
  }

  format
}

#' Prepare data for writing
#'
#' @inheritParams write_overture
#' @return A list containing the prepared data and any temporary objects created.
prepare_data <- function(data, conn, ...) {
  temp_objects <- list()

  if (is.null(data)) {
    data <- open_curtain(..., conn = conn)
  } else if (inherits(data, "sf")) {
    temp_table_name <- paste0("temp_sf_", format(Sys.time(), "%Y%m%d%H%M%S"))
    sf_as_dbplyr(conn, temp_table_name, data, overwrite = TRUE)
    data <- dplyr::tbl(conn, temp_table_name)
    temp_objects$temp_table_name <- temp_table_name
  } else if (!inherits(data, "tbl_lazy")) {
    stop("Data must be a dbplyr lazy query, an sf object, or NULL (to use open_curtain parameters).")
  }

  query <- dbplyr::sql_render(data)
  temp_view_name <- paste0("temp_view_", format(Sys.time(), "%Y%m%d%H%M%S"))
  DBI::dbExecute(conn, glue::glue("CREATE TEMPORARY VIEW {temp_view_name} AS ({query})"))
  temp_objects$temp_view_name <- temp_view_name

  list(data = data, temp_objects = temp_objects)
}

#' Write data to directory
#'
#' @param conn A DuckDB connection.
#' @param temp_view_name Name of the temporary view containing the data.
#' @inheritParams write_overture
write_to_directory <- function(conn, temp_view_name, output) {
  dir.create(output, showWarnings = FALSE, recursive = TRUE)

  type_theme <- DBI::dbGetQuery(conn, glue::glue("SELECT DISTINCT type, theme FROM {temp_view_name} LIMIT 1"))

  if (nrow(type_theme) == 0) {
    stop("Unable to determine type and theme from the data.")
  }

  partitioned_output <- file.path(output, glue::glue("theme={type_theme$theme}/type={type_theme$type}"))
  dir.create(partitioned_output, showWarnings = FALSE, recursive = TRUE)

  DBI::dbExecute(conn, glue::glue("
    COPY {temp_view_name} TO '{partitioned_output}'
    (FORMAT 'parquet', PARTITION_BY (theme, type), CODEC 'ZSTD')
  "))
}

#' Write data to file
#'
#' @param conn A DuckDB connection.
#' @param temp_view_name Name of the temporary view containing the data.
#' @inheritParams write_overture
write_to_file <- function(conn, temp_view_name, output, format) {
  DBI::dbExecute(conn, glue::glue("
    COPY {temp_view_name} TO '{output}'
    (FORMAT '{format}'{if(format == 'parquet') ', CODEC \'ZSTD\'' else ''})
  "))
}
