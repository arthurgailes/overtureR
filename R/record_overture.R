#' Download Overture Maps Data to Local Directory
#'
#' This function downloads Overture Maps data to a local directory, maintaining
#' the same partition structure as in S3. `snapshot_overture` defaults
#' 'output_dir' to `tempdir()` and overwrite to TRUE.
#'
#' @param curtain_call A overture_call object.
#' @param output_dir The directory where the data will be saved.
#' @param overwrite Logical, if FALSE (default), existing directories will not be
#' overwritten.
#' @param write_opts a character vector passed to DuckDB's COPY command.
#'
#' @seealso \href{https://duckdb.org/docs/data/partitioning/partitioned_writes}{DuckDB documentation on partitioned writes}
#' @importFrom rlang := .data
#'
#' @examplesIf interactive()
#' broadway <- c(xmin = -73.99, ymin = 40.76, xmax = -73.98, ymax = 40.76)
#' buildings <- open_curtain("building", spatial_filter = bbox)
#' local_buildings <- record_overture(buildings, tempdir(), overwrite = TRUE)
#'
#' @returns Another tbl_lazy. Use [dplyr::show_query()] to see the generated query, and
#' use [collect()] to execute the query and return data to R.
#'
#' @return An 'overture_call' for the downloaded data
#' @export
record_overture <- function(
    curtain_call,
    output_dir,
    overwrite = FALSE,
    write_opts = NULL) {

  conn <- dbplyr::remote_con(curtain_call)

  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (isFALSE(overwrite) & length(list.files(output_dir, include.dirs = TRUE))) {
      stop("'output_dir' is not empty; 'overwrite' must be set to TRUE")
  }

  config_extensions(conn)

  if (!inherits(curtain_call, "overture_call")) {
    stop("Input must be a overture_call object or NULL.")
  }

  # get theme/type from attributes if necessary
  playbill <- attr(curtain_call, "overture_playbill")

  type <- playbill[["type"]]
  theme <- playbill[["theme"]]

  # TODO: allow custom partition scheme
  cols <- colnames(curtain_call)
  for (col in c("theme", "type")) {
    if (!col %in% cols) {
      curtain_call <- dplyr::mutate(curtain_call, {{ col }} := get(col))
    }
  }

  # recast geometry to wkb
  if ("geometry" %in% cols) {
    curtain_call <- dplyr::mutate(curtain_call, geometry = ST_AsWKB(.data$geometry))
  }

  sql <- dbplyr::sql_render(curtain_call)

  write_opts <- process_write_opts(write_opts, overwrite)

  query <- glue::glue(
    "COPY ({sql}) TO '{output_dir}' (
      FORMAT PARQUET, {write_opts})"
  )

  DBI::dbExecute(conn, query)

  new_tbl <- open_curtain(
    type = type, theme = theme, conn = conn,
    base_url = output_dir
  )

  return(new_tbl)
}

process_write_opts <- function(opts, overwrite) {
  has_opt <- function(str, x) isTRUE(any(grepl(str, x, ignore.case = TRUE)))
  if (has_opt("overwrite", opts)) stop("use 'overwrite' paramater; not implemented as write_opts")
  if (has_opt("PARTITION_BY", opts)) stop("custom partitions are not implemented")

  opts <- c(opts, "PARTITION_BY (theme, type)")

  if (isTRUE(overwrite)) opts <- c(opts, "OVERWRITE_OR_IGNORE")

  opts_str <- paste(opts, collapse = ", ")

  return(opts_str)
}

#' @rdname record_overture
#' @export
snapshot_overture <- function(
  curtain_call,
  output_dir = tempdir(),
  overwrite = TRUE,
  write_opts = NULL) {
  return(
    record_overture(
      curtain_call, output_dir, overwrite = overwrite, write_opts = write_opts
    )
  )
}
