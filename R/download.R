#' Download Overture Data to Local Directory
#'
#' This function downloads Overture Maps data to a local directory, maintaining
#' the same partition structure as in S3.
#'
#' @param output_dir The directory where the data will be saved.
#' @param curtain_call A overture_call object or NULL. If NULL,
#' 'type' must be provided, and `open_curtain` will open a connection.
#' @param overwrite Logical, if FALSE, existing directories will not be
#' overwritten.
#' @param write_opts a character vector passed to DuckDB's copy See
#' \href{https://duckdb.org/docs/data/partitioning/partitioned_writes}{DuckDB documentation on partitioned writes}
#' @inheritDotParams open_curtain
#'
#' @return Invisibly returns NULL.
#' @export
download_overture <- function(
  output_dir,
  curtain_call = NULL,
  overwrite = FALSE,
  write_opts = NULL,
  ...
) {
  dots <- list(...)
  conn <- dots[["conn"]]

  if (is.null(conn)) conn <- stage_conn()
  config_extensions(conn)

  if (is.null(curtain_call)) {
    # Check if necessary parameters for open_curtain are provided
    if (!"type" %in% names(dots)) {
      stop("When 'curtain_call' is NULL, 'type' must be specified")
    }
    # Call open_curtain with the captured arguments
    curtain_call <- open_curtain(...)
  } else if (!inherits(curtain_call, "overture_call")) {
    stop("Input must be a overture_call object or NULL.")
  }

  # get theme/type from attributes if necessary
  playbill <- attr(curtain_call, "overture_playbill")

  type <- playbill[["type"]]
  theme <- playbill[["theme"]]

  cols <- colnames(curtain_call)
  for (col in c("theme", "type")) {
    if(!col %in% cols) {
      curtain_call <- dplyr::mutate(curtain_call, {{ col }} := get(col))
    }
  }

  # recast geometry to wkb
  if("geometry" %in% cols) {
    curtain_call <- dplyr::mutate(curtain_call, geometry = ST_AsWKB(geometry))
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


process_write_opts <- function(opts, overwrite){

  has_opt <- function(str, x) isTRUE(any(grepl(str, x, ignore.case = TRUE)))

  default_partition <- "PARTITION_BY (theme, type)"

  overwrite <- if(isTRUE(overwrite)) "OVERWRITE_OR_IGNORE" else NULL

  if(!has_opt("overwrite", opts)) opts <- c(opts, overwrite)
  if(!has_opt("PARTITION_BY", opts)) opts <- c(opts, default_partition)

  opts_str <- paste(opts, collapse = ", ")

  return(opts_str)
}
