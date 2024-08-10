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
#' @inheritDotParams open_curtain
#'
#' @return Invisibly returns NULL.
#' @export
download_overture <- function(
    output_dir,
    curtain_call = NULL,
    overwrite = TRUE,
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

  playbill <- attr(curtain_call, "overture_playbill")

  type <- playbill[["type"]]
  theme <- playbill[["theme"]]

  cols <- colnames(curtain_call)
  for (col in c("theme", "type")) {
    if(!col %in% cols) {
      curtain_call <- dplyr::mutate(curtain_call, {{ col }} := get(col))
    }
  }

  sql <- dbplyr::sql_render(curtain_call)

  # overwrite = ifelse(isTRUE(overwrite), "OVERWRITE_OR_IGNORE", "")

  query <- glue::glue(
    "COPY ({sql}) TO '{output_dir}' (
      FORMAT PARQUET,
      PARTITION_BY (theme, type),
      OVERWRITE_OR_IGNORE {overwrite}
    )"
  )

  DBI::dbExecute(conn, query)

  new_tbl <- open_curtain(
    type = type, theme = theme, conn = conn,
    base_url = output_dir
  )

  return(new_tbl)
}
