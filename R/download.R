#' Download Overture Data to Local Directory
#'
#' This function downloads Overture Maps data to a local directory, maintaining
#' the same partition structure as in S3.
#'
#' @param curtain_call A tbl_overture object or NULL. If NULL, open_curtain parameters will be used.
#' @param output_dir The directory where the data will be saved.
#' @param overwrite Logical, whether to overwrite existing files.
#' @inheritDotParams open_curtain
#'
#' @return Invisibly returns NULL.
#' @export
download_overture <- function(
    curtain_call = NULL,
    output_dir,
    overwrite = FALSE,
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
    curtain_call <- do.call(open_curtain, dots)
  }

  if (!inherits(curtain_call, "tbl_overture")) {
    stop("Input must be a tbl_overture object or NULL.")
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

  query <- glue::glue(
    "COPY ({sql}) TO '{partition_dir}' (
      FORMAT PARQUET,
      PARTITION_BY (theme, type),
      OVERWRITE_OR_IGNORE={overwrite}
    )"
  )

  DBI::dbExecute(conn, query)

  invisible(NULL)
}
