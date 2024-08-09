#' Convert a tbl_sql object to a tbl_overture object
#'
#' This function adds the tbl_overture class to a tbl_sql object.  It is
#' primarily used internally#' by the open_curtain() function but can also be
#' used directly on tbl_sql #' objects representing Overture Maps data.
#'
#' @param x A tbl_sql object representing an Overture Maps dataset.
#'
#' @return A tbl_sql object with the additional class tbl_overture and
#'         attributes overture_type and overture_theme.
#'
#' @details
#' The function adds the tbl_overture class as the first class of the object
#' @export
#'
#' @examplesIf interactive()
#' conn <- stage_conn()
#' division <- open_curtain("division", tablename = "test")
#'
#' class(division)
#'
#' # views
#' division2 <- tbl(conn, "test")
#' division2 <- as_overture(division2)
#'
#' exit_stage(conn)
#'
#' # The open_curtain() function already uses as_overture() internally,
#' # but you can also use it directly:
#' buildings_overture <- as_overture(buildings)
#' class(buildings_overture)  # Should include "tbl_overture"

as_overture <- function(x) {
  if (!inherits(x, "tbl_sql")) stop("Input must be a tbl_sql object")

  if (!inherits(x, "tbl_overture")) class(x) <- c("tbl_overture", class(x))

  return(x)
}
