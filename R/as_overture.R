#' Convert a tbl_sql object to a overture_call object
#'
#' This function adds the overture_call class to a tbl_sql object.  It is
#' primarily used internally#' by the open_curtain() function but can also be
#' used directly on tbl_sql #' objects representing Overture Maps data.
#'
#' @param x A tbl_sql object representing an Overture Maps dataset.
#'
#' @return A tbl_sql object with the additional class overture_call and
#'         attributes overture_type and overture_theme.
#'
#' @details
#' The function adds the overture_call class as the first class of the object
#'
#' @examplesIf interactive()
#' # The open_curtain() function already uses as_overture() internally,
#' # but you can also use it directly:
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
#' @export
as_overture <- function(x, type, theme = get_theme_from_type(type)) {
  if (!inherits(x, "tbl_sql")) stop("Input must be a tbl_sql object")

  if (!inherits(x, "overture_call")) {
    class(x) <- c("overture_call", class(x))
    attr(x, "overture_playbill") <- c(type = type, theme = theme)
  }

  return(x)
}
