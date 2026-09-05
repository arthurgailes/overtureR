#' Convert a tbl_sql object to an overture_call object
#'
#' Adds the `overture_call` class to a `tbl_sql` object. [open_curtain()]
#' does this for you; call it directly on a lazy table of Overture data you
#' built yourself, so that [collect()] returns `sf` and [record_overture()]
#' knows the type and theme of the data.
#'
#' @param x A tbl_sql object representing an Overture Maps dataset.
#' @param release The Overture release the data came from, such as
#' `"2026-08-19.0"`, or `NULL` if unknown.
#' @inheritParams open_curtain
#'
#' @return A tbl_sql object with the additional class `overture_call` and an
#'   `overture_playbill` attribute: a list with `type`, `theme` and `release`.
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
#' division2 <- as_overture(division2, "division")
#'
#' strike_stage(conn)
#' @export
as_overture <- function(
  x,
  type,
  theme = get_theme_from_type(type),
  release = NULL
) {
  if (!inherits(x, "tbl_sql")) stop("Input must be a tbl_sql object")

  if (!inherits(x, "overture_call")) {
    conn <- dbplyr::remote_con(x)
    config_extensions(conn)

    class(x) <- c("overture_call", class(x))
    attr(x, "overture_playbill") <- list(
      type = type, theme = theme, release = release
    )
  }

  x
}

playbill <- function(x) {
  as.list(attr(x, "overture_playbill"))
}

#' @export
print.overture_call <- function(x, ...) {
  bill <- playbill(x)
  what <- if (identical(bill$type, "*")) {
    paste("theme", bill$theme)
  } else {
    paste("type", bill$type)
  }
  release <- if (is.null(bill$release)) {
    "release unknown"
  } else {
    paste("release", bill$release)
  }
  cat("# Overture ", release, ", ", what, "\n", sep = "")
  NextMethod()
  invisible(x)
}
