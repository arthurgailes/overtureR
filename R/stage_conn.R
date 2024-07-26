#' create a cachable duckdb connection. In dev
#'
#' `stage_conn` is primarily intended for internal use by other
#' `overtureR` functions.  However, it can be called directly by
#' the user whenever it is desirable to have direct access to the
#' connection object. The core code is copied from `duckdbfs`, which deserves
#' all credit for the implementation
#'
#' When first called (by a user or internal function),
#' this function both creates a duckdb connection and places
#' that connection into a cache (`overturer_conn` option).
#' On subsequent calls, this function returns the cached connection,
#' rather than recreating a fresh connection.
#'
#' This frees the user from the responsibility of managing a
#' connection object, because functions needing access to the
#' connection can use this to create or access the existing connection.
#' At the close of the global environment, this function's finalizer
#' should gracefully shutdown the connection before removing the cache.
#'
#' `strike_stage` closes
#'
#' @inheritParams duckdb::duckdb
#' @inheritDotParams DBI::dbConnect
#'
#' @returns a [duckdb::duckdb()] connection object
#' @examples
#'
#' con <- stage_conn()
#' strike_stage(con)
#'
#' @export
stage_conn <- function(
    dbdir = ":memory:",
    read_only = FALSE,
    bigint = "numeric",
    config = list(),
    ...) {
  conn <- getOption("overturer_conn", NULL)

  ## destroy invalid (closed) connections first
  if (inherits(conn, "duckdb_connection")) {
    if (!DBI::dbIsValid(conn)) {
      strike_stage(conn)
      conn <- NULL
    }
  }

  if (!inherits(conn, "duckdb_connection")) {
    if (getOption("overturer_debug", FALSE)) {
      message("Making a duckdb connection!")
    }
    conn <- DBI::dbConnect(duckdb::duckdb(), ...)
    options(overturer_conn = conn)
    config_extensions(conn)
  }
  ## create finalizer to avoid duckdb complaining that connection
  ## was not shut down before gc
  e <- globalenv()
  reg.finalizer(e, function(e) strike_stage(), TRUE)

  conn
}

#' close connection
#' @inheritParams open_curtain
#' @rdname stage_conn
#' @export
strike_stage <- function(conn = stage_conn()) {
  if (DBI::dbIsValid(conn)) {
    DBI::dbDisconnect(conn, shutdown = TRUE)
  }

  ## clear cached reference to the now-closed connection
  .Options$overturer_conn <- NULL

  rm(conn)
}
