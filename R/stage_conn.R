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
#' rather than recreating a fresh connection. The `dbdir`, `read_only`,
#' `bigint`, and `config` arguments only take effect when a connection is
#' created.
#'
#' This frees the user from the responsibility of managing a
#' connection object, because functions needing access to the
#' connection can use this to create or access the existing connection.
#' At the close of the global environment, this function's finalizer
#' should gracefully shutdown the connection before removing the cache.
#'
#' `strike_stage` closes the connection.
#'
#' @inheritParams duckdb::duckdb
#' @param ... Further arguments passed to [DBI::dbConnect]
#'
#' @returns a [duckdb::duckdb()]connection object
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
    conn <- DBI::dbConnect(
      duckdb::duckdb(
        dbdir = dbdir, read_only = read_only, bigint = bigint, config = config
      ),
      ...
    )
    options(overturer_conn = conn)
  }

  ## create finalizer to avoid duckdb complaining that connection
  ## was not shut down before gc. One finalizer is enough.
  if (!isTRUE(.stage_state$finalizer_set)) {
    register_stage_finalizer()
    .stage_state$finalizer_set <- TRUE
  }

  conn
}

.stage_state <- new.env(parent = emptyenv())

register_stage_finalizer <- function() {
  reg.finalizer(globalenv(), function(e) strike_stage(), onexit = TRUE)
}

#' close connection
#' @param conn A duckdb connection. Defaults to the cached session connection,
#' if there is one.
#' @rdname stage_conn
#' @export
strike_stage <- function(conn = getOption("overturer_conn", NULL)) {
  if (inherits(conn, "duckdb_connection") && DBI::dbIsValid(conn)) {
    DBI::dbDisconnect(conn, shutdown = TRUE)
  }

  ## clear cached reference to the now-closed connection
  cached <- getOption("overturer_conn", NULL)
  if (!is.null(cached)) {
    if (identical(conn, cached) || !DBI::dbIsValid(cached)) {
      options(overturer_conn = NULL)
    }
  }

  invisible(NULL)
}
