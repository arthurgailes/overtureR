#' Check duckdb extension and config settings
#' @inheritParams open_curtain
config_extensions <- function(conn) {
  # temp fix for duckdb package errors, see
  # https://github.com/duckdb/duckdb-r/issues/600
  fix_113 <- utils::packageVersion("duckdb") == "1.1.3"
  inst_sep <- ifelse(fix_113, "core_nightly;", ";")

  extensions <- DBI::dbGetQuery(conn, (
    "SELECT extension_name, installed, loaded FROM duckdb_extensions()"
  ))

  needed <- c("httpfs", "spatial")

  queries <- lapply(needed, function(ext) {
    status <- extensions[which(extensions$extension_name == ext), ]

    q <- ""
    if (isFALSE(status$installed)) q <- paste(q, "INSTALL", ext, inst_sep)

    if (isFALSE(status$loaded)) q <- paste(q, "LOAD", ext, ";")
    q
  })

  queries <- paste(queries, collapse = "")
  if (queries != "") DBI::dbExecute(conn, queries)

  config_settings(conn)
}

# Session settings that make repeated remote reads cheaper: keep Parquet
# footers and HTTP metadata in memory across queries on this connection.
# Older duckdb versions lack some settings, so a failure is not an error.
config_settings <- function(conn) {
  settings <- c(
    "SET parquet_metadata_cache = true",
    "SET enable_http_metadata_cache = true"
  )
  for (setting in settings) {
    tryCatch(DBI::dbExecute(conn, setting), error = function(e) NULL)
  }
  invisible(conn)
}

# duckdb reads and writes GEOMETRY as its own native type starting at 1.1;
# before that, geometry has to be manually cast to/from WKB
duckdb_native_geometry <- function(version = utils::packageVersion("duckdb")) {
  package_version(version) >= "1.1.0"
}

# Column types of a lazy query, as a named character vector, without running
# the query.
describe_columns <- function(x) {
  conn <- dbplyr::remote_con(x)
  sql <- dbplyr::remote_query(x)
  desc <- DBI::dbGetQuery(conn, paste("DESCRIBE", sql))
  stats::setNames(desc$column_type, desc$column_name)
}

# follwing R Packages advice on unused imports:
# https://r-pkgs.org/code.html#sec-code-r-landscape
ignore_unused_imports <- function() {
  ignore_con <- DBI::dbConnect(duckdb::duckdb())
  dbplyr::db_copy_to(ignore_con, data.frame(x = 1), "dummy", temporary = TRUE)
}
