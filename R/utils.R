#' Check duckdb extension and config settings
#' @inheritParams open_curtain
config_extensions <- function(conn) {
  extensions <- DBI::dbGetQuery(conn, (
    "SELECT extension_name, installed, loaded FROM duckdb_extensions()"
  ))

  needed <- c("httpfs", "spatial")

  queries <- lapply(needed, function(ext){
    status <- extensions[which(extensions$extension_name == ext), ]

    q <- ""
    if (isFALSE(status$installed)) q <- paste(q, "INSTALL", ext, ";")

    if (isFALSE(status$loaded)) q <- paste(q, "LOAD", ext, ";")
    return(q)
  })

  queries <- paste(queries, collapse = "")
  print(queries)
  if(queries != "" ) DBI::dbExecute(conn, queries)
}

# follwing R Packages advice on unused imports:
# https://r-pkgs.org/code.html#sec-code-r-landscape
ignore_unused_imports <- function() {
  ignore_con <- DBI::dbConnect(duckdb::duckdb())
  dbplyr::db_copy_to(ignore_con, data.frame(x=1), "dummy", temporary = TRUE)
}
