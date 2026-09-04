# last-resort value if the STAC catalog can't be reached (e.g. offline)
overture_fallback_release <- "2026-08-19.0"

#' Discover the latest available Overture Maps release
#'
#' `open_curtain()` needs a release date/version to build its S3 path (e.g.
#' `"2026-08-19.0"`). Overture cuts a new release roughly monthly, so hardcoding
#' one quickly goes stale. This queries Overture's STAC catalog
#' (<https://stac.overturemaps.org/catalog.json>) for its current `latest`
#' release, so callers don't have to track releases themselves or wait on a
#' package update. The result is cached for the session.
#'
#' @param conn A duckdb connection. Uses the cached session connection by
#' default.
#' @param refresh If `TRUE`, bypass the session cache and re-query the
#' catalog.
#'
#' @return A string identifying the latest release, e.g. `"2026-08-19.0"`.
#'
#' @examplesIf interactive()
#' latest_overture_release()
#' @export
latest_overture_release <- function(conn = NULL, refresh = FALSE) {
  cached <- getOption("overturer_latest_release")
  if (!isTRUE(refresh) && !is.null(cached)) {
    return(cached)
  }

  if (is.null(conn)) conn <- stage_conn()
  config_extensions(conn)

  stac_url <- "https://stac.overturemaps.org/catalog.json"

  release <- tryCatch(
    {
      catalog <- DBI::dbGetQuery(
        conn,
        glue::glue("SELECT latest FROM read_json_auto('{stac_url}')")
      )
      catalog$latest
    },
    error = function(e) {
      warning(
        "Could not reach Overture's release catalog (", conditionMessage(e),
        "). Falling back to the last known release, ",
        overture_fallback_release, ".",
        call. = FALSE
      )
      overture_fallback_release
    }
  )

  options(overturer_latest_release = release)
  release
}
