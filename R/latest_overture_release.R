#' Discover the latest available Overture Maps release
#'
#' `open_curtain()` needs a release date/version to build its S3 path (e.g.
#' `"2026-08-19.0"`). Overture cuts a new release roughly monthly, so hardcoding
#' one quickly goes stale. This queries Overture's STAC catalog
#' (<https://stac.overturemaps.org/catalog.json>) for its current `latest`
#' release, so callers don't have to track releases themselves or wait on a
#' package update. The result is cached for the session.
#'
#' If the catalog can't be reached, the newest release in the package's local
#' catalog cache is used with a warning (Overture removes releases after a few
#' months, so it may itself be gone). With no cache, the call fails with an
#' error; pass `base_url` to `open_curtain()` to work offline or from a local
#' copy.
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

  stac_url <- paste0(stac_root(), "/catalog.json")

  release <- tryCatch(
    {
      catalog <- DBI::dbGetQuery(
        conn,
        glue::glue("SELECT latest FROM read_json_auto('{stac_url}')")
      )
      catalog$latest
    },
    error = function(e) {
      known <- stac_cached_releases()
      if (length(known) == 0) {
        stop(
          "Could not reach Overture's release catalog (", conditionMessage(e),
          ") and no release is cached locally. Check your connection, or ",
          "pass `base_url` to open_curtain() to use a specific release ",
          "or a local copy.",
          call. = FALSE
        )
      }
      warning(
        "Could not reach Overture's release catalog (", conditionMessage(e),
        "). Using the newest cached release, ", known[[1]], ", which ",
        "Overture may no longer host.",
        call. = FALSE
      )
      known[[1]]
    }
  )

  options(overturer_latest_release = release)
  release
}
