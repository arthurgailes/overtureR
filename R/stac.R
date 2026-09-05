# Overture's STAC catalog: one JSON tree at https://stac.overturemaps.org
#
#   catalog.json                                  -> every live release
#   <release>/catalog.json                        -> every theme in the release
#   <release>/<theme>/catalog.json                -> every type in the theme
#   <release>/<theme>/<type>/collection.json      -> one item per Parquet file
#   <release>/<theme>/<type>/<i>/<i>.json         -> that file's bbox and URLs
#
# The functions here read that tree with DuckDB's read_json, so the package
# needs no extra JSON dependency, and cache the answers per release. A
# release never changes after publication, so the cache never goes stale.

stac_default_root <- "https://stac.overturemaps.org"

# `options(overturer_stac_url = )` points the reader at a mirror or at a
# local copy of the tree (the tests use a saved copy).
stac_root <- function() {
  getOption("overturer_stac_url", stac_default_root)
}

# The catalog links to itself with absolute URLs. When reading a local copy,
# rewrite those to the copy's root so the walk stays inside it.
stac_rewrite <- function(href) {
  root <- stac_root()
  if (identical(root, stac_default_root)) {
    return(href)
  }
  sub(paste0("^", stac_default_root), root, href)
}

# Read the `links` array of one or more STAC documents and keep one relation.
stac_links <- function(conn, urls, rel) {
  query <- glue::glue(
    "SELECT l.href AS href
     FROM (SELECT unnest(links) AS l FROM read_json({sql_file_list(urls)}))
     WHERE l.rel = '{rel}'"
  )
  DBI::dbGetQuery(conn, query)$href
}

# ---- caching -----------------------------------------------------------------

.stac_cache <- new.env(parent = emptyenv())

stac_cache_dir <- function() {
  getOption(
    "overturer_cache_dir",
    tools::R_user_dir("overtureR", which = "cache")
  )
}

stac_cache_enabled <- function() {
  isTRUE(getOption("overturer_cache", TRUE))
}

stac_cache_path <- function(release, key) {
  file.path(stac_cache_dir(), release, paste0(key, ".rds"))
}

stac_cache_get <- function(release, key) {
  id <- paste(release, key, sep = "/")
  if (exists(id, envir = .stac_cache, inherits = FALSE)) {
    return(get(id, envir = .stac_cache, inherits = FALSE))
  }

  path <- stac_cache_path(release, key)
  if (stac_cache_enabled() && file.exists(path)) {
    value <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.null(value)) assign(id, value, envir = .stac_cache)
    return(value)
  }

  NULL
}

stac_cache_set <- function(release, key, value) {
  assign(paste(release, key, sep = "/"), value, envir = .stac_cache)

  if (stac_cache_enabled()) {
    path <- stac_cache_path(release, key)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tryCatch(saveRDS(value, path), error = function(e) NULL)
    stac_cache_prune()
  }

  invisible(value)
}

release_pattern <- "^\\d{4}-\\d{2}-\\d{2}\\.\\d+$"

# Keep the disk cache small: Overture removes releases after a few months,
# so keep only the newest few release directories.
stac_cache_prune <- function(keep = 3) {
  dir <- stac_cache_dir()
  releases <- list.dirs(dir, full.names = FALSE, recursive = FALSE)
  releases <- sort(releases[grepl(release_pattern, releases)])
  stale <- utils::head(releases, max(0, length(releases) - keep))
  unlink(file.path(dir, stale), recursive = TRUE)
  invisible(stale)
}

# Releases the disk cache knows about, newest first. Used as an offline
# fallback for the release lookup.
stac_cached_releases <- function() {
  releases <- list.dirs(stac_cache_dir(), full.names = FALSE, recursive = FALSE)
  releases <- releases[grepl(release_pattern, releases)]
  sort(releases, decreasing = TRUE)
}

#' Clear overtureR's catalog cache
#'
#' overtureR caches what it reads from Overture's STAC catalog (the list of
#' types in a release, and the bounding box of every Parquet file) in memory
#' and on disk under `tools::R_user_dir("overtureR", "cache")`. Releases never
#' change, so the cache never goes stale, but you can clear it here. Set
#' `options(overturer_cache = FALSE)` to keep the cache in memory only, or
#' `options(overturer_cache_dir = )` to move it.
#'
#' @return The cache directory, invisibly.
#' @examples
#' \dontrun{
#' clear_overture_cache()
#' }
#' @export
clear_overture_cache <- function() {
  rm(list = ls(envir = .stac_cache), envir = .stac_cache)
  dir <- stac_cache_dir()
  unlink(dir, recursive = TRUE)
  invisible(dir)
}

# ---- releases and types ------------------------------------------------------

# Every release the root catalog lists, newest first.
stac_releases <- function(conn) {
  hrefs <- stac_links(conn, paste0(stac_root(), "/catalog.json"), "child")
  releases <- basename(dirname(hrefs))
  sort(releases[grepl(release_pattern, releases)], decreasing = TRUE)
}

#' List the Overture releases still online
#'
#' Overture publishes a release about once a month and removes old ones after
#' a few months. This reads the releases its STAC catalog currently lists, so
#' you can pick one for `open_curtain(release = )` or check that a pinned
#' release is still available.
#'
#' @inheritParams overture_types
#'
#' @return A character vector of releases, newest first, such as
#'   `c("2026-08-19.0", "2026-07-22.0")`.
#' @examplesIf interactive()
#' overture_releases()
#' @export
overture_releases <- function(conn = NULL) {
  if (is.null(conn)) conn <- stage_conn()
  config_extensions(conn)

  tryCatch(
    stac_releases(conn),
    error = function(e) {
      stop(
        "Could not reach Overture's release catalog (", conditionMessage(e),
        "). Check your connection.",
        call. = FALSE
      )
    }
  )
}

#' List the dataset types in an Overture release
#'
#' Reads the type-to-theme table for a release from Overture's STAC catalog,
#' so new types (such as `bathymetry`) appear without a package update. The
#' answer is cached per release. If the catalog can't be reached, the
#' package's built-in table is returned with a warning.
#'
#' @param release An Overture release, such as `"2026-08-19.0"`. Defaults to
#' the latest release.
#' @param conn A duckdb connection. Uses the cached session connection by
#' default.
#'
#' @return A data frame with columns `type` and `theme`.
#' @examplesIf interactive()
#' overture_types()
#' @export
overture_types <- function(
  release = latest_overture_release(conn),
  conn = NULL
) {
  if (is.null(conn)) conn <- stage_conn()
  config_extensions(conn)

  cached <- stac_cache_get(release, "types")
  if (!is.null(cached)) {
    return(cached)
  }

  types <- tryCatch(
    {
      root <- stac_root()
      themes <- stac_rewrite(stac_links(
        conn, glue::glue("{root}/{release}/catalog.json"), "child"
      ))
      collections <- stac_rewrite(stac_links(conn, themes, "child"))
      data.frame(
        type = basename(dirname(collections)),
        theme = basename(dirname(dirname(collections))),
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      warning(
        "Could not read the type list for release ", release,
        " from Overture's catalog (", conditionMessage(e),
        "). Using the package's built-in list.",
        call. = FALSE
      )
      NULL
    }
  )

  if (is.null(types) || nrow(types) == 0) {
    return(static_overture_types())
  }

  types <- types[order(types$theme, types$type), ]
  rownames(types) <- NULL
  stac_cache_set(release, "types", types)
}

static_overture_types <- function() {
  data.frame(
    type = names(type_theme_map),
    theme = unlist(type_theme_map, use.names = FALSE),
    stringsAsFactors = FALSE
  )
}

# ---- manifests and file pruning ---------------------------------------------

# One row per Parquet file in a type: the file name and the bounding box of
# the geometries it holds. Returns NULL (with a warning) if the catalog can't
# be read, so callers can fall back to the wildcard path.
stac_manifest <- function(release, theme, type, conn = NULL) {
  if (is.null(conn)) conn <- stage_conn()
  config_extensions(conn)

  key <- paste("manifest", theme, type, sep = "-")
  cached <- stac_cache_get(release, key)
  if (!is.null(cached)) {
    return(cached)
  }

  manifest <- tryCatch(
    {
      root <- stac_root()
      collection <- glue::glue(
        "{root}/{release}/{theme}/{type}/collection.json"
      )
      items <- stac_rewrite(stac_links(conn, collection, "item"))
      if (length(items) == 0) stop("collection lists no files")

      files <- DBI::dbGetQuery(conn, glue::glue(
        "SELECT assets.aws.href AS href,
                bbox[1] AS xmin, bbox[2] AS ymin,
                bbox[3] AS xmax, bbox[4] AS ymax
         FROM read_json({sql_file_list(items)})"
      ))
      data.frame(
        file = basename(files$href),
        xmin = files$xmin,
        ymin = files$ymin,
        xmax = files$xmax,
        ymax = files$ymax,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      warning(
        "Could not read the file list for ", theme, "/", type, " in release ",
        release, " from Overture's catalog (", conditionMessage(e),
        "). Reading every file instead.",
        call. = FALSE
      )
      NULL
    }
  )

  if (is.null(manifest)) {
    return(NULL)
  }

  stac_cache_set(release, key, manifest)
}

# The files in a manifest whose bounding box touches `bbox` (a named numeric
# vector with xmin, ymin, xmax, ymax). Uses the same closed comparisons as
# the SQL that set_stage_boundary() builds, so it never drops a file that
# could hold a matching row.
prune_manifest <- function(manifest, bbox) {
  keep <- manifest$xmax >= bbox[["xmin"]] &
    manifest$xmin <= bbox[["xmax"]] &
    manifest$ymax >= bbox[["ymin"]] &
    manifest$ymin <= bbox[["ymax"]]
  manifest$file[keep]
}

# A base_url refers to one Overture release when it holds a
# `release/<version>` path segment. Local copies from record_overture() don't.
release_from_url <- function(base_url) {
  m <- regmatches(
    base_url, regexec("release/(\\d{4}-\\d{2}-\\d{2}\\.\\d+)", base_url)
  )[[1]]
  if (length(m) < 2) NULL else m[[2]]
}

is_remote_url <- function(base_url) {
  grepl("^[a-z][a-z0-9+.-]*://", base_url)
}

prune_files <- function(conn, base_url, release, theme, type, bbox) {
  manifest <- stac_manifest(release, theme, type, conn = conn)
  if (is.null(manifest)) {
    return(NULL)
  }
  dir <- partition_dir(base_url, theme, type)
  manifest$file <- file.path(dir, manifest$file)
  prune_paths(manifest, bbox)
}

# NULL when the manifest can't be trusted (a bbox missing, a local file
# gone). When nothing touches the bbox, one file is kept: reading its
# footer costs less than reading every footer to learn the same thing.
prune_paths <- function(manifest, bbox) {
  corners <- c("xmin", "ymin", "xmax", "ymax")
  if (nrow(manifest) == 0 || anyNA(manifest[corners])) {
    return(NULL)
  }

  files <- prune_manifest(manifest, bbox)
  if (length(files) == 0) files <- manifest$file[1]

  if (!all(is_remote_url(files) | file.exists(files))) {
    return(NULL)
  }
  as.character(files)
}
