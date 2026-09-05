# Map an Overture dataset type (e.g. "building") to its theme ("buildings").
# The built-in table answers instantly and offline; a type it doesn't know is
# looked up in the release's catalog before giving up.
get_theme_from_type <- function(type, release = NULL) {
  if (is.null(type) || identical(type, "*")) {
    stop("`theme` must be set when `type` is \"*\" or NULL")
  }
  if (!is.character(type) || length(type) != 1) {
    stop(
      "`type` must be a single string; see overture_types() for valid values"
    )
  }

  theme <- type_theme_map[[type]]
  if (length(theme) == 1) {
    return(theme)
  }

  types <- tryCatch(
    suppressWarnings({
      if (is.null(release)) release <- latest_overture_release()
      overture_types(release)
    }),
    error = function(e) static_overture_types()
  )
  theme <- types$theme[types$type == type]
  if (length(theme) == 1) {
    return(theme)
  }

  stop(
    "Unknown Overture type \"", type, "\". Valid types are: ",
    paste(sort(unique(c(types$type, names(type_theme_map)))), collapse = ", "),
    ". Set `theme` manually to use a type not listed here."
  )
}

type_theme_map <- list(
  address = "addresses",
  bathymetry = "base",
  building = "buildings",
  building_part = "buildings",
  division = "divisions",
  division_area = "divisions",
  division_boundary = "divisions",
  place = "places",
  segment = "transportation",
  connector = "transportation",
  infrastructure = "base",
  land = "base",
  land_cover = "base",
  land_use = "base",
  water = "base"
)
