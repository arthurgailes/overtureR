# mapping specific overture dataset types to their corresponding thematic categories.
get_theme_from_type <- function(type) {
  theme <- type_theme_map[[type]]
  if (length(theme) != 1) {
    stop("Could not find theme for the provided type, please enter manually")
  }

  return(theme)
}

type_theme_map <- list(
  address = "addresses",
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
