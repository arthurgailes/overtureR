#' Doanload the current version of Overture's schema for use in package
#' functions.
library(git2r)

schema_dir <- "data-raw/overture_schema"
clone_dir <- "data-raw/overture_schema/schema"

if (dir.exists(clone_dir)) {
  unlink(clone_dir, recursive = TRUE, force = TRUE)
}

clone("https://github.com/OvertureMaps/schema.git", clone_dir)

# extract just the yaml
yamls <- list.files(paste0(clone_dir, "/schema"), "yaml$", recursive = TRUE)
yamls <- grep("/", yamls, value = TRUE)

dir.create(paste0(schema_dir, "/yaml"), showWarnings = FALSE)

file.copy(paste0(clone_dir, "/schema/", yamls), paste0(schema_dir, "/yaml"))

unlink(clone_dir, recursive = TRUE)
