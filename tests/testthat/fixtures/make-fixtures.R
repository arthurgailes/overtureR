# Rebuild the offline test fixtures. Run from the package root with network
# access; takes about a minute. The data is a few hundred Overture features
# (buildings, building parts, and places) around Times Square, New York,
# one Parquet file per type in fixtures/data, plus a miniature of Overture's
# STAC catalog in fixtures/stac describing them as a made-up release,
# `2024-01-01.0`.
#
# Data (c) Overture Maps Foundation and contributors, ODbL 1.0 / CDLA
# Permissive 2.0. See https://docs.overturemaps.org/attribution/.
devtools::load_all(".", quiet = TRUE)
library(jsonlite)

fixtures <- file.path("tests", "testthat", "fixtures")
release <- "2024-01-01.0"
release_dir <- file.path(tempdir(), "make_fixtures", "release", release)
data_dir <- file.path(fixtures, "data")
stac_dir <- file.path(fixtures, "stac")
unlink(c(release_dir, data_dir, stac_dir), recursive = TRUE)
dir.create(data_dir, recursive = TRUE)

bbox <- c(xmin = -73.99, ymin = 40.755, xmax = -73.98, ymax = 40.762)
types <- list(
  building = "buildings", building_part = "buildings", place = "places"
)

# ---- data ----------------------------------------------------------------
for (type in names(types)) {
  x <- open_curtain(type, bbox)
  # the bbox holds thousands of places; a few hundred is plenty for tests
  if (type == "place") x <- head(dplyr::arrange(x, id), 500)
  record_overture(x, release_dir, overwrite = TRUE)
}

# One file per type. The files are stored flat (CRAN limits path lengths to
# 100 bytes); the test helper fixture_base_url() lays them out in Overture's
# partition structure under these Overture-like names.
file_of <- list()
for (type in names(types)) {
  dir <- file.path(
    release_dir, paste0("theme=", types[[type]]), paste0("type=", type)
  )
  old <- list.files(dir, full.names = TRUE)
  stopifnot(length(old) == 1)
  file.copy(old, file.path(data_dir, paste0(type, ".parquet")))
  file_of[[type]] <- sprintf("part-00000-fixture-%s.zstd.parquet", type)
}

# ---- STAC miniature --------------------------------------------------------
root <- "https://stac.overturemaps.org"
tiles <- "https://tiles.overturemaps.org"
aws <- "https://overturemaps-us-west-2.s3.us-west-2.amazonaws.com/release"
s3 <- "s3://overturemaps-us-west-2/release"
azure <- "https://overturemapswestus2.blob.core.windows.net/release"
link <- function(rel, href, ...) c(list(rel = rel, href = href), list(...))
write <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_json(x, path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
}

# root: two releases, the newer marked latest (like the real catalog)
old_release <- "2023-12-01.0"
write(list(
  type = "Catalog", id = "Overture Releases", stac_version = "1.1.0",
  links = list(
    link("root", paste0(root, "/catalog.json")),
    link("child", sprintf("%s/%s/catalog.json", root, release), latest = TRUE),
    link("child", sprintf("%s/%s/catalog.json", root, old_release)),
    link("self", paste0(root, "/catalog.json"))
  ),
  latest = release
), file.path(stac_dir, "catalog.json"))

themes <- unique(unlist(types))
write(list(
  type = "Catalog", id = release, stac_version = "1.1.0",
  links = c(
    list(link("root", sprintf("%s/%s/catalog.json", root, release))),
    lapply(themes, function(th) {
      href <- sprintf("%s/%s/%s/catalog.json", root, release, th)
      link("child", href, title = th)
    })
  ),
  latest = TRUE
), file.path(stac_dir, release, "catalog.json"))

for (th in themes) {
  th_types <- names(types)[unlist(types) == th]
  write(list(
    type = "Catalog", id = th, stac_version = "1.1.0",
    links = c(
      list(
        link("root", sprintf("%s/%s/catalog.json", root, release)),
        link("pmtiles", sprintf("%s/%s/%s.pmtiles", tiles, release, th))
      ),
      lapply(th_types, function(ty) {
        href <- sprintf("%s/%s/%s/%s/collection.json", root, release, th, ty)
        link("child", href, title = ty)
      })
    )
  ), file.path(stac_dir, release, th, "catalog.json"))
}

# Items: the real fixture file covers the world so any filter in New York
# finds it; the made-up neighbours cover Europe and Australia so a New York
# filter drops them. `building_part` has one file, so pruning can't shrink it.
fake_boxes <- list(
  europe = c(-10, 35, 30, 60),
  australia = c(112, -44, 154, -10)
)
items_for <- list(
  building = c("fixture", "europe", "australia"),
  building_part = "fixture",
  place = c("fixture", "australia")
)

for (type in names(types)) {
  th <- types[[type]]
  coll_dir <- file.path(stac_dir, release, th, type)
  collection_url <- sprintf(
    "%s/%s/%s/%s/collection.json", root, release, th, type
  )
  ids <- sprintf("%05d", seq_along(items_for[[type]]) - 1)

  for (i in seq_along(ids)) {
    kind <- items_for[[type]][[i]]
    if (kind == "fixture") {
      file <- file_of[[type]]
      box <- c(-180, -90, 180, 90)
    } else {
      file <- sprintf("part-%s-fake-%s.zstd.parquet", ids[[i]], kind)
      box <- fake_boxes[[kind]]
    }
    href <- sprintf("%s/%s/theme=%s/type=%s/%s", aws, release, th, type, file)
    write(list(
      type = "Feature", stac_version = "1.1.0", id = ids[[i]],
      bbox = box,
      properties = list(num_rows = 1L, datetime = "2024-01-01T00:00:00Z"),
      links = list(
        link("collection", collection_url)
      ),
      assets = list(
        aws = list(
          href = href, type = "application/vnd.apache.parquet",
          alternate = list(s3 = list(href = sub(aws, s3, href, fixed = TRUE)))
        ),
        azure = list(
          href = sub(aws, azure, href, fixed = TRUE),
          type = "application/vnd.apache.parquet"
        )
      ),
      collection = type
    ), file.path(coll_dir, ids[[i]], paste0(ids[[i]], ".json")))
  }

  write(list(
    type = "Collection", id = type, stac_version = "1.1.0",
    links = c(
      list(link("root", sprintf("%s/%s/catalog.json", root, release))),
      lapply(ids, function(id) {
        href <- sprintf(
          "%s/%s/%s/%s/%s/%s.json", root, release, th, type, id, id
        )
        link("item", href, type = "application/geo+json")
      }),
      list(link("parent", sprintf("%s/%s/%s/catalog.json", root, release, th)))
    ),
    license = "ODbL-1.0"
  ), file.path(coll_dir, "collection.json"))
}

sizes <- file.size(list.files(fixtures, recursive = TRUE, full.names = TRUE))
message("fixtures total: ", round(sum(sizes) / 1024), " KB")
