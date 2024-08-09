library(devtools)
library(dplyr)
library(duckdb)
library(sf)
library(wkb)
library(DBI)
load_all()

# I want to find the transit and POIs around the Kennedy Center
st_bbox

overture_bbox <- function(tbl) {
  bbox <- dplyr::summarize(
    tbl,
    xmin = min(bbox[["xmin"]], na.rm = TRUE),
    xmax = max(bbox[["xmax"]], na.rm = TRUE),
    ymin = min(bbox[["ymin"]], na.rm = TRUE),
    ymax = max(bbox[["ymax"]], na.rm = TRUE)
  )

  bbox <- dplyr::collect(bbox)
  bbox <- sf::st_bbox(unlist(list(bbox)))
  return(bbox)
}

  
con <- stage_conn()

dc <- open_curtain("division") |> 
  filter(region == "US-DC", subtype == "neighborhood")

dc_head <- head(dc) |> collect_sf()

overture_bbox(dc)

View(dc_head)

dc_data <- collect_sf(dc)

dc_data |> filter(grepl(""))
head(as.data.frame(dc_data))

dplyr::mutate(tbl, {{geom_col}} := ST_AsWKB(geom_col)) |> dbplyr::sql_render()

iterior_query <- glue::glue(
  "SELECT *
   FROM read_parquet('{url}', filename=true, hive_partitioning=true, union_by_name = {union_by_name})"
)

query_suffix <- glue::glue("WHERE 1=1 {bbox} ")

query <- (
  "CREATE OR REPLACE VIEW test AS
  (
  SELECT * REPLACE (ST_GeomFromWKB(geometry) as geometry)
   FROM read_parquet('s3://overturemaps-us-west-2/release/2024-07-22.0/theme=divisions/type=division/*', filename=true, hive_partitioning=true, union_by_name = 0)
  )"
)
dbExecute(con, query)

geomed <- dbGetQuery(con, "select * from test limit 5")

tbl(con, "test") |> 
  mutate(geometry = ST_AsWKB(geometry)) |> 
  head()

writeClipboard(geomed$geometry)

dbExecute(con, "update division set geometry = st_GeomFromWKB(geometry)")


hoods <- collect_sf(dc)

types <- dbGetQuery(con, "select column_name, data_type from duckdb_columns() where table_name == 'division'")
View(types)
dbListFields(con, "division")

filter(hoods, grepl(name))