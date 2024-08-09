#' Join overture datasets
#' Ie all the buildings from a county/state
#' So you give me an admin selection,
#'   then I use the admin bounding box to filter buildings you want
#'   then i do duckdb spatial join (lazily? duckdbfs?)
#'   
#' 
#' What I need:
#'  - a lazy bbox method (max min from a dbplyr)
#'   - actually, I could in theory recognize either an sf or a dbplyr, as long as
#'     it's duckdb (or postgres?) I can use the db backend for the join, and if
#'     it's sf, I can gen a bbox, and delay the join until collect_sf
#'  - if I do that, the easiest start is to add an sf method for bbox.
#'  - then, I'll probably want to add a join param to open_curtain()
#'  - `join` could start as an object with intersect, then over time add 
#'    list(st_intersects = objects)
#'  - a way to recognize overture data
#'  - a within method for dbplyr/overture table
#' 