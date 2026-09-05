#' Download Overture Maps data to a local directory
#'
#' Writes the rows of an `overture_call` to Parquet files under `output_dir`,
#' in Overture's own `theme=<theme>/type=<type>/` layout, and returns a new
#' `overture_call` that reads from the copy. Each `type=` directory also gets
#' an `_overture.json` manifest recording the source release and the bounding
#' box of every file, so [open_curtain()] on the copy skips files by location
#' just as it does on S3. `snapshot_overture()` defaults `output_dir` to
#' `tempdir()` and `overwrite` to `TRUE`.
#'
#' @param curtain_call An `overture_call` object, or the name of an Overture
#' type (such as `"building"`) to open with [open_curtain()] first.
#' @param output_dir The directory where the data will be saved.
#' @param overwrite If `FALSE` (default), `output_dir` must be empty. If
#' `TRUE`, the `theme=/type=` directories being written are replaced; other
#' files in `output_dir` are left alone.
#' @param write_opts A character vector of extra options for DuckDB's `COPY`
#' command, such as `"ROW_GROUP_SIZE 100000"`. Use `partition_by`, not
#' `PARTITION_BY`, to change the layout.
#' @param partition_by Names of columns to partition by, below `theme` and
#' `type`. Add columns with [dplyr::mutate()] first if needed.
#' @param grid Cell size in degrees. If set, the copy is further partitioned
#' into a grid of that size (columns `x_cell` and `y_cell`, the cell's
#' south-west corner), so each file covers a compact area and a later
#' `spatial_filter` skips most of them.
#' @param spatial_filter,... Passed to [open_curtain()] when `curtain_call` is
#' a type name.
#'
#' @seealso \href{https://duckdb.org/docs/data/partitioning/partitioned_writes}{DuckDB documentation on partitioned writes}
#' @importFrom rlang := .data %||%
#'
#' @examplesIf interactive()
#' broadway <- c(xmin = -73.99, ymin = 40.755, xmax = -73.98, ymax = 40.762)
#' buildings <- open_curtain("building", spatial_filter = broadway)
#' local_buildings <- record_overture(buildings, tempdir(), overwrite = TRUE)
#'
#' # or in one call
#' local_buildings <- record_overture(
#'   "building", tempdir(), overwrite = TRUE, spatial_filter = broadway
#' )
#'
#' @return An `overture_call` reading from the downloaded data. Use
#'   [dplyr::show_query()] to see its query and [dplyr::collect()] to bring
#'   the rows into R.
#' @export
record_overture <- function(
  curtain_call,
  output_dir,
  overwrite = FALSE,
  write_opts = NULL,
  partition_by = NULL,
  grid = NULL,
  spatial_filter = NULL,
  ...
) {
  if (is.character(curtain_call)) {
    curtain_call <- open_curtain(curtain_call, spatial_filter, ...)
  } else if (!is.null(spatial_filter) || ...length() > 0) {
    stop(
      "`spatial_filter` and `...` only apply when `curtain_call` is a type ",
      "name; filter the overture_call before passing it"
    )
  }
  if (!inherits(curtain_call, "overture_call")) {
    stop("Input must be a overture_call object.")
  }

  conn <- dbplyr::remote_con(curtain_call)
  config_extensions(conn)
  bill <- playbill(curtain_call)

  output_dir <- stage_output_dir(output_dir, overwrite, bill)
  curtain_call <- cast_partitions(curtain_call, bill, partition_by, grid)
  partitions <- attr(curtain_call, "partitions")

  write_opts <- process_write_opts(write_opts, overwrite, partitions)
  DBI::dbExecute(conn, glue::glue(
    "COPY ({dbplyr::sql_render(curtain_call)}) TO {sql_string(output_dir)} (
      FORMAT PARQUET, {write_opts})"
  ))
  write_recording_manifest(conn, output_dir, bill)

  open_curtain(
    type = bill$type, theme = bill$theme, conn = conn,
    base_url = output_dir, release = bill$release
  )
}

#' @rdname record_overture
#' @export
snapshot_overture <- function(
  curtain_call,
  output_dir = tempdir(),
  overwrite = TRUE,
  write_opts = NULL,
  partition_by = NULL,
  grid = NULL,
  spatial_filter = NULL,
  ...
) {
  record_overture(
    curtain_call, output_dir,
    overwrite = overwrite, write_opts = write_opts,
    partition_by = partition_by, grid = grid,
    spatial_filter = spatial_filter, ...
  )
}

stage_output_dir <- function(output_dir, overwrite, bill) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_dir <- normalizePath(output_dir, winslash = "/")

  has_files <- length(list.files(output_dir, include.dirs = TRUE)) > 0
  if (isFALSE(overwrite) && has_files) {
    stop("'output_dir' is not empty; 'overwrite' must be set to TRUE")
  }
  if (isTRUE(overwrite)) {
    stale <- Sys.glob(partition_dir(output_dir, bill$theme, bill$type))
    unlink(stale, recursive = TRUE)
  }
  output_dir
}

cast_partitions <- function(curtain_call, bill, partition_by, grid) {
  for (col in setdiff(c("theme", "type"), colnames(curtain_call))) {
    curtain_call <- dplyr::mutate(curtain_call, !!col := !!bill[[col]])
  }
  if (!is.null(grid)) {
    curtain_call <- cast_grid(curtain_call, grid)
    partition_by <- c(partition_by, "x_cell", "y_cell")
  }

  partitions <- unique(c("theme", "type", partition_by))
  unknown <- setdiff(partitions, colnames(curtain_call))
  if (length(unknown) > 0) {
    stop(
      "`partition_by` names columns that are not in the data: ",
      paste(unknown, collapse = ", ")
    )
  }
  attr(curtain_call, "partitions") <- partitions
  curtain_call
}

cast_grid <- function(x, grid) {
  if (!is.numeric(grid) || length(grid) != 1 || !is.finite(grid) || grid <= 0) {
    stop("`grid` must be a single positive number of degrees")
  }
  if (!"bbox" %in% colnames(x)) {
    stop("`grid` needs the `bbox` column; keep it in the query")
  }
  cell <- function(side) {
    dbplyr::sql(glue::glue("floor(bbox.{side} / {grid}) * {grid}"))
  }
  dplyr::mutate(x, x_cell = !!cell("xmin"), y_cell = !!cell("ymin"))
}

process_write_opts <- function(
  opts,
  overwrite,
  partitions = c("theme", "type")
) {
  has_opt <- function(str, x) isTRUE(any(grepl(str, x, ignore.case = TRUE)))
  if (has_opt("overwrite", opts)) {
    stop("use the `overwrite` argument, not a write_opts entry")
  }
  if (has_opt("PARTITION_BY", opts)) {
    stop("use the `partition_by` argument, not a write_opts entry")
  }

  partition <- paste0("PARTITION_BY (", paste(partitions, collapse = ", "), ")")
  opts <- c(opts, partition)
  # unique names keep DuckDB's Parquet metadata cache from serving a stale
  # footer after a rewrite
  if (!has_opt("FILENAME_PATTERN", opts)) {
    opts <- c(opts, "FILENAME_PATTERN 'data_{uuid}'")
  }

  if (isTRUE(overwrite)) opts <- c(opts, "OVERWRITE_OR_IGNORE")

  paste(opts, collapse = ", ")
}

# ---- local manifests ---------------------------------------------------------
#
# Each type directory record_overture() writes gets an `_overture.json` with
# the source release and every file's bounding box, so open_curtain() can
# skip files the way the STAC catalog lets it skip files on S3.

manifest_path <- function(base_url, theme, type) {
  file.path(partition_dir(base_url, theme, type), "_overture.json")
}

write_recording_manifest <- function(conn, output_dir, bill) {
  glob <- file.path(
    partition_dir(output_dir, bill$theme, bill$type), "**", "*.parquet"
  )
  files <- parquet_file_bounds(conn, glob)
  files$type <- sub(".*/type=([^/]+)/.*", "\\1", files$file)

  header <- list(
    overtureR = as.character(utils::packageVersion("overtureR")),
    release = bill$release,
    theme = bill$theme,
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  corners <- c("file", "xmin", "ymin", "xmax", "ymax")
  for (type in unique(files$type)) {
    dir <- partition_dir(output_dir, bill$theme, type)
    entries <- files[files$type == type, corners]
    entries$file <- substring(entries$file, nchar(dir) + 2)
    header$type <- type
    path <- manifest_path(output_dir, bill$theme, type)
    write_manifest_json(conn, path, header, entries)
  }
  invisible(files)
}

# Per-file bounds from the Parquet footers' column statistics, so the rows
# just written are never re-read
parquet_file_bounds <- function(conn, glob) {
  corner <- function(side, fun) {
    stat <- if (fun == "min") "stats_min_value" else "stats_max_value"
    glue::glue(
      "{fun}(CASE WHEN path_in_schema = 'bbox, {side}'
                  THEN try_cast({stat} AS DOUBLE) END) AS {side}"
    )
  }
  files <- DBI::dbGetQuery(conn, glue::glue(
    "SELECT file_name AS file,
            {corner('xmin', 'min')}, {corner('ymin', 'min')},
            {corner('xmax', 'max')}, {corner('ymax', 'max')}
     FROM parquet_metadata({sql_string(glob)})
     GROUP BY file_name
     ORDER BY file_name"
  ))
  # parquet_metadata() reports Windows paths with backslashes
  files$file <- gsub("\\", "/", files$file, fixed = TRUE)
  files
}

# Written with DuckDB so the package needs no JSON dependency
write_manifest_json <- function(conn, path, header, entries) {
  duckdb::duckdb_register(conn, "overtureR_manifest", entries)
  on.exit(duckdb::duckdb_unregister(conn, "overtureR_manifest"))

  scalar <- function(x) if (is.null(x)) "NULL" else sql_string(x)
  DBI::dbExecute(conn, glue::glue(
    "COPY (
       SELECT {scalar(header$overtureR)} AS overtureR,
              {scalar(header$release)} AS release,
              {scalar(header$theme)} AS theme,
              {scalar(header$type)} AS type,
              {scalar(header$created)} AS created,
              (SELECT list({{'file': file, 'xmin': xmin, 'ymin': ymin,
                             'xmax': xmax, 'ymax': ymax}})
               FROM overtureR_manifest) AS files
     ) TO {sql_string(path)} (FORMAT JSON)"
  ))
  invisible(path)
}

# The local copy's release and files (full paths with bounds), or NULL when
# base_url has no manifest
recording_manifest <- function(conn, base_url, theme, type) {
  paths <- Sys.glob(manifest_path(base_url, theme, type))
  if (length(paths) == 0) {
    return(NULL)
  }

  tryCatch(
    {
      rows <- DBI::dbGetQuery(conn, glue::glue(
        "SELECT release, theme, type, unnest(files, recursive := true)
         FROM read_json({sql_file_list(paths)})"
      ))
      if (nrow(rows) == 0) {
        stop("the manifest lists no files")
      }
      rows$file <- file.path(
        partition_dir(base_url, rows$theme, rows$type), rows$file
      )
      release <- unique(stats::na.omit(rows$release))
      list(
        release = if (length(release) == 1) release else NULL,
        files = rows[c("file", "xmin", "ymin", "xmax", "ymax")]
      )
    },
    error = function(e) {
      warning(
        "Could not read the manifest in ", base_url, " (",
        conditionMessage(e), "). Reading every file instead.",
        call. = FALSE
      )
      NULL
    }
  )
}
