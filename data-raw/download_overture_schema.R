#' Doanload the current version of Overture's schema for use in package
#' functions.
library(git2r)
library(yaml)
library(yyjsonr)
library(purrr)
library(dplyr)

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



# Function to convert YAML to JSON while handling special cases
process_yaml_directory <- function(yaml_files) {
  # Create a named list to store the schema definitions
  schema_definitions <- list()

  # Process each YAML file
  for (file_path in yaml_files) {
    # Read the YAML file
    yaml_content <- readLines(file_path, warn = FALSE)
    yaml_content <- paste(yaml_content, collapse = "\n")

    # Handle the special case of "$schema" and similar fields
    yaml_content <- gsub('"\\$schema":', '"$schema":', yaml_content)

    # Parse YAML
    parsed_yaml <- try(yaml.load(yaml_content), silent = TRUE)

    if (!inherits(parsed_yaml, "try-error")) {
      schema_name <- tools::file_path_sans_ext(basename(file_path))
      schema_definitions[[schema_name]] <- parsed_yaml
    } else {
      warning(sprintf("Error parsing YAML content in %s", file_path))
    }
  }

  return(schema_definitions)
}

# Function to save schemas to json. May remove
save_schemas <- function(schema_defs, output_dir) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Save individual schemas
  walk2(schema_defs, names(schema_defs), function(content, name) {
    yyjsonr::write_json_file(
      content,
      file.path(output_dir, paste0(name, ".json")),
      opts = opts_write_json(pretty = TRUE)
    )
  })

  # Save combined schema
  yyjsonr::write_json_file(
    schema_defs,
    file.path(output_dir, "overture_schema.json"),
    opts = opts_write_json(pretty = TRUE)
  )
}

# Process YAML files
yamls2 <- list.files(paste0(schema_dir, "/yaml"), full.names = TRUE)
schema_defs <- process_yaml_directory(yamls2)

save_schemas(schema_defs, paste0(schema_dir, "/tidy"))


flatten_schema_properties <- function(schema) {
  result <- list()

  if ("properties" %in% names(schema)) {
    # Core GeoJSON fields
    if ("id" %in% names(schema$properties)) {
      result[["id"]] <- "character"
    }

    # Get nested properties
    if ("properties" %in% names(schema$properties)) {
      props <- schema$properties$properties

      # Handle allOf references
      if ("allOf" %in% names(props)) {
        for (ref in props$allOf) {
          # Handle levelContainer reference
          if ("$ref" %in% names(ref) && grepl("levelContainer", ref[["$ref"]], fixed = TRUE)) {
            result[["level"]] <- "numeric"
            next
          }

          # Handle shapeContainer reference (for height)
          if ("$ref" %in% names(ref) && grepl("shapeContainer", ref[["$ref"]], fixed = TRUE)) {
            result[["height"]] <- "numeric"
            next
          }

          # Direct property definitions in allOf
          if ("properties" %in% names(ref)) {
            for (field_name in names(ref$properties)) {
              prop <- ref$properties[[field_name]]

              if ("$ref" %in% names(prop)) {
                if (field_name == "class") {
                  result[[field_name]] <- "character"
                } else if (field_name == "level") {
                  result[[field_name]] <- "numeric"
                }
              } else if ("type" %in% names(prop)) {
                result[[field_name]] <- switch(prop$type[1],
                  "string" = "character",
                  "number" = "numeric",
                  "boolean" = "logical",
                  "character"
                )
              }
            }
          }
        }
      }

      # Process direct properties
      if ("properties" %in% names(props)) {
        for (field_name in names(props$properties)) {
          prop <- props$properties[[field_name]]

          # Special cases
          if (field_name == "categories") {
            result[["categories$primary"]] <- "character"
            result[["categories$alternate"]] <- "character[]"
            next
          }

          # Handle direct level property
          if (field_name == "level") {
            result[["level"]] <- "numeric"
            next
          }

          # Get type definition
          if ("type" %in% names(prop)) {
            type <- prop$type[1]

            r_type <- switch(type,
              "string" = "character",
              "boolean" = "logical",
              "number" = "numeric",
              "integer" = "numeric",
              "array" = {
                if ("items" %in% names(prop) && "type" %in% names(prop$items)) {
                  item_type <- prop$items$type[1]
                  base_type <- switch(item_type,
                    "string" = "character",
                    "number" = "numeric",
                    "integer" = "numeric",
                    "boolean" = "logical",
                    "character"
                  )
                  paste0(base_type, "[]")
                } else {
                  "character[]"
                }
              },
              "character"
            )

            # Handle enums
            if ("enum" %in% names(prop)) {
              r_type <- sprintf("factor(%s)", paste(prop$enum, collapse = ", "))
            }

            result[[field_name]] <- r_type
          }
        }
      }
    }
  }

  return(result)
}
# Process schemas
schemas <- schema_defs[!names(schema_defs) %in% c("defs")]
tidyllm_schema_list <- lapply(schemas, flatten_schema_properties)

# Add bbox to all elements
tidyllm_schema_list <- lapply(tidyllm_schema_list, function(x) {
  x[["geometry"]] <- NULL
  return(x)
})

test_schema_properties <- function() {
  # Test cases for different schemas and their expected properties
  test_cases <- list(
    building = c(
      "id", "subtype", "class", "has_parts", "height"
    ),
    place = c(
      "id", "categories$primary", "categories$alternate",
      "confidence", "websites", "phones"
    ),
    water = c(
      "id", "subtype", "is_salt", "is_intermittent"
    ),
    segment = c(
      "id", "subtype", "connectors", "class", "level"
    )
  )

  # Track all failures
  failures <- character(0)

  # Test each schema
  for (schema_name in names(test_cases)) {
    expected_props <- test_cases[[schema_name]]
    actual_props <- names(tidyllm_schema_list[[schema_name]])
    missing_props <- setdiff(expected_props, actual_props)

    if (length(missing_props) > 0) {
      failures <- c(
        failures,
        sprintf(
          "Schema '%s' missing expected properties: %s",
          schema_name,
          paste(missing_props, collapse = ", ")
        )
      )
    }
  }

  # Report all failures at once
  if (length(failures) > 0) {
    stop(paste(failures, collapse = "\n"))
  }

  cat("All schema property tests passed!\n")
}

# Run the tests
test_schema_properties()

yyjsonr::write_json_file(
  tidyllm_schema_list, paste0(schema_dir, "/tidy/overture_schema_types.json"),
  pretty = TRUE
)

# Save result once
usethis::use_data(tidyllm_schema_list, schema_defs, internal = TRUE, overwrite = TRUE)
