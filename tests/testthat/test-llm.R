testthat::test_that("stage_prompt validates inputs correctly", {
  # Mock data
  mock_data <- structure(
    data.frame(),
    class = c("overture_call", "tbl_df", "tbl", "data.frame"),
    overture_playbill = list(type = "building")
  )

  expect_error(
    stage_prompt(data.frame(), "test"),
    "Input must be an overture_call object"
  )

  expect_error(
    stage_prompt(mock_data, c("test1", "test2")),
    "message must be a single character string"
  )

  expect_error(
    stage_prompt(mock_data, 123),
    "message must be a single character string"
  )
})

testthat::test_that("create_tidyllm_scehma generates valid schemas", {
  expect_warning(
    create_tidyllm_scehma("invalid_type"),
    "No schema definition found for type"
  )

  schema <- create_tidyllm_scehma("building")

  # Check that the schema is a non-empty character string
  testthat::expect_type(schema, "list")
  testthat::expect_true(length(schema) > 0, "Schema should not be empty")

  # Define expected properties for the "building" schema
  expected_properties <- c("id", "subtype", "class", "has_parts", "height")

  # Check that all expected properties are present
  actual_properties <- names(schema$properties)
  testthat::expect_true(
    all(expected_properties %in% actual_properties),
    info = paste(
      "Schema is missing expected properties:",
      paste(setdiff(expected_properties, actual_properties), collapse = ", ")
    )
  )

  # Define expected types for each property
  expected_types <- list(
    id = "string",
    subtype = "string",
    class = "string",
    has_parts = "boolean",
    height = "number"
  )

  # Check that each property has the correct type
  for (prop in expected_properties) {
    actual_type <- schema$properties[[prop]]$type
    testthat::expect_identical(
      actual_type,
      expected_types[[prop]],
      info = paste("Property", prop, "should be of type", expected_types[[prop]])
    )
  }

  # Optionally, check for required properties if applicable
  if (!is.null(schema$required)) {
    required_props <- schema$required
    testthat::expect_true(
      all(expected_properties %in% required_props),
      info = "Not all expected properties are marked as required"
    )
  }
})



testthat::test_that("stage_prompt works with real building data", {
  skip_if_offline()
  skip_on_cran()

  # Test with actual building data
  nyc_bbox <- c(xmin = -74.01, ymin = 40.70, xmax = -73.99, ymax = 40.72)
  buildings <- open_curtain("building", spatial_filter = nyc_bbox)

  # Test height filter
  tall_buildings <- stage_prompt(buildings, "buildings taller than 100 meters")
  result <- dplyr::collect(tall_buildings)

  expect_s3_class(tall_buildings, "overture_call")
  expect_true(all(result$height > 100))
  expect_gt(nrow(result), 0)
})

testthat::test_that("stage_prompt works with real places data", {
  skip_if_offline()
  skip_on_cran()

  # Test with actual places data
  nyc_bbox <- c(xmin = -74.01, ymin = 40.70, xmax = -73.99, ymax = 40.72)
  places <- open_curtain("place", spatial_filter = nyc_bbox)

  # Test category filter
  restaurants <- stage_prompt(places, "show only restaurants")
  result <- dplyr::collect(restaurants)

  expect_s3_class(restaurants, "overture_call")
  expect_true(all(grepl("restaurant", result$categories.primary, ignore.case = TRUE)))
  expect_gt(nrow(result), 0)
})

testthat::test_that("stage_prompt handles complex queries with real data", {
  skip_if_offline()
  skip_on_cran()

  nyc_bbox <- c(xmin = -74.01, ymin = 40.70, xmax = -73.99, ymax = 40.72)
  places <- open_curtain("place", spatial_filter = nyc_bbox)

  # Test multiple conditions
  coffee_shops <- stage_prompt(places, "find coffee shops with high confidence")
  result <- dplyr::collect(coffee_shops)

  expect_s3_class(coffee_shops, "overture_call")
  expect_true(all(grepl("coffee", result$categories.primary, ignore.case = TRUE)))
  expect_true(all(result$confidence > 0.8))
  expect_gt(nrow(result), 0)
})


test_that("create_nl_message correctly parses natural language queries with real data", {
  # Skip the test if offline or on CRAN
  skip_if_offline()
  skip_on_cran()

  # Ensure that Tidyllm is authenticated
  if (is.null(getOption("tidyllm_api_key"))) {
    skip("Tidyllm API key not set. Skipping test.")
  }

  # Retrieve real data using open_curtain
  # Adjust the bounding box as needed or use a predefined one
  nyc_bbox <- c(xmin = -74.01, ymin = 40.70, xmax = -73.99, ymax = 40.72)

  # Open a real 'building' overture_call object
  buildings <- open_curtain("building", spatial_filter = nyc_bbox)

  # Define the natural language query
  query <- "Find buildings taller than 100 meters and residential"

  # Invoke create_nl_message with real data
  result <- overtureR:::create_nl_message(
    message = query,
    data = buildings,
    .provider = tidyllm::openai,
    .model = "gpt-4o",
    .temperature = 0.1
  )

  # Validate the structure of the result
  expect_type(result, "list")

  # Check that expected filter parameters are present
  expected_params <- c("height", "subtype")
  expect_true(all(expected_params %in% names(result)),
    info = paste(
      "Expected parameters missing:",
      paste(setdiff(expected_params, names(result)), collapse = ", ")
    )
  )

  # Validate the filter expressions
  # For 'height', expect a numerical comparison
  expect_match(result$height, "^[><]=?\\s*\\d+",
    info = paste("Height filter does not match expected pattern:", result$height)
  )
  # for character, expect internal quotes
  expect_match(result$subtype, "residential")
  expect_match(result$subtype, "\"'")
})
