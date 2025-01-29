
testthat::test_that("stage_prompt validates inputs correctly", {
  # Mock data
  mock_data <- structure(
    data.frame(),
    class = c("overture_call", "tbl_df", "tbl", "data.frame"),
    overture_playbill = list(type = "building")
  )

  expect_error(stage_prompt(data.frame(), "test"),
               "Input must be an overture_call object")

  expect_error(stage_prompt(mock_data, c("test1", "test2")),
               "message must be a single character string")

  expect_error(stage_prompt(mock_data, 123),
               "message must be a single character string")
})

testthat::test_that("create_nl_message constructs appropriate prompts", {
  mock_data <- structure(
    data.frame(),
    class = c("overture_call", "tbl_df", "tbl", "data.frame"),
    overture_playbill = list(
      type = "building",
      bbox = c(-74, 40, -73, 41)
    )
  )

  # Test with missing type
  bad_data <- structure(data.frame(), class = "overture_call")
  expect_error(create_nl_message("test", bad_data, .provider = tidyllm::openai, .temperature = 0.1),
               "Could not determine Overture type")

  # Test basic prompt construction (mocked response)
  mockr::with_mock(
    tidyllm::chat = function(...) list(data = list(height = 50)),
    {
      result <- create_nl_message("buildings taller than 50m", mock_data,
                                .provider = tidyllm::openai, .temperature = 0.1)
      expect_type(result, "list")
      expect_equal(result$height, 50)
    }
  )
})

testthat::test_that("create_json_schema generates valid schemas", {
  expect_error(create_json_schema("invalid_type"),
               "No schema definition found for type")

  schema <- create_json_schema("building")
  expect_true(inherits(schema, "tidyllm_schema"))
  expect_equal(schema$name, "building_schema")
})

testthat::test_that("stage_prompt handles filter expressions correctly", {
  mock_data <- structure(
    data.frame(height = 1:5),
    class = c("overture_call", "tbl_df", "tbl", "data.frame"),
    overture_playbill = list(type = "building")
  )

  mockr::with_mock(
    create_nl_message = function(...) list(height = 3),
    {
      result <- stage_prompt(mock_data, "buildings taller than 3m")
      expect_s3_class(result, "overture_call")
      expect_equal(nrow(result), 3) # Should have heights 3,4,5
    }
  )
})

testthat::test_that("stage_prompt works with real building data", {
  skip_if_offline()
  skip_on_cran()
  
  # Test with actual building data
  nyc_bbox <- c(xmin = -74.01, ymin = 40.70, xmax = -73.99, ymax = 40.72)
  buildings <- open_curtain("building", spatial_filter = nyc_bbox)
  
  # Test height filter
  tall_buildings <- stage_prompt(buildings, "find buildings taller than 100 meters")
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
