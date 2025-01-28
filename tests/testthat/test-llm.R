test_that("stage_prompt validates inputs correctly", {
  # Test non-overture_call input
  expect_error(
    stage_prompt(mtcars, "find cars"),
    "Input must be an overture_call object"
  )

  # Test invalid message
  skip_if_offline()
  skip_on_cran()
  data <- open_curtain("building")
  expect_error(
    stage_prompt(data, c("query1", "query2")),
    "message must be a single character string"
  )
})

test_that("stage_prompt generates correct filter expressions", {
  skip_if_offline()
  skip_on_cran()

  # Test basic building height filter
  broadway_bbox <- sf::st_bbox(c(
    xmin = -74.017,
    ymin = 40.704,
    xmax = -73.929,
    ymax = 40.816
  ))

  buildings <- open_curtain("building", broadway_bbox)
  nl_query <- stage_prompt(buildings, "find buildings taller than 100 meters")

  # Verify subquery structure
  query_sql <- dbplyr::sql_render(nl_query)
  expect_match(as.character(query_sql), "^SELECT .*? FROM \\(SELECT .*? WHERE height > 100.*?\\)", perl = TRUE)

  # Verify type preservation
  expect_s3_class(nl_query, "overture_call")
  expect_equal(attr(nl_query, "overture_playbill")[["type"]], "building")
  expect_equal(attr(nl_query, "overture_playbill")[["theme"]], attr(buildings, "overture_playbill")[["theme"]])

  # Test category filter with multiple conditions
  places <- open_curtain("place") |>
    stage_prompt("show only restaurants with high confidence")

  query_sql <- dbplyr::sql_render(places)
  expect_match(as.character(query_sql), "^SELECT .*? FROM \\(SELECT .*? WHERE.*?categories\\.primary = 'restaurant'.*?confidence > .*?\\)", perl = TRUE)
})

test_that("stage_prompt respects schema constraints", {
  skip_if_offline()
  skip_on_cran()

  # Test schema validation
  places <- open_curtain("place")
  expect_error(
    stage_prompt(places, "find places with invalid_field > 10"),
    NA # Should not error, but should ignore invalid field
  )

  # Test array field handling
  result <- places |>
    stage_prompt("find places with categories containing restaurant")

  query_sql <- dbplyr::sql_render(result)
  expect_match(as.character(query_sql), "categories", fixed = TRUE)
})

test_that("stage_prompt works in dplyr pipelines", {
  skip_if_offline()
  skip_on_cran()

  result <- open_curtain("building") |>
    dplyr::select(id, height, type) |>
    stage_prompt("buildings taller than 50 meters") |>
    dplyr::filter(dplyr::n() > 0) |>
    head(1) |>
    collect()

  expect_s3_class(result, "sf")
  expect_true(all(result$height > 50))
})

test_that("stage_prompt handles errors gracefully", {
  skip_if_offline()
  skip_on_cran()

  # Test invalid LLM response
  mock_chat <- function(...) {
    tidyllm::llm_message("Invalid JSON response")
  }

  data <- open_curtain("building")
  expect_error(
    stage_prompt(data, "test query", .provider = mock_chat),
    "Failed to parse LLM response"
  )

  # Test invalid filter expression
  mock_chat <- function(...) {
    tidyllm::llm_message("{\"filters\": [\"invalid R syntax >\"]}")
  }

  expect_error(
    stage_prompt(data, "test query", .provider = mock_chat),
    "Invalid filter expression"
  )
})
