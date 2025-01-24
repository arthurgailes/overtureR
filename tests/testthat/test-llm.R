test_that("open_curtain_nl validates inputs correctly", {
  # Test non-overture_call input
  expect_error(
    open_curtain_nl(mtcars, "find cars"),
    "Input must be an overture_call object"
  )

  # Test invalid message
  data <- open_curtain("building")
  expect_error(
    open_curtain_nl(data, c("query1", "query2")),
    "message must be a single character string"
  )
})

test_that("open_curtain_nl generates correct filter expressions", {
  skip_if_offline()

  # Test basic building height filter
  buildings <- open_curtain("building") |>
    open_curtain_nl("find buildings taller than 100 meters")

  query <- dbplyr::sql_render(buildings)
  expect_match(as.character(query), "height > 100", fixed = TRUE)

  # Test category filter
  places <- open_curtain("place") |>
    open_curtain_nl("show only restaurants")

  query <- dbplyr::sql_render(places)
  expect_match(as.character(query), "categories.primary = 'restaurant'", fixed = TRUE)
})

test_that("open_curtain_nl respects schema constraints", {
  skip_if_offline()

  # Test enum constraint
  places <- open_curtain("place")
  result <- places |>
    open_curtain_nl("find places with high confidence")

  query <- dbplyr::sql_render(result)
  expect_match(as.character(query), "confidence > ", fixed = TRUE)

  # Check that invalid category isn't generated
  places <- open_curtain("place")
  result <- places |>
    open_curtain_nl("find invalid_category_name places")

  query <- dbplyr::sql_render(result)
  expect_false(grepl("invalid_category_name", as.character(query), fixed = TRUE))
})

test_that("open_curtain_nl works in dplyr pipelines", {
  skip_if_offline()

  result <- open_curtain("building") |>
    dplyr::select(id, height, type) |>
    open_curtain_nl("buildings taller than 50 meters") |>
    dplyr::filter(dplyr::n() > 0) |>
    head(1) |>
    collect()

  expect_s3_class(result, "sf")
  expect_true(all(result$height > 50))
})

test_that("open_curtain_nl handles errors gracefully", {
  skip_if_offline()

  # Test invalid LLM response
  mock_chat <- function(...) {
    llm_message("Invalid JSON response")
  }

  data <- open_curtain("building")
  expect_error(
    open_curtain_nl(data, "test query", .provider = mock_chat),
    "Failed to parse LLM response"
  )

  # Test invalid filter expression
  mock_chat <- function(...) {
    llm_message("{\"filters\": [\"invalid R syntax >\"]}")
  }

  expect_error(
    open_curtain_nl(data, "test query", .provider = mock_chat),
    "Invalid filter expression"
  )
})