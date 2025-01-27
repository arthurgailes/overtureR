#' Filter Overture Maps Data Using Natural Language
#'
#' @description
#' Allows filtering Overture Maps data using natural language queries. This function
#' extends the dplyr pipeline functionality by interpreting natural language descriptions
#' into filter conditions while respecting the Overture Maps schema.
#'
#' @param .data An overture_call object (the result of open_curtain or subsequent operations)
#' @param message Character string containing the natural language query
#' @inheritParams tidyllm::chat
#' @inheritDotParams tidyllm::chat
#'
#' @return An overture_call object with the natural language filters applied
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' open_curtain("building") |>
#'   open_curtain_nl("find buildings taller than 50 meters")
#'
#' # Within a pipeline
#' open_curtain("place", spatial_filter = nyc_bbox) |>
#'   dplyr::select(id, names, categories) |>
#'   open_curtain_nl("show only coffee shops") |>
#'   dplyr::collect()
#' }
#'
#' @importFrom rlang parse_expr
#' @importFrom glue glue
#' @importFrom dplyr filter
#' @export
open_curtain_nl <- function(
    .data,
    message,
    .provider = getOption("tidyllm_chat_default", default = tidyllm::openai),
    .temperature = 0.1,
    ...) {
  # Validate input
  if (!inherits(.data, "overture_call")) {
    stop("Input must be an overture_call object. Use open_curtain() first.", call. = FALSE)
  }

  if (!is.character(message) || length(message) != 1) {
    stop("message must be a single character string", call. = FALSE)
  }

  # Get type from data attributes
  playbill <- attr(.data, "overture_playbill")
  type <- playbill[["type"]]

  if (is.null(type)) {
    stop("Could not determine Overture type from data.", call. = FALSE)
  }

  # Create prompt
  prompt <- create_nl_prompt(message, .data, type)

  # Send to LLM for interpretation
  parsed <- tryCatch(
    {
      tidyllm::chat(
        prompt,
        .provider = .provider,
        .temperature = .temperature,
        .json_schema = create_json_schema(type),
        ...
      )
    },
    error = function(e) {
      stop("Error in LLM query interpretation: ", e$message, call. = FALSE)
    }
  )

  # Extract and validate filter conditions
  params <- tryCatch(
    {
      tidyllm::get_reply_data(parsed)
    },
    error = function(e) {
      stop("Failed to parse LLM response: ", e$message, call. = FALSE)
    }
  )


  # Convert string expressions to quosures and apply filters
  filter_exprs <- lapply(params$filters, function(expr) {
    tryCatch(
      {
        rlang::parse_expr(expr)
      },
      error = function(e) {
        stop("Invalid filter expression: ", expr, "\nError: ", e$message, call. = FALSE)
      }
    )
  })

  # Apply filters
  result <- do.call(dplyr::filter, c(list(.data), filter_exprs))

  return(result)
}

#' Create Natural Language Query Prompt
#'
#' @description
#' Internal function to create the prompt for the language model.
#'
#' @param message The user's natural language query
#' @param data The overture_call object
#' @param type The Overture Maps type
#'
#' @return A tidyllm::LLMMessage object
#'
#' @noRd
create_nl_prompt <- function(message, data, type) {
  bbox <- attr(data, "bbox")

  # Build system prompt
  sys_prompt <- ("You are a specialized assistant that converts natural language
    queries into dplyr filter conditions for Overture Maps data.
  ")

  if (!is.null(bbox)) {
    sys_prompt <- paste0(
      sys_prompt, "Return the same bounding box as the input: ", bbox, "."
    )
  } else {
    sys_prompt <- paste0(sys_prompt, "Guess the bounding box from the query.")
  }

  # Build user message with context and query
  msg <- glue::glue("message")

  tidyllm::llm_message(msg, .system_prompt = sys_prompt)
}

create_json_schema <- function(type) {
  schema <- tidyllm_schema_list[[type]]
  schema[["name"]] <- paste0(type, "_schema")
  # TODO: add bbox

  if (is.null(schema)) {
    stop(sprintf("No schema definition found for type '%s'.", type), call. = FALSE)
  }

  json_schema <- do.call(tidyllm::tidyllm_schema, schema)
  return(json_schema)
}
