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
#'   stage_prompt("find buildings taller than 50 meters")
#'
#' # Within a pipeline
#' open_curtain("place", spatial_filter = nyc_bbox) |>
#'   dplyr::select(id, names, categories) |>
#'   stage_prompt("show only coffee shops") |>
#'   dplyr::collect()
#' }
#'
#' @export
stage_prompt <- function(
    .data,
    message,
    .provider = getOption("tidyllm_chat_default", default = tidyllm::openai),
    .temperature = 0.1,
    print_response = FALSE,
    ...) {

  # Validate input
  if (!inherits(.data, "overture_call")) {
    stop("Input must be an overture_call object. Use open_curtain() first.", call. = FALSE)
  }

  if (!is.character(message) || length(message) != 1) {
    stop("message must be a single character string", call. = FALSE)
  }

  # Get parsed response from LLM
  params <- create_nl_message(message, .data, .provider, .temperature, ...)
  if(isTRUE(print_response)) print(params)

  # Convert parameter list into filter expressions
  filter_exprs <- lapply(names(params), function(param_name) {
    param_value <- params[[param_name]]

    # Create filter expression based on parameter value
    filter_str <- sprintf("%s >= %s", param_name, param_value)

    tryCatch({
      rlang::parse_expr(filter_str)
    },
    error = function(e) {
      stop("Invalid filter expression for parameter ", param_name,
           " with value ", param_value, "\nError: ", e$message, call. = FALSE)
    })
  })

  # Apply filters
  result <- do.call(dplyr::filter, c(list(.data), filter_exprs))

  return(result)
}

#' Create Natural Language Query Prompt
#'
#' @description
#' Internal function to create the prompt for the language model and handle the chat interaction.
#'
#' @param message The user's natural language query
#' @param data The overture_call object
#' @param .provider The LLM provider to use
#' @param .temperature The temperature setting for the LLM
#' @param ... Additional arguments passed to tidyllm::chat
#'
#' @return A list containing the parsed filter expressions
#'
#' @noRd
create_nl_message <- function(message, data, .provider, .temperature, ...) {
  # Get type from data attributes
  playbill <- attr(data, "overture_playbill")
  type <- playbill[["type"]]
  bbox <- playbill[["bbox"]]

  if (is.null(type)) {
    stop("Could not determine Overture type from data.", call. = FALSE)
  }

  full_schema <- yyjsonr::write_json_str(schema_defs[[type]])

  # Simplified system prompt without redundant schema
  sys_prompt <- glue::glue("
    You are a specialized assistant that converts natural language queries into dplyr filter conditions for Overture Maps {type} data.
    Your task is to generate ONLY relevant filter expressions based on the query.

    Rules:
    1. Only return filter conditions that directly relate to the user's query
    2. For numerical comparisons, use proper operators (>, <, >=, <=, ==)
    3. For text matching, use appropriate string functions (grepl, %in%, etc.)
    4. Never generate example or placeholder values
    5. If a query cannot be translated to a filter, return an empty response

    Here is the full schema with descriptions: {full_schema}
  ")

  if (!is.null(bbox)) {
    sys_prompt <- paste0(sys_prompt, "\nBounding box context: ", bbox)
  }

  msg <- glue::glue("
    Convert this query to dplyr filter expressions for {type} data: {message}
    Only include filters that are specifically requested in the query.
  ")

  # Create LLM message and get response
  llm_msg <- tidyllm::llm_message(msg, .system_prompt = sys_prompt)

  parsed <- tryCatch(
    {
      tidyllm::chat(
        llm_msg,
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

  return(params)
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
