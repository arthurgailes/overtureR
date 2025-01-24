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
  theme <- playbill[["theme"]]

  if (is.null(type) || is.null(theme)) {
    stop("Could not determine Overture type/theme from data.", call. = FALSE)
  }

  # Create prompt
  prompt <- create_nl_prompt(message, .data, type, theme)

  # Send to LLM for interpretation
  parsed <- tryCatch({
    tidyllm::chat(prompt, .provider =.provider, .temperature = .temperature, ...)
  }, error = function(e) {
    stop("Error in LLM query interpretation: ", e$message, call. = FALSE)
  })

  # Extract and validate filter conditions
  params <- tryCatch({
    tidyllm::get_reply_data(parsed)
  }, error = function(e) {
    stop("Failed to parse LLM response: ", e$message, call. = FALSE)
  })

  if (!is.list(params) || is.null(params$filters)) {
    stop("Invalid LLM response format. Expected list with 'filters' field.", call. = FALSE)
  }

  # Convert string expressions to quosures and apply filters
  filter_exprs <- lapply(params$filters, function(expr) {
    tryCatch({
      rlang::parse_expr(expr)
    }, error = function(e) {
      stop("Invalid filter expression: ", expr, "\nError: ", e$message, call. = FALSE)
    })
  })

  # Apply filters
  result <- .data |>
    dplyr::filter(!!!filter_exprs)

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
#' @param theme The Overture Maps theme
#'
#' @return A tidyllm::LLMMessage object
#'
#' @noRd
create_nl_prompt <- function(message, data, type, theme) {
  # Get available columns
  cols <- colnames(data)

  # Get schema definition
  schema_def <- schema_defs[[type]]
  if (is.null(schema_def)) {
    stop(sprintf("No schema definition found for type '%s'.", type), call. = FALSE)
  }

  # Extract property definitions and constraints
  properties <- schema_def$properties$properties$properties
  schema_info <- lapply(names(properties), function(prop) {
    prop_def <- properties[[prop]]
    if (!is.null(prop_def$enum)) {
      return(sprintf("%s: enum [%s]", prop, paste(prop_def$enum, collapse = ", ")))
    }
    if (!is.null(prop_def$type)) {
      type_info <- if (is.list(prop_def$type)) {
        paste(unlist(prop_def$type), collapse = "|")
      } else {
        prop_def$type
      }
      return(sprintf("%s: %s", prop, type_info))
    }
    if (!is.null(prop_def$description)) {
      return(sprintf("%s: %s", prop, prop_def$description))
    }
    return(NULL)
  })

  schema_text <- paste(Filter(Negate(is.null), schema_info), collapse = "\n")

  # Build system prompt
  sys_prompt <- "You are a specialized assistant that converts natural language queries
into dplyr filter conditions for Overture Maps data."

  # Build user message with context and query
  msg <- glue::glue("
Using the following context, convert my query into dplyr filter expressions.

Data Context:
- Type: {type}
- Theme: {theme}
- Available columns: {paste(cols, collapse = ', ')}

Schema constraints:
{schema_text}

Return a JSON object with a single field 'filters' containing an array of dplyr filter expressions.
Each filter expression must:
1. Use only available columns
2. Respect schema constraints
3. Be a valid dplyr expression
4. Use proper R syntax

Example responses:
{{
  \"filters\": [\"height > 100\"]
}}

{{
  \"filters\": [
    \"categories.primary == 'restaurant'\",
    \"confidence > 0.8\"
  ]
}}

Query: {message}")

  tidyllm::llm_message(msg, .system_prompt = sys_prompt)
}