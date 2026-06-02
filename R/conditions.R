#' Abort with an rxref API error
#'
#' @keywords internal
#' @noRd
rx_abort_api <- function(
    message,
    class = NULL,
    parent = NULL,
    call = rlang::caller_env()
) {
  rlang::abort(
    message = message,
    class = c(class, "rxref_api_error"),
    parent = parent,
    call = call
  )
}

#' Is this an rxref API error?
#'
#' @keywords internal
#' @noRd
is_rxref_api_error <- function(e) {
  inherits(e, "rxref_api_error")
}


#' Is this an API error that should not be swallowed?
#'
#' @keywords internal
#' @noRd
is_rxref_hard_api_error <- function(e) {
  inherits(e, c(
    "rxref_connection_error",
    "rxref_rate_limit_error",
    "rxref_server_error",
    "rxref_response_error",
    "rxref_empty_response_error",
    "rxref_json_error",
    "rxref_http_error"
  ))
}


#' Evaluate an optional API request
#'
#' Optional API calls should not usually make a whole workflow fail, but
#' infrastructure/API failures should not be silently hidden.
#'
#' @param expr Expression to evaluate.
#' @param fallback Value to return if the optional request fails.
#' @param context Short description of what was attempted.
#' @param warn Logical. Warn when a non-404 API error is downgraded.
#'
#' @keywords internal
#' @noRd
rx_try_optional_api <- function(
    expr,
    fallback = NULL,
    context = "Optional RxNorm API request failed",
    warn = TRUE
) {
  tryCatch(
    expr,
    rxref_not_found_error = function(e) {
      fallback
    },
    rxref_api_error = function(e) {
      if (isTRUE(warn)) {
        cli::cli_warn(c(
          "{context}.",
          "i" = conditionMessage(e),
          "i" = "Returning missing values for this optional enrichment step."
        ))
      }

      fallback
    }
  )
}
