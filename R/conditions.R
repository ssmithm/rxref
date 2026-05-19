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
