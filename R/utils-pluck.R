#' Safely pluck an object
#'
#' @keywords internal
#' @noRd
rx_pluck <- function(x, ..., .default = NULL) {
  purrr::pluck(x, ..., .default = .default)
}


#' Safely pluck a character vector
#'
#' @keywords internal
#' @noRd
rx_pluck_chr <- function(x, ...) {
  out <- purrr::pluck(x, ..., .default = character())

  if (is.null(out)) {
    character()
  } else {
    as.character(out)
  }
}


#' Safely pluck a list
#'
#' @keywords internal
#' @noRd
rx_pluck_list <- function(x, ...) {
  out <- purrr::pluck(x, ..., .default = list())

  if (is.null(out)) {
    list()
  } else if (is.list(out)) {
    out
  } else {
    list(out)
  }
}


#' Return first character value, or NA
#'
#' @keywords internal
#' @noRd
rx_scalar_chr <- function(x) {
  x <- as.character(x)

  if (length(x) == 0L) {
    NA_character_
  } else {
    x[[1]]
  }
}
