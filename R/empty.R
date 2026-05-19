#' Empty resolve result
#'
#' @keywords internal
#' @noRd
empty_resolve_result <- function(query = character()) {
  tibble::tibble(
    query = query,
    rxcui = NA_character_,
    name = NA_character_,
    tty = NA_character_
  )
}

#' Empty NDC-to-RxCUI result
#'
#' @keywords internal
#' @noRd
empty_ndc_to_rxcui_result <- function(ndc = character()) {
  tibble::tibble(
    ndc = ndc,
    rxcui = NA_character_,
    name = NA_character_,
    tty = NA_character_
  )
}

#' Empty RxCUI-to-NDC result
#'
#' @keywords internal
#' @noRd
empty_rxcui_to_ndc_result <- function() {
  tibble::tibble(
    rxcui = character(),
    ndc = character()
  )
}
