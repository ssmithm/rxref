#' Get core RxNorm properties for one or more RxCUIs
#'
#' @param rxcui Character vector of RxCUIs
#' @param show_progress Logical. Show a progress bar in interactive sessions.
#'
#' @return tibble with rxcui, name, synonym, tty, language, suppress, umlscui.
#'
#' @examples
#' if (identical(Sys.getenv("RXREF_ONLINE"), "1")) {
#' get_properties(c("860975","1049630"))
#' }
#' @export
get_properties <- function(rxcui, show_progress = interactive()) {
  stopifnot(is.character(rxcui))

  .rxref_progress_map_dfr(
    rxcui,
    function(id) {
      if (is.na(id) || !nzchar(id)) {
        return(empty_properties_result(id))
      }

      res <- rx_get_json(paste0("/rxcui/", id, "/properties"))
      p <- rx_pluck(res, "properties", .default = list())

      api_rxcui <- rx_scalar_chr(rx_pluck_chr(p, "rxcui"))

      tibble::tibble(
        rxcui = if (is.na(api_rxcui) || !nzchar(api_rxcui)) id else api_rxcui,
        name = rx_scalar_chr(rx_pluck_chr(p, "name")),
        synonym = rx_scalar_chr(rx_pluck_chr(p, "synonym")),
        tty = rx_scalar_chr(rx_pluck_chr(p, "tty")),
        language = rx_scalar_chr(rx_pluck_chr(p, "language")),
        suppress = rx_scalar_chr(rx_pluck_chr(p, "suppress")),
        umlscui = rx_scalar_chr(rx_pluck_chr(p, "umlscui"))
      )
    },
    name = "Getting properties",
    show_progress = show_progress
  )
}

#' Empty properties result
#'
#' @keywords internal
#' @noRd
empty_properties_result <- function(rxcui = NA_character_) {
  tibble::tibble(
    rxcui = rxcui,
    name = NA_character_,
    synonym = NA_character_,
    tty = NA_character_,
    language = NA_character_,
    suppress = NA_character_,
    umlscui = NA_character_
  )
}
