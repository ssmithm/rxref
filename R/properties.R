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
        return(tibble::tibble(rxcui = NA_character_))
      }

      res <- rx_get_json(paste0("/rxcui/", id, "/properties"))
      p <- res$properties

      tibble::tibble(
        rxcui = null2na(p$rxcui),
        name = null2na(p$name),
        synonym = null2na(p$synonym),
        tty = null2na(p$tty),
        language = null2na(p$language),
        suppress = null2na(p$suppress),
        umlscui = null2na(p$umlscui)
      )
    },
    name = "Getting properties",
    show_progress = show_progress
  )
}
