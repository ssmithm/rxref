#' Get RxNav status for NDCs
#' @param ndc character vector; hyphenated or digits
#' @return tibble ndc_input, ndc11, ndc_hyph, status
#' @export
ndc_status <- function(ndc) {
  stopifnot(is.character(ndc))
  purrr::map_dfr(ndc, function(x) {
    ndc11  <- ndc_to_11(x)
    ndc_h  <- hyphenate_ndc_5_4_2(ndc11)
    st <- tryCatch(rx_get_json("/ndcstatus", query = list(ndc = ndc_h)), error = function(e) NULL)
    tibble::tibble(
      ndc_input = x,
      ndc11 = ndc11,
      ndc_hyph = ndc_h,
      status = if (is.null(st) || is.null(st$ndcStatus$status)) NA_character_ else as.character(st$ndcStatus$status)
    )
  })
}
