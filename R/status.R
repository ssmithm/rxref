#' Get RxNav status for NDCs
#'
#' `ndc_status()` requests the status of an NDC entry for one or more user-supplied
#' NDC numbers. The returned status will be one "ACTIVE", "OBSOLETE", or "UNSPECIFIED".
#' On occasion, a NULL value may be pulled (resulting in an `NA` in the returned tibble),
#' when RxNorm has no status for a given NDC, or if an invalid NDC is supplied.
#'
#' @param ndc character vector; hyphenated or digits
#' @return A tibble with three columns: ndc_input, ndc11, ndc_hyph, status
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
