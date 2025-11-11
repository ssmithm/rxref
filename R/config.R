#' Configure rxref
#'
#' @param base_url Override the RxNav base URL (e.g., a local mirror)
#' @param rate_delay Seconds to wait between HTTP calls
#' @param cache A cachem cache object used by memoised calls
#' @return A named list of current settings
#' @export
rxref_conf <- function(base_url = getOption("rxref.base_url"),
                       rate_delay = getOption("rxref.rate_delay"),
                       cache = getOption("rxref.cache")) {
  if (!missing(base_url)) options(rxref.base_url = base_url)
  if (!missing(rate_delay)) options(rxref.rate_delay = rate_delay)
  if (!missing(cache)) options(rxref.cache = cache)
  list(
    base_url = getOption("rxref.base_url"),
    rate_delay = getOption("rxref.rate_delay"),
    cache = getOption("rxref.cache")
  )
}


#' Select backend (API-only for now)
#'
#' @param backend Character scalar; only "api" is currently supported.
#' @export
set_backend <- function(backend = c("api")) {
  backend <- match.arg(backend)
  options(rxref.backend = backend)
  invisible(backend)
}
