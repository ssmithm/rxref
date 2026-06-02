#' Configure rxref
#'
#' Configure package-level settings used by rxref, including the RxNav base URL,
#' API rate delay, and memoised API-response cache.
#'
#' @param base_url Override the RxNav base URL (e.g., a local mirror)
#' @param rate_delay Seconds to wait between HTTP calls
#' @param cache A `cachem` cache object used by memoised API calls. See
#'   [rxref_cache()] for cache management helpers.
#' @return A named list of current settings
#' @export
rxref_conf <- function(base_url = getOption("rxref.base_url"),
                       rate_delay = getOption("rxref.rate_delay"),
                       cache = getOption("rxref.cache")) {
  if (!missing(base_url)) {
    options(rxref.base_url = base_url)
  }

  if (!missing(rate_delay)) {
    options(rxref.rate_delay = rate_delay)
  }

  if (!missing(cache)) {
    if (!inherits(cache, "cachem")) {
      stop(
        "`cache` must be a cachem cache object, such as `cachem::cache_mem()`.",
        call. = FALSE
      )
    }

    options(rxref.cache = cache)
  }

  list(
    base_url = getOption("rxref.base_url"),
    rate_delay = getOption("rxref.rate_delay"),
    cache = getOption("rxref.cache")
  )
}

#' Manage the rxref cache
#'
#' `rxref_cache()` returns the currently configured cache object.
#' `rxref_cache_info()` returns a small summary of the current cache.
#' `rxref_cache_clear()` clears cached RxNorm/RxClass API responses.
#' `rxref_cache_use_memory()` switches rxref to an in-memory cache.
#' `rxref_cache_use_disk()` switches rxref to an on-disk cache.
#' `rxref_cache_disable()` uses an immediately expiring cache, effectively
#' disabling cache reuse for the current R session.
#'
#' rxref caches memoised API responses through a `cachem` cache object stored in
#' `getOption("rxref.cache")`. By default, rxref uses an in-memory cache for the
#' current R session.
#'
#' @return
#' `rxref_cache()` returns a `cachem` cache object.
#'
#' `rxref_cache_info()` returns a tibble with cache metadata.
#'
#' `rxref_cache_clear()`, `rxref_cache_use_memory()`, `rxref_cache_use_disk()`,
#' and `rxref_cache_disable()` invisibly return the configured cache object.
#'
#' @examples
#' rxref_cache_info()
#'
#' rxref_cache_clear()
#'
#' rxref_cache_use_memory()
#'
#' \dontrun{
#' rxref_cache_use_disk()
#' }
#'
#' @export
rxref_cache <- function() {
  .rxref_get_cache()
}


#' @rdname rxref_cache
#' @export
rxref_cache_info <- function() {
  cache <- rxref_cache()

  keys <- tryCatch(
    cache$keys(),
    error = function(e) character()
  )

  size <- tryCatch(
    cache$size(),
    error = function(e) NA_real_
  )

  tibble::tibble(
    cache_class = paste(class(cache), collapse = "/"),
    n_keys = length(keys),
    size_bytes = as.numeric(size)
  )
}


#' @rdname rxref_cache
#' @export
rxref_cache_clear <- function() {
  cache <- rxref_cache()

  tryCatch(
    cache$reset(),
    error = function(e) {
      cli::cli_abort(
        "Could not clear the current rxref cache.",
        parent = e
      )
    }
  )

  invisible(cache)
}


#' @rdname rxref_cache
#' @param max_size Maximum cache size in bytes.
#' @param max_age Maximum age of cached objects, in seconds.
#' @param evict Cache eviction policy passed to `cachem::cache_mem()`.
#' @export
rxref_cache_use_memory <- function(
    max_size = 200 * 1024^2,
    max_age = Inf,
    evict = c("lru", "fifo")
) {
  evict <- match.arg(evict)

  cache <- cachem::cache_mem(
    max_size = max_size,
    max_age = max_age,
    evict = evict
  )

  rxref_conf(cache = cache)

  invisible(cache)
}


#' @rdname rxref_cache
#' @param dir Directory to use for an on-disk cache.
#' @export
rxref_cache_use_disk <- function(
    dir = rxref_cache_dir(),
    max_size = 1024^3,
    max_age = Inf,
    evict = c("lru", "fifo")
) {
  evict <- match.arg(evict)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  cache <- cachem::cache_disk(
    dir = dir,
    max_size = max_size,
    max_age = max_age,
    evict = evict
  )

  rxref_conf(cache = cache)

  invisible(cache)
}


#' @rdname rxref_cache
#' @export
rxref_cache_disable <- function() {
  cache <- cachem::cache_mem(max_age = 0)

  rxref_conf(cache = cache)

  invisible(cache)
}


#' Default rxref cache directory
#'
#' @return Path to the default user cache directory for rxref.
#'
#' @keywords internal
#' @noRd
rxref_cache_dir <- function() {
  tools::R_user_dir("rxref", which = "cache")
}
