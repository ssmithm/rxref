#' @importFrom digest digest
NULL

#' @keywords internal
#' @noRd
rx_http_client <- function() {
  httr2::request(getOption("rxref.base_url")) |>
    httr2::req_user_agent(getOption("rxref.user_agent"))
}


#' @keywords internal
#' @noRd
rx_sleep <- function() {
  delay <- getOption("rxref.rate_delay", 0)
  if (isTRUE(delay > 0)) Sys.sleep(delay)
}

#' Perform an RxNav/RxClass request and parse JSON
#'
#' @param req An httr2 request object.
#' @param service Character label for the service, e.g. "RxNorm" or "RxClass".
#'
#' @return Parsed JSON as a list.
#'
#' @keywords internal
#' @noRd
rx_perform_json <- function(req, service = "RxNav") {
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli::cli_abort(
        c(
          "Could not reach the {service} API.",
          "i" = "Check your internet connection or try again later.",
          "i" = "Original error: {conditionMessage(e)}"
        ),
        class = "rxref_api_error"
      )
    }
  )

  tryCatch(
    httr2::resp_check_status(resp),
    error = function(e) {
      status <- httr2::resp_status(resp)

      if (identical(status, 429L)) {
        cli::cli_abort(
          c(
            "The {service} API returned HTTP 429: too many requests.",
            "i" = "You may be exceeding the RxNorm API request limit.",
            "i" = "Try increasing the delay between API calls, for example: {.code rxref_conf(rate_delay = 0.1)}.",
            "i" = "If you are running many repeated queries, consider using cached results or batching inputs where possible."
          ),
          class = "rxref_api_rate_limit_error",
          parent = e
        )
      }

      cli::cli_abort(
        c(
          "The {service} API returned an unsuccessful response.",
          "i" = "HTTP status: {status}.",
          "i" = "Original error: {conditionMessage(e)}"
        ),
        class = "rxref_api_error",
        parent = e
      )
    }
  )

  txt <- tryCatch(
    httr2::resp_body_string(resp),
    error = function(e) {
      cli::cli_abort(
        c(
          "Could not read the response from the {service} API.",
          "i" = "Original error: {conditionMessage(e)}"
        ),
        class = "rxref_api_error"
      )
    }
  )

  tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(
        c(
          "Could not parse the response from the {service} API as JSON.",
          "i" = "The API response may have changed or may be temporarily unavailable.",
          "i" = "Original error: {conditionMessage(e)}"
        ),
        class = "rxref_api_error"
      )
    }
  )
}


#' @keywords internal
#' @noRd
rx_get_json <- (function() {
  get_cache <- function() {
    opt <- getOption("rxref.cache")
    if (inherits(opt, "memoise_cache")) return(opt)

    memoise::cache_filesystem(
      path = tools::R_user_dir("rxref", which = "cache")
    )
  }

  mem_fun <- memoise::memoise(
    function(path, query = list()) {
      rx_sleep()

      path_json <- if (grepl("\\.json$", path)) {
        path
      } else {
        paste0(path, ".json")
      }

      req <- rx_http_client() |>
        httr2::req_url_path_append(path_json) |>
        httr2::req_url_query(!!!query, .multi = "explode")

      rx_perform_json(req, service = "RxNorm")
    },
    cache = get_cache()
  )

  mem_fun
})()

#' @keywords internal
#' @noRd
rxclass_http_client <- function() {
  httr2::request(
    getOption(
      "rxref.rxclass_base_url",
      "https://rxnav.nlm.nih.gov/REST/rxclass"
    )
  ) |>
    httr2::req_user_agent(getOption("rxref.user_agent"))
}

#' @keywords internal
#' @noRd
.rxclass_collapse_query <- function(x) {
  if (is.null(x)) return(NULL)
  if (length(x) == 0) return(NULL)

  paste(x, collapse = " ")
}

#' @keywords internal
#' @noRd
rxclass_get_json <- (function() {
  get_cache <- function() {
    opt <- getOption("rxref.cache")
    if (inherits(opt, "memoise_cache")) return(opt)

    memoise::cache_filesystem(
      path = tools::R_user_dir("rxref", which = "cache")
    )
  }

  mem_fun <- memoise::memoise(
    function(path, query = list()) {
      rx_sleep()

      path_json <- if (grepl("\\.json$", path)) {
        path
      } else {
        paste0(path, ".json")
      }

      req <- rxclass_http_client() |>
        httr2::req_url_path_append(path_json) |>
        httr2::req_url_query(!!!query, .multi = "explode")

      rx_perform_json(req, service = "RxClass")
    },
    cache = get_cache()
  )

  mem_fun
})()

#' @keywords internal
#' @noRd
null2chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  as.character(x[[1]])
}


# tidy helpers --------------------------------------------------------------

#' @keywords internal
#' @noRd
.as_tibble <- function(x) tibble::as_tibble(x, .name_repair = "unique")

#' @keywords internal
#' @noRd
null2na <- function(x) if (is.null(x)) NA else x

#' @keywords internal
#' @noRd
vec_recycle_len <- function(x, n) vctrs::vec_recycle(x, n)

#' @keywords internal
#' @noRd
is_rxcui <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(FALSE)

  # Only digits
  if (!grepl("^[0-9]+$", x)) return(FALSE)

  # If it's 10 or 11 digits, treat as NDC-ish, not RxCUI
  !nchar(x) %in% c(10L, 11L)
}

#' @keywords internal
#' @noRd
is_ndcish <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(FALSE)

  digits <- gsub("[^0-9]", "", x)

  grepl("^[0-9]{10,11}$", digits)
}


#' @keywords internal
#' @noRd
.ndc_parts_from_string <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    return(list(
      labeler = NA_character_,
      product = NA_character_,
      package = NA_character_,
      raw = x
    ))
  }

  raw <- x
  x <- trimws(x)

  # Preserve hyphenated structure if present
  cleaned <- gsub("[^0-9-]", "", x)

  if (grepl("^\\d+-\\d+-\\d+$", cleaned)) {
    p <- strsplit(cleaned, "-", fixed = TRUE)[[1]]

    return(list(
      labeler = p[[1]],
      product = p[[2]],
      package = p[[3]],
      raw = raw
    ))
  }

  digits <- gsub("[^0-9]", "", x)

  if (!grepl("^[0-9]{10,11}$", digits)) {
    return(list(
      labeler = NA_character_,
      product = NA_character_,
      package = NA_character_,
      raw = raw
    ))
  }

  # If 11 digits, assume already normalized 5-4-2
  if (nchar(digits) == 11L) {
    return(list(
      labeler = substr(digits, 1L, 5L),
      product = substr(digits, 6L, 9L),
      package = substr(digits, 10L, 11L),
      raw = raw
    ))
  }

  # If 10 digits and not hyphenated, grouping cannot be known reliably
  list(
    labeler = NA_character_,
    product = NA_character_,
    package = NA_character_,
    raw = raw
  )
}

#' @keywords internal
#' @noRd
ndc_to_11 <- function(ndc) {
  stopifnot(is.character(ndc))

  purrr::map_chr(ndc, function(x) {
    if (is.na(x) || !nzchar(x)) {
      return(NA_character_)
    }

    x <- trimws(x)

    # If already an 11-digit NDC, return digits only
    digits_only <- gsub("[^0-9]", "", x)

    if (nchar(digits_only) == 11 && !grepl("-", x)) {
      return(digits_only)
    }

    # Handle hyphenated FDA 10-digit formats
    parts <- strsplit(x, "-", fixed = TRUE)[[1]]

    if (length(parts) == 3) {
      parts <- trimws(parts)

      labeler <- parts[[1]]
      product <- parts[[2]]
      package <- parts[[3]]

      widths <- nchar(parts)

      # 4-4-2 format: pad labeler to 5 digits
      if (identical(widths, c(4L, 4L, 2L))) {
        return(paste0(
          stringr::str_pad(labeler, width = 5, side = "left", pad = "0"),
          product,
          package
        ))
      }

      # 5-3-2 format: pad product to 4 digits
      if (identical(widths, c(5L, 3L, 2L))) {
        return(paste0(
          labeler,
          stringr::str_pad(product, width = 4, side = "left", pad = "0"),
          package
        ))
      }

      # 5-4-1 format: pad package to 2 digits
      if (identical(widths, c(5L, 4L, 1L))) {
        return(paste0(
          labeler,
          product,
          stringr::str_pad(package, width = 2, side = "left", pad = "0")
        ))
      }
    }

    # Do not infer 11-digit format from non-hyphenated 10-digit NDCs.
    # Without hyphens, the original FDA format is ambiguous, e.g.,
    # 4-4-2, 5-3-2, or 5-4-1?
    if (nchar(digits_only) == 10L && !grepl("-", x)) {
      return(NA_character_)
    }


    # Fallback: if removing punctuation gives 11 digits, use that
    if (nchar(digits_only) == 11) {
      return(digits_only)
    }

    NA_character_
  })
}

#' @keywords internal
#' @noRd
hyphenate_ndc_5_4_2 <- function(ndc) {
  if (!is.character(ndc)) {
    return(ndc)
  }

  purrr::map_chr(ndc, function(x) {
    if (is.na(x)) return(NA_character_)

    digits <- gsub("[^0-9]", "", x)

    if (!grepl("^[0-9]{11}$", digits)) {
      return(x)
    }

    paste0(
      substr(digits, 1L, 5L), "-",
      substr(digits, 6L, 9L), "-",
      substr(digits, 10L, 11L)
    )
  })
}

#' @keywords internal
#' @noRd
rx_perform <- function(req) {
  tryCatch(
    httr2::req_perform(req),
    httr2_http_429 = function(cnd) {
      cli::cli_abort(c(
        "RxNorm returned HTTP 429: too many requests.",
        "i" = "You may be exceeding NLM's recommended limit of 20 requests per second per IP address.",
        "i" = "Try increasing the request delay, for example: {.code rxref_conf(rate_delay = 0.1)}.",
        "i" = "If you are making many repeated calls, consider relying on caching or batching inputs where possible."
      ), parent = cnd)
    }
  )
}
