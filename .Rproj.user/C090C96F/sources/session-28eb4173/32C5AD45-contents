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


#' @keywords internal
#' @noRd
rx_get_json <- (function(){
  get_cache <- function() {
    opt <- getOption("rxref.cache")
    if (inherits(opt, "memoise_cache")) return(opt)
    memoise::cache_filesystem(path = tools::R_user_dir("rxref", which = "cache"))
  }
  mem_fun <- memoise::memoise(
    function(path, query = list()) {
      rx_sleep()
      path_json <- if (grepl("\\.json$", path)) path else paste0(path, ".json")
      req <- rx_http_client() |>
        httr2::req_url_path_append(path_json) |>
        # FIX: use explode to repeat vector keys (e.g., tty=SCD&tty=SBD)
        httr2::req_url_query(!!!query, .multi = "explode")
      resp <- httr2::req_perform(req)
      httr2::resp_check_status(resp)
      txt <- httr2::resp_body_string(resp)
      jsonlite::fromJSON(txt, simplifyVector = FALSE)
    },
    cache = get_cache()
  )
  mem_fun
})()


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
  if (!is.character(x)) return(FALSE)
  # Only digits
  if (!grepl("^[0-9]+$", x)) return(FALSE)
  # If it's 10 or 11 digits, treat as NDC-ish, not RxCUI
  !nchar(x) %in% c(10L, 11L)
}

#' @keywords internal
#' @noRd
is_ndcish <- function(x) {
  if (!is.character(x)) return(FALSE)
  digits <- gsub("-", "", x)
  grepl("^[0-9]{10,11}$", digits)
}


#' @keywords internal
#' @noRd
.ndc_parts_from_string <- function(x) {
  x <- gsub("[^0-9-]", "", x)
  if (grepl("^\\d+-\\d+-\\d+$", x)) {
    p <- strsplit(x, "-", fixed = TRUE)[[1]]
    return(list(labeler=p[1], product=p[2], package=p[3], raw=x))
  }
  digits <- gsub("-", "", x)
  if (!grepl("^[0-9]{10,11}$", digits)) return(list(labeler=NA, product=NA, package=NA, raw=x))
  # If 11 digits, assume already 5-4-2
  if (nchar(digits) == 11) {
    return(list(labeler=substr(digits,1,5), product=substr(digits,6,9), package=substr(digits,10,11), raw=x))
  }
  # If 10 digits, we can't know grouping for sure—fall back to common heuristics via hyphen input
  # Leave as unknown; callers can attempt guesses if needed
  list(labeler=NA, product=NA, package=NA, raw=x)
}

#' @keywords internal
#' @noRd
ndc_to_11 <- function(x) {
  # Accept 10- or 11-digit strings, or hyphenated 10-digit in FDA forms.
  x <- trimws(x)
  # If hyphenated, detect the FDA pattern and pad the correct segment
  if (grepl("^\\d+-\\d+-\\d+$", x)) {
    p <- strsplit(x, "-", fixed = TRUE)[[1]]
    lens <- nchar(p)
    if (all(lens == c(4,4,2))) {
      # 4-4-2 -> 5-4-2 (pad labeler)
      return(paste0(sprintf("%05s", p[1]), p[2], p[3]))
    } else if (all(lens == c(5,3,2))) {
      # 5-3-2 -> 5-4-2 (pad product)
      return(paste0(p[1], sprintf("%04s", p[2]), p[3]))
    } else if (all(lens == c(5,4,1))) {
      # 5-4-1 -> 5-4-2 (pad package)
      return(paste0(p[1], p[2], sprintf("%02s", p[3])))
    } else if (all(lens == c(5,4,2))) {
      # already canonical hyphenation; just strip hyphens
      return(paste0(p, collapse = ""))
    }
  }
  # Non-hyphenated: strip hyphens and pad 10->11 on the left (fallback)
  digits <- gsub("-", "", x)
  if (nchar(digits) == 11) return(digits)
  if (nchar(digits) == 10)  return(stringr::str_pad(digits, width = 11, side = "left", pad = "0"))
  digits
}

#' @keywords internal
#' @noRd
hyphenate_ndc_5_4_2 <- function(ndc) {
  ndc <- gsub("-", "", ndc)
  if (!grepl("^[0-9]{11}$", ndc)) return(ndc)
  paste0(substr(ndc,1,5), "-", substr(ndc,6,9), "-", substr(ndc,10,11))
}

