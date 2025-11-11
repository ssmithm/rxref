#' Resolve free text, RxCUI, or NDC to RxCUI and preferred name
#'
#' Vectorized over `x`. For free text, uses RxNorm approximateTerm.
#' For NDC, uses findRxcuiById. For RxCUI, validates and returns properties.
#'
#' @param x Character vector: drug string, RxCUI, or NDC (10/11-digit or hyphenated)
#' @param type One of c("auto","name","rxcui","ndc"). Default "auto" infers.
#' @param max_entries Integer, passed to approximateTerm for name queries.
#' @return A tibble with columns: input, type, rxcui, name, tty, score (if name),
#' ndc11 (if ndc input), matched_term (if name input)
#' @export
resolve <- function(x, type = c("auto","name","rxcui","ndc"), max_entries = 1) {
  stopifnot(is.character(x))
  type <- match.arg(type)
  out <- purrr::imap(x, function(val, idx) {
    t <- switch(type,
                auto = {
                  # Prefer NDC if it's 10/11 digits (with or without hyphens)
                  if (is_ndcish(val)) "ndc" else if (is_rxcui(val)) "rxcui" else "name"
                },
                name = "name",
                rxcui = "rxcui",
                ndc   = "ndc"
    )
    switch(t,
           name = resolve_name(val, max_entries = max_entries),
           ndc = resolve_ndc(val),
           rxcui = resolve_rxcui(val)
    )
  })
  dplyr::bind_rows(out)
}

#' @keywords internal
#' @noRd
resolve_name <- function(term, max_entries = 1) {
  # spelling suggestions (optional)
  # NOTE: this is mostly a hint; we don't use it downstream here
  #       but it can be useful later for UX / alternative candidates
  try({
    rx_get_json("/spellingsuggestions", query = list(name = term))
  }, silent = TRUE)

  # approximate matching (primary)
  approx <- rx_get_json("/approximateTerm", query = list(
    term = term,
    maxEntries = max_entries
  ))

  cand <- approx$approximateGroup$candidate
  if (is.null(cand)) {
    return(tibble::tibble(
      input = term, type = "name",
      rxcui = NA_character_, name = NA_character_, tty = NA_character_,
      score = NA_real_, matched_term = NA_character_
    ))
  }

  cand <- cand[seq_len(min(length(cand), max_entries))]
  rows <- purrr::map(cand, function(c) {
    rxcui <- null2na(c$rxcui)
    props <- if (!is.na(rxcui))
      rx_get_json(paste0("/rxcui/", rxcui, "/properties"))
    else NULL

    tibble::tibble(
      input = term,
      type = "name",
      rxcui = rxcui,
      name = null2na(props$properties$name),
      tty = null2na(props$properties$tty),
      score = suppressWarnings(as.numeric(null2na(c$score))),
      matched_term = null2na(c$name)
    )
  })

  dplyr::bind_rows(rows)
}

#' @keywords internal
#' @noRd
resolve_ndc <- function(ndc) {
  ndc_norm <- ndc_to_11(ndc)
  res <- rx_get_json("/rxcui", query = list(idtype = "NDC", id = ndc_norm))
  rx <- res$idGroup$rxnormId
  rxcui <- if (length(rx)) as.character(rx[[1]]) else NA_character_
  props <- if (!is.na(rxcui)) rx_get_json(paste0("/rxcui/", rxcui, "/properties")) else NULL
  tibble::tibble(
    input = ndc,
    type = "ndc",
    ndc11 = ndc_norm,
    rxcui = rxcui,
    name = null2na(props$properties$name),
    tty  = null2na(props$properties$tty),
    score = NA_real_,
    matched_term = NA_character_
  )
}

#' @keywords internal
#' @noRd
resolve_rxcui <- function(rxcui) {
  props <- rx_get_json(paste0("/rxcui/", rxcui, "/properties"))
  tibble::tibble(
    input = rxcui,
    type = "rxcui",
    rxcui = rxcui,
    name = null2na(props$properties$name),
    tty = null2na(props$properties$tty),
    score = NA_real_,
    matched_term = NA_character_
  )
}
