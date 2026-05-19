#' Resolve free text, RxCUI, or NDC to RxCUI and preferred name
#'
#' Vectorized over `x`. For free text, uses RxNorm approximateTerm.
#' For NDC, uses findRxcuiById. For RxCUI, validates and returns properties.
#'
#' @param x Character vector: drug string, RxCUI, or NDC (10/11-digit or hyphenated)
#' @param type One of c("auto","name","rxcui","ndc"). Default "auto" infers.
#' @param max_entries Integer, passed to approximateTerm for name queries.
#' @param show_progress Logical. Show a progress bar in interactive sessions.
#'   Progress is shown only when at least 5 inputs are supplied.
#'
#' @return A tibble with columns: input, type, rxcui, name, tty, score (if name),
#' ndc11 (if ndc input), matched_term (if name input)
#' @export
resolve <- function(
    x,
    type = c("auto", "name", "rxcui", "ndc"),
    max_entries = 1,
    show_progress = interactive()
) {
  stopifnot(is.character(x))
  type <- match.arg(type)

  .rxref_progress_map_dfr(
    x,
    function(val) {
      t <- switch(
        type,
        auto = {
          if (is_ndcish(val)) {
            "ndc"
          } else if (is_rxcui(val)) {
            "rxcui"
          } else {
            "name"
          }
        },
        name = "name",
        rxcui = "rxcui",
        ndc = "ndc"
      )

      switch(
        t,
        name = resolve_name(val, max_entries = max_entries),
        ndc = resolve_ndc(val),
        rxcui = resolve_rxcui(val)
      )
    },
    name = "Resolving inputs",
    show_progress = show_progress
  )
}

#' @keywords internal
#' @noRd
resolve_name <- function(term, max_entries = 1) {
  # spelling suggestions (optional)
  # NOTE: this is mostly a hint; we don't use it downstream here
  #       but may be useful later for UX / alternative candidates?
  try({
    rx_get_json("/spellingsuggestions", query = list(name = term))
  }, silent = TRUE)

  # approximate matching (primary)
  approx <- rx_get_json("/approximateTerm", query = list(
    term = term,
    maxEntries = max_entries
  ))

  cand <- rx_pluck_list(approx, "approximateGroup", "candidate")

  if (!length(cand)) {
    return(tibble::tibble(
      input = term,
      type = "name",
      rxcui = NA_character_,
      name = NA_character_,
      tty = NA_character_,
      score = NA_real_,
      matched_term = NA_character_
    ))
  }

  cand <- cand[seq_len(min(length(cand), max_entries))]
  rows <- purrr::map(cand, function(c) {
    rxcui <- rx_scalar_chr(rx_pluck_chr(c, "rxcui"))

    props <- if (!is.na(rxcui)) {
      rx_get_json(paste0("/rxcui/", rxcui, "/properties"))
    } else {
      NULL
    }

    tibble::tibble(
      input = term,
      type = "name",
      rxcui = rxcui,
      name = rx_scalar_chr(rx_pluck_chr(props, "properties", "name")),
      tty = rx_scalar_chr(rx_pluck_chr(props, "properties", "tty")),
      score = suppressWarnings(as.numeric(rx_scalar_chr(rx_pluck_chr(c, "score")))),
      matched_term = rx_scalar_chr(rx_pluck_chr(c, "name"))
    )
  })

  dplyr::bind_rows(rows)
}

#' @keywords internal
#' @noRd
resolve_ndc <- function(ndc) {
  ndc_norm <- ndc_to_11(ndc)

  if (is.na(ndc_norm) || !nzchar(ndc_norm)) {
    return(
      tibble::tibble(
        input = ndc,
        type = "ndc",
        ndc11 = NA_character_,
        rxcui = NA_character_,
        name = NA_character_,
        tty = NA_character_,
        score = NA_real_,
        matched_term = NA_character_
      )
    )
  }

  res <- rx_get_json("/rxcui", query = list(idtype = "NDC", id = ndc_norm))

  rx <- rx_pluck_chr(res, "idGroup", "rxnormId")
  rxcui <- rx_scalar_chr(rx)

  props <- if (!is.na(rxcui)) {
    rx_get_json(paste0("/rxcui/", rxcui, "/properties"))
  } else {
    NULL
  }

  tibble::tibble(
    input = ndc,
    type = "ndc",
    ndc11 = ndc_norm,
    rxcui = rxcui,
    name = rx_scalar_chr(rx_pluck_chr(props, "properties", "name")),
    tty  = rx_scalar_chr(rx_pluck_chr(props, "properties", "tty")),
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
    name = rx_scalar_chr(rx_pluck_chr(props, "properties", "name")),
    tty = rx_scalar_chr(rx_pluck_chr(props, "properties", "tty")),
    score = NA_real_,
    matched_term = NA_character_
  )
}
