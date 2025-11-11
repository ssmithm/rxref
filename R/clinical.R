#' Clinical attributes from the concept (SCD/SBD) or related SCD/SBD
#'
#' If `rxcui` is already a clinical drug (SCD/SBD), parse attributes directly
#' from its own name. Otherwise, query \code{/rxcui/{id}/related?tty=SCD,SBD} and parse.
#'
#' @param rxcui Character vector of RxCUIs
#' @return tibble with rxcui, related_rxcui, name, tty, strength, dose_form
#' @examples
#' \donttest{
#' get_clinical_attributes(c("861007","860975")) |>
#'   dplyr::select(rxcui, related_rxcui, name, strength, dose_form, tty) |>
#'   head()
#' }
#' @export
get_clinical_attributes <- function(rxcui) {
  stopifnot(is.character(rxcui))

  # parser helper: pull "500 MG", "10 MG/ML", "0.1 %", "5,000 UNITS" etc.
  parse_strength_dose <- function(nm) {
    if (is.na(nm) || !nzchar(nm)) {
      return(list(strength = NA_character_, dose_form = NA_character_))
    }
    # allow commas in numbers; allow ratios and %
    # capture the FIRST strength occurrence
    # e.g., "24 HR metformin hydrochloride 500 MG Extended Release Oral Tablet"
    rx <- "(\\d{1,3}(?:[\\d,]*)(?:\\.\\d+)?(?:\\s*(?:/|per)\\s*\\d+(?:[\\d,]*)(?:\\.\\d+)?)?\\s*(?:MCG|MG|G|KG|ML|L|MEQ|MMOL|IU|UNITS|%)(?:\\s*/\\s*(?:ML|G|L))?)"
    m  <- regexpr(rx, nm, ignore.case = TRUE, perl = TRUE)
    if (m[1] <= 0) {
      return(list(strength = NA_character_, dose_form = NA_character_))
    }
    strength <- trimws(regmatches(nm, m)[[1]])
    pos <- m[1] + attr(m, "match.length")
    tail <- trimws(substr(nm, pos, nchar(nm)))
    # heuristic: dose form is the remainder, but drop leading punctuation
    tail <- sub("^[,;:.-]\\s*", "", tail)
    list(strength = strength, dose_form = if (nzchar(tail)) tail else NA_character_)
  }

  purrr::map_dfr(rxcui, function(id) {
    # 1) Pull properties to know TTY and get name
    props <- tryCatch(rx_get_json(paste0("/rxcui/", id, "/properties")),
                      error = function(e) NULL)
    p <- props$properties
    tty_self  <- null2na(p$tty)
    name_self <- null2na(p$name)

    # If the concept is already SCD/SBD, parse itself
    if (!is.na(tty_self) && tty_self %in% c("SCD","SBD")) {
      pr <- parse_strength_dose(name_self)
      return(tibble::tibble(
        rxcui = id,
        related_rxcui = id,
        name = name_self,
        tty = tty_self,
        strength = pr$strength,
        dose_form = pr$dose_form
      ))
    }

    # 2) Otherwise, fetch related SCD/SBD
    rel <- tryCatch(
      rx_get_json(paste0("/rxcui/", id, "/related"), query = list(tty = "SCD+SBD")),
      error = function(e) NULL
    )
    groups <- rel$relatedGroup$conceptGroup
    if (is.null(groups)) {
      # no related clinical concepts found
      return(tibble::tibble(
        rxcui = id,
        related_rxcui = NA_character_,
        name = NA_character_,
        tty = NA_character_,
        strength = NA_character_,
        dose_form = NA_character_
      ))
    }

    # Flatten conceptProperties across groups robustly
    concepts <- purrr::map(groups, "conceptProperties") |>
      purrr::compact() |>
      unlist(recursive = FALSE)

    if (!length(concepts)) {
      return(tibble::tibble(
        rxcui = id,
        related_rxcui = NA_character_,
        name = NA_character_,
        tty = NA_character_,
        strength = NA_character_,
        dose_form = NA_character_
      ))
    }

    purrr::map_dfr(concepts, function(cp) {
      nm  <- null2na(cp$name)
      tty <- null2na(cp$tty)
      pr  <- parse_strength_dose(nm)
      tibble::tibble(
        rxcui = id,
        related_rxcui = null2na(cp$rxcui),
        name = nm,
        tty = tty,
        strength = pr$strength,
        dose_form = pr$dose_form
      )
    })
  })
}
