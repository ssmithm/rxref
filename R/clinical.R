#' Clinical attributes from the concept (SCD/SBD) or related SCD/SBD
#'
#' If `rxcui` is already a clinical drug (TTY = SCD or SBD), parse attributes directly
#' from its own name. Otherwise, query \code{/rxcui/{id}/related?tty=SCD,SBD} and parse.
#'
#' In addition to strength and dose form, this returns route, dose-form group (DFG),
#' brand/generic flags, ingredient summaries, and a simple active/inactive status.
#'
#' Note there is a fair amount of parsing of the RxNorm STR value to try to extract
#' relevant information (e.g., strength, dose_form), so check closely before trusting.
#' There may be edge cases that are not correctly parsed.
#'
#' @param rxcui Character vector of RxCUIs
#' @return A tibble with columns:
#'   \describe{
#'     \item{rxcui}{Input RxCUI}
#'     \item{related_rxcui}{Clinical drug RxCUI (SCD/SBD) used for attributes}
#'     \item{name}{Clinical drug name}
#'     \item{tty}{Term type (SCD/SBD, etc.)}
#'     \item{strength}{Parsed strength string (e.g. "500 MG")}
#'     \item{dose_form}{Parsed dose form (e.g. "Extended Release Oral Tablet")}
#'     \item{route}{Route parsed from dose form / DFG (e.g. "ORAL", "INJECTION")}
#'     \item{dose_form_group}{Dose form group (DFG), if available}
#'     \item{is_brand}{Logical; TRUE for branded clinical concepts (SBD/BPCK)}
#'     \item{is_generic}{Logical; TRUE for generic clinical concepts (SCD/GPCK)}
#'     \item{ingredient_count}{Number of distinct ingredients}
#'     \item{ingredient_rxcui}{List-column of ingredient RxCUIs}
#'     \item{ingredient_name}{List-column of ingredient names}
#'     \item{ingredient_tty}{List-column of ingredient term types (IN/PIN/MIN)}
#'     \item{is_multi_ingredient}{Logical; TRUE if >1 ingredient}
#'     \item{suppress}{Raw RxNorm suppress flag from properties}
#'     \item{status}{Simple status derived from suppress: "ACTIVE" vs "INACTIVE"}
#'   }
#' @examples
#' \donttest{
#' get_clinical_attributes(c("861007","860975")) |>
#'   dplyr::select(rxcui, related_rxcui, name, strength, dose_form, route, tty) |>
#'   head()
#' }
#' @export
get_clinical_attributes <- function(rxcui) {
  stopifnot(is.character(rxcui))

  # parser helper: pull "500 MG", "10 MG/ML", "0.1 %", "5,000 UNITS" etc.
  # also correctly pull combo strengths, e.g., "25 MG / 20 MG"
  parse_strength_dose <- function(nm) {
    if (is.na(nm) || !nzchar(nm)) {
      return(list(strength = NA_character_, dose_form = NA_character_))
    }

    # Capture strength-like tokens:
    #  - 2.5 MG, 25 MG
    #  - 1 MG/ML, 0.68 MG/ML
    #  - 0.00417 MG/HR (transdermal systems)
    #  - 1 %, etc.
    #
    # Leading: number + unit (no HR here, we don't want "24 HR").
    # Optional: "/ [optional number] + unit", where unit can include HR.
    rx <- paste0(
      "(",
      "\\d{1,4}(?:[\\d,]*)(?:\\.\\d+)?",           # leading number
      "\\s*(?:MCG|MG|G|KG|ML|L|MEQ|MMOL|IU|UNITS|%)\\b",  # unit + word boundary
      "\\s*(?:/\\s*",                              # optional "/ ..."
      "(?:\\d{1,4}(?:[\\d,]*)(?:\\.\\d+)?)?",      # optional denominator number
      "\\s*(?:MCG|MG|G|KG|ML|L|MEQ|MMOL|IU|UNITS|%|HR)\\b", # denominator unit + HR allowed
      ")?",
      ")"
    )

    m <- gregexpr(rx, nm, ignore.case = TRUE, perl = TRUE)
    starts <- m[[1]]

    # no matches
    if (length(starts) == 1L && starts[1] == -1L) {
      return(list(strength = NA_character_, dose_form = NA_character_))
    }

    lens    <- attr(m[[1]], "match.length")
    matches <- regmatches(nm, m)[[1]]
    matches <- trimws(matches)

    # "ratio-like" strengths: MG/ML, MG/HR, %, "per"
    has_ratio <- grepl("(/|per|%)", matches, ignore.case = TRUE)

    if (any(has_ratio)) {
      # For concentrations or per-time (1.34 MG/ML, 1 MG/ML, 0.00417 MG/HR, 0.1 %)
      idx <- tail(which(has_ratio), 1L)
      strength <- matches[idx]
    } else if (length(matches) > 1L) {
      # Combination product with multiple strengths, no explicit ratio:
      # e.g. "chlorthalidone 15 MG / clonidine 0.1 MG Oral Tablet"
      strength <- paste(matches, collapse = " / ")
      idx <- length(matches)
    } else {
      # Single plain strength
      idx <- 1L
      strength <- matches[idx]
    }

    pos_end <- starts[idx] + lens[idx] - 1L
    tail    <- trimws(substr(nm, pos_end + 1L, nchar(nm)))
    tail    <- sub("^[,;:.-]\\s*", "", tail)

    list(
      strength  = strength,
      dose_form = if (nzchar(tail)) tail else NA_character_
    )
  }

  # 1. Core clinical mapping
  core <- purrr::map_dfr(rxcui, function(id) {
    # 1) Pull properties to know TTY and get name
    props <- tryCatch(
      rx_get_json(paste0("/rxcui/", id, "/properties")),
      error = function(e) NULL
    )
    p <- props$properties
    tty_self  <- null2na(p$tty)
    name_self <- null2na(p$name)

    # If the concept is already SCD/SBD, parse it
    if (!is.na(tty_self) && tty_self %in% c("SCD","SBD")) {
      pr <- parse_strength_dose(name_self)
      return(tibble::tibble(
        rxcui        = id,
        related_rxcui = id,
        name         = name_self,
        tty          = tty_self,
        strength     = pr$strength,
        dose_form    = pr$dose_form
      ))
    }

    # Otherwise, fetch related SCD/SBD
    rel <- tryCatch(
      rx_get_json(
        paste0("/rxcui/", id, "/related"),
        query = list(tty = "SCD SBD")
      ),
      error = function(e) NULL
    )
    groups <- rel$relatedGroup$conceptGroup
    if (is.null(groups)) {
      # no related clinical concepts found
      return(tibble::tibble(
        rxcui        = id,
        related_rxcui = NA_character_,
        name         = NA_character_,
        tty          = NA_character_,
        strength     = NA_character_,
        dose_form    = NA_character_
      ))
    }

    # Flatten conceptProperties across groups robustly
    concepts <- purrr::map(groups, "conceptProperties") |>
      purrr::compact() |>
      unlist(recursive = FALSE)

    if (!length(concepts)) {
      return(tibble::tibble(
        rxcui        = id,
        related_rxcui = NA_character_,
        name         = NA_character_,
        tty          = NA_character_,
        strength     = NA_character_,
        dose_form    = NA_character_
      ))
    }

    purrr::map_dfr(concepts, function(cp) {
      nm  <- null2na(cp$name)
      tty <- null2na(cp$tty)
      pr  <- parse_strength_dose(nm)
      tibble::tibble(
        rxcui        = id,
        related_rxcui = null2na(cp$rxcui),
        name         = nm,
        tty          = tty,
        strength     = pr$strength,
        dose_form    = pr$dose_form
      )
    })
  })

  # nothing else to do if everything failed
  if (!nrow(core)) return(core)

  # 2. Brand / generic flags
  core <- core |>
    dplyr::mutate(
      is_brand   = .data$tty %in% c("SBD", "BPCK"),
      is_generic = .data$tty %in% c("SCD", "GPCK")
    )

  clinical_ids <- unique(core$related_rxcui[!is.na(core$related_rxcui)])

  # if no clinical concepts, just add empty columns and return
  if (!length(clinical_ids)) {
    return(
      core |>
        dplyr::mutate(
          dose_form_group     = NA_character_,
          ingredient_count    = NA_integer_,
          ingredient_rxcui    = list(NULL),
          ingredient_name     = list(NULL),
          ingredient_tty      = list(NULL),
          is_multi_ingredient = NA,
          suppress            = NA_character_,
          status              = NA_character_,
          route               = NA_character_
        )
    )
  }

  # 3. Dose form group (DFG) from RxNorm (TTY = DFG)
  dfg_tbl <- purrr::map_dfr(clinical_ids, function(id) {
    rel <- tryCatch(
      rx_get_json(paste0("/rxcui/", id, "/related"), query = list(tty = "DFG")),
      error = function(e) NULL
    )
    groups <- rel$relatedGroup$conceptGroup
    if (is.null(groups)) {
      return(tibble::tibble(
        related_rxcui   = id,
        dose_form_group = NA_character_
      ))
    }
    concepts <- purrr::map(groups, "conceptProperties") |>
      purrr::compact() |>
      unlist(recursive = FALSE)
    if (!length(concepts)) {
      return(tibble::tibble(
        related_rxcui   = id,
        dose_form_group = NA_character_
      ))
    }
    # take the first DFG name (usually there is only one)
    nm <- null2na(concepts[[1]]$name)
    tibble::tibble(
      related_rxcui   = id,
      dose_form_group = nm
    )
  })

  # 4. Ingredient summary
  ing_raw <- tryCatch(
    .rxref_get_ingredients_for_rxcui(
      clinical_ids,
      include_pin = TRUE,
      include_min = FALSE
    ),
    error = function(e) NULL
  )

  if (!is.null(ing_raw) && nrow(ing_raw)) {
    ing_raw <- ing_raw |>
      dplyr::filter(!is.na(.data$ingredient_rxcui))
  }

  if (is.null(ing_raw) || !nrow(ing_raw)) {
    ing_summary <- tibble::tibble(
      related_rxcui        = clinical_ids,
      ingredient_count     = 0L,
      ingredient_rxcui     = replicate(length(clinical_ids), list(character(0L))),
      ingredient_name      = replicate(length(clinical_ids), list(character(0L))),
      ingredient_tty       = replicate(length(clinical_ids), list(character(0L))),
      is_multi_ingredient  = FALSE
    )
  } else {
    # Prefer IN over PIN when both present for a given product
    ing_raw <- ing_raw |>
      dplyr::group_by(.data$related_rxcui) |>
      dplyr::filter(!(.data$ingredient_tty == "PIN" & any(.data$ingredient_tty == "IN"))) |>
      dplyr::ungroup()

    ing_raw2 <- ing_raw |>
      dplyr::distinct(
        .data$related_rxcui,
        .data$ingredient_rxcui,
        .keep_all = TRUE
      )

    ing_summary <- ing_raw2 |>
      dplyr::group_by(.data$related_rxcui) |>
      dplyr::summarise(
        ingredient_count  = dplyr::n(),
        ingredient_rxcui  = list(.data$ingredient_rxcui),
        ingredient_name   = list(.data$ingredient_name),
        ingredient_tty    = list(.data$ingredient_tty),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        is_multi_ingredient = .data$ingredient_count > 1L
      )
  }

  # 5. Status from suppress (via get_properties)
  props_clin <- tryCatch(
    get_properties(clinical_ids),
    error = function(e) tibble::tibble(rxcui = clinical_ids, suppress = NA_character_)
  )

  status_tbl <- props_clin |>
    dplyr::transmute(
      related_rxcui = .data$rxcui,
      suppress      = .data$suppress,
      status        = dplyr::case_when(
        is.na(.data$suppress) ~ NA_character_,
        .data$suppress == "N" ~ "ACTIVE",
        TRUE                  ~ "INACTIVE"
      )
    )

  # 6. Merge everything + expanded route mapping
  out <- core |>
    dplyr::left_join(dfg_tbl,     by = "related_rxcui") |>
    dplyr::left_join(ing_summary, by = "related_rxcui") |>
    dplyr::left_join(status_tbl,  by = "related_rxcui") |>
    dplyr::mutate(
      route = dplyr::case_when(
        # ORAL-like products (DFG-based)
        dose_form_group %in% c(
          "Oral Product",
          "Disintegrating Oral Product",
          "Oral Liquid Product",
          "Oral Cream Product",
          "Oral Foam Product",
          "Oral Gel Product",
          "Oral Ointment Product",
          "Oral Paste Product",
          "Oral Powder Product",
          "Oral Spray Product",
          "Oral Strip Product",
          "Flake Product",
          "Granule Product",
          "Pellet Product",
          "Pill",
          "Lozenge Product",
          "Buccal Product",
          "Sublingual Product",
          "Wafer Product",
          "Mouthwash Product",
          "Toothpaste Product",
          "Dental Product"
        ) ~ "ORAL",

        # Injectables and related
        dose_form_group %in% c(
          "Injectable Product",
          "Intraperitoneal Product",
          "Intratracheal Product",
          "Irrigation Product"
        ) ~ "INJECTION",

        # Ophthalmic / Otic / Nasal / Inhalant
        dose_form_group == "Ophthalmic Product" ~ "OPHTHALMIC",
        dose_form_group == "Otic Product"       ~ "OTIC",
        dose_form_group %in% c("Nasal Product", "Inhalant Product") ~ "INHALATION",

        # Rectal / Vaginal / Urethral
        dose_form_group == "Rectal Product"   ~ "RECTAL",
        dose_form_group == "Vaginal Product"  ~ "VAGINAL",
        dose_form_group == "Urethral Product" ~ "URETHRAL",

        # Topical / Transdermal / Mucosal / skin-type products
        dose_form_group %in% c(
          "Topical Product",
          "Transdermal Product",
          "Mucosal Product",
          "Medicated Pad or Tape",
          "Shampoo Product",
          "Soap Product"
        ) ~ "TOPICAL",

        # Implants
        dose_form_group == "Drug Implant Product" ~ "IMPLANT",

        # Fallbacks when DFG missing or unfamiliar: derive from dose_form string
        stringr::str_detect(dose_form, "(?i)ophthalmic") ~ "OPHTHALMIC",
        stringr::str_detect(dose_form, "(?i)otic")        ~ "OTIC",
        stringr::str_detect(dose_form, "(?i)nasal|inhal") ~ "INHALATION",
        stringr::str_detect(dose_form, "(?i)rectal|enema") ~ "RECTAL",
        stringr::str_detect(dose_form, "(?i)vaginal|douche") ~ "VAGINAL",
        stringr::str_detect(dose_form, "(?i)urethral")    ~ "URETHRAL",
        stringr::str_detect(dose_form, "(?i)implant")     ~ "IMPLANT",
        stringr::str_detect(dose_form, "(?i)transdermal|topical|gel|cream|ointment|lotion|patch") ~ "TOPICAL",
        stringr::str_detect(dose_form, "(?i)oral")        ~ "ORAL",

        TRUE ~ NA_character_
      )
    )

  out
}



# Internal: get ingredient concepts (IN/PIN/MIN) for one or more RxCUIs
#' @keywords internal
#' @noRd
.rxref_get_ingredients_for_rxcui <- function(rxcui,
                                             include_pin = TRUE,
                                             include_min = TRUE) {
  stopifnot(is.character(rxcui))

  ttys <- c("IN")
  if (include_pin) ttys <- c(ttys, "PIN")
  if (include_min) ttys <- c(ttys, "MIN")

  # tty_query <- paste(ttys, collapse = "+")
  tty_query <- paste(ttys, collapse = " ")  # "IN PIN MIN" -> encoded as IN+PIN+MIN

  purrr::map_dfr(rxcui, function(id) {
    if (is.na(id) || !nzchar(id)) {
      return(tibble::tibble(
        related_rxcui    = character(0),
        ingredient_rxcui = character(0),
        ingredient_name  = character(0),
        ingredient_tty   = character(0)
      ))
    }

    rel <- tryCatch(
      rx_get_json(
        paste0("/rxcui/", id, "/related"),
        query = list(tty = tty_query)
      ),
      error = function(e) NULL
    )

    groups <- rel$relatedGroup$conceptGroup
    if (is.null(groups)) {
      return(tibble::tibble(
        related_rxcui    = character(0),
        ingredient_rxcui = character(0),
        ingredient_name  = character(0),
        ingredient_tty   = character(0)
      ))
    }

    concepts <- purrr::map(groups, "conceptProperties") |>
      purrr::compact() |>
      unlist(recursive = FALSE)

    if (!length(concepts)) {
      return(tibble::tibble(
        related_rxcui    = character(0),
        ingredient_rxcui = character(0),
        ingredient_name  = character(0),
        ingredient_tty   = character(0)
      ))
    }

    purrr::map_dfr(concepts, function(cp) {
      tibble::tibble(
        related_rxcui    = id,
        ingredient_rxcui = null2na(cp$rxcui),
        ingredient_name  = null2na(cp$name),
        ingredient_tty   = null2na(cp$tty)
      )
    })
  }) |>
    dplyr::distinct()
}
