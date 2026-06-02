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
#' For combination products, ingredient-related columns may contain multiple
#' semicolon-delimited values, such as `"amlodipine; valsartan"`.
#'
#' @param rxcui Character vector of RxCUIs
#' @param include_historical Logical. If `TRUE`, use RxCUI history status
#'   metadata as a fallback for RxCUIs that do not return active clinical
#'   attributes. This is useful for obsolete, remapped, quantified, or otherwise
#'   non-current RxCUIs found in historical prescribing data.
#' @param show_progress Logical. Show a progress bar in interactive sessions.
#'   Progress is shown only when at least 5 inputs are supplied.
#'
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
#'     \item{ingredient_rxcui}{Ingredient RxCUI. For combination products,
#'       multiple values are returned as semicolon-delimited strings.}
#'     \item{ingredient_name}{Ingredient name. For combination products,
#'       multiple values are returned as semicolon-delimited strings.}
#'     \item{ingredient_tty}{Ingredient term type (IN/PIN/MIN). For combination
#'       products, multiple values are returned as semicolon-delimited strings.}
#'     \item{is_multi_ingredient}{Logical; TRUE if >1 ingredient}
#'     \item{suppress}{Raw RxNorm suppress flag from properties}
#'     \item{status}{Simple status derived from suppress: "ACTIVE" vs "INACTIVE"}
#'   }
#' @examples
#' \dontrun{
#' get_clinical_attributes(c("861007","860975")) |>
#'   dplyr::select(rxcui, related_rxcui, name, strength, dose_form, route, tty) |>
#'   head()
#' }
#' @export
get_clinical_attributes <- function(rxcui,
                                    include_historical = FALSE,
                                    show_progress = interactive()) {
  stopifnot(is.character(rxcui))

  rxcui_ids <- unique(stats::na.omit(rxcui))

  # 1. Core clinical mapping
  core <- .rxref_progress_map_dfr(
    rxcui_ids,
    function(id) {
      # 1) Pull properties to know TTY and get name
      props <- rx_get_json(paste0("/rxcui/", id, "/properties"))

      p <- rx_pluck(props, "properties", .default = list())
      tty_self <- rx_scalar_chr(rx_pluck_chr(p, "tty"))
      name_self <- rx_scalar_chr(rx_pluck_chr(p, "name"))

      # If the concept is already SCD/SBD, parse it
      if (!is.na(tty_self) && tty_self %in% c("SCD", "SBD")) {
        pr <- .rxref_parse_strength_dose(name_self)

        return(tibble::tibble(
          rxcui         = id,
          related_rxcui = id,
          name          = name_self,
          tty           = tty_self,
          strength      = pr$strength,
          dose_form     = pr$dose_form
        ))
      }

      # Otherwise, fetch related SCD/SBD
      rel <- rx_get_json(
        paste0("/rxcui/", id, "/related"),
        query = list(tty = "SCD SBD")
      )

      groups <- rx_pluck_list(rel, "relatedGroup", "conceptGroup")

      if (!length(groups)) {
        return(tibble::tibble(
          rxcui         = id,
          related_rxcui = NA_character_,
          name          = NA_character_,
          tty           = NA_character_,
          strength      = NA_character_,
          dose_form     = NA_character_
        ))
      }

      # Flatten conceptProperties across groups robustly
      concepts <- purrr::map(groups, \(g) rx_pluck_list(g, "conceptProperties")) |>
        purrr::compact() |>
        unlist(recursive = FALSE)

      if (!length(concepts)) {
        return(tibble::tibble(
          rxcui         = id,
          related_rxcui = NA_character_,
          name          = NA_character_,
          tty           = NA_character_,
          strength      = NA_character_,
          dose_form     = NA_character_
        ))
      }

      purrr::map_dfr(concepts, function(cp) {
        nm <- rx_scalar_chr(rx_pluck_chr(cp, "name"))
        tty <- rx_scalar_chr(rx_pluck_chr(cp, "tty"))
        pr <- .rxref_parse_strength_dose(nm)

        tibble::tibble(
          rxcui         = id,
          related_rxcui = rx_scalar_chr(rx_pluck_chr(cp, "rxcui")),
          name          = nm,
          tty           = tty,
          strength      = pr$strength,
          dose_form     = pr$dose_form
        )
      })
    },
    name = "Getting clinical attributes",
    show_progress = show_progress
  )

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
    out <- core |>
      dplyr::mutate(
        dose_form_group = NA_character_,
        ingredient_count = NA_integer_,
        ingredient_rxcui = NA_character_,
        ingredient_name = NA_character_,
        ingredient_tty = NA_character_,
        is_multi_ingredient = NA,
        suppress = NA_character_,
        status = NA_character_,
        route = NA_character_
      )

    if (!isTRUE(include_historical)) {
      return(out)
    }

    hist <- .rxref_clinical_attributes_historical(
      unique(out$rxcui),
      show_progress = show_progress
    ) |>
      dplyr::filter(
        !is.na(.data$name) |
          !is.na(.data$tty) |
          !is.na(.data$dose_form) |
          !is.na(.data$dose_form_group) |
          !is.na(.data$ingredient_count)
      )

    return(dplyr::bind_rows(
      dplyr::filter(out, !(.data$rxcui %in% hist$rxcui)),
      hist
    ) |>
      dplyr::distinct()
    )
  }

  # 3. Dose form group (DFG) from RxNorm (TTY = DFG)
  dfg_tbl <- purrr::map_dfr(clinical_ids, function(id) {
    rel <- rx_try_optional_api(
      rx_get_json(
        paste0("/rxcui/", id, "/related"),
        query = list(tty = "DFG")
      ),
      fallback = NULL,
      context = paste0("Could not retrieve dose-form group for RxCUI ", id)
    )

    groups <- rx_pluck_list(rel, "relatedGroup", "conceptGroup")

    if (!length(groups)) {
      return(tibble::tibble(
        related_rxcui   = id,
        dose_form_group = NA_character_
      ))
    }
    concepts <- purrr::map(groups, \(g) rx_pluck_list(g, "conceptProperties")) |>
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
  ing_raw <- rx_try_optional_api(
    .rxref_get_ingredients_for_rxcui(
      clinical_ids,
      include_pin = TRUE,
      include_min = FALSE
    ),
    fallback = NULL,
    context = "Could not retrieve ingredient summaries for clinical RxCUIs"
  )

  if (!is.null(ing_raw) && nrow(ing_raw)) {
    ing_raw <- ing_raw |>
      dplyr::filter(
        !is.na(.data$ingredient_rxcui) | !is.na(.data$ingredient_name)
      )
  }

  if (is.null(ing_raw) || !nrow(ing_raw)) {
    ing_summary <- tibble::tibble(
      related_rxcui        = clinical_ids,
      ingredient_count     = NA_integer_,
      ingredient_rxcui     = NA_character_,
      ingredient_name      = NA_character_,
      ingredient_tty       = NA_character_,
      is_multi_ingredient  = NA
    )
  } else {
    # Prefer IN over PIN when both present for a given product
    ing_raw <- ing_raw |>
      dplyr::group_by(.data$related_rxcui) |>
      dplyr::filter(!(.data$ingredient_tty == "PIN" & any(.data$ingredient_tty == "IN"))) |>
      dplyr::ungroup()

    ing_raw2 <- ing_raw |>
      dplyr::mutate(
        ingredient_key = dplyr::coalesce(
          .data$ingredient_rxcui,
          stringr::str_to_lower(.data$ingredient_name)
        )
      ) |>
      dplyr::filter(!is.na(.data$ingredient_key), nzchar(.data$ingredient_key)) |>
      dplyr::distinct(
        .data$related_rxcui,
        .data$ingredient_key,
        .keep_all = TRUE
      )

    ing_summary <- ing_raw2 |>
      dplyr::group_by(.data$related_rxcui) |>
      dplyr::summarise(
        ingredient_count = dplyr::n(),
        ingredient_rxcui = .rxref_collapse_values(.data$ingredient_rxcui),
        ingredient_name  = .rxref_collapse_values(.data$ingredient_name),
        ingredient_tty   = .rxref_collapse_values(.data$ingredient_tty),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        is_multi_ingredient = .data$ingredient_count > 1L
      )
  }

  # 5. Status from suppress (via get_properties)
  props_clin <- rx_try_optional_api(
    get_properties(clinical_ids, show_progress = FALSE),
    fallback = tibble::tibble(
      rxcui = clinical_ids,
      suppress = NA_character_
    ),
    context = "Could not retrieve suppress/status metadata for clinical RxCUIs"
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
      is_multi_ingredient = dplyr::if_else(
        is.na(.data$ingredient_count),
        NA,
        .data$ingredient_count > 1L
      ),
      route = .rxref_route_from_dose_form(
        .data$dose_form,
        .data$dose_form_group
      )
    )

  if (!isTRUE(include_historical)) {
    return(out)
  }

  needs_hist <- out |>
    dplyr::filter(
      is.na(.data$related_rxcui) |
        is.na(.data$dose_form) |
        is.na(.data$dose_form_group) |
        is.na(.data$route) |
        is.na(.data$ingredient_count)
    ) |>
    dplyr::pull(.data$rxcui) |>
    unique()

  if (!length(needs_hist)) {
    return(out)
  }

  hist <- .rxref_clinical_attributes_historical(
    needs_hist,
    show_progress = show_progress
  ) |>
    dplyr::filter(
      !is.na(.data$name) |
        !is.na(.data$tty) |
        !is.na(.data$dose_form) |
        !is.na(.data$dose_form_group) |
        !is.na(.data$ingredient_count)
    )

  out_active_keep <- out |>
    dplyr::filter(!(.data$rxcui %in% hist$rxcui))

  dplyr::bind_rows(
    out_active_keep,
    hist
  ) |>
    dplyr::distinct()
}



# Internal: collapse multiple scalar values into a semicolon-delimited string
#' @keywords internal
#' @noRd
.rxref_collapse_values <- function(x, delim = "; ") {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  x <- as.character(x)
  x <- trimws(x)
  x <- x[!is.na(x) & nzchar(x)]
  x <- unique(x)

  if (!length(x)) {
    return(NA_character_)
  }

  paste(x, collapse = delim)
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

    rel <- rx_get_json(
      paste0("/rxcui/", id, "/related"),
      query = list(tty = tty_query)
    )

    groups <- rx_pluck_list(rel, "relatedGroup", "conceptGroup")
    if (!length(groups)) {
      return(tibble::tibble(
        related_rxcui    = character(0),
        ingredient_rxcui = character(0),
        ingredient_name  = character(0),
        ingredient_tty   = character(0)
      ))
    }

    concepts <- purrr::map(groups, \(g) rx_pluck_list(g, "conceptProperties")) |>
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
        ingredient_rxcui = rx_scalar_chr(rx_pluck_chr(cp, "rxcui")),
        ingredient_name  = rx_scalar_chr(rx_pluck_chr(cp, "name")),
        ingredient_tty   = rx_scalar_chr(rx_pluck_chr(cp, "tty"))
      )
    })
  }) |>
    dplyr::distinct()
}

#' Parse strength and dose form from an RxNorm name
#'
#' @keywords internal
#' @noRd
.rxref_parse_strength_dose <- function(nm) {
  if (is.na(nm) || !nzchar(nm)) {
    return(list(strength = NA_character_, dose_form = NA_character_))
  }

  rx <- paste0(
    "(",
    "\\d{1,4}(?:[\\d,]*)(?:\\.\\d+)?",
    "\\s*(?:MCG|MG|G|KG|ML|L|MEQ|MMOL|IU|UNITS|%)\\b",
    "\\s*(?:/\\s*",
    "(?:\\d{1,4}(?:[\\d,]*)(?:\\.\\d+)?)?",
    "\\s*(?:MCG|MG|G|KG|ML|L|MEQ|MMOL|IU|UNITS|%|HR)\\b",
    ")?",
    ")"
  )

  m <- gregexpr(rx, nm, ignore.case = TRUE, perl = TRUE)
  starts <- m[[1]]

  if (length(starts) == 1L && starts[1] == -1L) {
    return(list(strength = NA_character_, dose_form = NA_character_))
  }

  lens <- attr(m[[1]], "match.length")
  matches <- regmatches(nm, m)[[1]]
  matches <- trimws(matches)

  has_ratio <- grepl("(/|per|%)", matches, ignore.case = TRUE)

  if (any(has_ratio)) {
    idx <- tail(which(has_ratio), 1L)
    strength <- matches[idx]
  } else if (length(matches) > 1L) {
    strength <- paste(matches, collapse = " / ")
    idx <- length(matches)
  } else {
    idx <- 1L
    strength <- matches[idx]
  }

  pos_end <- starts[idx] + lens[idx] - 1L
  tail <- trimws(substr(nm, pos_end + 1L, nchar(nm)))
  tail <- sub("^[,;:.-]\\s*", "", tail)

  list(
    strength = strength,
    dose_form = if (nzchar(tail)) tail else NA_character_
  )
}

#' Infer route from dose form and dose form group
#'
#' @keywords internal
#' @noRd
.rxref_route_from_dose_form <- function(dose_form, dose_form_group) {
  dplyr::case_when(
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

    dose_form_group %in% c(
      "Injectable Product",
      "Intraperitoneal Product",
      "Intratracheal Product",
      "Irrigation Product"
    ) ~ "INJECTION",

    dose_form_group == "Ophthalmic Product" ~ "OPHTHALMIC",
    dose_form_group == "Otic Product" ~ "OTIC",
    dose_form_group %in% c("Nasal Product", "Inhalant Product") ~ "INHALATION",

    dose_form_group == "Rectal Product" ~ "RECTAL",
    dose_form_group == "Vaginal Product" ~ "VAGINAL",
    dose_form_group == "Urethral Product" ~ "URETHRAL",

    dose_form_group %in% c(
      "Topical Product",
      "Transdermal Product",
      "Mucosal Product",
      "Medicated Pad or Tape",
      "Shampoo Product",
      "Soap Product"
    ) ~ "TOPICAL",

    dose_form_group == "Drug Implant Product" ~ "IMPLANT",

    stringr::str_detect(dose_form, "(?i)ophthalmic") ~ "OPHTHALMIC",
    stringr::str_detect(dose_form, "(?i)otic") ~ "OTIC",
    stringr::str_detect(dose_form, "(?i)nasal|inhal") ~ "INHALATION",
    stringr::str_detect(dose_form, "(?i)rectal|enema") ~ "RECTAL",
    stringr::str_detect(dose_form, "(?i)vaginal|douche") ~ "VAGINAL",
    stringr::str_detect(dose_form, "(?i)urethral") ~ "URETHRAL",
    stringr::str_detect(dose_form, "(?i)implant") ~ "IMPLANT",
    stringr::str_detect(dose_form, "(?i)transdermal|topical|gel|cream|ointment|lotion|patch") ~ "TOPICAL",
    stringr::str_detect(dose_form, "(?i)oral") ~ "ORAL",

    TRUE ~ NA_character_
  )
}

#' Get clinical attributes from RxCUI history status
#'
#' @keywords internal
#' @noRd
.rxref_clinical_attributes_historical <- function(rxcui,
                                                  show_progress = interactive()) {
  stopifnot(is.character(rxcui))

  rxcui <- unique(stats::na.omit(rxcui))

  empty_hist_ings <- function() {
    tibble::tibble(
      ingredient_rxcui = character(),
      ingredient_name = character(),
      ingredient_tty = character()
    )
  }

  empty_dose_forms <- function() {
    tibble::tibble(
      dose_form_rxcui = character(),
      dose_form = character()
    )
  }

  empty_dfg <- function() {
    tibble::tibble(
      dose_form_group_rxcui = character(),
      dose_form_group = character()
    )
  }

  .rxref_progress_map_dfr(
    rxcui,
    function(id) {
      hs <- rx_try_optional_api(
        rx_get_json(paste0("/rxcui/", id, "/historystatus")),
        fallback = NULL,
        context = paste0("Could not retrieve historical status for RxCUI ", id)
      )

      hist <- rx_pluck_list(hs, "rxcuiStatusHistory")

      if (is.null(hist) || !length(hist)) {
        return(tibble::tibble(
          rxcui = id,
          related_rxcui = id,
          name = NA_character_,
          tty = NA_character_,
          strength = NA_character_,
          dose_form = NA_character_,
          route = NA_character_,
          dose_form_group = NA_character_,
          is_brand = NA,
          is_generic = NA,
          ingredient_count = NA_integer_,
          ingredient_rxcui = NA_character_,
          ingredient_name = NA_character_,
          ingredient_tty = NA_character_,
          is_multi_ingredient = NA,
          suppress = NA_character_,
          status = NA_character_,
          history_status = NA_character_,
          history_is_current = NA_character_
        ))
      }

      meta <- hist$metaData
      attrs <- hist$attributes
      defs <- hist$definitionalFeatures
      derived <- hist$derivedConcepts

      nm <- rx_scalar_chr(attrs$name)
      tty <- rx_scalar_chr(attrs$tty)
      pr <- .rxref_parse_strength_dose(nm)

      dose_forms <- as_rx_records(
        defs$doseFormConcept,
        id_field = "doseFormRxcui"
      )

      dose_forms_tbl <- if (!length(dose_forms)) {
        empty_dose_forms()
      } else {
        purrr::map_dfr(dose_forms, function(z) {
          tibble::tibble(
            dose_form_rxcui = rx_scalar_chr(z$doseFormRxcui),
            dose_form = rx_scalar_chr(z$doseFormName)
          )
        })
      }

      dfgs <- as_rx_records(
        defs$doseFormGroupConcept,
        id_field = "doseFormGroupRxcui"
      )

      dfg_tbl <- if (!length(dfgs)) {
        empty_dfg()
      } else {
        purrr::map_dfr(dfgs, function(z) {
          tibble::tibble(
            dose_form_group_rxcui = rx_scalar_chr(z$doseFormGroupRxcui),
            dose_form_group = rx_scalar_chr(z$doseFormGroupName)
          )
        })
      }

      ing_concepts <- as_rx_records(
        derived$ingredientConcept,
        id_field = "ingredientRxcui"
      )

      ingredients_tbl <- if (!length(ing_concepts)) {
        empty_hist_ings()
      } else {
        purrr::map_dfr(ing_concepts, function(z) {
          tibble::tibble(
            ingredient_rxcui = rx_scalar_chr(z$ingredientRxcui),
            ingredient_name = rx_scalar_chr(z$ingredientName),
            ingredient_tty = "IN"
          )
        })
      }

      # Fallback: ingredientAndStrength often has useful ingredient fields even
      # when derived ingredientConcept is absent.
      ing_strength <- as_rx_records(
        defs$ingredientAndStrength,
        id_field = "baseRxcui"
      )

      strength_ingredients_tbl <- if (!length(ing_strength)) {
        empty_hist_ings()
      } else {
        purrr::map_dfr(ing_strength, function(z) {
          tibble::tibble(
            ingredient_rxcui = rx_scalar_chr(z$baseRxcui),
            ingredient_name = rx_scalar_chr(z$baseName),
            ingredient_tty = "IN"
          )
        })
      }

      ingredients_tbl <- dplyr::bind_rows(
        ingredients_tbl,
        strength_ingredients_tbl
      ) |>
        dplyr::filter(
          !is.na(.data$ingredient_rxcui) | !is.na(.data$ingredient_name)
        ) |>
        dplyr::mutate(
          ingredient_key = dplyr::coalesce(
            .data$ingredient_rxcui,
            stringr::str_to_lower(.data$ingredient_name)
          )
        ) |>
        dplyr::filter(!is.na(.data$ingredient_key), nzchar(.data$ingredient_key)) |>
        dplyr::group_by(.data$ingredient_key) |>
        dplyr::summarise(
          ingredient_rxcui = dplyr::first(stats::na.omit(.data$ingredient_rxcui), default = NA_character_),
          ingredient_name  = dplyr::first(stats::na.omit(.data$ingredient_name), default = NA_character_),
          ingredient_tty   = dplyr::first(stats::na.omit(.data$ingredient_tty), default = NA_character_),
          .groups = "drop"
        )

      ingredient_count <- nrow(ingredients_tbl)

      if (ingredient_count == 0L) {
        ingredient_count <- NA_integer_
      }

      dose_form_hist <- .rxref_collapse_values(dose_forms_tbl$dose_form)
      dose_form_group_hist <- .rxref_collapse_values(dfg_tbl$dose_form_group)

      # Prefer parsed dose form from the name if available; otherwise use the
      # structured dose form concept name from historystatus.
      dose_form_final <- dplyr::coalesce(pr$dose_form, dose_form_hist)

      tibble::tibble(
        rxcui = id,
        related_rxcui = id,
        name = nm,
        tty = tty,
        strength = pr$strength,
        dose_form = dose_form_final,
        route = .rxref_route_from_dose_form(
          dose_form_final,
          dose_form_group_hist
        ),
        dose_form_group = dose_form_group_hist,
        is_brand = tty %in% c("SBD", "BPCK"),
        is_generic = tty %in% c("SCD", "GPCK"),
        ingredient_count = ingredient_count,
        ingredient_rxcui = .rxref_collapse_values(ingredients_tbl$ingredient_rxcui),
        ingredient_name = .rxref_collapse_values(ingredients_tbl$ingredient_name),
        ingredient_tty = .rxref_collapse_values(ingredients_tbl$ingredient_tty),
        is_multi_ingredient = dplyr::if_else(
          is.na(ingredient_count),
          NA,
          ingredient_count > 1L
        ),
        suppress = NA_character_,
        status = rx_scalar_chr(meta$status),
        history_status = rx_scalar_chr(meta$status),
        history_is_current = rx_scalar_chr(meta$isCurrent)
      )
    },
    name = "Getting historical clinical attributes",
    show_progress = show_progress
  ) |>
    dplyr::distinct()
}
