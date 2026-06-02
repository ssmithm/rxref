#' Resolve a free-text drug name to ingredient CUIs (IN/PIN)
#'
#' Uses RxNav approximateTerm, then fetches properties for each candidate and
#' filters to ingredient-type concepts (TTY `IN` and, optionally, `PIN`).
#'
#' @param term Character vector (free text).
#' @param max_entries Integer. Max approximate-term candidates per input (default 10).
#' @param include_pin Logical. Include precise-ingredient (`PIN`) CUIs (default TRUE).
#' @param show_progress Logical. Show a progress bar in interactive sessions.
#'   Progress is shown only when at least 5 inputs are supplied.
#'
#' @return A tibble with columns: `input`, `rxcui`, `name`, `tty`, `score`.
#'
#' @examples
#' if (identical(Sys.getenv("RXREF_ONLINE"), "1")) {
#'   find_ingredients("metformin")
#' }
#' @export
find_ingredients <- function(
    term,
    max_entries = 10,
    include_pin = TRUE,
    show_progress = interactive()
) {
  stopifnot(is.character(term))

  .rxref_progress_map_dfr(
    term,
    function(t) {
      approx <- rx_get_json(
        "/approximateTerm",
        query = list(term = t, maxEntries = max_entries)
      )

      cand <- rx_pluck_list(approx, "approximateGroup", "candidate")

      if (!length(cand)) {
        return(tibble::tibble(
          input = character(),
          rxcui = character(),
          name = character(),
          tty = character(),
          score = numeric()
        ))
      }

      rows <- purrr::map_dfr(cand, function(c) {
        id <- rx_scalar_chr(rx_pluck_chr(c, "rxcui"))

        pr <- if (!is.na(id)) {
          rx_get_json(paste0("/rxcui/", id, "/properties"))
        } else {
          NULL
        }

        tibble::tibble(
          input = t,
          rxcui = id,
          name = rx_scalar_chr(rx_pluck_chr(pr, "properties", "name")),
          tty = rx_scalar_chr(rx_pluck_chr(pr, "properties", "tty")),
          score = suppressWarnings(as.numeric(rx_scalar_chr(rx_pluck_chr(c, "score"))))
        )
      })

      keep_ttys <- if (include_pin) c("IN", "PIN") else "IN"

      rows |>
        dplyr::filter(.data$tty %in% keep_ttys) |>
        dplyr::arrange(dplyr::desc(.data$score))
    },
    name = "Finding ingredients",
    show_progress = show_progress
  ) |>
    dplyr::distinct()
}


#' Expand ingredient CUIs to product CUIs that truly contain the ingredient
#'
#' Tries multiple RxNav endpoints and verifies candidates truly contain the
#' queried ingredient or one of its acceptable related ingredient concepts.
#' The function unions candidates from multiple sources, verifies ingredient
#' containment, and reports the number of ingredients represented by each
#' product concept.
#'
#' @param ingredient_rxcui Character vector of ingredient CUIs (TTY `IN` or `PIN`).
#' @param ttys Character vector of TTYs to include. Defaults to product-facing
#'   TTYs returned by [product_ttys()]. Pass a larger set if you want groups,
#'   components, branded concepts, or other product-related concepts, for example
#'   `product_ttys("extended_product")`.
#' @param route Optional character vector of routes to retain. If `NULL`, no
#'   route filtering is performed. Route filtering uses [get_clinical_attributes()].
#'   Route filtering is intended for product-level TTYs and may not filter well
#'   on broader group or package TTYs.
#' @param include_combos Logical. If `FALSE`, keep only single-ingredient
#'   products, where ingredient count is based on distinct `IN` concepts when
#'   available and otherwise falls back to distinct `PIN` concepts.
#' @param concept_status Character. Which RxNorm concept universe to search.
#'   `"active"` uses active-scope RxNav relationship endpoints and is the
#'   default. `"active_and_historical"` also searches historical RxNorm concepts
#'   using all-status concept retrieval and RxCUI history status metadata.
#'   Historical searching is slower and is intended for mapping older
#'   prescribing or dispensing data.
#' @param historical_status Character vector of historical RxNorm statuses to
#'   include when `concept_status = "active_and_historical"`. Defaults to
#'   `c("Obsolete", "Remapped", "Quantified", "NotCurrent")`. These values use
#'   RxNorm status definitions:
#'   \describe{
#'     \item{`"Obsolete"`}{The concept is obsolete in the current RxNorm data set,
#'       and RxNorm has not designated an active concept as equivalent.}
#'     \item{`"Remapped"`}{The concept was active or obsolete at one time, is no
#'       longer in the current data set, and has been remapped to one or more
#'       active or obsolete concepts.}
#'     \item{`"Quantified"`}{The concept has been designated as non-dispensable
#'       because it lacks a quantity factor; related concepts with quantity
#'       factors may be available.}
#'     \item{`"NotCurrent"`}{The concept either exists in the current data set
#'       without RxNorm vocabulary terms, or existed in a previous monthly
#'       release but has since been removed and not remapped.}
#'   }
#'   See the RxNorm API documentation for concept status values:
#'   \url{https://lhncbc.nlm.nih.gov/RxNav/APIs/api-RxNorm.getAllConceptsByStatus.html}.
#' @param show_progress Logical. Show progress bars for long-running API
#'   retrieval, product matching, and optional route filtering steps. Defaults
#'   to `interactive()`.
#'
#' @return A tibble with one row per matched ingredient/product concept pair.
#'   For `concept_status = "active"`, columns include `ingredient_rxcui`,
#'   `product_rxcui`, `name`, `tty`, and `n_ingredients`. When
#'   `concept_status = "active_and_historical"`, additional columns include
#'   `concept_status`, `active_start_date`, `active_end_date`,
#'   `release_start_date`, and `release_end_date`.
#'
#' @export
products_for_ingredients <- function(ingredient_rxcui,
                                     ttys = .rxref_default_ttys,
                                     route = NULL,
                                     include_combos = TRUE,
                                     concept_status = c("active", "active_and_historical"),
                                     historical_status = c("Obsolete", "Remapped", "Quantified", "NotCurrent"),
                                     show_progress = interactive()) {

  concept_status <- match.arg(concept_status)

  include_historical <- identical(concept_status, "active_and_historical")

  stopifnot(
    is.character(ingredient_rxcui),
    is.character(ttys),
    length(ttys) >= 1,
    is.character(historical_status)
  )

  tty_vec <- unique(ttys)
  historical_status <- unique(historical_status)

  ingredient_ids <- unique(stats::na.omit(ingredient_rxcui))

  # helper: safe scalarization
  nz1 <- function(x) rx_scalar_chr(x)

  # helper: collect concepts, using group tty when concept tty is missing
  collect_concepts <- function(groups, allowed_ttys) {
    if (is.null(groups) || !length(groups)) {
      return(tibble::tibble(
        product_rxcui = character(),
        name = character(),
        tty  = character()
      ))
    }

    rows <- purrr::map_dfr(groups, function(grp) {
      grp_tty <- nz1(grp$tty)
      cps <- grp$conceptProperties
      if (is.null(cps) || !length(cps)) {
        return(tibble::tibble(
          product_rxcui = character(),
          name = character(),
          tty  = character()
        ))
      }
      purrr::map_dfr(cps, function(cp) {
        tty_here <- nz1(cp$tty)
        tibble::tibble(
          product_rxcui = nz1(cp$rxcui),
          name          = nz1(cp$name),
          tty           = if (!is.na(tty_here) && nzchar(tty_here)) tty_here else grp_tty
        )
      })
    })
    if (!nrow(rows)) {
      return(tibble::tibble(
        product_rxcui = character(),
        name = character(),
        tty  = character()
      ))
    }
    rows |>
      dplyr::filter(
        !is.na(.data$product_rxcui),
        !is.na(.data$tty),
        .data$tty %in% allowed_ttys
      ) |>
      dplyr::distinct()
  }

  # acceptance set: input IN + its PIN children; also a loose name pattern
  acceptance_for_ing <- function(ing) {
    pr <- rx_get_json(paste0("/rxcui/", ing, "/properties"))

    ing_name <- tolower(rx_scalar_chr(rx_pluck_chr(pr, "properties", "name")))

    if (is.na(ing_name) || !nzchar(ing_name)) {
      base_pat <- "a^"
    } else {
      base_token <- sub("\\s+.*$", "", ing_name)
      base_pat <- paste0("\\b", gsub("([\\W_])", "\\\\\\1", base_token), "\\b")
    }

    pins <- character(0)
    tty_self <- rx_scalar_chr(rx_pluck_chr(pr, "properties", "tty"))
    if (!identical(tty_self, "PIN")) {
      rel <- rx_try_optional_api(
        rx_get_json(
          paste0("/rxcui/", ing, "/related"),
          query = list(tty = "PIN", rela = "has_precise_ingredient")
        ),
        fallback = NULL,
        context = paste0("Could not retrieve precise ingredients for RxCUI ", ing)
      )

      pins_tbl <- collect_concepts(
        rx_pluck_list(rel, "relatedGroup", "conceptGroup"),
        "PIN"
      )

      pins <- pins_tbl$product_rxcui
    }
    list(cui_ok = unique(c(ing, pins)), name_pat = base_pat)
  }

  # fetchers that each return a tibble: product_rxcui, name, tty
  fetch_via_rela <- function(ing) {
    rel <- rx_try_optional_api(
      rx_get_json(
        paste0("/rxcui/", ing, "/related"),
        query = list(tty = tty_vec, rela = "ingredient_of")
      ),
      fallback = NULL,
      context = paste0("Could not retrieve ingredient_of products for RxCUI ", ing)
    )

    collect_concepts(
      rx_pluck_list(rel, "relatedGroup", "conceptGroup"),
      tty_vec
    )
  }

  fetch_via_related <- function(ing) {
    rel <- rx_try_optional_api(
      rx_get_json(
        paste0("/rxcui/", ing, "/related"),
        query = list(tty = tty_vec)
      ),
      fallback = NULL,
      context = paste0("Could not retrieve related products for RxCUI ", ing)
    )

    collect_concepts(
      rx_pluck_list(rel, "relatedGroup", "conceptGroup"),
      tty_vec
    )
  }

  fetch_via_allrelated <- function(ing) {
    rel <- rx_try_optional_api(
      rx_get_json(paste0("/rxcui/", ing, "/allrelated")),
      fallback = NULL,
      context = paste0("Could not retrieve all related products for RxCUI ", ing)
    )

    collect_concepts(
      rx_pluck_list(rel, "allRelatedGroup", "conceptGroup"),
      tty_vec
    )
  }

  fetch_via_drugs_name <- function(ing) {
    props <- rx_get_json(paste0("/rxcui/", ing, "/properties"))

    nm <- rx_scalar_chr(rx_pluck_chr(props, "properties", "name"))

    if (is.na(nm) || !nzchar(nm)) {
      return(tibble::tibble(
        product_rxcui = character(),
        name = character(),
        tty = character()
      ))
    }

    dg <- rx_try_optional_api(
      rx_get_json("/drugs", query = list(name = nm)),
      fallback = NULL,
      context = paste0("Could not retrieve drug products by name for ", nm)
    )

    collect_concepts(
      rx_pluck_list(dg, "drugGroup", "conceptGroup"),
      tty_vec
    )
  }

  fetch_allstatus_products <- function() {
    .rxref_progress_map_dfr(
      historical_status,
      function(status) {
        res <- rx_try_optional_api(
          rx_get_json("/allstatus", query = list(status = status)),
          fallback = NULL,
          context = paste0("Could not retrieve all-status concepts with status ", status)
        )

        concepts <- rx_pluck_list(res, "minConceptGroup", "minConcept")
        concepts <- as_rx_records(concepts)

        if (!length(concepts)) {
          return(tibble::tibble(
            product_rxcui = character(),
            name = character(),
            tty = character(),
            concept_status = character()
          ))
        }

        purrr::map_dfr(concepts, function(cp) {
          tibble::tibble(
            product_rxcui = nz1(cp$rxcui),
            name = nz1(cp$name),
            tty = nz1(cp$tty),
            concept_status = status
          )
        })
      },
      name = "Fetching historical products from RxNorm",
      show_progress = show_progress,
      min_n = 1L
    ) |>
      dplyr::filter(
        !is.na(.data$product_rxcui),
        !is.na(.data$name),
        !is.na(.data$tty),
        .data$tty %in% tty_vec
      ) |>
      dplyr::distinct()
  }

  filter_allstatus_name <- function(allstatus_products, name_pat) {
    if (identical(name_pat, "a^") || !nrow(allstatus_products)) {
      return(tibble::tibble(
        product_rxcui = character(),
        name = character(),
        tty = character(),
        concept_status = character()
      ))
    }

    allstatus_products |>
      dplyr::filter(grepl(name_pat, tolower(.data$name), perl = TRUE)) |>
      dplyr::distinct()
  }

  # verify that product lists the ingredient (IN/PIN) by CUI or name
  verify_contains <- function(prod_rxcui, cui_ok, name_pat) {
    rel_ing <- rx_try_optional_api(
      rx_get_json(
        paste0("/rxcui/", prod_rxcui, "/related"),
        query = list(tty = c("IN", "PIN"))
      ),
      fallback = NULL,
      context = paste0("Could not verify ingredients for product RxCUI ", prod_rxcui)
    )

    gs <- rx_pluck_list(rel_ing, "relatedGroup", "conceptGroup")

    if (is.null(gs) || !length(gs)) {
      return(tibble::tibble(
        product_rxcui = prod_rxcui,
        n_ingredients = NA_integer_,
        contains = FALSE
      ))
    }

    ings <- purrr::map_dfr(gs, function(grp) {
      cps <- rx_pluck_list(grp, "conceptProperties")

      if (is.null(cps)) {
        return(tibble::tibble())
      }

      purrr::map_dfr(cps, function(z) {
        tibble::tibble(
          rxcui = nz1(z$rxcui),
          tty = nz1(z$tty),
          name = tolower(nz1(z$name))
        )
      })
    }) |>
      dplyr::filter(.data$tty %in% c("IN", "PIN")) |>
      dplyr::filter(!is.na(.data$rxcui) | !is.na(.data$name)) |>
      dplyr::mutate(
        ingredient_key = dplyr::coalesce(
          .data$rxcui,
          .data$name
        )
      ) |>
      dplyr::filter(!is.na(.data$ingredient_key), nzchar(.data$ingredient_key)) |>
      dplyr::distinct(.data$tty, .data$ingredient_key, .keep_all = TRUE)

    n_in <- ings |>
      dplyr::filter(.data$tty == "IN") |>
      dplyr::distinct(.data$ingredient_key) |>
      nrow()

    n_pin <- ings |>
      dplyr::filter(.data$tty == "PIN") |>
      dplyr::distinct(.data$ingredient_key) |>
      nrow()

    n_total <- if (n_in > 0L) n_in else n_pin

    if (n_total == 0L) {
      n_total <- NA_integer_
    }

    contains <- any(ings$rxcui %in% cui_ok, na.rm = TRUE) ||
      any(grepl(name_pat, ings$name, perl = TRUE), na.rm = TRUE)

    tibble::tibble(
      product_rxcui = prod_rxcui,
      n_ingredients = as.integer(n_total),
      contains = contains
    )
  }

  verify_contains_historical <- function(prod_rxcui, cui_ok, name_pat) {
    hs <- rx_try_optional_api(
      rx_get_json(paste0("/rxcui/", prod_rxcui, "/historystatus")),
      fallback = NULL,
      context = paste0("Could not verify historical ingredients for product RxCUI ", prod_rxcui)
    )

    x <- rx_pluck_list(hs, "rxcuiStatusHistory")

    if (is.null(x) || !length(x)) {
      return(tibble::tibble(
        product_rxcui = prod_rxcui,
        n_ingredients = NA_integer_,
        contains = FALSE,
        concept_status = NA_character_,
        active_start_date = NA_character_,
        active_end_date = NA_character_,
        release_start_date = NA_character_,
        release_end_date = NA_character_
      ))
    }

    meta <- x$metaData
    attrs <- x$attributes
    defs <- x$definitionalFeatures
    derived <- x$derivedConcepts

    empty_hist_ings <- function() {
      tibble::tibble(
        rxcui = character(),
        name = character()
      )
    }

    ing_strength <- as_rx_records(
      defs$ingredientAndStrength,
      id_field = "baseRxcui"
    )

    # Canonical fallback ingredients from ingredientAndStrength.
    # These may contribute to n_ingredients.
    strength_base_ings <- if (!length(ing_strength)) {
      empty_hist_ings()
    } else {
      purrr::map_dfr(ing_strength, function(z) {
        tibble::tibble(
          rxcui = nz1(z$baseRxcui),
          name = tolower(nz1(z$baseName))
        )
      })
    }

    # Broader ingredient-related concepts from ingredientAndStrength.
    # These are useful for confirming containment, but are not counted
    # as separate ingredients.
    strength_match_ings <- if (!length(ing_strength)) {
      empty_hist_ings()
    } else {
      purrr::map_dfr(ing_strength, function(z) {
        tibble::tibble(
          rxcui = c(
            nz1(z$baseRxcui),
            nz1(z$bossRxcui),
            nz1(z$activeIngredientRxcui),
            nz1(z$moietyRxcui)
          ),
          name = tolower(c(
            nz1(z$baseName),
            nz1(z$bossName),
            nz1(z$activeIngredientName),
            nz1(z$moietyName)
          ))
        )
      })
    }

    ing_concepts <- as_rx_records(
      derived$ingredientConcept,
      id_field = "ingredientRxcui"
    )

    # Preferred canonical ingredient source.
    derived_ings <- if (!length(ing_concepts)) {
      empty_hist_ings()
    } else {
      purrr::map_dfr(ing_concepts, function(z) {
        tibble::tibble(
          rxcui = nz1(z$ingredientRxcui),
          name = tolower(nz1(z$ingredientName))
        )
      })
    }

    # Count canonical ingredients only.
    # Prefer derived ingredientConcept when available; fall back to base
    # ingredientAndStrength concepts otherwise.
    count_ings <- if (nrow(derived_ings)) {
      derived_ings
    } else {
      strength_base_ings
    }

    count_ings <- count_ings |>
      dplyr::filter(!is.na(.data$rxcui) | !is.na(.data$name)) |>
      dplyr::mutate(
        ingredient_key = dplyr::coalesce(
          .data$rxcui,
          .data$name
        )
      ) |>
      dplyr::filter(!is.na(.data$ingredient_key), nzchar(.data$ingredient_key)) |>
      dplyr::distinct(.data$ingredient_key, .keep_all = TRUE)

    # Use the broader table only for containment verification.
    match_ings <- dplyr::bind_rows(
      derived_ings,
      strength_base_ings,
      strength_match_ings
    ) |>
      dplyr::filter(!is.na(.data$rxcui) | !is.na(.data$name)) |>
      dplyr::distinct()

    n_total <- nrow(count_ings)

    if (n_total == 0L) {
      n_total <- NA_integer_
    }

    contains <- any(match_ings$rxcui %in% cui_ok, na.rm = TRUE) ||
      any(grepl(name_pat, match_ings$name, perl = TRUE), na.rm = TRUE)

    tibble::tibble(
      product_rxcui = prod_rxcui,
      n_ingredients = as.integer(n_total),
      contains = contains,
      concept_status = nz1(meta$status),
      active_start_date = nz1(meta$activeStartDate),
      active_end_date = nz1(meta$activeEndDate),
      release_start_date = nz1(meta$releaseStartDate),
      release_end_date = nz1(meta$releaseEndDate)
    )
  }

  allstatus_products <- if (isTRUE(include_historical)) {
    fetch_allstatus_products()
  } else {
    tibble::tibble(
      product_rxcui = character(),
      name = character(),
      tty = character(),
      concept_status = character()
    )
  }

  out <- .rxref_progress_map_dfr(
    ingredient_ids,
    function(ing) {
      ## orig.
      acc <- acceptance_for_ing(ing)
      cand_rela <- fetch_via_rela(ing)
      cand_related <- fetch_via_related(ing)
      cand_allrel <- fetch_via_allrelated(ing)
      cand_drugs <- fetch_via_drugs_name(ing)

      hist_prods <- if (isTRUE(include_historical)) {
        filter_allstatus_name(allstatus_products, acc$name_pat)
      } else {
        tibble::tibble(
          product_rxcui = character(),
          name = character(),
          tty = character(),
          concept_status = character()
        )
      }

      active_prods <- dplyr::bind_rows(
        cand_rela,
        cand_related,
        cand_allrel,
        cand_drugs
      ) |>
        dplyr::distinct()

      prods <- dplyr::bind_rows(
        dplyr::mutate(active_prods, concept_status = "Active"),
        hist_prods
      ) |>
        dplyr::distinct()

      if (!nrow(prods)) {
        return(empty_products(
          include_history_cols = isTRUE(include_historical)
        ))
      }

      active_ids <- prods |>
        dplyr::filter(.data$concept_status == "Active") |>
        dplyr::pull(.data$product_rxcui) |>
        unique()

      historical_ids <- prods |>
        dplyr::filter(.data$concept_status != "Active") |>
        dplyr::pull(.data$product_rxcui) |>
        unique()

      chk_active <- purrr::map_dfr(
        active_ids,
        verify_contains,
        cui_ok = acc$cui_ok,
        name_pat = acc$name_pat
      ) |>
        dplyr::mutate(
          concept_status = "Active",
          active_start_date = NA_character_,
          active_end_date = NA_character_,
          release_start_date = NA_character_,
          release_end_date = NA_character_
        )

      chk_historical <- purrr::map_dfr(
        historical_ids,
        verify_contains_historical,
        cui_ok = acc$cui_ok,
        name_pat = acc$name_pat
      )

      chk <- dplyr::bind_rows(chk_active, chk_historical)

      out_ <- prods |>
        dplyr::select(-dplyr::any_of("concept_status")) |>
        dplyr::left_join(chk, by = "product_rxcui") |>
        dplyr::filter(.data$contains %in% TRUE) |>
        dplyr::mutate(ingredient_rxcui = ing)

      if (isTRUE(include_historical)) {
        out_ <- out_ |>
          dplyr::select(dplyr::all_of(c(
            "ingredient_rxcui",
            "product_rxcui",
            "name",
            "tty",
            "n_ingredients",
            "concept_status",
            "active_start_date",
            "active_end_date",
            "release_start_date",
            "release_end_date"
          )))
      } else {
        out_ <- out_ |>
          dplyr::select(dplyr::all_of(c(
            "ingredient_rxcui",
            "product_rxcui",
            "name",
            "tty",
            "n_ingredients"
          )))
      }

      if (!isTRUE(include_combos)) {
        out_ <- dplyr::filter(out_, .data$n_ingredients <= 1L)
      }

      out_
    },
    name = "Matching products to ingredients",
    show_progress = show_progress
  ) |>
    dplyr::distinct()

  if (!is.null(route)) {
    out <- filter_products_by_route(
      out,
      route = route,
      include_historical = isTRUE(include_historical),
      show_progress = show_progress
    )
  }

  out
}


#' Search free-text drug name and return product CUIs and/or NDCs
#'
#' High-level convenience: free text -> ingredient(s) (IN/PIN) -> verified product
#' CUIs, and optionally expand to NDCs with status filtering.
#'
#' @param term Character vector; free-text drug names.
#' @param return One of `c("rxcui","ndc","both")`. Note that `"both"` will return a list
#'  with both an rxcui tibble and an ndc tibble.
#' @param ndc_status Optional character vector to filter NDCs. Options are "ACTIVE",
#'  "OBSOLETE", "UNSPECIFIED" (the API may also return no value, which will appear as NA).
#' @param ttys Character vector of TTYs to include in product search.
#'   Defaults to `.rxref_default_ttys`. Other prespecified option is `.rxref_extended_ttys`
#'   or a character vector of explicit TTYs. Run `tty_catalogue()` to review options.
#' @param route Optional character vector of routes to retain before returning
#'   products or mapping to NDCs. If `NULL`, no route filtering is performed.
#'   Common values include `"ORAL"`, `"INJECTION"`, `"OPHTHALMIC"`,
#'   `"INHALATION"`, and `"TOPICAL"`.
#' @param show_progress Logical. Show a progress bar in interactive sessions.
#'   Progress is shown only when at least 5 inputs are supplied.
#' @param ... Passed to `products_for_ingredients()` (e.g., include_combos = FALSE)
#' @return If `return="rxcui"`: tibble of products.
#'   If `"ndc"`: tibble of NDCs with `ingredient_rxcui`, `product_rxcui`, `ndc11`, `ndc_status`.
#'   If `"both"`: list(products=…, ndcs=…).
#' @export
search_drug <- function(term,
                        return = c("rxcui","ndc","both"),
                        ndc_status = NULL,
                        ttys = .rxref_default_ttys,
                        route = NULL,
                        show_progress = interactive(),
                        ...) {
  return <- match.arg(return)

  ings <- find_ingredients(
    term,
    show_progress = show_progress
  )
  ing_ids <- unique(stats::na.omit(ings$rxcui))

  empty_products <- tibble::tibble(
    ingredient_rxcui = character(),
    product_rxcui    = character(),
    name             = character(),
    tty              = character(),
    n_ingredients    = integer()
  )
  empty_ndc <- tibble::tibble(
    ingredient_rxcui = character(),
    product_rxcui    = character(),
    ndc11            = character(),
    ndc_status       = character()
  )

  if (!length(ing_ids)) {
    if (return == "rxcui") return(empty_products)
    if (return == "ndc")   return(empty_ndc)
    return(list(products = empty_products, ndcs = empty_ndc))
  }

  prods <- products_for_ingredients(
    ing_ids,
    ttys = ttys,
    route = route,
    show_progress = show_progress,
    ...
  )
  if (return == "rxcui") return(prods)

  # Only product-ish TTYs cleanly map to NDCs
  ndc_map_ttys <- c("SCD","SBD","GPCK","BPCK")
  prods_for_ndc <- dplyr::filter(prods, .data$tty %in% ndc_map_ttys)

  prod_ids <- unique(stats::na.omit(prods_for_ndc$product_rxcui))
  if (!length(prod_ids)) {
    if (return == "ndc")   return(empty_ndc)
    return(list(products = prods, ndcs = empty_ndc))
  }

  # Map each product rxcui to NDCs; rename rxcui -> product_rxcui for join
  ndcs <- map_rxcui_to_ndc(
    prod_ids,
    status = ndc_status,
    show_progress = show_progress
  ) |>
    dplyr::filter(!is.na(.data$ndc11)) |>
    dplyr::mutate(product_rxcui = .data$rxcui) |>
    dplyr::select(.data$product_rxcui, .data$ndc11, dplyr::any_of("ndc_status")) |>
    dplyr::left_join(
      dplyr::select(prods_for_ndc, .data$product_rxcui, .data$ingredient_rxcui),
      by = "product_rxcui"
    ) |>
    dplyr::relocate(.data$ingredient_rxcui, .before = 1L) |>
    dplyr::distinct()

  if (return == "ndc") return(ndcs)
  list(products = prods, ndcs = ndcs)
}


#' Get ingredient concepts for RxCUIs
#'
#' Maps one or more RxCUIs to related ingredient concepts, returning ingredient
#' RxCUIs, names, and term types. This is useful when the input is already a
#' specific RxNorm product concept rather than a free-text drug name.
#'
#' @param rxcui Character vector of RxCUIs.
#' @param include_pin Logical. Include precise ingredient concepts (`PIN`).
#' @param include_min Logical. Include multiple ingredient concepts (`MIN`).
#' @param show_progress Logical. Show a progress bar in interactive sessions.
#'
#' @return A tibble with columns `rxcui`, `ingredient_rxcui`,
#'   `ingredient_name`, and `ingredient_tty`.
#'
#' @export
ingredients_for_rxcui <- function(
    rxcui,
    include_pin = TRUE,
    include_min = FALSE,
    show_progress = interactive()
) {
  stopifnot(is.character(rxcui))

  rxcui_ids <- unique(stats::na.omit(rxcui))

  .rxref_progress_map_dfr(
    rxcui_ids,
    function(x) {
      .rxref_get_ingredients_for_rxcui(
        x,
        include_pin = include_pin,
        include_min = include_min
      )
    },
    name = "Finding ingredients",
    show_progress = show_progress
  ) |>
    dplyr::rename(rxcui = "related_rxcui")
}

#' Normalize RxNav records to a list of records
#'
#' RxNav sometimes returns repeated elements as a list of lists, but when there
#' is only one result it may return a single named list. This helper normalizes
#' both cases to a list of records.
#'
#' @keywords internal
#' @noRd
as_rx_records <- function(x, id_field = "rxcui") {
  if (is.null(x) || !length(x)) {
    return(list())
  }

  if (is.data.frame(x)) {
    return(split(x, seq_len(nrow(x))))
  }

  if (is.list(x) && !is.null(x[[id_field]])) {
    return(list(x))
  }

  x
}


#' Empty products tibble
#'
#' @keywords internal
#' @noRd
empty_products <- function(include_history_cols = FALSE) {
  out <- tibble::tibble(
    ingredient_rxcui = character(),
    product_rxcui = character(),
    name = character(),
    tty = character(),
    n_ingredients = integer()
  )

  if (isTRUE(include_history_cols)) {
    out <- dplyr::mutate(
      out,
      concept_status = character(),
      active_start_date = character(),
      active_end_date = character(),
      release_start_date = character(),
      release_end_date = character()
    )
  }

  out
}
