# File: R/search.R

# Some helpers:
# Default product-ish TTYs (good for mapping to NDC)
.rxref_default_ttys <- c("SCD", "SBD", "GPCK", "BPCK")

# Extended “structure/group” TTYs you may want for richer CUIs (though they rarely map to NDC)
# - Default product TTYs: SCD, SBD, GPCK, BPCK
# - Components: SCDC, SBDC
# - Drug forms / groups: SCDF, SBDF, SCDFP, SBDFP, SCDG, SCDGP
# - Names / ingredients: BN (brand name), MIN (multi-ingredient), IN (ingredients)
.rxref_extended_ttys <- c("SCD", "SBD", "GPCK", "BPCK","SCDC","SBDC","SCDF","SBDF","SCDFP","SBDFP","SCDG","SCDGP","BN","MIN","IN")


#' Resolve a free-text drug name to ingredient CUIs (IN/PIN)
#'
#' Uses RxNav approximateTerm, then fetches properties for each candidate and
#' filters to ingredient-type concepts (TTY `IN` and, optionally, `PIN`).
#'
#' @param term Character vector (free text).
#' @param max_entries Integer. Max approximate-term candidates per input (default 10).
#' @param include_pin Logical. Include precise-ingredient (`PIN`) CUIs (default TRUE).
#'
#' @return A tibble with columns: `input`, `rxcui`, `name`, `tty`, `score`.
#'
#' @examples
#' if (identical(Sys.getenv("RXREF_ONLINE"), "1")) {
#'   find_ingredients("metformin")
#' }
#' @export
find_ingredients <- function(term, max_entries = 10, include_pin = TRUE) {
  stopifnot(is.character(term))
  purrr::map_dfr(term, function(t) {
    approx <- rx_get_json("/approximateTerm", query = list(term = t, maxEntries = max_entries))
    cand <- approx$approximateGroup$candidate
    if (is.null(cand) || !length(cand)) {
      return(tibble::tibble(
        input = t, rxcui = NA_character_, name = NA_character_,
        tty = NA_character_, score = NA_real_
      ))
    }
    rows <- purrr::map_dfr(cand, function(c) {
      id <- null2na(c$rxcui)
      pr <- if (!is.na(id)) rx_get_json(paste0("/rxcui/", id, "/properties")) else NULL
      tibble::tibble(
        input = t,
        rxcui = id,
        name  = null2na(pr$properties$name),
        tty   = null2na(pr$properties$tty),
        score = suppressWarnings(as.numeric(null2na(c$score)))
      )
    })
    keep_ttys <- if (include_pin) c("IN","PIN") else "IN"
    rows |>
      dplyr::filter(.data$tty %in% keep_ttys) |>
      dplyr::arrange(dplyr::desc(.data$score))
  }) |>
    dplyr::distinct()
}


#' Expand ingredient CUIs to product CUIs that truly contain the ingredient
#'
#' Tries multiple RxNav endpoints and verifies candidates truly contain the
#' queried ingredient (or its PIN children). Handles cases where TTY appears
#' only at the group level. Unions candidates from all sources.
#'
#' @param ingredient_rxcui Character vector of ingredient CUIs (TTY `IN` or `PIN`).
#' @param ttys Character vector of TTYs to include (default: product-facing
#'   `c("SCD","SBD","GPCK","BPCK")`). Pass a larger set if you want groups,
#'   components, names, etc. (e.g., `c(.rxref_default_ttys, .rxref_extended_ttys)`).
#' @param include_combos Logical; if `FALSE`, keep only single-ingredient
#'   products (counting distinct `IN`; if none present, falls back to distinct `PIN`).
#' @return Tibble with columns: `ingredient_rxcui`, `product_rxcui`, `name`, `tty`, `n_ingredients`.
#' @export
products_for_ingredients <- function(ingredient_rxcui,
                                     ttys = .rxref_default_ttys,
                                     include_combos = TRUE) {
  stopifnot(is.character(ingredient_rxcui), is.character(ttys), length(ttys) >= 1)
  tty_vec <- unique(ttys)

  # helper: safe scalarization
  nz1 <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)

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
    pr <- tryCatch(rx_get_json(paste0("/rxcui/", ing, "/properties")), error = function(e) NULL)
    ing_name <- tolower(nz1(pr$properties$name))
    base_token <- sub("\\s+.*$", "", ing_name)
    base_pat   <- paste0("\\b", gsub("([\\W_])", "\\\\\\1", base_token), "\\b")

    pins <- character(0)
    tty_self <- nz1(pr$properties$tty)
    if (!identical(tty_self, "PIN")) {
      rel <- tryCatch(
        rx_get_json(paste0("/rxcui/", ing, "/related"),
                    query = list(tty = "PIN", rela = "has_precise_ingredient")),
        error = function(e) NULL
      )
      pins_tbl <- collect_concepts(rel$relatedGroup$conceptGroup, "PIN")
      pins <- pins_tbl$product_rxcui
    }
    list(cui_ok = unique(c(ing, pins)), name_pat = base_pat)
  }

  # fetchers (each returns tibble: product_rxcui, name, tty)
  fetch_via_rela <- function(ing) {
    rel <- tryCatch(
      rx_get_json(paste0("/rxcui/", ing, "/related"),
                  query = list(tty = tty_vec, rela = "ingredient_of")),
      error = function(e) NULL
    )
    collect_concepts(rel$relatedGroup$conceptGroup, tty_vec)
  }
  fetch_via_related <- function(ing) {
    rel <- tryCatch(
      rx_get_json(paste0("/rxcui/", ing, "/related"),
                  query = list(tty = tty_vec)),
      error = function(e) NULL
    )
    collect_concepts(rel$relatedGroup$conceptGroup, tty_vec)
  }
  fetch_via_allrelated <- function(ing) {
    rel <- tryCatch(rx_get_json(paste0("/rxcui/", ing, "/allrelated")), error = function(e) NULL)
    collect_concepts(rel$allRelatedGroup$conceptGroup, tty_vec)
  }
  fetch_via_drugs_name <- function(ing) {
    props <- tryCatch(rx_get_json(paste0("/rxcui/", ing, "/properties")), error = function(e) NULL)
    nm <- nz1(props$properties$name)
    if (is.na(nm) || !nzchar(nm)) {
      return(tibble::tibble(product_rxcui = character(), name = character(), tty = character()))
    }
    dg <- tryCatch(rx_get_json("/drugs", query = list(name = nm)), error = function(e) NULL)
    collect_concepts(dg$drugGroup$conceptGroup, tty_vec)
  }

  # verification: ensure product lists the ingredient (IN/PIN) by CUI or name
  verify_contains <- function(prod_rxcui, cui_ok, name_pat) {
    rel_ing <- tryCatch(
      rx_get_json(paste0("/rxcui/", prod_rxcui, "/related"),
                  query = list(tty = c("IN","PIN"))),
      error = function(e) NULL
    )
    gs <- rel_ing$relatedGroup$conceptGroup
    if (is.null(gs) || !length(gs)) {
      return(tibble::tibble(product_rxcui = prod_rxcui, n_ingredients = 0L, contains = FALSE))
    }
    ings <- purrr::map_dfr(gs, function(grp) {
      cps <- grp$conceptProperties
      if (is.null(cps)) return(tibble::tibble())
      purrr::map_dfr(cps, function(z) {
        tibble::tibble(
          rxcui = as.character(z$rxcui),
          tty   = nz1(z$tty),
          name  = tolower(nz1(z$name))
        )
      })
    }) |>
      dplyr::filter(.data$tty %in% c("IN","PIN")) |>
      dplyr::distinct()

    n_in  <- ings |> dplyr::filter(.data$tty == "IN")  |> dplyr::distinct(.data$rxcui) |> nrow()
    n_pin <- ings |> dplyr::filter(.data$tty == "PIN") |> dplyr::distinct(.data$rxcui) |> nrow()
    n_total <- if (n_in > 0L) n_in else n_pin

    contains <- any(ings$rxcui %in% cui_ok) || any(grepl(name_pat, ings$name, perl = TRUE))

    tibble::tibble(product_rxcui = prod_rxcui, n_ingredients = as.integer(n_total), contains = contains)
  }

  purrr::map_dfr(ingredient_rxcui, function(ing) {
    # UNION candidates from all sources
    cand_rela    <- fetch_via_rela(ing)
    cand_related <- fetch_via_related(ing)
    cand_allrel  <- fetch_via_allrelated(ing)
    cand_drugs   <- fetch_via_drugs_name(ing)

    prods <- dplyr::bind_rows(cand_rela, cand_related, cand_allrel, cand_drugs) |>
      dplyr::distinct()

    if (!nrow(prods)) {
      return(tibble::tibble(
        ingredient_rxcui = character(),
        product_rxcui    = character(),
        name             = character(),
        tty              = character(),
        n_ingredients    = integer()
      ))
    }

    acc <- acceptance_for_ing(ing)
    chk <- purrr::map_dfr(
      prods$product_rxcui,
      verify_contains,
      cui_ok = acc$cui_ok,
      name_pat = acc$name_pat
    )

    out <- prods |>
      dplyr::left_join(chk, by = "product_rxcui") |>
      dplyr::filter(.data$contains %in% TRUE) |>
      dplyr::mutate(ingredient_rxcui = ing) |>
      dplyr::select(.data$ingredient_rxcui, .data$product_rxcui, .data$name, .data$tty, .data$n_ingredients)

    if (!isTRUE(include_combos)) {
      out <- dplyr::filter(out, .data$n_ingredients <= 1L)
    }
    out
  }) |>
    dplyr::distinct()
}


#' Search free-text drug name and return product CUIs and/or NDCs
#'
#' High-level convenience: free text -> ingredient(s) (IN/PIN) -> verified product
#' CUIs, and optionally expand to NDCs with status filtering.
#'
#' @param term Character vector; free-text drug names.
#' @param return One of `c("rxcui","ndc","both")`.
#' @param ndc_status Optional character vector to filter NDCs. Options are "ACTIVE",
#'  "OBSOLETE", "UNSPECIFIED" (the API may also return no value, which will appear as NA).
#' @param ttys Character vector of TTYs to include in product search.
#'   Defaults to `.rxref_default_ttys`. Other prespecified option is `.rxref_extended_ttys`
#'   or a character vector of explicit TTYs. Run `tty_catalogue()` to review options.
#' @param ... Passed to `products_for_ingredients()` (e.g., include_combos = FALSE)
#' @return If `return="rxcui"`: tibble of products.
#'   If `"ndc"`: tibble of NDCs with `ingredient_rxcui`, `product_rxcui`, `ndc11`, `ndc_status`.
#'   If `"both"`: list(products=…, ndcs=…).
#' @export
search_drug <- function(term,
                        return = c("rxcui","ndc","both"),
                        ndc_status = NULL,
                        ttys = .rxref_default_ttys,
                        ...) {
  return <- match.arg(return)

  ings <- find_ingredients(term)
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

  prods <- products_for_ingredients(ing_ids, ttys = ttys, ...)
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
  ndcs <- purrr::map_dfr(prod_ids, function(p) {
    map_rxcui_to_ndc(p, status = ndc_status)
  }) |>
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


