#' Filter RxNorm product concepts by route of administration.
#'
#' Filters a product-level rxref table using route information from
#' [get_clinical_attributes()]. Useful when a drug or class includes
#' products across multiple routes, such as oral tablets, ophthalmic solutions,
#' injectables, patches, or topical formulations.
#'
#' @param products A tibble containing a `product_rxcui` column, such as the
#'   output of [products_for_ingredients()].
#' @param route Character vector of routes to keep. Common values include
#'   `"ORAL"`, `"INJECTION"`, `"OPHTHALMIC"`, `"OTIC"`, `"INHALATION"`,
#'   `"TOPICAL"`, `"RECTAL"`, `"VAGINAL"`, `"URETHRAL"`, and `"IMPLANT"`.
#' @param keep_route_info Logical. If `TRUE` (default), append route, dose form, and
#'   dose-form group summaries to the returned table.
#' @param include_historical Logical. If `TRUE`, use RxCUI history status
#'   metadata as a fallback for RxCUIs that do not return active clinical
#'   attributes. This is useful for obsolete, remapped, quantified, or otherwise
#'   non-current RxCUIs found in historical prescribing data. If `products`
#'   contains a `concept_status` column with non-`"active"` value, historical
#'   lookup is enabled automatically.
#' @param show_progress Logical. Show a progress bar in interactive sessions.
#'   Progress is shown only when at least 5 inputs are supplied.
#'
#' @return A tibble containing only rows whose `product_rxcui` has at least one
#'   matching route.
#'
#' @examples
#' \dontrun{
#' ing <- find_ingredients("metoprolol")
#'
#' prods <- products_for_ingredients(
#'   ing$rxcui,
#'   include_combos = TRUE
#' )
#'
#' filter_products_by_route(prods, route = "ORAL")
#'
#' # Historical/current products can also be route-filtered
#' prods_hist <- products_for_ingredients(
#'   ing$rxcui,
#'   include_combos = TRUE,
#'   concept_status = "active_and_historical"
#' )
#'
#' filter_products_by_route(prods_hist, route = "ORAL")
#' }
#'
#' @export
filter_products_by_route <- function(
    products,
    route = "ORAL",
    keep_route_info = TRUE,
    include_historical = FALSE,
    show_progress = interactive()
) {
  stopifnot(is.data.frame(products))
  stopifnot(is.character(route), length(route) >= 1)

  if (!"product_rxcui" %in% names(products)) {
    cli::cli_abort("{.arg products} must contain a {.field product_rxcui} column.")
  }

  route_keep <- toupper(route)

  # infer whether historical lookup needed if concept_status in `products` tibble.
  include_historical <- isTRUE(include_historical) ||
    (
      "concept_status" %in% names(products) &&
        any(!is.na(products$concept_status) & products$concept_status != "Active")
    )

  product_ids <- unique(stats::na.omit(products$product_rxcui))

  if (!length(product_ids)) {
    return(products[0, , drop = FALSE])
  }

  attrs <- get_clinical_attributes(
    product_ids,
    include_historical = include_historical,
    show_progress = show_progress
  )

  collapse_nonmissing <- function(x) {
    x <- sort(unique(stats::na.omit(x)))
    if (!length(x)) {
      return(NA_character_)
    }
    paste(x, collapse = "; ")
  }

  route_tbl <- attrs |>
    dplyr::group_by(product_rxcui = .data$rxcui) |>
    dplyr::summarise(
      route_match = any(.data$route %in% route_keep, na.rm = TRUE),
      route = collapse_nonmissing(.data$route),
      dose_form = collapse_nonmissing(.data$dose_form),
      dose_form_group = collapse_nonmissing(.data$dose_form_group),
      .groups = "drop"
    )

  products_for_join <- products |>
    dplyr::select(
      -dplyr::any_of(c(
        "route_match",
        "route",
        "routes",
        "dose_form",
        "dose_forms",
        "dose_form_group",
        "dose_form_groups"
      ))
    )

  out <- products_for_join |>
    dplyr::left_join(route_tbl, by = "product_rxcui") |>
    dplyr::filter(.data$route_match %in% TRUE)

  if (!isTRUE(keep_route_info)) {
    out <- out |>
      dplyr::select(
        -dplyr::any_of(c(
          "route_match",
          "route",
          "dose_form",
          "dose_form_group"
        ))
      )
  } else {
    out <- out |>
      dplyr::select(-dplyr::any_of("route_match"))
  }

  out
}
