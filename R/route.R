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
#'
#' @return A tibble containing only rows whose `product_rxcui` has at least one
#'   matching route.
#'
#' @examples
#' \donttest{
#' ing <- find_ingredients("metoprolol")
#'
#' prods <- products_for_ingredients(
#'   ing$rxcui,
#'   include_combos = TRUE
#' )
#'
#' filter_products_by_route(prods, route = "ORAL")
#' }
#'
#' @export
filter_products_by_route <- function(
    products,
    route = "ORAL",
    keep_route_info = TRUE
) {
  stopifnot(is.data.frame(products))
  stopifnot(is.character(route), length(route) >= 1)

  if (!"product_rxcui" %in% names(products)) {
    cli::cli_abort("{.arg products} must contain a {.field product_rxcui} column.")
  }

  product_ids <- unique(stats::na.omit(products$product_rxcui))

  if (!length(product_ids)) {
    return(products[0, , drop = FALSE])
  }

  attrs <- get_clinical_attributes(product_ids)

  route_tbl <- attrs |>
    dplyr::group_by(product_rxcui = .data$rxcui) |>
    dplyr::summarise(
      route_match = any(.data$route %in% .env$route, na.rm = TRUE),
      routes = paste(sort(unique(stats::na.omit(.data$route))), collapse = "; "),
      dose_forms = paste(sort(unique(stats::na.omit(.data$dose_form))), collapse = "; "),
      dose_form_groups = paste(sort(unique(stats::na.omit(.data$dose_form_group))), collapse = "; "),
      .groups = "drop"
    )

  out <- products |>
    dplyr::left_join(route_tbl, by = "product_rxcui") |>
    dplyr::filter(.data$route_match %in% TRUE)

  if (!isTRUE(keep_route_info)) {
    out <- out |>
      dplyr::select(
        -dplyr::any_of(c(
          "route_match",
          "routes",
          "dose_forms",
          "dose_form_groups"
        ))
      )
  } else {
    out <- out |>
      dplyr::select(-dplyr::any_of("route_match"))
  }

  out
}
