#' rxref: Tidy RxNorm utilities
#'
#' `rxref` provides tidy, API-first tools for working with RxNorm and RxClass
#' data in R. The package helps users resolve drug names and identifiers,
#' inspect RxNorm concepts, map between RxCUIs and NDCs, expand ingredients to
#' product-level concepts, retrieve drug class information, and derive clinically
#' useful product attributes.
#'
#' Common workflows include:
#'
#' * Resolving drug names or identifiers with [resolve()] and [search_drug()].
#' * Retrieving concept metadata with [get_properties()].
#' * Building product-level medication lists with [find_ingredients()],
#'   [ingredients_for_rxcui()], and [products_for_ingredients()].
#' * Mapping between RxCUIs and NDCs with [map_to()],
#'   [map_ndc_to_rxcui()], and [map_rxcui_to_ndc()].
#' * Retrieving therapeutic class information with [get_classes()],
#'   [find_classes()], [get_class_members()], [get_atc()], [get_epc()],
#'   and [get_va()].
#' * Deriving clinical product attributes with [get_clinical_attributes()] and
#'   [filter_products_by_route()].
#'
#' Configure package behavior with [rxref_conf()].
#'
#' @keywords internal
"_PACKAGE"


#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate select rename relocate arrange distinct filter left_join bind_rows bind_cols
#' @importFrom tidyr unnest unnest_wider unnest_longer replace_na
#' @importFrom purrr map map2 map_chr map_int map_lgl imap compact pluck possibly quietly
#' @importFrom stringr str_detect str_trim str_remove_all str_replace_all str_squish
#' @importFrom vctrs vec_recycle vec_cast vec_c vec_rbind
#' @importFrom jsonlite fromJSON
#' @importFrom memoise memoise cache_filesystem
#' @importFrom rlang .data .env
NULL
