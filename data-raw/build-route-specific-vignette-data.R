# data-raw/build-route-specific-vignette-data.R

# Recompute precomputed datasets for:
# vignettes/route_specific_lists.Rmd
#
# Expected output files:
# inst/extdata/bb_ingredients.rds
# inst/extdata/bb_products.rds
# inst/extdata/bb_single_ingredient_products.rds
# inst/extdata/bb_attrs.rds
# inst/extdata/bb_oral_products.rds
# inst/extdata/bb_oral_ndcs.rds
# inst/extdata/bb_oral_rxcuis_search.rds
# inst/extdata/bb_oral_ndcs_search.rds
# inst/extdata/bb_oral_both_search.rds

library(rxref)
library(dplyr)

# Optional: be gentle with the RxNav APIs during batch calls
rxref_conf(rate_delay = 0.2)

# Ensure extdata exists
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------
# Ingredient names used in the vignette
# -------------------------------------------------------------------------

beta_blocker_names <- c(
  "acebutolol",
  "atenolol",
  "betaxolol",
  "bisoprolol",
  "carvedilol",
  "labetalol",
  "metoprolol",
  "nadolol",
  "nebivolol",
  "penbutolol",
  "pindolol",
  "propranolol",
  "sotalol",
  "timolol"
)

# -------------------------------------------------------------------------
# 1. Resolve names to ingredient RxCUIs
# -------------------------------------------------------------------------

bb_ingredients <- find_ingredients(beta_blocker_names) |>
  filter(tty == "IN") |>
  distinct(
    input,
    ingredient_rxcui = rxcui,
    ingredient_name = name,
    ingredient_tty = tty
  ) |>
  arrange(input, ingredient_name)

saveRDS(
  bb_ingredients,
  file = "inst/extdata/bb_ingredients.rds",
  version = 2
)

# -------------------------------------------------------------------------
# 2. Expand ingredients to product RxCUIs, including combination products
# -------------------------------------------------------------------------

bb_products <- products_for_ingredients(
  bb_ingredients$ingredient_rxcui,
  ttys = product_ttys("default"),
  include_combos = TRUE,
  concept_status = "active"
) |>
  left_join(bb_ingredients, by = "ingredient_rxcui") |>
  arrange(ingredient_name, product_rxcui)

saveRDS(
  bb_products,
  file = "inst/extdata/bb_products.rds",
  version = 2
)

# -------------------------------------------------------------------------
# 3. Expand ingredients to product RxCUIs, excluding combination products
# -------------------------------------------------------------------------

bb_single_ingredient_products <- products_for_ingredients(
  bb_ingredients$ingredient_rxcui,
  ttys = product_ttys("default"),
  include_combos = FALSE,
  concept_status = "active"
) |>
  left_join(bb_ingredients, by = "ingredient_rxcui") |>
  arrange(ingredient_name, product_rxcui)

saveRDS(
  bb_single_ingredient_products,
  file = "inst/extdata/bb_single_ingredient_products.rds",
  version = 2
)

# -------------------------------------------------------------------------
# 4. Retrieve clinical attributes for product RxCUIs
# -------------------------------------------------------------------------

bb_attrs <- get_clinical_attributes(
  unique(bb_products$product_rxcui)
) |>
  rename(product_rxcui = rxcui) |>
  arrange(product_rxcui)

saveRDS(
  bb_attrs,
  file = "inst/extdata/bb_attrs.rds",
  version = 2
)

# -------------------------------------------------------------------------
# 5. Filter product list to oral products
# -------------------------------------------------------------------------

bb_oral_products <- bb_products |>
  filter_products_by_route(route = "ORAL") |>
  arrange(ingredient_name, product_rxcui)

saveRDS(
  bb_oral_products,
  file = "inst/extdata/bb_oral_products.rds",
  version = 2
)

# -------------------------------------------------------------------------
# Helper functions for enriching compact search outputs
# -------------------------------------------------------------------------

add_ingredient_metadata <- function(x, ingredients = bb_ingredients) {
  if ("ingredient_name" %in% names(x)) {
    return(x)
  }

  x |>
    left_join(
      ingredients |>
        select(ingredient_rxcui, ingredient_name),
      by = "ingredient_rxcui"
    )
}

add_product_metadata <- function(x, products = bb_oral_products) {
  product_cols <- c(
    "name",
    "tty",
    "route",
    "dose_form",
    "dose_form_group"
  )

  missing_product_cols <- setdiff(product_cols, names(x))

  if (length(missing_product_cols) == 0) {
    return(x)
  }

  x |>
    left_join(
      products |>
        select(
          ingredient_rxcui,
          product_rxcui,
          all_of(missing_product_cols)
        ),
      by = c("ingredient_rxcui", "product_rxcui")
    )
}

has_join_suffixes <- function(x) {
  any(grepl("\\.(x|y)$", names(x)))
}

# -------------------------------------------------------------------------
# 6. Map oral product RxCUIs to active NDCs
# -------------------------------------------------------------------------

bb_oral_ndc_map <- map_rxcui_to_ndc(
  unique(bb_oral_products$product_rxcui),
  status = "ACTIVE"
)

bb_oral_ndcs <- bb_oral_ndc_map |>
  left_join(
    bb_oral_products,
    by = c("rxcui" = "product_rxcui")
  ) |>
  rename(
    product_rxcui = rxcui
  ) |>
  distinct() |>
  arrange(ingredient_name, product_rxcui, ndc11)

saveRDS(
  bb_oral_ndcs,
  file = "inst/extdata/bb_oral_ndcs.rds",
  version = 2
)

# -------------------------------------------------------------------------
# 7. Compact search_drug() workflow: product RxCUIs only
# -------------------------------------------------------------------------

bb_oral_rxcuis_search <- search_drug(
  term = beta_blocker_names,
  return = "rxcui",
  route = "ORAL",
  include_combos = TRUE,
  concept_status = "active"
) |>
  arrange(ingredient_rxcui, product_rxcui)

saveRDS(
  bb_oral_rxcuis_search,
  file = "inst/extdata/bb_oral_rxcuis_search.rds",
  version = 2
)

# -------------------------------------------------------------------------
# 8. Compact search_drug() workflow: NDCs
# -------------------------------------------------------------------------

bb_oral_ndcs_search_raw <- search_drug(
  term = beta_blocker_names,
  return = "ndc",
  route = "ORAL",
  ndc_status = "ACTIVE",
  include_combos = TRUE,
  concept_status = "active"
)

bb_oral_ndcs_search <- bb_oral_ndcs_search_raw |>
  add_ingredient_metadata() |>
  add_product_metadata() |>
  distinct() |>
  arrange(ingredient_name, product_rxcui, ndc11)

saveRDS(
  bb_oral_ndcs_search,
  file = "inst/extdata/bb_oral_ndcs_search.rds",
  version = 2
)

# -------------------------------------------------------------------------
# 9. Compact search_drug() workflow: both products and NDCs
# -------------------------------------------------------------------------

bb_oral_both_raw <- search_drug(
  term = beta_blocker_names,
  return = "both",
  route = "ORAL",
  ndc_status = "ACTIVE",
  include_combos = TRUE,
  concept_status = "active"
)

# Depending on implementation, return = "both" should return a list with
# product and NDC components. Normalize names defensively.
bb_oral_both <- bb_oral_both_raw

if ("products" %in% names(bb_oral_both)) {
  bb_oral_both$products <- bb_oral_both$products |>
    add_ingredient_metadata() |>
    add_product_metadata() |>
    distinct() |>
    select(
      ingredient_rxcui,
      ingredient_name,
      product_rxcui,
      name,
      tty,
      n_ingredients,
      route,
      dose_form,
      dose_form_group,
      everything()
    ) |>
    arrange(ingredient_name, product_rxcui)
}

if ("ndcs" %in% names(bb_oral_both)) {
  bb_oral_both$ndcs <- bb_oral_both$ndcs |>
    add_ingredient_metadata() |>
    add_product_metadata()

  if ("status" %in% names(bb_oral_both$ndcs) &&
      !"ndc_status" %in% names(bb_oral_both$ndcs)) {
    bb_oral_both$ndcs <- bb_oral_both$ndcs |>
      rename(ndc_status = status)
  }

  bb_oral_both$ndcs <- bb_oral_both$ndcs |>
    distinct() |>
    select(
      ingredient_rxcui,
      ingredient_name,
      product_rxcui,
      name,
      tty,
      route,
      dose_form,
      dose_form_group,
      ndc11,
      ndc_status,
      everything()
    ) |>
    arrange(ingredient_name, product_rxcui, ndc11)
}

saveRDS(
  bb_oral_both,
  file = "inst/extdata/bb_oral_both_search.rds",
  version = 2
)

# -------------------------------------------------------------------------
# 10. Lightweight checks
# -------------------------------------------------------------------------

message("Saved route-specific beta-blocker vignette example datasets:")

both_products_n <- if (
  is.list(bb_oral_both) &&
  "products" %in% names(bb_oral_both) &&
  is.data.frame(bb_oral_both$products)
) {
  nrow(bb_oral_both$products)
} else {
  NA_integer_
}

both_ndcs_n <- if (
  is.list(bb_oral_both) &&
  "ndcs" %in% names(bb_oral_both) &&
  is.data.frame(bb_oral_both$ndcs)
) {
  nrow(bb_oral_both$ndcs)
} else {
  NA_integer_
}

checks <- tibble::tibble(
  file = c(
    "bb_ingredients.rds",
    "bb_products.rds",
    "bb_single_ingredient_products.rds",
    "bb_attrs.rds",
    "bb_oral_products.rds",
    "bb_oral_ndcs.rds",
    "bb_oral_rxcuis_search.rds",
    "bb_oral_ndcs_search.rds",
    "bb_oral_both_search.rds: products",
    "bb_oral_both_search.rds: ndcs"
  ),
  n_rows = c(
    nrow(bb_ingredients),
    nrow(bb_products),
    nrow(bb_single_ingredient_products),
    nrow(bb_attrs),
    nrow(bb_oral_products),
    nrow(bb_oral_ndcs),
    nrow(bb_oral_rxcuis_search),
    nrow(bb_oral_ndcs_search),
    both_products_n,
    both_ndcs_n
  )
)

print(checks)

# Required column checks ----------------------------------------------------

stopifnot(
  all(c(
    "input",
    "ingredient_rxcui",
    "ingredient_name",
    "ingredient_tty"
  ) %in% names(bb_ingredients))
)

stopifnot(
  all(c(
    "ingredient_rxcui",
    "ingredient_name",
    "product_rxcui"
  ) %in% names(bb_products))
)

stopifnot(
  all(c(
    "ingredient_rxcui",
    "ingredient_name",
    "product_rxcui"
  ) %in% names(bb_single_ingredient_products))
)

stopifnot(
  all(c(
    "product_rxcui",
    "route",
    "dose_form",
    "dose_form_group"
  ) %in% names(bb_attrs))
)

stopifnot(
  all(c(
    "ingredient_rxcui",
    "ingredient_name",
    "product_rxcui",
    "route",
    "dose_form",
    "dose_form_group"
  ) %in% names(bb_oral_products))
)

stopifnot(
  all(c(
    "product_rxcui",
    "ndc11",
    "ndc_status"
  ) %in% names(bb_oral_ndcs))
)

stopifnot(
  all(c(
    "ingredient_rxcui",
    "product_rxcui"
  ) %in% names(bb_oral_rxcuis_search))
)

stopifnot(
  all(c(
    "ingredient_rxcui",
    "ingredient_name",
    "product_rxcui",
    "ndc11",
    "ndc_status"
  ) %in% names(bb_oral_ndcs_search))
)

stopifnot(
  is.list(bb_oral_both),
  all(c("products", "ndcs") %in% names(bb_oral_both))
)

stopifnot(
  all(c(
    "ingredient_rxcui",
    "ingredient_name",
    "product_rxcui",
    "name",
    "tty",
    "route",
    "dose_form",
    "dose_form_group"
  ) %in% names(bb_oral_both$products))
)

stopifnot(
  all(c(
    "ingredient_rxcui",
    "ingredient_name",
    "product_rxcui",
    "ndc11",
    "ndc_status",
    "name",
    "tty",
    "route",
    "dose_form",
    "dose_form_group"
  ) %in% names(bb_oral_both$ndcs))
)

# No accidental join suffixes ------------------------------------------------

stopifnot(!has_join_suffixes(bb_oral_ndcs_search))

if (is.data.frame(bb_oral_both$products)) {
  stopifnot(!has_join_suffixes(bb_oral_both$products))
}

if (is.data.frame(bb_oral_both$ndcs)) {
  stopifnot(!has_join_suffixes(bb_oral_both$ndcs))
}

message("Done.")
