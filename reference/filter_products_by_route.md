# Filter RxNorm product concepts by route of administration.

Filters a product-level rxref table using route information from
[`get_clinical_attributes()`](https://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md).
Useful when a drug or class includes products across multiple routes,
such as oral tablets, ophthalmic solutions, injectables, patches, or
topical formulations.

## Usage

``` r
filter_products_by_route(
  products,
  route = "ORAL",
  keep_route_info = TRUE,
  include_historical = FALSE,
  show_progress = interactive()
)
```

## Arguments

- products:

  A tibble containing a `product_rxcui` column, such as the output of
  [`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md).

- route:

  Character vector of routes to keep. Common values include `"ORAL"`,
  `"INJECTION"`, `"OPHTHALMIC"`, `"OTIC"`, `"INHALATION"`, `"TOPICAL"`,
  `"RECTAL"`, `"VAGINAL"`, `"URETHRAL"`, and `"IMPLANT"`.

- keep_route_info:

  Logical. If `TRUE` (default), append route, dose form, and dose-form
  group summaries to the returned table.

- include_historical:

  Logical. If `TRUE`, use RxCUI history status metadata as a fallback
  for RxCUIs that do not return active clinical attributes. This is
  useful for obsolete, remapped, quantified, or otherwise non-current
  RxCUIs found in historical prescribing data. If `products` contains a
  `concept_status` column with non-`"active"` value, historical lookup
  is enabled automatically.

- show_progress:

  Logical. Show a progress bar in interactive sessions. Progress is
  shown only when at least 5 inputs are supplied.

## Value

A tibble containing only rows whose `product_rxcui` has at least one
matching route.

## Examples

``` r
if (FALSE) { # \dontrun{
ing <- find_ingredients("metoprolol")

prods <- products_for_ingredients(
  ing$rxcui,
  include_combos = TRUE
)

filter_products_by_route(prods, route = "ORAL")

# Historical/current products can also be route-filtered
prods_hist <- products_for_ingredients(
  ing$rxcui,
  include_combos = TRUE,
  concept_status = "active_and_historical"
)

filter_products_by_route(prods_hist, route = "ORAL")
} # }
```
