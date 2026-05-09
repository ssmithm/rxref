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

- show_progress:

  Logical. Show a progress bar in interactive sessions. Progress is
  shown only when at least 5 inputs are supplied.

## Value

A tibble containing only rows whose `product_rxcui` has at least one
matching route.

## Examples

``` r
# \donttest{
ing <- find_ingredients("metoprolol")

prods <- products_for_ingredients(
  ing$rxcui,
  include_combos = TRUE
)

filter_products_by_route(prods, route = "ORAL")
#> # A tibble: 34 × 8
#>    ingredient_rxcui product_rxcui name     tty   n_ingredients routes dose_forms
#>    <chr>            <chr>         <chr>    <chr>         <int> <chr>  <chr>     
#>  1 6918             2047766       24 HR m… SBD               1 ORAL   Extended …
#>  2 6918             2047769       24 HR m… SBD               1 ORAL   Extended …
#>  3 6918             2047772       24 HR m… SBD               1 ORAL   Extended …
#>  4 6918             2047775       24 HR m… SBD               1 ORAL   Extended …
#>  5 6918             2712152       metopro… SBD               1 ORAL   Oral Solu…
#>  6 6918             2723027       metopro… SBD               1 ORAL   Oral Tabl…
#>  7 6918             866414        24 HR m… SBD               1 ORAL   Extended …
#>  8 6918             866421        24 HR m… SBD               1 ORAL   Extended …
#>  9 6918             866429        24 HR m… SBD               1 ORAL   Extended …
#> 10 6918             866438        24 HR m… SBD               1 ORAL   Extended …
#> # ℹ 24 more rows
#> # ℹ 1 more variable: dose_form_groups <chr>
# }
```
