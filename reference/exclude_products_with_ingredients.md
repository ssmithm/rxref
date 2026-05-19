# Exclude products containing specified ingredients

Remove all rows for products that contain one or more user-specified
ingredients. This is useful when broad ingredient-based product searches
return fixed-dose combination products that contain an ingredient of
interest but are not clinically relevant to a specific study definition.

## Usage

``` r
exclude_products_with_ingredients(
  data,
  ingredients = NULL,
  ingredient_rxcuis = NULL,
  product_id_col = "product_rxcui",
  ingredient_name_col = "ingredient_name",
  ingredient_rxcui_col = "ingredient_rxcui",
  return_excluded = FALSE
)
```

## Arguments

- data:

  A data frame containing product RxCUIs and ingredient information,
  such as that produced by
  [`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md).

- ingredients:

  Optional character vector of ingredient names to exclude. These are
  resolved to ingredient RxCUIs using
  [`find_ingredients()`](https://www.stevenmsmith.org/rxref/reference/find_ingredients.md).

- ingredient_rxcuis:

  Optional character vector of ingredient RxCUIs to exclude. These are
  used directly and do not require name resolution.

- product_id_col:

  Name of the column containing product RxCUIs. Default is
  `product_rxcui`.

- ingredient_name_col:

  Name of the column containing ingredient names. Used for reporting
  and, when needed, fallback matching. Default is `ingredient_name`.

- ingredient_rxcui_col:

  Name of the column containing ingredient RxCUIs. Default is
  `ingredient_rxcui`.

- return_excluded:

  Logical. If `FALSE`, return the filtered data frame. If `TRUE`, return
  a list with `data`, `excluded`, and `resolved_ingredients`.

## Value

If `return_excluded = FALSE`, a data frame of the same general shape as
`data`, with excluded product rows removed.

If `return_excluded = TRUE`, a list with:

- data:

  The filtered data frame.

- excluded:

  Rows from the input data belonging to excluded products.

- resolved_ingredients:

  The ingredient names/RxCUIs used for exclusion.

## Details

Exclusions are applied at the product level. If any ingredient in a
product matches the exclusion list, all rows for that product are
removed.

## Examples

``` r
if (FALSE) { # \dontrun{
antihtn_clean <- antihtn_products |>
  exclude_products_with_ingredients(ingredients = "sacubitril")

antihtn_audit <- antihtn_products |>
  exclude_products_with_ingredients(
    ingredients = "sacubitril",
    return_excluded = TRUE
  )

antihtn_audit$data
antihtn_audit$excluded
antihtn_audit$resolved_ingredients
} # }
```
