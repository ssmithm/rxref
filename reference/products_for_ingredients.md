# Expand ingredient CUIs to product CUIs that truly contain the ingredient

Tries multiple RxNav endpoints and verifies candidates truly contain the
queried ingredient (or its PIN children). Handles cases where TTY
appears only at the group level. Unions candidates from all sources.

## Usage

``` r
products_for_ingredients(
  ingredient_rxcui,
  ttys = .rxref_default_ttys,
  route = NULL,
  include_combos = TRUE
)
```

## Arguments

- ingredient_rxcui:

  Character vector of ingredient CUIs (TTY `IN` or `PIN`).

- ttys:

  Character vector of TTYs to include (default: product-facing
  `c("SCD","SBD","GPCK","BPCK")`). Pass a larger set if you want groups,
  components, names, etc. (e.g.,
  `c(.rxref_default_ttys, .rxref_extended_ttys)`).

- route:

  Optional character vector of routes to retain. If `NULL`, no route
  filtering is performed. Route filtering uses
  [`get_clinical_attributes()`](http://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md).
  Note that route is intended for product-level TTYs (see
  `.rxref_default_ttys`) and may not filter well on other TTYs such as
  those in `.rxref_extended_ttys`.

- include_combos:

  Logical; if `FALSE`, keep only single-ingredient products (counting
  distinct `IN`; if none present, falls back to distinct `PIN`).

## Value

Tibble with columns: `ingredient_rxcui`, `product_rxcui`, `name`, `tty`,
`n_ingredients`.
