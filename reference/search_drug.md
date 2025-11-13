# Search free-text drug name and return product CUIs and/or NDCs

High-level convenience: free text -\> ingredient(s) (IN/PIN) -\>
verified product CUIs, and optionally expand to NDCs with status
filtering.

## Usage

``` r
search_drug(
  term,
  return = c("rxcui", "ndc", "both"),
  ndc_status = NULL,
  ttys = .rxref_default_ttys,
  ...
)
```

## Arguments

- term:

  Character vector; free-text drug names.

- return:

  One of `c("rxcui","ndc","both")`.

- ndc_status:

  Optional character vector to filter NDCs. Options are "ACTIVE",
  "OBSOLETE", "UNSPECIFIED" (the API may also return no value, which
  will appear as NA).

- ttys:

  Character vector of TTYs to include in product search. Defaults to
  `.rxref_default_ttys`. Other prespecified option is
  `.rxref_extended_ttys` or a character vector of explicit TTYs. Run
  [`tty_catalogue()`](http://www.stevenmsmith.org/rxref/reference/tty_catalogue.md)
  to review options.

- ...:

  Passed to
  [`products_for_ingredients()`](http://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
  (e.g., include_combos = FALSE)

## Value

If `return="rxcui"`: tibble of products. If `"ndc"`: tibble of NDCs with
`ingredient_rxcui`, `product_rxcui`, `ndc11`, `ndc_status`. If `"both"`:
list(products=…, ndcs=…).
