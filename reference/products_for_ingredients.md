# Expand ingredient CUIs to product CUIs that truly contain the ingredient

Tries multiple RxNav endpoints and verifies candidates truly contain the
queried ingredient or one of its acceptable related ingredient concepts.
The function unions candidates from multiple sources, verifies
ingredient containment, and reports the number of ingredients
represented by each product concept.

## Usage

``` r
products_for_ingredients(
  ingredient_rxcui,
  ttys = .rxref_default_ttys,
  route = NULL,
  include_combos = TRUE,
  concept_status = c("active", "active_and_historical"),
  historical_status = c("Obsolete", "Remapped", "Quantified", "NotCurrent"),
  show_progress = interactive()
)
```

## Arguments

- ingredient_rxcui:

  Character vector of ingredient CUIs (TTY `IN` or `PIN`).

- ttys:

  Character vector of TTYs to include. Defaults to product-facing TTYs
  returned by
  [`product_ttys()`](https://www.stevenmsmith.org/rxref/reference/product_ttys.md).
  Pass a larger set if you want groups, components, branded concepts, or
  other product-related concepts, for example
  `product_ttys("extended_product")`.

- route:

  Optional character vector of routes to retain. If `NULL`, no route
  filtering is performed. Route filtering uses
  [`get_clinical_attributes()`](https://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md).
  Route filtering is intended for product-level TTYs and may not filter
  well on broader group or package TTYs.

- include_combos:

  Logical. If `FALSE`, keep only single-ingredient products, where
  ingredient count is based on distinct `IN` concepts when available and
  otherwise falls back to distinct `PIN` concepts.

- concept_status:

  Character. Which RxNorm concept universe to search. `"active"` uses
  active-scope RxNav relationship endpoints and is the default.
  `"active_and_historical"` also searches historical RxNorm concepts
  using all-status concept retrieval and RxCUI history status metadata.
  Historical searching is slower and is intended for mapping older
  prescribing or dispensing data.

- historical_status:

  Character vector of historical RxNorm statuses to include when
  `concept_status = "active_and_historical"`. Defaults to
  `c("Obsolete", "Remapped", "Quantified", "NotCurrent")`. These values
  use RxNorm status definitions:

  `"Obsolete"`

  :   The concept is obsolete in the current RxNorm data set, and RxNorm
      has not designated an active concept as equivalent.

  `"Remapped"`

  :   The concept was active or obsolete at one time, is no longer in
      the current data set, and has been remapped to one or more active
      or obsolete concepts.

  `"Quantified"`

  :   The concept has been designated as non-dispensable because it
      lacks a quantity factor; related concepts with quantity factors
      may be available.

  `"NotCurrent"`

  :   The concept either exists in the current data set without RxNorm
      vocabulary terms, or existed in a previous monthly release but has
      since been removed and not remapped.

  See the RxNorm API documentation for concept status values:
  <https://lhncbc.nlm.nih.gov/RxNav/APIs/api-RxNorm.getAllConceptsByStatus.html>.

- show_progress:

  Logical. Show progress bars for long-running API retrieval, product
  matching, and optional route filtering steps. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

A tibble with one row per matched ingredient/product concept pair. For
`concept_status = "active"`, columns include `ingredient_rxcui`,
`product_rxcui`, `name`, `tty`, and `n_ingredients`. When
`concept_status = "active_and_historical"`, additional columns include
`concept_status`, `active_start_date`, `active_end_date`,
`release_start_date`, and `release_end_date`.
