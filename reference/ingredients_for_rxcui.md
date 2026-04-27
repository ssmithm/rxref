# Get ingredient concepts for RxCUIs

Maps one or more RxCUIs to related ingredient concepts, returning
ingredient RxCUIs, names, and term types. This is useful when the input
is already a specific RxNorm product concept rather than a free-text
drug name.

## Usage

``` r
ingredients_for_rxcui(
  rxcui,
  include_pin = TRUE,
  include_min = FALSE,
  show_progress = interactive()
)
```

## Arguments

- rxcui:

  Character vector of RxCUIs.

- include_pin:

  Logical. Include precise ingredient concepts (`PIN`).

- include_min:

  Logical. Include multiple ingredient concepts (`MIN`).

- show_progress:

  Logical. Show a progress bar in interactive sessions.

## Value

A tibble with columns `rxcui`, `ingredient_rxcui`, `ingredient_name`,
and `ingredient_tty`.
