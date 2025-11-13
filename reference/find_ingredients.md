# Resolve a free-text drug name to ingredient CUIs (IN/PIN)

Uses RxNav approximateTerm, then fetches properties for each candidate
and filters to ingredient-type concepts (TTY `IN` and, optionally,
`PIN`).

## Usage

``` r
find_ingredients(term, max_entries = 10, include_pin = TRUE)
```

## Arguments

- term:

  Character vector (free text).

- max_entries:

  Integer. Max approximate-term candidates per input (default 10).

- include_pin:

  Logical. Include precise-ingredient (`PIN`) CUIs (default TRUE).

## Value

A tibble with columns: `input`, `rxcui`, `name`, `tty`, `score`.

## Examples

``` r
if (identical(Sys.getenv("RXREF_ONLINE"), "1")) {
  find_ingredients("metformin")
}
```
