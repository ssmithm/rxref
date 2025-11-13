# Resolve free text, RxCUI, or NDC to RxCUI and preferred name

Vectorized over `x`. For free text, uses RxNorm approximateTerm. For
NDC, uses findRxcuiById. For RxCUI, validates and returns properties.

## Usage

``` r
resolve(x, type = c("auto", "name", "rxcui", "ndc"), max_entries = 1)
```

## Arguments

- x:

  Character vector: drug string, RxCUI, or NDC (10/11-digit or
  hyphenated)

- type:

  One of c("auto","name","rxcui","ndc"). Default "auto" infers.

- max_entries:

  Integer, passed to approximateTerm for name queries.

## Value

A tibble with columns: input, type, rxcui, name, tty, score (if name),
ndc11 (if ndc input), matched_term (if name input)
