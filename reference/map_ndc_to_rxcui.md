# Map NDCs to RxCUIs

Convenience wrapper around
[`map_to()`](https://www.stevenmsmith.org/rxref/reference/map_to.md) for
mapping National Drug Codes (NDCs) to RxNorm Concept Unique Identifiers
(RxCUIs).

## Usage

``` r
map_ndc_to_rxcui(x, show_progress = interactive())
```

## Arguments

- x:

  Character vector of NDCs.

- show_progress:

  Logical; if `TRUE`, show a progress bar for vectorized mapping
  operations. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

A tibble with columns:

- input:

  Original input NDC.

- ndc11:

  Normalized 11-digit NDC.

- rxcui:

  Mapped RxCUI, if found.

## Details

NDCs may be supplied as 10-digit, 11-digit, or hyphenated values. Input
NDCs are normalized to 11-digit format before querying RxNorm.

## Examples

``` r
if (FALSE) { # \dontrun{
map_ndc_to_rxcui("00093-7424-56")
} # }
```
