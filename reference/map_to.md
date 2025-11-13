# Map between NDC and RxCUI in either direction

Map between NDC and RxCUI in either direction

Convenience wrappers

## Usage

``` r
map_to(x, to = c("rxcui", "ndc"), status = NULL)

map_ndc_to_rxcui(x)

map_rxcui_to_ndc(x, status = NULL)
```

## Arguments

- x:

  Character vector of NDCs (10/11-digit or hyphenated) or RxCUIs.

- to:

  One of c("rxcui","ndc") indicating desired output id.

- status:

  For `to = "ndc"`, filter by NDC status (e.g., "ACTIVE"). Use NULL for
  all.

## Value

tibble with columns depending on direction.
