# Map between NDC and RxCUI in either direction

Map between NDC and RxCUI in either direction

Convenience wrappers

## Usage

``` r
map_to(x, to = c("rxcui", "ndc"), status = NULL, show_progress = interactive())

map_ndc_to_rxcui(x, show_progress = interactive())

map_rxcui_to_ndc(x, status = NULL, show_progress = interactive())
```

## Arguments

- x:

  Character vector of NDCs (10/11-digit or hyphenated) or RxCUIs.

- to:

  One of c("rxcui","ndc") indicating desired output id.

- status:

  For `to = "ndc"`, filter by NDC status (e.g., "ACTIVE"). Use NULL for
  all.

- show_progress:

  Logical. Show a progress bar in interactive sessions.

## Value

tibble with columns depending on direction.
