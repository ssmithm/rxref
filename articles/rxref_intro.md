# rxref: Getting started

``` r
# install.packages("devtools")
# devtools::install_github("ssmithm/rxref")
library(rxref)
library(dplyr)
```

## Resolve messy inputs to RxCUIs

``` r
resolve(c("metformin 500 mg tablet", "00093-1048-01", "8610"))
```

## Properties for a set of RxCUIs

``` r
ids <- c("860975", "860976")
get_properties(ids)
```

## Map in both directions

``` r
# NDC -> RxCUI(s)
map_ndc_to_rxcui(c("00093-1048-01", "00093-1048-10"))


# RxCUI -> NDCs (optionally filter by ACTIVE)
map_rxcui_to_ndc("860975")
map_rxcui_to_ndc("860975", status = "ACTIVE")
```

## Notes

- Results are memoised on disk between sessions. Use
  `rxref_conf(cache = cachem::cache_mem())` for in-memory only.
- Be kind to the API; increase `rxref.rate_delay` if batching many
  requests.
- For strict reproducibility, we’ll add a SQL backend bound to monthly
  RxNorm releases in a later version.
