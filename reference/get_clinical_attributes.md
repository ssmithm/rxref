# Clinical attributes from the concept (SCD/SBD) or related SCD/SBD

If `rxcui` is already a clinical drug (SCD/SBD), parse attributes
directly from its own name. Otherwise, query
`/rxcui/{id}/related?tty=SCD,SBD` and parse.

## Usage

``` r
get_clinical_attributes(rxcui)
```

## Arguments

- rxcui:

  Character vector of RxCUIs

## Value

A tibble with 6 columns: `rxcui`, `related_rxcui`, `name`, `tty`,
`strength`, `dose_form`

## Details

Note: This works, but is still in active development.

## Examples

``` r
# \donttest{
get_clinical_attributes(c("861007","860975")) |>
  dplyr::select(rxcui, related_rxcui, name, strength, dose_form, tty) |>
  head()
#> # A tibble: 2 × 6
#>   rxcui  related_rxcui name                             strength dose_form tty  
#>   <chr>  <chr>         <chr>                            <chr>    <chr>     <chr>
#> 1 861007 861007        metformin hydrochloride 500 MG … 500 MG   Oral Tab… SCD  
#> 2 860975 860975        24 HR metformin hydrochloride 5… 500 MG   Extended… SCD  
# }
```
