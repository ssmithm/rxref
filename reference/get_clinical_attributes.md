# Clinical attributes from the concept (SCD/SBD) or related SCD/SBD

If `rxcui` is already a clinical drug (TTY = SCD or SBD), parse
attributes directly from its own name. Otherwise, query
`/rxcui/{id}/related?tty=SCD,SBD` and parse.

## Usage

``` r
get_clinical_attributes(rxcui)
```

## Arguments

- rxcui:

  Character vector of RxCUIs

## Value

A tibble with columns:

- rxcui:

  Input RxCUI

- related_rxcui:

  Clinical drug RxCUI (SCD/SBD) used for attributes

- name:

  Clinical drug name

- tty:

  Term type (SCD/SBD, etc.)

- strength:

  Parsed strength string (e.g. "500 MG")

- dose_form:

  Parsed dose form (e.g. "Extended Release Oral Tablet")

- route:

  Route parsed from dose form / DFG (e.g. "ORAL", "INJECTION")

- dose_form_group:

  Dose form group (DFG), if available

- is_brand:

  Logical; TRUE for branded clinical concepts (SBD/BPCK)

- is_generic:

  Logical; TRUE for generic clinical concepts (SCD/GPCK)

- ingredient_count:

  Number of distinct ingredients

- ingredient_rxcui:

  List-column of ingredient RxCUIs

- ingredient_name:

  List-column of ingredient names

- ingredient_tty:

  List-column of ingredient term types (IN/PIN/MIN)

- is_multi_ingredient:

  Logical; TRUE if \>1 ingredient

- suppress:

  Raw RxNorm suppress flag from properties

- status:

  Simple status derived from suppress: "ACTIVE" vs "INACTIVE"

## Details

In addition to strength and dose form, this returns route, dose-form
group (DFG), brand/generic flags, ingredient summaries, and a simple
active/inactive status.

Note there is a fair amount of parsing of the RxNorm STR value to try to
extract relevant information (e.g., strength, dose_form), so check
closely before trusting. There may be edge cases that are not correctly
parsed.

## Examples

``` r
# \donttest{
get_clinical_attributes(c("861007","860975")) |>
  dplyr::select(rxcui, related_rxcui, name, strength, dose_form, route, tty) |>
  head()
#> # A tibble: 2 × 7
#>   rxcui  related_rxcui name                       strength dose_form route tty  
#>   <chr>  <chr>         <chr>                      <chr>    <chr>     <chr> <chr>
#> 1 861007 861007        metformin hydrochloride 5… 500 MG   Oral Tab… ORAL  SCD  
#> 2 860975 860975        24 HR metformin hydrochlo… 500 MG   Extended… ORAL  SCD  
# }
```
