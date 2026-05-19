# Clinical attributes from the concept (SCD/SBD) or related SCD/SBD

If `rxcui` is already a clinical drug (TTY = SCD or SBD), parse
attributes directly from its own name. Otherwise, query
`/rxcui/{id}/related?tty=SCD,SBD` and parse.

## Usage

``` r
get_clinical_attributes(
  rxcui,
  include_historical = FALSE,
  show_progress = interactive()
)
```

## Arguments

- rxcui:

  Character vector of RxCUIs

- include_historical:

  Logical. If `TRUE`, use RxCUI history status metadata as a fallback
  for RxCUIs that do not return active clinical attributes. This is
  useful for obsolete, remapped, quantified, or otherwise non-current
  RxCUIs found in historical prescribing data.

- show_progress:

  Logical. Show a progress bar in interactive sessions. Progress is
  shown only when at least 5 inputs are supplied.

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

  Ingredient RxCUI. For combination products, multiple values are
  returned as semicolon-delimited strings.

- ingredient_name:

  Ingredient name. For combination products, multiple values are
  returned as semicolon-delimited strings.

- ingredient_tty:

  Ingredient term type (IN/PIN/MIN). For combination products, multiple
  values are returned as semicolon-delimited strings.

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

For combination products, ingredient-related columns may contain
multiple semicolon-delimited values, such as `"amlodipine; valsartan"`.

## Examples

``` r
if (FALSE) { # \dontrun{
get_clinical_attributes(c("861007","860975")) |>
  dplyr::select(rxcui, related_rxcui, name, strength, dose_form, route, tty) |>
  head()
} # }
```
