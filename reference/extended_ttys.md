# Extended RxNorm term types

Returns an extended set of RxNorm term types that includes the default
and extended product term types plus drug component, brand name, and
ingredient concepts. This list includes essentially all term types that
capture a specific ingredient, thus it excludes dose form, dose form
group, prescribable name, synonyms, and others that are not associated
with a specific ingredient.

## Usage

``` r
extended_ttys()
```

## Value

A character vector of RxNorm term type abbreviations.

## Details

This can be useful when the broadest set of RxNorm concepts that still
capture an ingredient is desired.

## Examples

``` r
extended_ttys()
#>  [1] "SCD"   "SBD"   "GPCK"  "BPCK"  "SCDG"  "SBDG"  "SCDF"  "SBDF"  "SBDFP"
#> [10] "SCDFP" "SCDGP" "SCDC"  "SBDC"  "BN"    "MIN"   "PIN"   "IN"   
```
