# Extended RxNorm product term types

Returns an extended set of RxNorm term types that includes the default
product term types plus dose-form and dose-form-group concepts.

## Usage

``` r
extended_product_ttys()
```

## Value

A character vector of RxNorm term type abbreviations.

## Details

This can be useful when a broader set of product-related RxNorm concepts
is desired.

## Examples

``` r
extended_product_ttys()
#>  [1] "SCD"   "SBD"   "GPCK"  "BPCK"  "SCDG"  "SBDG"  "SCDF"  "SBDF"  "SBDFP"
#> [10] "SCDFP" "SCDGP"
```
