# RxNorm product term type sets

Returns common RxNorm term type sets used by rxref.

## Usage

``` r
product_ttys(set = c("default", "extended_product", "extended"))
```

## Arguments

- set:

  One of `"default"`, `"extended_product"`, or `"extended"`.

## Value

A character vector of RxNorm term type abbreviations.

## Examples

``` r
product_ttys()
#> [1] "SCD"  "SBD"  "GPCK" "BPCK"
product_ttys("extended")
#>  [1] "SCD"   "SBD"   "GPCK"  "BPCK"  "SCDG"  "SBDG"  "SCDF"  "SBDF"  "SBDFP"
#> [10] "SCDFP" "SCDGP" "SCDC"  "SBDC"  "BN"    "MIN"   "PIN"   "IN"   
```
