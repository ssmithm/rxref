# Default RxNorm product term types

Returns the default RxNorm term types used by rxref when identifying
drug products.

## Usage

``` r
default_product_ttys()
```

## Value

A character vector of RxNorm term type abbreviations.

## Details

The default set is intentionally focused on product-level concepts:
semantic clinical drugs, semantic branded drugs, generic packs, and
branded packs.

## Examples

``` r
default_product_ttys()
#> [1] "SCD"  "SBD"  "GPCK" "BPCK"
```
