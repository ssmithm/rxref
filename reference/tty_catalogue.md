# Catalogue of RxNorm TTY (Term Types)

Returns a tibble describing common RxNorm TTYs you may want to use when
expanding ingredients to products. Includes whether each TTY typically
maps cleanly to NDCs and whether it's included in the package's default
or extended TTY sets.

## Usage

``` r
tty_catalogue()
```

## Value

A tibble with metadata for key TTYs.

## Details

Columns:

- `tty`: RxNorm term type code.

- `label`: Short, human-friendly name.

- `description`: What the TTY represents in RxNorm.

- `maps_to_ndc`: Logical; whether CUIs of this TTY usually map to NDCs
  via `/rxcui/{rxcui}/ndcs`.

- `typical_role`: How it’s commonly used (product, component, group,
  name).

- `include_default`: Logical; included in `.rxref_default_ttys`.

- `include_extended`: Logical; included in `.rxref_extended_ttys`.

## Examples

``` r
tty_catalogue()
#> # A tibble: 14 × 7
#>    tty   label              description maps_to_ndc typical_role include_default
#>    <chr> <chr>              <chr>       <lgl>       <chr>        <lgl>          
#>  1 SCD   Semantic Clinical… Normalized… TRUE        product      TRUE           
#>  2 SBD   Semantic Branded … Normalized… TRUE        product      TRUE           
#>  3 GPCK  Generic Pack       Package th… TRUE        pack         TRUE           
#>  4 BPCK  Branded Pack       Package th… TRUE        pack         TRUE           
#>  5 SCDC  SCD Component      Component … FALSE       component    FALSE          
#>  6 SBDC  SBD Component      Component … FALSE       component    FALSE          
#>  7 SCDF  SCD + Dose Form    Group of S… FALSE       group        FALSE          
#>  8 SBDF  SBD + Dose Form    Group of S… FALSE       group        FALSE          
#>  9 SCDFP SCD + DF + Pack    Group of S… FALSE       group        FALSE          
#> 10 SBDFP SBD + DF + Pack    Group of S… FALSE       group        FALSE          
#> 11 SCDG  SCD Group          Higher-lev… FALSE       group        FALSE          
#> 12 SCDGP SCD Group + Pack   Group of S… FALSE       group        FALSE          
#> 13 BN    Brand Name         Brand name… FALSE       name         FALSE          
#> 14 MIN   Multi-Ingredient … Name conce… FALSE       name         FALSE          
#> # ℹ 1 more variable: include_extended <lgl>

# TTYs that map to NDCs
subset(tty_catalogue(), maps_to_ndc)$tty
#> [1] "SCD"  "SBD"  "GPCK" "BPCK"

# See what your defaults and extended sets contain
subset(tty_catalogue(), include_default)$tty
#> [1] "SCD"  "SBD"  "GPCK" "BPCK"
subset(tty_catalogue(), include_extended)$tty
#>  [1] "SCD"   "SBD"   "GPCK"  "BPCK"  "SCDC"  "SBDC"  "SCDF"  "SBDF"  "SCDFP"
#> [10] "SBDFP" "SCDG"  "SCDGP" "BN"    "MIN"  

# Pick a custom set: products + components
with(tty_catalogue(),
     tty[tty %in% c("SCD","SBD","GPCK","BPCK","SCDC","SBDC")])
#> [1] "SCD"  "SBD"  "GPCK" "BPCK" "SCDC" "SBDC"
```
