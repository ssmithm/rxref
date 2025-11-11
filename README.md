
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rxref

<!-- badges: start -->

<!-- badges: end -->

*rxref* is a set of tidy utilities for querying the [National Library of
Medicine](https://www.nlm.nih.gov/)’s
[RxNorm](https://www.nlm.nih.gov/research/umls/rxnorm/index.html) API.
RxNorm is a system of normalized names for clinical drugs as well as
linkages from these names to many of the drug vocabularies commonly used
in pharmacy management and drug interaction software, including those of
First Databank, Micromedex, Multum, and Gold Standard Drug Database.

*rxref* was originally built to facilitate pharmacoepidemiologic
research in the [UF CVmedLab](https://cvmedlab.org/), where we often
need to identify medication prescriptions or pharmacy dispensings as a
proxy for medication exposure or outcomes.

The core use case for *rxref* is identifying a set of RxCUIs (Rx concept
unique identifiers) and/or NDCs (national drug codes) that relate to a
single medication or set of medications supplied by the user. Additional
functionality (still in early stage development) will include extracting
additional properties (e.g., strength, dosage form, indications, adverse
events, and so on) related to a user-supplied list of RxCUIs.

## Installation

You can install the development version of rxref from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ssmithm/rxref")
```

## Example

This is a basic example for querying RxCUIs associated with lisinopril
(an blood pressure-lowering drug):

``` r
library(rxref)
lisinopril.ing <- find_ingredients('lisinopril')
lisinopril.ing
#> # A tibble: 1 × 5
#>   input      rxcui name       tty   score
#>   <chr>      <chr> <chr>      <chr> <dbl>
#> 1 lisinopril 29046 lisinopril IN     11.9

lisinopril.rxcuis <- products_for_ingredients(lisinopril.ing$rxcui, ttys = c("SCD", "SBD"), include_combos = TRUE)
lisinopril.rxcuis |> head(10)
#> # A tibble: 10 × 5
#>    ingredient_rxcui product_rxcui name                       tty   n_ingredients
#>    <chr>            <chr>         <chr>                      <chr>         <int>
#>  1 29046            104375        lisinopril 2.5 MG Oral Ta… SBD               1
#>  2 29046            104376        lisinopril 5 MG Oral Tabl… SBD               1
#>  3 29046            104377        lisinopril 10 MG Oral Tab… SBD               1
#>  4 29046            104378        lisinopril 20 MG Oral Tab… SBD               1
#>  5 29046            1806890       lisinopril 1 MG/ML Oral S… SBD               1
#>  6 29046            206765        lisinopril 10 MG Oral Tab… SBD               1
#>  7 29046            206766        lisinopril 20 MG Oral Tab… SBD               1
#>  8 29046            206771        lisinopril 40 MG Oral Tab… SBD               1
#>  9 29046            213482        lisinopril 30 MG Oral Tab… SBD               1
#> 10 29046            823971        hydrochlorothiazide 25 MG… SBD               2
```
