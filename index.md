# rxref

**rxref** is an R package that includes a set of tidy utilities for
querying the [National Library of Medicine](https://www.nlm.nih.gov/)’s
[RxNorm](https://www.nlm.nih.gov/research/umls/rxnorm/index.html) API.
RxNorm is a system of normalized names for clinical drugs as well as
linkages from these names to many of the drug vocabularies commonly used
in pharmacy management and drug interaction software, including those of
First Databank, Micromedex, Multum, and Gold Standard Drug Database.

**rxref** was originally built to facilitate pharmacoepidemiologic
research in the [UF CVmedLab](https://cvmedlab.org/), where we often
need to identify medication prescriptions or pharmacy dispensings as a
proxy for medication exposure or outcomes.

The core use case for **rxref** is identifying a set of RxCUIs (Rx
Concept Unique Identifiers) and/or NDCs (national drug codes) that
relate to a single medication or set of medications (i.e., a class of
medications) supplied by the user. Additional functionality (still in
early stage development) will include extracting additional properties
(e.g., strength, dosage form, indications, adverse events, and so on)
related to a user-supplied list of RxCUIs.

## Installation

You can install the development version of rxref from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ssmithm/rxref")
```

## Example

This is a basic example for querying RxCUIs associated with lisinopril
(a blood pressure-lowering drug):

``` r
library(rxref)
lisinopril.ing <- find_ingredients('lisinopril')
lisinopril.ing |> knitr::kable()
```

| input      | rxcui | name       | tty |   score |
|:-----------|:------|:-----------|:----|--------:|
| lisinopril | 29046 | lisinopril | IN  | 11.9192 |

``` r

lisinopril.rxcuis <- products_for_ingredients(lisinopril.ing$rxcui, ttys = c("SCD", "SBD"), include_combos = TRUE)

# Resulting table of all related products with TTY in {SCD, SBD}
lisinopril.rxcuis |> 
  dplyr::mutate(name = paste0("`", name, "`")) |> 
  knitr::kable()
```

| ingredient_rxcui | product_rxcui | name                                                                      | tty | n_ingredients |
|:-----------------|:--------------|:--------------------------------------------------------------------------|:----|--------------:|
| 29046            | 104375        | `lisinopril 2.5 MG Oral Tablet [Zestril]`                                 | SBD |             1 |
| 29046            | 104376        | `lisinopril 5 MG Oral Tablet [Zestril]`                                   | SBD |             1 |
| 29046            | 104377        | `lisinopril 10 MG Oral Tablet [Zestril]`                                  | SBD |             1 |
| 29046            | 104378        | `lisinopril 20 MG Oral Tablet [Zestril]`                                  | SBD |             1 |
| 29046            | 1806890       | `lisinopril 1 MG/ML Oral Solution [Qbrelis]`                              | SBD |             1 |
| 29046            | 206765        | `lisinopril 10 MG Oral Tablet [Prinivil]`                                 | SBD |             1 |
| 29046            | 206766        | `lisinopril 20 MG Oral Tablet [Prinivil]`                                 | SBD |             1 |
| 29046            | 206771        | `lisinopril 40 MG Oral Tablet [Zestril]`                                  | SBD |             1 |
| 29046            | 213482        | `lisinopril 30 MG Oral Tablet [Zestril]`                                  | SBD |             1 |
| 29046            | 823971        | `hydrochlorothiazide 25 MG / lisinopril 20 MG Oral Tablet [Zestoretic]`   | SBD |             2 |
| 29046            | 823982        | `hydrochlorothiazide 12.5 MG / lisinopril 20 MG Oral Tablet [Zestoretic]` | SBD |             2 |
| 29046            | 823986        | `hydrochlorothiazide 12.5 MG / lisinopril 10 MG Oral Tablet [Zestoretic]` | SBD |             2 |
| 29046            | 1806884       | `lisinopril 1 MG/ML Oral Solution`                                        | SCD |             1 |
| 29046            | 197884        | `lisinopril 40 MG Oral Tablet`                                            | SCD |             1 |
| 29046            | 197885        | `hydrochlorothiazide 12.5 MG / lisinopril 10 MG Oral Tablet`              | SCD |             2 |
| 29046            | 197886        | `hydrochlorothiazide 12.5 MG / lisinopril 20 MG Oral Tablet`              | SCD |             2 |
| 29046            | 197887        | `hydrochlorothiazide 25 MG / lisinopril 20 MG Oral Tablet`                | SCD |             2 |
| 29046            | 205326        | `lisinopril 30 MG Oral Tablet`                                            | SCD |             1 |
| 29046            | 311353        | `lisinopril 2.5 MG Oral Tablet`                                           | SCD |             1 |
| 29046            | 311354        | `lisinopril 5 MG Oral Tablet`                                             | SCD |             1 |
| 29046            | 314076        | `lisinopril 10 MG Oral Tablet`                                            | SCD |             1 |
| 29046            | 314077        | `lisinopril 20 MG Oral Tablet`                                            | SCD |             1 |

``` r

# Count of TTYs in resulting table
aggregate(
  x = list(count = rep(1, nrow(lisinopril.rxcuis))), 
  by = list(tty = lisinopril.rxcuis$tty), 
  FUN = sum
) |> knitr::kable()
```

| tty | count |
|:----|------:|
| SBD |    12 |
| SCD |    10 |
