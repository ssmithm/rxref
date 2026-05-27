
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rxref

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/rxref)](https://CRAN.R-project.org/package=rxref)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/rxref)](https://cran.r-project.org/package=rxref)
[![R-CMD-check](https://github.com/ssmithm/rxref/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ssmithm/rxref/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

**rxref** is an R package for working with medication identifiers from
the National Library of Medicine’s RxNorm and RxClass APIs. It provides
tidy, vectorized helpers for resolving medication names to RxCUIs,
expanding ingredients to product concepts, mapping between RxCUIs and
NDCs, retrieving clinical product attributes, and querying selected drug
class relationships.

The package was designed for pharmacoepidemiologic and health services
research workflows where medication exposure definitions often need to
move between ingredient names, RxNorm concepts, National Drug Codes
(NDCs), routes, dose forms, and therapeutic classes.

**rxref** was originally built to support the [UF
CVmedLab](https://cvmedlab.org/), where we often need reproducible,
transparent medication definitions for prescriptions, pharmacy
dispensings, and claims-based drug exposure measures.

## What rxref can do

Common workflows include:

- resolve free-text medication names to RxNorm ingredient concepts with
  `find_ingredients()` and `resolve()`;
- expand ingredient RxCUIs to product RxCUIs with
  `products_for_ingredients()`;
- include active-only or active plus historical RxNorm concepts using
  `concept_status`;
- map between RxCUIs and NDCs with `map_to()`, `map_rxcui_to_ndc()`, and
  `map_ndc_to_rxcui()`;
- retrieve product metadata with `get_properties()` and
  `get_clinical_attributes()`;
- filter products by route with `filter_products_by_route()`;
- query selected RxClass relationships with helpers such as `get_atc()`,
  `get_epc()`, and `get_medrt()`;
- use predefined RxNorm term type sets with `product_ttys()` and
  `tty_catalogue()`.

## Installation

You can install the released version of `rxref` from CRAN:

``` r
install.packages("rxref")
```

You can install the development version of `rxref` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ssmithm/rxref")
```

## Basic Workflow: ingredients to products

This is a basic example for querying product RxCUIs associated with the
ingredient lisinopril (a blood pressure-lowering drug):

``` r
library(rxref)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

lisinopril_ing <- find_ingredients("lisinopril")

lisinopril_products <- products_for_ingredients(
  lisinopril_ing$rxcui,
  ttys = product_ttys("default"),
  include_combos = TRUE
)

# Resulting table 
lisinopril_products |> 
  dplyr::mutate(name = paste0("`", name, "`")) |> 
  knitr::kable()
```

| ingredient_rxcui | product_rxcui | name | tty | n_ingredients |
|:---|:---|:---|:---|---:|
| 29046 | 104375 | `lisinopril 2.5 MG Oral Tablet [Zestril]` | SBD | 1 |
| 29046 | 104376 | `lisinopril 5 MG Oral Tablet [Zestril]` | SBD | 1 |
| 29046 | 104377 | `lisinopril 10 MG Oral Tablet [Zestril]` | SBD | 1 |
| 29046 | 104378 | `lisinopril 20 MG Oral Tablet [Zestril]` | SBD | 1 |
| 29046 | 1806890 | `lisinopril 1 MG/ML Oral Solution [Qbrelis]` | SBD | 1 |
| 29046 | 206765 | `lisinopril 10 MG Oral Tablet [Prinivil]` | SBD | 1 |
| 29046 | 206766 | `lisinopril 20 MG Oral Tablet [Prinivil]` | SBD | 1 |
| 29046 | 206771 | `lisinopril 40 MG Oral Tablet [Zestril]` | SBD | 1 |
| 29046 | 213482 | `lisinopril 30 MG Oral Tablet [Zestril]` | SBD | 1 |
| 29046 | 823971 | `hydrochlorothiazide 25 MG / lisinopril 20 MG Oral Tablet [Zestoretic]` | SBD | 2 |
| 29046 | 823982 | `hydrochlorothiazide 12.5 MG / lisinopril 20 MG Oral Tablet [Zestoretic]` | SBD | 2 |
| 29046 | 823986 | `hydrochlorothiazide 12.5 MG / lisinopril 10 MG Oral Tablet [Zestoretic]` | SBD | 2 |
| 29046 | 1806884 | `lisinopril 1 MG/ML Oral Solution` | SCD | 1 |
| 29046 | 197884 | `lisinopril 40 MG Oral Tablet` | SCD | 1 |
| 29046 | 197885 | `hydrochlorothiazide 12.5 MG / lisinopril 10 MG Oral Tablet` | SCD | 2 |
| 29046 | 197886 | `hydrochlorothiazide 12.5 MG / lisinopril 20 MG Oral Tablet` | SCD | 2 |
| 29046 | 197887 | `hydrochlorothiazide 25 MG / lisinopril 20 MG Oral Tablet` | SCD | 2 |
| 29046 | 205326 | `lisinopril 30 MG Oral Tablet` | SCD | 1 |
| 29046 | 311353 | `lisinopril 2.5 MG Oral Tablet` | SCD | 1 |
| 29046 | 311354 | `lisinopril 5 MG Oral Tablet` | SCD | 1 |
| 29046 | 314076 | `lisinopril 10 MG Oral Tablet` | SCD | 1 |
| 29046 | 314077 | `lisinopril 20 MG Oral Tablet` | SCD | 1 |

``` r


# Count of TTYs in resulting table
lisinopril_products |> 
  count(tty, n_ingredients, sort=TRUE)
#> # A tibble: 4 × 3
#>   tty   n_ingredients     n
#>   <chr>         <int> <int>
#> 1 SBD               1     9
#> 2 SCD               1     7
#> 3 SBD               2     3
#> 4 SCD               2     3
```

## Active and historical RxNorm concepts

By default, `products_for_ingredients()` searches active RxNorm
concepts:

``` r
products_for_ingredients(
  "29046",
  ttys = product_ttys("extended_product"),
  concept_status = "active"
) |> knitr::kable()
```

| ingredient_rxcui | product_rxcui | name | tty | n_ingredients |
|:---|:---|:---|:---|---:|
| 29046 | 1806883 | lisinopril Oral Solution | SCDF | 1 |
| 29046 | 372613 | hydrochlorothiazide / lisinopril Oral Tablet | SCDF | 2 |
| 29046 | 372614 | lisinopril Oral Tablet | SCDF | 1 |
| 29046 | 1162124 | hydrochlorothiazide / lisinopril Oral Product | SCDG | 2 |
| 29046 | 1162125 | hydrochlorothiazide / lisinopril Pill | SCDG | 2 |
| 29046 | 1164689 | lisinopril Oral Product | SCDG | 1 |
| 29046 | 1164690 | lisinopril Pill | SCDG | 1 |
| 29046 | 1806882 | lisinopril Oral Liquid Product | SCDG | 1 |
| 29046 | 104375 | lisinopril 2.5 MG Oral Tablet \[Zestril\] | SBD | 1 |
| 29046 | 104376 | lisinopril 5 MG Oral Tablet \[Zestril\] | SBD | 1 |
| 29046 | 104377 | lisinopril 10 MG Oral Tablet \[Zestril\] | SBD | 1 |
| 29046 | 104378 | lisinopril 20 MG Oral Tablet \[Zestril\] | SBD | 1 |
| 29046 | 1806890 | lisinopril 1 MG/ML Oral Solution \[Qbrelis\] | SBD | 1 |
| 29046 | 206765 | lisinopril 10 MG Oral Tablet \[Prinivil\] | SBD | 1 |
| 29046 | 206766 | lisinopril 20 MG Oral Tablet \[Prinivil\] | SBD | 1 |
| 29046 | 206771 | lisinopril 40 MG Oral Tablet \[Zestril\] | SBD | 1 |
| 29046 | 213482 | lisinopril 30 MG Oral Tablet \[Zestril\] | SBD | 1 |
| 29046 | 823971 | hydrochlorothiazide 25 MG / lisinopril 20 MG Oral Tablet \[Zestoretic\] | SBD | 2 |
| 29046 | 823982 | hydrochlorothiazide 12.5 MG / lisinopril 20 MG Oral Tablet \[Zestoretic\] | SBD | 2 |
| 29046 | 823986 | hydrochlorothiazide 12.5 MG / lisinopril 10 MG Oral Tablet \[Zestoretic\] | SBD | 2 |
| 29046 | 1806887 | lisinopril Oral Solution \[Qbrelis\] | SBDF | 1 |
| 29046 | 368248 | lisinopril Oral Tablet \[Zestril\] | SBDF | 1 |
| 29046 | 368865 | lisinopril Oral Tablet \[Prinivil\] | SBDF | 1 |
| 29046 | 823970 | hydrochlorothiazide / lisinopril Oral Tablet \[Zestoretic\] | SBDF | 2 |
| 29046 | 1178275 | Prinivil Oral Product | SBDG | 1 |
| 29046 | 1178276 | Prinivil Pill | SBDG | 1 |
| 29046 | 1187277 | Zestoretic Oral Product | SBDG | 2 |
| 29046 | 1187278 | Zestoretic Pill | SBDG | 2 |
| 29046 | 1187283 | Zestril Oral Product | SBDG | 1 |
| 29046 | 1187284 | Zestril Pill | SBDG | 1 |
| 29046 | 1806888 | Qbrelis Oral Liquid Product | SBDG | 1 |
| 29046 | 1806889 | Qbrelis Oral Product | SBDG | 1 |
| 29046 | 1806884 | lisinopril 1 MG/ML Oral Solution | SCD | 1 |
| 29046 | 197884 | lisinopril 40 MG Oral Tablet | SCD | 1 |
| 29046 | 197885 | hydrochlorothiazide 12.5 MG / lisinopril 10 MG Oral Tablet | SCD | 2 |
| 29046 | 197886 | hydrochlorothiazide 12.5 MG / lisinopril 20 MG Oral Tablet | SCD | 2 |
| 29046 | 197887 | hydrochlorothiazide 25 MG / lisinopril 20 MG Oral Tablet | SCD | 2 |
| 29046 | 205326 | lisinopril 30 MG Oral Tablet | SCD | 1 |
| 29046 | 311353 | lisinopril 2.5 MG Oral Tablet | SCD | 1 |
| 29046 | 311354 | lisinopril 5 MG Oral Tablet | SCD | 1 |
| 29046 | 314076 | lisinopril 10 MG Oral Tablet | SCD | 1 |
| 29046 | 314077 | lisinopril 20 MG Oral Tablet | SCD | 1 |

For older prescribing (or dispensing) data, users may need products that
are no longer active in the current RxNorm release. Set
`concept_status = "active_and_historical"` to include active concepts
plus selected historical statuses:

``` r
products_for_ingredients(
  "1369",
  ttys = "SCD",
  include_combos = FALSE,
  concept_status = "active_and_historical"
) |> knitr::kable()
```

| ingredient_rxcui | product_rxcui | name | tty | n_ingredients | concept_status | active_start_date | active_end_date | release_start_date | release_end_date |
|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|
| 1369 | 197394 | bendroflumethiazide 5 MG Oral Tablet | SCD | 1 | Active | NA | NA | NA | NA |
| 1369 | 308614 | bendroflumethiazide 2.5 MG Oral Tablet | SCD | 1 | Active | NA | NA | NA | NA |
| 1369 | 197393 | bendroflumethiazide 10 MG Oral Tablet | SCD | 1 | Obsolete | 042005 | 062014 | 042005 |  |
| 1369 | 308617 | bendroflumethiazide 5 MG Extended Release Oral Tablet | SCD | 1 | Obsolete | 042005 | 092010 | 042005 |  |
| 1369 | 429502 | bendroflumethiazide 1.25 MG Oral Tablet | SCD | 1 | Obsolete | 042005 | 072005 | 042005 |  |

Historical searches return additional status and date fields, including
concept_status, active_start_date, active_end_date, release_start_date,
and release_end_date.

**Note**: Historical searches are slower than active-only searches
because they require broader RxNorm status lookups. `rxref` caches
repeated API calls where possible, but active-only queries should be
preferred when historical concepts are not needed.

## Clinical product attributes and route filtering

`get_clinical_attributes()` retrieves product-level attributes such as
ingredients, dose form, dose form group, route, strength, and ingredient
count.

``` r
attrs <- get_clinical_attributes(lisinopril_products$product_rxcui)

attrs |>
  select(rxcui, related_rxcui, name, strength, dose_form, route, tty)
#> # A tibble: 22 × 7
#>    rxcui   related_rxcui name                     strength dose_form route tty  
#>    <chr>   <chr>         <chr>                    <chr>    <chr>     <chr> <chr>
#>  1 104375  104375        lisinopril 2.5 MG Oral … 2.5 MG   Oral Tab… ORAL  SBD  
#>  2 104376  104376        lisinopril 5 MG Oral Ta… 5 MG     Oral Tab… ORAL  SBD  
#>  3 104377  104377        lisinopril 10 MG Oral T… 10 MG    Oral Tab… ORAL  SBD  
#>  4 104378  104378        lisinopril 20 MG Oral T… 20 MG    Oral Tab… ORAL  SBD  
#>  5 1806890 1806890       lisinopril 1 MG/ML Oral… 1 MG/ML  Oral Sol… ORAL  SBD  
#>  6 206765  206765        lisinopril 10 MG Oral T… 10 MG    Oral Tab… ORAL  SBD  
#>  7 206766  206766        lisinopril 20 MG Oral T… 20 MG    Oral Tab… ORAL  SBD  
#>  8 206771  206771        lisinopril 40 MG Oral T… 40 MG    Oral Tab… ORAL  SBD  
#>  9 213482  213482        lisinopril 30 MG Oral T… 30 MG    Oral Tab… ORAL  SBD  
#> 10 823971  823971        hydrochlorothiazide 25 … 25 MG /… Oral Tab… ORAL  SBD  
#> # ℹ 12 more rows
```

Products can also be filtered by route:

``` r
oral_lisinopril <- filter_products_by_route(
  lisinopril_products,
  route = "ORAL"
)

oral_lisinopril 
#> # A tibble: 22 × 8
#>    ingredient_rxcui product_rxcui name       tty   n_ingredients route dose_form
#>    <chr>            <chr>         <chr>      <chr>         <int> <chr> <chr>    
#>  1 29046            104375        lisinopri… SBD               1 ORAL  Oral Tab…
#>  2 29046            104376        lisinopri… SBD               1 ORAL  Oral Tab…
#>  3 29046            104377        lisinopri… SBD               1 ORAL  Oral Tab…
#>  4 29046            104378        lisinopri… SBD               1 ORAL  Oral Tab…
#>  5 29046            1806890       lisinopri… SBD               1 ORAL  Oral Sol…
#>  6 29046            206765        lisinopri… SBD               1 ORAL  Oral Tab…
#>  7 29046            206766        lisinopri… SBD               1 ORAL  Oral Tab…
#>  8 29046            206771        lisinopri… SBD               1 ORAL  Oral Tab…
#>  9 29046            213482        lisinopri… SBD               1 ORAL  Oral Tab…
#> 10 29046            823971        hydrochlo… SBD               2 ORAL  Oral Tab…
#> # ℹ 12 more rows
#> # ℹ 1 more variable: dose_form_group <chr>
```

## Mapping between RxCUIs and NDCs

RxCUIs can be mapped to NDCs (and vice-versa):

``` r
ndcs <- map_rxcui_to_ndc("314077")

ndcs
#> # A tibble: 249 × 3
#>    rxcui  ndc11       ndc_status
#>    <chr>  <chr>       <chr>     
#>  1 314077 00143971301 ACTIVE    
#>  2 314077 00143971310 ACTIVE    
#>  3 314077 00185062001 ACTIVE    
#>  4 314077 00185062010 ACTIVE    
#>  5 314077 00378207501 ACTIVE    
#>  6 314077 00378207510 ACTIVE    
#>  7 314077 00591040801 ACTIVE    
#>  8 314077 00591040810 ACTIVE    
#>  9 314077 00615825505 ACTIVE    
#> 10 314077 00615825530 ACTIVE    
#> # ℹ 239 more rows
```

The direction-agnostic `map_to()` function can be used when you want to
map between identifier types with one interface:

``` r
map_to("314077", to = "ndc") 
#> # A tibble: 249 × 3
#>    rxcui  ndc11       ndc_status
#>    <chr>  <chr>       <chr>     
#>  1 314077 00143971301 ACTIVE    
#>  2 314077 00143971310 ACTIVE    
#>  3 314077 00185062001 ACTIVE    
#>  4 314077 00185062010 ACTIVE    
#>  5 314077 00378207501 ACTIVE    
#>  6 314077 00378207510 ACTIVE    
#>  7 314077 00591040801 ACTIVE    
#>  8 314077 00591040810 ACTIVE    
#>  9 314077 00615825505 ACTIVE    
#> 10 314077 00615825530 ACTIVE    
#> # ℹ 239 more rows
```

## Drug class relationships

`rxref` includes selected helpers for querying RxClass relationships,
including ATC, EPC, and MED-RT classes.

``` r
get_atc("29046")
#> # A tibble: 1 × 10
#>   input rxcui drug_name  drug_tty class_id class_name class_type class_url rela 
#>   <chr> <chr> <chr>      <chr>    <chr>    <chr>      <chr>      <chr>     <chr>
#> 1 29046 29046 lisinopril IN       C09AA    ACE inhib… ATC1-4     <NA>      ""   
#> # ℹ 1 more variable: rela_source <chr>
get_epc("29046")
#> # A tibble: 2 × 10
#>   input rxcui  drug_name drug_tty class_id class_name class_type class_url rela 
#>   <chr> <chr>  <chr>     <chr>    <chr>    <chr>      <chr>      <chr>     <chr>
#> 1 29046 15460… lisinopr… PIN      N000017… Angiotens… EPC        <NA>      has_…
#> 2 29046 15460… lisinopr… PIN      N000017… Angiotens… EPC        <NA>      has_…
#> # ℹ 1 more variable: rela_source <chr>
get_medrt("29046")
#> # A tibble: 43 × 10
#>    input rxcui drug_name drug_tty class_id class_name class_type class_url rela 
#>    <chr> <chr> <chr>     <chr>    <chr>    <chr>      <chr>      <chr>     <chr>
#>  1 29046 1546… lisinopr… PIN      D000799  Angioedema DISEASE    <NA>      ci_w…
#>  2 29046 1546… lisinopr… PIN      D003928  Diabetic … DISEASE    <NA>      may_…
#>  3 29046 1546… lisinopr… PIN      D003928  Diabetic … DISEASE    <NA>      may_…
#>  4 29046 1546… lisinopr… PIN      D004342  Drug Hype… DISEASE    <NA>      ci_w…
#>  5 29046 1546… lisinopr… PIN      D006333  Heart Fai… DISEASE    <NA>      may_…
#>  6 29046 1546… lisinopr… PIN      D006929  Hyperaldo… DISEASE    <NA>      ci_w…
#>  7 29046 1546… lisinopr… PIN      D006973  Hypertens… DISEASE    <NA>      may_…
#>  8 29046 1546… lisinopr… PIN      D007022  Hypotensi… DISEASE    <NA>      ci_w…
#>  9 29046 1546… lisinopr… PIN      D009203  Myocardia… DISEASE    <NA>      may_…
#> 10 29046 1546… lisinopr… PIN      D011247  Pregnancy  DISEASE    <NA>      ci_w…
#> # ℹ 33 more rows
#> # ℹ 1 more variable: rela_source <chr>
```

## API use and caching

`rxref` queries public RxNorm and RxClass APIs. The package includes
configurable rate-delay and caching behavior to reduce repeated API
requests and to support longer medication-definition workflows. Note
that RxNorm indicates a rate of [no more than 20 API requests per second
per IP](https://lhncbc.nlm.nih.gov/RxNav/TermsofService.html).

``` r
rxref_conf()
#> $base_url
#> [1] "https://rxnav.nlm.nih.gov/REST"
#> 
#> $rate_delay
#> [1] 0.1
#> 
#> $cache
#> <cache_mem> <cachem>
#>   Methods:
#>     get(key, missing = missing_)
#>     set(key, value)
#>     exists(key)
#>     keys()
#>     remove(key)
#>     reset()
#>     prune()
#>     size()
#>     info()
rxref_conf(rate_delay = 0.1)
#> $base_url
#> [1] "https://rxnav.nlm.nih.gov/REST"
#> 
#> $rate_delay
#> [1] 0.1
#> 
#> $cache
#> <cache_mem> <cachem>
#>   Methods:
#>     get(key, missing = missing_)
#>     set(key, value)
#>     exists(key)
#>     keys()
#>     remove(key)
#>     reset()
#>     prune()
#>     size()
#>     info()
```

## Notes

RxNorm concepts and NDC mappings can change over time. For reproducible
research, consider recording the date of API access and retaining
intermediate drug-definition outputs used in analyses.

`rxref` is intended to support research workflows and
medication-definition development. Users should review outputs carefully
before using them in any high-stakes regulatory, clinical, or production
settings.

## Funding

Development of `rxref` was supported in part by the U.S. National Heart,
Lung and Blood Institute (NHLBI), grants K01HL138172 and R01HL171387.
The content is solely the responsibility of the author and does not
necessarily represent the official views of the National Institutes of
Health.
