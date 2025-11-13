# Case Study: GLP1 receptor agonists

## Case

Suppose we have a need to identify all glucagon-like peptide-1 receptor
agonist (GLP1-RA) users from either EHR prescribing data, or pharmacy
claims data (or both). To accomplish this, we need a complete list of
RxCUIs and their corresponding NDCs (when available).

## The Set-up

First, we need to clarify the set of drugs that are considered part of
the GLP1-RA class. A quick google search will give us the list of
GLP1-RA agents currently approved for use: - exenatide - liraglutide -
lixisenatide - dulaglutide - albiglutide - semaglutide - tirzepatide

## Identifying Ingredient RxCUIs

Let’s turn this into a character vector and use
[`find_ingredients()`](http://www.stevenmsmith.org/rxref/reference/find_ingredients.md)
to gather RxCUIs for TTY = ‘IN’ (note: you can get a complete list of
TTYs and their description from
[`tty_catalogue()`](http://www.stevenmsmith.org/rxref/reference/tty_catalogue.md)).

``` r
# remotes::install_github("ssmithm/rxref")
library(rxref)
glp1.names <- c("semaglutide", "exenatide", "liraglutide", "lixisenatide", "dulaglutide", "albiglutide", "tirzepatide")

if (run_live) {
  # run this next line only, if recreating on your own
  glp1.ings <- find_ingredients(glp1.names)
} else {
  glp1.ings <- readRDS(system.file("extdata", "glp1_ings.rds", package = "rxref"))
}

glp1.ings
#> # A tibble: 7 × 5
#>   input        rxcui   name         tty   score
#>   <chr>        <chr>   <chr>        <chr> <dbl>
#> 1 semaglutide  1991302 semaglutide  IN     12.9
#> 2 exenatide    60548   exenatide    IN     13.5
#> 3 liraglutide  475968  liraglutide  IN     13.5
#> 4 lixisenatide 1440051 lixisenatide IN     13.8
#> 5 dulaglutide  1551291 dulaglutide  IN     13.6
#> 6 albiglutide  1534763 albiglutide  IN     14.0
#> 7 tirzepatide  2601723 tirzepatide  IN     12.6
```

## Getting Product RxCUIs from Ingredient RxCUIs

Now, let’s look for all of the related RxCUIs using
`product_from_ingredients()`. Here, we’re going to just focus on the
TTYs that are likely to have NDCs (SCD, SBD, GPCK, BPCK) by using the
function’s default TTY.

There are two default TTY lists:

- **.rxref_default_ttys** = “SCD”, “SBD”, “GPCK”, “BPCK”

- **.rxref_extended_ttys** = “SCD”, “SBD”, “GPCK”, “BPCK”, “SCDC”,
  “SBDC”, “SCDF”, “SBDF”, “SCDFP”, “SBDFP”, “SCDG”, “SCDGP”, “BN”,
  “MIN”, “IN”

But, you could supply your own set of TTYs to include instead of using
these two default lists.

Lastly, we’re going to include combination products (i.e., if they have
a GLP1-RA plus one or more other drugs), again using the function’s
default option of include_combo = TRUE.

``` r
if (run_live) {
  # run this next line only, if recreating on your own
  glp1.prods <- products_for_ingredients(glp1.ings$rxcui)
} else {
  glp1.prods <- readRDS(system.file("extdata", "glp1_prods.rds", package = "rxref"))
}

glp1.prods |> head(30)
#> # A tibble: 30 × 5
#>    ingredient_rxcui product_rxcui name                       tty   n_ingredients
#>    <chr>            <chr>         <chr>                      <chr>         <int>
#>  1 1991302          1991311       0.25 MG, 0.5 MG Dose 1.5 … SBD               1
#>  2 1991302          1991317       1 MG Dose 1.5 ML semaglut… SBD               1
#>  3 1991302          2200650       semaglutide 14 MG Oral Ta… SBD               1
#>  4 1991302          2200654       semaglutide 3 MG Oral Tab… SBD               1
#>  5 1991302          2200658       semaglutide 7 MG Oral Tab… SBD               1
#>  6 1991302          2398842       3 ML semaglutide 1.34 MG/… SBD               1
#>  7 1991302          2553506       0.5 ML semaglutide 0.5 MG… SBD               1
#>  8 1991302          2553603       0.5 ML semaglutide 1 MG/M… SBD               1
#>  9 1991302          2553803       0.5 ML semaglutide 2 MG/M… SBD               1
#> 10 1991302          2553903       0.75 ML semaglutide 2.27 … SBD               1
#> # ℹ 20 more rows
```

## Adding in NDCs from Product RxCUIs

Now, let’s identify the related NDCs for each RxCUI. Note that not all
RxCUIs will link to NDCs, so some NAs will likely show up in the
resulting file.

``` r
if (run_live) {
  # run this next line only, if recreating on your own
  glp1.ndc.map <- map_rxcui_to_ndc(unique(glp1.prods$product_rxcui))
} else {
  glp1.ndc.map <- readRDS(system.file("extdata", "glp1_ndc_map.rds", package = "rxref"))
}

# Join back names/TTYs and de-dupe
glp1_ndcs <- glp1.ndc.map |> 
  dplyr::left_join(glp1.prods, by = c("rxcui" = "product_rxcui")) |> 
  dplyr::distinct(rxcui, ingredient_rxcui, ndc11, name, tty) |> 
  dplyr::arrange(ingredient_rxcui, rxcui, ndc11, name)

glp1.ndcs <- dplyr::left_join(
  x = glp1_ndcs,
  y = glp1.ings |> dplyr::select(rxcui, name) |> dplyr::rename(ingredient = name, ingredient_rxcui = rxcui),
  by = "ingredient_rxcui"
)

glp1.ndcs |> head(30)
#> # A tibble: 30 × 6
#>    rxcui   ingredient_rxcui ndc11       name                    tty   ingredient
#>    <chr>   <chr>            <chr>       <chr>                   <chr> <chr>     
#>  1 1803892 1440051          NA          3 ML lixisenatide 0.05… SCD   lixisenat…
#>  2 1803893 1440051          NA          3 ML lixisenatide 0.05… SBD   lixisenat…
#>  3 1803894 1440051          NA          3 ML lixisenatide 0.1 … SCD   lixisenat…
#>  4 1803896 1440051          00024574702 3 ML lixisenatide 0.1 … SBD   lixisenat…
#>  5 1803902 1440051          NA          {1 (3 ML lixisenatide … GPCK  lixisenat…
#>  6 1803903 1440051          00024574502 {1 (3 ML lixisenatide … BPCK  lixisenat…
#>  7 1858995 1440051          NA          3 ML insulin glargine … SCD   lixisenat…
#>  8 1859000 1440051          00024576101 3 ML insulin glargine … SBD   lixisenat…
#>  9 1859000 1440051          00024576102 3 ML insulin glargine … SBD   lixisenat…
#> 10 1859000 1440051          00024576105 3 ML insulin glargine … SBD   lixisenat…
#> # ℹ 20 more rows
```

Voila, we now have a complete list of GLP1-RAs that can be used to query
against the prescribing data or pharmacy claims data.

## An Alternate Approach

This same goal can be accomplished all in one step, if you prefer, using
the
[`search_drug()`](http://www.stevenmsmith.org/rxref/reference/search_drug.md)
function. Suppose we want to just get NDCs, and we want to make sure
we’re capturing *active*, as well as *obsolete* or *unspecified* NDCs.
That is, we don’t actually care about the RxCUIs that don’t have a
corresponding NDC.

``` r

if (run_live) {
  # run this next line only, if recreating on your own
  alt.glp1.ndcs <- search_drug(term = glp1.names,
                               return = "ndc",
                               ndc_status = c("ACTIVE", "OBSOLETE", "UNSPECIFIED")
                               )
} else {
  alt.glp1.ndcs <- readRDS(system.file("extdata", "alt_glp1_ndc.rds", package = "rxref"))
}

alt.glp1.ndcs1 <- 
  dplyr::left_join(alt.glp1.ndcs, glp1.prods |> select(-ingredient_rxcui), by = c("product_rxcui" = "product_rxcui")) |> 
  dplyr::distinct(product_rxcui, ingredient_rxcui, ndc11, name, tty) |> 
  dplyr::arrange(ingredient_rxcui, product_rxcui, ndc11, name)
alt.glp1.ndcs2 <- dplyr::left_join(
  x = alt.glp1.ndcs1,
  y = glp1.ings |> dplyr::select(rxcui, name) |> dplyr::rename(ingredient = name, ingredient_rxcui = rxcui),
  by = "ingredient_rxcui")
alt.glp1.ndcs2 
#> # A tibble: 150 × 6
#>    product_rxcui ingredient_rxcui ndc11       name              tty   ingredient
#>    <chr>         <chr>            <chr>       <chr>             <chr> <chr>     
#>  1 1803896       1440051          00024574702 3 ML lixisenatid… SBD   lixisenat…
#>  2 1803903       1440051          00024574502 {1 (3 ML lixisen… BPCK  lixisenat…
#>  3 1859000       1440051          00024576101 3 ML insulin gla… SBD   lixisenat…
#>  4 1859000       1440051          00024576102 3 ML insulin gla… SBD   lixisenat…
#>  5 1859000       1440051          00024576105 3 ML insulin gla… SBD   lixisenat…
#>  6 1859000       1440051          00024576302 3 ML insulin gla… SBD   lixisenat…
#>  7 1534805       1534763          00173086602 0.5 ML albigluti… SBD   albigluti…
#>  8 1551300       1551291          00002143301 0.5 ML dulagluti… SBD   dulagluti…
#>  9 1551300       1551291          00002143361 0.5 ML dulagluti… SBD   dulagluti…
#> 10 1551300       1551291          00002143380 0.5 ML dulagluti… SBD   dulagluti…
#> # ℹ 140 more rows
```

Compare that against the original table, filtered for non-NA `ndc11`
values:

``` r
glp1.ndcs |> filter(!is.na(ndc11))
#> # A tibble: 150 × 6
#>    rxcui   ingredient_rxcui ndc11       name                    tty   ingredient
#>    <chr>   <chr>            <chr>       <chr>                   <chr> <chr>     
#>  1 1803896 1440051          00024574702 3 ML lixisenatide 0.1 … SBD   lixisenat…
#>  2 1803903 1440051          00024574502 {1 (3 ML lixisenatide … BPCK  lixisenat…
#>  3 1859000 1440051          00024576101 3 ML insulin glargine … SBD   lixisenat…
#>  4 1859000 1440051          00024576102 3 ML insulin glargine … SBD   lixisenat…
#>  5 1859000 1440051          00024576105 3 ML insulin glargine … SBD   lixisenat…
#>  6 1859000 1440051          00024576302 3 ML insulin glargine … SBD   lixisenat…
#>  7 1534805 1534763          00173086602 0.5 ML albiglutide 60 … SBD   albigluti…
#>  8 1551300 1551291          00002143301 0.5 ML dulaglutide 1.5… SBD   dulagluti…
#>  9 1551300 1551291          00002143361 0.5 ML dulaglutide 1.5… SBD   dulagluti…
#> 10 1551300 1551291          00002143380 0.5 ML dulaglutide 1.5… SBD   dulagluti…
#> # ℹ 140 more rows
```
