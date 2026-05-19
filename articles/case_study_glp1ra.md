# Case Study: GLP-1 and related incretin therapies

## Case

Suppose we need to identify users of GLP-1 receptor agonists and related
incretin-based therapies from EHR prescribing data, pharmacy claims
data, or both. To accomplish this, we need a medication list that
includes relevant RxNorm product concepts and, when available,
corresponding NDCs.

This vignette walks through two approaches:

1.  a transparent step-by-step workflow that starts with a curated list
    of ingredient names; and
2.  a compact workflow using
    [`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md).

The examples use precomputed data by default so the vignette can be
built without querying the live RxNorm API. To rebuild the examples with
live API calls, set:

``` r

Sys.setenv(RXREF_BUILD_VIGNETTES_ONLINE = "true")
```

## Defining the ingredient list

For this example, we start with a prespecified list of GLP-1 receptor
agonists and related incretin-based therapies:

- exenatide
- liraglutide
- lixisenatide
- dulaglutide
- albiglutide
- semaglutide
- tirzepatide

Tirzepatide is included here because many applied studies group it with
GLP-1-based incretin therapies, although it is a dual GIP/GLP-1 receptor
agonist rather than a GLP-1 receptor agonist alone.

``` r

glp1.names <- c(
  "semaglutide",
  "exenatide",
  "liraglutide",
  "lixisenatide",
  "dulaglutide",
  "albiglutide",
  "tirzepatide"
)
```

## Option 1: Step-by-step medication list construction

### Identify ingredient RxCUIs

First, we use
[`find_ingredients()`](https://www.stevenmsmith.org/rxref/reference/find_ingredients.md)
to identify ingredient-level RxCUIs. For this example, we retain
concepts with TTY = `"IN"`, corresponding to RxNorm ingredient concepts.
You can see available TTY values and descriptions with
[`tty_catalogue()`](https://www.stevenmsmith.org/rxref/reference/tty_catalogue.md).

``` r

if (run_live) {
  glp1.ings <- find_ingredients(glp1.names) |>
    filter(tty == "IN") |>
    distinct(
      input,
      ingredient_rxcui = rxcui,
      ingredient_name = name,
      ingredient_tty = tty
    )
} else {
  glp1.ings <- read_rxref_example("glp1_ings.rds")
}

glp1.ings
#> # A tibble: 7 × 4
#>   input        ingredient_rxcui ingredient_name ingredient_tty
#>   <chr>        <chr>            <chr>           <chr>         
#> 1 albiglutide  1534763          albiglutide     IN            
#> 2 dulaglutide  1551291          dulaglutide     IN            
#> 3 exenatide    60548            exenatide       IN            
#> 4 liraglutide  475968           liraglutide     IN            
#> 5 lixisenatide 1440051          lixisenatide    IN            
#> 6 semaglutide  1991302          semaglutide     IN            
#> 7 tirzepatide  2601723          tirzepatide     IN
```

### Expand ingredients to product RxCUIs

Next, we use
[`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
to identify product concepts related to the ingredient RxCUIs.

By default, this workflow focuses on active RxNorm concepts. This is
usually appropriate for current medication list construction. For
studies covering older calendar periods, users may want to include
historical RxNorm concepts as well.

``` r

product_ttys("default")
#> [1] "SCD"  "SBD"  "GPCK" "BPCK"
product_ttys("extended_product")
#>  [1] "SCD"   "SBD"   "GPCK"  "BPCK"  "SCDG"  "SBDG"  "SCDF"  "SBDF"  "SBDFP"
#> [10] "SCDFP" "SCDGP"
```

The default product TTY set is intended to capture product concepts that
are commonly useful for medication list construction and NDC mapping. A
broader product-related set is available with
`product_ttys("extended_product")`. Users can also supply their own
character vector of TTYs.

In this example, we include combination products. That means products
containing a GLP-1-related ingredient plus one or more other ingredients
may be retained.

``` r

if (run_live) {
  glp1.prods <- products_for_ingredients(
    glp1.ings$ingredient_rxcui,
    ttys = product_ttys("default"),
    include_combos = TRUE,
    concept_status = "active"
  )
} else {
  glp1.prods <- read_rxref_example("glp1_prods.rds")
}

glp1.prods |>
  head(30)
#> # A tibble: 30 × 5
#>    ingredient_rxcui product_rxcui name                       tty   n_ingredients
#>    <chr>            <chr>         <chr>                      <chr>         <int>
#>  1 1440051          1858995       3 ML insulin glargine 100… SCD               2
#>  2 1440051          1859000       3 ML insulin glargine 100… SBD               2
#>  3 1534763          1534800       0.5 ML albiglutide 60 MG/… SCD               1
#>  4 1534763          1534820       0.5 ML albiglutide 100 MG… SCD               1
#>  5 1551291          1551295       0.5 ML dulaglutide 1.5 MG… SCD               1
#>  6 1551291          1551300       0.5 ML dulaglutide 1.5 MG… SBD               1
#>  7 1551291          1551304       0.5 ML dulaglutide 3 MG/M… SCD               1
#>  8 1551291          1551306       0.5 ML dulaglutide 3 MG/M… SBD               1
#>  9 1551291          2395777       0.5 ML dulaglutide 6 MG/M… SCD               1
#> 10 1551291          2395779       0.5 ML dulaglutide 6 MG/M… SBD               1
#> # ℹ 20 more rows
```

Combination products may appear when `include_combos = TRUE`. Depending
on the function, ingredient fields for combination products may be
summarized across ingredients, while product-level rows remain one row
per product concept. Users should inspect ingredient fields and
`n_ingredients` before deciding whether to include or exclude fixed-dose
combination products.

### Include historical RxNorm concepts when needed

For historical studies, formulary reconstruction, or claims data
spanning older calendar periods, users may want to include both active
and historical RxNorm concepts.

``` r

glp1.prods_historical <- products_for_ingredients(
  glp1.ings$ingredient_rxcui,
  ttys = product_ttys("default"),
  include_combos = TRUE,
  concept_status = "active_and_historical"
)
```

Historical concepts can be useful when reconstructing medication
exposure during older study periods. However, some historical concepts
may have less complete clinical attribute information than active
concepts. Users should review route, dose form, ingredient count, and
NDC mappings carefully when including historical concepts.

### Map product RxCUIs to NDCs

Next, we identify NDCs associated with the product RxCUIs. Not all
RxCUIs map to NDCs, so some product concepts may not have corresponding
NDC values.

``` r

if (run_live) {
  glp1.ndc.map <- map_rxcui_to_ndc(
    unique(glp1.prods$product_rxcui),
    status = "ACTIVE"
  )
} else {
  glp1.ndc.map <- read_rxref_example("glp1_ndc_map.rds")
}

glp1.ndcs <- glp1.ndc.map |>
  left_join(
    glp1.prods,
    by = c("rxcui" = "product_rxcui")
  ) |>
  left_join(
    glp1.ings |>
      select(ingredient_rxcui, ingredient_name),
    by = "ingredient_rxcui"
  ) |>
  distinct(
    ingredient_rxcui,
    ingredient_name,
    product_rxcui = rxcui,
    ndc11,
    ndc_status,
    name,
    tty
  ) |>
  arrange(ingredient_name, product_rxcui, ndc11)

glp1.ndcs |>
  head(30)
#> # A tibble: 30 × 7
#>    ingredient_rxcui ingredient_name product_rxcui ndc11   ndc_status name  tty  
#>    <chr>            <chr>           <chr>         <chr>   <chr>      <chr> <chr>
#>  1 1551291          dulaglutide     1551300       000021… ACTIVE     0.5 … SBD  
#>  2 1551291          dulaglutide     1551300       000021… ACTIVE     0.5 … SBD  
#>  3 1551291          dulaglutide     1551300       000021… ACTIVE     0.5 … SBD  
#>  4 1551291          dulaglutide     1551300       500903… ACTIVE     0.5 … SBD  
#>  5 1551291          dulaglutide     1551300       500906… ACTIVE     0.5 … SBD  
#>  6 1551291          dulaglutide     1551306       000021… ACTIVE     0.5 … SBD  
#>  7 1551291          dulaglutide     1551306       000021… ACTIVE     0.5 … SBD  
#>  8 1551291          dulaglutide     1551306       000021… ACTIVE     0.5 … SBD  
#>  9 1551291          dulaglutide     1551306       500903… ACTIVE     0.5 … SBD  
#> 10 1551291          dulaglutide     1551306       500906… ACTIVE     0.5 … SBD  
#> # ℹ 20 more rows
```

At this point, we have a product-level and NDC-level medication list
that can be used to query EHR prescribing data, pharmacy dispensing
data, or pharmacy claims data.

## Option 2: Use `search_drug()` for a compact workflow

The same goal can often be accomplished in one step with
[`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md).
This function combines ingredient searching, product expansion, optional
route filtering, and optional NDC mapping.

Suppose we want NDCs for the same ingredient list, and we want to
include active, obsolete, and unspecified NDCs.

``` r

if (run_live) {
  alt.glp1.ndcs <- search_drug(
    term = glp1.names,
    return = "ndc",
    concept_status = "active",
    ndc_status = c("ACTIVE", "OBSOLETE", "UNSPECIFIED")
  )
} else {
  alt.glp1.ndcs <- read_rxref_example("alt_glp1_ndc.rds")
}

alt.glp1.ndcs |>
  arrange(ingredient_name, product_rxcui, ndc11) |>
  head(30)
#> # A tibble: 30 × 7
#>    ingredient_rxcui ingredient_name product_rxcui product_name product_tty ndc11
#>    <chr>            <chr>           <chr>         <chr>        <chr>       <chr>
#>  1 1551291          dulaglutide     1551300       0.5 ML dula… SBD         0000…
#>  2 1551291          dulaglutide     1551300       0.5 ML dula… SBD         0000…
#>  3 1551291          dulaglutide     1551300       0.5 ML dula… SBD         0000…
#>  4 1551291          dulaglutide     1551300       0.5 ML dula… SBD         5009…
#>  5 1551291          dulaglutide     1551300       0.5 ML dula… SBD         5009…
#>  6 1551291          dulaglutide     1551306       0.5 ML dula… SBD         0000…
#>  7 1551291          dulaglutide     1551306       0.5 ML dula… SBD         0000…
#>  8 1551291          dulaglutide     1551306       0.5 ML dula… SBD         0000…
#>  9 1551291          dulaglutide     1551306       0.5 ML dula… SBD         5009…
#> 10 1551291          dulaglutide     1551306       0.5 ML dula… SBD         5009…
#> # ℹ 20 more rows
#> # ℹ 1 more variable: ndc_status <chr>
```

Here, `concept_status` controls whether active or historical RxNorm
concepts are considered. The `ndc_status` argument controls which NDC
status categories are returned.

For example, to include historical RxNorm concepts and broader NDC
status categories, use:

``` r

search_drug(
  term = glp1.names,
  return = "ndc",
  concept_status = "active_and_historical",
  ndc_status = c("ACTIVE", "OBSOLETE", "UNSPECIFIED")
)
```

This can be useful for studies spanning older calendar periods, but
users should carefully inspect the resulting concepts and NDCs before
finalizing an exposure definition.

## Comparing the two approaches

The step-by-step approach is more verbose, but it makes each decision
explicit:

1.  identify ingredient RxCUIs;
2.  expand ingredients to product concepts;
3.  decide whether to include combination products;
4.  decide whether to include active concepts only or active and
    historical concepts;
5.  map product RxCUIs to NDCs.

The
[`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md)
approach is more compact and is useful for common workflows where users
want a product list or NDC list from one or more drug names.

To compare the NDCs from the step-by-step workflow against the compact
workflow:

``` r

glp1.ndcs |>
  filter(!is.na(ndc11)) |>
  arrange(ingredient_name, product_rxcui, ndc11) |>
  head(30)
#> # A tibble: 30 × 7
#>    ingredient_rxcui ingredient_name product_rxcui ndc11   ndc_status name  tty  
#>    <chr>            <chr>           <chr>         <chr>   <chr>      <chr> <chr>
#>  1 1551291          dulaglutide     1551300       000021… ACTIVE     0.5 … SBD  
#>  2 1551291          dulaglutide     1551300       000021… ACTIVE     0.5 … SBD  
#>  3 1551291          dulaglutide     1551300       000021… ACTIVE     0.5 … SBD  
#>  4 1551291          dulaglutide     1551300       500903… ACTIVE     0.5 … SBD  
#>  5 1551291          dulaglutide     1551300       500906… ACTIVE     0.5 … SBD  
#>  6 1551291          dulaglutide     1551306       000021… ACTIVE     0.5 … SBD  
#>  7 1551291          dulaglutide     1551306       000021… ACTIVE     0.5 … SBD  
#>  8 1551291          dulaglutide     1551306       000021… ACTIVE     0.5 … SBD  
#>  9 1551291          dulaglutide     1551306       500903… ACTIVE     0.5 … SBD  
#> 10 1551291          dulaglutide     1551306       500906… ACTIVE     0.5 … SBD  
#> # ℹ 20 more rows
```

## Choosing active versus historical concepts

For many current medication lists, `concept_status = "active"` is a good
default. This limits the workflow to active RxNorm concepts.

Historical concepts may be appropriate when:

- the study period includes older calendar years;
- medication exposure is being reconstructed from historical claims;
- users need to capture products that may no longer be active in RxNorm;
- obsolete NDCs are intentionally included.

However, active and historical RxNorm concepts should not be confused
with NDC status. These are separate choices:

- `concept_status` controls which RxNorm concepts are considered.
- `ndc_status` controls which NDC status categories are returned.

For example:

``` r

search_drug(
  term = "semaglutide",
  return = "ndc",
  concept_status = "active_and_historical",
  ndc_status = c("ACTIVE", "OBSOLETE", "UNSPECIFIED")
)
```

## Practical considerations

Medication list construction often requires study-specific decisions.
Before using the resulting list in an analysis, users should consider:

- whether to include fixed-dose combination products;
- whether to include branded products, clinical products, packs, or all
  product-related TTYs;
- whether the study period requires historical RxNorm concepts;
- whether active NDCs only are sufficient, or obsolete/unspecified NDCs
  should also be included;
- whether route, dose form, or strength restrictions are needed;
- whether the final list should be reviewed clinically.

For strict reproducibility, users should save the final product list,
NDC list, and package/API versions used to construct the medication
exposure definition.
