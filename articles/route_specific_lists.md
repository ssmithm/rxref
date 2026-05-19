# Building route-specific RxCUI and NDC lists

## Overview

A common use case for `rxref` is to develop a list of RxNorm product
concepts and National Drug Codes (NDCs) for a drug class.

For example, suppose we want to identify product RxCUIs and active NDCs
for oral beta-blockers. This is not as simple as searching for
beta-blocker ingredients and mapping every product to NDCs, because some
beta-blockers are available through multiple routes.

For example:

- `metoprolol`, `atenolol`, `labetalol`, `propranolol`, and `sotalol`
  may have oral and injectable products;
- `esmolol` is injectable only;
- `timolol`, `betaxolol`, and related drugs may have ophthalmic
  products.

For many pharmacoepidemiologic studies, we may want oral outpatient-use
products only.

This vignette walks through a tidy workflow for:

1.  resolving drug names to ingredient RxCUIs;
2.  expanding ingredient RxCUIs to product RxCUIs;
3.  filtering product RxCUIs by route; and
4.  mapping the filtered product RxCUIs to active NDCs.

The examples use precomputed data by default so the vignette can be
built without querying the live RxNorm API. To rebuild the examples with
live API calls, set:

``` r

Sys.setenv(RXREF_BUILD_VIGNETTES_ONLINE = "true")
```

## Define a beta-blocker ingredient list

In this example, we start with a curated list of beta-blocker ingredient
names.

Users can also use RxClass-related helpers, such as
[`find_classes()`](https://www.stevenmsmith.org/rxref/reference/find_classes.md)
or
[`get_class_members()`](https://www.stevenmsmith.org/rxref/reference/get_class_members.md),
when a suitable source vocabulary is available. However, curated
ingredient lists remain useful because class membership may not
perfectly match a study-specific exposure definition.

``` r

beta_blocker_names <- c(
  "acebutolol",
  "atenolol",
  "betaxolol",
  "bisoprolol",
  "carvedilol",
  "labetalol",
  "metoprolol",
  "nadolol",
  "nebivolol",
  "penbutolol",
  "pindolol",
  "propranolol",
  "sotalol",
  "timolol"
)
```

## Resolve drug names to ingredient RxCUIs

First, use
[`find_ingredients()`](https://www.stevenmsmith.org/rxref/reference/find_ingredients.md)
to resolve each drug name to an RxNorm ingredient concept.

``` r

if (run_live) {
  bb_ingredients <- find_ingredients(beta_blocker_names) |>
    filter(tty == "IN") |>
    distinct(
      input,
      ingredient_rxcui = rxcui,
      ingredient_name = name,
      ingredient_tty = tty
    )
} else {
  bb_ingredients <- read_rxref_example("bb_ingredients.rds")
}

bb_ingredients
#> # A tibble: 14 × 4
#>    input       ingredient_rxcui ingredient_name ingredient_tty
#>    <chr>       <chr>            <chr>           <chr>         
#>  1 acebutolol  149              acebutolol      IN            
#>  2 atenolol    1202             atenolol        IN            
#>  3 betaxolol   1520             betaxolol       IN            
#>  4 bisoprolol  19484            bisoprolol      IN            
#>  5 carvedilol  20352            carvedilol      IN            
#>  6 labetalol   6185             labetalol       IN            
#>  7 metoprolol  6918             metoprolol      IN            
#>  8 nadolol     7226             nadolol         IN            
#>  9 nebivolol   31555            nebivolol       IN            
#> 10 penbutolol  7973             penbutolol      IN            
#> 11 pindolol    8332             pindolol        IN            
#> 12 propranolol 8787             propranolol     IN            
#> 13 sotalol     9947             sotalol         IN            
#> 14 timolol     10600            timolol         IN
```

The resulting table contains one row per resolved ingredient. These
ingredient RxCUIs are the starting point for product expansion.

## Expand ingredients to product RxCUIs

Next, use
[`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
to identify product-level RxNorm concepts containing each ingredient.

In this example, we use active RxNorm concepts and include combination
products.

``` r

if (run_live) {
  bb_products <- products_for_ingredients(
    bb_ingredients$ingredient_rxcui,
    ttys = product_ttys("default"),
    include_combos = TRUE,
    concept_status = "active"
  ) |>
    left_join(bb_ingredients, by = "ingredient_rxcui")
} else {
  bb_products <- read_rxref_example("bb_products.rds")
}

bb_products
#> # A tibble: 219 × 8
#>    ingredient_rxcui product_rxcui name                 tty   n_ingredients input
#>    <chr>            <chr>         <chr>                <chr>         <int> <chr>
#>  1 149              998685        acebutolol 400 MG O… SCD               1 aceb…
#>  2 149              998689        acebutolol 200 MG O… SCD               1 aceb…
#>  3 149              998693        acebutolol 100 MG O… SCD               1 aceb…
#>  4 149              998694        acebutolol 200 MG /… SCD               2 aceb…
#>  5 149              998695        acebutolol 400 MG O… SCD               1 aceb…
#>  6 1202             104308        atenolol 0.5 MG/ML … SCD               1 aten…
#>  7 1202             150750        atenolol 25 MG Oral… SBD               1 aten…
#>  8 1202             152414        atenolol 50 MG Oral… SBD               1 aten…
#>  9 1202             152916        atenolol 50 MG / ch… SCD               2 aten…
#> 10 1202             153155        atenolol 25 MG / be… SCD               2 aten…
#> # ℹ 209 more rows
#> # ℹ 2 more variables: ingredient_name <chr>, ingredient_tty <chr>
```

Because `include_combos = TRUE`, combination products are retained. For
beta-blockers, this means products such as beta-blocker/thiazide
combinations may be included.

To exclude combination products, set `include_combos = FALSE`.

``` r

if (run_live) {
  bb_single_ingredient_products <- products_for_ingredients(
    bb_ingredients$ingredient_rxcui,
    ttys = product_ttys("default"),
    include_combos = FALSE,
    concept_status = "active"
  ) |>
    left_join(bb_ingredients, by = "ingredient_rxcui")
} else {
  bb_single_ingredient_products <- read_rxref_example(
    "bb_single_ingredient_products.rds"
  )
}

bb_single_ingredient_products
#> # A tibble: 177 × 8
#>    ingredient_rxcui product_rxcui name                 tty   n_ingredients input
#>    <chr>            <chr>         <chr>                <chr>         <int> <chr>
#>  1 149              998685        acebutolol 400 MG O… SCD               1 aceb…
#>  2 149              998689        acebutolol 200 MG O… SCD               1 aceb…
#>  3 149              998693        acebutolol 100 MG O… SCD               1 aceb…
#>  4 149              998695        acebutolol 400 MG O… SCD               1 aceb…
#>  5 1202             104308        atenolol 0.5 MG/ML … SCD               1 aten…
#>  6 1202             150750        atenolol 25 MG Oral… SBD               1 aten…
#>  7 1202             152414        atenolol 50 MG Oral… SBD               1 aten…
#>  8 1202             197379        atenolol 100 MG Ora… SCD               1 aten…
#>  9 1202             197380        atenolol 25 MG Oral… SCD               1 aten…
#> 10 1202             197381        atenolol 50 MG Oral… SCD               1 aten…
#> # ℹ 167 more rows
#> # ℹ 2 more variables: ingredient_name <chr>, ingredient_tty <chr>
```

## Active versus historical RxNorm concepts

For many current medication list workflows, `concept_status = "active"`
is a good default. This limits product expansion to active RxNorm
concepts.

For studies covering older calendar periods, users may want to include
historical RxNorm concepts as well.

``` r

bb_products_historical <- products_for_ingredients(
  bb_ingredients$ingredient_rxcui,
  ttys = product_ttys("default"),
  include_combos = TRUE,
  concept_status = "active_and_historical"
)
```

Historical concepts can be useful when reconstructing medication
exposure during older study periods. However, historical product
concepts may have less complete clinical attribute information than
active concepts. Route filtering should therefore be reviewed carefully
when `concept_status = "active_and_historical"` is used.

## Why route filtering matters

At this point, the product list may include products from multiple
routes. For example, some beta-blockers have oral tablets, injectable
products, or ophthalmic solutions.

We can use
[`get_clinical_attributes()`](https://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md)
to inspect route and dose-form information for the product RxCUIs.

``` r

if (run_live) {
  bb_attrs <- get_clinical_attributes(
    unique(bb_products$product_rxcui)
  ) |>
    rename(product_rxcui = rxcui)
} else {
  bb_attrs <- read_rxref_example("bb_attrs.rds")
}

bb_attrs |>
  count(route, dose_form_group, sort = TRUE)
#> # A tibble: 3 × 3
#>   route      dose_form_group        n
#>   <chr>      <chr>              <int>
#> 1 ORAL       Oral Product         176
#> 2 OPHTHALMIC Ophthalmic Product    29
#> 3 INJECTION  Injectable Product    14
```

This helps verify whether the product list includes routes that are
outside the intended use case.

## Filter to oral products

To keep only orally available products, use
[`filter_products_by_route()`](https://www.stevenmsmith.org/rxref/reference/filter_products_by_route.md).

``` r

if (run_live) {
  bb_oral_products <- bb_products |>
    filter_products_by_route(route = "ORAL")
} else {
  bb_oral_products <- read_rxref_example("bb_oral_products.rds")
}

bb_oral_products
#> # A tibble: 176 × 11
#>    ingredient_rxcui product_rxcui name                 tty   n_ingredients input
#>    <chr>            <chr>         <chr>                <chr>         <int> <chr>
#>  1 149              998685        acebutolol 400 MG O… SCD               1 aceb…
#>  2 149              998689        acebutolol 200 MG O… SCD               1 aceb…
#>  3 149              998693        acebutolol 100 MG O… SCD               1 aceb…
#>  4 149              998694        acebutolol 200 MG /… SCD               2 aceb…
#>  5 149              998695        acebutolol 400 MG O… SCD               1 aceb…
#>  6 1202             150750        atenolol 25 MG Oral… SBD               1 aten…
#>  7 1202             152414        atenolol 50 MG Oral… SBD               1 aten…
#>  8 1202             152916        atenolol 50 MG / ch… SCD               2 aten…
#>  9 1202             153155        atenolol 25 MG / be… SCD               2 aten…
#> 10 1202             197379        atenolol 100 MG Ora… SCD               1 aten…
#> # ℹ 166 more rows
#> # ℹ 5 more variables: ingredient_name <chr>, ingredient_tty <chr>, route <chr>,
#> #   dose_form <chr>, dose_form_group <chr>
```

The returned table keeps product-level metadata and appends summarized
route and dose-form information.

``` r

bb_oral_products |>
  count(route, dose_form_group, sort = TRUE)
#> # A tibble: 1 × 3
#>   route dose_form_group     n
#>   <chr> <chr>           <int>
#> 1 ORAL  Oral Product      176
```

This should retain products such as oral tablets, capsules, and oral
solutions, while excluding injectable or ophthalmic products.

Route filtering relies on clinical attribute information available
through RxNorm. If a product concept lacks route or dose-form
information, it may not be retained by route-specific filters. This is
especially important when historical concepts are included.

## Map oral product RxCUIs to active NDCs

Once the product list is restricted to oral products, use
[`map_rxcui_to_ndc()`](https://www.stevenmsmith.org/rxref/reference/map_rxcui_to_ndc.md)
to map the product RxCUIs to NDCs.

``` r

if (run_live) {
  bb_oral_ndc_map <- map_rxcui_to_ndc(
    unique(bb_oral_products$product_rxcui),
    status = "ACTIVE"
  )

  bb_oral_ndcs <- bb_oral_ndc_map |>
    left_join(
      bb_oral_products,
      by = c("rxcui" = "product_rxcui")
    ) |>
    rename(
      product_rxcui = rxcui,
    ) |>
    distinct()
} else {
  bb_oral_ndcs <- read_rxref_example("bb_oral_ndcs.rds")
}

bb_oral_ndcs
#> # A tibble: 4,670 × 13
#>    product_rxcui ndc11     ndc_status ingredient_rxcui name  tty   n_ingredients
#>    <chr>         <chr>     <chr>      <chr>            <chr> <chr>         <int>
#>  1 998685        00378140… ACTIVE     149              aceb… SCD               1
#>  2 998685        10135063… ACTIVE     149              aceb… SCD               1
#>  3 998685        51407066… ACTIVE     149              aceb… SCD               1
#>  4 998685        53746067… ACTIVE     149              aceb… SCD               1
#>  5 998685        53746067… ACTIVE     149              aceb… SCD               1
#>  6 998685        53746067… ACTIVE     149              aceb… SCD               1
#>  7 998685        62559025… ACTIVE     149              aceb… SCD               1
#>  8 998685        65162067… ACTIVE     149              aceb… SCD               1
#>  9 998685        65162067… ACTIVE     149              aceb… SCD               1
#> 10 998685        65162067… ACTIVE     149              aceb… SCD               1
#> # ℹ 4,660 more rows
#> # ℹ 6 more variables: input <chr>, ingredient_name <chr>, ingredient_tty <chr>,
#> #   route <chr>, dose_form <chr>, dose_form_group <chr>
```

The output is a flat tibble with one row per product RxCUI/NDC pair.
Product metadata is retained alongside the NDC.

``` r

bb_oral_ndcs |>
  select(
    ingredient_name,
    product_rxcui,
    name,
    tty,
    route,
    dose_form,
    ndc11,
    ndc_status
  )
#> # A tibble: 4,670 × 8
#>    ingredient_name product_rxcui name     tty   route dose_form ndc11 ndc_status
#>    <chr>           <chr>         <chr>    <chr> <chr> <chr>     <chr> <chr>     
#>  1 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 0037… ACTIVE    
#>  2 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 1013… ACTIVE    
#>  3 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 5140… ACTIVE    
#>  4 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 5374… ACTIVE    
#>  5 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 5374… ACTIVE    
#>  6 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 5374… ACTIVE    
#>  7 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 6255… ACTIVE    
#>  8 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 6516… ACTIVE    
#>  9 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 6516… ACTIVE    
#> 10 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 6516… ACTIVE    
#> # ℹ 4,660 more rows
```

The `status = "ACTIVE"` argument controls the NDC status returned by
[`map_rxcui_to_ndc()`](https://www.stevenmsmith.org/rxref/reference/map_rxcui_to_ndc.md).
This is distinct from `concept_status`, which controls whether active or
historical RxNorm concepts are included during product expansion.

## Summarize the resulting list

For quality control, it is useful to summarize the number of products
and NDCs by ingredient.

``` r

bb_oral_products |>
  count(ingredient_name, sort = TRUE, name = "n_product_rxcuis")
#> # A tibble: 14 × 2
#>    ingredient_name n_product_rxcuis
#>    <chr>                      <int>
#>  1 metoprolol                    34
#>  2 propranolol                   31
#>  3 sotalol                       22
#>  4 carvedilol                    16
#>  5 atenolol                      15
#>  6 bisoprolol                    12
#>  7 nebivolol                     10
#>  8 labetalol                      9
#>  9 nadolol                        9
#> 10 timolol                        6
#> 11 acebutolol                     5
#> 12 pindolol                       4
#> 13 betaxolol                      2
#> 14 penbutolol                     1
```

``` r

bb_oral_ndcs |>
  count(ingredient_name, sort = TRUE, name = "n_active_ndcs")
#> # A tibble: 13 × 2
#>    ingredient_name n_active_ndcs
#>    <chr>                   <int>
#>  1 metoprolol               1587
#>  2 carvedilol                712
#>  3 propranolol               588
#>  4 atenolol                  525
#>  5 labetalol                 312
#>  6 nebivolol                 265
#>  7 bisoprolol                262
#>  8 sotalol                   184
#>  9 nadolol                   173
#> 10 acebutolol                 21
#> 11 pindolol                   19
#> 12 betaxolol                  11
#> 13 timolol                    11
```

You may also want to inspect combination products separately.

``` r

bb_oral_products |>
  filter(n_ingredients > 1) |>
  select(
    ingredient_name,
    product_rxcui,
    name,
    tty,
    n_ingredients,
    route,
    dose_form
  )
#> # A tibble: 35 × 7
#>    ingredient_name product_rxcui name        tty   n_ingredients route dose_form
#>    <chr>           <chr>         <chr>       <chr>         <int> <chr> <chr>    
#>  1 acebutolol      998694        acebutolol… SCD               2 ORAL  Oral Tab…
#>  2 atenolol        152916        atenolol 5… SCD               2 ORAL  Oral Tab…
#>  3 atenolol        153155        atenolol 2… SCD               2 ORAL  Oral Cap…
#>  4 atenolol        197382        atenolol 1… SCD               2 ORAL  Oral Tab…
#>  5 atenolol        197383        atenolol 5… SCD               2 ORAL  Oral Tab…
#>  6 atenolol        393275        atenolol 5… SCD               2 ORAL  Extended…
#>  7 atenolol        746023        atenolol 1… SBD               2 ORAL  Oral Tab…
#>  8 atenolol        746030        atenolol 5… SBD               2 ORAL  Oral Tab…
#>  9 atenolol        977920        amiloride … SCD               3 ORAL  Oral Cap…
#> 10 bisoprolol      854908        bisoprolol… SCD               2 ORAL  Oral Tab…
#> # ℹ 25 more rows
```

Depending on the scientific question, combination products may be
appropriate to include or exclude. For example, beta-blocker/thiazide
combination products may be relevant for antihypertensive exposure
definitions but not for studies focused on beta-blocker monotherapy.

## A shortcut using `search_drug()`

The same core workflow can be run more compactly with
[`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md).
This function combines ingredient searching, product expansion, optional
route filtering, and optional NDC mapping.

To return oral product RxCUIs only:

``` r

if (run_live) {
  bb_oral_rxcuis <- search_drug(
    beta_blocker_names,
    return = "rxcui",
    route = "ORAL",
    include_combos = TRUE,
    concept_status = "active"
  )
} else {
  bb_oral_rxcuis <- read_rxref_example("bb_oral_rxcuis_search.rds")
}

bb_oral_rxcuis
#> # A tibble: 176 × 8
#>    ingredient_rxcui product_rxcui name       tty   n_ingredients route dose_form
#>    <chr>            <chr>         <chr>      <chr>         <int> <chr> <chr>    
#>  1 10600            198284        timolol 1… SCD               1 ORAL  Oral Tab…
#>  2 10600            198285        timolol 2… SCD               1 ORAL  Oral Tab…
#>  3 10600            198286        timolol 5… SCD               1 ORAL  Oral Tab…
#>  4 10600            250543        bendroflu… SCD               2 ORAL  Oral Tab…
#>  5 10600            310811        hydrochlo… SCD               2 ORAL  Oral Tab…
#>  6 10600            977949        amiloride… SCD               3 ORAL  Oral Tab…
#>  7 1202             150750        atenolol … SBD               1 ORAL  Oral Tab…
#>  8 1202             152414        atenolol … SBD               1 ORAL  Oral Tab…
#>  9 1202             152916        atenolol … SCD               2 ORAL  Oral Tab…
#> 10 1202             153155        atenolol … SCD               2 ORAL  Oral Cap…
#> # ℹ 166 more rows
#> # ℹ 1 more variable: dose_form_group <chr>
```

To return a flat table of oral product RxCUIs and active NDCs:

``` r

if (run_live) {
  bb_oral_ndcs_search_raw <- search_drug(
    beta_blocker_names,
    return = "ndc",
    route = "ORAL",
    ndc_status = "ACTIVE",
    include_combos = TRUE,
    concept_status = "active"
  )

  bb_oral_ndcs_search <- bb_oral_ndcs_search_raw

  if (!"ingredient_name" %in% names(bb_oral_ndcs_search)) {
    bb_oral_ndcs_search <- bb_oral_ndcs_search |>
      left_join(
        bb_ingredients |>
          select(ingredient_rxcui, ingredient_name),
        by = "ingredient_rxcui"
      )
  }

  product_cols <- c(
    "name",
    "tty",
    "route",
    "dose_form",
    "dose_form_group"
  )

  missing_product_cols <- setdiff(product_cols, names(bb_oral_ndcs_search))

  if (length(missing_product_cols) > 0) {
    bb_oral_ndcs_search <- bb_oral_ndcs_search |>
      left_join(
        bb_oral_products |>
          select(
            ingredient_rxcui,
            product_rxcui,
            all_of(missing_product_cols)
          ),
        by = c("ingredient_rxcui", "product_rxcui")
      )
  }

  bb_oral_ndcs_search <- bb_oral_ndcs_search |>
    distinct()
} else {
  bb_oral_ndcs_search <- read_rxref_example("bb_oral_ndcs_search.rds")
}

bb_oral_ndcs_search
#> # A tibble: 4,670 × 10
#>    ingredient_rxcui product_rxcui ndc11   ndc_status ingredient_name name  tty  
#>    <chr>            <chr>         <chr>   <chr>      <chr>           <chr> <chr>
#>  1 149              998685        003781… ACTIVE     acebutolol      aceb… SCD  
#>  2 149              998685        101350… ACTIVE     acebutolol      aceb… SCD  
#>  3 149              998685        514070… ACTIVE     acebutolol      aceb… SCD  
#>  4 149              998685        537460… ACTIVE     acebutolol      aceb… SCD  
#>  5 149              998685        537460… ACTIVE     acebutolol      aceb… SCD  
#>  6 149              998685        537460… ACTIVE     acebutolol      aceb… SCD  
#>  7 149              998685        625590… ACTIVE     acebutolol      aceb… SCD  
#>  8 149              998685        651620… ACTIVE     acebutolol      aceb… SCD  
#>  9 149              998685        651620… ACTIVE     acebutolol      aceb… SCD  
#> 10 149              998685        651620… ACTIVE     acebutolol      aceb… SCD  
#> # ℹ 4,660 more rows
#> # ℹ 3 more variables: route <chr>, dose_form <chr>, dose_form_group <chr>
```

The raw `search_drug(return = "ndc")` output is intentionally compact.
In this vignette, we join it back to the ingredient and product tables
so that the displayed example includes readable ingredient names,
product names, routes, and dose forms.

## Returning both products and NDCs

If you want both the unique product RxCUI table and the expanded NDC
table, use `return = "both"`.

``` r

if (run_live) {
  bb_oral_both <- search_drug(
    beta_blocker_names,
    return = "both",
    route = "ORAL",
    ndc_status = "ACTIVE",
    include_combos = TRUE,
    concept_status = "active"
  )
} else {
  bb_oral_both <- read_rxref_example("bb_oral_both_search.rds")
}

names(bb_oral_both)
#> [1] "products" "ndcs"
```

This returns a list because the two tables have different grains:

- `products`: one row per product RxCUI;
- `ndcs`: one row per product RxCUI/NDC pair.

``` r

bb_oral_both$products
#> # A tibble: 176 × 9
#>    ingredient_rxcui ingredient_name product_rxcui name       tty   n_ingredients
#>    <chr>            <chr>           <chr>         <chr>      <chr>         <int>
#>  1 149              acebutolol      998685        acebutolo… SCD               1
#>  2 149              acebutolol      998689        acebutolo… SCD               1
#>  3 149              acebutolol      998693        acebutolo… SCD               1
#>  4 149              acebutolol      998694        acebutolo… SCD               2
#>  5 149              acebutolol      998695        acebutolo… SCD               1
#>  6 1202             atenolol        150750        atenolol … SBD               1
#>  7 1202             atenolol        152414        atenolol … SBD               1
#>  8 1202             atenolol        152916        atenolol … SCD               2
#>  9 1202             atenolol        153155        atenolol … SCD               2
#> 10 1202             atenolol        197379        atenolol … SCD               1
#> # ℹ 166 more rows
#> # ℹ 3 more variables: route <chr>, dose_form <chr>, dose_form_group <chr>
```

``` r

bb_oral_both$ndcs
#> # A tibble: 4,670 × 10
#>    ingredient_rxcui ingredient_name product_rxcui name     tty   route dose_form
#>    <chr>            <chr>           <chr>         <chr>    <chr> <chr> <chr>    
#>  1 149              acebutolol      998685        acebuto… SCD   ORAL  Oral Cap…
#>  2 149              acebutolol      998685        acebuto… SCD   ORAL  Oral Cap…
#>  3 149              acebutolol      998685        acebuto… SCD   ORAL  Oral Cap…
#>  4 149              acebutolol      998685        acebuto… SCD   ORAL  Oral Cap…
#>  5 149              acebutolol      998685        acebuto… SCD   ORAL  Oral Cap…
#>  6 149              acebutolol      998685        acebuto… SCD   ORAL  Oral Cap…
#>  7 149              acebutolol      998685        acebuto… SCD   ORAL  Oral Cap…
#>  8 149              acebutolol      998685        acebuto… SCD   ORAL  Oral Cap…
#>  9 149              acebutolol      998685        acebuto… SCD   ORAL  Oral Cap…
#> 10 149              acebutolol      998685        acebuto… SCD   ORAL  Oral Cap…
#> # ℹ 4,660 more rows
#> # ℹ 3 more variables: dose_form_group <chr>, ndc11 <chr>, ndc_status <chr>
```

## Suggested QC checks

When developing a study-specific medication list, it is good practice to
review the resulting concepts before finalizing the cohort definition.
Even if the package is working correctly, RxNorm concepts and source
data may occasionally contain surprises for a specific study use case.

The following are some examples.

### Check whether any non-oral product names remain

``` r

bb_oral_products |>
  filter(grepl(
    "Injection|Injectable|Ophthalmic|Topical|Transdermal|Nasal|Inhalation",
    name,
    ignore.case = TRUE
  ))
#> # A tibble: 0 × 11
#> # ℹ 11 variables: ingredient_rxcui <chr>, product_rxcui <chr>, name <chr>,
#> #   tty <chr>, n_ingredients <int>, input <chr>, ingredient_name <chr>,
#> #   ingredient_tty <chr>, route <chr>, dose_form <chr>, dose_form_group <chr>
```

### Check the observed routes and dose-form groups

``` r

bb_oral_products |>
  count(route, dose_form_group, sort = TRUE)
#> # A tibble: 1 × 3
#>   route dose_form_group     n
#>   <chr> <chr>           <int>
#> 1 ORAL  Oral Product      176
```

### Check combination products

``` r

bb_oral_products |>
  filter(n_ingredients > 1) |>
  count(ingredient_name, sort = TRUE)
#> # A tibble: 10 × 2
#>    ingredient_name     n
#>    <chr>           <int>
#>  1 atenolol            8
#>  2 metoprolol          7
#>  3 bisoprolol          6
#>  4 propranolol         4
#>  5 timolol             3
#>  6 nadolol             2
#>  7 nebivolol           2
#>  8 acebutolol          1
#>  9 labetalol           1
#> 10 pindolol            1
```

### Check products without active NDCs

Some product RxCUIs may not map to active NDCs.

``` r

bb_oral_products |>
  anti_join(
    bb_oral_ndcs |>
      distinct(product_rxcui),
    by = "product_rxcui"
  ) |>
  select(
    ingredient_name,
    product_rxcui,
    name,
    tty,
    route,
    dose_form
  )
#> # A tibble: 41 × 6
#>    ingredient_name product_rxcui name                      tty   route dose_form
#>    <chr>           <chr>         <chr>                     <chr> <chr> <chr>    
#>  1 acebutolol      998693        acebutolol 100 MG Oral C… SCD   ORAL  Oral Cap…
#>  2 acebutolol      998694        acebutolol 200 MG / hydr… SCD   ORAL  Oral Tab…
#>  3 acebutolol      998695        acebutolol 400 MG Oral T… SCD   ORAL  Oral Tab…
#>  4 atenolol        152916        atenolol 50 MG / chlorth… SCD   ORAL  Oral Tab…
#>  5 atenolol        153155        atenolol 25 MG / bendrof… SCD   ORAL  Oral Cap…
#>  6 atenolol        393275        atenolol 50 MG / nifedip… SCD   ORAL  Extended…
#>  7 atenolol        755528        atenolol 5 MG/ML Oral So… SCD   ORAL  Oral Sol…
#>  8 atenolol        977920        amiloride hydrochloride … SCD   ORAL  Oral Cap…
#>  9 bisoprolol      865154        bisoprolol fumarate 1.25… SCD   ORAL  Oral Tab…
#> 10 bisoprolol      865157        bisoprolol fumarate 3.75… SCD   ORAL  Oral Tab…
#> # ℹ 31 more rows
```

## Summary

This vignette demonstrated how to build a route-specific medication
list, with oral beta-blockers as an example, using `rxref`.

The key steps are:

1.  [`find_ingredients()`](https://www.stevenmsmith.org/rxref/reference/find_ingredients.md)
    to resolve drug names to ingredient RxCUIs;
2.  [`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
    to expand ingredients to product RxCUIs;
3.  [`filter_products_by_route()`](https://www.stevenmsmith.org/rxref/reference/filter_products_by_route.md)
    to retain products for the route of interest;
4.  [`map_rxcui_to_ndc()`](https://www.stevenmsmith.org/rxref/reference/map_rxcui_to_ndc.md)
    to obtain NDCs; or
5.  [`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md)
    for a compact end-to-end workflow.

For drug classes with products available through multiple routes, route
filtering is an important quality-control step before mapping to NDCs or
using the resulting list in pharmacoepidemiologic analyses.

Users should also distinguish between two separate choices:

- `concept_status` controls whether active or historical RxNorm concepts
  are included.
- `status` or `ndc_status` controls which NDC status categories are
  returned.

For reproducible research, save the final product list, NDC list,
package version, and API-derived outputs used in the analytic workflow.
