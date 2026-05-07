# Building route-specific RxCUI and NDC lists

## Overview

A common use case for rxref is to develop a list of RxNorm product
concepts and National Drug Codes (NDCs) for a drug class.

For example, suppose we want to identify product RxCUIs and active NDCs
for oral beta-blockers. This is not as simple as searching for
beta-blocker ingredients and mapping everything to NDCs, because some
beta-blockers are available through multiple routes.

For example:

- `metoprolol`, `atenolol`, `labetalol`, `propranolol`, and `sotalol`
  may have oral and injectable products.
- `esmolol` is injectable only.
- `timolol`, `betaxolol`, and related drugs may have ophthalmic
  products.

For many pharmacoepidemiologic studies, we may want oral outpatient-use
products only.

This vignette walks through a tidy workflow for:

1.  resolving drug names to ingredient RxCUIs;
2.  expanding ingredient RxCUIs to product RxCUIs;
3.  filtering product RxCUIs by route; and,
4.  mapping the filtered product RxCUIs to active NDCs.

## Define a beta-blocker ingredient list

In this example, we start with generic drug names. In future versions of
`rxref`, class-level helpers may make this step more automated. For now,
users can supply a curated ingredient list for the class of interest.

``` r

library(rxref)
library(dplyr)
library(purrr)

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
[`find_ingredients()`](http://www.stevenmsmith.org/rxref/reference/find_ingredients.md)
to resolve each drug name to an RxNorm ingredient concept.

``` r


bb_ingredients <- find_ingredients(beta_blocker_names) |>
  filter(tty == "IN") |>
  distinct(
    input,
    ingredient_rxcui = rxcui,
    ingredient_name = name
  )

bb_ingredients
```

The resulting table contains one row per resolved ingredient. These
ingredient RxCUIs are the starting point for product expansion.

## Expand ingredients to product RxCUIs

Next, use
[`products_for_ingredients()`](http://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
to identify product-level RxNorm concepts containing each ingredient.

``` r


bb_products <- products_for_ingredients(
  bb_ingredients$ingredient_rxcui,
  include_combos = TRUE
) |>
  left_join(bb_ingredients, by = "ingredient_rxcui")

bb_products
```

By default, this returns product-level RxNorm concepts such as clinical
drugs and branded drugs. Because `include_combos = TRUE`, combination
products are retained. For beta-blockers, this means products such as
beta-blocker/thiazide combinations may be included.

To exclude combination products, set:

``` r


bb_single_ingredient_products <- products_for_ingredients(
  bb_ingredients$ingredient_rxcui,
  include_combos = FALSE
)

bb_single_ingredient_products
```

## Why route filtering matters

At this point, the product list may include products from multiple
routes. For example, some beta-blockers have oral tablets, injectable
products, or ophthalmic solutions.

We can use
[`get_clinical_attributes()`](http://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md)
to inspect route information for the product RxCUIs.

``` r


bb_attrs <- get_clinical_attributes(
  unique(bb_products$product_rxcui)
)

bb_attrs |>
  count(route, dose_form_group, sort = TRUE)
```

This helps verify whether the product list includes routes that are
outside the intended use case.

## Filter to oral products

To keep only orally available products, use
[`filter_products_by_route()`](http://www.stevenmsmith.org/rxref/reference/filter_products_by_route.md).

``` r


bb_oral_products <- bb_products |>
  filter_products_by_route(route = "ORAL")

bb_oral_products
```

The returned table keeps product-level metadata and appends summarized
route and dose-form information.

``` r


bb_oral_products |>
  count(routes, dose_form_groups, sort = TRUE)
```

This should retain products such as oral tablets, capsules, and oral
solutions, while excluding injectable or ophthalmic products.

## Map oral product RxCUIs to active NDCs

Once the product list is restricted to oral products, use
`map_products_to_ndcs()` to map the product RxCUIs to NDCs.

``` r


bb_oral_ndcs <- map_products_to_ndcs(
  bb_oral_products,
  status = "ACTIVE"
)

bb_oral_ndcs
```

The output is a flat tibble with one row per product RxCUI/NDC pair.
Product metadata is retained alongside the NDC.

``` r


bb_oral_ndcs |>
  select(
    ingredient_name,
    product_rxcui,
    product_name,
    product_tty,
    routes,
    dose_forms,
    ndc11,
    ndc_status
  )
```

## Summarize the resulting list

For quality control, it is useful to summarize the number of products
and NDCs by ingredient.

``` r


bb_oral_products |>
  count(ingredient_name, sort = TRUE, name = "n_product_rxcuis")

bb_oral_ndcs |>
  count(ingredient_name, sort = TRUE, name = "n_active_ndcs")
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
    routes,
    dose_forms
  )
```

## A shortcut using search_drug()

The same workflow can be run more compactly with
[`search_drug()`](http://www.stevenmsmith.org/rxref/reference/search_drug.md).

To return oral product RxCUIs only:

``` r


bb_oral_rxcuis <- search_drug(
  beta_blocker_names,
  return = "rxcui",
  route = "ORAL",
  include_combos = TRUE
)

bb_oral_rxcuis
```

To return a flat table of oral product RxCUIs and active NDCs:

``` r


bb_oral_ndcs2 <- search_drug(
  beta_blocker_names,
  return = "ndc",
  route = "ORAL",
  ndc_status = "ACTIVE",
  include_combos = TRUE
)

bb_oral_ndcs2
```

This is the most direct workflow when the goal is a usable product
RxCUI/NDC table.

## Returning both products and NDCs

If you want both the unique product RxCUI table and the expanded NDC
table, use `return = "both"`.

``` r


bb_oral_both <- search_drug(
  beta_blocker_names,
  return = "both",
  route = "ORAL",
  ndc_status = "ACTIVE",
  include_combos = TRUE
)

names(bb_oral_both)

bb_oral_both$products

bb_oral_both$ndcs
```

This returns a list because the two tables have different grains:

- `products`: one row per product RxCUI;
- `ndcs`: one row per product RxCUI/NDC pair.

## Suggested QC checks

When developing a study-specific medication list, it is good practice to
review the resulting concepts before finalizing the cohort definition.
Even if the package is working correctly, there are on occasion errors
in RxNorm itself.

The following are some examples.

### Check whether any non-oral product names remain

``` r


bb_oral_products |>
  filter(grepl(
    "Injection|Injectable|Ophthalmic|Topical|Transdermal|Nasal|Inhalation",
    name,
    ignore.case = TRUE
  ))
```

### Check the observed routes and dose-form groups

``` r


bb_oral_products |>
  count(routes, dose_form_groups, sort = TRUE)
```

### Check combination products

Depending on the scientific question, combination products may be
appropriate to include or exclude.

``` r


bb_oral_products |>
  filter(n_ingredients > 1) |>
  count(ingredient_name, sort = TRUE)
```

## Summary

This vignette demonstrated how to build a route-specific medication
list, with oral beta-blockers as an example, using `rxref`.

The key steps are:

1.  [`find_ingredients()`](http://www.stevenmsmith.org/rxref/reference/find_ingredients.md)
    to resolve drug names to ingredient RxCUIs;
2.  [`products_for_ingredients()`](http://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
    to expand ingredients to product RxCUIs;
3.  [`filter_products_by_route()`](http://www.stevenmsmith.org/rxref/reference/filter_products_by_route.md)
    to retain products for the route of interest;
4.  `map_products_to_ndcs()` to obtain NDCs; OR,
5.  [`search_drug()`](http://www.stevenmsmith.org/rxref/reference/search_drug.md)
    for a compact end-to-end workflow.

For drug classes with products available through multiple routes, route
filtering is an important quality-control step before mapping to NDCs or
using the resulting list in pharmacoepidemiologic analyses.
