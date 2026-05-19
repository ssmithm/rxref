# rxref: Getting started

## Overview

`rxref` provides tidy, vectorized helpers for working with drug
identifiers and metadata from the RxNorm and RxClass APIs. It is
designed for workflows that need to resolve drug names, RxCUIs, and
NDCs; retrieve RxNorm concept metadata; expand ingredients to product
concepts; map products to NDCs; and build medication lists for
pharmacoepidemiologic or health services research.

Common tasks include:

- resolving messy drug names or NDCs to RxCUIs;
- retrieving RxNorm concept properties;
- mapping NDCs to RxCUIs, or RxCUIs to NDCs;
- finding ingredients and related product concepts;
- creating route-specific product or NDC lists;
- optionally including historical RxNorm concepts when studying older
  calendar periods.

The examples below are not evaluated when the vignette is built, because
they query live APIs. You can run them interactively after installing
rxref.

## Installation

Install the stable version of `rxref` from CRAN:

``` r

# Install from CRAN
# install.packages("rxref")
```

Or the development version:

``` r

# install.packages("pak")
# pak::pak("ssmithm/rxref")
```

Then load the package (the vignette will also use dplyr).

``` r

library(rxref)
library(dplyr)
```

## Resolve messy inputs to RxCUIs

The
[`resolve()`](https://www.stevenmsmith.org/rxref/reference/resolve.md)
function accepts common drug identifiers, including free-text drug
names, NDCs, and RxCUIs. This is often the first step when working with
real-world medication data.

``` r

resolve(c(
  "metformin 500 mg tablet",
  "00093-1048-01",
  "8610"
))
```

The returned object includes the original input, the resolved RxCUI, and
available concept information.

## Retrieve properties for RxCUIs

Once you have one or more RxCUIs, use
[`get_properties()`](https://www.stevenmsmith.org/rxref/reference/get_properties.md)
to retrieve RxNorm concept metadata. This is useful when you want to
inspect concept names, term types, suppress status, or other basic
RxNorm properties.

``` r

ids <- c("860975", "860976")

get_properties(ids)
```

## Map NDCs and RxCUIs

`rxref` includes helpers for mapping between NDCs and RxCUIs.

To map NDCs to RxCUIs:

``` r

map_ndc_to_rxcui(c(
  "00093-1048-01",
  "00093-1048-10"
))
```

To map an RxCUI to NDCs:

``` r

map_rxcui_to_ndc("860975")
```

You can optionally filter NDC mappings by NDC status:

``` r

map_rxcui_to_ndc("860975", status = "ACTIVE")
```

NDC status and RxNorm concept status are related but distinct. NDC
status refers to whether an NDC is active, obsolete, or otherwise
categorized by RxNav. RxNorm concept status refers to whether the RxNorm
concept itself is active or historical.

## Find ingredients

The
[`find_ingredients()`](https://www.stevenmsmith.org/rxref/reference/find_ingredients.md)
function helps identify ingredient-level RxCUIs from approximate drug
names.

``` r

find_ingredients(c("metformin", "semaglutide", "lisinopril"))
```

For many medication list workflows, users first identify a set of
ingredient RxCUIs and then expand those ingredients to product concepts.

``` r

ingredients <- find_ingredients(c("metformin", "semaglutide")) |>
  filter(tty == "IN") |>
  distinct(
    ingredient_rxcui = rxcui,
    ingredient_name = name
  )

ingredients
```

## Expand ingredients to product concepts

Use
[`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
to identify product concepts associated with one or more ingredient
RxCUIs.

``` r

products <- products_for_ingredients(
  ingredients$ingredient_rxcui,
  concept_status = "active"
)

products
```

By default, most workflows should focus on active RxNorm concepts. For
studies that span older calendar periods, you may want to include
*historical* concepts as well.

``` r

products_historical <- products_for_ingredients(
  ingredients$ingredient_rxcui,
  concept_status = "active_and_historical"
)

products_historical
```

Historical concepts can be useful when reconstructing medication
exposure during older study periods. However, some historical concepts
may have less complete clinical attribute information than active
concepts, so users should review route, dose form, and ingredient
information carefully when using historical concepts.

## Choose product term types

RxNorm includes several term types, or TTYs, for clinical drugs, branded
drugs, packs, and other related concepts. `rxref` provides helpers for
common product-focused TTY sets.

``` r

product_ttys("default")
product_ttys("extended_product")
product_ttys("extended")
```

For example, you can request an expanded set of product concepts:

``` r

products_extended <- products_for_ingredients(
  ingredients$ingredient_rxcui,
  ttys = product_ttys("extended_product"),
  concept_status = "active"
)

products_extended
```

The appropriate TTY set depends on the study question. A narrower
product set may be preferable for simple ingredient-to-product
workflows, while an extended set may be useful when users want to
capture additional product or pack concepts. More information on TTYs
can be found by running
[`tty_catalogue()`](https://www.stevenmsmith.org/rxref/reference/tty_catalogue.md).

## Search for drugs in one step

For many common workflows,
[`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md)
provides a compact interface that combines ingredient search, product
expansion, route filtering, and optional NDC mapping.

For example, to search for oral metformin products:

``` r

search_drug(
  term = "metformin",
  return = "product",
  route = "ORAL"
)
```

To return NDCs instead of product concepts:

``` r

search_drug(
  term = "metformin",
  return = "ndc",
  route = "ORAL",
  ndc_status = "ACTIVE"
)
```

To return both product concepts and NDCs:

``` r

search_drug(
  term = "metformin",
  return = "both",
  route = "ORAL",
  ndc_status = "ACTIVE"
)
```

For historical studies, you can include historical RxNorm concepts and
broader NDC status categories:

``` r

search_drug(
  term = "metformin",
  return = "ndc",
  route = "ORAL",
  concept_status = "active_and_historical",
  ndc_status = c("ACTIVE", "OBSOLETE", "UNSPECIFIED")
)
```

Here, `concept_status` controls whether active or historical RxNorm
concepts are considered. The `ndc_status` argument controls which NDC
status categories are returned.

## Retrieve clinical attributes

Some workflows require route, dose form, strength, or ingredient-count
information. The
[`get_clinical_attributes()`](https://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md)
function retrieves product-level clinical attributes for RxCUIs.

``` r

attrs <- get_clinical_attributes(products$product_rxcui)

attrs
```

These attributes can be useful for route-specific medication lists:

``` r

attrs |>
  count(routes, dose_form_groups, sort = TRUE)
```

Combination products may include more than one ingredient. Depending on
the workflow, users may want to exclude fixed-dose combinations, retain
them, or inspect them separately.

``` r

attrs |>
  filter(n_ingredients > 1)
```

## Build route-specific lists

The
[`filter_products_by_route()`](https://www.stevenmsmith.org/rxref/reference/filter_products_by_route.md)
helper can be used after product expansion to retain products matching a
specified route.

``` r

oral_products <- products |>
  filter_products_by_route(route = "ORAL")

oral_products
```

Route filtering relies on available clinical attribute information. If a
product concept lacks route or dose-form information, it may not be
retained by route-specific filters. This is especially important when
working with historical RxNorm concepts.

## Caching and API use

`rxref` is designed to support vectorized workflows, but large
medication lists can require many API calls. Repeated requests can be
reduced by enabling caching through
[`rxref_conf()`](https://www.stevenmsmith.org/rxref/reference/rxref_conf.md).

For example, to use an in-memory cache during a session:

``` r

rxref_conf(cache = cachem::cache_mem())
```

You can also adjust the delay between API requests:

``` r

rxref_conf(rate_delay = 0.2)
```

This can be helpful for large batch workflows or when working
interactively with many RxCUIs. But, be kind to the API. RxNorm
specifies a [maximum request rate of 20 per second per
IP](https://lhncbc.nlm.nih.gov/RxNav/TermsofService.html).

## Reproducibility

The public RxNorm and RxClass APIs reflect current source data and may
change over time as RxNorm releases are updated. For strict
reproducibility, especially in studies tied to a specific calendar
period, users should save the medication lists, product concepts, NDC
mappings, and API outputs used in the final analytic workflow.

At some point, a future version of `rxref` may support local or
release-specific backends for users who need to bind analyses to
specific RxNorm releases.

## Summary

`rxref` supports common medication-identification workflows, including:

1.  resolving names, NDCs, or RxCUIs with
    [`resolve()`](https://www.stevenmsmith.org/rxref/reference/resolve.md);
2.  retrieving concept metadata with
    [`get_properties()`](https://www.stevenmsmith.org/rxref/reference/get_properties.md);
3.  mapping between NDCs and RxCUIs with
    [`map_ndc_to_rxcui()`](https://www.stevenmsmith.org/rxref/reference/map_ndc_to_rxcui.md)
    and
    [`map_rxcui_to_ndc()`](https://www.stevenmsmith.org/rxref/reference/map_rxcui_to_ndc.md);
4.  identifying ingredients with
    [`find_ingredients()`](https://www.stevenmsmith.org/rxref/reference/find_ingredients.md);
5.  expanding ingredients to products with
    [`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md);
6.  retrieving route and dose-form information with
    [`get_clinical_attributes()`](https://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md);
7.  building route-specific product lists with
    [`filter_products_by_route()`](https://www.stevenmsmith.org/rxref/reference/filter_products_by_route.md);
8.  using
    [`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md)
    for compact end-to-end workflows.

These functions are intended to support transparent, reproducible
medication list construction while keeping outputs tidy and compatible
with common dplyr workflows.
