# rxref: Getting started

## Overview

`rxref` provides tidy, vectorized tools for working with drug
identifiers and metadata from the RxNorm and RxClass APIs (part of the
[Unified Medical Language
System](https://www.nlm.nih.gov/research/umls/index.html) maintained by
the U.S. National Library of Medicine. It is designed for workflows that
need to resolve drug names and identifiers, inspect [RxNorm
concepts](https://www.nlm.nih.gov/research/umls/rxnorm/overview.html),
expand ingredients to product concepts, map between RxNorm Concept
Unique Identifiers (RxCUIs) and National Drug Codes (NDCs), retrieve
clinical product attributes, and query drug-class information.

A common medication-list workflow looks like this:

1.  resolve a drug name or identifier to an RxCUI;
2.  identify the ingredient concept or concepts of interest;
3.  expand ingredients to RxNorm product concepts;
4.  refine the product set using term type, route, combination-product
    status, or other clinical attributes;
5.  map the final product concepts to NDCs when needed; and
6.  save the resulting medication definition for reproducible downstream
    use.

The examples below query live RxNorm or RxClass APIs and therefore are
not evaluated when this vignette is built. You can run them
interactively after installing `rxref`.

## Installation

Install the stable version of `rxref` from CRAN:

``` r

install.packages("rxref")
```

Or install the development version from GitHub:

``` r

# install.packages("pak")
# pak::pak("ssmithm/rxref")
```

Then load `rxref`. The examples below also use `dplyr` for data
manipulation.

``` r

library(rxref)
library(dplyr)
```

## Resolve drug names and identifiers

[`resolve()`](https://www.stevenmsmith.org/rxref/reference/resolve.md)
accepts free-text drug names, NDCs, and RxCUIs. With the default
`type = "auto"`, `rxref` infers the input type and returns the
corresponding RxNorm concept information.

``` r

resolve(c(
  "metformin 500 mg tablet",
  "00093-1048-01",
  "860975"
))
```

This is useful when medication data arrive in mixed or imperfect formats
and you first need to determine which RxNorm concepts they represent.

## Inspect RxNorm concepts

Once you have one or more RxCUIs, use
[`get_properties()`](https://www.stevenmsmith.org/rxref/reference/get_properties.md)
to retrieve core RxNorm metadata such as the preferred concept name,
term type (TTY), suppress flag, and UMLS Concept Unique Identifier
(CUI).

``` r

ids <- c("860975", "860976")

get_properties(ids)
```

Understanding the TTY is often important because RxNorm distinguishes
ingredients, clinical drugs, branded drugs, packs, and several other
concept types. Only some of these are prescribable products, as
discussed below.

## Identify ingredient concepts

For medication-list construction, the next step is often to identify the
ingredient-level RxCUIs corresponding to one or more drug names.

``` r

find_ingredients(c("metformin", "semaglutide", "lisinopril"))
```

[`find_ingredients()`](https://www.stevenmsmith.org/rxref/reference/find_ingredients.md)
returns ingredient (`IN`) and, when available, precise ingredient
(`PIN`) concepts. A common pattern is to retain the base ingredient
concepts before expanding them to products.

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
to identify RxNorm product concepts that contain the selected
ingredients.

``` r

products <- products_for_ingredients(
  ingredients$ingredient_rxcui
)

products
```

By default, `rxref` uses a product-focused set of TTYs corresponding to
semantic clinical drugs, semantic branded drugs, generic packs, and
branded packs. These are the concept types most likely to be useful when
the ultimate goal is NDC mapping.

You can inspect the available predefined TTY sets with:

``` r

product_ttys("default")
product_ttys("extended_product")
product_ttys("extended")
```

For more detail about individual RxNorm term types, use:

``` r

tty_catalogue()
```

A broader TTY set can be supplied explicitly when the study question
requires additional product-related concepts:

``` r

products_extended <- products_for_ingredients(
  ingredients$ingredient_rxcui,
  ttys = product_ttys("extended_product")
)

products_extended
```

The narrowest useful product definition is often preferable. Broader TTY
sets can capture additional structural or grouping concepts, but many of
these do not map directly to NDCs.

## Refine products using clinical attributes

[`get_clinical_attributes()`](https://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md)
adds clinically useful information such as strength, dose form, route,
dose-form group, brand/generic status, and ingredient count.

``` r

attrs <- get_clinical_attributes(products$product_rxcui)

attrs
```

For example, you can inspect the routes and dose-form groups represented
in a product set:

``` r

attrs |>
  count(route, dose_form_group, sort = TRUE)
```

Or identify combination products:

``` r

attrs |>
  filter(ingredient_count > 1)
```

Clinical attributes are derived from RxNorm concept information,
including some parsing of RxNorm names and dose-form metadata. They are
useful for medication-list construction, but study-specific product
definitions should still be reviewed before final use.

### Filter products by route

When only one route is relevant,
[`filter_products_by_route()`](https://www.stevenmsmith.org/rxref/reference/filter_products_by_route.md)
provides a convenient way to retain products matching that route.

``` r

oral_products <- products |>
  filter_products_by_route(route = "ORAL")

oral_products
```

Route filtering can also be requested directly during product expansion:

``` r

oral_products <- products_for_ingredients(
  ingredients$ingredient_rxcui,
  route = "ORAL"
)
```

For more involved medication definitions, it is often useful to inspect
the unfiltered product set first and then apply route restrictions
explicitly.

## Map between RxCUIs and NDCs

`rxref` provides helpers for mapping in both directions between NDCs and
RxCUIs.

To map NDCs to RxCUIs:

``` r

map_ndc_to_rxcui(c(
  "00093-1048-01",
  "00093-1048-10"
))
```

To map product RxCUIs to currently active NDC associations:

``` r

ndcs <- map_rxcui_to_ndc(
  oral_products$product_rxcui,
  status = "ACTIVE"
)

ndcs
```

Because multiple NDCs may correspond to the same RxNorm product concept,
NDC mapping commonly increases the number of rows substantially.

## Use `search_drug()` for a compact workflow

The preceding sections show the individual steps so that users can
inspect and control each stage of medication-list construction. For many
common workflows,
[`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md)
combines ingredient search, product expansion, route filtering, and
optional NDC mapping in one call.

To return oral metformin product concepts:

``` r

search_drug(
  term = "metformin",
  return = "rxcui",
  route = "ORAL"
)
```

To return currently active NDCs:

``` r

search_drug(
  term = "metformin",
  return = "ndc",
  route = "ORAL",
  ndc_status = "ACTIVE"
)
```

Or return both product concepts and NDCs:

``` r

metformin <- search_drug(
  term = "metformin",
  return = "both",
  route = "ORAL",
  ndc_status = "ACTIVE"
)

metformin$products
metformin$ndcs
```

[`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md)
is useful for concise workflows, while the step-by-step approach is
preferable when you need to audit intermediate concepts or apply custom
study-specific rules.

## Work with historical concepts and NDC associations

Historical medication-list construction involves two related but
distinct questions:

- **RxNorm concept history:** should the product search include concepts
  that are no longer active in the current RxNorm release?
- **NDC association history:** should NDC mapping include historical
  direct or indirect NDC associations rather than only currently active
  associations?

To include active and historical RxNorm product concepts, use
`concept_status = "active_and_historical"`:

``` r

products_historical <- products_for_ingredients(
  ingredients$ingredient_rxcui,
  concept_status = "active_and_historical"
)

products_historical
```

To retrieve historical NDC associations for selected product RxCUIs, use
the `history` argument to
[`map_rxcui_to_ndc()`](https://www.stevenmsmith.org/rxref/reference/map_rxcui_to_ndc.md):

``` r

ndcs_historical <- map_rxcui_to_ndc(
  products_historical$product_rxcui,
  history = "all"
)

ndcs_historical
```

Use `history = "direct"` when only NDCs ever directly associated with
the input RxCUI are desired, and `history = "all"` when indirect
associations through remapped or archived concepts should also be
considered. The `status` argument can then be used to filter the NDCs
that were retrieved.

Historical concepts and NDCs can be important when reconstructing
exposure in older study periods. They also require additional review
because historical concepts may have less complete route, dose-form, or
other clinical metadata than current concepts.

## Query drug classes with RxClass

`rxref` also provides access to drug-class relationships from the
RxClass API. Because different sources classify drugs differently, class
source and relationship information are retained rather than collapsed
into a single universal class definition.

Source-specific convenience functions include
[`get_atc()`](https://www.stevenmsmith.org/rxref/reference/get_atc.md),
[`get_epc()`](https://www.stevenmsmith.org/rxref/reference/get_epc.md),
and
[`get_va()`](https://www.stevenmsmith.org/rxref/reference/get_va.md).

``` r

get_atc("metformin", by = "name")
get_epc("metformin", by = "name")
```

For a broader class-oriented summary,
[`get_drug_classes()`](https://www.stevenmsmith.org/rxref/reference/get_drug_classes.md)
combines selected class-like assertions from multiple sources:

``` r

get_drug_classes("metformin", by = "name")
```

[`get_drug_classes()`](https://www.stevenmsmith.org/rxref/reference/get_drug_classes.md)
is experimental because “drug class” is not a single native RxClass
concept and different source vocabularies use different classification
logic. For source-specific or relationship-specific work, use
[`get_classes()`](https://www.stevenmsmith.org/rxref/reference/get_classes.md)
and the dedicated RxClass helpers.

## Configure API behavior

`rxref` uses an in-memory cache by default so repeated identical
requests within a session do not need to be sent to the API again.
Current package settings can be inspected with:

``` r

rxref_conf()
```

You can also adjust the delay between API requests:

``` r

rxref_conf(rate_delay = 0.2)
```

For large batch workflows, caching and an appropriate request delay help
reduce unnecessary API traffic. RxNav currently specifies a maximum
request rate of 20 requests per second per IP address; see the [RxNav
Terms of Service](https://lhncbc.nlm.nih.gov/RxNav/TermsofService.html).

## Reproducibility

The public RxNorm and RxClass APIs reflect source data that change as
new releases are published. For analyses that require a reproducible
medication definition, save the final ingredient list, product concepts,
NDC mappings, and other API-derived metadata used in the analytic
workflow.

This is especially important for studies tied to historical calendar
periods, because the concepts and mappings returned by the current APIs
may differ from those available when the original data were generated.

## Summary

A typical `rxref` workflow is:

1.  resolve names or identifiers with
    [`resolve()`](https://www.stevenmsmith.org/rxref/reference/resolve.md);
2.  inspect concepts with
    [`get_properties()`](https://www.stevenmsmith.org/rxref/reference/get_properties.md);
3.  identify ingredients with
    [`find_ingredients()`](https://www.stevenmsmith.org/rxref/reference/find_ingredients.md);
4.  expand ingredients to products with
    [`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md);
5.  refine products using
    [`get_clinical_attributes()`](https://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md)
    and
    [`filter_products_by_route()`](https://www.stevenmsmith.org/rxref/reference/filter_products_by_route.md);
6.  map between RxCUIs and NDCs with
    [`map_ndc_to_rxcui()`](https://www.stevenmsmith.org/rxref/reference/map_ndc_to_rxcui.md)
    and
    [`map_rxcui_to_ndc()`](https://www.stevenmsmith.org/rxref/reference/map_rxcui_to_ndc.md);
7.  use
    [`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md)
    when a compact end-to-end workflow is sufficient; and
8.  query drug-class information with the RxClass helpers when needed.

These tools are intended to support transparent, auditable, and
reproducible medication-list construction while keeping outputs tidy and
compatible with common `tidyverse` workflows.
