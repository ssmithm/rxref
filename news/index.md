# Changelog

## rxref 0.5.0

### Improvements

- Added cache management helpers:
  [`rxref_cache()`](https://www.stevenmsmith.org/rxref/reference/rxref_cache.md),
  [`rxref_cache_info()`](https://www.stevenmsmith.org/rxref/reference/rxref_cache.md),
  [`rxref_cache_clear()`](https://www.stevenmsmith.org/rxref/reference/rxref_cache.md),
  [`rxref_cache_use_memory()`](https://www.stevenmsmith.org/rxref/reference/rxref_cache.md),
  [`rxref_cache_use_disk()`](https://www.stevenmsmith.org/rxref/reference/rxref_cache.md),
  and
  [`rxref_cache_disable()`](https://www.stevenmsmith.org/rxref/reference/rxref_cache.md).
  These helpers make it easier to inspect, clear, persist, or disable
  rxref’s memoised RxNorm/RxClass API-response cache.
- Revised vignettes to better distinguish use cases for `rxref`, cleaned
  up vignette data bundling and removed potentially confusing avoidance
  of API calls during vignette building from the vignettes themselves.

### API and error-handling improvements

- Improved API error-handling consistency across `rxref`. Centralized
  `rxref` API condition classes are now used more consistently for
  connection errors, rate limits, server errors, empty responses,
  malformed JSON, and other unsuccessful HTTP responses from the NLM.
- Added internal optional-API handling helpers to return fallback values
  with informative warnings as opposed to silent API failures.
- Updated NDC status retrieval, clinical attribute enrichment,
  historical clinical fallback, and product-search fallback logic to
  distinguish required API calls from optional enrichment calls.

## rxref 0.4.0

CRAN release: 2026-05-27

### New features

- Added support for retrieving active and historical RxNorm product
  concepts in product discovery workflows, including
  [`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
  and related search helpers.
- Added `concept_status` support to allow users to request active
  concepts only or active plus historical RxNorm concepts, where
  supported.
- Added
  [`exclude_products_with_ingredients()`](https://www.stevenmsmith.org/rxref/reference/exclude_products_with_ingredients.md),
  a product-level filtering helper for removing RxNorm products that
  contain user-specified ingredients.
- [`exclude_products_with_ingredients()`](https://www.stevenmsmith.org/rxref/reference/exclude_products_with_ingredients.md)
  supports fixed-dose combination products represented with
  semicolon-delimited ingredient names or RxCUIs, such as
  `"atenolol; chlorthalidone"`.
- Added safer internal handling for historical concept verification and
  lookups, reducing failures when RxNorm returns incomplete, empty, or
  unexpected API responses.

### Improvements

- Improved retrieval of clinical product attributes for historical
  RxCUIs by attempting to recover dose form, dose form group, route,
  ingredient count, and ingredient metadata when available.
- Improved route filtering workflows for products that include
  historical RxNorm concepts.
- Improved fixed-dose combination formatting in
  [`get_clinical_attributes()`](https://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md).
  Ingredient fields such as `ingredient_rxcui`, `ingredient_name`, and
  `ingredient_tty` now use semicolon-delimited strings instead of
  pseudo-vector strings.
- Improved consistency of product-level ingredient metadata to support
  cleaner downstream joins and filtering.
- Improved reuse of cached API responses in product discovery,
  historical concept lookup, and clinical attribute workflows to reduce
  repeated calls to the RxNorm and RxClass APIs.
- Added or improved progress reporting for longer-running product
  discovery workflows, including API retrieval steps where appropriate.
- Updated route-specific vignette example datasets so package vignettes
  do not rely on live API calls during package checks.
- Updated vignette example datasets to avoid duplicated `.x` / `.y` join
  columns and to use current, singular column naming conventions.
- Updated README examples and package vignettes to reflect current
  function behavior, argument names, output columns, and recommended
  workflows.
- Improved internal helper functions for normalizing product ingredient
  fields and handling fixed-dose combination products.

### Bug fixes

- Fixed issues where historical or obsolete RxCUIs could return
  incomplete clinical attributes and be unintentionally dropped from
  route-filtered workflows.
- Fixed cases where historical product concepts could produce incorrect
  or inflated ingredient counts.
- Fixed validation and test coverage for `concept_status` arguments.
- Fixed issues arising from incomplete or malformed historical concept
  lookups.
- Fixed vignette data-generation issues that could create unexpected
  `NA` rows in pre-built example datasets.
- Fixed CRAN submission issues related to API name formatting, web
  references, and package citation handling.

### Documentation

- Updated package documentation to describe active and historical RxNorm
  concept support.
- Updated documentation for product discovery, clinical attribute
  retrieval, route filtering, and fixed-dose combination handling.
- Updated NEWS, README, vignettes, and CRAN-facing metadata for the
  0.4.0 release.
- Updated package citation handling to avoid errors when the package is
  not installed.

## rxref 0.3.0

- Removed the experimental `set_backend()` helper. `rxref` currently
  uses the RxNorm/RxClass APIs directly.
