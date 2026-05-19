# rxref 0.4.0

## New features

* Added support for retrieving active and historical RxNorm product concepts in product discovery workflows, including `products_for_ingredients()` and related search helpers.
* Added `concept_status` support to allow users to request active concepts only or active plus historical RxNorm concepts, where supported.
* Added `exclude_products_with_ingredients()`, a product-level filtering helper for removing RxNorm products that contain user-specified ingredients.
* `exclude_products_with_ingredients()` supports fixed-dose combination products represented with semicolon-delimited ingredient names or RxCUIs, such as `"atenolol; chlorthalidone"`.
* Added safer internal handling for historical concept verification and lookups, reducing failures when RxNorm returns incomplete, empty, or unexpected API responses.

## Improvements

* Improved retrieval of clinical product attributes for historical RxCUIs by attempting to recover dose form, dose form group, route, ingredient count, and ingredient metadata when available.
* Improved route filtering workflows for products that include historical RxNorm concepts.
* Improved fixed-dose combination formatting in `get_clinical_attributes()`. Ingredient fields such as `ingredient_rxcui`, `ingredient_name`, and `ingredient_tty` now use semicolon-delimited strings instead of pseudo-vector strings.
* Improved consistency of product-level ingredient metadata to support cleaner downstream joins and filtering.
* Improved reuse of cached API responses in product discovery, historical concept lookup, and clinical attribute workflows to reduce repeated calls to the RxNorm and RxClass APIs.
* Added or improved progress reporting for longer-running product discovery workflows, including API retrieval steps where appropriate.
* Updated route-specific vignette example datasets so package vignettes do not rely on live API calls during package checks.
* Updated vignette example datasets to avoid duplicated `.x` / `.y` join columns and to use current, singular column naming conventions.
* Updated README examples and package vignettes to reflect current function behavior, argument names, output columns, and recommended workflows.
* Improved internal helper functions for normalizing product ingredient fields and handling fixed-dose combination products.

## Bug fixes

* Fixed issues where historical or obsolete RxCUIs could return incomplete clinical attributes and be unintentionally dropped from route-filtered workflows.
* Fixed cases where historical product concepts could produce incorrect or inflated ingredient counts.
* Fixed validation and test coverage for `concept_status` arguments.
* Fixed issues arising from incomplete or malformed historical concept lookups.
* Fixed vignette data-generation issues that could create unexpected `NA` rows in pre-built example datasets.
* Fixed CRAN submission issues related to API name formatting, web references, and package citation handling.

## Documentation

* Updated package documentation to describe active and historical RxNorm concept support.
* Updated documentation for product discovery, clinical attribute retrieval, route filtering, and fixed-dose combination handling.
* Updated NEWS, README, vignettes, and CRAN-facing metadata for the 0.4.0 release.
* Updated package citation handling to avoid errors when the package is not installed.

# rxref 0.3.0

* Removed the experimental `set_backend()` helper. `rxref` currently uses the RxNorm/RxClass APIs directly.
