# rxref: Tidy RxNorm utilities

`rxref` provides tidy, API-first tools for working with RxNorm and
RxClass data in R. The package helps users resolve drug names and
identifiers, inspect RxNorm concepts, map between RxCUIs and NDCs,
expand ingredients to product-level concepts, retrieve drug class
information, and derive clinically useful product attributes.

## Details

Common workflows include:

- Resolving drug names or identifiers with
  [`resolve()`](https://www.stevenmsmith.org/rxref/reference/resolve.md)
  and
  [`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md).

- Retrieving concept metadata with
  [`get_properties()`](https://www.stevenmsmith.org/rxref/reference/get_properties.md).

- Building product-level medication lists with
  [`find_ingredients()`](https://www.stevenmsmith.org/rxref/reference/find_ingredients.md),
  [`ingredients_for_rxcui()`](https://www.stevenmsmith.org/rxref/reference/ingredients_for_rxcui.md),
  and
  [`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md).

- Mapping between RxCUIs and NDCs with
  [`map_to()`](https://www.stevenmsmith.org/rxref/reference/map_to.md),
  [`map_ndc_to_rxcui()`](https://www.stevenmsmith.org/rxref/reference/map_ndc_to_rxcui.md),
  and
  [`map_rxcui_to_ndc()`](https://www.stevenmsmith.org/rxref/reference/map_rxcui_to_ndc.md).

- Retrieving therapeutic class information with
  [`get_classes()`](https://www.stevenmsmith.org/rxref/reference/get_classes.md),
  [`find_classes()`](https://www.stevenmsmith.org/rxref/reference/find_classes.md),
  [`get_class_members()`](https://www.stevenmsmith.org/rxref/reference/get_class_members.md),
  [`get_atc()`](https://www.stevenmsmith.org/rxref/reference/get_atc.md),
  [`get_epc()`](https://www.stevenmsmith.org/rxref/reference/get_epc.md),
  and
  [`get_va()`](https://www.stevenmsmith.org/rxref/reference/get_va.md).

- Deriving clinical product attributes with
  [`get_clinical_attributes()`](https://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md)
  and
  [`filter_products_by_route()`](https://www.stevenmsmith.org/rxref/reference/filter_products_by_route.md).

Configure package behavior with
[`rxref_conf()`](https://www.stevenmsmith.org/rxref/reference/rxref_conf.md).

## See also

Useful links:

- <https://github.com/ssmithm/rxref>

- <https://www.stevenmsmith.org/rxref/>

- Report bugs at <https://github.com/ssmithm/rxref/issues>

## Author

**Maintainer**: Steven Smith <ssmith@cop.ufl.edu>
