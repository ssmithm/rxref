# Package index

## Core verbs

- [`resolve()`](http://www.stevenmsmith.org/rxref/reference/resolve.md)
  : Resolve free text, RxCUI, or NDC to RxCUI and preferred name
- [`get_properties()`](http://www.stevenmsmith.org/rxref/reference/get_properties.md)
  : Get core RxNorm properties for one or more RxCUIs
- [`map_to()`](http://www.stevenmsmith.org/rxref/reference/map_to.md)
  [`map_ndc_to_rxcui()`](http://www.stevenmsmith.org/rxref/reference/map_to.md)
  [`map_rxcui_to_ndc()`](http://www.stevenmsmith.org/rxref/reference/map_to.md)
  : Map between NDC and RxCUI in either direction
- [`ndc_status()`](http://www.stevenmsmith.org/rxref/reference/ndc_status.md)
  : Get RxNav status for NDCs

## RxCUI\<–\>NDC mapping

- [`map_to()`](http://www.stevenmsmith.org/rxref/reference/map_to.md)
  [`map_ndc_to_rxcui()`](http://www.stevenmsmith.org/rxref/reference/map_to.md)
  [`map_rxcui_to_ndc()`](http://www.stevenmsmith.org/rxref/reference/map_to.md)
  : Map between NDC and RxCUI in either direction

## Search & catalog utilities

- [`find_ingredients()`](http://www.stevenmsmith.org/rxref/reference/find_ingredients.md)
  : Resolve a free-text drug name to ingredient CUIs (IN/PIN)
- [`products_for_ingredients()`](http://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
  : Expand ingredient CUIs to product CUIs that truly contain the
  ingredient
- [`search_drug()`](http://www.stevenmsmith.org/rxref/reference/search_drug.md)
  : Search free-text drug name and return product CUIs and/or NDCs
- [`tty_catalogue()`](http://www.stevenmsmith.org/rxref/reference/tty_catalogue.md)
  : Catalogue of RxNorm TTY (Term Types)

## Clinical utilities

- [`get_clinical_attributes()`](http://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md)
  : Clinical attributes from the concept (SCD/SBD) or related SCD/SBD

## Configurations

- [`set_backend()`](http://www.stevenmsmith.org/rxref/reference/set_backend.md)
  : Select backend (API-only for now)
- [`rxref_conf()`](http://www.stevenmsmith.org/rxref/reference/rxref_conf.md)
  : Configure rxref
