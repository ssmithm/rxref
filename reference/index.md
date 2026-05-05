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

## RxNav Search & catalog utilities

- [`find_ingredients()`](http://www.stevenmsmith.org/rxref/reference/find_ingredients.md)
  : Resolve a free-text drug name to ingredient CUIs (IN/PIN)
- [`ingredients_for_rxcui()`](http://www.stevenmsmith.org/rxref/reference/ingredients_for_rxcui.md)
  : Get ingredient concepts for RxCUIs
- [`products_for_ingredients()`](http://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
  : Expand ingredient CUIs to product CUIs that truly contain the
  ingredient
- [`search_drug()`](http://www.stevenmsmith.org/rxref/reference/search_drug.md)
  : Search free-text drug name and return product CUIs and/or NDCs
- [`tty_catalogue()`](http://www.stevenmsmith.org/rxref/reference/tty_catalogue.md)
  : Catalogue of RxNorm TTY (Term Types)

## RxClass Search & catalog utilities

- [`find_classes()`](http://www.stevenmsmith.org/rxref/reference/find_classes.md)
  : Find RxClass drug classes by class name
- [`get_atc()`](http://www.stevenmsmith.org/rxref/reference/get_atc.md)
  : Get ATC classes for RxNorm drugs
- [`get_atcprod()`](http://www.stevenmsmith.org/rxref/reference/get_atcprod.md)
  : Get ATC product-level classes for RxNorm drugs
- [`get_chemical_structure()`](http://www.stevenmsmith.org/rxref/reference/get_chemical_structure.md)
  : Get chemical-structure assertions
- [`get_class_members()`](http://www.stevenmsmith.org/rxref/reference/get_class_members.md)
  : Get RxNorm members of an RxClass class
- [`get_classes()`](http://www.stevenmsmith.org/rxref/reference/get_classes.md)
  **\[experimental\]** : Get RxClass assertions for RxNorm drugs
- [`get_contraindications()`](http://www.stevenmsmith.org/rxref/reference/get_contraindications.md)
  : Get contraindication assertions from MED-RT
- [`get_drug_classes()`](http://www.stevenmsmith.org/rxref/reference/get_drug_classes.md)
  **\[experimental\]** : Get class-like RxClass assertions for RxNorm
  drugs
- [`get_epc()`](http://www.stevenmsmith.org/rxref/reference/get_epc.md)
  : Get FDA/SPL established pharmacologic classes
- [`get_may_treat()`](http://www.stevenmsmith.org/rxref/reference/get_may_treat.md)
  : Get indication/treatment assertions from MED-RT
- [`get_medrt()`](http://www.stevenmsmith.org/rxref/reference/get_medrt.md)
  : Get MED-RT assertions for RxNorm drugs
- [`get_medrt_moa()`](http://www.stevenmsmith.org/rxref/reference/get_medrt_moa.md)
  : Get MED-RT mechanism-of-action assertions
- [`get_medrt_pe()`](http://www.stevenmsmith.org/rxref/reference/get_medrt_pe.md)
  : Get MED-RT physiologic-effect assertions
- [`get_va()`](http://www.stevenmsmith.org/rxref/reference/get_va.md) :
  Get VA drug classes for RxNorm drugs
- [`rxclass_rela_sources()`](http://www.stevenmsmith.org/rxref/reference/rxclass_rela_sources.md)
  : Get RxClass relationship sources
- [`rxclass_relas()`](http://www.stevenmsmith.org/rxref/reference/rxclass_relas.md)
  : Get RxClass relationship types
- [`rxclass_relationships()`](http://www.stevenmsmith.org/rxref/reference/rxclass_relationships.md)
  : Summarize RxClass relationship types for drugs

## Clinical utilities

- [`get_clinical_attributes()`](http://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md)
  : Clinical attributes from the concept (SCD/SBD) or related SCD/SBD

## Configurations

- [`set_backend()`](http://www.stevenmsmith.org/rxref/reference/set_backend.md)
  : Select backend (API-only for now)
- [`rxref_conf()`](http://www.stevenmsmith.org/rxref/reference/rxref_conf.md)
  : Configure rxref
