# Get RxClass assertions for RxNorm drugs

**\[experimental\]**

`get_classes()` returns RxClass assertions for RxNorm concepts or drug
names. These include drug-class relationships from sources such as ATC,
FDASPL, DAILYMED, MEDRT, SNOMEDCT, and VA. Depending on the source and
relationship, returned assertions may represent pharmacologic classes,
chemical structures, mechanisms of action, physiologic effects,
indications, contraindications, VA classes, ATC classes, or SNOMED CT
dispositions/structures.

Because different sources use different classification logic, this
function preserves class type, relationship, and relationship source
rather than collapsing results into a single class label.

## Usage

``` r
get_classes(
  x,
  by = c("rxcui", "name"),
  rela_source = NULL,
  relas = NULL,
  class_types = NULL,
  keep_input = TRUE
)
```

## Arguments

- x:

  Character vector of RxCUIs or drug names.

- by:

  One of `"rxcui"` or `"name"`.

- rela_source:

  Optional RxClass relationship source filter, such as `"ATC"`,
  `"ATCPROD"`, `"DAILYMED"`, `"FDASPL"`, `"MEDRT"`, `"SNOMEDCT"`, or
  `"VA"`. May be a character vector; values are queried separately.

- relas:

  Optional relationship filter. Examples include `"has_EPC"`,
  `"has_MoA"`, `"has_PE"`, `"may_treat"`, `"ci_with"`,
  `"has_chemical_structure"`, and others supported by RxClass.

- class_types:

  Optional filter on returned class types, such as `"EPC"`, `"MOA"`,
  `"PE"`, `"DISEASE"`, `"CHEM"`, `"VA"`, or `"ATC1-4"`.

- keep_input:

  Logical; if `TRUE`, includes the original input value.

## Value

A tibble with one row per RxClass assertion.
