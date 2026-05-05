# Get RxNorm members of an RxClass class

`get_class_members()` queries the RxClass API to identify members of a
specific drug class. Typically, this would be used to identify all
ingredient RxCUIs in a class, which can then be combined with other
search functions to extract related product RxCUIs or NDCs.

## Usage

``` r
get_class_members(
  class_id,
  rela_source,
  rela = NULL,
  include_indirect = TRUE,
  ttys = NULL
)
```

## Arguments

- class_id:

  RxClass class identifier.

- rela_source:

  RxClass relationship source, such as `"ATC"`, `"ATCPROD"`,
  `"DAILYMED"`, `"FDASPL"`, `"MEDRT"`, `"SNOMEDCT"`, or `"VA"`.

- rela:

  Optional relationship filter.

- include_indirect:

  Logical; if `TRUE`, include direct and indirect members. If `FALSE`,
  return direct members only.

- ttys:

  Optional RxNorm term type filter, such as `"IN"`, `"PIN"`, `"SCD"`, or
  `"SBD"`.

## Value

A tibble of RxNorm drug members.
