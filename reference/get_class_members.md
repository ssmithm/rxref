# Get RxNorm members of an RxClass class

`get_class_members()` queries the RxClass API to identify RxNorm drugs
associated with a specific drug class. Depending on the relationship
source, returned members may represent ingredients, clinical drugs,
branded products, or other RxNorm concepts.

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

  Character string identifying an RxClass class.

- rela_source:

  RxClass relationship source, such as `"ATC"`, `"ATCPROD"`,
  `"DAILYMED"`, `"FDASPL"`, `"MEDRT"`, `"SNOMEDCT"`, or `"VA"`.

- rela:

  Optional character vector of RxClass relationship types. When multiple
  relationships are supplied, each is queried separately and the results
  are combined, preserving the relationship associated with each
  returned member. If `NULL`, no relationship filter is sent.

- include_indirect:

  Logical. If `TRUE` (default), include direct and indirect members. If
  `FALSE`, return direct members only.

- ttys:

  Optional character vector of RxNorm term types to retrieve, such as
  `"IN"`, `"PIN"`, `"SCD"`, or `"SBD"`.

## Value

A tibble containing RxNorm class members, including their RxCUIs, names,
term types, class identifiers, relationship sources, and relationships.
Additional columns describe source-specific membership attributes when
available.

## Details

RxClass organizes drug-class membership using source-specific
relationships. For ATC and ATCPROD, the `rela` argument can generally be
omitted. Other sources, such as MEDRT and VA, use named relationships
that may need to be specified explicitly.

For example, MEDRT associates drugs with diseases through distinct
relationships, including `may_treat` and `CI_with`. These represent
different clinical assertions and should not be interpreted as
equivalent.

Use
[`rxclass_relas()`](https://www.stevenmsmith.org/rxref/reference/rxclass_relas.md)
to identify relationship types supported by a source.

Multiple relationships can be supplied as a character vector. Each
relationship is queried separately, and results are combined while
preserving the originating relationship in the `rela` column.

If `rela = NULL` (default), no relationship filter is sent to the API.
For non-ATC sources, a warning is issued when this produces no members,
since an omitted relationship may explain the empty result.

## See also

[`find_classes()`](https://www.stevenmsmith.org/rxref/reference/find_classes.md),
[`get_classes()`](https://www.stevenmsmith.org/rxref/reference/get_classes.md),
[`rxclass_relas()`](https://www.stevenmsmith.org/rxref/reference/rxclass_relas.md)

## Examples

``` r
if (identical(Sys.getenv("RXREF_ONLINE"), "1")) {
  # ATC class membership
  get_class_members("C02", "ATCPROD")

  # MED-RT: drugs associated with treating hypertension
  get_class_members("D006973", "MEDRT", rela = "may_treat")

  # MED-RT: retrieve multiple relationships
  get_class_members(
    "D006973", "MEDRT",
    rela = c("may_treat", "CI_with")
  )

  # VA drug class membership
  get_class_members("CV400", "VA", rela = "has_VAClass")
}
```
