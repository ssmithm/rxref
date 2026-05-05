# Summarize RxClass relationship types for drugs

`rxclass_relationships()` is a diagnostic helper that evaluates the
type(s) of relationship (rela) asserted in RxNorm MED-RT data and the
number within each relationship asserted. In some cases,
[`get_medrt()`](http://www.stevenmsmith.org/rxref/reference/get_medrt.md)
or its various convenience wrappers may return empty or
shorter-than-expected tibbles because some relationships expected by the
user are not asserted in RxNorm (for example, an MOA may not be asserted
by MED-RT).

## Usage

``` r
rxclass_relationships(x, by = c("rxcui", "name"), rela_source = NULL)
```

## Arguments

- x:

  Character vector of RxCUIs or drug names.

- by:

  One of `"rxcui"` or `"name"`.

- rela_source:

  Optional RxClass relationship source filter.

## Value

A tibble summarizing available relationship/class-type combinations.
