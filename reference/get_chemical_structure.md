# Get chemical-structure assertions

`get_chemical_structure()` is a convenience wrapper for
[`get_classes()`](http://www.stevenmsmith.org/rxref/reference/get_classes.md)
that returns chemical-structure assertions for RxNorm drugs.

## Usage

``` r
get_chemical_structure(
  x,
  by = c("rxcui", "name"),
  rela_source = c("FDASPL", "DAILYMED", "MEDRT"),
  keep_input = TRUE
)
```

## Arguments

- x:

  Character vector of RxCUIs or drug names.

- by:

  One of `"rxcui"` or `"name"`.

- rela_source:

  Relationship source. Defaults to FDASPL, DAILYMED, and MEDRT.

- keep_input:

  Logical; if `TRUE`, includes the original input value.

## Value

A tibble of chemical-structure assertions.
