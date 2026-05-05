# Get MED-RT assertions for RxNorm drugs

`get_medrt()` is a convenience wrapper for
[`get_classes()`](http://www.stevenmsmith.org/rxref/reference/get_classes.md)
that returns MED-RT assertions for RxNorm drugs. These include, for
example, asserted mechanisms of action, contraindications, physiologic
effects (including adverse side effects), etc.

## Usage

``` r
get_medrt(
  x,
  by = c("rxcui", "name"),
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

- relas:

  Optional MED-RT relationship filter.

- class_types:

  Optional MED-RT class type filter.

- keep_input:

  Logical; if `TRUE`, includes the original input value.

## Value

A tibble of MED-RT assertions.
