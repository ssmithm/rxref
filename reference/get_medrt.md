# Get MED-RT assertions for RxNorm drugs

`get_medrt()` is a convenience wrapper for
[`get_classes()`](https://www.stevenmsmith.org/rxref/reference/get_classes.md)
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
  keep_input = TRUE,
  show_progress = interactive()
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

- show_progress:

  Logical. Show a progress bar in interactive sessions. Progress is
  shown only when at least 5 inputs are supplied.

## Value

A tibble of MED-RT assertions.
