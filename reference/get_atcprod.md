# Get ATC product-level classes for RxNorm drugs

[`get_atc()`](http://www.stevenmsmith.org/rxref/reference/get_atc.md) is
a convenience wrapper for
[`get_classes()`](http://www.stevenmsmith.org/rxref/reference/get_classes.md)
that returns Anatomical Therapeutic Chemical (ATC) product-level class
assertions for RxNorm drugs.

## Usage

``` r
get_atcprod(
  x,
  by = c("rxcui", "name"),
  keep_input = TRUE,
  show_progress = interactive()
)
```

## Arguments

- x:

  Character vector of RxCUIs or drug names.

- by:

  One of `"rxcui"` or `"name"`.

- keep_input:

  Logical; if `TRUE`, includes the original input value.

- show_progress:

  Logical. Show a progress bar in interactive sessions. Progress is
  shown only when at least 5 inputs are supplied.

## Value

A tibble of ATCPROD class assertions.
