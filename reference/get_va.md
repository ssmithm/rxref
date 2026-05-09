# Get VA drug classes for RxNorm drugs

`get_va()` is a convenience wrapper for
[`get_classes()`](https://www.stevenmsmith.org/rxref/reference/get_classes.md)
that returns VA drug class assertions for RxNorm drugs.

## Usage

``` r
get_va(
  x,
  by = c("rxcui", "name"),
  extended = TRUE,
  keep_input = TRUE,
  show_progress = interactive()
)
```

## Arguments

- x:

  Character vector of RxCUIs or drug names.

- by:

  One of `"rxcui"` or `"name"`.

- extended:

  Logical; if `TRUE`, includes extended VA class assertions.

- keep_input:

  Logical; if `TRUE`, includes the original input value.

- show_progress:

  Logical. Show a progress bar in interactive sessions. Progress is
  shown only when at least 5 inputs are supplied.

## Value

A tibble of VA class assertions.
