# Get FDA/SPL established pharmacologic classes

`get_epc()` is a convenience wrapper for
[`get_classes()`](http://www.stevenmsmith.org/rxref/reference/get_classes.md)
that returns FDA labeling class assertions for RxNorm drugs.

## Usage

``` r
get_epc(x, by = c("rxcui", "name"), keep_input = TRUE)
```

## Arguments

- x:

  Character vector of RxCUIs or drug names.

- by:

  One of `"rxcui"` or `"name"`.

- keep_input:

  Logical; if `TRUE`, includes the original input value.

## Value

A tibble of established pharmacologic class assertions.
