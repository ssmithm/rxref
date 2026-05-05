# Get MED-RT physiologic-effect assertions

`get_medrt_pe()` is a convenience wrapper for
[`get_medrt()`](http://www.stevenmsmith.org/rxref/reference/get_medrt.md)
that returns only physiologic effect MED-RT assertions for RxNorm drugs.

## Usage

``` r
get_medrt_pe(x, by = c("rxcui", "name"), keep_input = TRUE)
```

## Arguments

- x:

  Character vector of RxCUIs or drug names.

- by:

  One of `"rxcui"` or `"name"`.

- keep_input:

  Logical; if `TRUE`, includes the original input value.

## Value

A tibble of MED-RT physiologic-effect assertions.
