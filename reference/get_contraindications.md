# Get contraindication assertions from MED-RT

`get_contraindications()` is a convenience wrapper for
[`get_medrt()`](http://www.stevenmsmith.org/rxref/reference/get_medrt.md)
that returns only contraindication MED-RT assertions for RxNorm drugs.

## Usage

``` r
get_contraindications(x, by = c("rxcui", "name"), keep_input = TRUE)
```

## Arguments

- x:

  Character vector of RxCUIs or drug names.

- by:

  One of `"rxcui"` or `"name"`.

- keep_input:

  Logical; if `TRUE`, includes the original input value.

## Value

A tibble of MED-RT contraindication assertions.
