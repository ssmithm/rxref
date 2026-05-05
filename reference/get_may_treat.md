# Get indication/treatment assertions from MED-RT

`get_may_treat()` is a convenience wrapper for
[`get_medrt()`](http://www.stevenmsmith.org/rxref/reference/get_medrt.md)
that returns indications (disease assertions) from MED-RT for RxNorm
drugs, when available. Note that absence of a row (or an empty table
altogether) should not necessarily be interpreted as absence of an
indication.

## Usage

``` r
get_may_treat(x, by = c("rxcui", "name"), keep_input = TRUE)
```

## Arguments

- x:

  Character vector of RxCUIs or drug names.

- by:

  One of `"rxcui"` or `"name"`.

- keep_input:

  Logical; if `TRUE`, includes the original input value.

## Value

A tibble of MED-RT may-treat assertions.
