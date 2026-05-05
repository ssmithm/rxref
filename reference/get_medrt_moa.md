# Get MED-RT mechanism-of-action assertions

`get_medrt_moa()` is a convenience wrapper for
[`get_medrt()`](http://www.stevenmsmith.org/rxref/reference/get_medrt.md)
that returns MED-RT mechanism-of-action assertions when available. Not
all drugs have MED-RT has_moa assertions; for some drugs, mechanism-like
or class-like information may instead be available from FDA/SPL EPC via
[`get_epc()`](http://www.stevenmsmith.org/rxref/reference/get_epc.md) or
as physiologic effects via
[`get_medrt_pe()`](http://www.stevenmsmith.org/rxref/reference/get_medrt_pe.md).

## Usage

``` r
get_medrt_moa(x, by = c("rxcui", "name"), keep_input = TRUE)
```

## Arguments

- x:

  Character vector of RxCUIs or drug names.

- by:

  One of `"rxcui"` or `"name"`.

- keep_input:

  Logical; if `TRUE`, includes the original input value.

## Value

A tibble of MED-RT mechanism-of-action assertions.
