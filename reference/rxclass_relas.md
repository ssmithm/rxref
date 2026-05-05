# Get RxClass relationship types

`rxclass_relas()` is a helper function that queries the RxClass API for
generally valid relationships within the data. Note that just because a
relationship is generally valid within a rela_source, not every drug or
RxCUI will have a corresponding relationship asserted.

## Usage

``` r
rxclass_relas(rela_source)
```

## Arguments

- rela_source:

  RxClass relationship source, such as `"ATC"`, `"DAILYMED"`,
  `"FDASPL"`, `"MEDRT"`, `"SNOMEDCT"`, or `"VA"`.

## Value

A tibble of relationship names for the selected source.
