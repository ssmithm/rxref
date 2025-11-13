# Get RxNav status for NDCs

`ndc_status()` requests the status of an NDC entry for one or more
user-supplied NDC numbers. The returned status will be one "ACTIVE",
"OBSOLETE", or "UNSPECIFIED". On occasion, a NULL value may be pulled
(resulting in an `NA` in the returned tibble), when RxNorm has no status
for a given NDC, or if an invalid NDC is supplied.

## Usage

``` r
ndc_status(ndc)
```

## Arguments

- ndc:

  character vector; hyphenated or digits

## Value

A tibble with three columns: ndc_input, ndc11, ndc_hyph, status
