# Get RxNav status for NDCs

`ndc_status()` requests the status of an NDC entry for one or more
user-supplied NDC numbers. The returned status will be one "ACTIVE",
"OBSOLETE", or "UNSPECIFIED". On occasion, a NULL value may be pulled
(resulting in an `NA` in the returned tibble), when RxNorm has no status
for a given NDC, or if an invalid NDC is supplied.

## Usage

``` r
ndc_status(ndc, show_progress = interactive())
```

## Arguments

- ndc:

  character vector; hyphenated or digits

- show_progress:

  Logical. Show a progress bar in interactive sessions. Progress is
  shown only when at least 5 inputs are supplied.

## Value

A tibble with three columns: ndc_input, ndc11, ndc_hyph, status
