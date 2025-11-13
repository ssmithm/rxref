# Configure rxref

Configure rxref

## Usage

``` r
rxref_conf(
  base_url = getOption("rxref.base_url"),
  rate_delay = getOption("rxref.rate_delay"),
  cache = getOption("rxref.cache")
)
```

## Arguments

- base_url:

  Override the RxNav base URL (e.g., a local mirror)

- rate_delay:

  Seconds to wait between HTTP calls

- cache:

  A cachem cache object used by memoised calls

## Value

A named list of current settings
