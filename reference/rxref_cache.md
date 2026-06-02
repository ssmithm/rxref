# Manage the rxref cache

`rxref_cache()` returns the currently configured cache object.
`rxref_cache_info()` returns a small summary of the current cache.
`rxref_cache_clear()` clears cached RxNorm/RxClass API responses.
`rxref_cache_use_memory()` switches rxref to an in-memory cache.
`rxref_cache_use_disk()` switches rxref to an on-disk cache.
`rxref_cache_disable()` uses an immediately expiring cache, effectively
disabling cache reuse for the current R session.

## Usage

``` r
rxref_cache()

rxref_cache_info()

rxref_cache_clear()

rxref_cache_use_memory(
  max_size = 200 * 1024^2,
  max_age = Inf,
  evict = c("lru", "fifo")
)

rxref_cache_use_disk(
  dir = rxref_cache_dir(),
  max_size = 1024^3,
  max_age = Inf,
  evict = c("lru", "fifo")
)

rxref_cache_disable()
```

## Arguments

- max_size:

  Maximum cache size in bytes.

- max_age:

  Maximum age of cached objects, in seconds.

- evict:

  Cache eviction policy passed to
  [`cachem::cache_mem()`](https://cachem.r-lib.org/reference/cache_mem.html).

- dir:

  Directory to use for an on-disk cache.

## Value

`rxref_cache()` returns a `cachem` cache object.

`rxref_cache_info()` returns a tibble with cache metadata.

`rxref_cache_clear()`, `rxref_cache_use_memory()`,
`rxref_cache_use_disk()`, and `rxref_cache_disable()` invisibly return
the configured cache object.

## Details

rxref caches memoised API responses through a `cachem` cache object
stored in `getOption("rxref.cache")`. By default, rxref uses an
in-memory cache for the current R session.

## Examples

``` r
rxref_cache_info()
#> # A tibble: 1 × 3
#>   cache_class      n_keys size_bytes
#>   <chr>             <int>      <dbl>
#> 1 cache_mem/cachem      0          0

rxref_cache_clear()

rxref_cache_use_memory()

if (FALSE) { # \dontrun{
rxref_cache_use_disk()
} # }
```
