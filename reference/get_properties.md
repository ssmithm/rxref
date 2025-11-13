# Get core RxNorm properties for one or more RxCUIs

Get core RxNorm properties for one or more RxCUIs

## Usage

``` r
get_properties(rxcui)
```

## Arguments

- rxcui:

  Character vector of RxCUIs

## Value

tibble with rxcui, name, synonym, tty, language, suppress, umlscui.

## Examples

``` r
if (identical(Sys.getenv("RXREF_ONLINE"), "1")) {
get_properties(c("860975","1049630"))
}
```
