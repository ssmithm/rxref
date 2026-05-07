# Get core RxNorm properties for one or more RxCUIs

Get core RxNorm properties for one or more RxCUIs

## Usage

``` r
get_properties(rxcui, show_progress = interactive())
```

## Arguments

- rxcui:

  Character vector of RxCUIs

- show_progress:

  Logical. Show a progress bar in interactive sessions.

## Value

tibble with rxcui, name, synonym, tty, language, suppress, umlscui.

## Examples

``` r
if (identical(Sys.getenv("RXREF_ONLINE"), "1")) {
get_properties(c("860975","1049630"))
}
```
