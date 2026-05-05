# Find RxClass drug classes by class name

Find RxClass drug classes by class name

## Usage

``` r
find_classes(query, class_types = NULL)
```

## Arguments

- query:

  Character string to search for.

- class_types:

  Optional class type filter, such as `"EPC"`, `"ATC1"`, `"ATC2"`,
  `"ATC3"`, `"ATC4"`, `"CHEM"`, `"DISEASE"`, or `"VA"`.

## Value

A tibble of matching class concepts.
