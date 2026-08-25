# Case Study: GLP-1 and related incretin therapies

## Case

Suppose we need to identify users of GLP-1 receptor agonists and related
incretin-based therapies from EHR prescribing data, pharmacy claims
data, or both. To accomplish this, we need a reproducible medication
definition that identifies relevant RxNorm product concepts and, when
needed, corresponding National Drug Codes (NDCs).

This case study builds that definition in four steps:

1.  define the ingredients of interest;
2.  resolve the ingredient names to RxNorm ingredient RxCUIs;
3.  expand those ingredients to product-level RxNorm concepts; and
4.  map the final product concepts to NDCs.

We first work through these steps explicitly. At the end, we show how
[`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md)
can provide a more compact version of the same common workflow.

The displayed results use precomputed data bundled with `rxref`, so the
vignette can be built without requiring access to the live RxNorm API.
The code shown below is the code users can run interactively to generate
the results from the current API.

## Define the ingredient list

For this example, we start with a list of GLP-1 receptor agonists and
related incretin-based therapies:

- exenatide
- liraglutide
- lixisenatide
- dulaglutide
- albiglutide
- semaglutide
- tirzepatide

Tirzepatide is included because many applied studies group it with
GLP-1-based incretin therapies, even though it is technically a dual
GIP/GLP-1 receptor agonist rather than a standalone GLP-1 receptor
agonist.

``` r

glp1_names <- c(
  "semaglutide",
  "exenatide",
  "liraglutide",
  "lixisenatide",
  "dulaglutide",
  "albiglutide",
  "tirzepatide"
)

glp1_names
#> [1] "semaglutide"  "exenatide"    "liraglutide"  "lixisenatide" "dulaglutide" 
#> [6] "albiglutide"  "tirzepatide"
```

## Step 1: Identify ingredient RxCUIs

[`find_ingredients()`](https://www.stevenmsmith.org/rxref/reference/find_ingredients.md)
resolves free-text drug names to ingredient-level RxNorm concepts. For
this medication definition, we retain TTY = `"IN"`, which represents the
base RxNorm ingredient concept.

``` r

glp1_ingredients <- find_ingredients(glp1_names) |>
  filter(tty == "IN") |>
  distinct(
    input,
    ingredient_rxcui = rxcui,
    ingredient_name = name,
    ingredient_tty = tty
  )

glp1_ingredients
```

    #> # A tibble: 7 × 4
    #>   input        ingredient_rxcui ingredient_name ingredient_tty
    #>   <chr>        <chr>            <chr>           <chr>         
    #> 1 semaglutide  1991302          semaglutide     IN            
    #> 2 exenatide    60548            exenatide       IN            
    #> 3 liraglutide  475968           liraglutide     IN            
    #> 4 lixisenatide 1440051          lixisenatide    IN            
    #> 5 dulaglutide  1551291          dulaglutide     IN            
    #> 6 albiglutide  1534763          albiglutide     IN            
    #> 7 tirzepatide  2601723          tirzepatide     IN

This step is useful for checking that each study-defined ingredient maps
to the intended RxNorm concept before expanding the definition to
products. For unfamiliar term types,
[`tty_catalogue()`](https://www.stevenmsmith.org/rxref/reference/tty_catalogue.md)
provides a description of common RxNorm TTYs.

## Step 2: Expand ingredients to product RxCUIs

Next,
[`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
identifies product concepts that contain the selected ingredients.

By default, `product_ttys("default")` includes the product-focused TTYs
SCD, SBD, GPCK, and BPCK. These are generally the most useful concepts
when the goal is to identify prescribable or dispensable products and
eventually map them to NDCs.

``` r

product_ttys("default")
#> [1] "SCD"  "SBD"  "GPCK" "BPCK"
```

For this example, we retain fixed-dose combination products and use only
currently active RxNorm concepts.

``` r

glp1_products <- products_for_ingredients(
  glp1_ingredients$ingredient_rxcui,
  ttys = product_ttys("default"),
  include_combos = TRUE,
  concept_status = "active"
)

glp1_products
```

    #> # A tibble: 30 × 5
    #>    ingredient_rxcui product_rxcui name                       tty   n_ingredients
    #>    <chr>            <chr>         <chr>                      <chr>         <int>
    #>  1 1440051          1859000       3 ML insulin glargine 100… SBD               2
    #>  2 1440051          1858995       3 ML insulin glargine 100… SCD               2
    #>  3 1534763          1534820       0.5 ML albiglutide 100 MG… SCD               1
    #>  4 1534763          1534800       0.5 ML albiglutide 60 MG/… SCD               1
    #>  5 1551291          1551300       0.5 ML dulaglutide 1.5 MG… SBD               1
    #>  6 1551291          1551306       0.5 ML dulaglutide 3 MG/M… SBD               1
    #>  7 1551291          2395779       0.5 ML dulaglutide 6 MG/M… SBD               1
    #>  8 1551291          2395785       0.5 ML dulaglutide 9 MG/M… SBD               1
    #>  9 1551291          1551295       0.5 ML dulaglutide 1.5 MG… SCD               1
    #> 10 1551291          1551304       0.5 ML dulaglutide 3 MG/M… SCD               1
    #> # ℹ 20 more rows

### Inspect the product set

Before mapping products to NDCs, it is useful to inspect what was
returned. For example, we can summarize the product TTYs:

``` r

glp1_products |>
  count(tty, sort = TRUE)
#> # A tibble: 2 × 2
#>   tty       n
#>   <chr> <int>
#> 1 SBD      82
#> 2 SCD      55
```

Because `include_combos = TRUE`, the product set may also contain
fixed-dose combination products. `n_ingredients` can be used to identify
them:

``` r

glp1_products |>
  filter(n_ingredients > 1) |>
  select(ingredient_rxcui, product_rxcui, name, tty, n_ingredients) |>
  arrange(name) |>
  head(20)
#> # A tibble: 4 × 5
#>   ingredient_rxcui product_rxcui name                        tty   n_ingredients
#>   <chr>            <chr>         <chr>                       <chr>         <int>
#> 1 475968           1860167       3 ML insulin degludec 100 … SCD               2
#> 2 475968           1860172       3 ML insulin degludec 100 … SBD               2
#> 3 1440051          1858995       3 ML insulin glargine 100 … SCD               2
#> 4 1440051          1859000       3 ML insulin glargine 100 … SBD               2
```

Whether these products belong in the final exposure definition depends
on the study. The important point is to make that decision explicit
vs. assuming that every product containing an ingredient of interest
should automatically be retained.

## Step 3: Map product RxCUIs to NDCs

If the exposure data contain NDCs, the next step is to map the selected
product RxCUIs to corresponding NDCs.

Here we request currently active NDC associations and retain NDCs whose
reported status is `"ACTIVE"`.

``` r

glp1_ndc_map <- map_rxcui_to_ndc(
  unique(glp1_products$product_rxcui),
  history = "active",
  status = "ACTIVE"
)
```

We can then join the NDC mapping back to the product and ingredient
information to create a study-friendly medication list.

``` r

glp1_ndcs <- glp1_ndc_map |>
  left_join(
    glp1_products,
    by = c("rxcui" = "product_rxcui")
  ) |>
  left_join(
    glp1_ingredients |>
      select(ingredient_rxcui, ingredient_name),
    by = "ingredient_rxcui"
  ) |>
  distinct(
    ingredient_rxcui,
    ingredient_name,
    product_rxcui = rxcui,
    ndc11,
    ndc_status,
    product_name = name,
    tty
  ) |>
  arrange(ingredient_name, product_rxcui, ndc11)

glp1_ndcs |>
  head(30)
#> # A tibble: 30 × 7
#>    ingredient_rxcui ingredient_name product_rxcui ndc11  ndc_status product_name
#>    <chr>            <chr>           <chr>         <chr>  <chr>      <chr>       
#>  1 1551291          dulaglutide     1551300       00002… ACTIVE     0.5 ML dula…
#>  2 1551291          dulaglutide     1551300       00002… ACTIVE     0.5 ML dula…
#>  3 1551291          dulaglutide     1551300       00002… ACTIVE     0.5 ML dula…
#>  4 1551291          dulaglutide     1551300       50090… ACTIVE     0.5 ML dula…
#>  5 1551291          dulaglutide     1551300       50090… ACTIVE     0.5 ML dula…
#>  6 1551291          dulaglutide     1551306       00002… ACTIVE     0.5 ML dula…
#>  7 1551291          dulaglutide     1551306       00002… ACTIVE     0.5 ML dula…
#>  8 1551291          dulaglutide     1551306       00002… ACTIVE     0.5 ML dula…
#>  9 1551291          dulaglutide     1551306       50090… ACTIVE     0.5 ML dula…
#> 10 1551291          dulaglutide     1551306       50090… ACTIVE     0.5 ML dula…
#> # ℹ 20 more rows
#> # ℹ 1 more variable: tty <chr>
```

At this point, we have two useful study resources:

- `glp1_products`, for data represented with RxCUIs (EHR prescribing
  data, often); and
- `glp1_ndcs`, for data represented with NDCs (pharmacy fill data,
  often).

Keeping both tables can be useful when the same medication definition is
applied across multiple data sources.

## Step 4: Perform a few quality checks

Medication-list construction should generally include some inspection of
the final product and NDC sets. Even if you’re confident you’ve done
everything right, RxNorm is occasionally imperfect and sometimes makes
weird connections between ingredients and products, etc…

For example, some basic QC might include a count the number of unique
products and NDCs represented by each ingredient, to make sure these
seem plausible:

``` r

glp1_ndcs |>
  group_by(ingredient_name) |>
  summarise(
    n_products = n_distinct(product_rxcui),
    n_ndcs = n_distinct(ndc11, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(ingredient_name)
#> # A tibble: 6 × 3
#>   ingredient_name n_products n_ndcs
#>   <chr>                <int>  <int>
#> 1 dulaglutide              4     19
#> 2 exenatide                5      7
#> 3 liraglutide              5     51
#> 4 lixisenatide             1      4
#> 5 semaglutide             23     53
#> 6 tirzepatide             48     94
```

We can also identify product concepts for which no active NDC was found:

``` r

glp1_products |>
  anti_join(
    glp1_ndcs |>
      filter(!is.na(ndc11)) |>
      distinct(product_rxcui),
    by = "product_rxcui"
  ) |>
  select(ingredient_rxcui, product_rxcui, name, tty, n_ingredients) |>
  arrange(name)
#> # A tibble: 51 × 5
#>    ingredient_rxcui product_rxcui name                       tty   n_ingredients
#>    <chr>            <chr>         <chr>                      <chr>         <int>
#>  1 1991302          2619152       0.25 MG, 0.5 MG Dose 3 ML… SCD               1
#>  2 1534763          1534820       0.5 ML albiglutide 100 MG… SCD               1
#>  3 1534763          1534800       0.5 ML albiglutide 60 MG/… SCD               1
#>  4 1551291          1551295       0.5 ML dulaglutide 1.5 MG… SCD               1
#>  5 1551291          1551304       0.5 ML dulaglutide 3 MG/M… SCD               1
#>  6 1551291          2395777       0.5 ML dulaglutide 6 MG/M… SCD               1
#>  7 1551291          2395783       0.5 ML dulaglutide 9 MG/M… SCD               1
#>  8 1991302          2553501       0.5 ML semaglutide 0.5 MG… SCD               1
#>  9 1991302          2553601       0.5 ML semaglutide 1 MG/M… SCD               1
#> 10 1991302          2553802       0.5 ML semaglutide 2 MG/M… SCD               1
#> # ℹ 41 more rows
```

A product without an NDC is not necessarily an error. RxNorm includes
concepts that do not have a direct current NDC mapping (sometimes no
company is marketing a drug, etc…). The appropriate response depends on
the identifiers available in the study data and the intended exposure
definition.

## A compact alternative with `search_drug()`

Once the underlying workflow is understood,
[`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md)
can provide a convenient shortcut. It combines ingredient resolution,
product expansion, and optional NDC mapping.

For the same current-concept/current-NDC definition used above:

``` r

glp1_ndcs_shortcut <- search_drug(
  term = glp1_names,
  return = "ndc",
  ndc_status = "ACTIVE",
  concept_status = "active"
)

glp1_ndcs_shortcut
```

    #> # A tibble: 30 × 4
    #>    ingredient_rxcui product_rxcui ndc11       ndc_status
    #>    <chr>            <chr>         <chr>       <chr>     
    #>  1 1551291          1551300       00002143301 ACTIVE    
    #>  2 1551291          1551300       00002143361 ACTIVE    
    #>  3 1551291          1551300       00002143380 ACTIVE    
    #>  4 1551291          1551300       50090348400 ACTIVE    
    #>  5 1551291          1551300       50090645300 ACTIVE    
    #>  6 1551291          1551306       00002143401 ACTIVE    
    #>  7 1551291          1551306       00002143461 ACTIVE    
    #>  8 1551291          1551306       00002143480 ACTIVE    
    #>  9 1551291          1551306       50090348300 ACTIVE    
    #> 10 1551291          1551306       50090645600 ACTIVE    
    #> # ℹ 20 more rows

The shortcut is useful when the desired medication definition matches
the standard workflow. The explicit approach is preferable when users
need to inspect intermediate results, modify the product set, apply
study-specific exclusions, or document exactly how the final medication
definition was developed.

## Adapting the workflow

The choices above define one particular medication list, i.e., GLP1-RAs.
Other studies may need a different product universe and different needs.

### Exclude fixed-dose combination products

If the study should include only single-ingredient products, set
`include_combos = FALSE` during product expansion:

``` r

glp1_products_single <- products_for_ingredients(
  glp1_ingredients$ingredient_rxcui,
  ttys = product_ttys("default"),
  include_combos = FALSE,
  concept_status = "active"
)
```

Alternatively, you might retain combination products initially, inspect
them, and apply more specific study-defined exclusions before mapping to
NDCs.

### Use a broader set of product-related TTYs

The default TTY set is intentionally focused on product concepts that
commonly map to NDCs. A broader product-related set is available when
the research question requires it:

``` r

glp1_products_extended <- products_for_ingredients(
  glp1_ingredients$ingredient_rxcui,
  ttys = product_ttys("extended_product"),
  include_combos = TRUE,
  concept_status = "active"
)
```

Broader TTY sets can be useful for some RxCUI-based searches, but note
that not all of the additional concept types will map directly to NDCs.

### Include historical RxNorm concepts and NDC associations

Historical RxNorm concepts and historical NDC associations are related
but distinct choices.

To include historical RxNorm product concepts, use
`concept_status = "active_and_historical"`:

``` r

glp1_products_historical <- products_for_ingredients(
  glp1_ingredients$ingredient_rxcui,
  ttys = product_ttys("default"),
  include_combos = TRUE,
  concept_status = "active_and_historical"
)
```

To retrieve historical NDC associations for those products, use the
`history` argument in
[`map_rxcui_to_ndc()`](https://www.stevenmsmith.org/rxref/reference/map_rxcui_to_ndc.md):

``` r

glp1_ndcs_historical <- map_rxcui_to_ndc(
  unique(glp1_products_historical$product_rxcui),
  history = "all"
)
```

The distinction is important:

- `concept_status` determines whether the product search includes
  historical RxNorm concepts.
- `history` determines whether RxCUI-to-NDC mapping uses current or
  historical NDC associations.
- `status` filters NDCs after those associations have been retrieved.

For example, `history = "all", status = "OBSOLETE"` requests historical
NDC associations and then retains NDCs currently reported as obsolete.

[`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md)
is convenient for current medication-list construction, but it does not
currently expose the NDC `history` argument. For studies that require
historical NDC associations, the explicit
[`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
plus
[`map_rxcui_to_ndc()`](https://www.stevenmsmith.org/rxref/reference/map_rxcui_to_ndc.md)
workflow provides the necessary control.

## Finalizing the medication definition

Before using a medication list in an analysis, users should consider:

- whether the ingredient list reflects the intended clinical definition;
- whether fixed-dose combination products should be included;
- whether the default product TTYs are sufficient;
- whether route, dose form, or strength restrictions are needed;
- whether the study period requires historical RxNorm concepts or
  historical NDC associations; and
- whether the final product and NDC lists should undergo clinical
  review.

For reproducible research, save the final medication tables used in the
analysis rather than rebuilding them each time the analysis is run. It
is also useful to record the `rxref` version and the date on which the
medication definition was generated.
