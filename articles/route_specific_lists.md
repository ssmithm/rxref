# Case Study: Building route-specific RxCUI and NDC lists

## Case

A common use case for `rxref` is to develop a list of RxNorm product
concepts (RxCUIs) and National Drug Codes (NDCs) for a medication class.
However, sometimes the ingredient alone is not sufficient to define the
exposure.

Suppose we want to identify **oral beta-blocker products** for an
outpatient pharmacoepidemiologic study. Several beta-blockers are
available via multiple routes of administration. For example:

- metoprolol, atenolol, labetalol, propranolol, and sotalol have oral
  and injectable products;
- esmolol is primarily used as an injectable product; and
- timolol, betaxolol, and related drugs may have ophthalmic products.

If we simply expand beta-blocker ingredients to all products and map
them to NDCs, we would capture products outside the intended exposure
definition.

This case study builds an oral beta-blocker medication list in five
steps:

1.  define the ingredients of interest;
2.  resolve ingredient names to RxNorm ingredient RxCUIs;
3.  expand the ingredients to product-level RxNorm concepts;
4.  inspect and filter products by route; and
5.  map the final oral products to active NDCs.

The displayed results use precomputed data bundled with `rxref`, so the
vignette can be built without requiring access to the live RxNorm API.
The code shown below is the code users can run interactively to generate
the results from the current API.

## Define the ingredient list

We begin with a curated list of beta-blocker ingredients.

``` r

beta_blocker_names <- c(
  "acebutolol",
  "atenolol",
  "betaxolol",
  "bisoprolol",
  "carvedilol",
  "labetalol",
  "metoprolol",
  "nadolol",
  "nebivolol",
  "penbutolol",
  "pindolol",
  "propranolol",
  "sotalol",
  "timolol"
)

beta_blocker_names
#>  [1] "acebutolol"  "atenolol"    "betaxolol"   "bisoprolol"  "carvedilol" 
#>  [6] "labetalol"   "metoprolol"  "nadolol"     "nebivolol"   "penbutolol" 
#> [11] "pindolol"    "propranolol" "sotalol"     "timolol"
```

A curated ingredient list is often appropriate when the study definition
is known in advance. `rxref` also provides RxClass helpers such as
[`find_classes()`](https://www.stevenmsmith.org/rxref/reference/find_classes.md)
and
[`get_class_members()`](https://www.stevenmsmith.org/rxref/reference/get_class_members.md)
when users want to begin from a source vocabulary class rather than a
prespecified list.

## Step 1: Identify ingredient RxCUIs

Use
[`find_ingredients()`](https://www.stevenmsmith.org/rxref/reference/find_ingredients.md)
to resolve each name to an RxNorm ingredient concept. Here we retain TTY
= `"IN"`, representing the base ingredient concept.

``` r

bb_ingredients <- find_ingredients(beta_blocker_names) |>
  filter(tty == "IN") |>
  distinct(
    input,
    ingredient_rxcui = rxcui,
    ingredient_name = name,
    ingredient_tty = tty
  )

bb_ingredients
```

    #> # A tibble: 14 × 4
    #>    input       ingredient_rxcui ingredient_name ingredient_tty
    #>    <chr>       <chr>            <chr>           <chr>         
    #>  1 acebutolol  149              acebutolol      IN            
    #>  2 atenolol    1202             atenolol        IN            
    #>  3 betaxolol   1520             betaxolol       IN            
    #>  4 bisoprolol  19484            bisoprolol      IN            
    #>  5 carvedilol  20352            carvedilol      IN            
    #>  6 labetalol   6185             labetalol       IN            
    #>  7 metoprolol  6918             metoprolol      IN            
    #>  8 nadolol     7226             nadolol         IN            
    #>  9 nebivolol   31555            nebivolol       IN            
    #> 10 penbutolol  7973             penbutolol      IN            
    #> 11 pindolol    8332             pindolol        IN            
    #> 12 propranolol 8787             propranolol     IN            
    #> 13 sotalol     9947             sotalol         IN            
    #> 14 timolol     10600            timolol         IN

This is a useful checkpoint: before expanding to hundreds of product
concepts, verify that each study-defined name resolved to the intended
ingredient.

## Step 2: Expand ingredients to product RxCUIs

Next, use
[`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
to identify product-level RxNorm concepts containing the selected
ingredients.

For this example, we use the default product-focused TTYs, retain
fixed-dose combination products, and initially keep products from all
routes.

``` r

bb_products <- products_for_ingredients(
  bb_ingredients$ingredient_rxcui,
  ttys = product_ttys("default"),
  include_combos = TRUE,
  concept_status = "active"
) |>
  # rejoin the ingredients list so you know what maps to what
  left_join(bb_ingredients, by = "ingredient_rxcui")

bb_products
```

    #> # A tibble: 30 × 8
    #>    ingredient_rxcui product_rxcui name                 tty   n_ingredients input
    #>    <chr>            <chr>         <chr>                <chr>         <int> <chr>
    #>  1 149              998693        acebutolol 100 MG O… SCD               1 aceb…
    #>  2 149              998694        acebutolol 200 MG /… SCD               2 aceb…
    #>  3 149              998689        acebutolol 200 MG O… SCD               1 aceb…
    #>  4 149              998685        acebutolol 400 MG O… SCD               1 aceb…
    #>  5 149              998695        acebutolol 400 MG O… SCD               1 aceb…
    #>  6 1202             746023        atenolol 100 MG / c… SBD               2 aten…
    #>  7 1202             201322        atenolol 100 MG Ora… SBD               1 aten…
    #>  8 1202             150750        atenolol 25 MG Oral… SBD               1 aten…
    #>  9 1202             746030        atenolol 50 MG / ch… SBD               2 aten…
    #> 10 1202             152414        atenolol 50 MG Oral… SBD               1 aten…
    #> # ℹ 20 more rows
    #> # ℹ 2 more variables: ingredient_name <chr>, ingredient_tty <chr>

At this stage, the product table intentionally contains products from
all routes. Keeping route filtering separate for the moment makes it
easier to inspect what would otherwise be included in the medication
definition.

Because `include_combos = TRUE`, fixed-dose combination products may
also be present. We’ll return to that later.

## Step 3: Inspect route and dose-form information

[`get_clinical_attributes()`](https://www.stevenmsmith.org/rxref/reference/get_clinical_attributes.md)
derives clinically useful information from the product concepts,
including route, dose form, dose-form group, ingredient count, and
brand/generic indicators.

``` r

bb_attributes <- get_clinical_attributes(
  unique(bb_products$product_rxcui)
) |>
  rename(product_rxcui = rxcui)

bb_attributes
```

A quick summary shows why route filtering is needed:

``` r

bb_attributes |>
  count(route, dose_form_group, sort = TRUE)
#> # A tibble: 3 × 3
#>   route      dose_form_group        n
#>   <chr>      <chr>              <int>
#> 1 ORAL       Oral Product         176
#> 2 OPHTHALMIC Ophthalmic Product    29
#> 3 INJECTION  Injectable Product    14
```

The exact mix of routes depends on the current RxNorm concepts returned
for the selected ingredients, but the important point is that an
ingredient-based search can identify products that do not correspond to
the intended route of exposure.

It can also be useful to inspect individual products associated with
non-oral routes:

``` r

bb_products |>
  left_join(
    bb_attributes |>
      select(product_rxcui, route, dose_form, dose_form_group),
    by = "product_rxcui"
  ) |>
  filter(is.na(route) | route != "ORAL") |>
  select(
    ingredient_name,
    product_rxcui,
    name,
    tty,
    route,
    dose_form,
    dose_form_group
  ) |>
  arrange(ingredient_name, route, name) |>
  head(30)
#> # A tibble: 30 × 7
#>    ingredient_name product_rxcui name      tty   route dose_form dose_form_group
#>    <chr>           <chr>         <chr>     <chr> <chr> <chr>     <chr>          
#>  1 atenolol        104308        atenolol… SCD   INJE… Injectab… Injectable Pro…
#>  2 betaxolol       308719        betaxolo… SCD   OPHT… Ophthalm… Ophthalmic Pro…
#>  3 betaxolol       213729        betaxolo… SBD   OPHT… Ophthalm… Ophthalmic Pro…
#>  4 betaxolol       308720        betaxolo… SCD   OPHT… Ophthalm… Ophthalmic Pro…
#>  5 labetalol       2479566       100 ML l… SCD   INJE… Injection Injectable Pro…
#>  6 labetalol       2598343       2 ML lab… SCD   INJE… Prefille… Injectable Pro…
#>  7 labetalol       2479564       200 ML l… SCD   INJE… Injection Injectable Pro…
#>  8 labetalol       2479567       300 ML l… SCD   INJE… Injection Injectable Pro…
#>  9 labetalol       1234256       4 ML lab… SCD   INJE… Cartridge Injectable Pro…
#> 10 labetalol       2477889       4 ML lab… SCD   INJE… Injection Injectable Pro…
#> # ℹ 20 more rows
```

This inspection step is especially useful when working with a medication
class for which route heterogeneity is clinically important.

## Step 4: Filter to oral products

Once we have confirmed that the unfiltered product set contains multiple
routes, use
[`filter_products_by_route()`](https://www.stevenmsmith.org/rxref/reference/filter_products_by_route.md)
to retain only oral products.

``` r

bb_oral_products <- bb_products |>
  filter_products_by_route(route = "ORAL")

bb_oral_products
```

    #> # A tibble: 30 × 11
    #>    ingredient_rxcui product_rxcui name                 tty   n_ingredients input
    #>    <chr>            <chr>         <chr>                <chr>         <int> <chr>
    #>  1 149              998693        acebutolol 100 MG O… SCD               1 aceb…
    #>  2 149              998694        acebutolol 200 MG /… SCD               2 aceb…
    #>  3 149              998689        acebutolol 200 MG O… SCD               1 aceb…
    #>  4 149              998685        acebutolol 400 MG O… SCD               1 aceb…
    #>  5 149              998695        acebutolol 400 MG O… SCD               1 aceb…
    #>  6 1202             746023        atenolol 100 MG / c… SBD               2 aten…
    #>  7 1202             201322        atenolol 100 MG Ora… SBD               1 aten…
    #>  8 1202             150750        atenolol 25 MG Oral… SBD               1 aten…
    #>  9 1202             746030        atenolol 50 MG / ch… SBD               2 aten…
    #> 10 1202             152414        atenolol 50 MG Oral… SBD               1 aten…
    #> # ℹ 20 more rows
    #> # ℹ 5 more variables: ingredient_name <chr>, ingredient_tty <chr>, route <chr>,
    #> #   dose_form <chr>, dose_form_group <chr>

The returned table preserves the product-level information and appends
summarized route, dose-form, and dose-form-group fields.

``` r

bb_oral_products |>
  count(route, dose_form_group, sort = TRUE)
#> # A tibble: 1 × 3
#>   route dose_form_group     n
#>   <chr> <chr>           <int>
#> 1 ORAL  Oral Product      176
```

Route filtering depends on clinical attribute information available for
a product RxCUI. A product without a route match is not retained, so
route-specific medication lists should always be reviewed before they
are used in an analysis.

## Step 5: Map oral product RxCUIs to active NDCs

After restricting the RxCUI definition to oral products, map those
products to NDCs.

For a current medication list, we use current NDC associations and
retain NDCs whose reported status is `"ACTIVE"`.

``` r

bb_oral_ndc_map <- map_rxcui_to_ndc(
  unique(bb_oral_products$product_rxcui),
  history = "active",
  status = "ACTIVE"
)

bb_oral_ndcs <- bb_oral_ndc_map |>
  left_join(
    bb_oral_products,
    by = c("rxcui" = "product_rxcui")
  ) |>
  rename(product_rxcui = rxcui) |>
  distinct()

bb_oral_ndcs
```

    #> # A tibble: 30 × 8
    #>    ingredient_name product_rxcui name     tty   route dose_form ndc11 ndc_status
    #>    <chr>           <chr>         <chr>    <chr> <chr> <chr>     <chr> <chr>     
    #>  1 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 0037… ACTIVE    
    #>  2 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 1013… ACTIVE    
    #>  3 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 5140… ACTIVE    
    #>  4 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 5374… ACTIVE    
    #>  5 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 5374… ACTIVE    
    #>  6 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 5374… ACTIVE    
    #>  7 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 6255… ACTIVE    
    #>  8 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 6516… ACTIVE    
    #>  9 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 6516… ACTIVE    
    #> 10 acebutolol      998685        acebuto… SCD   ORAL  Oral Cap… 6516… ACTIVE    
    #> # ℹ 20 more rows

At this point, we have two useful study resources:

- `bb_oral_products`, containing the route-restricted RxCUI definition;
  and
- `bb_oral_ndcs`, containing the corresponding active NDC mappings.

Keeping both can be useful when the medication definition will be
applied to data sources that use different drug identifiers.

## Quality-control checks

Route-specific medication definitions benefit from several simple
quality checks before they are finalized.

### Confirm the observed routes

``` r

bb_oral_products |>
  count(route, dose_form_group, sort = TRUE)
#> # A tibble: 1 × 3
#>   route dose_form_group     n
#>   <chr> <chr>           <int>
#> 1 ORAL  Oral Product      176
```

For this definition, every retained product should have an oral route
assignment.

### Look for non-oral terminology in product names

A name-based check is not a substitute for structured route filtering,
but it is a useful secondary screen.

``` r

bb_oral_products |>
  filter(grepl(
    "Injection|Injectable|Ophthalmic|Topical|Transdermal|Nasal|Inhalation",
    name,
    ignore.case = TRUE
  )) |>
  select(
    ingredient_name,
    product_rxcui,
    name,
    tty,
    route,
    dose_form
  )
#> # A tibble: 0 × 6
#> # ℹ 6 variables: ingredient_name <chr>, product_rxcui <chr>, name <chr>,
#> #   tty <chr>, route <chr>, dose_form <chr>
```

Unexpected rows should be reviewed rather than automatically removed,
because product naming conventions do not always map perfectly to route.
And, again, sometimes RxNorm has mistakes in the source data that will
eventually get corrected.

### Review combination products

Because this example retained combination products, inspect them
explicitly:

``` r

bb_oral_products |>
  filter(n_ingredients > 1) |>
  select(
    ingredient_name,
    product_rxcui,
    name,
    tty,
    n_ingredients,
    route,
    dose_form
  ) |>
  arrange(ingredient_name, name)
#> # A tibble: 35 × 7
#>    ingredient_name product_rxcui name        tty   n_ingredients route dose_form
#>    <chr>           <chr>         <chr>       <chr>         <int> <chr> <chr>    
#>  1 acebutolol      998694        acebutolol… SCD               2 ORAL  Oral Tab…
#>  2 atenolol        977920        amiloride … SCD               3 ORAL  Oral Cap…
#>  3 atenolol        197382        atenolol 1… SCD               2 ORAL  Oral Tab…
#>  4 atenolol        746023        atenolol 1… SBD               2 ORAL  Oral Tab…
#>  5 atenolol        153155        atenolol 2… SCD               2 ORAL  Oral Cap…
#>  6 atenolol        152916        atenolol 5… SCD               2 ORAL  Oral Tab…
#>  7 atenolol        197383        atenolol 5… SCD               2 ORAL  Oral Tab…
#>  8 atenolol        746030        atenolol 5… SBD               2 ORAL  Oral Tab…
#>  9 atenolol        393275        atenolol 5… SCD               2 ORAL  Extended…
#> 10 bisoprolol      854908        bisoprolol… SCD               2 ORAL  Oral Tab…
#> # ℹ 25 more rows
```

For example, beta-blocker/thiazide combination products may be
appropriate for a broad antihypertensive exposure definition but
inappropriate for a study specifically evaluating beta-blocker
monotherapy.

### Check products without active NDCs

Some valid RxNorm product concepts may not map to a current active NDC.

``` r

bb_oral_products |>
  anti_join(
    bb_oral_ndcs |>
      filter(!is.na(ndc11)) |>
      distinct(product_rxcui),
    by = "product_rxcui"
  ) |>
  select(
    ingredient_name,
    product_rxcui,
    name,
    tty,
    route,
    dose_form
  ) |>
  arrange(ingredient_name, name)
#> # A tibble: 43 × 6
#>    ingredient_name product_rxcui name                      tty   route dose_form
#>    <chr>           <chr>         <chr>                     <chr> <chr> <chr>    
#>  1 acebutolol      998693        acebutolol 100 MG Oral C… SCD   ORAL  Oral Cap…
#>  2 acebutolol      998694        acebutolol 200 MG / hydr… SCD   ORAL  Oral Tab…
#>  3 acebutolol      998695        acebutolol 400 MG Oral T… SCD   ORAL  Oral Tab…
#>  4 atenolol        977920        amiloride hydrochloride … SCD   ORAL  Oral Cap…
#>  5 atenolol        153155        atenolol 25 MG / bendrof… SCD   ORAL  Oral Cap…
#>  6 atenolol        755528        atenolol 5 MG/ML Oral So… SCD   ORAL  Oral Sol…
#>  7 atenolol        152916        atenolol 50 MG / chlorth… SCD   ORAL  Oral Tab…
#>  8 atenolol        393275        atenolol 50 MG / nifedip… SCD   ORAL  Extended…
#>  9 bisoprolol      865154        bisoprolol fumarate 1.25… SCD   ORAL  Oral Tab…
#> 10 bisoprolol      865157        bisoprolol fumarate 3.75… SCD   ORAL  Oral Tab…
#> # ℹ 33 more rows
```

Whether products without active NDCs matter depends on the identifiers
available in the study data.

### Summarize the final definition

Finally, summarize the number of unique products and NDCs associated
with each ingredient:

``` r

bb_oral_ndcs |>
  group_by(ingredient_name) |>
  summarise(
    n_products = n_distinct(product_rxcui),
    n_ndcs = n_distinct(ndc11, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(ingredient_name)
#> # A tibble: 13 × 3
#>    ingredient_name n_products n_ndcs
#>    <chr>                <int>  <int>
#>  1 acebutolol               2     21
#>  2 atenolol                10    545
#>  3 betaxolol                2     11
#>  4 bisoprolol               9    273
#>  5 carvedilol              16    719
#>  6 labetalol                7    315
#>  7 metoprolol              31   1666
#>  8 nadolol                  5    180
#>  9 nebivolol                9    278
#> 10 pindolol                 2     21
#> 11 propranolol             18    612
#> 12 sotalol                 19    181
#> 13 timolol                  3     11
```

## A compact alternative with `search_drug()`

Once the explicit workflow is understood,
[`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md)
can combine ingredient resolution, product expansion, route filtering,
and NDC mapping in a single call.

Using `return = "both"` is particularly useful here because the product
table and the NDC table have different grains.

``` r

bb_oral <- search_drug(
  beta_blocker_names,
  return = "both",
  route = "ORAL",
  ndc_status = "ACTIVE",
  include_combos = TRUE,
  concept_status = "active"
)

bb_oral$products
bb_oral$ndcs
```

The result is a list with separate product and NDC tables:

``` r

names(bb_oral)
#> [1] "products" "ndcs"
```

``` r

bb_oral$products |>
  arrange(ingredient_rxcui, tty, name) |>
  head(20)
#> # A tibble: 20 × 8
#>    ingredient_rxcui product_rxcui name       tty   n_ingredients route dose_form
#>    <chr>            <chr>         <chr>      <chr>         <int> <chr> <chr>    
#>  1 10600            977949        amiloride… SCD               3 ORAL  Oral Tab…
#>  2 10600            250543        bendroflu… SCD               2 ORAL  Oral Tab…
#>  3 10600            310811        hydrochlo… SCD               2 ORAL  Oral Tab…
#>  4 10600            198284        timolol 1… SCD               1 ORAL  Oral Tab…
#>  5 10600            198285        timolol 2… SCD               1 ORAL  Oral Tab…
#>  6 10600            198286        timolol 5… SCD               1 ORAL  Oral Tab…
#>  7 1202             746023        atenolol … SBD               2 ORAL  Oral Tab…
#>  8 1202             201322        atenolol … SBD               1 ORAL  Oral Tab…
#>  9 1202             150750        atenolol … SBD               1 ORAL  Oral Tab…
#> 10 1202             746030        atenolol … SBD               2 ORAL  Oral Tab…
#> 11 1202             152414        atenolol … SBD               1 ORAL  Oral Tab…
#> 12 1202             977920        amiloride… SCD               3 ORAL  Oral Cap…
#> 13 1202             197382        atenolol … SCD               2 ORAL  Oral Tab…
#> 14 1202             197379        atenolol … SCD               1 ORAL  Oral Tab…
#> 15 1202             153155        atenolol … SCD               2 ORAL  Oral Cap…
#> 16 1202             197380        atenolol … SCD               1 ORAL  Oral Tab…
#> 17 1202             755528        atenolol … SCD               1 ORAL  Oral Sol…
#> 18 1202             152916        atenolol … SCD               2 ORAL  Oral Tab…
#> 19 1202             197383        atenolol … SCD               2 ORAL  Oral Tab…
#> 20 1202             393275        atenolol … SCD               2 ORAL  Extended…
#> # ℹ 1 more variable: dose_form_group <chr>
```

``` r

bb_oral$ndcs |>
  arrange(ingredient_rxcui, product_rxcui, ndc11) |>
  head(20)
#> # A tibble: 20 × 4
#>    ingredient_rxcui product_rxcui ndc11       ndc_status
#>    <chr>            <chr>         <chr>       <chr>     
#>  1 10600            198284        00378022101 ACTIVE    
#>  2 10600            198284        16571022401 ACTIVE    
#>  3 10600            198284        73152003001 ACTIVE    
#>  4 10600            198285        00378071501 ACTIVE    
#>  5 10600            198285        13811062010 ACTIVE    
#>  6 10600            198285        16571022501 ACTIVE    
#>  7 10600            198285        73152003101 ACTIVE    
#>  8 10600            198286        00378005501 ACTIVE    
#>  9 10600            198286        13811061810 ACTIVE    
#> 10 10600            198286        16571022301 ACTIVE    
#> 11 10600            198286        73152002901 ACTIVE    
#> 12 1202             150750        24979024407 ACTIVE    
#> 13 1202             150750        52427042990 ACTIVE    
#> 14 1202             152414        24979024507 ACTIVE    
#> 15 1202             152414        52427043090 ACTIVE    
#> 16 1202             152414        52959028030 ACTIVE    
#> 17 1202             152414        55289025430 ACTIVE    
#> 18 1202             197379        00093075301 ACTIVE    
#> 19 1202             197379        00093075305 ACTIVE    
#> 20 1202             197379        00378075701 ACTIVE
```

The shortcut is useful when the standard workflow matches the study
definition. The explicit approach remains preferable when users need to
inspect route heterogeneity before filtering, modify the product set, or
document study-specific inclusion and exclusion decisions.

## Adapting the workflow

The example above represents one particular route-specific medication
definition. Several common modifications are possible.

### Filter by route during product expansion

When users already know the intended route and do not need to inspect
the unfiltered product universe first,
[`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
can apply the route restriction directly:

``` r

bb_oral_products_direct <- products_for_ingredients(
  bb_ingredients$ingredient_rxcui,
  ttys = product_ttys("default"),
  route = "ORAL",
  include_combos = TRUE,
  concept_status = "active"
)
```

This uses the same route-filtering infrastructure but provides a more
compact workflow.

### Exclude fixed-dose combination products

If the study should include beta-blocker single drug entities (i.e., not
combination products) only, set `include_combos = FALSE`:

``` r

bb_oral_single_products <- products_for_ingredients(
  bb_ingredients$ingredient_rxcui,
  ttys = product_ttys("default"),
  route = "ORAL",
  include_combos = FALSE,
  concept_status = "active"
)
```

Alternatively, retain combination products initially, review them, and
apply study-specific exclusions after product expansion.

### Include historical RxNorm concepts and NDC associations

For older study periods, users may need concepts that are no longer
active in the current RxNorm release.

Historical RxNorm product concepts can be included with:

``` r

bb_products_historical <- products_for_ingredients(
  bb_ingredients$ingredient_rxcui,
  ttys = product_ttys("default"),
  include_combos = TRUE,
  concept_status = "active_and_historical"
)

bb_oral_products_historical <- bb_products_historical |>
  filter_products_by_route(route = "ORAL")
```

[`filter_products_by_route()`](https://www.stevenmsmith.org/rxref/reference/filter_products_by_route.md)
can use historical RxCUI metadata as a fallback when the product table
contains non-active concepts. Even so, historical route assignments
deserve careful review because clinical attribute information may be
less complete for older concepts.

Historical NDC associations are a separate choice. Retrieve them with
the `history` argument in
[`map_rxcui_to_ndc()`](https://www.stevenmsmith.org/rxref/reference/map_rxcui_to_ndc.md):

``` r

bb_oral_ndcs_historical <- map_rxcui_to_ndc(
  unique(bb_oral_products_historical$product_rxcui),
  history = "all"
)
```

The distinction is important:

- `concept_status` determines whether product expansion includes
  historical RxNorm concepts;
- `history` determines whether RxCUI-to-NDC mapping uses current or
  historical NDC associations; and
- `status` filters NDCs after those associations have been retrieved.

[`search_drug()`](https://www.stevenmsmith.org/rxref/reference/search_drug.md)
is convenient for current route-specific medication lists, but it does
not currently expose the NDC `history` argument. For studies requiring
historical NDC associations, use the explicit
[`products_for_ingredients()`](https://www.stevenmsmith.org/rxref/reference/products_for_ingredients.md)
plus
[`map_rxcui_to_ndc()`](https://www.stevenmsmith.org/rxref/reference/map_rxcui_to_ndc.md)
workflow.

## Finalizing a route-specific medication definition

Before using a route-specific medication list in an analysis, consider:

- whether the ingredient list matches the intended clinical definition;
- whether the selected route is represented correctly across products;
- whether combination products should be included;
- whether the default product TTYs are sufficient;
- whether products without route information require manual review;
- whether products without active NDCs matter for the source data; and
- whether the study period requires historical RxNorm concepts or NDC
  associations.

For reproducible research, save the final product and NDC tables used in
the analysis rather than rebuilding them each time. It is also useful to
record the `rxref` version and the date on which the medication
definition was generated.
