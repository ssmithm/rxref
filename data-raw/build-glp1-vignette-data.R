# data-raw/build-glp1-vignette-data.R

# Recompute precomputed datasets for:
# vignettes/case_study_glp1ra.Rmd
#
# Expected output files:
# inst/extdata/glp1_ings.rds
# inst/extdata/glp1_prods.rds
# inst/extdata/glp1_ndc_map.rds
# inst/extdata/alt_glp1_ndc.rds

library(rxref)
library(dplyr)

# Optional: be gentle with the API during batch calls
rxref_conf(rate_delay = 0.2)

# Ensure extdata exists
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

# Ingredient names used in the vignette
glp1.names <- c(
  "semaglutide",
  "exenatide",
  "liraglutide",
  "lixisenatide",
  "dulaglutide",
  "albiglutide",
  "tirzepatide"
)

# 1. Ingredient RxCUIs -----------------------------------------------------

glp1_ings <- find_ingredients(glp1.names) |>
  filter(tty == "IN") |>
  distinct(
    input,
    ingredient_rxcui = rxcui,
    ingredient_name = name,
    ingredient_tty = tty
  ) |>
  arrange(input, ingredient_name)

saveRDS(
  glp1.ings,
  file = "inst/extdata/glp1_ings.rds",
  version = 2
)

# 2. Product RxCUIs --------------------------------------------------------

glp1.prods <- products_for_ingredients(
  glp1.ings$ingredient_rxcui,
  ttys = product_ttys("default"),
  include_combos = TRUE,
  concept_status = "active"
) |>
  arrange(ingredient_rxcui, product_rxcui)

saveRDS(
  glp1.prods,
  file = "inst/extdata/glp1_prods.rds",
  version = 2
)

# 3. Product RxCUI -> active NDC mappings ---------------------------------

glp1.ndc.map <- map_rxcui_to_ndc(
  unique(glp1.prods$product_rxcui),
  status = "ACTIVE"
) |>
  arrange(rxcui, ndc11)

saveRDS(
  glp1.ndc.map,
  file = "inst/extdata/glp1_ndc_map.rds",
  version = 2
)

# 4. Compact search_drug() workflow ---------------------------------------

alt.glp1.ndcs_raw <- search_drug(
  term = glp1.names,
  return = "ndc",
  concept_status = "active",
  ndc_status = c("ACTIVE", "OBSOLETE", "UNSPECIFIED")
)

alt.glp1.ndcs <- alt.glp1.ndcs_raw |>
  left_join(
    glp1.ings |>
      select(ingredient_rxcui, ingredient_name),
    by = "ingredient_rxcui"
  ) |>
  left_join(
    glp1.prods |>
      select(
        ingredient_rxcui,
        product_rxcui,
        product_name = name,
        product_tty = tty
      ),
    by = c("ingredient_rxcui", "product_rxcui")
  ) |>
  distinct(
    ingredient_rxcui,
    ingredient_name,
    product_rxcui,
    product_name,
    product_tty,
    ndc11,
    ndc_status
  ) |>
  arrange(ingredient_name, product_rxcui, ndc11)

saveRDS(
  alt.glp1.ndcs,
  file = "inst/extdata/alt_glp1_ndc.rds",
  version = 2
)

# 5. Lightweight checks ----------------------------------------------------

message("Saved GLP-1 vignette example datasets:")

checks <- tibble::tibble(
  file = c(
    "glp1_ings.rds",
    "glp1_prods.rds",
    "glp1_ndc_map.rds",
    "alt_glp1_ndc.rds"
  ),
  n_rows = c(
    nrow(glp1.ings),
    nrow(glp1.prods),
    nrow(glp1.ndc.map),
    nrow(alt.glp1.ndcs)
  )
)

print(checks)

# Optional structural checks

stopifnot(
  all(c(
    "input",
    "ingredient_rxcui",
    "ingredient_name",
    "ingredient_tty"
  ) %in% names(glp1.ings))
)

stopifnot(
  all(c(
    "ingredient_rxcui",
    "product_rxcui"
  ) %in% names(glp1.prods))
)

stopifnot(
  all(c(
    "rxcui",
    "ndc11",
    "ndc_status"
  ) %in% names(glp1.ndc.map))
)

stopifnot(
  all(c(
    "ingredient_name",
    "product_rxcui",
    "ndc11"
  ) %in% names(alt.glp1.ndcs))
)

message("Done.")
