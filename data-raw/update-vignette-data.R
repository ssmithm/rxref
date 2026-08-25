# data-raw/update-vignette-data.R
#
# Regenerate the precomputed RxNorm snapshots used by the rxref case-study
# vignettes.
#
# Run this script deliberately from the package root in a fresh R session:
#
#   source("data-raw/update-vignette-data.R")
#
# This script uses the live RxNorm API and the current development version of
# rxref. It should not be run automatically during R CMD build or R CMD check.

if (!file.exists("DESCRIPTION")) {
  stop(
    "Run data-raw/update-vignette-data.R from the rxref package root.",
    call. = FALSE
  )
}

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop(
    "Package 'pkgload' is required to regenerate vignette data.",
    call. = FALSE
  )
}

if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop(
    "Package 'dplyr' is required to regenerate vignette data.",
    call. = FALSE
  )
}

if (!requireNamespace("cachem", quietly = TRUE)) {
  stop(
    "Package 'cachem' is required to regenerate vignette data.",
    call. = FALSE
  )
}

# Load the current development version of rxref rather than an installed copy.
pkgload::load_all(".", quiet = FALSE)

# Use a fresh in-memory cache so the snapshots are regenerated from live API
# calls during this session.
invisible(
  rxref_conf(
    cache = cachem::cache_mem()
  )
)

extdata_dir <- file.path("inst", "extdata")
dir.create(extdata_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------
# Some validation helpers
# -------------------------------------------------------------------------

check_nonempty <- function(x, object_name) {
  if (!is.data.frame(x) || nrow(x) == 0L) {
    stop(
      "`", object_name, "` is empty or is not a data frame.",
      call. = FALSE
    )
  }

  invisible(x)
}

check_cols <- function(x, cols, object_name) {
  missing <- setdiff(cols, names(x))

  if (length(missing)) {
    stop(
      "`", object_name, "` is missing expected column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(x)
}

check_inputs_resolved <- function(expected, resolved, object_name) {
  missing <- setdiff(expected, unique(resolved$input))

  if (length(missing)) {
    stop(
      "`", object_name, "` did not resolve all expected inputs: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(resolved)
}

has_route <- function(x, route) {
  !is.na(x) &
    grepl(
      paste0("(^|; )", route, "($|;)"),
      x
    )
}

message("")
message("Regenerating rxref vignette snapshots")
message("======================================")
message("")

# =========================================================================
# GLP1-RA case study
# =========================================================================

message("GLP-1 case study")

glp1_names <- c(
  "semaglutide",
  "exenatide",
  "liraglutide",
  "lixisenatide",
  "dulaglutide",
  "albiglutide",
  "tirzepatide"
)

message("  Resolving ingredients...")
glp1_ingredients <- find_ingredients(
  glp1_names,
  show_progress = interactive()
) |>
  dplyr::filter(.data$tty == "IN") |>
  dplyr::distinct(
    .data$input,
    ingredient_rxcui = .data$rxcui,
    ingredient_name = .data$name,
    ingredient_tty = .data$tty
  )

check_nonempty(glp1_ingredients, "glp1_ingredients")
check_cols(
  glp1_ingredients,
  c("input", "ingredient_rxcui", "ingredient_name", "ingredient_tty"),
  "glp1_ingredients"
)
check_inputs_resolved(glp1_names, glp1_ingredients, "glp1_ingredients")

if (!all(glp1_ingredients$ingredient_tty == "IN")) {
  stop("GLP-1 ingredient snapshot contains a non-IN concept.", call. = FALSE)
}

message("  Expanding products...")
glp1_products <- products_for_ingredients(
  glp1_ingredients$ingredient_rxcui,
  ttys = product_ttys("default"),
  include_combos = TRUE,
  concept_status = "active",
  show_progress = interactive()
)

check_nonempty(glp1_products, "glp1_products")
check_cols(
  glp1_products,
  c("ingredient_rxcui", "product_rxcui", "name", "tty", "n_ingredients"),
  "glp1_products"
)

if (!all(glp1_products$tty %in% product_ttys("default"))) {
  stop(
    "GLP-1 product snapshot contains an unexpected TTY.",
    call. = FALSE
  )
}

message("  Mapping active NDCs...")
glp1_ndc_map <- map_rxcui_to_ndc(
  unique(glp1_products$product_rxcui),
  history = "active",
  status = "ACTIVE",
  show_progress = interactive()
)

check_nonempty(glp1_ndc_map, "glp1_ndc_map")
check_cols(
  glp1_ndc_map,
  c("rxcui", "ndc11", "ndc_status"),
  "glp1_ndc_map"
)

unexpected_glp1_status <- unique(
  stats::na.omit(glp1_ndc_map$ndc_status[
    glp1_ndc_map$ndc_status != "ACTIVE"
  ])
)

if (length(unexpected_glp1_status)) {
  stop(
    "GLP-1 NDC snapshot contains unexpected NDC status(es): ",
    paste(unexpected_glp1_status, collapse = ", "),
    call. = FALSE
  )
}

message("  Running shortcut workflow...")
glp1_ndcs_shortcut <- search_drug(
  term = glp1_names,
  return = "ndc",
  ndc_status = "ACTIVE",
  concept_status = "active",
  show_progress = interactive()
)

check_nonempty(glp1_ndcs_shortcut, "glp1_ndcs_shortcut")
check_cols(
  glp1_ndcs_shortcut,
  c("ingredient_rxcui", "product_rxcui", "ndc11", "ndc_status"),
  "glp1_ndcs_shortcut"
)

# =========================================================================
# Route-specific beta-blocker case study
# =========================================================================

message("")
message("Route-specific beta-blocker case study")

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

message("  Resolving ingredients...")
bb_ingredients <- find_ingredients(
  beta_blocker_names,
  show_progress = interactive()
) |>
  dplyr::filter(.data$tty == "IN") |>
  dplyr::distinct(
    .data$input,
    ingredient_rxcui = .data$rxcui,
    ingredient_name = .data$name,
    ingredient_tty = .data$tty
  )

check_nonempty(bb_ingredients, "bb_ingredients")
check_cols(
  bb_ingredients,
  c("input", "ingredient_rxcui", "ingredient_name", "ingredient_tty"),
  "bb_ingredients"
)
check_inputs_resolved(beta_blocker_names, bb_ingredients, "bb_ingredients")

if (!all(bb_ingredients$ingredient_tty == "IN")) {
  stop(
    "Beta-blocker ingredient snapshot contains a non-IN concept.",
    call. = FALSE
  )
}

message("  Expanding products...")
bb_products <- products_for_ingredients(
  bb_ingredients$ingredient_rxcui,
  ttys = product_ttys("default"),
  include_combos = TRUE,
  concept_status = "active",
  show_progress = interactive()
) |>
  dplyr::left_join(
    bb_ingredients,
    by = "ingredient_rxcui"
  )

check_nonempty(bb_products, "bb_products")
check_cols(
  bb_products,
  c(
    "ingredient_rxcui",
    "product_rxcui",
    "name",
    "tty",
    "n_ingredients",
    "ingredient_name"
  ),
  "bb_products"
)

message("  Retrieving clinical attributes...")
bb_attributes <- get_clinical_attributes(
  unique(bb_products$product_rxcui),
  show_progress = interactive()
) |>
  dplyr::rename(product_rxcui = .data$rxcui)

check_nonempty(bb_attributes, "bb_attributes")
check_cols(
  bb_attributes,
  c(
    "product_rxcui",
    "name",
    "tty",
    "dose_form",
    "dose_form_group",
    "route",
    "ingredient_count"
  ),
  "bb_attributes"
)

if (!any(has_route(bb_attributes$route, "ORAL"))) {
  stop(
    "No oral beta-blocker products were found; inspect current RxNorm results.",
    call. = FALSE
  )
}

if (!any(!is.na(bb_attributes$route) & !has_route(bb_attributes$route, "ORAL"))) {
  warning(
    "The beta-blocker attribute snapshot did not contain a clearly non-oral ",
    "route. The case-study motivation may need to be rechecked.",
    call. = FALSE
  )
}

message("  Filtering to oral products...")
bb_oral_products <- bb_products |>
  filter_products_by_route(
    route = "ORAL",
    show_progress = interactive()
  )

check_nonempty(bb_oral_products, "bb_oral_products")
check_cols(
  bb_oral_products,
  c(
    "ingredient_rxcui",
    "ingredient_name",
    "product_rxcui",
    "name",
    "tty",
    "n_ingredients",
    "route",
    "dose_form",
    "dose_form_group"
  ),
  "bb_oral_products"
)

if (!all(has_route(bb_oral_products$route, "ORAL"))) {
  stop(
    "Route-filtered beta-blocker snapshot contains a product without an ",
    "ORAL route assignment.",
    call. = FALSE
  )
}

message("  Mapping active oral-product NDCs...")
bb_oral_ndc_map <- map_rxcui_to_ndc(
  unique(bb_oral_products$product_rxcui),
  history = "active",
  status = "ACTIVE",
  show_progress = interactive()
)

bb_oral_ndcs <- bb_oral_ndc_map |>
  dplyr::left_join(
    bb_oral_products,
    by = c("rxcui" = "product_rxcui")
  ) |>
  dplyr::rename(product_rxcui = .data$rxcui) |>
  dplyr::distinct()

check_nonempty(bb_oral_ndcs, "bb_oral_ndcs")
check_cols(
  bb_oral_ndcs,
  c(
    "ingredient_name",
    "product_rxcui",
    "name",
    "tty",
    "route",
    "dose_form",
    "ndc11",
    "ndc_status"
  ),
  "bb_oral_ndcs"
)

unexpected_bb_status <- unique(
  stats::na.omit(bb_oral_ndcs$ndc_status[
    bb_oral_ndcs$ndc_status != "ACTIVE"
  ])
)

if (length(unexpected_bb_status)) {
  stop(
    "Beta-blocker NDC snapshot contains unexpected NDC status(es): ",
    paste(unexpected_bb_status, collapse = ", "),
    call. = FALSE
  )
}

message("  Running route-specific shortcut workflow...")
bb_oral_both <- search_drug(
  beta_blocker_names,
  return = "both",
  route = "ORAL",
  ndc_status = "ACTIVE",
  include_combos = TRUE,
  concept_status = "active",
  show_progress = interactive()
)

if (!is.list(bb_oral_both) ||
    !all(c("products", "ndcs") %in% names(bb_oral_both))) {
  stop(
    "`bb_oral_both` does not have the expected products/ndcs structure.",
    call. = FALSE
  )
}

check_nonempty(bb_oral_both$products, "bb_oral_both$products")
check_nonempty(bb_oral_both$ndcs, "bb_oral_both$ndcs")

# The shortcut and explicit workflows should ordinarily identify the same
# product universe. Warn rather than stop so an intentional API/package change
# can be reviewed before the snapshots are saved.
if (!setequal(
  unique(bb_oral_products$product_rxcui),
  unique(bb_oral_both$products$product_rxcui)
)) {
  warning(
    "The explicit and search_drug() beta-blocker workflows returned different ",
    "product RxCUI sets. Review the differences before committing snapshots.",
    call. = FALSE
  )
}

# ============================================================================
# Save snapshots only after all live workflows and core checks have completed
# ============================================================================

snapshots <- list(
  "glp1_ings.rds" = glp1_ingredients,
  "glp1_prods.rds" = glp1_products,
  "glp1_ndc_map.rds" = glp1_ndc_map,
  "glp1_ndcs_search.rds" = glp1_ndcs_shortcut,
  "bb_ingredients.rds" = bb_ingredients,
  "bb_products.rds" = bb_products,
  "bb_attrs.rds" = bb_attributes,
  "bb_oral_products.rds" = bb_oral_products,
  "bb_oral_ndcs.rds" = bb_oral_ndcs,
  "bb_oral_both_search.rds" = bb_oral_both
)

message("")
message("Saving snapshots to inst/extdata/...")

for (file in names(snapshots)) {
  saveRDS(
    snapshots[[file]],
    file = file.path(extdata_dir, file)
  )
}

description <- read.dcf("DESCRIPTION")
rxref_version <- description[1, "Version"]

summary_rows <- lapply(names(snapshots), function(file) {
  x <- snapshots[[file]]

  if (is.data.frame(x)) {
    rows <- nrow(x)
  } else if (
    is.list(x) &&
    all(c("products", "ndcs") %in% names(x))
  ) {
    rows <- paste0(
      "products=", nrow(x$products),
      "; ndcs=", nrow(x$ndcs)
    )
  } else {
    rows <- NA_character_
  }

  data.frame(
    file = file,
    rows = as.character(rows),
    stringsAsFactors = FALSE
  )
})

snapshot_summary <- do.call(rbind, summary_rows)

message("")
message("Snapshot refresh complete")
message("-------------------------")
message("rxref version: ", rxref_version)
message("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
message("")

print(snapshot_summary, row.names = FALSE)

message("")
message(
  "Next: review the changed .rds files, render the vignettes, and inspect the ",
  "resulting HTML before committing."
)
