#' Catalogue of RxNorm TTY (Term Types)
#'
#' Returns a tibble describing common RxNorm TTYs you may want to use when
#' expanding ingredients to products. Includes whether each TTY typically maps
#' cleanly to NDCs and whether it's included in the package's default or
#' extended TTY sets.
#'
#' Columns:
#' - `tty`: RxNorm term type code.
#' - `label`: Short, human-friendly name.
#' - `description`: What the TTY represents in RxNorm.
#' - `maps_to_ndc`: Logical; whether CUIs of this TTY usually map to NDCs via
#'    `/rxcui/{rxcui}/ndcs`.
#' - `typical_role`: How it’s commonly used (product, component, group, name).
#' - `include_default`: Logical; included in `.rxref_default_ttys`.
#' - `include_extended`: Logical; included in `.rxref_extended_ttys`.
#'
#' @return A tibble with metadata for key TTYs.
#' @examples
#' tty_catalogue()
#'
#' # TTYs that map to NDCs
#' subset(tty_catalogue(), maps_to_ndc)$tty
#'
#' # See what your defaults and extended sets contain
#' subset(tty_catalogue(), include_default)$tty
#' subset(tty_catalogue(), include_extended)$tty
#'
#' # Pick a custom set: products + components
#' with(tty_catalogue(),
#'      tty[tty %in% c("SCD","SBD","GPCK","BPCK","SCDC","SBDC")])
#' @export
tty_catalogue <- function() {
  # guard in case user hasn't defined these constants yet
  default_set  <- if (exists(".rxref_default_ttys", inherits = TRUE))
    get(".rxref_default_ttys", inherits = TRUE) else c("SCD","SBD","GPCK","BPCK")
  extended_set <- if (exists(".rxref_extended_ttys", inherits = TRUE))
    get(".rxref_extended_ttys", inherits = TRUE)
  else c("SCDC","SBDC","SCDF","SBDF","SCDFP","SBDFP","SCDG","SCDGP","BN","MIN")

  df <- tibble::tribble(
    ~tty,    ~label,                  ~description,                                                                                 ~maps_to_ndc, ~typical_role,
    "SCD",   "Semantic Clinical Drug","Normalized clinical product (ingredient+strength+dose form), no brand.",                     TRUE,         "product",
    "SBD",   "Semantic Branded Drug", "Normalized branded product (brand + SCD composition).",                                      TRUE,         "product",
    "GPCK",  "Generic Pack",          "Package that contains one or more SCD items.",                                               TRUE,         "pack",
    "BPCK",  "Branded Pack",          "Package that contains one or more SBD items.",                                               TRUE,         "pack",

    "SCDC",  "SCD Component",         "Component of an SCD (e.g., within a combo).",                                               FALSE,        "component",
    "SBDC",  "SBD Component",         "Component of an SBD.",                                                                       FALSE,        "component",

    "SCDF",  "SCD + Dose Form",       "Group of SCDs sharing a dose form; structural grouping.",                                   FALSE,        "group",
    "SBDF",  "SBD + Dose Form",       "Group of SBDs sharing a dose form; structural grouping.",                                   FALSE,        "group",
    "SCDFP", "SCD + DF + Pack",       "Group of SCDs sharing dose form and pack; structural grouping.",                            FALSE,        "group",
    "SBDFP", "SBD + DF + Pack",       "Group of SBDs sharing dose form and pack; structural grouping.",                            FALSE,        "group",
    "SCDG",  "SCD Group",             "Higher-level group of related SCDs.",                                                        FALSE,        "group",
    "SCDGP", "SCD Group + Pack",      "Group of SCDs by pack status.",                                                             FALSE,        "group",

    "BN",    "Brand Name",            "Brand name term (name concept, not a product instance).",                                   FALSE,        "name",
    "MIN",   "Multi-Ingredient Name", "Name concept representing a specific multi-ingredient combo (not a product instance).",      FALSE,        "name"
  )

  df |>
    dplyr::mutate(
      include_default  = .data$tty %in% default_set,
      include_extended = .data$tty %in% extended_set
    )
}


# Some helpers:
# Default product-ish TTYs (good for mapping to NDC)
.rxref_default_ttys <- c(
  "SCD",  # Semantic Clinical Drug
  "SBD",  # Semantic Branded Drug
  "GPCK", # Generic Pack
  "BPCK"  # Brand Pack
)

.rxref_extended_product_ttys <- c(
  .rxref_default_ttys,
  "SCDG",  # Semantic Clinical Dose Form Group
  "SBDG",  # Semantic Branded Dose Form Group
  "SCDF",  # Semantic Clinical Dose Form
  "SBDF",  # Semantic Branded Dose Form
  "SBDFP", # Semantic Branded Drug Form Precise
  "SCDFP", # Semantic Clinical Drug Form Precise
  "SCDGP" # Semantic Clinical Dose Form Group Precise
)

# Extended “structure/group” TTYs for richer CUIs; note many of these will rarely map to NDC,
# but may be useful in RXCUI searches, particularly where RXCUIs are not cleanly mapped to a
# very limited set of prescribable products.
# - Default product TTYs: SCD, SBD, GPCK, BPCK
# - Components: SCDC, SBDC
# - Drug forms / groups: SCDF, SBDF, SCDFP, SBDFP, SCDG, SCDGP
# - Names / ingredients: BN (brand name), MIN (multi-ingredient), IN (ingredients)
.rxref_extended_ttys <- c(
  .rxref_extended_product_ttys,
  "SCDC",  # Semantic Clinical Drug Component
  "SBDC",  # Semantic Branded Drug Component
  "BN",    # Brand Name
  "MIN",   # Multiple Ingredients
  "PIN",   # Precise Ingredient
  "IN"     # Ingredient
  )

#' Default RxNorm product term types
#'
#' Returns the default RxNorm term types used by rxref when identifying
#' drug products.
#'
#' The default set is intentionally focused on product-level concepts:
#' semantic clinical drugs, semantic branded drugs, generic packs, and
#' branded packs.
#'
#' @return A character vector of RxNorm term type abbreviations.
#'
#' @examples
#' default_product_ttys()
#'
#' @export
default_product_ttys <- function() {
  .rxref_default_ttys
}


#' Extended RxNorm product term types
#'
#' Returns an extended set of RxNorm term types that includes the default
#' product term types plus dose-form and dose-form-group concepts.
#'
#' This can be useful when a broader set of product-related RxNorm concepts
#' is desired.
#'
#' @return A character vector of RxNorm term type abbreviations.
#'
#' @examples
#' extended_product_ttys()
#'
#' @export
extended_product_ttys <- function() {
  .rxref_extended_product_ttys
}

#' Extended RxNorm term types
#'
#' Returns an extended set of RxNorm term types that includes the default and extended
#' product term types plus drug component, brand name, and ingredient concepts.
#' This list includes essentially all term types that capture a specific ingredient,
#' thus it excludes dose form, dose form group, prescribable name, synonyms, and others
#' that are not associated with a specific ingredient.
#'
#' This can be useful when the broadest set of RxNorm concepts that still capture an ingredient
#' is desired.
#'
#' @return A character vector of RxNorm term type abbreviations.
#'
#' @examples
#' extended_ttys()
#'
#' @export
extended_ttys <- function() {
  .rxref_extended_ttys
}

#' RxNorm product term type sets
#'
#' Returns common RxNorm term type sets used by rxref.
#'
#' @param set One of `"default"`, `"extended_product"`, or `"extended"`.
#'
#' @return A character vector of RxNorm term type abbreviations.
#'
#' @examples
#' product_ttys()
#' product_ttys("extended")
#'
#' @export
product_ttys <- function(set = c("default", "extended_product", "extended")) {
  set <- match.arg(set)

  switch(
    set,
    default = default_product_ttys(),
    extended_product = extended_product_ttys(),
    extended = extended_ttys()
  )
}
