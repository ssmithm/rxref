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
