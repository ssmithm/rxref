#' Exclude products containing specified ingredients
#'
#' Remove all rows for products that contain one or more user-specified
#' ingredients. This is useful when broad ingredient-based product searches
#' return fixed-dose combination products that contain an ingredient of interest
#' but are not clinically relevant to a specific study definition.
#'
#' Exclusions are applied at the product level. If any ingredient in a product
#' matches the exclusion list, all rows for that product are removed.
#'
#' @param data A data frame containing product RxCUIs and ingredient information,
#'   such as that produced by [products_for_ingredients()].
#' @param ingredients Optional character vector of ingredient names to exclude.
#'   These are resolved to ingredient RxCUIs using [find_ingredients()].
#' @param ingredient_rxcuis Optional character vector of ingredient RxCUIs to
#'   exclude. These are used directly and do not require name resolution.
#' @param product_id_col Name of the column containing product RxCUIs. Default
#'   is `product_rxcui`.
#' @param ingredient_name_col Name of the column containing ingredient names.
#'   Used for reporting and, when needed, fallback matching. Default is
#'   `ingredient_name`.
#' @param ingredient_rxcui_col Name of the column containing ingredient RxCUIs.
#'   Default is `ingredient_rxcui`.
#' @param return_excluded Logical. If `FALSE`, return the filtered data frame.
#'   If `TRUE`, return a list with `data`, `excluded`, and
#'   `resolved_ingredients`.
#'
#' @return
#' If `return_excluded = FALSE`, a data frame of the same general shape as
#' `data`, with excluded product rows removed.
#'
#' If `return_excluded = TRUE`, a list with:
#' \describe{
#'   \item{data}{The filtered data frame.}
#'   \item{excluded}{Rows from the input data belonging to excluded products.}
#'   \item{resolved_ingredients}{The ingredient names/RxCUIs used for exclusion.}
#' }
#'
#' @examples
#' \dontrun{
#' antihtn_clean <- antihtn_products |>
#'   exclude_products_with_ingredients(ingredients = "sacubitril")
#'
#' antihtn_audit <- antihtn_products |>
#'   exclude_products_with_ingredients(
#'     ingredients = "sacubitril",
#'     return_excluded = TRUE
#'   )
#'
#' antihtn_audit$data
#' antihtn_audit$excluded
#' antihtn_audit$resolved_ingredients
#' }
#'
#' @export
exclude_products_with_ingredients <- function(
    data,
    ingredients = NULL,
    ingredient_rxcuis = NULL,
    product_id_col = "product_rxcui",
    ingredient_name_col = "ingredient_name",
    ingredient_rxcui_col = "ingredient_rxcui",
    return_excluded = FALSE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!product_id_col %in% names(data)) {
    stop(
      "`data` must contain the product ID column `",
      product_id_col,
      "`.",
      call. = FALSE
    )
  }

  has_ingredient_rxcui_col <- ingredient_rxcui_col %in% names(data)
  has_ingredient_name_col <- ingredient_name_col %in% names(data)

  if (!has_ingredient_rxcui_col && !has_ingredient_name_col) {
    stop(
      "`data` must contain either `",
      ingredient_rxcui_col,
      "` or `",
      ingredient_name_col,
      "`.",
      call. = FALSE
    )
  }

  if (is.null(ingredients) && is.null(ingredient_rxcuis)) {
    stop(
      "Supply at least one of `ingredients` or `ingredient_rxcuis`.",
      call. = FALSE
    )
  }

  ingredient_rxcuis <- unique(as.character(ingredient_rxcuis))
  ingredient_rxcuis <- ingredient_rxcuis[!is.na(ingredient_rxcuis) & ingredient_rxcuis != ""]

  resolved_ingredients <- tibble::tibble(
    input = character(),
    ingredient_rxcui = character(),
    ingredient_name = character(),
    tty = character(),
    score = numeric()
  )

  if (!is.null(ingredients)) {
    ingredients <- unique(as.character(ingredients))
    ingredients <- ingredients[!is.na(ingredients) & ingredients != ""]

    if (length(ingredients) > 0) {
      resolved_ingredients <- find_ingredients(ingredients) |>
        dplyr::transmute(
          input = .data$input,
          ingredient_rxcui = .data$rxcui,
          ingredient_name = .data$name,
          tty = .data$tty,
          score = .data$score
        )

      resolved_rxcuis <- unique(as.character(resolved_ingredients$ingredient_rxcui))
      resolved_rxcuis <- resolved_rxcuis[!is.na(resolved_rxcuis) & resolved_rxcuis != ""]

      ingredient_rxcuis <- unique(c(ingredient_rxcuis, resolved_rxcuis))
    }
  }

  if (length(ingredient_rxcuis) == 0 && !has_ingredient_name_col) {
    stop(
      "No valid ingredient RxCUIs were supplied or resolved.",
      call. = FALSE
    )
  }

  ingredient_map <- normalize_product_ingredients(
    data = data,
    product_id_col = product_id_col,
    ingredient_name_col = ingredient_name_col,
    ingredient_rxcui_col = ingredient_rxcui_col
  )

  if (length(ingredient_rxcuis) > 0 && "ingredient_rxcui" %in% names(ingredient_map)) {
    products_to_exclude <- ingredient_map |>
      dplyr::filter(.data$ingredient_rxcui %in% ingredient_rxcuis) |>
      dplyr::distinct(.data$product_id)
  } else {
    products_to_exclude <- tibble::tibble(product_id = character())
  }

  # Fallback: if ingredient names were supplied but no RxCUI match was possible,
  # use normalized exact name matching.
  if (
    nrow(products_to_exclude) == 0 &&
    !is.null(ingredients) &&
    has_ingredient_name_col &&
    "ingredient_name" %in% names(ingredient_map)
  ) {
    ingredients_norm <- normalize_ingredient_name(ingredients)

    products_to_exclude <- ingredient_map |>
      dplyr::filter(normalize_ingredient_name(.data$ingredient_name) %in% ingredients_norm) |>
      dplyr::distinct(.data$product_id)
  }

  product_ids_to_exclude <- products_to_exclude$product_id

  filtered_data <- data |>
    dplyr::filter(!as.character(.data[[product_id_col]]) %in% product_ids_to_exclude)

  excluded_data <- data |>
    dplyr::filter(as.character(.data[[product_id_col]]) %in% product_ids_to_exclude)

  if (return_excluded) {
    return(
      list(
        data = filtered_data,
        excluded = excluded_data,
        resolved_ingredients = resolved_ingredients
      )
    )
  }

  filtered_data
}

# Internal helpers

#' Normalize product ingredient fields
#'
#' Converts product-level ingredient columns into a long product-ingredient
#' mapping table. Handles single values, semicolon-delimited fixed-dose
#' combination values, and older pseudo-vector strings.
#'
#' @param data A data frame containing product and ingredient columns.
#' @param product_id_col Name of the product identifier column.
#' @param ingredient_name_col Name of the ingredient name column.
#' @param ingredient_rxcui_col Name of the ingredient RxCUI column.
#'
#' @return A tibble with `product_id`, `ingredient_rxcui`, and
#'   `ingredient_name`.
#'
#' @noRd
normalize_product_ingredients <- function(
    data,
    product_id_col,
    ingredient_name_col,
    ingredient_rxcui_col
) {
  product_id <- as.character(data[[product_id_col]])

  has_rxcui <- ingredient_rxcui_col %in% names(data)
  has_name <- ingredient_name_col %in% names(data)

  rxcui_list <- if (has_rxcui) {
    split_ingredient_field(data[[ingredient_rxcui_col]])
  } else {
    vector("list", length(product_id))
  }

  name_list <- if (has_name) {
    split_ingredient_field(data[[ingredient_name_col]])
  } else {
    vector("list", length(product_id))
  }

  pieces <- lapply(seq_along(product_id), function(i) {
    rxcuis <- if (has_rxcui) rxcui_list[[i]] else NA_character_
    names_ <- if (has_name) name_list[[i]] else NA_character_

    n <- max(length(rxcuis), length(names_))

    tibble::tibble(
      product_id = rep(product_id[[i]], n),
      ingredient_rxcui = rep_len(as.character(rxcuis), n),
      ingredient_name = rep_len(as.character(names_), n)
    )
  })

  dplyr::bind_rows(pieces) |>
    dplyr::mutate(
      ingredient_rxcui = trimws(.data$ingredient_rxcui),
      ingredient_name = trimws(.data$ingredient_name)
    ) |>
    dplyr::filter(!is.na(.data$product_id), .data$product_id != "")
}

#' Split ingredient fields
#'
#' Splits ingredient fields that may contain single values, semicolon-delimited
#' values, or older pseudo-vector strings such as `c("a", "b")`.
#'
#' @param x A character vector.
#'
#' @return A list of character vectors.
#'
#' @noRd
split_ingredient_field <- function(x) {
  lapply(x, function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value)) {
      return(NA_character_)
    }

    value <- as.character(value)

    # Handle semicolon-delimited values, e.g. "sacubitril; valsartan"
    if (grepl(";", value, fixed = TRUE)) {
      return(trimws(strsplit(value, ";", fixed = TRUE)[[1]]))
    }

    # Handle older pseudo-vector strings, e.g. c("sacubitril", "valsartan")
    if (grepl("^c\\(", value)) {
      value <- gsub("^c\\(|\\)$", "", value)
      value <- gsub("\"|'", "", value)
      return(trimws(strsplit(value, ",", fixed = TRUE)[[1]]))
    }

    trimws(value)
  })
}

#' Normalize ingredient names for matching
#'
#' Converts ingredient names to trimmed lower-case character values.
#'
#' @param x A character vector.
#'
#' @return A normalized character vector.
#'
#' @noRd
normalize_ingredient_name <- function(x) {
  x |>
    as.character() |>
    trimws() |>
    tolower()
}
