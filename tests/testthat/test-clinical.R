test_that(".rxref_collapse_values() trims, drops missing values, and collapses unique values", {
  expect_equal(
    .rxref_collapse_values(c("17767", "69749")),
    "17767; 69749"
  )

  expect_equal(
    .rxref_collapse_values(c("amlodipine", "valsartan ")),
    "amlodipine; valsartan"
  )

  expect_equal(
    .rxref_collapse_values(c("IN", "IN")),
    "IN"
  )

  expect_equal(
    .rxref_collapse_values(c(" amlodipine ", "amlodipine", NA, "")),
    "amlodipine"
  )

  expect_equal(
    .rxref_collapse_values(character(0)),
    NA_character_
  )

  expect_equal(
    .rxref_collapse_values(c(NA_character_, "")),
    NA_character_
  )
})


mock_hist_status_two_ingredients <- function() {
  list(
    rxcuiStatusHistory = list(
      metaData = list(
        status = "Obsolete",
        isCurrent = "NO"
      ),
      attributes = list(
        rxcui = "999999",
        name = "amlodipine 5 MG / valsartan 160 MG Oral Tablet",
        tty = "SCD",
        isMultipleIngredient = "YES"
      ),
      definitionalFeatures = list(
        doseFormConcept = list(
          list(
            doseFormRxcui = "317541",
            doseFormName = "Oral Tablet"
          )
        ),
        doseFormGroupConcept = list(
          list(
            doseFormGroupRxcui = "1151131",
            doseFormGroupName = "Oral Product"
          )
        ),
        ingredientAndStrength = list(
          list(
            baseRxcui = "17767",
            baseName = "amlodipine"
          ),
          list(
            baseRxcui = "69749",
            baseName = "valsartan"
          )
        )
      ),
      derivedConcepts = list(
        ingredientConcept = list(
          # Deliberately duplicated with ingredientAndStrength above.
          # This protects the de-duplication logic.
          list(
            ingredientRxcui = "17767",
            ingredientName = "amlodipine"
          )
        )
      )
    )
  )
}


mock_hist_status_no_ingredients <- function() {
  list(
    rxcuiStatusHistory = list(
      metaData = list(
        status = "Obsolete",
        isCurrent = "NO"
      ),
      attributes = list(
        rxcui = "888888",
        name = "obsolete product with unavailable ingredients",
        tty = "SCD",
        isMultipleIngredient = NA_character_
      ),
      definitionalFeatures = list(
        doseFormConcept = list(),
        doseFormGroupConcept = list(),
        ingredientAndStrength = list()
      ),
      derivedConcepts = list(
        ingredientConcept = list()
      )
    )
  )
}


test_that("historical clinical attributes combine and deduplicate ingredient sources", {
  testthat::local_mocked_bindings(
    rx_get_json = function(path, query = NULL) {
      expect_equal(path, "/rxcui/999999/historystatus")
      mock_hist_status_two_ingredients()
    }
  )

  out <- .rxref_clinical_attributes_historical(
    "999999",
    show_progress = FALSE
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)

  expect_equal(out$rxcui, "999999")
  expect_equal(out$related_rxcui, "999999")
  expect_equal(out$tty, "SCD")

  expect_equal(out$ingredient_count, 2L)
  expect_equal(out$ingredient_rxcui, "17767; 69749")
  expect_equal(out$ingredient_name, "amlodipine; valsartan")
  expect_equal(out$ingredient_tty, "IN")
  expect_true(out$is_multi_ingredient)

  expect_equal(out$dose_form, "Oral Tablet")
  expect_equal(out$dose_form_group, "Oral Product")
  expect_equal(out$route, "ORAL")
})


test_that("historical clinical attributes return NA ingredient count when no ingredients are found", {
  testthat::local_mocked_bindings(
    rx_get_json = function(path, query = NULL) {
      expect_equal(path, "/rxcui/888888/historystatus")
      mock_hist_status_no_ingredients()
    }
  )

  out <- .rxref_clinical_attributes_historical(
    "888888",
    show_progress = FALSE
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)

  expect_equal(out$rxcui, "888888")
  expect_equal(out$related_rxcui, "888888")

  expect_true(is.na(out$ingredient_count))
  expect_true(is.na(out$ingredient_rxcui))
  expect_true(is.na(out$ingredient_name))
  expect_true(is.na(out$ingredient_tty))
  expect_true(is.na(out$is_multi_ingredient))
})


test_that("missing active ingredient count triggers historical fallback", {
  testthat::local_mocked_bindings(
    rx_get_json = function(path, query = NULL) {
      if (identical(path, "/rxcui/999999/properties")) {
        return(list(
          properties = list(
            rxcui = "999999",
            name = "amlodipine 5 MG / valsartan 160 MG Oral Tablet",
            tty = "SCD",
            suppress = "N"
          )
        ))
      }

      if (
        identical(path, "/rxcui/999999/related") &&
        identical(query, list(tty = "DFG"))
      ) {
        return(list(
          relatedGroup = list(
            conceptGroup = list()
          )
        ))
      }

      if (identical(path, "/rxcui/999999/historystatus")) {
        return(mock_hist_status_two_ingredients())
      }

      stop("Unexpected mocked rx_get_json() call: ", path)
    },
    .rxref_get_ingredients_for_rxcui = function(rxcui,
                                                include_pin = TRUE,
                                                include_min = FALSE) {
      tibble::tibble(
        related_rxcui = character(),
        ingredient_rxcui = character(),
        ingredient_name = character(),
        ingredient_tty = character()
      )
    }
  )

  out <- get_clinical_attributes(
    "999999",
    include_historical = TRUE,
    show_progress = FALSE
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)

  expect_equal(out$rxcui, "999999")
  expect_equal(out$related_rxcui, "999999")

  expect_equal(out$ingredient_count, 2L)
  expect_equal(out$ingredient_rxcui, "17767; 69749")
  expect_equal(out$ingredient_name, "amlodipine; valsartan")
  expect_equal(out$ingredient_tty, "IN")
  expect_true(out$is_multi_ingredient)

  expect_equal(out$dose_form_group, "Oral Product")
  expect_equal(out$route, "ORAL")
  expect_equal(out$history_status, "Obsolete")
  expect_equal(out$history_is_current, "NO")
})
