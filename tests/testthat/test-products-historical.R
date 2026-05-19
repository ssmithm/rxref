test_that("products_for_ingredients active only excludes historical products", {
  skip_if_not(identical(Sys.getenv("RXREF_ONLINE"), "1"))

  out <- products_for_ingredients(
    "1369",
    ttys = "SCD",
    route = NULL,
    include_combos = FALSE,
    concept_status = "active"
  )

  expect_false("concept_status" %in% names(out))
  expect_false("active_start_date" %in% names(out))
  expect_false("active_end_date" %in% names(out))
  expect_false("release_start_date" %in% names(out))
  expect_false("release_end_date" %in% names(out))

  expect_equal(nrow(out), 2)
  expect_setequal(out$product_rxcui, c("197394", "308614"))
  expect_true(all(out$tty == "SCD"))
  expect_true(all(out$n_ingredients == 1L))
})

test_that("products_for_ingredients active_and_historical includes obsolete SCD products", {
  skip_if_not(identical(Sys.getenv("RXREF_ONLINE"), "1"))
  skip_if_not(identical(Sys.getenv("RXREF_SLOW"), "1"))

  out <- products_for_ingredients(
    "1369",
    ttys = "SCD",
    route = NULL,
    include_combos = FALSE,
    concept_status = "active_and_historical"
  )

  expect_true("concept_status" %in% names(out))
  expect_true("active_start_date" %in% names(out))
  expect_true("active_end_date" %in% names(out))
  expect_true("release_start_date" %in% names(out))
  expect_true("release_end_date" %in% names(out))

  expect_setequal(
    out$product_rxcui,
    c("197394", "308614", "197393", "308617", "429502")
  )

  expect_setequal(out$concept_status, c("Active", "Obsolete"))
  expect_true(all(out$tty == "SCD"))
  expect_true(all(out$n_ingredients == 1L))
})

test_that("historical combination products have correct ingredient counts", {
  skip_if_not(identical(Sys.getenv("RXREF_ONLINE"), "1"))
  skip_if_not(identical(Sys.getenv("RXREF_SLOW"), "1"))

  out <- products_for_ingredients(
    "1369",
    ttys = product_ttys("extended_product"),
    route = NULL,
    include_combos = TRUE,
    concept_status = "active_and_historical"
  )

  x <- dplyr::filter(out, product_rxcui == "104267")

  expect_equal(nrow(x), 1)
  expect_equal(x$n_ingredients, 2L)
  expect_equal(x$concept_status, "Obsolete")
})

test_that("products_for_ingredients validates concept_status", {
  expect_error(
    products_for_ingredients(
      "1369",
      concept_status = "current"
    ),
    "should be one of"
  )
})

test_that("historical_status can limit returned historical statuses", {
  skip_if_not(identical(Sys.getenv("RXREF_ONLINE"), "1"))
  skip_if_not(identical(Sys.getenv("RXREF_SLOW"), "1"))

  out <- products_for_ingredients(
    "1369",
    ttys = "SCD",
    route = NULL,
    include_combos = FALSE,
    concept_status = "active_and_historical",
    historical_status = "Obsolete"
  )

  expect_true(all(out$concept_status %in% c("Active", "Obsolete")))
})

test_that("products_for_ingredients rejects deprecated concept_status values", {
  expect_error(
    products_for_ingredients(
      "11289",
      concept_status = "current_and_historical"
    ),
    "active"
  )
})
