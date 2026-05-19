test_that("exclude_products_with_ingredients removes entire matching product", {
  dat <- tibble::tibble(
    product_rxcui = c("1", "1", "2"),
    name = c(
      "sacubitril / valsartan tablet",
      "sacubitril / valsartan tablet",
      "valsartan tablet"
    ),
    ingredient_rxcui = c("161", "69749", "69749"),
    ingredient_name = c("sacubitril", "valsartan", "valsartan")
  )

  out <- exclude_products_with_ingredients(
    dat,
    ingredient_rxcuis = "161"
  )

  expect_equal(unique(out$product_rxcui), "2")
  expect_false("1" %in% out$product_rxcui)
})


test_that("exclude_products_with_ingredients handles semicolon-delimited ingredients", {
  dat <- tibble::tibble(
    product_rxcui = c("1", "2"),
    name = c(
      "sacubitril / valsartan tablet",
      "valsartan tablet"
    ),
    ingredient_rxcui = c("161; 69749", "69749"),
    ingredient_name = c("sacubitril; valsartan", "valsartan")
  )

  out <- exclude_products_with_ingredients(
    dat,
    ingredient_rxcuis = "161"
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$product_rxcui, "2")
})


test_that("exclude_products_with_ingredients returns audit output", {
  dat <- tibble::tibble(
    product_rxcui = c("1", "2"),
    name = c(
      "sacubitril / valsartan tablet",
      "valsartan tablet"
    ),
    ingredient_rxcui = c("161; 69749", "69749"),
    ingredient_name = c("sacubitril; valsartan", "valsartan")
  )

  out <- exclude_products_with_ingredients(
    dat,
    ingredient_rxcuis = "161",
    return_excluded = TRUE
  )

  expect_named(out, c("data", "excluded", "resolved_ingredients"))
  expect_equal(out$data$product_rxcui, "2")
  expect_equal(out$excluded$product_rxcui, "1")
})


test_that("exclude_products_with_ingredients errors when no exclusion input supplied", {
  dat <- tibble::tibble(
    product_rxcui = "1",
    ingredient_rxcui = "161"
  )

  expect_error(
    exclude_products_with_ingredients(dat),
    "Supply at least one"
  )
})


test_that("exclude_products_with_ingredients errors without product column", {
  dat <- tibble::tibble(
    rxcui = "1",
    ingredient_rxcui = "161"
  )

  expect_error(
    exclude_products_with_ingredients(dat, ingredient_rxcuis = "161"),
    "product ID column"
  )
})
