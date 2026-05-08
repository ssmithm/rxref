test_that("estradiol clinical attributes can be unnested", {
  skip_if_not(identical(Sys.getenv("RXREF_ONLINE"), "1"))

  a1 <- find_ingredients("estradiol")
  a2 <- products_for_ingredients(a1$rxcui)
  a3 <- get_clinical_attributes(a2$product_rxcui)

  expect_s3_class(a3, "tbl_df")
  expect_true(all(c(
    "ingredient_rxcui",
    "ingredient_name",
    "ingredient_tty"
  ) %in% names(a3)))

  expect_no_error(
    tidyr::unnest(a3, c(ingredient_rxcui, ingredient_name, ingredient_tty))
  )
})
