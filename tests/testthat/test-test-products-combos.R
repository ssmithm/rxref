test_that("products_for_ingredients can exclude combination products", {
  skip_if_not(identical(Sys.getenv("RXREF_ONLINE"), "1"))

  ing <- find_ingredients("lisinopril") |>
    dplyr::filter(tty == "IN")

  single_only <- products_for_ingredients(
    ing$rxcui,
    ttys = c("SCD", "SBD"),
    include_combos = FALSE
  )

  expect_s3_class(single_only, "tbl_df")
  expect_true(nrow(single_only) > 0)
  expect_true(all(single_only$n_ingredients <= 1))
})

test_that("products_for_ingredients includes combination products when requested", {
  skip_if_not(identical(Sys.getenv("RXREF_ONLINE"), "1"))

  ing <- find_ingredients("lisinopril") |>
    dplyr::filter(tty == "IN")

  with_combos <- products_for_ingredients(
    ing$rxcui,
    ttys = c("SCD", "SBD"),
    include_combos = TRUE
  )

  expect_true(any(with_combos$n_ingredients > 1))
  expect_true(any(grepl("hydrochlorothiazide", with_combos$name, ignore.case = TRUE)))
})
