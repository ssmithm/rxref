test_that("filter_products_by_route requires product_rxcui", {
  bad <- tibble::tibble(rxcui = "123")

  expect_error(
    filter_products_by_route(bad, route = "ORAL"),
    "product_rxcui"
  )
})


test_that("filter_products_by_route keeps oral metoprolol products", {
  skip_if_not(identical(Sys.getenv("RXREF_ONLINE"), "1"))

  ing <- find_ingredients("metoprolol") |>
    dplyr::filter(tty == "IN")

  prods <- products_for_ingredients(
    ing$rxcui,
    include_combos = TRUE
  )

  oral <- filter_products_by_route(prods, route = "ORAL")

  expect_true(nrow(oral) > 0)
  expect_true(all(grepl("Oral|Tablet|Capsule|Solution", oral$name, ignore.case = TRUE)))
})

test_that("search_drug can filter products by route", {
  skip_if_not(identical(Sys.getenv("RXREF_ONLINE"), "1"))

  res <- search_drug(
    "timolol",
    return = "rxcui",
    route = "ORAL",
    include_combos = TRUE
  )

  expect_true(is.data.frame(res))

  if (nrow(res) > 0) {
    expect_false(any(grepl("ophthalmic", res$name, ignore.case = TRUE)))
  }
})
