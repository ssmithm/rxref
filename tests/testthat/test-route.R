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

test_that("filter_products_by_route filters using mocked clinical route attributes", {
  fake_get_clinical_attributes <- function(rxcui) {
    expect_equal(sort(rxcui), c("10", "11", "12"))

    tibble::tibble(
      rxcui = c("10", "11", "12"),
      dose_form = c(
        "Oral Tablet",
        "Ophthalmic Solution",
        "Injectable Solution"
      ),
      dose_form_group = c(
        "PILL",
        "OPHTHALMIC",
        "INJECTION"
      ),
      route = c(
        "ORAL",
        "OPHTHALMIC",
        "INJECTION"
      )
    )
  }

  testthat::local_mocked_bindings(
    get_clinical_attributes = fake_get_clinical_attributes,
    .package = "rxref"
  )

  x <- tibble::tibble(
    ingredient_rxcui = c("1", "1", "1"),
    product_rxcui = c("10", "11", "12"),
    name = c(
      "timolol 10 MG Oral Tablet",
      "timolol 0.5 % Ophthalmic Solution",
      "timolol 5 MG/ML Injectable Solution"
    ),
    tty = c("SCD", "SCD", "SCD"),
    n_ingredients = c(1L, 1L, 1L)
  )

  out <- filter_products_by_route(x, route = "ORAL")

  expect_equal(nrow(out), 1)
  expect_equal(out$product_rxcui, "10")
  expect_equal(out$routes, "ORAL")
  expect_equal(out$dose_forms, "Oral Tablet")
  expect_equal(out$dose_form_groups, "PILL")
})

test_that("filter_products_by_route can drop appended route columns", {
  fake_get_clinical_attributes <- function(rxcui) {
    tibble::tibble(
      rxcui = c("10", "11"),
      dose_form = c("Oral Tablet", "Ophthalmic Solution"),
      dose_form_group = c("PILL", "OPHTHALMIC"),
      route = c("ORAL", "OPHTHALMIC")
    )
  }

  testthat::local_mocked_bindings(
    get_clinical_attributes = fake_get_clinical_attributes,
    .package = "rxref"
  )

  x <- tibble::tibble(
    ingredient_rxcui = c("1", "1"),
    product_rxcui = c("10", "11"),
    name = c(
      "timolol 10 MG Oral Tablet",
      "timolol 0.5 % Ophthalmic Solution"
    ),
    tty = c("SCD", "SCD"),
    n_ingredients = c(1L, 1L)
  )

  out <- filter_products_by_route(
    x,
    route = "ORAL",
    keep_route_info = FALSE
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$product_rxcui, "10")
  expect_false("routes" %in% names(out))
  expect_false("dose_forms" %in% names(out))
  expect_false("dose_form_groups" %in% names(out))
})
