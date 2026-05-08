test_that("find_ingredients finds lisinopril online", {
  skip_if_not(identical(Sys.getenv("RXREF_ONLINE"), "1"))

  out <- find_ingredients("lisinopril")

  expect_s3_class(out, "tbl_df")
  expect_true(any(out$rxcui == "29046"))
  expect_true(any(out$tty == "IN"))
})

test_that("get_properties retrieves known RxCUI online", {
  skip_if_not(identical(Sys.getenv("RXREF_ONLINE"), "1"))

  out <- get_properties("29046")

  expect_equal(out$rxcui, "29046")
  expect_equal(tolower(out$name), "lisinopril")
  expect_equal(out$tty, "IN")
})

test_that("rx_get_json gives informative error when API is unavailable", {
  old <- getOption("rxref.base_url")
  on.exit(options(rxref.base_url = old), add = TRUE)

  options(rxref.base_url = "http://127.0.0.1:9")

  expect_error(
    rx_get_json("/rxcui", query = list(name = "lisinopril")),
    class = "rxref_api_error"
  )
})

test_that("rxclass_get_json gives informative error when API is unavailable", {
  old <- getOption("rxref.rxclass_base_url")
  on.exit(options(rxref.rxclass_base_url = old), add = TRUE)

  options(rxref.rxclass_base_url = "http://127.0.0.1:9")

  expect_error(
    rxclass_get_json("/class/byDrugName", query = list(drugName = "lisinopril")),
    class = "rxref_api_error"
  )
})
