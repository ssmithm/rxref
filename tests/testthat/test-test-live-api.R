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
