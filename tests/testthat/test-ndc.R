test_that("ndc_to_11 handles common FDA 10-digit formats", {
  expect_equal(rxref:::ndc_to_11("0002-0800-01"), "00002080001") # 4-4-2
  expect_equal(rxref:::ndc_to_11("50242-040-62"), "50242004062") # 5-3-2
  expect_equal(rxref:::ndc_to_11("60505-0829-1"), "60505082901") # 5-4-1
  expect_equal(rxref:::ndc_to_11("60505-0829-01"), "60505082901") # 5-4-2

  # Unhyphenated 10-digit NDCs are ambiguous because the original grouping
  # could be 4-4-2, 5-3-2, or 5-4-1.
  expect_equal(rxref:::ndc_to_11("1234567890"), NA_character_)
})

test_that("ndc_to_11 handles non-hyphenated values", {
  expect_equal(rxref:::ndc_to_11("60505082901"), "60505082901")
  expect_equal(rxref:::ndc_to_11("00002080001"), "00002080001")
  expect_equal(rxref:::ndc_to_11("50242004062"), "50242004062")
})

test_that("is_ndcish identifies 10/11 digit NDC-like strings", {
  expect_true(rxref:::is_ndcish("1234567890"))
  expect_true(rxref:::is_ndcish("12345-6789-01"))
  expect_false(rxref:::is_ndcish("lisinopril"))
})

test_that("is_ndcish recognizes 10- and 11-digit NDC-like values", {
  expect_true(rxref:::is_ndcish("0002-0800-01"))
  expect_true(rxref:::is_ndcish("00002080001"))
  expect_true(rxref:::is_ndcish("50242-040-62"))
  expect_true(rxref:::is_ndcish("60505-0829-1"))

  expect_false(rxref:::is_ndcish(NA_character_))
  expect_false(rxref:::is_ndcish("lisinopril"))
  expect_false(rxref:::is_ndcish("12345"))
})

test_that(".ndc_parts_from_string parses hyphenated and normalized NDCs", {
  x <- rxref:::.ndc_parts_from_string("0002-0800-01")

  expect_equal(x$labeler, "0002")
  expect_equal(x$product, "0800")
  expect_equal(x$package, "01")

  y <- rxref:::.ndc_parts_from_string("00002080001")

  expect_equal(y$labeler, "00002")
  expect_equal(y$product, "0800")
  expect_equal(y$package, "01")
})

test_that(".ndc_parts_from_string does not infer grouping for unhyphenated 10-digit NDCs", {
  x <- rxref:::.ndc_parts_from_string("0002080001")

  expect_true(is.na(x$labeler))
  expect_true(is.na(x$product))
  expect_true(is.na(x$package))
})

test_that("hyphenate_ndc_5_4_2 hyphenates normalized 11-digit NDCs", {
  expect_equal(
    rxref:::hyphenate_ndc_5_4_2("00002080001"),
    "00002-0800-01"
  )

  expect_equal(
    rxref:::hyphenate_ndc_5_4_2("50242004062"),
    "50242-0040-62"
  )
})

test_that("hyphenate_ndc_5_4_2 is vectorized and preserves invalid inputs", {
  expect_equal(
    rxref:::hyphenate_ndc_5_4_2(c("00002080001", "not-an-ndc", NA_character_)),
    c("00002-0800-01", "not-an-ndc", NA_character_)
  )
})
