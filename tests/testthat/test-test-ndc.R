test_that("ndc_to_11 handles common FDA 10-digit formats", {
  expect_equal(rxref:::ndc_to_11("0002-0800-01"), "00002080001") # 4-4-2
  expect_equal(rxref:::ndc_to_11("50242-040-62"), "50242004062") # 5-3-2
  expect_equal(rxref:::ndc_to_11("60505-0829-1"), "60505082901") # 5-4-1
  expect_equal(rxref:::ndc_to_11("60505-0829-01"), "60505082901") # 5-4-2
})

test_that("ndc_to_11 handles non-hyphenated values", {
  expect_equal(rxref:::ndc_to_11("60505082901"), "60505082901")
  expect_equal(rxref:::ndc_to_11("1234567890"), "01234567890")
})

test_that("is_ndcish identifies 10/11 digit NDC-like strings", {
  expect_true(rxref:::is_ndcish("1234567890"))
  expect_true(rxref:::is_ndcish("12345-6789-01"))
  expect_false(rxref:::is_ndcish("lisinopril"))
})
