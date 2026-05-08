test_that("default_product_ttys returns expected product TTYs", {
  expect_type(default_product_ttys(), "character")
  expect_equal(
    default_product_ttys(),
    c("SCD", "SBD", "GPCK", "BPCK")
  )
})

test_that("extended_product_ttys includes default product TTYs", {
  expect_type(extended_product_ttys(), "character")
  expect_true(all(default_product_ttys() %in% extended_product_ttys()))
  expect_true(length(extended_product_ttys()) > length(default_product_ttys()))
})

test_that("product_ttys returns requested TTY set", {
  expect_equal(product_ttys("default"), default_product_ttys())
  expect_equal(product_ttys("extended_product"), extended_product_ttys())
  expect_equal(product_ttys("extended"), extended_ttys())
})
