test_that("get_properties parses RxNorm properties response", {
  fake_rx_get_json <- function(path, query = list()) {
    expect_equal(path, "/rxcui/29046/properties")

    list(
      properties = list(
        rxcui = "29046",
        name = "lisinopril",
        synonym = "",
        tty = "IN",
        language = "ENG",
        suppress = "N",
        umlscui = "C0065374"
      )
    )
  }

  testthat::local_mocked_bindings(
    rx_get_json = fake_rx_get_json,
    .package = "rxref"
  )

  out <- get_properties("29046")

  expect_s3_class(out, "tbl_df")
  expect_equal(out$rxcui, "29046")
  expect_equal(out$name, "lisinopril")
  expect_equal(out$tty, "IN")
  expect_equal(out$suppress, "N")
})

test_that("get_properties() returns expected columns for missing input", {
  out <- get_properties(NA_character_, show_progress = FALSE)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
  expect_named(out, c(
    "rxcui",
    "name",
    "synonym",
    "tty",
    "language",
    "suppress",
    "umlscui"
  ))
  expect_true(is.na(out$rxcui))
  expect_true(is.na(out$name))
})
