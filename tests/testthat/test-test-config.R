test_that("rxref_conf sets and returns options", {
  old <- options(
    rxref.base_url = getOption("rxref.base_url"),
    rxref.rate_delay = getOption("rxref.rate_delay"),
    rxref.cache = getOption("rxref.cache")
  )
  on.exit(options(old), add = TRUE)

  out <- rxref_conf(
    base_url = "https://example.com",
    rate_delay = 0.1,
    cache = NULL
  )

  expect_equal(out$base_url, "https://example.com")
  expect_equal(out$rate_delay, 0.1)
  expect_null(out$cache)
})

test_that("set_backend only accepts api", {
  expect_equal(set_backend("api"), "api")
  expect_error(set_backend("local"))
})
