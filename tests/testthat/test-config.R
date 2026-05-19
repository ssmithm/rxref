test_that("rxref_conf sets and returns options", {
  old <- options(
    rxref.base_url = getOption("rxref.base_url"),
    rxref.rate_delay = getOption("rxref.rate_delay"),
    rxref.cache = getOption("rxref.cache")
  )
  on.exit(options(old), add = TRUE)

  test_cache <- cachem::cache_mem()

  out <- rxref_conf(
    base_url = "https://example.com",
    rate_delay = 0.1,
    cache = test_cache
  )

  expect_equal(out$base_url, "https://example.com")
  expect_equal(out$rate_delay, 0.1)
  expect_identical(out$cache, test_cache)
})

test_that("rxref_conf validates cache objects", {
  expect_error(
    rxref_conf(cache = NULL),
    "`cache` must be a cachem cache object",
    fixed = TRUE
  )

  expect_error(
    rxref_conf(cache = "not-a-cache"),
    "`cache` must be a cachem cache object",
    fixed = TRUE
  )
})
