test_that("rxref cache helpers return cachem objects", {
  old_cache <- getOption("rxref.cache")
  on.exit(options(rxref.cache = old_cache), add = TRUE)

  cache <- rxref_cache_use_memory()

  expect_s3_class(cache, "cachem")
  expect_s3_class(rxref_cache(), "cachem")
})


test_that("rxref_cache_info returns expected metadata", {
  old_cache <- getOption("rxref.cache")
  on.exit(options(rxref.cache = old_cache), add = TRUE)

  rxref_cache_use_memory()

  info <- rxref_cache_info()

  expect_s3_class(info, "tbl_df")
  expect_named(info, c("cache_class", "n_keys", "size_bytes"))
  expect_equal(nrow(info), 1)
  expect_true(is.numeric(info$n_keys))
})


test_that("rxref_cache_clear clears current cache", {
  old_cache <- getOption("rxref.cache")
  on.exit(options(rxref.cache = old_cache), add = TRUE)

  cache <- rxref_cache_use_memory()
  cache$set("abc", list(value = 1))

  expect_true("abc" %in% cache$keys())

  rxref_cache_clear()

  expect_false("abc" %in% cache$keys())
})


test_that("rxref_cache_use_disk configures a disk cache", {
  old_cache <- getOption("rxref.cache")
  on.exit(options(rxref.cache = old_cache), add = TRUE)

  dir <- tempfile("rxref-cache-")
  cache <- rxref_cache_use_disk(dir = dir)

  expect_s3_class(cache, "cachem")
  expect_true(dir.exists(dir))
})


test_that("rxref_cache_disable configures an immediately expiring cache", {
  old_cache <- getOption("rxref.cache")
  on.exit(options(rxref.cache = old_cache), add = TRUE)

  cache <- rxref_cache_disable()

  expect_s3_class(cache, "cachem")
  expect_identical(rxref_cache(), cache)
})
