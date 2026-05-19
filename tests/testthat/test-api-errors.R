test_that("rx_abort_api creates rxref API errors", {
  expect_error(
    rx_abort_api("Test error.", class = "rxref_test_error"),
    class = "rxref_test_error"
  )

  expect_error(
    rx_abort_api("Test error.", class = "rxref_test_error"),
    class = "rxref_api_error"
  )
})

test_that("rx_perform_json() gives a friendly error for 429 responses", {
  req <- httr2::request("https://example.com")

  local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(status_code = 429)
    },
    .package = "httr2"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    class = "rxref_rate_limit_error"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    regexp = "rate-limited"
  )
})

test_that("rx_perform_json() gives a friendly error for 404 responses", {
  req <- httr2::request("https://example.com")

  local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(status_code = 404)
    },
    .package = "httr2"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    class = "rxref_not_found_error"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    regexp = "status 404"
  )
})

test_that("rx_perform_json() gives a friendly error for 500 responses", {
  req <- httr2::request("https://example.com")

  local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(status_code = 500)
    },
    .package = "httr2"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    class = "rxref_server_error"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    regexp = "temporary server-side issue"
  )
})

test_that("rx_perform_json() gives a friendly error for other 400 responses", {
  req <- httr2::request("https://example.com")

  local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(status_code = 400)
    },
    .package = "httr2"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    class = "rxref_http_error"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    regexp = "unsuccessful response"
  )
})

test_that("rx_perform_json() gives a friendly error for connection failures", {
  req <- httr2::request("https://example.com")

  local_mocked_bindings(
    req_perform = function(req, ...) {
      stop("Could not resolve host")
    },
    .package = "httr2"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    class = "rxref_connection_error"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    regexp = "could not connect"
  )
})

test_that("rx_perform_json() gives a friendly error for malformed JSON", {
  req <- httr2::request("https://example.com")

  local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(
        status_code = 200,
        body = charToRaw("{not valid json")
      )
    },
    .package = "httr2"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    class = "rxref_json_error"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    regexp = "could not be parsed"
  )
})

test_that("rx_perform_json() gives a friendly error for empty response bodies", {
  req <- httr2::request("https://example.com")

  local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(
        status_code = 200,
        body = charToRaw("")
      )
    },
    .package = "httr2"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    class = "rxref_empty_response_error"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    regexp = "response was empty"
  )
})

test_that("rx_perform_json() parses successful JSON responses", {
  req <- httr2::request("https://example.com")

  local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(
        status_code = 200,
        body = charToRaw('{"idGroup":{"rxnormId":["123"]}}')
      )
    },
    .package = "httr2"
  )

  out <- rx_perform_json(req, service = "RxNorm")

  expect_type(out, "list")
  expect_equal(out$idGroup$rxnormId[[1]], "123")
})

test_that("specific API errors inherit from rxref_api_error", {
  req <- httr2::request("https://example.com")

  local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(status_code = 429)
    },
    .package = "httr2"
  )

  expect_error(
    rx_perform_json(req, service = "RxNorm"),
    class = "rxref_api_error"
  )
})
