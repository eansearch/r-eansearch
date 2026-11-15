test_that("HTTP 429 is retried up to 3 times", {
  ok <- list(status = 200L, body = '[{"ean":"1","asin":"B"}]')
  busy <- list(status = 429L, body = "")
  api <- local_fake_api(responses = list(busy, busy, ok))
  expect_equal(find_asin_for_ean("1", token = "t"), "B")
  expect_length(api$urls, 3L)
  expect_equal(api$pauses, 2L)

  api <- local_fake_api(responses = list(busy))
  expect_identical(find_asin_for_ean("1", token = "t"), NA_character_)
  expect_length(api$urls, 3L)
  expect_equal(eansearch_error(), "Server error 429")
})

test_that("HTTP errors are reported", {
  local_fake_api(status = 402L, body = "")
  expect_identical(barcode_lookup("1", token = "t"), NA_character_)
  expect_equal(eansearch_error(), "Server error 402")
})

test_that("connection errors and timeouts fail gracefully", {
  local_fake_api()
  local_mocked_bindings(http_get = function(url) stop("Timeout was reached: [api] Operation timed out"))
  expect_identical(barcode_lookup("1", token = "t"), NA_character_)
  expect_equal(eansearch_error(), "Timeout")

  local_mocked_bindings(http_get = function(url) stop("Could not resolve host: api.ean-search.org"))
  expect_equal(nrow(product_search("x", token = "t")), 0L)
  expect_match(eansearch_error(), "^Connection error Could not resolve host")
})

test_that("credits remaining is tracked and cached", {
  api <- local_fake_api('[{"requests":"1"}]', credits = "42")
  expect_equal(credits_remaining(token = "t"), 42L)
  expect_match(api$urls[1], "op=account-status", fixed = TRUE)
  credits_remaining(token = "t")
  expect_length(api$urls, 1L)
})

test_that("error is cleared by the next successful call", {
  local_fake_api(responses = list(list(status = 500L, body = ""),
                                  list(status = 200L, body = '[{"ean":"1","name":"x"}]')))
  barcode_lookup("1", token = "t")
  expect_equal(eansearch_error(), "Server error 500")
  barcode_lookup("1", token = "t")
  expect_equal(eansearch_error(), "")
})

test_that("language codes", {
  expect_identical(ean_languages[["english"]], 1L)
  expect_identical(ean_languages[["any"]], 99L)
})
