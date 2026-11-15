test_that("barcode lookup returns name and details", {
  api <- local_fake_api('[{"ean":"5099750442227","name":"Michael Jackson: Thriller","categoryId":"45","categoryName":"Music","googleCategoryId":"855","issuingCountry":"UK"}]')
  expect_equal(barcode_lookup("5099750442227", token = "t"), "Michael Jackson: Thriller")
  expect_match(api$urls[1], "op=barcode-lookup&ean=5099750442227&language=1", fixed = TRUE)
  expect_match(api$urls[1], "token=t&format=json", fixed = TRUE)

  p <- barcode_search("5099750442227", lang = 3, token = "t")
  expect_identical(p$categoryId, 45L)
  expect_identical(p$googleCategoryId, 855L)
  expect_equal(p$issuingCountry, "UK")
  expect_match(api$urls[2], "language=3", fixed = TRUE)
  expect_equal(eansearch_error(), "")
})

test_that("numeric EANs are not formatted in scientific notation", {
  api <- local_fake_api('[{"ean":"5099750442227","name":"x"}]')
  barcode_lookup(5099750442227, token = "t")
  expect_match(api$urls[1], "ean=5099750442227&", fixed = TRUE)
})

test_that("not found returns NA / NULL and sets the error", {
  local_fake_api('[{"error":"Barcode not found"}]')
  expect_identical(barcode_lookup("1", token = "t"), NA_character_)
  expect_equal(eansearch_error(), "Barcode not found")
  expect_null(barcode_search("1", token = "t"))
  expect_identical(find_asin_for_ean("1", token = "t"), NA_character_)
  expect_identical(verify_checksum("1", token = "t"), NA)
  expect_null(barcode_image("1", token = "t"))
})

test_that("unexpected responses are handled", {
  local_fake_api("[]")
  expect_identical(barcode_lookup("1", token = "t"), NA_character_)
  local_fake_api("garbage")
  expect_identical(find_lccn_for_ean("1", token = "t"), NA_character_)
  expect_equal(eansearch_error(), "Invalid response from API")
})

test_that("ISBN lookup cleans input and handles ISBN-13 as EAN", {
  api <- local_fake_api('[{"ean":"9781119578888","name":"Book"}]')
  expect_equal(isbn_lookup("1-119-57888-4", token = "t"), "Book")
  expect_match(api$urls[1], "isbn=1119578884", fixed = TRUE)
  isbn_lookup("978-1119578888", token = "t")
  expect_match(api$urls[2], "ean=9781119578888", fixed = TRUE)
})

test_that("ASIN and LCCN mappings", {
  api <- local_fake_api('[{"ean":"9781119578888","asin":"1119578884"}]')
  expect_equal(find_asin_for_ean("9781119578888", token = "t"), "1119578884")
  expect_match(api$urls[1], "op=asin-for-ean-lookup&ean=9781119578888", fixed = TRUE)
  expect_equal(find_ean_for_asin("1119578884", token = "t"), "9781119578888")

  api <- local_fake_api('[{"ean":"9781119578888","lccn":"2019 000000"}]')
  expect_equal(find_lccn_for_ean("9781119578888", token = "t"), "2019 000000")
  expect_equal(find_ean_for_lccn("2019 000000", token = "t"), "9781119578888")
  expect_match(api$urls[2], "op=ean-for-lccn-lookup&lccn=2019%20000000", fixed = TRUE)
})

test_that("checksum, issuing country and barcode image", {
  local_fake_api('[{"ean":"5099750442227","valid":"1"}]')
  expect_true(verify_checksum("5099750442227", token = "t"))
  local_fake_api('[{"ean":"5099750442228","valid":"0"}]')
  expect_false(verify_checksum("5099750442228", token = "t"))

  local_fake_api('[{"ean":"5099750442227","name":"x","issuingCountry":"UK"}]')
  expect_equal(issuing_country_lookup("5099750442227", token = "t"), "UK")

  api <- local_fake_api('[{"ean":"5099750442227","barcode":"iVBORw0KGgo="}]')
  img <- barcode_image("5099750442227", 300, 200, token = "t")
  expect_type(img, "raw")
  expect_equal(img[2:4], charToRaw("PNG"))
  expect_match(api$urls[1], "width=300&height=200", fixed = TRUE)
  expect_equal(barcode_image("5099750442227", base64 = TRUE, token = "t"), "iVBORw0KGgo=")
})

test_that("invalid arguments are rejected", {
  expect_error(barcode_lookup(c("1", "2"), token = "t"), "single")
  expect_error(barcode_lookup(NA, token = "t"), "single")
})

test_that("missing token gives a helpful error", {
  withr_env <- Sys.getenv("EAN_SEARCH_API_TOKEN", unset = NA)
  Sys.unsetenv("EAN_SEARCH_API_TOKEN")
  on.exit(if (!is.na(withr_env)) Sys.setenv(EAN_SEARCH_API_TOKEN = withr_env))
  expect_error(barcode_lookup("5099750442227"), "EAN_SEARCH_API_TOKEN")
})
