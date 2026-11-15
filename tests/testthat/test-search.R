search_json <- '{"page":0,"moreproducts":true,"totalproducts":2,"productlist":[
  {"ean":"0885909129852","name":"Apple iPod nano","categoryId":"15","categoryName":"Audio","issuingCountry":"US"},
  {"ean":"4007249146019","name":"iPod Case","categoryId":"0","categoryName":"Unknown","issuingCountry":"DE"}]}'

test_that("product search returns a data frame with page info", {
  api <- local_fake_api(search_json)
  res <- product_search("iPod nano", token = "t")
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2L)
  expect_equal(res$ean[1], "0885909129852")
  expect_identical(res$categoryId, c(15L, 0L))
  expect_true(attr(res, "more_products"))
  expect_equal(attr(res, "total_products"), 2)
  expect_match(api$urls[1], "op=product-search&name=iPod%20nano&page=0&language=1", fixed = TRUE)
})

test_that("all search operations build the right URL", {
  api <- local_fake_api(search_json)
  similar_product_search("Größe", page = 2, lang = 99, token = "t")
  category_search(45, "thriller", token = "t")
  barcode_prefix_search("4007249146", token = "t")
  expect_match(api$urls[1], "op=similar-product-search&name=Gr%C3%B6%C3%9Fe&page=2&language=99", fixed = TRUE)
  expect_match(api$urls[2], "op=category-search&category=45&name=thriller", fixed = TRUE)
  expect_match(api$urls[3], "op=barcode-prefix-search&prefix=4007249146", fixed = TRUE)
})

test_that("empty results and errors give an empty data frame", {
  local_fake_api('{"page":0,"moreproducts":false,"totalproducts":0,"productlist":[]}')
  res <- product_search("nothing", token = "t")
  expect_equal(nrow(res), 0L)
  expect_true("ean" %in% names(res))
  expect_false(attr(res, "more_products"))

  local_fake_api('[{"error":"Invalid token"}]')
  expect_equal(nrow(product_search("x", token = "t")), 0L)
  expect_equal(eansearch_error(), "Invalid token")

  local_fake_api('{"error":"Missing parameter"}')
  expect_equal(nrow(barcode_prefix_search("1", token = "t")), 0L)
  expect_equal(eansearch_error(), "Missing parameter")
})
