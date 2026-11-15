#' Search products
#'
#' Search the database by product name, by category and name, or by barcode
#' prefix. Each call returns one page of results.
#'
#' * `product_search()` finds products whose name contains all keywords.
#' * `similar_product_search()` also finds products with similar names.
#' * `category_search()` restricts the search to one product category.
#' * `barcode_prefix_search()` lists all EANs starting with a prefix.
#'
#' @param name Product name or keywords.
#' @param category Numeric category ID, see the API documentation for a list.
#' @param prefix Leading digits of the EANs to list.
#' @param page Page number of the results, starting with 0.
#' @inheritParams barcode_lookup
#' @return A data frame with the columns `ean`, `name`, `categoryId`,
#'   `categoryName` and `issuingCountry`. It has zero rows if nothing was found
#'   or an error occurred (see [eansearch_error()]). The attributes `page`,
#'   `more_products` and `total_products` describe the result pages, if the
#'   API reports them.
#' @export
#' @examplesIf nzchar(Sys.getenv("EAN_SEARCH_API_TOKEN"))
#' products <- product_search("iPod")
#' products[, c("ean", "name")]
#' attr(products, "more_products")
#'
#' similar_product_search("iPod with extra features", lang = ean_languages[["any"]])
#' category_search(45, "thriller")
#' barcode_prefix_search("4007249146")
product_search <- function(name, page = 0, lang = 1, token = eansearch_token()) {
  body <- api_call("product-search", name = as_code(name), page = page,
                   language = lang, token = token)
  product_list(body)
}

#' @rdname product_search
#' @export
similar_product_search <- function(name, page = 0, lang = 1,
                                   token = eansearch_token()) {
  body <- api_call("similar-product-search", name = as_code(name), page = page,
                   language = lang, token = token)
  product_list(body)
}

#' @rdname product_search
#' @export
category_search <- function(category, name, page = 0, lang = 1,
                            token = eansearch_token()) {
  body <- api_call("category-search", category = as_code(category),
                   name = as_code(name), page = page, language = lang,
                   token = token)
  product_list(body)
}

#' @rdname product_search
#' @export
barcode_prefix_search <- function(prefix, page = 0, lang = 1,
                                  token = eansearch_token()) {
  body <- api_call("barcode-prefix-search", prefix = as_code(prefix),
                   page = page, language = lang, token = token)
  product_list(body)
}
