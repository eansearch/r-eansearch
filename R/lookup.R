#' Language codes
#'
#' Named vector of the language codes accepted by the `lang` argument of the
#' lookup and search functions. The API returns the product name in the
#' preferred language if available. Use `ean_languages[["any"]]` (99) to search
#' in all languages.
#'
#' @format A named integer vector.
#' @export
#' @examples
#' ean_languages[["german"]]
ean_languages <- c(
  english = 1L, danish = 2L, german = 3L, spanish = 4L, finnish = 5L,
  french = 6L, hungarian = 7L, italian = 8L, japanese = 9L, dutch = 10L,
  norwegian = 11L, polish = 12L, portuguese = 13L, swedish = 15L, czech = 16L,
  croatian = 18L, romanian = 19L, bulgarian = 20L, greek = 21L, any = 99L
)

#' Look up an EAN barcode
#'
#' `barcode_lookup()` returns the product name for an EAN, UPC or ISBN-13
#' barcode, `barcode_search()` returns all product details.
#'
#' @param ean EAN barcode as a string (preferred) or number.
#' @param lang Preferred language for the product name, see [ean_languages].
#' @param token API token, defaults to [eansearch_token()].
#' @return `barcode_lookup()`: the product name, or `NA` if the barcode is not
#'   found or an error occurs (see [eansearch_error()]).
#'
#'   `barcode_search()`: a list with the elements `ean`, `name`, `categoryId`,
#'   `categoryName`, `googleCategoryId` and `issuingCountry`, or `NULL`.
#' @export
#' @examplesIf nzchar(Sys.getenv("EAN_SEARCH_API_TOKEN"))
#' barcode_lookup("5099750442227")
#'
#' product <- barcode_search("5099750442227", lang = ean_languages[["english"]])
#' product$categoryName
#'
#' # look up several barcodes (one API call each)
#' vapply(c("5099750442227", "0722868396643"), barcode_lookup, "")
barcode_lookup <- function(ean, lang = 1, token = eansearch_token()) {
  product <- barcode_search(ean, lang = lang, token = token)
  if (is.null(product$name)) NA_character_ else product$name
}

#' @rdname barcode_lookup
#' @export
barcode_search <- function(ean, lang = 1, token = eansearch_token()) {
  body <- api_call("barcode-lookup", ean = as_code(ean), language = lang,
                   token = token)
  product <- first_result(body)
  if (is.null(product)) NULL else fix_types(product)
}

#' Look up an ISBN
#'
#' Returns the book title for an ISBN-10 or ISBN-13. Hyphens and spaces are
#' removed before the lookup.
#'
#' @param isbn ISBN-10 or ISBN-13 as a string.
#' @inheritParams barcode_lookup
#' @return The book title, or `NA` if not found (see [eansearch_error()]).
#' @export
#' @examplesIf nzchar(Sys.getenv("EAN_SEARCH_API_TOKEN"))
#' isbn_lookup("1119578884")
isbn_lookup <- function(isbn, lang = 1, token = eansearch_token()) {
  isbn <- gsub("[^0-9X]", "", toupper(as_code(isbn)))
  if (nchar(isbn) == 13L) {
    return(barcode_lookup(isbn, lang = lang, token = token))
  }
  body <- api_call("barcode-lookup", isbn = isbn, language = lang,
                   token = token)
  first_field(body, "name")
}

#' Map between EANs and ASINs or LCCNs
#'
#' Find the EAN for an Amazon Standard Identification Number (ASIN) or a
#' Library of Congress Control Number (LCCN), and vice versa.
#'
#' There can be multiple EANs for one LCCN, `find_ean_for_lccn()` returns the
#' first one.
#'
#' @param asin Amazon Standard Identification Number.
#' @param lccn Library of Congress Control Number.
#' @inheritParams barcode_lookup
#' @return The requested ID as a string, or `NA` if not found (see
#'   [eansearch_error()]).
#' @export
#' @examplesIf nzchar(Sys.getenv("EAN_SEARCH_API_TOKEN"))
#' find_ean_for_asin("B00000J1V5")
#' find_asin_for_ean("0722868396643")
#' find_lccn_for_ean("9780815346333")
#' find_ean_for_lccn("2020691629")
find_ean_for_asin <- function(asin, token = eansearch_token()) {
  body <- api_call("ean-for-asin-lookup", asin = as_code(asin), token = token)
  first_field(body, "ean")
}

#' @rdname find_ean_for_asin
#' @export
find_asin_for_ean <- function(ean, token = eansearch_token()) {
  body <- api_call("asin-for-ean-lookup", ean = as_code(ean), token = token)
  first_field(body, "asin")
}

#' @rdname find_ean_for_asin
#' @export
find_ean_for_lccn <- function(lccn, token = eansearch_token()) {
  body <- api_call("ean-for-lccn-lookup", lccn = as_code(lccn), token = token)
  first_field(body, "ean")
}

#' @rdname find_ean_for_asin
#' @export
find_lccn_for_ean <- function(ean, token = eansearch_token()) {
  body <- api_call("lccn-for-ean-lookup", ean = as_code(ean), token = token)
  first_field(body, "lccn")
}

#' Verify the checksum of an EAN barcode
#'
#' @inheritParams barcode_lookup
#' @return `TRUE` if the checksum is valid, `FALSE` if not, or `NA` on error
#'   (see [eansearch_error()]).
#' @export
#' @examplesIf nzchar(Sys.getenv("EAN_SEARCH_API_TOKEN"))
#' verify_checksum("5099750442227")
#' verify_checksum("5099750442228")
verify_checksum <- function(ean, token = eansearch_token()) {
  body <- api_call("verify-checksum", ean = as_code(ean), token = token)
  valid <- first_field(body, "valid", na = NA)
  if (is.na(valid)) NA else tolower(as.character(valid)) %in% c("1", "true")
}

#' Issuing country of an EAN barcode
#'
#' Returns the country where the barcode was issued. This works even if the
#' product is not in the database.
#'
#' @inheritParams barcode_lookup
#' @return Two-letter country code, or `NA` on error (see [eansearch_error()]).
#' @export
#' @examplesIf nzchar(Sys.getenv("EAN_SEARCH_API_TOKEN"))
#' issuing_country_lookup("5099750442227")
issuing_country_lookup <- function(ean, token = eansearch_token()) {
  body <- api_call("issuing-country", ean = as_code(ean), token = token)
  first_field(body, "issuingCountry")
}

#' PNG image of an EAN barcode
#'
#' @inheritParams barcode_lookup
#' @param width,height Image size in pixels.
#' @param base64 Return the image base64 encoded (eg. for an HTML data URI)
#'   instead of as raw bytes.
#' @return The PNG image as a raw vector (or a base64 string), or `NULL` on
#'   error (see [eansearch_error()]).
#' @export
#' @examplesIf nzchar(Sys.getenv("EAN_SEARCH_API_TOKEN"))
#' png <- barcode_image("5099750442227", width = 300, height = 200)
#' file <- tempfile(fileext = ".png")
#' writeBin(png, file)
#'
#' b64 <- barcode_image("5099750442227", base64 = TRUE)
#' html <- paste0('<img src="data:image/png;base64,', b64, '">')
barcode_image <- function(ean, width = 102, height = 50, base64 = FALSE,
                          token = eansearch_token()) {
  body <- api_call("barcode-image", ean = as_code(ean), width = width,
                   height = height, token = token)
  image <- first_result(body)$barcode
  if (is.null(image) || base64) image else jsonlite::base64_dec(image)
}

#' Remaining API credits
#'
#' Returns the number of API requests remaining this month. The value is
#' updated with every API call; if no call was made yet in this session, the
#' account status is queried.
#'
#' @inheritParams barcode_lookup
#' @return The number of remaining credits, or `NA` if unknown (see
#'   [eansearch_error()]).
#' @export
#' @examplesIf nzchar(Sys.getenv("EAN_SEARCH_API_TOKEN"))
#' credits_remaining()
credits_remaining <- function(token = eansearch_token()) {
  if (is.na(.state$remaining)) {
    api_call("account-status", token = token)
  }
  .state$remaining
}
