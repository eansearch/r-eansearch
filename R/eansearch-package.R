#' eansearch: EAN, UPC and ISBN Lookup via the EAN-Search.org API
#'
#' Look up and search the product database on <https://www.ean-search.org/>.
#'
#' @section API token:
#' All API functions need an access token, which you can get at
#' <https://www.ean-search.org/ean-database-api.html>. Set it in the
#' environment variable `EAN_SEARCH_API_TOKEN` (eg. in `.Renviron`) or pass
#' it to each function as `token`.
#'
#' @section Errors:
#' Network and API errors do not raise R errors: lookups return `NA` or
#' `NULL`, searches return an empty data frame, and [eansearch_error()] tells
#' what went wrong. Requests rejected with HTTP status 429 (too many requests)
#' are retried up to 3 times.
#'
#' @section Options:
#' * `eansearch.timeout`: HTTP timeout in seconds (default 30).
#' * `eansearch.useragent`: HTTP user agent (default `r-eansearch/<version>`).
#'
#' @keywords internal
"_PACKAGE"
