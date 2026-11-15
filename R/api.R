# Internal helpers for talking to the EAN-Search.org API

api_url <- "https://api.ean-search.org/api"
max_api_tries <- 3L

# per-session state: last error message and remaining API credits
.state <- new.env(parent = emptyenv())
.state$error <- ""
.state$remaining <- NA_integer_

default_user_agent <- function() {
  paste0("r-eansearch/", utils::packageVersion("eansearch"))
}

#' Default API token
#'
#' Returns the API token stored in the environment variable
#' `EAN_SEARCH_API_TOKEN`. All API functions use this as the default for
#' their `token` argument, so the easiest way to configure the package is to
#' put `EAN_SEARCH_API_TOKEN=<your token>` into your `.Renviron` file.
#'
#' You can get an API token at
#' <https://www.ean-search.org/ean-database-api.html>.
#'
#' @return The API token as a character string. Signals an error if the
#'   environment variable is not set.
#' @export
#' @examples
#' # TRUE if a token is configured
#' nzchar(Sys.getenv("EAN_SEARCH_API_TOKEN"))
eansearch_token <- function() {
  token <- Sys.getenv("EAN_SEARCH_API_TOKEN")
  if (!nzchar(token)) {
    stop("No API token: set the environment variable EAN_SEARCH_API_TOKEN ",
         "or pass `token`, see https://www.ean-search.org/ean-database-api.html",
         call. = FALSE)
  }
  token
}

#' Error message of the last API call
#'
#' Lookup functions return `NA` or `NULL` (and search functions an empty data
#' frame) when the API call fails or nothing is found. Use `eansearch_error()`
#' to find out why.
#'
#' @return The error message of the last API call, or `""` if it succeeded.
#' @export
#' @examplesIf nzchar(Sys.getenv("EAN_SEARCH_API_TOKEN"))
#' name <- barcode_lookup("5099750442228")
#' if (is.na(name)) message("Lookup failed: ", eansearch_error())
eansearch_error <- function() {
  .state$error
}

# Build the query string, URL-encoding every value
build_url <- function(params) {
  values <- vapply(params, function(v) curl::curl_escape(enc2utf8(as.character(v))), "")
  paste0(api_url, "?", paste(names(params), values, sep = "=", collapse = "&"))
}

# Perform one HTTP GET; separate function so tests can replace it.
# Returns list(status = <int>, credits = <chr or NULL>, body = <chr>)
http_get <- function(url) {
  handle <- curl::new_handle(
    useragent = getOption("eansearch.useragent", default_user_agent()),
    timeout = getOption("eansearch.timeout", 30)
  )
  res <- curl::curl_fetch_memory(url, handle = handle)
  headers <- curl::parse_headers_list(res$headers)
  list(
    status = res$status_code,
    credits = headers[["x-credits-remaining"]],
    body = rawToChar(res$content)
  )
}

# wrapper so tests don't have to wait
pause <- function(seconds) Sys.sleep(seconds)

set_error <- function(msg) {
  .state$error <- msg
  NULL
}

set_remaining <- function(value) {
  if (is.null(value)) return(invisible())
  n <- suppressWarnings(as.integer(value))
  if (!is.na(n)) .state$remaining <- n
  invisible()
}

# Call an API operation, returns the response body or NULL on error
api_call <- function(op, ..., token) {
  .state$error <- ""
  url <- build_url(c(list(token = token, format = "json", op = op), list(...)))
  for (tries in seq_len(max_api_tries)) {
    res <- tryCatch(http_get(url), error = function(e) e)
    if (inherits(res, "error")) {
      msg <- conditionMessage(res)
      if (grepl("timeout|timed out", msg, ignore.case = TRUE)) {
        return(set_error("Timeout"))
      }
      return(set_error(paste("Connection error", msg)))
    }
    set_remaining(res$credits)
    if (res$status == 429L && tries < max_api_tries) {
      pause(1)
      next
    }
    if (res$status != 200L) {
      return(set_error(paste("Server error", res$status)))
    }
    return(res$body)
  }
}

parse_json <- function(body, simplify) {
  if (is.null(body)) return(NULL)
  data <- tryCatch(jsonlite::fromJSON(body, simplifyVector = simplify),
                   error = function(e) NULL)
  if (is.null(data)) set_error("Invalid response from API")
  data
}

# First result object of a lookup as a named list, or NULL
first_result <- function(body) {
  data <- parse_json(body, simplify = FALSE)
  if (!is.list(data) || length(data) == 0L || !is.list(data[[1L]]) ||
      is.null(names(data[[1L]]))) {
    return(NULL)
  }
  first <- data[[1L]]
  if (!is.null(first$error)) return(set_error(as.character(first$error)))
  first
}

# A single field of the first result, or NA
first_field <- function(body, field, na = NA_character_) {
  value <- first_result(body)[[field]]
  if (is.null(value)) na else value
}

# case-insensitive field access, the API uses lower case names
get_field <- function(x, name) {
  i <- match(tolower(name), tolower(names(x)))
  if (is.na(i)) NULL else x[[i]]
}

empty_products <- function() {
  data.frame(ean = character(), name = character(), categoryId = integer(),
             categoryName = character(), issuingCountry = character(),
             stringsAsFactors = FALSE)
}

# convert numeric category IDs from the strings the API sends
fix_types <- function(x) {
  for (col in intersect(c("categoryId", "googleCategoryId"), names(x))) {
    x[[col]] <- suppressWarnings(as.integer(x[[col]]))
  }
  if (!is.null(x[["ean"]])) x[["ean"]] <- as.character(x[["ean"]])
  x
}

# Product list of a search as a data frame (empty on error)
product_list <- function(body) {
  data <- parse_json(body, simplify = TRUE)
  products <- empty_products()
  if (is.data.frame(data) && "error" %in% names(data)) {
    set_error(as.character(data$error[1L]))
  } else if (is.list(data) && !is.data.frame(data)) {
    if (!is.null(data$error)) {
      set_error(as.character(data$error))
    } else {
      plist <- get_field(data, "productlist")
      if (is.data.frame(plist) && nrow(plist) > 0L) {
        products <- fix_types(plist)
      }
      attr(products, "page") <- get_field(data, "page")
      attr(products, "more_products") <- get_field(data, "moreproducts")
      attr(products, "total_products") <- get_field(data, "totalproducts")
    }
  }
  products
}

# Validate a single barcode / ID argument and return it as a string
as_code <- function(x, arg = deparse(substitute(x))) {
  if (length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be a single, non-missing value", call. = FALSE)
  }
  if (is.numeric(x)) {
    x <- formatC(x, format = "f", digits = 0, big.mark = "")
  }
  trimws(as.character(x))
}
