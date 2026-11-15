# eansearch

An R package for EAN, UPC and ISBN lookup and validation using the API on https://www.ean-search.org

You can

- look up EAN barcodes and ISBNs (ISBN-10 or ISBN-13)
- search for products by name or keyword, optionally within a category
- list all EANs with a certain prefix
- map EANs to Amazon ASINs and Library of Congress Control Numbers (LCCN) and back
- verify if an EAN is valid
- look up the country where an EAN was issued
- generate PNG barcode images

## Installation

From CRAN:

```r
install.packages("eansearch")
```

Development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("eansearch/r-eansearch")
```

## API token

You need an API access token from https://www.ean-search.org/ean-database-api.html

Put it into your `~/.Renviron` file, then all functions use it automatically:

```
EAN_SEARCH_API_TOKEN=your-token
```

Alternatively, pass it to each function with the `token` argument.

## Usage

```r
library(eansearch)

ean <- "5099750442227" # Thriller

name <- barcode_lookup(ean)
cat(ean, "is", name, "\n")

# more detailed result, preferably in English
product <- barcode_search(ean, lang = ean_languages[["english"]])
cat(ean, "is", product$name, "from category", product$categoryName,
    "(Google ID", product$googleCategoryId, ") issued in", product$issuingCountry, "\n")

# lookups return NA if not found or on error
name <- barcode_lookup("5099750442228")
if (is.na(name)) message("Lookup failed: ", eansearch_error())

# look up several EANs (one API call each)
vapply(c("5099750442227", "0722868396643"), barcode_lookup, "")

# ISBN-10 or ISBN-13
title <- isbn_lookup("1119578884")

# verify the checksum
verify_checksum("5099750442227")

# search for product name "iPod", first page of results, English names
products <- product_search("iPod")
products[, c("ean", "name")]
attr(products, "more_products") # is there a next page?
product_search("iPod", page = 1)

# search for similar product names in any language
similar_product_search("iPod with extra features", lang = ean_languages[["any"]])

# search a category
category_search(45, "thriller")

# all EANs starting with a prefix
barcode_prefix_search("4007249146")

# EAN <-> ASIN
find_ean_for_asin("B00000J1V5")
find_asin_for_ean("0722868396643")

# ISBN-13 / EAN <-> LCCN
find_lccn_for_ean("9780815346333")
find_ean_for_lccn("2020691629")

# issuing country
issuing_country_lookup("5099750442227")

# barcode image as PNG
png <- barcode_image("5099750442227", width = 300, height = 200)
writeBin(png, "barcode.png")

# or base64 encoded for HTML
b64 <- barcode_image("5099750442227", base64 = TRUE)
cat('<img src="data:image/png;base64,', b64, '">', sep = "")

credits_remaining()
```

## Error handling

Network and API errors never stop your script: lookups return `NA` (or `NULL`
for `barcode_search()` and `barcode_image()`), searches return an empty data
frame, and `eansearch_error()` returns the reason. Requests rejected with HTTP
status 429 (too many requests) are retried automatically.

## Options

```r
options(eansearch.timeout = 60)          # HTTP timeout in seconds, default 30
options(eansearch.useragent = "my-app")  # HTTP user agent
```

## License

MIT, see [LICENSE.md](LICENSE.md).
