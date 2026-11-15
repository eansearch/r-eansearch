## Submission

This is a new submission.

The package is a client for the 'EAN-Search.org' web service
<https://www.ean-search.org/>, which requires a (paid) API token.

* Examples use `@examplesIf nzchar(Sys.getenv("EAN_SEARCH_API_TOKEN"))`, so they
  only run when a token is configured.
* Tests replace the HTTP layer with a mock and run fully offline.
* Network and API errors never raise R errors: functions return `NA`, `NULL`
  or an empty data frame and the reason is available via `eansearch_error()`,
  so the package fails gracefully if the service is unavailable.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
