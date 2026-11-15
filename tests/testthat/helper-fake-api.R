# Replace the HTTP layer, so the tests run offline.
# `responses` is a list of list(status, body); the last one is repeated.
# Returns an environment recording the requested URLs.
local_fake_api <- function(body = "[]", status = 200L, credits = "42",
                           responses = list(list(status = status, body = body)),
                           env = parent.frame()) {
  log <- new.env()
  log$urls <- character()
  log$pauses <- 0L
  local_mocked_bindings(
    http_get = function(url) {
      log$urls <- c(log$urls, url)
      res <- responses[[min(length(log$urls), length(responses))]]
      list(status = res$status, credits = credits, body = res$body)
    },
    pause = function(seconds) log$pauses <- log$pauses + 1L,
    .package = "eansearch",
    .env = env
  )
  .state <- get(".state", envir = asNamespace("eansearch"))
  .state$remaining <- NA_integer_
  .state$error <- ""
  log
}
