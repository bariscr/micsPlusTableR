test_that("the async loader validates and pins both requests to one commit", {
  requests <- list(); result <- failure <- NULL; cancelled <- FALSE
  local_mocked_bindings(survey_catalogue_request = function(url, stage, on_success, on_error, on_progress) {
    requests[[length(requests) + 1L]] <<- list(url = url, success = on_success, error = on_error)
    function() cancelled <<- TRUE
  })
  stop_load <- survey_catalogue_load(function(x) result <<- x, function(message, records) failure <<- message)
  expect_length(requests, 1)
  sha <- strrep("c", 40)
  record <- data.frame(stage = "Latest revision", attempt = 1, seconds = 0.1, result = "HTTP 200")
  requests[[1]]$success(list(sha = sha), record)
  expect_length(requests, 2)
  expect_match(requests[[2]]$url, paste0("/git/trees/", sha, "?recursive=1"), fixed = TRUE)
  requests[[2]]$success(list(truncated = FALSE, tree = list(list(type = "blob", mode = "100644",
    path = "JAM_W1/plan.xlsx", size = 3))), transform(record, stage = "File list"))
  expect_null(failure); expect_identical(attr(result, "commit"), sha)
  expect_equal(nrow(attr(result, "catalogue_diagnostics")), 2)
  expect_match(result$url, paste0("/", sha, "/"), fixed = TRUE)
  result <- NULL
  stop_load <- survey_catalogue_load(function(x) result <<- x, function(...) NULL)
  stop_load(); expect_true(cancelled)
  requests[[3]]$success(list(sha = sha), record)
  expect_length(requests, 3); expect_null(result)
})

test_that("invalid revisions and incomplete or empty catalogues fail explicitly", {
  requests <- list(); failure <- NULL
  local_mocked_bindings(survey_catalogue_request = function(url, stage, on_success, on_error, on_progress) {
    requests[[length(requests) + 1L]] <<- list(success = on_success, error = on_error)
    function() NULL
  })
  survey_catalogue_load(function(...) stop("unexpected success"), function(message, records) failure <<- message)
  requests[[1]]$success(list(sha = "invalid"), NULL)
  expect_match(failure, "valid commit"); expect_length(requests, 1)
  for(tree in list(list(truncated = TRUE, tree = list()), list(truncated = FALSE, tree = list()))) {
    requests <- list(); failure <- NULL
    survey_catalogue_load(function(...) stop("unexpected success"), function(message, records) failure <<- message,
      ref = strrep("a", 40))
    requests[[1]]$success(tree, NULL)
    expect_match(failure, "incomplete|No plans")
  }
})

catalogue_transport_mocks <- function() {
  state <- new.env(parent = emptyenv())
  state$callbacks <- state$timers <- list(); state$options <- NULL
  state$cancelled <- FALSE; state$timeouts <- numeric()
  testthat::local_mocked_bindings(
    new_pool = function() NULL,
    new_handle = function(...) { state$options <- list(...); new.env() },
    handle_setheaders = function(...) NULL,
    multi_add = function(handle, pool, done, fail) {
      state$callbacks[[length(state$callbacks) + 1L]] <- list(done = done, fail = fail)
      invisible(handle)
    },
    multi_run = function(timeout, pool) { state$timeouts <- c(state$timeouts, timeout); list(pending = 1) },
    multi_cancel = function(handle) state$cancelled <- TRUE,
    .package = "curl", .env = parent.frame())
  testthat::local_mocked_bindings(later = function(func, delay) {
    id <- length(state$timers) + 1L; state$timers[[id]] <- list(callback = func, delay = delay)
    function() invisible(TRUE)
  }, .package = "later", .env = parent.frame())
  state
}

test_that("catalogue requests poll without waiting and cancel pending work", {
  state <- catalogue_transport_mocks(); result <- NULL
  cancel <- survey_catalogue_request("https://example.invalid", "File list",
    function(value, records) result <<- value, function(...) stop("unexpected failure"))
  expect_equal(state$options$timeout, 15); expect_equal(state$options$connecttimeout, 5)
  state$timers[[1]]$callback(); expect_identical(state$timeouts, 0)
  cancel(); expect_true(state$cancelled)
  state$callbacks[[1]]$done(list(status_code = 200L, content = charToRaw('{"ok":true}')))
  expect_null(result)
})

test_that("temporary HTTP failures retry once and record both attempts", {
  state <- catalogue_transport_mocks(); result <- records <- failure <- NULL
  cancel <- survey_catalogue_request("https://example.invalid", "File list",
    function(value, diagnostics) { result <<- value; records <<- diagnostics },
    function(message, diagnostics) failure <<- message)
  state$callbacks[[1]]$done(list(status_code = 503L, content = charToRaw("unavailable")))
  retry <- which(vapply(state$timers, function(t) identical(t$delay, 0.5), logical(1)))
  expect_length(retry, 1); state$timers[[retry]]$callback()
  expect_length(state$callbacks, 2)
  state$callbacks[[2]]$done(list(status_code = 200L, content = charToRaw('{"ok":true}')))
  expect_true(result$ok); expect_null(failure)
  expect_equal(records$attempt, 1:2)
  expect_identical(records$result, c("HTTP 503", "HTTP 200"))
})

test_that("rate limits, missing repositories and invalid JSON do not retry", {
  for(response in list(list(status_code = 403L, content = charToRaw('{"message":"API rate limit exceeded"}')),
    list(status_code = 404L, content = charToRaw("not found")),
    list(status_code = 200L, content = charToRaw("invalid JSON")))) {
    state <- catalogue_transport_mocks(); failure <- NULL
    cancel <- survey_catalogue_request("https://example.invalid", "File list", function(...) stop("unexpected success"),
      function(message, diagnostics) failure <<- message)
    state$callbacks[[1]]$done(response)
    expect_match(failure, "request limit|not be found|unreadable")
    expect_length(state$callbacks, 1)
    expect_false(any(vapply(state$timers, function(t) identical(t$delay, 0.5), logical(1))))
  }
})

test_that("connection failures have at most one retry", {
  state <- catalogue_transport_mocks(); failure <- records <- NULL
  cancel <- survey_catalogue_request("https://example.invalid", "File list", function(...) stop("unexpected success"),
    function(message, diagnostics) { failure <<- message; records <<- diagnostics })
  state$callbacks[[1]]$fail("Connection timed out")
  retry <- which(vapply(state$timers, function(t) identical(t$delay, 0.5), logical(1)))
  state$timers[[retry]]$callback(); state$callbacks[[2]]$fail("Connection timed out")
  expect_match(failure, "Check GitHub access")
  expect_equal(nrow(records), 2); expect_length(state$callbacks, 2)
})
