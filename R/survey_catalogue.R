# Share only successful public catalogues between tabs/sessions in this R process.
survey_catalogue_cache <- new.env(parent = emptyenv())

survey_catalogue_cached <- function(repo = "bariscr/micsPlusTableR-Files", ref = "main") {
  get0(paste(repo, ref, sep = "@"), envir = survey_catalogue_cache, inherits = FALSE)
}

survey_catalogue_remember <- function(data, repo = "bariscr/micsPlusTableR-Files", ref = "main") {
  assign(paste(repo, ref, sep = "@"), list(data = data, time = Sys.time()),
    envir = survey_catalogue_cache)
  invisible(data)
}

survey_catalogue_diagnostics <- function(records) {
  if (is.null(records) || !nrow(records)) return(NULL)
  paste(sprintf("%s, attempt %d: %.2f s, %s", records$stage, records$attempt,
    records$seconds, records$result), collapse = "\n")
}

survey_catalogue_http_error <- function(status, body) {
  if (status == 429L || (status == 403L && grepl("rate limit", body, ignore.case = TRUE))) {
    return("GitHub's request limit was reached. Wait before refreshing, or use the ZIP download link.")
  }
  if (status == 404L) return("The published survey files could not be found on GitHub.")
  if (status == 403L) return("GitHub refused the survey-list request. Check whether GitHub access is restricted on this network.")
  sprintf("GitHub could not supply the survey list (HTTP %d). Please try again shortly.", status)
}

# Poll libcurl without waiting, allowing Shiny to process UI events between polls.
# Each request has one bounded retry for temporary connection/server failures.
survey_catalogue_request <- function(url, stage, on_success, on_error,
                                      on_progress = function(message) NULL,
                                      timeout = 15, retry_delay = 0.5) {
  pool <- curl::new_pool()
  handle <- NULL
  poll_timer <- retry_timer <- NULL
  finished <- active <- FALSE
  records <- NULL

  cancel <- function() {
    finished <<- TRUE
    if (!is.null(poll_timer)) poll_timer()
    if (!is.null(retry_timer)) retry_timer()
    if (active && !is.null(handle)) curl::multi_cancel(handle)
    active <<- FALSE
    invisible(NULL)
  }
  fail <- function(message) {
    if (finished) return(invisible(NULL))
    cancel()
    on_error(message, records)
  }
  ensure_poll <- function() {
    if (!finished && active && is.null(poll_timer)) {
      poll_timer <<- later::later(poll, 0.05)
    }
  }
  poll <- function() {
    poll_timer <<- NULL
    if (finished) return(invisible(NULL))
    tryCatch(curl::multi_run(timeout = 0, pool = pool),
      error = function(e) fail(paste("Could not load the survey list:", conditionMessage(e))))
    ensure_poll()
  }
  start <- function(attempt = 1L) {
    retry_timer <<- NULL
    if (finished) return(invisible(NULL))
    started <- Sys.time()
    record <- function(result) {
      records <<- rbind(records, data.frame(stage = stage, attempt = attempt,
        seconds = as.numeric(difftime(Sys.time(), started, units = "secs")), result = result))
    }
    retry_or_fail <- function(message, transient) {
      if (finished) return(invisible(NULL))
      active <<- FALSE
      if (transient && attempt == 1L) {
        on_progress("The survey-list request was interrupted. Retrying once…")
        retry_timer <<- later::later(function() start(2L), retry_delay)
      } else fail(message)
    }
    tryCatch({
      handle <<- curl::new_handle(url = url, connecttimeout = min(5, timeout),
        timeout = timeout, useragent = "micsPlusTableR survey catalogue")
      curl::handle_setheaders(handle, Accept = "application/vnd.github+json")
      active <<- TRUE
      curl::multi_add(handle, pool = pool, done = function(response) {
        if (finished) return(invisible(NULL))
        active <<- FALSE
        status <- response$status_code
        record(paste("HTTP", status))
        body <- rawToChar(response$content)
        if (status < 200L || status >= 300L) {
          retry_or_fail(survey_catalogue_http_error(status, body),
            status %in% c(408L, 425L, 500L, 502L, 503L, 504L))
          return(invisible(NULL))
        }
        value <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) e)
        if (inherits(value, "error")) {
          fail("GitHub returned an unreadable survey list. Please try refreshing again.")
          return(invisible(NULL))
        }
        cancel()
        on_success(value, records)
      }, fail = function(message) {
        if (finished) return(invisible(NULL))
        record(message)
        transient <- grepl("timed? ?out|resolv|connect|recv|send|reset|temporar|empty reply|HTTP/2",
          message, ignore.case = TRUE)
        retry_or_fail(paste("Could not load surveys. Check GitHub access and try again.", message), transient)
      })
      ensure_poll()
    }, error = function(e) fail(paste("Could not load surveys:", conditionMessage(e))))
  }
  start()
  cancel
}

survey_catalogue_load <- function(on_success, on_error,
                                  on_progress = function(message) NULL,
                                  repo = "bariscr/micsPlusTableR-Files", ref = "main") {
  finished <- FALSE
  cancel_request <- function() NULL
  records <- NULL
  cancel <- function() {
    finished <<- TRUE
    cancel_request()
    invisible(NULL)
  }
  fail <- function(message, diagnostics = NULL) {
    if (finished) return(invisible(NULL))
    records <<- rbind(records, diagnostics)
    finished <<- TRUE
    on_error(message, records)
  }
  load_tree <- function(sha) {
    if (finished) return(invisible(NULL))
    on_progress("Loading the available survey folders…")
    cancel_request <<- survey_catalogue_request(
      paste0("https://api.github.com/repos/", repo, "/git/trees/", sha, "?recursive=1"),
      stage = "File list", on_progress = on_progress, on_error = fail,
      on_success = function(tree, diagnostics) {
        if (finished) return(invisible(NULL))
        records <<- rbind(records, diagnostics)
        data <- tryCatch(survey_files_catalogue(tree, sha, repo), error = function(e) e)
        if (inherits(data, "error")) return(fail(conditionMessage(data)))
        if (!nrow(data)) return(fail("No plans or preparation files are published yet."))
        attr(data, "catalogue_diagnostics") <- records
        finished <<- TRUE
        on_success(data)
      })
  }
  if (grepl("^[a-fA-F0-9]{40}$", ref)) {
    load_tree(ref)
  } else {
    on_progress("Checking for updated survey files…")
    cancel_request <- survey_catalogue_request(
      paste0("https://api.github.com/repos/", repo, "/commits/", utils::URLencode(ref, reserved = TRUE)),
      stage = "Latest revision", on_progress = on_progress, on_error = fail,
      on_success = function(commit, diagnostics) {
        if (finished) return(invisible(NULL))
        records <<- rbind(records, diagnostics)
        sha <- tryCatch(survey_files_commit_sha(commit), error = function(e) e)
        if (inherits(sha, "error")) return(fail(conditionMessage(sha)))
        load_tree(sha)
      })
  }
  cancel
}
