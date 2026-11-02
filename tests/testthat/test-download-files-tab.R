download_tab_fixture <- function() {
  x <- data.frame(survey = c("Jamaica (2023-24) Wave 1", "Jamaica (2023-24) Wave 1", "Mongolia (2025-26) Wave 2"),
    path = c("Jamaica (2023-24) Wave 1/plan.xlsx", "Jamaica (2023-24) Wave 1/prep-files-Jamaica (2023-24) Wave 1/prep.R", "Mongolia (2025-26) Wave 2/plan.xlsx"),
    size = c(100, 200, 400), url = "https://example.invalid")
  attr(x, "commit") <- strrep("a", 40)
  x
}

test_that("opening the tab loads once without blocking or duplicate requests", {
  requests <- list(); cancelled <- FALSE
  visible <- shiny::reactiveVal(FALSE)
  local_mocked_bindings(survey_catalogue_cache = new.env(parent = emptyenv()),
    survey_catalogue_load = function(on_success, on_error, on_progress) {
      requests[[length(requests) + 1L]] <<- list(success = on_success, error = on_error)
      on_progress("Loading survey folders…")
      function() cancelled <<- TRUE
    })
  shiny::testServer(download_files_server,
    args = list(project_dir = tempdir(), active = shiny::reactive(visible())), {
      session$flushReact(); expect_length(requests, 0)
      visible(TRUE); session$flushReact()
      expect_length(requests, 1)
      expect_true(loading()); expect_null(catalogue())
      session$setInputs(destination = "another-folder", refresh = 1)
      expect_identical(input$destination, "another-folder")
      expect_length(requests, 1)
      requests[[1]]$success(download_tab_fixture()); session$flushReact()
      expect_false(loading()); expect_equal(nrow(catalogue()), 3)
      visible(FALSE); session$flushReact(); visible(TRUE); session$flushReact()
      expect_length(requests, 1)
      session$setInputs(refresh = 2)
      expect_length(requests, 2); expect_true(loading())
      requests[[2]]$error("GitHub is unavailable", data.frame(stage = "File list", attempt = 1,
        seconds = 2, result = "HTTP 503"))
      session$flushReact()
      expect_false(loading()); expect_equal(nrow(catalogue()), 3)
      expect_equal(notice()$kind, "warning")
      expect_match(notice()$message, "previously loaded list")
      expect_match(notice()$details, "File list, attempt 1: 2.00 s, HTTP 503", fixed = TRUE)
      session$setInputs(refresh = 3); session$close()
      expect_true(cancelled)
      requests[[3]]$success(download_tab_fixture())
    })
})

test_that("fresh caches load instantly and stale caches remain visible during refresh", {
  fixture <- download_tab_fixture(); requested <- 0L; failure <- NULL
  cache <- new.env(parent = emptyenv())
  local_mocked_bindings(survey_catalogue_cache = cache,
    survey_catalogue_load = function(on_success, on_error, on_progress) {
      requested <<- requested + 1L; failure <<- on_error
      function() NULL
    })
  survey_catalogue_remember(fixture)
  shiny::testServer(download_files_server, args = list(project_dir = tempdir()), {
    session$flushReact()
    expect_identical(catalogue(), fixture); expect_identical(requested, 0L)
    expect_false(loading())
  })
  key <- ls(cache)[1]; cached <- cache[[key]]; cached$time <- cached$time - 600; cache[[key]] <- cached
  shiny::testServer(download_files_server, args = list(project_dir = tempdir()), {
    session$flushReact()
    expect_identical(catalogue(), fixture); expect_identical(requested, 1L)
    expect_true(loading())
    failure("Connection timed out", NULL); session$flushReact()
    expect_identical(catalogue(), fixture); expect_false(loading())
  })
})

test_that("dropdown selection and its all/none buttons preserve download pinning and paths", {
  fixture <- download_tab_fixture(); calls <- updates <- list(); success <- NULL
  project <- tempfile("project-"); dir.create(project)
  local_mocked_bindings(survey_catalogue_cache = new.env(parent = emptyenv()),
    survey_catalogue_load = function(on_success, on_error, on_progress) {
      success <<- on_success; function() NULL
    }, download_survey_files = function(surveys, dest_dir, overwrite, quiet, ref) {
      calls[[length(calls) + 1L]] <<- list(surveys = surveys, dest_dir = dest_dir, overwrite = overwrite, ref = ref)
      fixture[fixture$survey %in% surveys, ]
    })
  local_mocked_bindings(updateSelectizeInput = function(session, inputId, ...) {
    updates[[length(updates) + 1L]] <<- list(...)
  }, .package = "shiny")
  shiny::testServer(download_files_server, args = list(project_dir = project), {
    session$setInputs(destination = project, overwrite = FALSE, download = 1)
    expect_match(notice()$message, "Wait for the survey list")
    success(fixture); session$flushReact()
    session$setInputs(download = 2)
    expect_match(notice()$message, "Choose at least one survey")
    session$setInputs(surveys = "Jamaica (2023-24) Wave 1", download = 3)
    expect_identical(calls[[1]]$surveys, "Jamaica (2023-24) Wave 1")
    expect_identical(calls[[1]]$ref, attr(fixture, "commit"))
    expect_identical(calls[[1]]$dest_dir, normalizePath(project, winslash = "/"))
    expect_false(calls[[1]]$overwrite)
    session$setInputs(select_all = 1)
    expect_setequal(tail(updates, 1)[[1]]$selected, unique(fixture$survey))
    session$setInputs(surveys = unique(fixture$survey), destination = "selected-files", download = 4)
    expect_setequal(calls[[2]]$surveys, unique(fixture$survey))
    expect_identical(calls[[2]]$dest_dir, file.path(project, "selected-files"))
    session$setInputs(deselect_all = 1)
    expect_identical(tail(updates, 1)[[1]]$selected, character())
    session$setInputs(surveys = character(), download = 5)
    expect_match(notice()$message, "Choose at least one survey"); expect_length(calls, 2)
    session$setInputs(surveys = unique(fixture$survey), overwrite = TRUE, destination = "", download = 6)
    expect_match(notice()$message, "Choose a download folder")
  })
})

test_that("first-load errors release the refresh button and show actionable feedback", {
  failure <- NULL
  local_mocked_bindings(survey_catalogue_cache = new.env(parent = emptyenv()),
    survey_catalogue_load = function(on_success, on_error, on_progress) {
      failure <<- on_error; function() NULL
    })
  shiny::testServer(download_files_server, args = list(project_dir = tempdir()), {
    session$flushReact(); failure("GitHub's request limit was reached", NULL); session$flushReact()
    expect_null(catalogue()); expect_false(loading())
    expect_equal(notice()$kind, "danger"); expect_match(notice()$message, "request limit")
  })
})

test_that("legacy IDs stay internal and refresh preserves surviving selections", {
  fixture <- download_tab_fixture(); fixture$survey <- c("JAM_W1", "JAM_W1", "MNG_W2")
  success <- NULL; updates <- list()
  local_mocked_bindings(survey_catalogue_cache = new.env(parent = emptyenv()),
    survey_catalogue_load = function(on_success, on_error, on_progress) {
      success <<- on_success; function() NULL
    })
  local_mocked_bindings(updateSelectizeInput = function(session, inputId, ...) {
    updates[[length(updates) + 1L]] <<- list(...)
  }, .package = "shiny")
  shiny::testServer(download_files_server, args = list(project_dir = tempdir()), {
    session$flushReact(); success(fixture); session$flushReact()
    expect_identical(names(updates[[1]]$choices), c("Jamaica (2023-24) Wave 1", "Mongolia (2025-26) Wave 2"))
    expect_identical(unname(updates[[1]]$choices), c("JAM_W1", "MNG_W2"))
    session$setInputs(surveys = c("JAM_W1", "MNG_W2"), refresh = 1)
    smaller <- fixture[fixture$survey == "JAM_W1", ]; attr(smaller, "commit") <- strrep("b", 40)
    success(smaller); session$flushReact()
    expect_identical(tail(updates, 1)[[1]]$selected, "JAM_W1")
    expect_match(output$preview, "Jamaica (2023-24) Wave 1", fixed = TRUE)
    expect_false(grepl("JAM_W1", output$preview, fixed = TRUE))
  })
})

test_that("the UI matches multi-sheet selection and starts at the caller's project", {
  project <- tempfile("caller-project-"); dir.create(project)
  old <- options(micsPlusTableR.project_dir = project, micsPlusTableR.output_dir = tempfile("output-"))
  on.exit(options(old), add = TRUE)
  env <- new.env(); source(system.file("shinyapp", "app.R", package = "micsPlusTableR"), local = env)
  html <- htmltools::renderTags(env$ui)$html
  expect_true(grepl(paste0('value="', normalizePath(project, winslash = "/"), '"'), html, fixed = TRUE))
  expect_lt(regexpr('data-value="Download Files"', html)[[1]], regexpr('data-value="Data Preparation"', html)[[1]])
  expect_match(html, 'id="main_nav"', fixed = TRUE)
  expect_match(html, 'id="survey_download-surveys" multiple="multiple"', fixed = TRUE)
  expect_match(html, 'id="survey_download-select_all"', fixed = TRUE)
  expect_match(html, 'id="survey_download-deselect_all"', fixed = TRUE)
  expect_match(html, "micsSurveyRefreshState", fixed = TRUE)
  expect_false(grepl('id="survey_download-scope"', html, fixed = TRUE))
  expect_match(html, 'id="survey_download-destination"', fixed = TRUE)
  expect_match(html, "Use current project", fixed = TRUE)
  expect_match(html, 'href="https://mics.unicef.org/surveys"', fixed = TRUE)
  expect_match(html, "Browse folders", fixed = TRUE)
})

test_that("run_app captures the project before Shiny changes working directory", {
  expected <- normalizePath(here::here(), winslash = "/")
  previous <- getOption("micsPlusTableR.project_dir")
  local_mocked_bindings(runApp = function(...) {
    expect_identical(getOption("micsPlusTableR.project_dir"), expected)
    expect_false(identical(getOption("micsPlusTableR.project_dir"), system.file("shinyapp", package = "micsPlusTableR")))
    invisible(NULL)
  }, .package = "shiny")
  run_app(output_dir = tempfile(), launch.browser = FALSE)
  expect_identical(getOption("micsPlusTableR.project_dir"), previous)
})

test_that("the main app starts loading only when Download Files is opened", {
  requested <- 0L
  old <- options(micsPlusTableR.output_dir = tempfile("download-app-"))
  on.exit(options(old), add = TRUE)
  local_mocked_bindings(survey_catalogue_cache = new.env(parent = emptyenv()),
    survey_catalogue_load = function(on_success, on_error, on_progress) {
      requested <<- requested + 1L
      function() NULL
    })
  app <- source(system.file("shinyapp", "app.R", package = "micsPlusTableR"),local = new.env())$value
  shiny::testServer(app, {
    session$setInputs(main_nav = "Data Preparation")
    expect_identical(requested, 0L)
    session$setInputs(main_nav = "Download Files")
    expect_identical(requested, 1L)
    session$setInputs(main_nav = "Data Preparation")
    session$setInputs(main_nav = "Download Files")
    expect_identical(requested, 1L)
  })
})

test_that("relative download paths stay anchored to the project", {
  project <- tempfile(); dir.create(project)
  expect_identical(resolve_survey_download_dir("inputs", project), file.path(project, "inputs"))
  expect_identical(resolve_survey_download_dir(project, tempdir()), normalizePath(project, winslash = "/"))
  expect_error(resolve_survey_download_dir("  ", project), "Choose a download folder")
})
