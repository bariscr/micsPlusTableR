download_tab_fixture <- function() {
  x <- data.frame(survey = c("Jamaica (2023-24) Wave 1", "Jamaica (2023-24) Wave 1", "Mongolia (2025-26) Wave 2"),
                  path = c("Jamaica (2023-24) Wave 1/plan.xlsx", "Jamaica (2023-24) Wave 1/prep-files-Jamaica (2023-24) Wave 1/prep.R", "Mongolia (2025-26) Wave 2/plan.xlsx"),
                  size = c(100, 200, 400), url = "https://example.invalid")
  attr(x, "commit") <- strrep("a", 40)
  x
}

test_that("the tab supports one, multiple, and all surveys with chosen destinations", {
  fixture <- download_tab_fixture()
  calls <- list()
  project <- tempfile("project-")
  dir.create(project)
  local_mocked_bindings(
    list_survey_files = function(...) fixture,
    download_survey_files = function(surveys, dest_dir, overwrite, quiet, ref) {
      calls[[length(calls) + 1L]] <<- list(surveys = surveys, dest_dir = dest_dir,
                                        overwrite = overwrite, ref = ref)
      fixture[fixture$survey %in% surveys, ]
    }
  )
  shiny::testServer(download_files_server, args = list(project_dir = project), {
    session$setInputs(scope = "selected", destination = project, overwrite = FALSE)
    session$setInputs(download = 1)
    expect_match(notice()$message, "Load the available surveys first")
    expect_length(calls, 0)
    session$setInputs(refresh = 1)
    expect_equal(nrow(catalogue()), 3)
    session$setInputs(download = 2)
    expect_match(notice()$message, "Choose at least one survey")
    session$setInputs(surveys = "Jamaica (2023-24) Wave 1", download = 3)
    expect_identical(calls[[1]]$surveys, "Jamaica (2023-24) Wave 1")
    expect_identical(calls[[1]]$dest_dir, normalizePath(project, winslash = "/"))
    expect_identical(calls[[1]]$ref, attr(fixture, "commit"))
    expect_false(calls[[1]]$overwrite)
    expect_equal(notice()$kind, "success")
    session$setInputs(surveys = c("Jamaica (2023-24) Wave 1", "Mongolia (2025-26) Wave 2"), destination = "selected-files", download = 4)
    expect_setequal(calls[[2]]$surveys, c("Jamaica (2023-24) Wave 1", "Mongolia (2025-26) Wave 2"))
    expect_identical(calls[[2]]$dest_dir, file.path(project, "selected-files"))
    session$setInputs(scope = "all", surveys = character(), destination = project,
                     overwrite = TRUE, download = 5)
    expect_setequal(calls[[3]]$surveys, unique(fixture$survey))
    expect_true(calls[[3]]$overwrite)
    session$setInputs(destination = "", download = 6)
    expect_match(notice()$message, "Choose a download folder")
    expect_length(calls, 3)
  })
})

test_that("listing and download failures are shown without stale success", {
  fail_listing <- FALSE
  local_mocked_bindings(
    list_survey_files = function(...) {
      if (fail_listing) stop("Repository unavailable")
      download_tab_fixture()
    },
    download_survey_files = function(...) stop("Files already exist")
  )
  shiny::testServer(download_files_server, args = list(project_dir = tempdir()), {
    session$setInputs(scope = "all", destination = tempdir(), refresh = 1)
    session$setInputs(download = 1)
    expect_equal(notice()$kind, "danger")
    expect_match(notice()$message, "Files already exist")
    fail_listing <<- TRUE
    session$setInputs(refresh = 2)
    expect_null(catalogue())
    expect_match(notice()$message, "Repository unavailable")
  })
})

test_that("the UI starts at the caller's project and precedes Data Preparation", {
  project <- tempfile("caller-project-")
  dir.create(project)
  old <- options(micsPlusTableR.project_dir = project,
                 micsPlusTableR.output_dir = tempfile("output-"))
  on.exit(options(old), add = TRUE)
  env <- new.env()
  source(system.file("shinyapp", "app.R", package = "micsPlusTableR"), local = env)
  html <- htmltools::renderTags(env$ui)$html
  expect_true(grepl(paste0('value="', normalizePath(project, winslash = "/"), '"'), html, fixed = TRUE))
  expect_lt(regexpr('data-value="Download Files"', html)[[1]],
            regexpr('data-value="Data Preparation"', html)[[1]])
  expect_match(html, 'id="survey_download-destination"', fixed = TRUE)
  expect_match(html, "Use current project", fixed = TRUE)
  expect_match(html, 'href="https://mics.unicef.org/surveys"', fixed = TRUE)
  expect_match(html, "Browse folders", fixed = TRUE)
  expect_match(html, "Household and household-member data are obtained separately", fixed = TRUE)
})

test_that("run_app captures the project before Shiny changes working directory", {
  expected <- normalizePath(here::here(), winslash = "/")
  previous <- getOption("micsPlusTableR.project_dir")
  local_mocked_bindings(runApp = function(...) {
    expect_identical(getOption("micsPlusTableR.project_dir"), expected)
    expect_false(identical(getOption("micsPlusTableR.project_dir"),
                           system.file("shinyapp", package = "micsPlusTableR")))
    invisible(NULL)
  }, .package = "shiny")
  run_app(output_dir = tempfile(), launch.browser = FALSE)
  expect_identical(getOption("micsPlusTableR.project_dir"), previous)
})

test_that("relative download paths stay anchored to the project", {
  project <- tempfile()
  dir.create(project)
  expect_identical(resolve_survey_download_dir("inputs", project), file.path(project, "inputs"))
  expect_identical(resolve_survey_download_dir(project, tempdir()), normalizePath(project, winslash = "/"))
  expect_error(resolve_survey_download_dir("  ", project), "Choose a download folder")
})

test_that("legacy codes are hidden in download choices, previews, and success messages", {
  fixture <- download_tab_fixture()
  fixture$survey <- c("JAM_W1", "JAM_W1", "MNG_W2")
  choices <- NULL
  local_mocked_bindings(
    list_survey_files = function(...) fixture,
    download_survey_files = function(...) fixture
  )
  local_mocked_bindings(updateCheckboxGroupInput = function(session, inputId, ...) {
    choices <<- list(...)$choices
  }, .package = "shiny")
  shiny::testServer(download_files_server, args = list(project_dir = tempdir()), {
    session$setInputs(scope = "all", destination = tempdir(), refresh = 1)
    expect_equal(names(choices), c("Jamaica (2023-24) Wave 1", "Mongolia (2025-26) Wave 2"))
    expect_equal(unname(choices), c("JAM_W1", "MNG_W2"))
    expect_match(output$preview, "Jamaica (2023-24) Wave 1", fixed = TRUE)
    expect_false(grepl("JAM_W1", output$preview, fixed = TRUE))
    session$setInputs(download = 1)
    expect_match(notice()$message, "Jamaica (2023-24) Wave 1", fixed = TRUE)
    expect_false(grepl("JAM_W1", notice()$message, fixed = TRUE))
  })
})
