test_that("the packaged survey CSV is available and valid", {
  choices <- micsPlusTableR:::read_survey_choices()
  expect_gt(nrow(choices), 0L)
  expect_true(all(vapply(choices, is.character, logical(1))))
})

test_that("invalid survey configuration fails with an actionable error", {
  read_choices <- micsPlusTableR:::read_survey_choices
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  expect_error(read_choices(path), "survey_choices.csv file is missing", fixed = TRUE)

  writeLines("country,period,wave\nJamaica,2023-24,Wave 1", path)
  expect_error(read_choices(path), "must contain country, country_code, period, and wave")

  header <- "country,country_code,period,wave"
  writeLines(header, path)
  expect_error(read_choices(path), "at least one row")
  writeLines(c(header, "Jamaica,JAM,2023-24, "), path)
  expect_error(read_choices(path), "no empty fields")
  writeLines(c(header, "Jamaica,JM,2023-24,Wave 1"), path)
  expect_error(read_choices(path), "three uppercase letters")
  writeLines(c(header, "Jamaica,JAM,2023-24,Wave 1",
               "Jamaica,XYZ,2023-24,Wave 2"), path)
  expect_error(read_choices(path), "one country code")
  writeLines(c(header, rep("Jamaica,JAM,2023-24,Wave 1", 2)), path)
  expect_error(read_choices(path), "duplicate country, period, and wave")
})

test_that("CSV edits drive the app choices, allowed waves, and output metadata", {
  path <- tempfile(fileext = ".csv")
  output_dir <- tempfile("survey-choice-outputs-")
  on.exit(unlink(c(path, output_dir), recursive = TRUE))
  writeLines(c(
    "country,country_code,period,wave",
    " Jamaica , JAM ,2023-24,Wave 1",
    "Jamaica,JAM,2023-24,Wave 2",
    "Fiji,FJI,2027-28,Wave 3",
    "Fiji,FJI,2027-28,Wave 2"
  ), path)
  read_choices <- micsPlusTableR:::read_survey_choices
  testthat::local_mocked_bindings(
    read_survey_choices = function() read_choices(path),
    .package = "micsPlusTableR"
  )
  old_options <- options(micsPlusTableR.output_dir = output_dir, sass.cache = FALSE)
  on.exit(options(old_options), add = TRUE)
  app_env <- new.env()
  app <- source(system.file("shinyapp", "app.R", package = "micsPlusTableR"),
                local = app_env)$value
  html <- htmltools::renderTags(app_env$ui)$html
  expect_match(html, "Fiji (2027-28)", fixed = TRUE)
  expect_false(grepl("Mongolia (2025-26)", html, fixed = TRUE))

  shiny::testServer(app, {
    wave_update <- NULL
    session$sendInputMessage <- function(inputId, message) {
      if (identical(inputId, "select_wave")) wave_update <<- message
    }

    # A country input may arrive before the wave input on first load.
    session$setInputs(select_country = "Jamaica (2023-24)")
    # The initial dropdown already supplies these choices and its default.
    expect_null(wave_update)
    session$setInputs(select_wave = "Wave 1")
    expect_identical(survey_meta_rv()$country_abb, "JAM")

    # Changing survey replaces an unavailable wave with the first CSV wave.
    session$setInputs(select_country = "Fiji (2027-28)")
    expect_identical(wave_update$value, "Wave 3")
    expect_match(as.character(wave_update$options), "Wave 3", fixed = TRUE)
    expect_false(grepl("Wave 1", as.character(wave_update$options), fixed = TRUE))
    expect_error(survey_meta_rv(), class = "shiny.silent.error")

    # Simulate the browser applying the updated selection.
    session$setInputs(select_wave = "Wave 3")
    expect_identical(survey_meta_rv(), list(
      country = "Fiji", country_abb = "FJI", period = "2027-28", wave = "Wave 3"
    ))
    expect_identical(engine_env$country_abb, "FJI")

    # Keep a chosen wave if it is also available in the next survey.
    session$setInputs(select_wave = "Wave 2")
    session$setInputs(select_country = "Jamaica (2023-24)")
    expect_identical(wave_update$value, "Wave 2")
    expect_identical(survey_meta_rv()$country_abb, "JAM")
  })
})

test_that("Turkmenistan Wave 2 survives uploads without stale wave updates", {
  output_dir <- tempfile("wave-upload-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  old_options <- options(micsPlusTableR.output_dir = output_dir)
  on.exit(options(old_options), add = TRUE)
  tab_path <- file.path(output_dir, "plan.xlsx")
  openxlsx2::wb_workbook()$add_worksheet("IDX")$
    add_data("IDX", c("Turkmenistan", "TKM", "2025-26", "Wave 2"))$
    add_worksheet("Example")$save(tab_path)
  prep_path <- file.path(output_dir, "prep.R")
  writeLines("# Upload fixture", prep_path)
  sav_path <- file.path(output_dir, "hh.sav")
  haven::write_sav(data.frame(id = 1), sav_path)
  upload <- function(path) data.frame(
    name = basename(path), size = file.info(path)$size,
    type = "application/octet-stream", datapath = path)
  app_env <- new.env()
  app <- source(system.file("shinyapp", "app.R", package = "micsPlusTableR"),
                local = app_env)$value

  shiny::testServer(app, {
    wave_updates <- list()
    session$sendInputMessage <- function(inputId, message) {
      if (identical(inputId, "select_wave")) {
        wave_updates[[length(wave_updates) + 1L]] <<- message
      }
    }
    session$setInputs(select_country = "Jamaica (2023-24)", select_wave = "Wave 1")
    session$setInputs(select_country = "Turkmenistan (2025-26)")
    # A queued Wave 1 update could overwrite the user's next choice in the browser.
    expect_length(wave_updates, 0L)
    session$setInputs(select_wave = "Wave 2")
    session$setInputs(hh_file = upload(sav_path))
    session$setInputs(hl_file = upload(sav_path))
    session$setInputs(tab_file = upload(tab_path))
    session$setInputs(prep_folder = upload(prep_path),
                      prep_folder_relative_paths = "preparation/prep.R")
    session$flushReact()
    expect_length(wave_updates, 0L)
    expect_identical(input$select_wave, "Wave 2")
    expect_identical(survey_meta_rv(), list(country = "Turkmenistan",
      country_abb = "TKM", period = "2025-26", wave = "Wave 2"))
    expect_identical(tab_path_rv(), tab_path)
    expect_true(file.exists(prep_folder_selection()$prep_script))
    # Replacing a plan must also preserve the wave.
    replacement <- upload(tab_path)
    replacement$name <- "replacement-plan.xlsx"
    session$setInputs(tab_file = replacement)
    expect_length(wave_updates, 0L)
    expect_identical(survey_meta_rv()$wave, "Wave 2")
  })
})
