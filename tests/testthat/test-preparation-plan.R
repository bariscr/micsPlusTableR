test_that("embedded preparation accepts multiline cells and lines down column A", {
  code <- c("# FIES mentioned only in a comment", "hh <- data.frame(id = 1)",
            "", "hl <- data.frame(id = 2)")
  for (split in c(FALSE, TRUE)) {
    plan <- preparation_plan(code = code, split_lines = split)
    parsed <- micsPlusTableR:::read_preparation_code(plan)
    expect_false(parsed$uses_fies)
    expect_equal(length(parse(text = parsed$code)), 2)
  }
  plan <- preparation_plan(code = paste(code, collapse = "\r\n"))
  expect_equal(length(parse(text = micsPlusTableR:::read_preparation_code(plan)$code)), 2)
})

test_that("invalid embedded preparation fails before executing any code", {
  plan <- tempfile(fileext = ".xlsx")
  wb <- openxlsx2::wb_workbook()$add_worksheet("IDX")
  wb$save(plan)
  expect_error(micsPlusTableR:::read_preparation_code(plan), "must contain.*Preparation File")
  wb$add_worksheet("Preparation File")$save(plan, overwrite = TRUE)
  expect_error(micsPlusTableR:::read_preparation_code(plan), "empty")
  wb$add_data("Preparation File", "hh <- (", col_names = FALSE)$save(plan, overwrite = TRUE)
  expect_error(micsPlusTableR:::read_preparation_code(plan), "Cannot read R code")
  wb$add_data("Preparation File", "hh <- data.frame()", col_names = FALSE)
  wb$add_data("Preparation File", "hl <- data.frame()", dims = "B1", col_names = FALSE)
  wb$save(plan, overwrite = TRUE)
  expect_error(micsPlusTableR:::read_preparation_code(plan), "column A")
})

test_that("non-FIES plans ignore supplied folders and never inspect their scripts", {
  plan <- preparation_plan(code = c("hh <- data.frame(id = 1)", "hl <- data.frame(id = 2)"))
  folder <- tempfile()
  dir.create(file.path(folder, "FIES-inputs"), recursive = TRUE)
  writeLines("this is not R code", file.path(folder, "FIES-inputs", "broken.R"))
  writeLines("stop('external preparation must not run')", file.path(folder, "prep.R"))
  files <- c(tempfile(), tempfile())
  file.create(files)
  for (prep_folder in list(NULL, folder, "nonexistent-folder")) {
    s <- mics_session()
    output <- tempfile()
    prepare_mics_data(s, files[1], files[2], plan, output_dir = output, prep_folder = prep_folder)
    expect_equal(s$hh$id, 1)
    expect_null(s$path_fies)
    expect_null(s$path_fies_output)
    expect_false(dir.exists(file.path(output, "FIES-outputs")))
  }
})

test_that("embedded helper calls require a FIES folder and dependencies are inspected", {
  plan <- preparation_plan(code = c("source(file.path(path_fies, 'helper.R'))",
    "hh <- data.frame(id = helper_value)", "hl <- data.frame(id = 2)"))
  expect_error(check_prep_dependencies(plan), "uses FIES helpers")
  folder <- tempfile()
  dir.create(file.path(folder, "FIES-inputs"), recursive = TRUE)
  writeLines("helper_value <- dplyr::n_distinct(c(1, 1, 2))",
             file.path(folder, "FIES-inputs", "helper.R"))
  deps <- check_prep_dependencies(plan, prep_folder = folder)
  expect_true("dplyr" %in% deps$package)
  files <- c(tempfile(), tempfile())
  file.create(files)
  s <- mics_session()
  output <- tempfile()
  prepare_mics_data(s, files[1], files[2], plan, output, folder)
  expect_equal(s$hh$id, 2)
  relative <- preparation_plan(code = c("source('FIES-inputs/helper.R')",
    "hh <- data.frame(id = helper_value)", "hl <- data.frame(id = 2)"))
  prepare_mics_data(s, files[1], files[2], relative, output, folder)
  expect_equal(s$hh$id, 2)
  # Reusing a session for a non-FIES plan clears all FIES paths.
  plain <- preparation_plan(code = c("hh <- data.frame(id = 3)", "hl <- data.frame(id = 4)"))
  prepare_mics_data(s, files[1], files[2], plain)
  expect_null(s$path_fies)
  expect_null(s$path_fies_output)
})

test_that("the app prepares an embedded plan without a folder and excludes the code sheet from tables", {
  output <- tempfile()
  old <- options(micsPlusTableR.output_dir = output)
  on.exit(options(old), add = TRUE)
  plan <- preparation_plan(code = c("hh <- haven::read_sav(hh_path)",
                                    "hl <- haven::read_sav(hl_path)"))
  sav <- tempfile(fileext = ".sav")
  haven::write_sav(data.frame(total = 1, id = 5), sav)
  upload <- function(path) data.frame(name = basename(path), size = file.info(path)$size,
                                     type = "application/octet-stream", datapath = path)
  app <- source(system.file("shinyapp", "app.R", package = "micsPlusTableR"),
                local = new.env())$value
  shiny::testServer(app, {
    session$setInputs(select_country = "Test (2026)", select_wave = "Wave 1",
      hh_file = upload(sav), hl_file = upload(sav), tab_file = upload(plan))
    expect_false("Preparation File" %in% sheets_all_rv())
    session$setInputs(run_settings = 1)
    expect_equal(as.numeric(hh_rv()$id), 5)
    expect_equal(as.numeric(hl_rv()$id), 5)
    expect_null(engine_env$path_fies)
  })
})
