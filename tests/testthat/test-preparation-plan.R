test_that("included preparation accepts multiline cells and ordered code rows", {
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
  wb$add_data("Preparation File", data.frame(code = "hh <- (", is_included = 1))$
    save(plan, overwrite = TRUE)
  expect_error(micsPlusTableR:::read_preparation_code(plan), "Cannot read R code")
  wb$add_data("Preparation File", "hh <- data.frame()", dims = "A1", col_names = FALSE)
  wb$save(plan, overwrite = TRUE)
  expect_error(micsPlusTableR:::read_preparation_code(plan), "'code' in A1.*'is_included' in B1")
  wb$add_data("Preparation File", "code", dims = "A1", col_names = FALSE)
  wb$add_data("Preparation File", "unexpected", dims = "C2", col_names = FALSE)
  wb$save(plan, overwrite = TRUE)
  expect_error(micsPlusTableR:::read_preparation_code(plan), "other columns must be empty")
})

test_that("only an inclusion flag of 1 selects preparation code", {
  flags <- c("1", "0", NA, "", "2", "-1", "TRUE", "yes", "01", "1.0", " 1 ")
  pieces <- rep("stop('disabled piece ran')", length(flags))
  pieces[c(1, 11)] <- c("value <- 10", "value <- value + 5")
  plan <- preparation_plan(code = pieces, split_lines = TRUE, is_included = flags)
  code <- micsPlusTableR:::read_preparation_code(plan)$code
  env <- new.env()
  eval(parse(text = code), env)
  expect_equal(env$value, 15)
  expect_false(grepl("disabled piece", code, fixed = TRUE))
  numeric_plan <- preparation_plan(code = c("value <- 3", "stop('excluded')"),
    split_lines = TRUE, is_included = c(1, 0))
  expect_identical(micsPlusTableR:::read_preparation_code(numeric_plan)$code, "value <- 3")
})

test_that("disabled pieces are excluded before syntax and dependency checks", {
  plan <- preparation_plan(code = c("hh <- data.frame(id = 1)",
    "hl <- data.frame(id = 2)", "broken <- (", "run_fies_table()",
    "missingPrepPackage::function_that_does_not_exist()"),
    split_lines = TRUE, is_included = c(1, 1, 0, 0, 0))
  parsed <- micsPlusTableR:::read_preparation_code(plan)
  expect_false(parsed$uses_fies)
  expect_false("missingPrepPackage" %in% check_prep_dependencies(plan)$package)
  files <- c(tempfile(), tempfile())
  file.create(files)
  session <- mics_session()
  prepare_mics_data(session, files[1], files[2], plan)
  expect_equal(session$hh$id, 1)
  expect_equal(session$hl$id, 2)
  expect_null(session$path_fies)
  # Enabling the same helper call makes the folder a prerequisite again.
  enabled <- preparation_plan(code = "run_fies_table()", is_included = 1)
  expect_error(check_prep_dependencies(enabled), "uses FIES helpers")
})

test_that("header-only or wholly excluded preparation reports no included instructions", {
  for (flag in list(0, NA, "", 2)) {
    plan <- preparation_plan(code = "stop('excluded')", is_included = flag)
    expect_error(micsPlusTableR:::read_preparation_code(plan), "no included R instructions")
  }
  plan <- tempfile(fileext = ".xlsx")
  openxlsx2::write_xlsx(list(`Preparation File` = data.frame(
    code = character(), is_included = integer())), plan)
  expect_error(micsPlusTableR:::read_preparation_code(plan), "no included R instructions")
})

test_that("preparation headers are required and cannot be reversed", {
  for (headers in list(c("is_included", "code"), c("code", "code"),
                       c("code", "included"))) {
    plan <- tempfile(fileext = ".xlsx")
    values <- stats::setNames(data.frame("hh <- data.frame()", "1"), headers)
    openxlsx2::write_xlsx(list(`Preparation File` = values), plan)
    expect_error(micsPlusTableR:::read_preparation_code(plan), "'code' in A1.*'is_included' in B1")
  }
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
    "hl <- haven::read_sav(hl_path)", "stop('disabled app preparation ran')"),
    split_lines = TRUE, is_included = c(1, 1, 0))
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
