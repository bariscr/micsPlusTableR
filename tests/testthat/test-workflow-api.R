test_that("the supported non-Shiny API is exported", {
  expected <- c(
    "mics_session", "prepare_mics_data", "read_mics_tabulation",
    "mics_table_name", "tabulate_mics_table", "pivot_mics_table",
    "check_mics_table", "write_mics_table", "write_mics_footnotes",
    "compare_mics_tables", "read_previous_mics_table"
  )
  expect_true(all(expected %in% getNamespaceExports("micsPlusTableR")))
})

test_that("mics_session stores state outside the global workspace", {
  session <- mics_session(hh = data.frame(id = 1), hl = data.frame(id = 2))
  expect_s3_class(session, "mics_tabulation_session")
  expect_equal(session$hh$id, 1)
  expect_equal(session$hl$id, 2)
  expect_false(exists("cell_results", envir = .GlobalEnv, inherits = FALSE))
})

test_that("prepare_mics_data loads required and extra-table objects", {
  hh_file <- tempfile(fileext = ".sav")
  hl_file <- tempfile(fileext = ".sav")
  prep_file <- tempfile(fileext = ".R")
  file.create(hh_file, hl_file)
  writeLines(c(
    "hh <- data.frame(id = 1:2, total = 1)",
    "hl <- data.frame(id = 1:3, total = 1)",
    "tables_extra <- 'X1'",
    "extra_tables_dict <- list(X1 = 'tb_x1')",
    "tb_x1 <- data.frame(stub = 'Total', value = 100)"
  ), prep_file)

  session <- mics_session()
  expect_invisible(prepare_mics_data(session, hh_file, hl_file, prep_file))
  expect_equal(nrow(session$hh), 2)
  expect_equal(session$tables_extra, "X1")
  expect_true(is.data.frame(session$tb_x1))

  writeLines(c(
    "hh <- data.frame(id = 10, total = 1)",
    "hl <- data.frame(id = 20, total = 1)"
  ), prep_file)
  prepare_mics_data(session, hh_file, hl_file, prep_file)
  expect_identical(session$tables_extra, character(0))
  expect_identical(session$extra_tables_dict, list())
  expect_false(exists("tb_x1", envir = session, inherits = FALSE))
})

test_that("a selected prep script discovers sibling FIES helpers", {
  prep_dir <- tempfile("prep-folder-")
  fies_dir <- file.path(prep_dir, "FIES-inputs")
  output_dir <- tempfile("mics-output-")
  dir.create(fies_dir, recursive = TRUE)

  prep_file <- file.path(prep_dir, "survey_prep-data.R")
  helper_file <- file.path(fies_dir, "fies-helper.R")
  writeLines(c(
    "source('fies-helper.R')",
    "hh <- data.frame(id = 1, helper = helper_value)",
    "hl <- data.frame(id = 2, helper = helper_value)"
  ), prep_file)
  writeLines(c(
    "helper_value <- 41",
    "writeLines('FIES result', 'helper-output.txt')"
  ), helper_file)

  hh_file <- tempfile(fileext = ".sav")
  hl_file <- tempfile(fileext = ".sav")
  file.create(hh_file, hl_file)

  inputs <- micsPlusTableR:::resolve_preparation_inputs(prep_file)
  expect_identical(inputs$prep_script, normalizePath(prep_file, winslash = "/"))
  expect_identical(inputs$fies_inputs_dir, normalizePath(fies_dir, winslash = "/"))

  session <- mics_session()
  expect_invisible(prepare_mics_data(
    session,
    hh_path = hh_file,
    hl_path = hl_file,
    prep_script = prep_file,
    output_dir = output_dir
  ))

  fies_output <- file.path(output_dir, "FIES-outputs")
  expect_equal(session$hh$helper, 41)
  expect_identical(session$path_fies, normalizePath(fies_dir, winslash = "/"))
  expect_identical(
    session$path_fies_output,
    normalizePath(fies_output, winslash = "/")
  )
  expect_true(file.exists(file.path(fies_output, "helper-output.txt")))
})

test_that("FIES-outputs is not created without FIES-inputs", {
  prep_dir <- tempfile("prep-folder-")
  output_dir <- tempfile("mics-output-")
  dir.create(prep_dir)
  writeLines(c(
    "hh <- data.frame(id = 1)",
    "hl <- data.frame(id = 2)"
  ), file.path(prep_dir, "prep.R"))

  hh_file <- tempfile(fileext = ".sav")
  hl_file <- tempfile(fileext = ".sav")
  file.create(hh_file, hl_file)

  prepare_mics_data(
    mics_session(),
    hh_path = hh_file,
    hl_path = hl_file,
    prep_script = file.path(prep_dir, "prep.R"),
    output_dir = output_dir
  )

  expect_false(dir.exists(file.path(output_dir, "FIES-outputs")))
})

test_that("the preparation input must be an R file", {
  prep_file <- tempfile(fileext = ".txt")
  file.create(prep_file)
  expect_error(
    micsPlusTableR:::resolve_preparation_inputs(prep_file),
    "must be an R file"
  )
})

test_that("a native preparation-folder upload is staged with FIES helpers", {
  upload_dir <- tempfile("browser-upload-")
  dir.create(upload_dir)
  uploaded_paths <- file.path(upload_dir, c("main", "helper", "asset"))
  writeLines("hh <- data.frame(id = 1); hl <- data.frame(id = 2)", uploaded_paths[[1L]])
  writeLines("helper_value <- 1", uploaded_paths[[2L]])
  writeLines("reference", uploaded_paths[[3L]])

  uploaded <- data.frame(
    name = c("country_prep.R", "fies-helper.R", "reference.txt"),
    datapath = uploaded_paths,
    stringsAsFactors = FALSE
  )
  relative <- c(
    "survey-preparation/country_prep.R",
    "survey-preparation/FIES-inputs/fies-helper.R",
    "survey-preparation/FIES-inputs/reference.txt"
  )

  staged <- micsPlusTableR:::stage_preparation_upload(
    uploaded,
    relative,
    stage_dir = tempfile("staged-prep-")
  )

  expect_identical(staged$folder_name, "survey-preparation")
  expect_identical(basename(staged$prep_script), "country_prep.R")
  expect_identical(basename(staged$fies_inputs_dir), "FIES-inputs")
  expect_true(file.exists(file.path(staged$fies_inputs_dir, "fies-helper.R")))
  expect_identical(staged$uploaded_file_count, 3L)
})

test_that("folder upload chooses the only prep-like top-level R file", {
  upload_dir <- tempfile("browser-upload-")
  dir.create(upload_dir)
  uploaded_paths <- file.path(upload_dir, c("notes", "main"))
  writeLines("notes <- TRUE", uploaded_paths[[1L]])
  writeLines("hh <- data.frame(); hl <- data.frame()", uploaded_paths[[2L]])

  staged <- micsPlusTableR:::stage_preparation_upload(
    data.frame(
      name = c("notes.R", "wave-preparation.R"),
      datapath = uploaded_paths,
      stringsAsFactors = FALSE
    ),
    c("prep-folder/notes.R", "prep-folder/wave-preparation.R"),
    stage_dir = tempfile("staged-prep-")
  )

  expect_identical(basename(staged$prep_script), "wave-preparation.R")
})

test_that("folder upload rejects ambiguous main preparation scripts", {
  upload_dir <- tempfile("browser-upload-")
  dir.create(upload_dir)
  uploaded_paths <- file.path(upload_dir, c("one", "two"))
  writeLines("x <- 1", uploaded_paths[[1L]])
  writeLines("x <- 2", uploaded_paths[[2L]])

  expect_error(
    micsPlusTableR:::stage_preparation_upload(
      data.frame(
        name = c("first-prep.R", "second-prep.R"),
        datapath = uploaded_paths,
        stringsAsFactors = FALSE
      ),
      c("prep-folder/first-prep.R", "prep-folder/second-prep.R"),
      stage_dir = tempfile("staged-prep-")
    ),
    "multiple possible preparation R files"
  )
})

test_that("compare_mics_tables exposes the pure comparison helper", {
  previous <- data.frame(a = c("1", "2"), b = c("x", "y"))
  current <- data.frame(a = c("1", "3"), b = c("x", "z"))
  differences <- compare_mics_tables(previous, current)
  expect_true(is.data.frame(differences))
  expect_equal(nrow(differences), 2)
})

test_that("workflow functions reject ordinary environments", {
  expect_error(mics_table_name(new.env()), "mics_session")
  expect_error(check_mics_table(mics_session()), "No cell results")
})
