test_that("standalone paths resolve from the helper folder and create outputs", {
  root <- tempfile("FIES setup with spaces ")
  inputs <- file.path(root, "preparation", "FIES-inputs")
  dir.create(inputs, recursive = TRUE)
  hh <- file.path(root, "TEST_W3_hh.sav")
  file.create(hh)
  before <- getwd()
  paths <- fies_resolve_paths(inputs, "../../TEST_W3_hh.sav", "../FIES-outputs")
  expect_identical(paths$hh, normalizePath(hh, winslash = "/"))
  expect_identical(paths$output, normalizePath(file.path(root, "preparation", "FIES-outputs"), winslash = "/"))
  expect_true(dir.exists(paths$output))
  paths <- fies_resolve_paths(inputs, hh, file.path(root, "app output"))
  expect_identical(paths$output, normalizePath(file.path(root, "app output"), winslash = "/"))
  expect_error(fies_resolve_paths(inputs, "missing.sav", "ignored"), "Household data not found")
  expect_error(fies_resolve_paths(inputs, hh, NULL), "Set path_fies_output")
  expect_identical(getwd(), before)
})

test_that("helper discovery is independent of survey prefixes and rejects ambiguity", {
  inputs <- tempfile()
  dir.create(inputs)
  helper <- file.path(inputs, "Different survey_Wave9_helper.R")
  file.create(helper)
  expect_identical(fies_input_file(inputs, "helper.R"), helper)
  expect_error(fies_input_file(inputs, "absent.R"), "found 0")
  file.create(file.path(inputs, "helper.R"))
  expect_error(fies_input_file(inputs, "helper.R"), "found 2")
})

test_that("distributed runtime matches package helpers and needs no package environment", {
  runtime <- system.file("fies", "FIES-runtime.R", package = "micsPlusTableR")
  standalone <- new.env(parent = baseenv())
  sys.source(runtime, envir = standalone)
  for (name in c("fies_rm_w", "fies_pc_w", "fies_model_writer", "fies_resolve_paths",
                 "fies_input_file", "fies_load_dependencies")) {
    expected <- get(name, envir = asNamespace("micsPlusTableR"))
    expect_identical(body(standalone[[name]]), body(expected))
    expect_identical(formals(standalone[[name]]), formals(expected))
  }
  inputs <- tempfile()
  dir.create(inputs)
  file.create(file.path(inputs, "helper.R"))
  expect_identical(standalone$fies_input_file(inputs, "helper.R"), file.path(inputs, "helper.R"))
})
