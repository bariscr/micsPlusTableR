make_dependency_script <- function(lines, helpers = list()) {
  directory <- tempfile("prep-dependencies-")
  dir.create(directory)
  script <- file.path(directory, "prep.R")
  writeLines(lines, script)
  for (name in names(helpers)) {
    path <- file.path(directory, name)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(helpers[[name]], path)
  }
  script
}

test_that("preparation inspection follows helpers without executing scripts", {
  script <- make_dependency_script(c(
    "prep_dependencies <- c('dplyr', 'tidyr')",
    "base::source(local = TRUE, file = 'helper.R')",
    "stop('must not execute')",
    "# library(unusedCommentPackage)",
    "description <- 'fakepkg::function()'"
  ), helpers = list(
    "helper.R" = c("haven::read_sav('not-a-file.sav')", "source('prep.R')"),
    "FIES-inputs/fies.R" = c("memisc::spss.system.file('not-a-file.sav')", "RM.w(x)")
  ))
  testthat::local_mocked_bindings(prep_package_problem = function(package) NA_character_)
  result <- check_prep_dependencies(script)
  expect_setequal(result$package, c("dplyr", "tidyr", "haven", "memisc", "RM.weights"))
  expect_true(all(result$available))
  expect_match(result$required_by[result$package == "haven"], "helper.R", fixed = TRUE)
  expect_match(result$required_by[result$package == "memisc"], "fies.R", fixed = TRUE)
})

test_that("library declarations and legacy calls are recognized", {
  script <- make_dependency_script(c(
    "library(survey)", "base::require('reshape2')", "requireNamespace('labelled')",
    "pkgs <- 'ignore_dynamic_name'", "library(pkgs, character.only = TRUE)",
    "as_survey_design(df)", "ggplot(df)", "Hmisc::wtd.mean(x)", "stats::median(x)"
  ))
  testthat::local_mocked_bindings(prep_package_problem = function(package) NA_character_)
  expect_setequal(check_prep_dependencies(script)$package,
    c("survey", "reshape2", "labelled", "srvyr", "ggplot2", "Hmisc"))
})

test_that("dependency declarations cannot execute arbitrary expressions", {
  script <- make_dependency_script("prep_dependencies <- system('must not execute')")
  expect_error(check_prep_dependencies(script), "literal character vector")
  writeLines("prep_dependencies <- c('not a package')", script)
  expect_error(check_prep_dependencies(script), "Invalid preparation package name")
  writeLines("x <- (", script)
  expect_error(check_prep_dependencies(script), "Cannot read preparation dependencies")
})

test_that("missing dependencies stop preparation before the script runs", {
  script <- make_dependency_script(c(
    "prep_dependencies <- c('missingPkg')",
    "stop('PREPARATION SHOULD NOT RUN')"
  ))
  testthat::local_mocked_bindings(prep_package_problem = function(package) "not installed")
  result <- check_prep_dependencies(script)
  expect_false(result$available)
  expect_error(check_prep_dependencies(script, TRUE), "missingPkg.*prep.R")
  files <- file.path(dirname(script), c("hh.sav", "hl.sav"))
  file.create(files)
  expect_error(prepare_mics_data(mics_session(), files[1], files[2], script),
    "install_prep_dependencies")
})

test_that("preparation functions are available privately during preparation", {
  skip_if_not_installed("labelled")
  script <- make_dependency_script(c(
    "prep_dependencies <- c('labelled')",
    "hh <- data.frame(value = 1)",
    "var_label(hh$value) <- 'Example label'",
    "hl <- data.frame(value = 2)"
  ))
  files <- file.path(dirname(script), c("hh.sav", "hl.sav"))
  file.create(files)
  search_before <- search()
  session <- mics_session()
  prepare_mics_data(session, files[1], files[2], script)
  expect_identical(attr(session$hh$value, "label"), "Example label")
  expect_identical(search(), search_before)
})

test_that("installation targets only unavailable prep packages", {
  script <- make_dependency_script("prep_dependencies <- c('availablePkg', 'missingPkg')")
  installed <- character()
  testthat::local_mocked_bindings(prep_package_problem = function(package) {
    if (package == "availablePkg" || package %in% installed) NA_character_ else "not installed"
  })
  testthat::local_mocked_bindings(install.packages = function(pkgs, ...) {
    installed <<- c(installed, pkgs)
  }, .package = "utils")
  result <- install_prep_dependencies(script)
  expect_identical(installed, "missingPkg")
  expect_true(all(result$available))
  install_prep_dependencies(script)
  expect_identical(installed, "missingPkg")
})

test_that("runtime namespace keeps preparation-only and unused imports separate", {
  imported <- names(getNamespaceImports("micsPlusTableR"))
  expect_false(any(c("DiagrammeR", "gt", "sjlabelled", "stringdist",
    "ggplot2", "Hmisc", "labelled", "memisc", "reshape2", "RM.weights",
    "survey", "srvyr", "knitr") %in% imported))
  expect_true(all(c("check_prep_dependencies", "install_prep_dependencies") %in%
    getNamespaceExports("micsPlusTableR")))
})


test_that("normal installation includes every maintained preparation dependency", {
  metadata <- utils::packageDescription("micsPlusTableR")
  packages <- function(field) {
    sub(" .*", "", trimws(strsplit(metadata[[field]], ",")[[1L]]))
  }
  prep <- packages("Config/micsPlusTableR/prep-packages")
  expect_setequal(prep, c("ggplot2", "Hmisc", "labelled", "memisc",
    "reshape2", "RM.weights", "survey", "srvyr"))
  expect_true(all(prep %in% packages("Imports")))
  expect_false(any(prep %in% packages("Suggests")))
  expect_false(any(c("DiagrammeR", "gt", "sjlabelled", "stringdist") %in%
    c(packages("Imports"), packages("Suggests"))))
})

test_that("required preparation namespaces load without attaching packages", {
  search_before <- search()
  packages <- c("ggplot2", "Hmisc", "labelled", "memisc", "reshape2",
    "RM.weights", "survey", "srvyr", "stats")
  for (package in packages) {
    expect_identical(load_preparation_namespace(package), asNamespace(package))
  }
  expect_identical(search(), search_before)
})
