survey_file_fixture <- function() {
  paths <- c("JAM_W1/JAM_W1_tabulation.xlsx",
             "JAM_W1/prep-files-JAM_W1/FIES-inputs/helper with spaces.R",
             "MNG_W2/MNG_W2_tabulation.xlsx")
  result <- data.frame(survey = c("JAM_W1", "JAM_W1", "MNG_W2"),
                       path = paths, size = rep(3, 3),
                       url = paste0("https://example.invalid/", seq_along(paths)))
  attr(result, "commit") <- strrep("a", 40)
  result
}

test_that("survey download helpers are exported", {
  expect_true(all(c("list_survey_files", "download_survey_files") %in%
                    getNamespaceExports("micsPlusTableR")))
})

test_that("listing discovers nested files and pins encoded URLs to a commit", {
  calls <- character()
  sha <- strrep("a", 40)
  entry <- function(path, type = "blob", mode = "100644") {
    list(path = path, type = type, mode = mode, size = 3)
  }
  local_mocked_bindings(survey_files_json = function(url) {
    calls <<- c(calls, url)
    if (length(calls) == 1) return(list(sha = sha))
    list(truncated = FALSE, tree = list(
      entry("README.md"), entry("JAM_W1", "tree", "040000"),
      entry("JAM_W1/.DS_Store"), entry("JAM_W1/.cache/a"),
      entry("JAM_W1/link", mode = "120000"),
      entry("JAM_W1/prep-files-JAM_W1/FIES helper #1%.R"),
      entry("MNG_W3/MNG_W3_tabulation.xlsx")
    ))
  })
  x <- list_survey_files(ref = "release/files")
  expect_equal(x$survey, c("JAM_W1", "MNG_W3"))
  expect_identical(attr(x, "commit"), sha)
  expect_match(calls[1], "release%2Ffiles", fixed = TRUE)
  expect_match(calls[2], paste0(sha, "?recursive=1"), fixed = TRUE)
  expect_match(x$url[1], "FIES%20helper%20%231%25.R", fixed = TRUE)
  expect_match(x$url[1], paste0("/", sha, "/"), fixed = TRUE)
})

test_that("incomplete and unsafe listings fail explicitly", {
  response <- list(truncated = TRUE, tree = list())
  local_mocked_bindings(survey_files_json = function(url) {
    if (grepl("/commits/", url)) return(list(sha = strrep("b", 40)))
    response
  })
  expect_error(list_survey_files(), "incomplete")
  for (path in c("JAM_W1/folder/../escape", "JAM_W1/a\\b")) {
    response <- list(truncated = FALSE, tree = list(
      list(type = "blob", mode = "100644", path = path, size = 3)))
    expect_error(list_survey_files(), "unsafe")
  }
  response <- list(truncated = FALSE, tree = list(
    list(type = "blob", mode = "100644", path = "JAM_W1/.DS_Store", size = 3)))
  expect_equal(nrow(list_survey_files()), 0)
  response <- list(truncated = FALSE, tree = list())
  expect_equal(nrow(list_survey_files()), 0)
  expect_error(download_survey_files(), "No published")
})

test_that("all, multiple surveys, folder names and individual files can be selected", {
  fixture <- survey_file_fixture()
  calls <- character()
  local_mocked_bindings(
    list_survey_files = function(...) fixture,
    survey_files_fetch = function(url, destfile, ...) {
      calls <<- c(calls, url)
      writeBin(as.raw(c(0, 128, 255)), destfile)
    }
  )
  for (selection in list(NULL, c("JAM_W1", "MNG_W2"), "JAM_W1")) {
    dest <- tempfile()
    x <- download_survey_files(selection, dest_dir = dest, quiet = TRUE)
    expected <- if (identical(selection, "JAM_W1")) 2L else 3L
    expect_equal(nrow(x), expected)
    expect_true(all(file.exists(file.path(dest, x$path))))
    expect_identical(attr(x, "commit"), attr(fixture, "commit"))
    expect_identical(readBin(x$local_path[1], "raw", 3), as.raw(c(0, 128, 255)))
  }
  calls <- character()
  x <- download_survey_files(files = fixture$path[2], dest_dir = tempfile(), quiet = TRUE)
  expect_equal(x$path, fixture$path[2])
  expect_identical(calls, fixture$url[2])
  expect_error(download_survey_files("BAD", dest_dir = tempfile()), "Unknown surveys")
  expect_error(download_survey_files("JAM_W1", files = fixture$path[3]), "Files not found")
})

test_that("existing inputs are protected and overwrite is deliberate", {
  fixture <- survey_file_fixture()[1, ]
  calls <- 0L
  local_mocked_bindings(
    list_survey_files = function(...) fixture,
    survey_files_fetch = function(url, destfile, ...) {
      calls <<- calls + 1L
      writeBin(charToRaw("new"), destfile)
    }
  )
  dest <- tempfile()
  target <- file.path(dest, fixture$path)
  dir.create(dirname(target), recursive = TRUE)
  writeBin(charToRaw("old"), target)
  expect_error(download_survey_files(dest_dir = dest), "already exist")
  expect_identical(calls, 0L)
  expect_identical(readChar(target, 3), "old")
  download_survey_files(dest_dir = dest, overwrite = TRUE, quiet = TRUE)
  expect_identical(readChar(target, 3), "new")
})

test_that("failed or short downloads do not change destination files", {
  fixture <- survey_file_fixture()
  calls <- 0L
  short <- FALSE
  local_mocked_bindings(
    list_survey_files = function(...) fixture,
    survey_files_fetch = function(url, destfile, ...) {
      calls <<- calls + 1L
      if (short) return(writeBin(charToRaw("x"), destfile))
      if (calls == 2L) stop("network failed")
      writeBin(charToRaw("new"), destfile)
    }
  )
  dest <- tempfile()
  target <- file.path(dest, fixture$path[1])
  dir.create(dirname(target), recursive = TRUE)
  writeBin(charToRaw("old"), target)
  expect_error(download_survey_files(dest_dir = dest, overwrite = TRUE), "network failed")
  expect_identical(readChar(target, 3), "old")
  expect_false(file.exists(file.path(dest, fixture$path[2])))
  short <- TRUE
  expect_error(download_survey_files(dest_dir = dest, overwrite = TRUE), "Incomplete download")
  expect_identical(readChar(target, 3), "old")
})

test_that("destination symlinks are rejected", {
  skip_on_os("windows")
  fixture <- survey_file_fixture()[1, ]
  local_mocked_bindings(list_survey_files = function(...) fixture)
  dest <- tempfile()
  outside <- tempfile()
  dir.create(dest)
  dir.create(outside)
  expect_true(file.symlink(outside, file.path(dest, "JAM_W1")))
  expect_error(download_survey_files(dest_dir = dest), "symbolic link")
})

test_that("argument errors happen before network access", {
  local_mocked_bindings(survey_files_json = function(...) stop("unexpected network"))
  expect_error(list_survey_files(repo = "bad"), "owner/name")
  expect_error(list_survey_files(ref = NA_character_), "non-empty string")
  expect_error(download_survey_files(surveys = character()), "non-empty character")
  expect_error(download_survey_files(files = NA_character_), "non-empty character")
  expect_error(download_survey_files(overwrite = NA), "TRUE or FALSE")
  expect_error(download_survey_files(dest_dir = ""), "non-empty string")
})

test_that("transport checks nonzero status and uses binary transfer with a timeout", {
  prior <- getOption("timeout")
  local_mocked_bindings(download.file = function(url, destfile, method, mode, quiet, headers) {
    expect_identical(mode, "wb")
    expect_identical(method, "libcurl")
    expect_gte(getOption("timeout"), 300)
    1L
  }, .package = "utils")
  expect_error(micsPlusTableR:::survey_files_fetch("https://example.invalid", tempfile()),
               "Could not download survey files")
  expect_identical(getOption("timeout"), prior)
})

test_that("only published materials are listed and downloadable in the new folders", {
  sha <- strrep("c", 40)
  paths <- c(
    "JAM_W1/Tabulation plan.xlsx",
    "JAM_W1/prep-files-JAM_W1/prep.R",
    "JAM_W1/prep-files-JAM_W1/FIES-inputs/reference.RData",
    "JAM_W1/JAM_W1_hh.sav", "JAM_W1/JAM_W1_hl.SAV",
    "JAM_W1/prep-files-JAM_W1/microdata.sav", "JAM_W1/households.csv",
    "JAM_W1/households.rds", "JAM_W1/households.zip",
    "input-files-JAM_W1/legacy.xlsx", "docs/guide.xlsx"
  )
  local_mocked_bindings(
    survey_files_json = function(url) {
      if (grepl("/commits/", url)) return(list(sha = sha))
      list(truncated = FALSE, tree = lapply(paths, function(path) {
        list(path = path, type = "blob", mode = "100644", size = 3)
      }))
    },
    survey_files_fetch = function(url, destfile, ...) {
      writeBin(charToRaw("new"), destfile)
    }
  )
  listed <- list_survey_files()
  expect_setequal(listed$path, paths[1:3])
  expect_equal(unique(listed$survey), "JAM_W1")
  downloaded <- download_survey_files("JAM_W1", dest_dir = tempfile(), quiet = TRUE)
  expect_setequal(downloaded$path, paths[1:3])
  expect_true(all(file.exists(downloaded$local_path)))
  expect_error(download_survey_files(files = paths[4]), "Files not found")
  expect_error(download_survey_files("input-files-JAM_W1"), "Unknown surveys")
})
