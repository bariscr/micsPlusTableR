invalid_instruction_workbook <- function() {
  cells <- matrix(NA_character_, 8, 4)
  cells[1, 1] <- "Table 6.2 Example"
  cells[1, 4] <- "IDX"
  cells[3, 1:3] <- c("Rows", "Conditions", "Indicator")
  cells[5, 2] <- "hh.sav\nfilter(total == 1)\nweight by w"
  cells[5, 3] <- paste("filter(sex == 1)", "- - -",
    "mutate(hhc = if_else(sex == 1, 1, 0)", "- - -", "hhc == 1", sep = "\n")
  cells[7, 1:3] <- c("Total", "total == 1", "n_unw")
  path <- tempfile(fileext = ".xlsx")
  wb <- openxlsx2::wb_workbook()$add_worksheet("6.2")$add_worksheet("IDX")
  wb$add_data("6.2", cells, col_names = FALSE)
  wb$add_data("IDX", "Table 6.2 Example", col_names = FALSE)
  wb$save(path)
  path
}

test_that("both reader entry points retain the cell and useful syntax error", {
  path <- invalid_instruction_workbook()
  for (reader in list(
    function() mics_session()$read_tabulation(path, "6.2"),
    function() read_mics_tabulation(mics_session(), path, "6.2")
  )) {
    error <- tryCatch(reader(), error = identity)
    expect_s3_class(error, "error")
    message <- micsPlusTableR:::mics_error_message(error)
    expect_match(message, "Worksheet '6.2', cell C5", fixed = TRUE)
    expect_match(message, "mutate() instruction is missing a closing parenthesis", fixed = TRUE)
    expect_match(message, "Add one ')' at the end.", fixed = TRUE)
    expect_false(grepl("purrr::map|In index:|In argument:", message))
  }
})

test_that("syntax hints do not mistake quotes and other syntax errors for missing parentheses", {
  parser <- micsPlusTableR:::mics_parse_condition_cell
  expect_silent(parser('mutate(x = ")")', 5, 27, "Example"))
  error <- tryCatch(parser('mutate(x = "unfinished)', 5, 27, "Example"), error = identity)
  message <- micsPlusTableR:::mics_error_message(error)
  expect_match(message, "cell AA5", fixed = TRUE)
  expect_match(message, "quotes", fixed = TRUE)
  expect_false(grepl("missing a closing parenthesis", message, fixed = TRUE))
  error <- tryCatch(parser("filter(sex == 1", 5, 2, "Example"), error = identity)
  expect_match(micsPlusTableR:::mics_error_message(error),
    "filter() instruction is missing a closing parenthesis", fixed = TRUE)
})

test_that("nested runtime errors expose their underlying cause", {
  error <- tryCatch(
    purrr::map(1, function(i) dplyr::mutate(data.frame(x = i), y = absent_variable)),
    error = identity
  )
  message <- micsPlusTableR:::mics_error_message(error)
  expect_match(message, "absent_variable", fixed = TRUE)
  expect_false(grepl("In index:|In argument:", message))
})

test_that("the app shows readable persistent errors for an invalid plan cell", {
  old <- options(micsPlusTableR.output_dir = tempfile("error-ui-"))
  on.exit(options(old), add = TRUE)
  path <- invalid_instruction_workbook()
  notices <- list()
  testthat::local_mocked_bindings(showNotification = function(ui, type, duration, ...) {
    notices[[length(notices) + 1L]] <<- list(
      html = as.character(ui), type = type, duration = duration)
  }, .package = "shiny")
  app <- source(system.file("shinyapp", "app.R", package = "micsPlusTableR"),
                local = new.env())$value
  shiny::testServer(app, {
    session$setInputs(tab_file = data.frame(name = basename(path),
      datapath = path, size = file.info(path)$size, type = "application/octet-stream"))
    session$setInputs(sheet_id = "6.2", run_tab = 1)
  })
  expect_length(notices, 1)
  expect_identical(notices[[1]]$type, "error")
  expect_null(notices[[1]]$duration)
  expect_match(notices[[1]]$html, "cell C5", fixed = TRUE)
  expect_match(notices[[1]]$html, "missing a closing parenthesis", fixed = TRUE)
  expect_match(notices[[1]]$html, "white-space: pre-wrap", fixed = TRUE)
  expect_false(grepl("purrr::map|In argument:", notices[[1]]$html))
})
