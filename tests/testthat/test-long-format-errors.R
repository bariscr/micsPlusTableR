test_that("long-format errors show the table and the underlying indexed error", {
  output_dir <- tempfile("long-format-errors-")
  plan <- tempfile(fileext = ".xlsx")
  file.create(plan)
  on.exit(unlink(c(plan, output_dir), recursive = TRUE))
  old_options <- options(micsPlusTableR.output_dir = output_dir)
  on.exit(options(old_options), add = TRUE)
  app_env <- new.env()
  app <- source(system.file("shinyapp", "app.R", package = "micsPlusTableR"),
                local = app_env)$value
  notifications <- list()
  app_env$showNotification <- function(ui, type, duration = 5, ...) {
    notifications[[length(notifications) + 1L]] <<- list(
      text = as.character(ui), type = type, duration = duration
    )
  }

  shiny::testServer(app, {
    # Isolate reporting from workbook I/O and simulate a nested calculation error.
    server_env <- environment(build_cell_results_for_sheet)
    assign("ensure_current_workbook", function() invisible(NULL), envir = server_env)
    assign("read_tabulation", function(...) invisible(NULL), envir = server_env)
    assign("tabulate_mics", function(...) {
      purrr::map(1, function(i) stop("Required survey weight is missing."))
    }, envir = server_env)
    tab_path_rv(plan)
    sheets_all_rv("1.1a")
    first <- app_env$survey_choices[1L, ]
    session$setInputs(select_country = first$label, select_wave = first$wave)
    session$setInputs(run_long_format_data = 1L)
  })

  error <- notifications[[length(notifications)]]
  expect_identical(error$type, "error")
  expect_null(error$duration)
  expect_match(error$text, "table '1.1a'", fixed = TRUE)
  expect_match(error$text, "calculating cells", fixed = TRUE)
  expect_match(error$text, "index: 1", fixed = TRUE)
  expect_match(error$text, "Required survey weight is missing.", fixed = TRUE)
})
