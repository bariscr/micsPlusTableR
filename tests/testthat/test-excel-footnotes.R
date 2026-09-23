test_that("single-sheet and multi-sheet formatted exports include the same footnotes", {
  output_dir <- tempfile("excel-footnotes-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  old_options <- options(micsPlusTableR.output_dir = output_dir)
  on.exit(options(old_options), add = TRUE)

  paths <- file.path(output_dir, c("single.xlsx", "multi.xlsx", "output.xlsx"))
  for (path in paths) {
    openxlsx2::wb_workbook()$add_worksheet("Example")$save(path)
  }
  cells <- tibble::tibble(
    row_index = 9:11, col_index = 3L, stat_type = "p",
    value = c(42, 20, NaN), value_f = c("(42.0)", "(*)", "-")
  )
  context <- list(
    tab = cells, tab_org = cells, is_supp = TRUE, tab_direction = "h",
    condition_row_index = 4L,
    condition_write = tibble::tibble(row_index = 4L, col_index = 3L, value = "TRUE"),
    a_cells = tibble::tibble(row = 1L, col = 5L, character = "IDX")
  )
  app_env <- new.env()
  app <- source(system.file("shinyapp", "app.R", package = "micsPlusTableR"),
                local = app_env)$value
  errors <- character()
  app_env$showNotification <- function(ui, type, ...) {
    if (identical(type, "error")) errors <<- c(errors, as.character(ui))
  }

  shiny::testServer(app, {
    session$flushReact()
    engine_env$out_glob <- context
    sheet_rv("Example")
    cell_results_rv(cells)
    dest_f_rv(paths[1])
    dest_rv(paths[3])
    tab_path_rv(paths[3])
    session$setInputs(write_formatted_table = 1L, write_output_table = 1L)

    # Reuse the same calculated cells; exercise the real workbook writers in both UIs.
    server_env <- environment(build_cell_results_for_sheet)
    assign("build_cell_results_for_sheet", function(sheet) cells, envir = server_env)
    assign("run_checks_core", function(...) invisible(TRUE), envir = server_env)
    table_name_rv("Example table")
    dest_f_rv(paths[2])
    session$setInputs(sheet_ids_multi = "Example", write_target_all = "formatted",
                      run_all_btn = 1L)
    for (i in seq_len(10L)) {
      later::run_now(timeoutSecs = 0)
      session$flushReact()
      if (!isTRUE(run_all_running_rv())) break
    }
    expect_false(run_all_running_rv())
    expect_identical(run_all_results_rv()$status, "OK")
  })

  expect_identical(errors, character())
  notes <- c(
    "( ) Figures that are based on 25-49 unweighted cases",
    "(*) Figures that are based on fewer than 25 unweighted cases",
    "- denotes 0 unweighted cases in the denominator"
  )
  for (path in paths[1:2]) {
    written <- tidyxl::xlsx_cells(path, sheets = "Example")
    expect_identical(written$character[written$col == 1L & written$row %in% 12:14], notes)
  }
  raw <- tidyxl::xlsx_cells(paths[3], sheets = "Example")
  expect_false(any(raw$character %in% notes))
})

test_that("footnotes require eligibility and the corresponding exact display marker", {
  cases <- list(
    list(enabled = FALSE, values = c("(42.0)", "(*)", "-"), notes = character()),
    list(enabled = TRUE, values = c("-42.0", "50", "100"), notes = character()),
    list(enabled = TRUE, values = c("(42.0)", "50", "100"),
         notes = "( ) Figures that are based on 25-49 unweighted cases"),
    list(enabled = TRUE, values = c("42.0", "(*)", "100"),
         notes = "(*) Figures that are based on fewer than 25 unweighted cases"),
    list(enabled = TRUE, values = c("42.0", "50", "-"),
         notes = "- denotes 0 unweighted cases in the denominator")
  )
  for (case in cases) {
    s <- small_plan_session()
    cells <- tibble::tibble(row_index = 9:11, col_index = 3L, stat_type = "p",
                           value = c(42, 50, 100), value_f = case$values)
    s$out_glob$tab <- s$out_glob$tab_org <- cells
    s$out_glob$is_supp <- case$enabled
    s$out_glob$a_cells <- tibble::tibble(row = 1L, col = 5L, character = "IDX")
    path <- tempfile(fileext = ".xlsx")
    on.exit(unlink(path), add = TRUE)
    openxlsx2::wb_workbook()$add_worksheet("Example")$save(path)
    write_mics_footnotes(s, path, table = cells)
    written <- tidyxl::xlsx_cells(path, sheets = "Example")
    notes <- written$character[written$col == 1L & written$row >= 12L &
                                !is.na(written$character)]
    expect_identical(notes, case$notes)
  }
})
