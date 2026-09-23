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
    table_context_rv(context)
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
    sheet <- openxlsx2::wb_load(path)$worksheets[[1]]
    columns <- sheet$unfold_cols()
    expect_identical(columns$width[columns$min == "2"], "1")
    rows <- sheet$sheet_data$row_attr
    expect_identical(rows$ht[rows$r == "4"], "3")
    expect_true(rows$hidden[rows$r == "4"] %in% c("", "0", "false"))
  }
  raw <- tidyxl::xlsx_cells(paths[3], sheets = "Example")
  expect_false(any(raw$character %in% notes))
})

test_that("only suppression footnotes require eligibility", {
  cases <- list(
    list(enabled = FALSE, values = c("(42.0)", "(*)", "-"),
         notes = "- denotes 0 unweighted cases in the denominator"),
    list(enabled = FALSE, values = c("(42.0)", "(*)", "100"), notes = character()),
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

test_that("rewriting an exempt table removes obsolete generated notes only", {
  s <- small_plan_session()
  cells <- tibble::tibble(row_index = 9:11, col_index = 3L, stat_type = "p",
    value = c(42, 20, NaN), value_f = c("(42.0)", "(*)", "-"))
  s$out_glob$tab <- s$out_glob$tab_org <- cells
  s$out_glob$a_cells <- tibble::tibble(row = 1L, col = 5L, character = "IDX")
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  wb <- openxlsx2::wb_workbook()$add_worksheet("Example")
  wb$add_data("Example", "Original explanatory footnote", dims = "A16")$save(path)
  s$out_glob$is_supp <- TRUE
  write_mics_footnotes(s, path, table = cells)
  s$out_glob$is_supp <- FALSE
  write_mics_footnotes(s, path, table = cells)
  written <- tidyxl::xlsx_cells(path, sheets = "Example")
  expect_identical(written$character[grepl("unweighted cases", written$character)],
                   "- denotes 0 unweighted cases in the denominator")
  expect_identical(written$character[written$address == "A16"],
                   "Original explanatory footnote")
})

test_that("conditional footnotes extend authored notes without an intervening border", {
  for (enabled in c(FALSE, TRUE)) {
    s <- small_plan_session()
    cells <- tibble::tibble(row_index = 9:11, col_index = 3L, stat_type = "p",
      value = c(42, 20, NaN), value_f = c("(42.0)", "(*)", "-"))
    if (!enabled) {
      cells$value <- c(42, 20, 100)
      cells$value_f <- as.character(cells$value)
    }
    s$out_glob$tab <- s$out_glob$tab_org <- cells
    s$out_glob$is_supp <- enabled
    s$out_glob$a_cells <- tibble::tibble(row = 1L, col = 5L, character = "IDX")
    path <- tempfile(fileext = ".xlsx")
    on.exit(unlink(path), add = TRUE)
    wb <- openxlsx2::wb_workbook()$add_worksheet("Example")
    wb$add_data("Example", "Original footnote", dims = "A12")
    wb$add_data("Example", "Another footnote", dims = "B13")
    wb$add_border("Example", "A13:C13", bottom_border = "thin",
                  top_border = NULL, left_border = "double", right_border = NULL)
    wb$add_border("Example", "A14:C14", top_border = "thin",
                  bottom_border = NULL, left_border = NULL, right_border = NULL)
    wb$save(path)

    # A second write should keep the same placement and closing rule.
    for (iteration in 1:2) {
      write_mics_footnotes(s, path, table = cells)
      written <- tidyxl::xlsx_cells(path, sheets = "Example")
      borders <- tidyxl::xlsx_formats(path)$local$border
      border_style <- function(addresses, side) {
        borders[[side]]$style[written$local_format_id[match(addresses, written$address)]]
      }
      expect_identical(written$character[written$address == "A12"], "Original footnote")
      expect_identical(written$character[written$address == "B13"], "Another footnote")
      expect_identical(border_style("A13", "left"), "double")
      for (edge in list(list(row = 13L, side = "bottom"), list(row = 14L, side = "top"))) {
        styles <- border_style(paste0(LETTERS[1:3], edge$row), edge$side)
        if (enabled) {
          expect_true(all(is.na(styles) | styles == "none"))
        } else {
          expect_identical(styles, rep("thin", 3))
        }
      }
      if (enabled) {
        expect_identical(written$row[grepl("unweighted cases", written$character)], 14:16)
        expect_identical(border_style(paste0(LETTERS[1:3], 17), "top"), rep("thin", 3))
        expect_identical(border_style(paste0("D", 12:16), "left"), rep("thin", 5))
      }
    }
  }
})

test_that("conditional notes reset indentation and height while authored notes retain taller rows", {
  for (enabled in c(FALSE, TRUE)) {
    for (default_height in c(8, 16)) {
      s <- small_plan_session()
      cells <- tibble::tibble(row_index = 9:11, col_index = 3L, stat_type = "p",
        value = c(42, 20, NaN), value_f = c("(42.0)", "(*)", "-"))
      if (!enabled) {
        cells$value <- c(42, 20, 100)
        cells$value_f <- as.character(cells$value)
      }
      s$out_glob$tab <- s$out_glob$tab_org <- cells
      s$out_glob$is_supp <- enabled
      s$out_glob$a_cells <- tibble::tibble(row = 1L, col = 5L, character = "IDX")
      path <- tempfile(fileext = ".xlsx")
      on.exit(unlink(path), add = TRUE)
      wb <- openxlsx2::wb_workbook()$add_worksheet("Example")
      wb$worksheets[[1]]$sheetFormatPr <- sprintf(
        '<sheetFormatPr defaultRowHeight="%s"/>', default_height)
      wb$add_data("Example", "Existing note", dims = "A16")
      wb$add_data("Example", "Wrapped\nexisting note", dims = "C17")
      wb$add_cell_style("Example", dims = "C17", wrap_text = TRUE, indent = 2)
      wb$add_cell_style("Example", dims = "A20:A22", wrap_text = TRUE, indent = 3)
      wb$add_data("Example", "Note with inherited height", dims = "A18")
      wb$add_data("Example", "Note at the minimum", dims = "A19")
      wb$set_row_heights("Example", rows = c(20:22, 16:17, 19, 23),
                        heights = c(1, 15, 24, 8, 30, 11.25, 2))
      wb$save(path)
      before <- openxlsx2::wb_load(path)$worksheets[[1]]$sheet_data$row_attr

      write_mics_footnotes(s, path, table = cells)

      after <- openxlsx2::wb_load(path)$worksheets[[1]]$sheet_data$row_attr
      expect_equal(as.numeric(after$ht[match(c(16:17, 19), after$r)]),
                   c(11.25, 30, 11.25))
      if (enabled) {
        expect_equal(as.numeric(after$ht[match(20:22, after$r)]), rep(11.25, 3))
      }
      if (default_height < 11.25) {
        expect_equal(as.numeric(after$ht[match(18, after$r)]), 11.25)
      }
      unchanged <- c(17, 19, 23,
                     if (!enabled) 20:22,
                     if (default_height >= 11.25) 18)
      expect_equal(after[match(unchanged, after$r), ],
                   before[match(unchanged, before$r), ], ignore_attr = TRUE)
      written <- tidyxl::xlsx_cells(path, sheets = "Example")
      expect_identical(written$character[written$address == "C17"], "Wrapped\nexisting note")
      formats <- tidyxl::xlsx_formats(path)
      expect_true(formats$local$alignment$wrapText[
        written$local_format_id[written$address == "C17"]])
      expect_equal(formats$local$alignment$indent[
        written$local_format_id[written$address == "C17"]], 2)
      conditional_formats <- written$local_format_id[match(paste0("A", 20:22), written$address)]
      expect_equal(formats$local$alignment$indent[conditional_formats],
                   rep(if (enabled) 0 else 3, 3))
      expect_identical(formats$local$alignment$wrapText[conditional_formats],
                       rep(!enabled, 3))
    }
  }
})

test_that("both Excel tabs write identical cells and styles after another sheet runs", {
  output_dir <- tempfile("excel-parity-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  old_options <- options(micsPlusTableR.output_dir = output_dir)
  on.exit(options(old_options), add = TRUE)
  paths <- file.path(output_dir, c("single-output.xlsx", "single-formatted.xlsx",
                                  "multi-output.xlsx", "multi-formatted.xlsx"))
  for (path in paths) {
    openxlsx2::wb_workbook()$add_worksheet("Exempt")$add_worksheet("Eligible")$save(path)
  }
  results <- list(
    Exempt = tibble::tibble(row_index = 9:11, col_index = 3L,
      stat_type = "mean_unw(x)", value = c(1.5, 6, NaN),
      value_f = c("1.5", "6", "-"), value_f_view = c("1.5", "6.0", "-")),
    Eligible = tibble::tibble(row_index = 15:17, col_index = 3L,
      stat_type = "p", value = c(42, 20, NaN),
      value_f = c("(42.0)", "(*)", "-"), value_f_view = c("(42.0)", "(*)", "-"))
  )
  contexts <- lapply(names(results), function(sheet) {
    condition_row <- if (sheet == "Exempt") 4L else 5L
    list(tab = results[[sheet]], tab_org = results[[sheet]],
      is_supp = sheet == "Eligible", tab_direction = if (sheet == "Exempt") "v" else "h",
      condition_row_index = condition_row,
      condition_write = tibble::tibble(row_index = condition_row, col_index = 3L, value = "TRUE"),
      a_cells = tibble::tibble(row = 1L, col = 5L, character = "IDX"))
  })
  names(contexts) <- names(results)
  app_env <- new.env()
  app <- source(system.file("shinyapp", "app.R", package = "micsPlusTableR"),
                local = app_env)$value
  errors <- character()
  app_env$showNotification <- function(ui, type, ...) {
    if (identical(type, "error")) errors <<- c(errors, as.character(ui))
  }
  shiny::testServer(app, {
    session$flushReact()
    server_env <- environment(build_cell_results_for_sheet)
    assign("build_cell_results_for_sheet", function(sheet) {
      engine_env$out_glob <- contexts[[sheet]]
      table_name_rv(sheet)
      results[[sheet]]
    }, envir = server_env)
    assign("run_checks_core", function(...) invisible(TRUE), envir = server_env)
    tab_path_rv(paths[1])
    dest_rv(paths[3])
    dest_f_rv(paths[4])
    session$setInputs(sheet_ids_multi = names(results), write_target_all = "both",
                       run_all_btn = 1L)
    for (i in seq_len(20L)) {
      later::run_now(timeoutSecs = 0)
      session$flushReact()
      if (!isTRUE(run_all_running_rv())) break
    }
    expect_false(run_all_running_rv())
    expect_true(all(run_all_results_rv()$status == "OK"))
    dest_rv(paths[1])
    dest_f_rv(paths[2])
    for (i in seq_along(results)) {
      sheet <- names(results)[i]
      session$setInputs(sheet_id = sheet, pivot_type = "index", format_type = "view", run_tab = i)
      # A subsequent batch/long-format calculation overwrites the shared plan.
      other <- setdiff(names(results), sheet)
      get("build_cell_results_for_sheet", envir = server_env)(other)
      session$setInputs(write_output_table = i, write_formatted_table = i)
      expect_identical(engine_env$out_glob, contexts[[other]])
    }
  })
  expect_identical(errors, character())
  read_written <- function(path, sheet) {
    cells <- tidyxl::xlsx_cells(path, sheets = sheet)
    cells$numfmt <- tidyxl::xlsx_formats(path)$local$numFmt[cells$local_format_id]
    cells[c("address", "character", "numeric", "numfmt")]
  }
  for (sheet in names(results)) {
    expect_equal(read_written(paths[1], sheet), read_written(paths[3], sheet))
    expect_equal(read_written(paths[2], sheet), read_written(paths[4], sheet))
  }
  exempt <- read_written(paths[2], "Exempt")
  expect_equal(exempt$numeric[exempt$address == "C10"], 6)
  expect_identical(exempt$character[exempt$address == "C11"], "-")
  expect_identical(exempt$character[grepl("unweighted cases", exempt$character)],
                   "- denotes 0 unweighted cases in the denominator")
  eligible <- read_written(paths[2], "Eligible")
  expect_identical(eligible$character[eligible$address == "C16"], "(*)")
  expect_identical(eligible$numfmt[eligible$address == "C15"], "(#,##0.0)")
  expect_equal(sum(grepl("unweighted cases", eligible$character), na.rm = TRUE), 3)
})
