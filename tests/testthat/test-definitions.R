definition_plan <- function(code = "mutate(z = n()) |>\n mutate(z = z + 1)", id = "score") {
  path <- tempfile(fileext = ".xlsx")
  cells <- matrix(NA_character_, 10, 8)
  cells[1, 1] <- "Table 6.2 Example"
  cells[3, ] <- c("Rows", "Conditions", LETTERS[3:7], "IDX")
  cells[4, 2] <- "hh.sav\nfilter(total == 1)\nweight by w"
  cells[4, 3:6] <- c("define(score)\n- - -\nTRUE", "filter(sex == 2)\nTRUE",
    "unfilter()\nTRUE", "mutate(z = z + 10)\nTRUE")
  cells[9:10, 1] <- c("Total", "Second row")
  cells[9:10, 2] <- "TRUE"
  cells[9, 3:6] <- "mean(z)"
  cells[4, 7] <- "TRUE"
  cells[9, 7] <- "n_unw"
  wb <- openxlsx2::wb_workbook()$add_worksheet("6.2")$add_worksheet("IDX")$add_worksheet("Define")
  wb$add_data("6.2", cells, col_names = FALSE)
  wb$add_cell_style("6.2", dims = "A9:A10", horizontal = "left", indent = 1)
  wb$add_data("IDX", "Table 6.2 Example", col_names = FALSE)
  wb$add_data("Define", data.frame(id = id, code = code))
  wb$save(path)
  path
}

definition_values <- function(session) {
  tabulate_h(session) |> dplyr::filter(stat_type == "mean(z)") |>
    dplyr::arrange(row_index, col_index) |> dplyr::pull(value)
}

test_that("definitions support both pipes, multiple mutations and current filtered data", {
  hh <- data.frame(total = 1, w = 1, sex = c(1, 2, 2))
  for (pipe in c("|>", "%>%")) {
    path <- definition_plan(paste0("# shared code\nmutate(z = n()) ", pipe,
      "\n dplyr::mutate(z = z + 1)\nmutate(note = 'brackets ) ( and define(no)')"))
    s <- mics_session(hh = hh)
    read_mics_tabulation(s, path, "6.2")
    expect_equal(definition_values(s), rep(c(4, 3, 4, 14), 2))
    expect_identical(s$hh, hh)
    expect_equal(definition_values(s), rep(c(4, 3, 4, 14), 2))
  }
})

test_that("multiline Excel definitions accept LF, CRLF and CR line breaks", {
  hh <- data.frame(total = 1, w = 1, sex = c(1, 2, 2))
  compile <- micsPlusTableR:::mics_compile_definition
  for (pipe in c("|>", "%>%")) {
    lines <- c(
      "# a comment must end before the mutation",
      "mutate(",
      "  z = n()",
      paste0(") ", pipe),
      "mutate(z = z + 1)",
      "mutate(note = 'literal \\r\\n escapes')"
    )
    expected <- compile(paste(lines, collapse = "\n"))
    for (newline in c("\n", "\r\n", "\r")) {
      code <- paste(lines, collapse = newline)
      expect_identical(compile(code), expected)
      path <- definition_plan(code)
      s <- mics_session(hh = hh)
      read_mics_tabulation(s, path, "6.2")
      expect_equal(definition_values(s), rep(c(4, 3, 4, 14), 2))
      expect_identical(s$hh, hh)
    }
  }
})

test_that("definitions are re-read across tables and workbooks without stale code", {
  s <- mics_session(hh = data.frame(total = 1, w = 1, sex = c(1, 2, 2)))
  path <- definition_plan("mutate(z = 7)")
  wb <- openxlsx2::wb_load(path)
  wb$clone_worksheet("6.2", "6.3")$save(path, overwrite = TRUE)
  for (sheet in c("6.2", "6.3")) {
    read_mics_tabulation(s, path, sheet)
    expect_equal(definition_values(s), rep(c(7, 7, 7, 17), 2))
  }
  wb$add_data("Define", "mutate(z = 8)", dims = "B2", col_names = FALSE)$save(path, overwrite = TRUE)
  read_mics_tabulation(s, path, "6.2")
  expect_equal(definition_values(s), rep(c(8, 8, 8, 18), 2))
  read_mics_tabulation(s, definition_plan("mutate(z = 9)"), "6.2")
  expect_equal(definition_values(s), rep(c(9, 9, 9, 19), 2))
})

test_that("definitions mix with inline mutations and work in row cells", {
  path <- definition_plan("mutate(z = 1) |> mutate(z = z + 1)")
  wb <- openxlsx2::wb_load(path)
  wb$add_data("6.2", "mutate(z = 20)\ndefine(score)\nmutate(z = z + 3)\nTRUE",
    dims = "C4", col_names = FALSE)
  wb$add_data("6.2", "mutate(z = z + 1)\ndefine(score)\nmutate(z = z + 5)\nTRUE",
    dims = "B9", col_names = FALSE)$save(path, overwrite = TRUE)
  s <- mics_session(hh = data.frame(total = 1, w = 1, sex = c(1, 2, 2)))
  read_mics_tabulation(s, path, "6.2")
  expect_equal(definition_values(s), rep(7, 8))
})

test_that("definitions work in shared setup and vertical row conditions", {
  path <- definition_plan("mutate(z = 2) %>% mutate(z = z + 1)")
  wb <- openxlsx2::wb_load(path)
  wb$add_data("6.2", "hh.sav\nfilter(total == 1)\ndefine(\"score\")\nweight by w",
    dims = "B4", col_names = FALSE)
  wb$add_data("6.2", matrix("TRUE", 1, 5), dims = "C4", col_names = FALSE)
  wb$add_data("6.2", matrix("mean(z)", 1, 5), dims = "C9", col_names = FALSE)
  wb$add_data("6.2", "define(score)\nmutate(z = z + 2)\nTRUE",
    dims = "B9", col_names = FALSE)$save(path, overwrite = TRUE)
  s <- mics_session(hh = data.frame(total = 1, w = 1, sex = c(1, 2, 2)))
  read_mics_tabulation(s, path, "6.2")
  expect_identical(s$out_glob$tab_direction, "v")
  expect_equal(tabulate_v(s)$value, rep(5, 10))
})

test_that("definition failures identify the caller and source", {
  for (code in c("mutate(z = 1) |> filter(z == 1)",
                 "mutate(z = 1) %>% unfilter()", "define(other)", "hh |> mutate(z = 1)")) {
    path <- definition_plan(code)
    error <- tryCatch(read_mics_tabulation(mics_session(), path, "6.2"), error = identity)
    expect_s3_class(error, "error")
    expect_match(conditionMessage(error), "cell C4", fixed = TRUE)
    expect_match(conditionMessage(error), "Define!B2", fixed = TRUE)
  }
  s <- mics_session(hh = data.frame(total = 1, w = 1, sex = c(1, 2, 2)))
  read_mics_tabulation(s, definition_plan("mutate(z = nonexistent_variable)"), "6.2")
  error <- tryCatch(tabulate_h(s), error = identity)
  expect_match(conditionMessage(error), "cell C4", fixed = TRUE)
  expect_match(conditionMessage(error), "Define!B2", fixed = TRUE)
  expect_match(conditionMessage(error), "nonexistent_variable", fixed = TRUE)
  error <- tryCatch(read_mics_tabulation(s, definition_plan("mutate(z = 1", id = "score"), "6.2"), error = identity)
  expect_match(conditionMessage(error), "Define!B2", fixed = TRUE)
})

test_that("Define layout and references have actionable validation", {
  for (ids in list("other", c("score", "score"))) {
    path <- definition_plan("mutate(z = 1)", id = ids)
    expect_error(read_mics_tabulation(mics_session(), path, "6.2"),
      if (length(ids) == 1) "not found" else "duplicated")
  }
  path <- definition_plan()
  wb <- openxlsx2::wb_load(path)
  wb$remove_worksheet("Define")$save(path, overwrite = TRUE)
  expect_error(read_mics_tabulation(mics_session(), path, "6.2"), "needs a worksheet named 'Define'")
  wb$add_worksheet("Define")$add_data("Define", data.frame(id = "score", logic = "mutate(z=1)"))$
    save(path, overwrite = TRUE)
  expect_error(read_mics_tabulation(mics_session(), path, "6.2"), "A1 and B1")
  wb$add_data("Define", data.frame(id = "score", code = ""))$save(path, overwrite = TRUE)
  expect_error(read_mics_tabulation(mics_session(), path, "6.2"), "both an id and code")
  definitions <- data.frame(id = "score", code = "mutate(z = 1)", row = 2L)
  for (bad in c("define()", "define(score, other)", "define(score) |> mutate(z = 1)")) {
    expect_error(micsPlusTableR:::mics_parse_condition_cell(bad, 4, 3, "6.2", definitions),
      "define\\(id\\)")
  }
})

test_that("Define is excluded from table choices and formatted exports only", {
  path <- definition_plan()
  wb <- openxlsx2::wb_load(path)
  wb$add_worksheet("Preparation File")$add_data("Preparation File", "hh <- data.frame()", col_names = FALSE)$
    save(path, overwrite = TRUE)
  created <- create_excel_workbooks(path, tempfile(), "SEN", "2026", "Wave 1")
  expect_true(all(c("Define", "Preparation File") %in% readxl::excel_sheets(created$output)))
  expect_false(any(c("Define", "Preparation File") %in% readxl::excel_sheets(created$formatted)))
  old <- options(micsPlusTableR.output_dir = tempfile())
  on.exit(options(old), add = TRUE)
  app <- source(system.file("shinyapp", "app.R", package = "micsPlusTableR"), local = new.env())$value
  shiny::testServer(app, {
    session$setInputs(tab_file = data.frame(name = basename(path), datapath = path,
      size = file.info(path)$size, type = "application/octet-stream"))
    expect_identical(sheets_all_rv(), "6.2")
    expect_true("Define" %in% tab_sheets_rv())
  })
})

test_that("definition errors use the real notification and leave the app connected", {
  path <- definition_plan("mutate(z = nonexistent_variable)")
  old <- options(micsPlusTableR.output_dir = tempfile())
  on.exit(options(old), add = TRUE)
  app <- source(system.file("shinyapp", "app.R", package = "micsPlusTableR"), local = new.env())$value
  shiny::testServer(app, {
    # Capture only delivery; run the real showNotification and HTML processing.
    notifications <- list()
    session$sendNotification <- function(type, message) {
      notifications[[length(notifications) + 1L]] <<- message
    }
    session$setInputs(tab_file = data.frame(name = basename(path), datapath = path,
      size = file.info(path)$size, type = "application/octet-stream"))
    engine_env$hh <- data.frame(total = 1, w = 1, sex = c(1, 2, 2))
    session$setInputs(sheet_id = "6.2", run_tab = 1)
    expect_false(session$isClosed())
    expect_length(notifications, 1)
    expect_match(notifications[[1]]$html, "Define!B2", fixed = TRUE)
    expect_match(notifications[[1]]$html, "cell C4", fixed = TRUE)
    expect_match(notifications[[1]]$html, "nonexistent_variable", fixed = TRUE)
    session$setInputs(run_tab = 2)
    expect_false(session$isClosed())
    expect_length(notifications, 2)
  })
})
