test_that("create_excel_workbooks copies requested targets without overwriting", {
  source <- tempfile(fileext = ".xlsx")
  wb <- openxlsx2::wb_workbook()$add_worksheet("Example")$add_worksheet("Preparation File")
  wb$add_data("Example", "placeholder", col_names = FALSE)
  wb$add_data("Preparation File", "hh <- data.frame()", col_names = FALSE)
  wb$save(source)
  out_dir <- tempfile("workbooks-")

  created <- create_excel_workbooks(
    path_tab_excel = source,
    output_dir = out_dir,
    country_code = "TST",
    period = "2026",
    wave = "Wave 1",
    target = "both",
    time_tag = "20260827010101"
  )

  expect_true(file.exists(created$output))
  expect_true(file.exists(created$formatted))
  expect_identical(unname(tools::md5sum(created$output)), unname(tools::md5sum(source)))
  expect_identical(readxl::excel_sheets(created$formatted), "Example")
  expect_true("Preparation File" %in% readxl::excel_sheets(source))

  created_again <- create_excel_workbooks(
    source, out_dir, "TST", "2026", "Wave 1", "both", "20260827010101"
  )
  expect_false(identical(created$output, created_again$output))
  expect_true(grepl("_2[.]xlsx$", created_again$output))
})

test_that("create_excel_workbooks validates inputs", {
  expect_error(
    create_excel_workbooks("missing.xlsx", tempdir(), "TST", "2026", "Wave 1"),
    "does not exist"
  )
})

test_that("removing preparation preserves named ranges and shifts later sheet scopes", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Before")$
    add_worksheet("Preparation File")$add_worksheet("After")
  names_before <- c(
    '<definedName name="keep" localSheetId="0">Before!$A$1</definedName>',
    '<definedName name="_xlnm.Print_Area" localSheetId="0">Before!#REF!</definedName>',
    '<definedName name="code" localSheetId="1">\'Preparation File\'!$A$1</definedName>',
    '<definedName name="later" localSheetId="2">After!$A$1</definedName>')
  wb$workbook$definedNames <- names_before
  micsPlusTableR:::remove_preparation_sheet(wb)
  expect_identical(unname(openxlsx2::wb_get_sheet_names(wb)), c("Before", "After"))
  expect_identical(wb$workbook$definedNames,
    c(names_before[1:2], sub('localSheetId="2"', 'localSheetId="1"', names_before[4])))
  path <- tempfile(fileext = ".xlsx")
  expect_no_error(wb$save(path))
  expect_identical(openxlsx2::wb_load(path)$workbook$definedNames, wb$workbook$definedNames)
})

test_that("removing both code sheets preserves names for remaining tables", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Before")$add_worksheet("Define")$
    add_worksheet("Preparation File")$add_worksheet("After")
  wb$workbook$definedNames <- c(
    '<definedName name="keep" localSheetId="0">Before!$A$1</definedName>',
    '<definedName name="definitions">Define!$A$1:$B$2</definedName>',
    '<definedName name="preparation">\'Preparation File\'!$A$1</definedName>',
    '<definedName name="later" localSheetId="3">After!$A$1</definedName>')
  micsPlusTableR:::remove_code_sheets(wb)
  expect_identical(unname(openxlsx2::wb_get_sheet_names(wb)), c("Before", "After"))
  expect_identical(wb$workbook$definedNames, c(
    '<definedName name="keep" localSheetId="0">Before!$A$1</definedName>',
    '<definedName name="later" localSheetId="1">After!$A$1</definedName>'))
})
