test_that("create_excel_workbooks copies requested targets without overwriting", {
  source <- tempfile(fileext = ".xlsx")
  writeBin(charToRaw("placeholder"), source)
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
  expect_identical(readBin(created$output, "raw", n = 20), charToRaw("placeholder"))

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

