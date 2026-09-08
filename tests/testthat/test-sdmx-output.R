test_that("legacy engine cells become the 35-variable SDMX contract", {
  cells <- tibble::tibble(
    row_index = c(9L, 9L),
    col_index = c(3L, 4L),
    row_logic = "area == 1",
    col_logic = c("total == 1", "indicator == 1"),
    stat_type = c("p", "n_unw"),
    value = c(54.26, 30),
    n_unw = c(30, 30),
    row_header = "Urban",
    variable_exp = "Area",
    col_header = c("Percent", "Unweighted")
  )
  context <- list(
    is_supp = TRUE,
    header_1 = tibble::tibble(
      col_index = c(3L, 4L),
      label = c("Percent", "Unweighted")
    ),
    header_2 = tibble::tibble(
      label = "Indicator results",
      col_start = 3L,
      col_end = 4L
    )
  )
  config <- micsPlusTableR:::new_mics_sdmx_config(
    country = "Jamaica",
    ref_area = "JAM",
    period = "2023-24",
    wave = "Wave 1"
  )

  result <- as_sdmx_compatible(
    cells,
    config = config,
    table_id = "JW1.2.3",
    table_context = context
  )

  expect_identical(names(result), micsPlusTableR:::mics_sdmx_long_columns())
  expect_length(result, 35L)
  expect_false(any(c("table_id", "value", "estimate", "digits", "unw",
                     "ROW_ID", "COLUMN_ID", "COL_ID", "unweighted_n",
                     "row_id", "column_id", "unit_measure") %in%
                     names(result)))
  expect_identical(unique(result$SURVEY_ID), "JAM_MICSPLUS_2023-24_W1")
  expect_identical(unique(result$TIME_PERIOD), "2023-24")
  expect_identical(result$TABLE_ID, rep("JW1.2.3", 2L))
  expect_identical(result$OBS_VALUE, cells$value)
  expect_identical(result$DECIMALS, c(1L, 0L))
  expect_identical(result$denominator_n, cells$n_unw)
  expect_identical(result$row_order, cells$row_index)
  expect_identical(result$column_order, cells$col_index)
  expect_identical(result$row_labels, c("Area|Urban", "Area|Urban"))
  expect_identical(
    result$column_group,
    c("Indicator results", "Indicator results")
  )
  expect_identical(result$stat_type, c("Percent", "Count"))
  expect_identical(result$UNIT_MEASURE, c("PERCENT", "NUMBER"))
  expect_identical(result$suppression_status, c("parenthesize", "publish"))
  expect_identical(result$display_value, c("(54.3)", "30"))
  expect_invisible(validate_sdmx_compatible(result))
})

test_that("SDMX validation uses row and column order in observation keys", {
  cells <- tibble::tibble(
    row_index = 9L,
    col_index = 3L,
    stat_type = "n_unw",
    value = 10,
    row_header = "Total",
    variable_exp = "#Total",
    col_header = "Number"
  )
  config <- micsPlusTableR:::new_mics_sdmx_config(
    country = "Mongolia",
    ref_area = "MNG",
    period = "2025-26",
    wave = "Wave 2"
  )
  result <- as_sdmx_compatible(cells, config, table_id = "MW2.1.1")

  expect_error(
    validate_sdmx_compatible(dplyr::bind_rows(result, result)),
    "Duplicate SDMX-compatible observation keys"
  )
  for (id in c("row_order", "column_order")) {
    distinct <- result
    distinct[[id]] <- distinct[[id]] + 1L
    expect_invisible(validate_sdmx_compatible(dplyr::bind_rows(result, distinct)))
    for (missing_id in c("", "  ", NA_character_)) {
      invalid <- result
      invalid[[id]] <- missing_id
      expect_error(validate_sdmx_compatible(invalid),
                   paste0("Missing required observation fields in: ", id))
    }
  }
})

test_that("suppressible observations require an unweighted denominator", {
  cells <- tibble::tibble(
    row_index = 9L,
    col_index = 3L,
    stat_type = "p",
    value = 50,
    row_header = "Total",
    variable_exp = "#Total",
    col_header = "Percent"
  )
  config <- micsPlusTableR:::new_mics_sdmx_config(
    country = "Mongolia",
    ref_area = "MNG",
    period = "2025-26",
    wave = "Wave 1"
  )

  expect_error(
    as_sdmx_compatible(
      cells,
      config,
      table_id = "MW1.1.1",
      table_context = list(is_supp = TRUE)
    ),
    "no suppression denominator"
  )
})
