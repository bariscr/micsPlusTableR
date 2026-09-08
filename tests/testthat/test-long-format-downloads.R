test_that("long-format downloads export the completed dataset in all formats", {
  cells <- tibble::tibble(
    row_index = c(9L, 10L), col_index = 3L, stat_type = "p",
    value = c(54.26, NA_real_), n_unw = c(30, 60),
    row_header = c('Café, "Urban"', "Rural\narea"),
    variable_exp = "Area", col_header = "Percent"
  )
  config <- micsPlusTableR:::new_mics_sdmx_config(
    "Mongolia", "MNG", "2025-26", "Wave 2"
  )
  expected <- as_sdmx_compatible(cells, config, table_id = "MW2.1.1")
  expect_identical(unique(expected$SURVEY_ID), "MNG_MICSPLUS_2025-26_W2")
  expect_identical(unique(expected$TIME_PERIOD), "2025-26")
  server <- function(input, output, session) {
    completed <- shiny::reactiveVal(NULL)
    completed_metadata <- shiny::reactiveVal(NULL)
    micsPlusTableR:::mics_long_format_downloads(output, completed, completed_metadata)
  }

  shiny::testServer(server, {
    expect_identical(output$long_format_download_ready, "false")
    completed_metadata(config)
    completed(expected)
    session$flushReact()
    expect_identical(output$long_format_download_ready, "true")

    rds_file <- output$download_long_format_rds
    expected_stem <- paste0("MNG MICSPlus 2025-26 Wave2_LongFormatData_",
                            format(Sys.Date(), "%Y%m%d"))
    expect_identical(basename(rds_file), paste0(expected_stem, ".rds"))
    expect_identical(readRDS(rds_file), expected)

    csv_file <- output$download_long_format_csv
    expect_identical(basename(csv_file), paste0(expected_stem, ".csv"))
    csv <- readr::read_csv(csv_file, show_col_types = FALSE)
    expect_identical(names(csv), names(expected))
    expect_equal(nrow(csv), nrow(expected))
    expect_identical(csv$row_label, expected$row_label)
    expect_identical(csv$OBS_VALUE, expected$OBS_VALUE)
    expect_identical(csv$denominator_n, expected$denominator_n)
    expect_identical(csv$suppression_status, expected$suppression_status)

    excel_file <- output$download_long_format_excel
    expect_identical(basename(excel_file), paste0(expected_stem, ".xlsx"))
    expect_identical(readxl::excel_sheets(excel_file), "Long Format Data")
    excel <- readxl::read_excel(excel_file)
    expect_identical(names(excel), names(expected))
    expect_equal(nrow(excel), nrow(expected))
    expect_identical(excel$row_label, expected$row_label)
    expect_identical(excel$OBS_VALUE, expected$OBS_VALUE)
    expect_identical(excel$denominator_n, expected$denominator_n)
    expect_identical(excel$suppression_status, expected$suppression_status)

    completed(expected[0, ])
    session$flushReact()
    expect_identical(output$long_format_download_ready, "false")
  })
})
