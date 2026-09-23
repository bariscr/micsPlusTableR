test_that("frequency reports reproduce the supplied SPSS example", {
  ds <- data.frame(address = haven::labelled(c(rep(1, 2097), rep(2, 2), rep(NA_real_, 1370)),
    labels = c(YES = 1, NO = 2), label = "Still living at the address"))
  report <- micsPlusTableR:::mics_explorer_frequency(ds, "address")[[1]]
  expect_identical(report$label, "Still living at the address")
  expect_identical(report$statistics$Value[1:2], c("2,099", "1,370"))
  expect_equal(report$frequencies$Frequency, c(2097, 2, 2099, 1370, 3469))
  expect_equal(round(report$frequencies$Percent, 1), c(60.4, 0.1, 60.5, 39.5, 100))
  expect_equal(round(report$frequencies$ValidPercent, 1), c(99.9, 0.1, 100, NA, NA))
  expect_equal(round(report$frequencies$CumulativePercent, 1), c(99.9, 100, NA, NA, NA))
  ds$city <- c(401, 501, rep(NA_real_, 3467))
  stats <- micsPlusTableR:::mics_explorer_frequency(ds, "city")[[1]]$statistics
  expect_identical(stats$Value[stats$Statistic %in% c("Mean", "Median", "Std. Deviation", "Range")],
                   c("451.00", "451.00", "70.711", "100"))
})

test_that("reports distinguish user and system missing values and sort by codes", {
  ds <- data.frame(x = haven::labelled_spss(c(10, 2, 99, 98, NA_real_),
    labels = c(Two = 2, Ten = 10, Refused = 99), na_range = c(98, 99)))
  report <- micsPlusTableR:::mics_explorer_frequency(ds, "x")[[1]]
  expect_identical(report$frequencies$Value, c("Two (2)", "Ten (10)", "Total", "98", "Refused (99)", "System", ""))
  expect_equal(report$frequencies$ValidPercent[1:2], c(50, 50))
  expect_identical(report$statistics$Value[1:2], c("2", "3"))
  expect_true(any(grepl("Multiple modes", report$notes)))
  expect_identical(report$statistics$Value[report$statistics$Statistic == "Mode"], "2")
})

test_that("frequency weights govern all statistics and exclude invalid weights", {
  ds <- data.frame(x = c(1, 3, 99, 98, 97, NA), w = c(2, 2, -1, 0, NA, 1))
  report <- micsPlusTableR:::mics_explorer_frequency(ds, "x", "w")[[1]]
  expect_equal(report$frequencies$Frequency, c(2, 2, 4, 1, 5))
  expect_equal(report$frequencies$ValidPercent[1:2], c(50, 50))
  expect_identical(report$statistics$Value[1:6], c("4", "1", "2.00", "2.00", "1", "1.155"))
  expect_true(any(grepl("3 cases excluded", report$notes)))
  ds$w <- c(0.5, 1.5, 0, 0, 0, 0)
  report <- micsPlusTableR:::mics_explorer_frequency(ds, "x", "w")[[1]]
  expect_equal(report$frequencies$Frequency[1:2], c(0.5, 1.5))
  expect_identical(report$statistics$Value[report$statistics$Statistic == "Mean"], "2.50")
})

test_that("empty, categorical, and all-missing reports remain usable", {
  build <- micsPlusTableR:::mics_explorer_frequency
  expect_length(build(data.frame(x = 1), character()), 0)
  for (x in list(numeric(), c(NA_real_, NA_real_))) {
    report <- build(data.frame(x = x), "x")[[1]]
    expect_true(all(is.na(report$frequencies$ValidPercent)))
    expect_false(any(is.nan(report$frequencies$Percent)))
    expect_identical(report$statistics$Value[report$statistics$Statistic == "Mean"], "—")
  }
  report <- build(data.frame(x = c("B", "A", "A", NA)), "x")[[1]]
  expect_identical(report$statistics$Statistic, c("Valid", "Missing", "Mode"))
  expect_identical(report$statistics$Value, c("3", "1", "A"))
  report <- build(data.frame(x = 1:3), "x", statistics = "Median")[[1]]
  expect_identical(report$statistics$Statistic, c("Valid", "Missing", "Median"))
  report <- build(data.frame(x = 1:3), "x", statistics = character())[[1]]
  expect_identical(report$statistics$Statistic, c("Valid", "Missing"))
})

test_that("report HTML escapes labels and retains SPSS table headings", {
  ds <- data.frame(x = haven::labelled(1, label = "<script>alert(1)</script>"))
  reports <- micsPlusTableR:::mics_explorer_frequency(ds, "x")
  html <- as.character(micsPlusTableR:::mics_explorer_report_ui(reports))
  expect_match(html, "&lt;script&gt;")
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, "Valid Percent")
  expect_match(html, "Cumulative Percent")
  expect_match(html, "Statistics")
})

test_that("the explorer runs with one dataset and honors filters and the weighting toggle", {
  app_env <- new.env()
  app <- source(system.file("shinyapp", "app.R", package = "micsPlusTableR"), local = app_env)$value
  shiny::testServer(app, {
    hh_rv(data.frame(x = c(1, 2, NA), y = c(1, 1, 2), w = c(5, 2, 1)))
    session$setInputs(select_data = "hh", select_variable = "x", analysis_type = "frequency",
      select_weight_type = "unweighted_type", select_weight = "w",
      explorer_statistics = c("Mean", "Mode"), filter_area1 = "", filter_area2 = "",
      run_data_analysis = 1L)
    expect_identical(exploration_rv()$reports[[1]]$statistics$Value[1:2], c("2", "1"))
    expect_match(output$explorer_report$html, "Cumulative Percent")
    session$setInputs(select_weight_type = "weighted_type", run_data_analysis = 2L)
    expect_identical(exploration_rv()$reports[[1]]$statistics$Value[1:2], c("7", "1"))
    session$setInputs(select_weight_type = "unweighted_type", filter_area1 = "y == 1", run_data_analysis = 3L)
    expect_identical(exploration_rv()$reports[[1]]$statistics$Value[1:2], c("2", "0"))
    session$setInputs(select_variable = c("x", "y"), analysis_type = "cross-table", run_data_analysis = 4L)
    expect_null(exploration_rv())
    expect_equal(sum(freq_rv()$Frequency), 2)
    session$setInputs(analysis_type = "frequency", filter_area1 = "not_a_variable == 1", run_data_analysis = 5L)
    expect_null(exploration_rv())
    expect_null(freq_rv())
  })
})
