test_that("d changes displays across statistic types without changing estimates", {
  for (direction in c("h", "v")) {
    for (stat in c("n", "n_unw", "p", "mean", "mean_unw", "mean(sex)",
                   "mean_unw(sex)", "median(sex)", "100")) {
      s <- small_plan_session(direction)
      s$out_glob$tab$stat_type <- stat
      baseline <- tabulate_mics_table(s)
      for (d in c(0L, 2L, 3L)) {
        spec <- if (endsWith(stat, ")")) sub("\\)$", paste0(", d=", d, ")"), stat) else
          paste0(stat, "(d=", d, ")")
        s$out_glob$tab$stat_type <- spec
        result <- tabulate_mics_table(s)
        expect_equal(result$value, baseline$value, info = spec)
        expect_identical(result$stat_type, baseline$stat_type, info = spec)
        expect_identical(result$display_digits, d, info = spec)
        expect_identical(s$pivot_table(result, formatted = FALSE)[[2]], result$value)
      }
    }
  }
  s <- small_plan_session("h")
  s$out_glob$tab$stat_type <- "n(d=2)"
  expect_identical(tabulate_h(s)$value_f_view, "6.00")
  s$out_glob$tab$stat_type <- "mean(sex, d=3)"
  expect_identical(tabulate_h(s)$value_f_view, "1.833")
  s$out_glob$tab$stat_type <- "mean(d=0)"
  expect_identical(tabulate_h(s)$value_f_view, "100")
})

test_that("precision parsing preserves statistic arguments and rejects invalid d", {
  parse <- micsPlusTableR:::mics_stat_spec
  expect_identical(parse("mean(HCS8,   d=0)")$calculation, "mean(HCS8)")
  expect_identical(parse("p(100, d=2)")$calculation, "p(100)")
  expect_identical(parse("n_unw(100, d=0)")$calculation, "n_unw(100)")
  expect_identical(parse("100(d=3)")$calculation, "100")
  expect_identical(parse("mean(d=2, sex)")$calculation, "mean(sex)")
  expect_true(is.na(parse("mean(sex)")$digits))
  for (stat in c("p(d=-1)", "n(d=1.5)", "mean(sex,d=NA)", "p(d=Inf)",
                 "n(d='2')", "n(d=2,d=3)", "p(d=1+1)", "n(d=sex)", "n(d=)")) {
    s <- small_plan_session("h")
    s$out_glob$tab$stat_type <- stat
    expect_error(tabulate_h(s), "Excel row 9, column 3.*non-negative whole number", info = stat)
  }
})

test_that("derived and unweighted means keep their calculation semantics", {
  s <- small_plan_session("v")
  s$out_glob$tab$stat_type <- "mean(derived = pmax(sex, 1) * 1000, d=2)"
  result <- tabulate_v(s)
  expect_equal(result$value, 11000/6)
  expect_identical(result$value_f_view, "1,833.33")
  expect_false("derived" %in% names(s$hh))
  s$out_glob$tab$stat_type <- "mean_unw(sex, d=2)"
  expect_identical(tabulate_v(s)$value_f_view, "1.67")
  for (stat in c("p(100, d=2)", "n_unw(100, d=2)")) {
    s$out_glob$tab$stat_type <- stat
    expect_identical(tabulate_v(s)$value_f_view, "100.00")
  }
  s$out_glob$tab$stat_type <- "Mean (sex, d=2)"
  expect_identical(tabulate_v(s)$value_f_view, "100.00")
})

test_that("supplied extra tables and missing groups retain precision metadata", {
  s <- small_plan_session("h")
  s$out_glob$tab$stat_type <- "mean(sex, d=0)"
  result <- tabulate_extra_table(s, data.frame(label = "Total", value = 442571.905044384))
  expect_identical(result$value_f_view, "442,572")
  expect_equal(result$value, 442571.905044384)
  s$out_glob$tab_r$row_lgc <- "sex == 99"
  result <- tabulate_h(s)
  expect_true(is.nan(result$value))
  expect_identical(result$value_f_view, "-")
})

test_that("long-format decimals follow overrides without changing observation values", {
  cells <- tibble::tibble(row_index = 9L, col_index = 3:5,
    stat_type = c("mean(x, d=0)", "p(d=2)", "n(d=3)"),
    value = c(442571.905044384, 73.3123, 881.34567),
    n_unw = 60, row_header = "Total", col_header = "Statistic")
  config <- micsPlusTableR:::new_mics_sdmx_config(country = "Mongolia", ref_area = "MNG",
                                                period = "2025-26", wave = "Wave 2")
  result <- as_sdmx_compatible(cells, config = config, table_id = "Example",
                              table_context = list(is_supp = FALSE))
  expect_identical(result$DECIMALS, c(0L, 2L, 3L))
  expect_identical(result$OBS_VALUE, cells$value)
  expect_identical(result$display_value, c("442572", "73.31", "881.346"))
})
