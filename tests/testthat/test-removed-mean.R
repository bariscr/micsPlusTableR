test_that("capitalized mean instructions fail with migration guidance", {
  for (direction in c("h", "v")) {
    for (stat in c("Mean", "Mean (sex)", "Mean(sex)", "Mean legacy",
                   "Mean(d=2)", "Mean (sex, d=2)")) {
      s <- small_plan_session(direction)
      s$out_glob$tab$stat_type <- stat
      expect_error(tabulate_mics_table(s),
                   "Excel row 9, column 3.*removed in 0.4.0.*Use mean_unw",
                   info = paste(direction, stat))
    }
  }
  s <- small_plan_session("h")
  s$out_glob$tab$stat_type <- "Mean"
  expect_error(tabulate_extra_table(s, data.frame(label = "Total", value = 25)),
               "removed in 0.4.0")
})

test_that("unweighted indicators remain distinct from numeric averages", {
  for (direction in c("h", "v")) {
    s <- small_plan_session(direction)
    if (direction == "h") s$out_glob$tab_c$col_lgc <- "sex == 2" else
      s$out_glob$tab_r$row_lgc <- "sex == 2"
    s$out_glob$tab$stat_type <- "mean_unw(d=2)"
    result <- tabulate_mics_table(s)
    expect_equal(result$value, 200 / 3)
    expect_identical(result$value_f_view, "66.67")
    s$out_glob$tab$stat_type <- "mean_unw(sex)"
    expect_equal(tabulate_mics_table(s)$value, 2)
    s$out_glob$tab$stat_type <- "mean"
    expect_equal(tabulate_mics_table(s)$value, 500 / 6)
  }
})
