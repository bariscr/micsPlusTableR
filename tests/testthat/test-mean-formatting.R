test_that("mean previews use grouping and one decimal without rounding estimates", {
  estimate <- 442571.905044384
  for (direction in c("h", "v")) {
    s <- small_plan_session(direction)
    s$hh$expense <- estimate
    s$out_glob$tab$stat_type <- "mean(expense)"
    result <- tabulate_mics_table(s)
    expect_equal(result$value, estimate)
    expect_identical(result$value_f_view, "442,571.9")
    expect_identical(s$pivot_table(result, formatted = "view")[[2]], "442,571.9")
    expect_equal(s$pivot_table(result, formatted = FALSE)[[2]], estimate)

    s$hh$expense <- 1000
    expect_identical(tabulate_mics_table(s)$value_f_view, "1,000.0")
  }
})

test_that("unweighted and legacy mean displays also use fixed decimals", {
  s <- small_plan_session("v")
  s$hh$expense <- 442571.905044384
  s$out_glob$tab$stat_type <- "mean_unw(expense)"
  expect_identical(tabulate_v(s)$value_f_view, "442,571.9")

  for (stat in c("mean", "Mean")) {
    s <- small_plan_session("h")
    s$out_glob$tab$stat_type <- stat
    expect_identical(tabulate_h(s)$value_f_view, "100.0")
  }
})

test_that("supplied mean estimates receive the same formatting", {
  s <- small_plan_session("h")
  s$out_glob$tab$stat_type <- "mean(sex)"
  result <- tabulate_extra_table(s, data.frame(label = "Total", value = 442571.905044384))
  expect_identical(result$value_f_view, "442,571.9")
  expect_equal(result$value, 442571.905044384)
})
