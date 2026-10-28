fies_test_result <- function() {
  data.frame(
    group = c("Total", "headlang", "headethnic"),
    category = c("Total", "Wolof", "Wolof"),
    p_mod = c(0.4, 0.25, NA_real_),
    p_sev = c(0.1, 0.05, NA_real_),
    count_w = c(100, 60, 40), count_unw = c(80, 50, 30)
  )
}

fies_test_context <- function(script_code = NULL, filename = "custom.script.R") {
  folder <- file.path(tempfile("fies-table-"), "FIES-inputs")
  dir.create(folder, recursive = TRUE)
  hh <- tempfile("arbitrary-household-name-")
  writeLines("uploaded household", hh)
  env <- new.env(parent = environment())
  env$run_fies_table <- run_fies_table
  env$.mics_fies_paths <- list(inputs = folder, hh = hh,
    output = file.path(folder, "reports"), survey_label = "Custom survey")
  env$hh <- data.frame(id = 101)
  env$hl <- data.frame(id = 202)
  if (!is.null(script_code)) writeLines(script_code, file.path(folder, filename))
  env
}

test_that("FIES table conversion preserves row order, group distinctions and missing estimates", {
  result <- micsPlusTableR:::fies_result_table(fies_test_result())
  expect_identical(names(result), c("stub", "Moderate food insecurity",
    "P_sev", "P_mod", "count_w", "count_unw"))
  expect_identical(result$stub, c("Total", "Wolof", "Wolof"))
  expect_equal(result$`Moderate food insecurity`, c(30, 20, NA_real_))
  expect_equal(result$P_sev, c(10, 5, NA_real_))
  expect_equal(result$P_mod, c(40, 25, NA_real_))
  expect_equal(result$count_w, c(100, 60, 40))
  expect_equal(result$count_unw, c(80, 50, 30))
  expect_true(all(vapply(result[-1L], is.numeric, logical(1))))
})

test_that("FIES table conversion rejects malformed output without changing its meaning", {
  convert <- micsPlusTableR:::fies_result_table
  expect_error(convert(NULL), "create a nonempty data frame")
  expect_error(convert(fies_test_result()[FALSE, ]), "nonempty")
  x <- fies_test_result()
  x$count_w <- NULL
  expect_error(convert(x), "missing columns: count_w")
  x <- fies_test_result()
  x$p_mod <- as.character(x$p_mod)
  expect_error(convert(x), "p_mod must be numeric")
  x <- fies_test_result()
  x$p_mod[1] <- 40
  expect_error(convert(x), "proportions between 0 and 1")
  x <- fies_test_result()
  x$p_sev[1] <- 0.9
  expect_error(convert(x), "cannot exceed p_mod")
  for (bad in c(NA_real_, Inf, -1)) {
    x <- fies_test_result()
    x$count_unw[1] <- bad
    expect_error(convert(x), "finite, nonnegative counts")
  }
  x <- fies_test_result()
  x$group[3] <- "headlang"
  expect_error(convert(x), "duplicate group/category pairs")
  x <- fies_test_result()
  x$category[1] <- " "
  expect_error(convert(x), "nonempty character labels")
})

test_that("FIES script discovery requires one main script and a preparation context", {
  expect_error(run_fies_table(), "inside Preparation File")
  env <- fies_test_context()
  expect_error(evalq(run_fies_table(), env), "found 0")
  writeLines("", file.path(env$.mics_fies_paths$inputs, "first.script.R"))
  writeLines("", file.path(env$.mics_fies_paths$inputs, "second_Script.R"))
  expect_error(evalq(run_fies_table(), env), "found 2")
})

test_that("FIES scripts receive local upload paths and cannot replace preparation datasets", {
  for (filename in c("custom.script.R", "ANY SURVEY_Script.R", "mixed.ScRiPt.r")) {
    env <- fies_test_context(c(
      "stopifnot(identical(get0('.mics_fies_paths', inherits = FALSE)$hh, hh_path))",
      "stopifnot(identical(readLines(hh_path), 'uploaded household'))",
      "writeLines(.mics_fies_paths$survey_label, file.path(path_fies_output, 'report.txt'))",
      "hh <- data.frame(id = -1)",
      "hl <- data.frame(id = -2)",
      "fies_result <- fies_test_result()"
    ), filename)
    # An unrelated helper is not a second main script.
    writeLines("", file.path(env$.mics_fies_paths$inputs, "other-helper.R"))
    result <- evalq(run_fies_table(), env)
    expect_equal(result$P_mod[1], 40)
    expect_equal(env$hh$id, 101)
    expect_equal(env$hl$id, 202)
    expect_identical(readLines(file.path(env$.mics_fies_paths$output, "report.txt")),
                     "Custom survey")
    expect_false(exists("fies_result", envir = env, inherits = FALSE))
  }
  env <- fies_test_context(c("hh <- NULL", "hl <- NULL", "stop('model failed')"))
  expect_error(evalq(run_fies_table(), env), "custom.script.R.*failed[[:space:][:punct:]]+model failed")
  expect_equal(env$hh$id, 101)
  expect_equal(env$hl$id, 202)
  env <- fies_test_context("invisible(NULL)")
  env$fies_result <- fies_test_result()
  expect_error(evalq(run_fies_table(), env), "must create a nonempty data frame")
})

test_that("a short embedded FIES call stages helpers and registers any table object name", {
  env <- fies_test_context(c(
    "stopifnot(!is.null(get0('.mics_fies_paths', inherits = FALSE)))",
    "hh <- NULL; hl <- NULL",
    "fies_result <- data.frame(group = 'Total', category = 'Total',",
    "  p_mod = 0.4, p_sev = 0.1, count_w = 100, count_unw = 80)"
  ))
  folder <- env$.mics_fies_paths$inputs
  writeLines("invisible(dplyr::n_distinct(1))", file.path(folder, "dependency.R"))
  for (call in c("run_fies_table()", "micsPlusTableR::run_fies_table()")) {
    plan <- preparation_plan(code = c(
      "hh <- data.frame(id = 101)", "hl <- data.frame(id = 202)",
      paste0("custom_food_table <- ", call),
      "extra_tables_dict <- list('X9' = 'custom_food_table')",
      "tables_extra <- names(extra_tables_dict)"
    ))
    expect_true(micsPlusTableR:::read_preparation_code(plan)$uses_fies)
    expect_error(check_prep_dependencies(plan), "uses FIES helpers")
    expect_true("dplyr" %in% check_prep_dependencies(plan, prep_folder = folder)$package)
    session <- mics_session()
    prepare_mics_data(session, env$.mics_fies_paths$hh, env$.mics_fies_paths$hh,
      plan, output_dir = tempfile(), prep_folder = folder)
    expect_equal(session$hh$id, 101)
    expect_equal(session$hl$id, 202)
    expect_identical(session$tables_extra, "X9")
    expect_equal(session$custom_food_table$P_mod, 40)
    expect_equal(session$custom_food_table$count_w, 100)
  }
})
