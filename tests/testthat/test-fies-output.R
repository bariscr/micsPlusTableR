test_that("FIES model reports use explicit paths without changing results or namespaces", {
  set.seed(42)
  x <- matrix(rbinom(1600, 1, rep(seq(.15, .85, length.out = 8), each = 200)),
              ncol = 8, dimnames = list(NULL, paste0("I", 1:8)))
  starting_dir <- getwd()
  original_rm <- RM.weights::RM.w
  original_pc <- RM.weights::PC.w
  for (wrapper in c("rm", "pc")) {
    output <- tempfile("FIES reports with spaces ")
    model <- if (wrapper == "rm") original_rm else original_pc
    writer <- if (wrapper == "rm") fies_rm_w else fies_pc_w
    input <- if (wrapper == "rm") x else as.matrix(expand.grid(rep(list(0:2), 4)))
    args <- list(input, rep(c(1, 2), length.out = nrow(input)),
                 country = "Survey label", write.file = FALSE)
    invisible(capture.output(expected <- do.call(model, args)))
    args$write.file <- TRUE
    args$output_dir <- output
    if (wrapper == "pc") args$write.iteration <- TRUE
    invisible(capture.output(actual <- do.call(writer, args)))
    expect_equal(actual, expected)
    expect_identical(getwd(), starting_dir)
    files <- list.files(output)
    expect_true(if (wrapper == "rm") "OutputSurvey label.csv" %in% files else
      all(c("Survey label_Polytomous_Output.csv", "Iteration history.csv") %in% files))
    expect_true(all(file.info(file.path(output, files))$size > 0))
  }
  expect_identical(RM.weights::RM.w, original_rm)
  expect_identical(RM.weights::PC.w, original_pc)
})

test_that("preparation leaves the working directory unchanged even during a failing helper", {
  folder <- tempfile()
  dir.create(file.path(folder, "FIES-inputs"), recursive = TRUE)
  writeLines(c(
    "writeLines(getwd(), file.path(path_fies_output, 'observed-directory.txt'))",
    "stop('helper failed')"
  ), file.path(folder, "FIES-inputs", "helper.R"))
  plan <- preparation_plan(code = "source('helper.R')")
  files <- c(tempfile(), tempfile())
  file.create(files)
  output <- tempfile()
  starting_dir <- getwd()
  expect_error(prepare_mics_data(mics_session(), files[1], files[2], plan,
                                output_dir = output, prep_folder = folder),
               "helper failed")
  expect_identical(getwd(), starting_dir)
  expect_identical(readLines(file.path(output, "FIES-outputs", "observed-directory.txt")),
                   starting_dir)
})
