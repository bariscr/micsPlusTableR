test_that("installed guide assets support the HTML and printable workflows", {
  doc <- system.file("doc", package = "micsPlusTableR")
  expect_true(file.exists(file.path(doc, "user-guide.pdf")))
  html <- paste(readLines(file.path(doc, "user-guide.html"), warn = FALSE), collapse = "\n")
  expect_match(html, "MICS Plus Tabulation", fixed = TRUE)
  expect_match(html, 'href="user-guide.pdf"', fixed = TRUE)
  expect_match(html, "data:image/png;base64,", fixed = TRUE)
  expect_false(grepl("setup-and-maintenance.md", html, fixed = TRUE))
})

test_that("the app embeds the package HTML guide with an accessible title", {
  output_dir <- tempfile("guide-ui-")
  old <- options(micsPlusTableR.output_dir = output_dir)
  on.exit(options(old), add = TRUE)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  app_env <- new.env()
  source(system.file("shinyapp", "app.R", package = "micsPlusTableR"), local = app_env)
  html <- htmltools::renderTags(app_env$ui)$html
  expect_match(html, '<iframe src="micsPlusTableR-guide/user-guide.html"', fixed = TRUE)
  expect_match(html, 'title="MICS Plus Tabulation application user guide"', fixed = TRUE)
  expect_match(html, "Open full guide", fixed = TRUE)
  expect_match(html, 'download="micsPlusTableR-user-guide.pdf"', fixed = TRUE)
  expect_identical(normalizePath(shiny::resourcePaths()[["micsPlusTableR-guide"]]),
                   normalizePath(system.file("doc", package = "micsPlusTableR")))
})
