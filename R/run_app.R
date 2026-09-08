#' Run the micsPlusTableR Shiny application
#'
#' Starts the packaged Shiny application. Output workbooks are written to
#' `output_dir` unless the user selects another destination in the app.
#'
#' @param host Host interface passed to [shiny::runApp()].
#' @param port Port passed to [shiny::runApp()]. Use `NULL` to select a free port.
#' @param launch.browser Whether to open the application in a browser.
#' @param output_dir Default directory for generated Excel workbooks. When
#'   `NULL`, the app creates `micsPlusTableR-output` in the project root
#'   detected by [here::here()].
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Invisibly returns the value from [shiny::runApp()].
#' @export
run_app <- function(host = "127.0.0.1",
                    port = getOption("shiny.port"),
                    launch.browser = interactive(),
                    output_dir = NULL,
                    ...) {
  app_dir <- system.file("shinyapp", package = "micsPlusTableR")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("The packaged Shiny application could not be found. Reinstall micsPlusTableR.",
         call. = FALSE)
  }

  if (is.null(output_dir)) {
    output_dir <- default_output_dir()
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) {
    stop("The Excel output directory could not be created: ", output_dir,
         call. = FALSE)
  }

  old_options <- options(micsPlusTableR.output_dir = output_dir)
  on.exit(options(old_options), add = TRUE)

  shiny::runApp(
    appDir = app_dir,
    host = host,
    port = port,
    launch.browser = launch.browser,
    ...
  )
}

default_output_dir <- function() {
  here::here("micsPlusTableR-output")
}
