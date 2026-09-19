#' Run the micsPlusTableR Shiny application
#'
#' The main user entry point to micsPlusTableR. Opens the browser-based MICS Plus
#' Tabulation app for preparing inputs, calculating and checking tables,
#' exploring data, and saving results. Other package functions mainly expose
#' its supporting engine for documentation, tests, and code-only workflows.
#'
#' @details The app opens on Data Preparation. User's Guide provides embedded
#' instructions, Open full guide, and Download PDF. Download Files saves plans
#' and preparation materials to the current project by default.
#'
#' Results can be inspected without exporting. Single-table Excel writing needs
#' a created or selected workbook. Multi-Sheet Tabulator automatically creates
#' required workbooks for unset destinations and reuses destinations already set
#' in Write to Excel. The default output folder is below the survey project;
#' `output_dir` selects another location. Existing destinations and uploaded
#' inputs must be selected again after restarting the app.
#'
#' Restart the R session after installing or updating the package before
#' launching, so previously loaded functions are not retained in memory.
#' The external SheetJS browser library is needed for the workbook preview;
#' local calculations and Excel output do not depend on that download.
#' @seealso [micsPlusTableR-package], [offline-workflow]
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

  # Capture the caller's project before Shiny changes to the installed app directory.
  project_dir <- normalizePath(here::here(), winslash = "/", mustWork = TRUE)
  old_options <- options(micsPlusTableR.output_dir = output_dir,
                         micsPlusTableR.project_dir = project_dir)
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
