#' micsPlusTableR: MICS Plus Excel tabulation and review
#'
#' Prepare, tabulate, check, review, and export MICS Plus survey tables using
#' an Excel tabulation plan. Work interactively with [run_app()] or use the
#' scripting functions with an isolated [mics_session()].
#'
#' @section Inputs:
#' Supply an Excel tabulation plan with an `IDX` worksheet and a worksheet for
#' each table, household and household-member SPSS files, and a survey-specific
#' R preparation script that reads `hh_path` and `hl_path` and creates data
#' frames named `hh` and `hl`. A sibling `FIES-inputs` directory is optional.
#'
#' @section Interactive workflow:
#' Start [run_app()], select the survey and wave, provide the inputs, prepare
#' the data, calculate tables, review checks, and export the results. The app
#' creates `micsPlusTableR-output` in the project root by default; use
#' `run_app(output_dir = "path/to/output")` to select another location.
#'
#' @section Scripting workflow:
#' The main steps are:
#' \itemize{
#'   \item [mics_session()] creates an isolated session.
#'   \item [prepare_mics_data()] runs the preparation script.
#'   \item [tabulate_mics_table()] calculates cell results.
#'   \item [check_mics_table()] checks consistency.
#'   \item [pivot_mics_table()] arranges results for review.
#'   \item [create_excel_workbooks()] creates output workbooks.
#'   \item [write_mics_table()] writes the results.
#'   \item [write_mics_footnotes()] writes table footnotes.
#'   \item [read_previous_mics_table()] reads an earlier output.
#'   \item [compare_mics_tables()] compares two outputs.
#' }
#' See [offline-workflow] for a complete scripting example.
#'
#' @section Survey and wave configuration:
#' Maintain `inst/extdata/survey_choices.csv` in the package source. It remains
#' a plain, editable CSV on GitHub and is bundled automatically on installation.
#' See [survey_choices] for its columns, validation rules, and update workflow.
#'
#' @section Outputs and interpretation:
#' Outputs include Excel workbooks and long-format observations. The app offers
#' Excel, CSV, and RDS downloads of the full prepared long-format dataset.
#' Use [as_sdmx_compatible()] to convert observations to the project's
#' SDMX-compatible contract. Use [validate_sdmx_compatible()] to check them.
#' This contract does not
#' by itself establish compliance with an external SDMX data structure.
#' Table checks assess consistency; analysts remain responsible for preparation
#' scripts, survey design, denominators, and interpretation of estimates.
#'
#' @seealso [run_app()], [survey_choices], [offline-workflow]
#' @examples
#' session <- mics_session()
#' session
#' system.file("extdata", "survey_choices.csv", package = "micsPlusTableR")
#' @keywords package
"_PACKAGE"
