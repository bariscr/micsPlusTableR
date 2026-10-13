#' micsPlusTableR: MICS Plus Excel tabulation and review
#'
#' micsPlusTableR is the R package behind the MICS Plus Tabulation application.
#' Its main purpose is to provide a browser-based interface for preparing survey
#' data, calculating tables from a tabulation plan, checking results, exploring
#' data, and saving outputs when needed. Start the application with [run_app()].
#' R performs the calculations; Shiny provides the interface running in the browser.
#'
#' @section The app and the supporting functions:
#' The package was created primarily to serve the application. Most other
#' functions perform background tasks used by, or exposed from, its tabulation
#' engine. Their help pages document how the system works and support testing,
#' diagnosis, and workflows run entirely in R. App users do not need to call
#' these functions individually or follow the function index as a workflow.
#'
#' Use the bundled User's Guide for setup, tab-by-tab instructions, and
#' understanding a tabulation plan. Use this reference manual for exact
#' function arguments, return values, examples, and engine behavior.
#'
#' @section Running the application:
#' Launch the Shiny application with [run_app()]:
#' ```r
#' micsPlusTableR::run_app()
#' ```
#'
#' @section Scripting workflow:
#' For work directly in R, use an isolated session and call the required steps.
#' A session is an ordinary R environment holding your data, plan, and results.
#' [mics_session()] does not start Shiny or open a browser. An empty session is
#' enough to read and inspect a worksheet; calculation additionally requires
#' prepared data. Use `mics_session(hh = hh, hl = hl)` for data already prepared
#' in your workspace, or [prepare_mics_data()] for raw survey files.
#' These interfaces also make the app's engine available for testing:
#' \itemize{
#'   \item [mics_session()] creates an isolated session.
#'   \item [prepare_mics_data()] runs the preparation script.
#'   \item [read_mics_tabulation()] reads a worksheet's instructions.
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
#' @section Individual steps and testing:
#' [tabulate_v()] and [tabulate_h()] calculate a loaded plan in a specific
#' direction; [calc_cells()] tests cells against explicit input data.
#' [tabulate_extra_table()] maps supplied values to the loaded plan.
#' [row_condition_f()] parses row predicates.
#' [col_condition_f()] parses column predicates.
#'
#' [normalize_condition_text()] normalizes Excel text.
#' [extract_var()] identifies variables.
#'
#' [get_blank_cols()] supports worksheet cleanup.
#' [filter_second()] supports comparison cleanup.
#'
#' Run a single consistency check:
#' \itemize{
#'   \item [row_group_total_check()] checks row counts.
#'   \item [row_group_perc_total_check()] checks row percentages.
#'   \item [col_group_total_check()] checks column counts.
#'   \item [col_group_perc_total_check()] checks column percentages.
#'   \item [row_indent_group_total_check()] checks indented counts.
#'   \item [row_indent_group_perc_total_check()] checks indented percentages.
#' }
#' Each returns status, issue count, all comparisons, and failing comparisons.
#' Stateful helpers take a session. See [offline-workflow] for individual-step
#' examples. The [calc_cells()] help page includes a runnable weighted example.
#'
#' @section Dependencies:
#' [package-dependencies] explains required app/engine packages and development
#' tools. [prep-script-dependencies] documents survey packages separately.
#' Both dependency groups install with the package; no separate preparation
#' setup is needed.
#'
#' For diagnosis, use [check_prep_dependencies()]. For repairs or extra
#' custom-script requirements, use [install_prep_dependencies()].
#'
#' @section Survey and wave configuration:
#' Maintain `inst/extdata/survey_choices.csv` in the package source. It remains
#' a plain, editable CSV on GitHub and is bundled automatically on installation.
#' See [survey_choices] for its columns, validation rules, and update workflow.
#'
#' @section Outputs and interpretation:
#' Outputs include Excel workbooks and long-format observations. The app offers
#' Excel, CSV, and RDS downloads of combined calculated table cells. These are
#' aggregate table results, distinct from the survey records shown in Data View.
#' Use [as_sdmx_compatible()] to convert observations to the project's
#' SDMX-compatible contract. Use [validate_sdmx_compatible()] to check them.
#' This contract does not
#' by itself establish compliance with an external SDMX data structure.
#' Table checks assess consistency; analysts remain responsible for preparation
#' scripts, survey design, denominators, and interpretation of estimates.
#'
#' @seealso [run_app()], [statistic-types], [statistic-precision],
#'   [survey_choices], [offline-workflow]
#' @examples
#' session <- mics_session()
#' session
#' system.file("extdata", "survey_choices.csv", package = "micsPlusTableR")
#' @keywords package
"_PACKAGE"
