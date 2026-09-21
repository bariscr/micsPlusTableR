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
#' @section Two routes to producing tables:
#' Use supplied plans, preparation files, and matching survey data, or create
#' and adapt a tabulation plan with a compatible preparation script and data.
#' Both routes can use the application. Creating a plan does not require
#' replacing the app with a scripted workflow. Results can be reviewed in the
#' app without exporting; Excel workbooks and combined table-data downloads
#' are available when files are needed.
#'
#' @section Inputs:
#' Supply an Excel tabulation plan with an `IDX` worksheet and a worksheet for
#' each table, household and household-member SPSS files, and a survey-specific
#' preparation folder containing one main R script that reads `hh_path` and
#' `hl_path` and creates data frames named `hh` and `hl`. Include its supporting
#' files; a `FIES-inputs` subfolder is needed when the selected script uses it.
#'
#' @section User setup:
#' Install R and an editor such as RStudio Desktop. Before installing this
#' package or starting the app, create or open your own survey project.
#' In RStudio, use File > New Project for a new or existing folder, or
#' File > Open Project or double-click an existing `.Rproj` file. Install
#' `remotes` only if it is not already available; once installed, it need not
#' be installed again for ordinary use. Then install this package with
#' `remotes::install_github("bariscr/micsPlusTableR")` and restart the R session
#' before launching the app. Restart R after package updates too. The bundled
#' `doc/user-guide.pdf` and `doc/user-guide.html`
#' explain computer setup, project creation, installation, and file uploads
#' for beginners. The app's User's Guide tab opens the guide after launch.
#'
#' Positron, VS Code, Cursor, Jupyter with an R kernel, and R's own console
#' can also run the package. Configure R support and start R in the survey
#' folder. Check `getwd()` and `here::here()` or set `output_dir` explicitly;
#' use `launch.browser = TRUE` if needed. Remote sessions need browser routing
#' and access to server files. Users do not need the maintainer's project.
#'
#' @section Application tabs and outputs:
#' Reopen the survey project on each visit and start [run_app()]. Select inputs
#' again for each app session. The app opens on Data Preparation; its tabs are
#' arranged in this order:
#' \enumerate{
#'   \item User's Guide: read the embedded instructions, open the full guide in
#'     a separate browser tab, or download the PDF.
#'   \item Download Files: obtain plans and preparation materials for one,
#'     several, or all surveys. Readable names include
#'     `Jamaica (2023-24) Wave 1`. Downloads default to the current project.
#'     Household/member microdata are obtained separately from
#'     the [MICS surveys page](https://mics.unicef.org/surveys) with permission.
#'   \item Data Preparation: choose the country, period, and wave; select the
#'     data, plan, and complete preparation folder; click Set.
#'   \item Tabulator: calculate one sheet and inspect its values. Header shows
#'     descriptive labels, Index shows worksheet positions, and Logic shows
#'     conditions. These views do not change the calculations.
#'   \item Consistency Checks: compare counts and percentages across groups.
#'     Checks and their results remain visible in the app without console output.
#'     A check that does not apply is not a passing validation.
#'   \item Write to Excel: create or select workbooks and write the current
#'     table. Keep the same destinations when adding more tables.
#'   \item Multi-Sheet Tabulator: calculate and write selected sheets together.
#'     It creates required workbooks automatically for unset destinations and
#'     reuses destinations already created or selected in Write to Excel.
#'   \item Data Exploration: investigate frequencies and cross-tabulations.
#'   \item Data View: inspect selected variables in the prepared survey data.
#'   \item Long Format Data: combine calculated table cells and download the
#'     complete result as Excel, CSV, or RDS, irrespective of preview filters.
#' }
#' Choose the tools needed for the task; using every tab or exporting is not
#' required. Generated workbooks default to `micsPlusTableR-output` in the
#' project root; [run_app()] accepts another `output_dir`. Previously saved
#' workbook destinations are not restored after restarting the app: select
#' them explicitly to continue writing to those files.
#'
#' @section Understanding a tabulation plan:
#' The User's Guide explains the worksheet layout, dataset name in B4:B7,
#' filters, row/column conditions, and cell statistics, for readers as well as
#' plan authors. [worksheet-conditions] explains output-oriented condition
#' design, total aliases, and household-member bases.
#' [statistic-types] documents each statistic's population,
#' weighting, and supported direction. [statistic-precision] explains optional
#' `d=` display settings. [tabulate_h()] documents global and additional filters,
#' including inheritance across means and clearing a filter with `unfilter()`.
#'
#' @section Internet access:
#' Installing packages and downloading survey materials require internet.
#' The app's workbook preview loads the SheetJS browser library externally;
#' a browser may cache it, but availability after closing or clearing the browser
#' is not guaranteed. Once packages and inputs are local, table calculations and
#' Excel output do not require that library or an external download.
#'
#' @section Scripting workflow:
#' For work directly in R, use an isolated session and call the required steps.
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
