#' Preparation script dependencies
#'
#' Survey preparation packages are tracked separately from the packages needed
#' to launch the app and run the tabulation engine. Both groups are required
#' Imports and install together with micsPlusTableR; no separate setup is needed.
#' @details
#' The maintained preparation scripts declare `prep_dependencies` near the top
#' of the file.
#'
#' [prepare_mics_data()] checks these packages and the detected
#' helper dependencies before executing the script. The Shiny Data Preparation
#' workflow uses the same check. Missing or unloadable packages produce an
#' error naming the package, the script requiring it, and an installation command.
#'
#' Use [check_prep_dependencies()] for diagnosis.
#'
#' Use [install_prep_dependencies()] for repair or extra custom-script packages.
#' Neither is a required step for normal installation or switching between
#' maintained surveys.
#'
#' Additional DESCRIPTION field:
#' `Config/micsPlusTableR/prep-packages`.
#'
#' Preparation-only packages in the maintained Jamaica workflows:
#' \itemize{
#'   \item `ggplot2`: FIES distribution and thermometer plots.
#'   \item `Hmisc`: weighted prevalence means in the FIES helpers.
#'   \item `memisc`: SPSS import retaining value labels for the FIES routines.
#'   \item `reshape2`: reshape FIES plotting data with melt.
#'   \item `RM.weights`: weighted Rasch model and food-insecurity calculations.
#'   \item `survey`: survey designs and design-based FIES estimates.
#'   \item `srvyr`: survey-design pipelines in the main preparation scripts.
#'   \item `labelled`: variable/value labels in Jamaica Wave 2 recodes.
#' }
#' All eight packages are installed by the normal package installation command.
#' They are not imported into the app's namespace. Packages required by the
#' selected script are made available in its private preparation environment;
#' package functions already supplied by the core workflow retain precedence.
#' Use explicit package::function calls if a script needs a conflicting name.
#'
#' Shared dependencies are also declared by the scripts: Mongolia Waves 1 and 2
#' use dplyr and haven. Jamaica Waves 1 and 2 also use here, tibble, and tidyr;
#' Wave 2 additionally uses purrr. These are already package Imports.
#'
#' Keep declarations current when editing scripts or adding helpers. The checker
#' scans the main script, all R files in its sibling FIES-inputs directory, and
#' literal source paths. Declare packages used through dynamically constructed
#' names or source paths explicitly. The checker does not execute preparation
#' code and does not certify statistical results or package-version compatibility.
#' @examples
#' \dontrun{
#' prep_script <- "survey-preparation/survey_prep.R"
#' check_prep_dependencies(prep_script)
#' # For repair or additional dependencies in a custom script only:
#' install_prep_dependencies(prep_script)
#' }
#' @seealso [prepare_mics_data()], [package-dependencies]
#' @name prep-script-dependencies
NULL

#' Package runtime and development dependencies
#'
#' Required Imports cover the app, tabulation engine, and maintained preparation
#' scripts. They install together. Preparation-only packages are documented
#' separately in [prep-script-dependencies].
#' @details
#' Runtime packages and their purposes:
#' \itemize{
#'   \item `shiny`, `bslib`, `shinyFiles`: application UI, server, and file selection.
#'   \item `htmltools`, `htmlwidgets`, `reactable`: HTML controls and table displays.
#'   \item `flextable`, `officer`: formatted table previews and borders.
#'   \item `openxlsx2`: Excel output workbooks and formatting.
#'   \item `readxl`, `tidyxl`: worksheet values, cell formats, and plan parsing.
#'   \item `dplyr`, `tidyr`, `tibble`: table transformation and cell results.
#'   \item `purrr`, `rlang`, `stringr`: iteration, expressions, and condition parsing.
#'   \item `haven`: display and clean labelled survey values in the app.
#'   \item `janitor`: remove empty columns from comparison workbooks.
#'   \item `here`: locate the user's project and default output folder.
#'   \item `jsonlite`: read the remote survey-file listing.
#'   \item `readr`: CSV downloads of long-format results.
#'   \item `later`: schedule application UI updates.
#' }
#' Development-only Suggests are testthat for tests and knitr/rmarkdown for
#' vignette building. Developer tools such as roxygen2, pkgload, and remotes
#' are installed separately as described in the README.
#'
#' The maintainer command `Rscript tools/check-dependencies.R` inspects
#' executable app and engine code, qualified references, and imported function
#' calls. It rejects undeclared direct references and Imports with no detected
#' usage in the app, engine, or preparation namespace loaders. It also checks
#' that all documented preparation dependencies are required Imports. Both
#' package and reference-manual builds run this audit.
#' @seealso [prep-script-dependencies], [micsPlusTableR-package]
#' @name package-dependencies
NULL
