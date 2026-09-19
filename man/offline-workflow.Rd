\name{offline-workflow}
\alias{offline-workflow}
\title{Run MICS Plus tabulation workflows outside Shiny}
\description{
The application is the main user interface. These supporting functions expose
its packaged engine for reference, tests, and workflows run directly in R.
They are not additional steps required when using the app. The explicit session keeps survey data, plan context, extra
tables, and calculated results together without writing them into the user's
global workspace.
}
\details{
Here, outside Shiny means running without the interface, not a guarantee that
every operation is offline. Installing packages or downloading survey files
requires internet. With local packages and inputs, table calculations and
workbook output can run offline.

Prepare survey data in a new session, calculate a table from the tabulation plan,
check its consistency, and pivot it for review before writing results.
The example requires analyst-supplied files. See the package overview for
links to the function references and workbook export steps.
}
\seealso{\code{\link{micsPlusTableR-package}}}
\examples{
\dontrun{
session <- mics_session()
prepare_mics_data(
  session,
  "hh.sav",
  "hl.sav",
  prep_script = "survey-preparation/survey_prep.R",
  output_dir = "micsPlusTableR-output"
)
result <- tabulate_mics_table(session, "TabulationPlan.xlsx", "1.1")
checks <- check_mics_table(session, result)
preview <- pivot_mics_table(session, result, type = "header")
# Export only when needed; this function requires an existing destination.
workbooks <- create_excel_workbooks(
  "TabulationPlan.xlsx", "micsPlusTableR-output",
  country_code = "JAM", period = "2023-24", wave = "Wave 1"
)
write_mics_table(session, workbooks$output, sheet = "1.1", table = result)
}
}
