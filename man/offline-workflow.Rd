\name{offline-workflow}
\alias{offline-workflow}
\title{Run MICS Plus tabulation workflows outside Shiny}
\description{
These functions provide the supported non-Shiny interface to the packaged
tabulation engine. The explicit session keeps survey data, plan context, extra
tables, and calculated results together without writing them into the user's
global workspace.
}
\details{
Prepare survey data in a new session, calculate a table from the Excel plan,
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
}
}
