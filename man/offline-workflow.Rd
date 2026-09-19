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
\section{Inspect individual steps}{
Stateful helpers take a session as their first argument; use a separate session
for each independent workflow. After preparing data, read the plan and inspect
its row and column predicates before calling the direction-specific engine.
\code{tabulate_mics_table()} selects the direction automatically and additionally
attaches variable-explanation labels.

Use raw long-format numeric results for consistency checks, rather than pivoted
or formatted tables. Each individual check returns \code{status},
\code{issue_count}, \code{details} (all comparisons), and \code{issues}
(failing comparisons). A \code{not_applicable} result means the required total
or distribution is absent; it is not a passing validation.
\code{check_mics_table()} runs all six checks with a default tolerance of
\code{1e-6}; individual helpers default to \code{1e-11}.

Calculation errors identify the worksheet, block or cell, and expression when
available. A missing-variable message means an expression refers to a column
absent from that block's prepared data. Correct the named input and rerun.
Checks cannot validate the substantive choices in a preparation script or plan.
}
\seealso{
\link{micsPlusTableR-package} for the package overview.

\code{\link{calc_cells}} for a runnable weighted-count example.

\link{statistic-types} and \link{statistic-precision} for statistic reference.

\code{\link{download_survey_files}} for survey-material download examples.
}
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
write_mics_footnotes(session, workbooks$output, sheet = "1.1", table = result)

# Alternatively, inspect and run individual steps after preparing the session.
plan <- read_mics_tabulation(session, "TabulationPlan.xlsx", "1.1")
row_rules <- row_condition_f(plan$tab_r)
column_rules <- col_condition_f(plan$tab_c)
result <- if (plan$tab_direction == "v") tabulate_v(session) else tabulate_h(session)
row_check <- row_group_total_check(session, result, diff = 1e-6)
row_check$status
row_check$issues
}
}
