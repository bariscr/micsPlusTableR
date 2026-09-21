\name{offline-workflow}
\alias{offline-workflow}
\title{Run MICS Plus tabulation workflows outside Shiny}
\description{
The application is the main user interface. These supporting functions expose
its packaged engine for reference, tests, and workflows run directly in R.
They are not additional steps required when using the app. A session is an
ordinary R environment that keeps survey data, plan context, extra tables,
and calculated results together. Creating it with \code{mics_session()}
does not start Shiny, open a browser, or launch the app.
}
\details{
Here, outside Shiny means running without the interface, not a guarantee that
every operation is offline. Installing packages or downloading survey files
requires internet. With local packages and inputs, table calculations and
workbook output can run offline.

Workflow functions such as \code{read_mics_tabulation()} require an explicit
\code{session} argument with no default. Create it once and pass the same
object to subsequent calls. Use a separate session for each independent test.
The examples require your own Excel plan, and prepared data or survey files
when calculating results. Replace the example paths and sheet names with yours.
See the package overview for links to the function references and export steps.
}
\section{Read and inspect a plan without survey data}{
Create an empty session with \code{mics_session()}, then pass it to
\code{read_mics_tabulation(session, path_tab_excel, sheet)}. Only the Excel
plan is needed for this step. The returned list includes \code{filter_row}
for filter instructions and \code{tab} for the cell statistics and conditions.
The plan is also kept in the session for subsequent calculations.
}
\section{Calculate with already prepared data}{
If \code{hh} and \code{hl} are already prepared data frames in your workspace,
add them to the session holding the plan with \code{session$hh <- hh} and
\code{session$hl <- hl}. Alternatively, start with
\code{mics_session(hh = hh, hl = hl)} and then read the plan.
Supply the datasets referenced by the plan; a household-only plan can use
\code{hh} alone. These operations do not prepare or transform the data.
The variables and weights required by the plan must already be present.
Call \code{tabulate_mics_table(session)} to calculate results, then
\code{pivot_mics_table(session, results, type = "header")} to review them.
}
\section{Start from raw survey files}{
Create a session and call \code{prepare_mics_data()} with your household file,
household-member file, and survey preparation script. Then read and calculate
the plan, check consistency, and pivot the results before writing them.
}
\section{Inspect individual steps}{
Stateful helpers take a session as their first argument. Read the plan and
inspect its row and column predicates before calculating. Prepared data is
needed for calculation, but not for reading the plan or parsing its predicates.
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
library(micsPlusTableR)

# 1. Read and inspect a plan without survey data or the app.
session <- mics_session()
plan <- read_mics_tabulation(
  session = session,
  path_tab_excel = "TabulationPlan.xlsx",
  sheet = "6.2"
)
plan$filter_row
plan$tab

# 2. Continue with hh and hl already prepared in your workspace.
session$hh <- hh
session$hl <- hl
results <- tabulate_mics_table(session)
preview <- pivot_mics_table(session, results, type = "header")

# Alternatively, supply prepared data when creating a new session.
prepared_session <- mics_session(hh = hh, hl = hl)
results <- tabulate_mics_table(prepared_session, "TabulationPlan.xlsx", "6.2")

# 3. Start from raw files instead, using your survey preparation script.
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
