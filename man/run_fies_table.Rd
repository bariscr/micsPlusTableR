\name{run_fies_table}
\alias{run_fies_table}
\title{Run a survey FIES script and return an extra table}
\usage{
run_fies_table()
}
\value{
A data frame with \code{stub}, \code{Moderate food insecurity}, \code{P_sev},
\code{P_mod}, \code{count_w}, and \code{count_unw}, in the script's row order.
Percentage columns are on a 0--100 scale. Assign this result to any object
name and register that name in \code{extra_tables_dict}.
}
\description{
Use this inside a tabulation plan's \code{Preparation File} sheet. The selected
\code{FIES-inputs} folder must contain exactly one file ending in \code{script.R}
(case-insensitive, including \code{_Script.R} and \code{.script.R}). Uploaded
data paths are supplied automatically. The script runs in a separate
environment so its assignments do not replace the preparation's \code{hh} and
\code{hl} objects.
}
\details{
The script must create a local data frame named \code{fies_result} with
character columns \code{group} and \code{category}, and numeric columns
\code{p_mod}, \code{p_sev}, \code{count_w}, and \code{count_unw}.
\code{p_mod} means moderate-or-severe prevalence and \code{p_sev} means severe
prevalence, both on a 0--1 scale. Missing prevalence estimates may be
\code{NA}; counts must be finite and nonnegative. Group/category pairs must
be unique.

The survey script defines questionnaire variables, recoding, eligibility,
weights, category mappings and row order. This helper does not infer or change
those definitions. Rows must match the target worksheet; the extra table
engine checks dimensions when that worksheet is tabulated. The helper
requires the preparation context supplied by \code{\link{prepare_mics_data}}.
Standalone survey scripts remain independent of it.
}
\examples{
\dontrun{
# In Preparation File, after preparing hh and hl:
food_table <- run_fies_table()
extra_tables_dict <- list("4.1" = "food_table")
tables_extra <- names(extra_tables_dict)
}
}
