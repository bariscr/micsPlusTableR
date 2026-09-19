\name{create_excel_workbooks}
\alias{create_excel_workbooks}
\title{Create output workbooks from a tabulation plan}
\usage{
create_excel_workbooks(
  path_tab_excel,
  output_dir,
  country_code,
  period,
  wave,
  target = c("both", "output", "formatted"),
  time_tag = format(Sys.time(), "\%Y\%m\%d\%H\%M\%S")
)
}
\arguments{
\item{path_tab_excel}{Existing \code{.xls} or \code{.xlsx} tabulation plan.}
\item{output_dir}{Directory where workbooks will be created.}
\item{country_code}{Short country code used in filenames.}
\item{period}{Survey period used in filenames.}
\item{wave}{Wave label used in filenames.}
\item{target}{One of \code{"output"}, \code{"formatted"}, or \code{"both"}.}
\item{time_tag}{Timestamp used in filenames.}
}
\value{A list with \code{output} and \code{formatted} paths.}
\description{
Copies the tabulation plan to standard output names without overwriting an
existing file. A numeric suffix is added after a filename collision.
}

\details{
Creates copies of the tabulation plan; it does not calculate or populate table
results. In the app, Write to Excel calls it on request, and Multi-Sheet
Tabulator calls it automatically for unset required destinations. In R
workflows, call it before \code{\link{write_mics_table}} unless using an existing
workbook. Unrequested entries in the returned list are \code{NULL}.
}
