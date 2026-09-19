\name{run_app}
\alias{run_app}
\title{Run the micsPlusTableR Shiny application}
\usage{
run_app(
  host = "127.0.0.1",
  port = getOption("shiny.port"),
  launch.browser = interactive(),
  output_dir = NULL,
  ...
)
}
\arguments{
\item{host}{Host interface passed to \code{shiny::runApp()}.}
\item{port}{Port passed to \code{shiny::runApp()}; \code{NULL} selects a free port.}
\item{launch.browser}{Whether to open the application in a browser.}
\item{output_dir}{Default directory for generated Excel workbooks. When
\code{NULL}, the app creates \code{micsPlusTableR-output} in the project root
detected by \code{here::here()}.}
\item{...}{Additional arguments passed to \code{shiny::runApp()}.}
}
\value{Invisibly returns the value from \code{shiny::runApp()}.}
\description{
The main user entry point to micsPlusTableR. Opens the browser-based MICS Plus
Tabulation app for preparing inputs, calculating and checking tables,
exploring data, and saving results. Other package functions mainly expose
its supporting engine for documentation, tests, and code-only workflows.
}
\details{
The app opens on Data Preparation. User's Guide provides embedded
instructions, Open full guide, and Download PDF. Download Files saves plans
and preparation materials to the current project by default.

Results can be inspected without exporting. Single-table Excel writing needs
a created or selected workbook. Multi-Sheet Tabulator automatically creates
required workbooks for unset destinations and reuses destinations already set
in Write to Excel. The default output folder is below the survey project;
\code{output_dir} selects another location. Existing destinations and uploaded
inputs must be selected again after restarting the app.

Restart the R session after installing or updating the package before
launching, so previously loaded functions are not retained in memory.
The external SheetJS browser library is needed for the workbook preview;
local calculations and Excel output do not depend on that download.
}
\seealso{\link{micsPlusTableR-package}, \link{offline-workflow}}
