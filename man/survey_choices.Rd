\name{survey_choices}
\alias{survey_choices}
\title{Maintain survey and wave choices}
\description{
The plain UTF-8 CSV file \file{inst/extdata/survey_choices.csv} in the package
source controls the application's survey and wave selectors. Edit this source
file directly before committing to GitHub or building the package.
}
\section{Columns}{
Each row represents one available country, period, and wave combination.
All four columns are read as character strings:
\describe{
\item{country}{Full country name, for example \code{Jamaica}.}
\item{country_code}{Exactly three uppercase letters, for example \code{JAM}.}
\item{period}{Survey period as text, for example \code{2023-24}.}
\item{wave}{Wave label, for example \code{Wave 1}.}
}
Keep the column names. The file must have at least one row, with no empty
fields or duplicate country/period/wave combinations. All waves of the same
country and period must use the same country code. Leading and trailing
whitespace is removed. Row order determines selector order and defaults.
}
\section{Editing and installation}{
Use a text editor or a spreadsheet editor that preserves the period and wave
columns as text. Save as UTF-8 CSV. From the package root, run
\code{source("run_dev.R")} to load and launch the development source.
Stop and restart the app after saving changes.

The CSV requires no conversion to an R data object.

R installs \file{inst/extdata} files into the package's \file{extdata} directory.
For installed packages, reinstall the updated source and restart the R session
before launching the app.
Direct edits to an installed copy are replaced by reinstallation or upgrades.
The selected code, full period, and wave also contribute to output filenames
and long-format metadata.
}
\examples{
path <- system.file("extdata", "survey_choices.csv", package = "micsPlusTableR")
choices <- read.csv(path, colClasses = "character", fileEncoding = "UTF-8-BOM")
choices
}
\seealso{\code{\link{run_app}}, \code{\link{micsPlusTableR-package}}}
\keyword{documentation}
