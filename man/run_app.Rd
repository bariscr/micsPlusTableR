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
\description{Starts the Shiny application bundled with micsPlusTableR.}
