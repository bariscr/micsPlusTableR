# Run from the package root, for example after opening micsPlusTableR.Rproj.
local({
  if (!file.exists("DESCRIPTION") || !file.exists("R/run_app.R")) {
    stop("Open the micsPlusTableR project or set the working directory to its package root.")
  }
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Install the development loader first: install.packages('pkgload')")
  }
  pkgload::load_all(".", export_all = FALSE)
  micsPlusTableR::run_app()
})
