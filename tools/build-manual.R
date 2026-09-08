# Run with Rscript tools/build-manual.R from the package root.
local({
  stopifnot(file.exists("DESCRIPTION"), dir.exists("man"))
  source("tools/check-documentation.R", local = TRUE)
  source("tools/check-dependencies.R", local = TRUE)
  dir.create("output/pdf", recursive = TRUE, showWarnings = FALSE)
  status <- system2(file.path(R.home("bin"), "R"), c(
    "CMD", "Rd2pdf", "--no-preview", "--force",
    "--output=output/pdf/micsPlusTableR-manual.pdf", "."
  ))
  if (status != 0L) stop("Reference manual build failed; check the output and LaTeX installation.")
})
