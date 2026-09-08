# Run with Rscript tools/build-guide.R from the package root.
local({
  if (!file.exists("DESCRIPTION") || !file.exists("inst/doc/user-guide.qmd")) {
    stop("Run this script from the micsPlusTableR package root.", call. = FALSE)
  }
  quarto <- Sys.which("quarto")
  if (!nzchar(quarto)) stop("Install Quarto to rebuild the user guide.", call. = FALSE)
  status <- system2(quarto, c("render", "inst/doc/user-guide.qmd", "--to", "all"))
  if (status != 0L) stop("Guide rendering failed; check Quarto and XeLaTeX.", call. = FALSE)
  stopifnot(file.exists("inst/doc/user-guide.html"), file.exists("inst/doc/user-guide.pdf"))
})
