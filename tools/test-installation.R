# Run from the checkout with Rscript tools/test-installation.R.
# Dependencies must already be installed. All installs use a disposable library.
local({
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 3L && identical(args[[1L]], "--worker")) {
    source_dir <- args[[2L]]
    library_dir <- args[[3L]]
    .libPaths(c(library_dir, .libPaths()))
    starting_dir <- getwd()
    # install.packages() reports failed installs as warnings. Fail the check
    # even if an earlier installed copy would otherwise still load.
    options(warn = 2)
    remotes::install_local(
      source_dir,
      lib = library_dir,
      dependencies = FALSE,
      upgrade = "never",
      force = TRUE
    )
    stopifnot(identical(getwd(), starting_dir))
    library("micsPlusTableR", lib.loc = library_dir, character.only = TRUE)
    stopifnot(
      identical(getwd(), starting_dir),
      identical(
        normalizePath(find.package("micsPlusTableR")),
        normalizePath(file.path(library_dir, "micsPlusTableR"))
      ),
      as.character(packageVersion("micsPlusTableR")) ==
        read.dcf(file.path(source_dir, "DESCRIPTION"))[1L, "Version"],
      file.exists(system.file("shinyapp", "app.R", package = "micsPlusTableR")),
      file.exists(system.file("extdata", "survey_choices.csv", package = "micsPlusTableR")),
      !length(list.files(library_dir, pattern = "^00LOCK"))
    )
  } else {
    stopifnot(length(args) == 0L)
    source_dir <- normalizePath(".", mustWork = TRUE)
    script <- normalizePath("tools/test-installation.R", mustWork = TRUE)
    scratch <- tempfile("mics-install-check-")
    library_dir <- file.path(scratch, "library with spaces")
    project_dir <- file.path(scratch, "unrelated project with spaces")
    dir.create(library_dir, recursive = TRUE)
    dir.create(project_dir, recursive = TRUE)
    on.exit(unlink(scratch, recursive = TRUE), add = TRUE)

    run_install <- function(directory) {
      old_dir <- setwd(directory)
      on.exit(setwd(old_dir), add = TRUE)
      status <- system2(file.path(R.home("bin"), "Rscript"), c(
        "--vanilla", shQuote(script), "--worker",
        shQuote(source_dir), shQuote(library_dir)
      ))
      if (status != 0L) stop("Installation check failed from: ", directory)
    }

    message("Checking a clean install from the source checkout")
    run_install(source_dir)
    marker <- file.path(library_dir, "micsPlusTableR", "previous-install-marker")
    writeLines("This file must disappear when the package is replaced.", marker)
    message("Checking replacement from an unrelated project directory")
    run_install(project_dir)
    stopifnot(!file.exists(marker))
    message("Clean install and replacement checks passed.")
  }
})
