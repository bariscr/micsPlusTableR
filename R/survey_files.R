#' List survey tabulation plans and preparation files
#'
#' Discover or download tabulation plans, preparation scripts, and their
#' supporting reference assets from the separate survey files repository.
#' Household and household-member microdata are not distributed here; obtain
#' them separately from the MICS Plus website with the required permission.
#' The repository must be public. No GitHub account or token is needed for
#' these plans and preparation files.
#'
#' @param repo GitHub repository in `owner/name` form.
#' @param ref Branch, tag, or commit to download. Defaults to `"main"`.
#'
#' @details
#' Survey folders use a three-letter country code and wave, such as `JAM_W1`.
#' Downloads include root-level Excel plans (`.xls`, `.xlsx`) and `.R` scripts
#' or `.RData` reference assets inside `prep-files-*` subfolders. Other files,
#' including SPSS microdata and hidden files, are excluded. Preparation
#' subfolders are preserved. A single Git commit is resolved before listing or downloading,
#' so changes to the branch during a download cannot mix remote versions.
#' Newly published survey folders are discovered without a package update.
#'
#' Downloads are staged and their sizes checked before destination files are
#' written. A network failure leaves existing destination files unchanged.
#' Files are downloaded in binary mode and are never executed. Selecting
#' individual files may omit preparation helpers; select a whole survey folder
#' for all its published materials. Microdata must still be obtained separately.
#' Git LFS pointers are not supported.
#'
#' Internet access is required. Unauthenticated GitHub API rate limits apply;
#' if a request fails, check the connection and public repository/ref, or retry
#' later. The browser ZIP link in the user's guide is an alternative.
#'
#' @return `list_survey_files()` returns a data frame with `survey`, `path`,
#'   `size` (bytes), and `url` columns, plus a `commit` attribute.
#' @seealso [download_survey_files()]
#' @examples
#' \dontrun{
#' available <- list_survey_files()
#' unique(available$survey)
#' available$path
#' }
#' @export
list_survey_files <- function(repo = "bariscr/micsPlusTableR-Files", ref = "main") {
  survey_files_string(repo, "repo")
  survey_files_string(ref, "ref")
  if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repo)) {
    stop("repo must have the form 'owner/name'.", call. = FALSE)
  }
  base <- paste0("https://api.github.com/repos/", repo)
  commit <- survey_files_json(paste0(base, "/commits/", utils::URLencode(ref, reserved = TRUE)))
  sha <- commit$sha
  if (!is.character(sha) || length(sha) != 1L ||
      is.na(sha) || !grepl("^[a-fA-F0-9]{40}$", sha)) {
    stop("GitHub did not return a valid commit.", call. = FALSE)
  }
  tree <- survey_files_json(paste0(base, "/git/trees/", sha, "?recursive=1"))
  if (!identical(tree$truncated, FALSE)) {
    stop("GitHub returned an incomplete file listing; use the repository ZIP download.", call. = FALSE)
  }
  entries <- Filter(function(x) {
    identical(x$type, "blob") && x$mode %in% c("100644", "100755") &&
      is.character(x$path) && length(x$path) == 1L &&
      grepl("^[A-Z]{3}_W[0-9]+/", x$path)
  }, tree$tree)
  result <- data.frame(survey = character(), path = character(),
                       size = numeric(), url = character())
  if (length(entries)) {
    paths <- vapply(entries, `[[`, character(1), "path")
    if (any(!vapply(paths, survey_files_safe_path, logical(1))) ||
        anyDuplicated(tolower(paths))) {
      stop("Repository contains unsafe or conflicting file paths.", call. = FALSE)
    }
    relative <- sub("^[^/]+/", "", paths)
    plans <- grepl("^[^/]+\\.(xls|xlsx)$", relative, ignore.case = TRUE)
    preparation <- grepl("^prep-files-[^/]+/.+\\.(r|rdata)$", relative,
                         ignore.case = TRUE)
    keep <- !grepl("(^|/)\\.", paths) & (plans | preparation)
    entries <- entries[keep]
    paths <- paths[keep]
    if (!length(paths)) {
      attr(result, "commit") <- sha
      return(result)
    }
    sizes <- vapply(entries, function(x) as.numeric(x$size), numeric(1))
    if (anyNA(sizes) || any(!is.finite(sizes) | sizes < 0)) {
      stop("GitHub returned invalid file sizes.", call. = FALSE)
    }
    encoded <- vapply(strsplit(paths, "/", fixed = TRUE), function(parts) {
      paste(vapply(parts, utils::URLencode, character(1), reserved = TRUE), collapse = "/")
    }, character(1))
    result <- data.frame(
      survey = sub("/.*$", "", paths),
      path = paths, size = sizes,
      url = paste0("https://raw.githubusercontent.com/", repo, "/", sha, "/", encoded)
    )
    result <- result[order(result$survey, result$path), , drop = FALSE]
    rownames(result) <- NULL
  }
  attr(result, "commit") <- sha
  result
}

#' Download survey tabulation plans and preparation files
#'
#' Save selected plans, preparation scripts, and supporting reference assets
#' from the public survey files repository. Household and household-member
#' microdata must be obtained separately with the required permission.
#' @inheritParams list_survey_files
#' @param surveys Character vector of root survey folder names, such as
#'   `"JAM_W1"`. `NULL` selects all surveys.
#' @param files Character vector of exact repository-relative paths from
#'   [list_survey_files()]. `NULL` selects all files in the selected surveys.
#' @param dest_dir Destination folder, relative to the current R working
#'   directory unless an absolute path is supplied. Defaults to `"inputs"`.
#' @param overwrite Replace existing files? Defaults to `FALSE`; any existing
#'   selected file stops the operation before downloading file contents.
#' @param quiet Suppress file download progress and the completion message?

#' @inherit list_survey_files details
#' @return Invisibly returns the selected file-list rows with `survey`, `path`,
#'   `size`, `url`, and `local_path` columns, plus the resolved `commit` attribute.
#' @seealso [list_survey_files()]
#' @examples
#' \dontrun{
#' download_survey_files("JAM_W1")
#' download_survey_files(c("JAM_W1", "MNG_W2"), dest_dir = "survey-inputs")
#' download_survey_files() # All surveys
#' available <- list_survey_files()
#' download_survey_files(files = available$path[1])
#' }
#' @export
download_survey_files <- function(surveys = NULL, files = NULL, dest_dir = "inputs",
                                  overwrite = FALSE, quiet = FALSE,
                                  repo = "bariscr/micsPlusTableR-Files", ref = "main") {
  survey_files_string(dest_dir, "dest_dir")
  for (name in c("overwrite", "quiet")) {
    value <- get(name)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(name, " must be TRUE or FALSE.", call. = FALSE)
    }
  }
  for (name in c("surveys", "files")) {
    value <- get(name)
    if (!is.null(value) && (!is.character(value) || !length(value) ||
                          anyNA(value) || any(!nzchar(trimws(value))))) {
      stop(name, " must be NULL or a non-empty character vector.", call. = FALSE)
    }
  }
  available <- list_survey_files(repo = repo, ref = ref)
  commit <- attr(available, "commit")
  if (!is.null(surveys)) {
    missing <- setdiff(surveys, available$survey)
    if (length(missing)) stop("Unknown surveys: ", paste(missing, collapse = ", "),
                              ". Use list_survey_files().", call. = FALSE)
    available <- available[available$survey %in% surveys, , drop = FALSE]
  }
  if (!is.null(files)) {
    missing <- setdiff(files, available$path)
    if (length(missing)) stop("Files not found in the selected surveys: ",
                              paste(missing, collapse = ", "), call. = FALSE)
    available <- available[available$path %in% files, , drop = FALSE]
  }
  if (!nrow(available)) stop("No published survey files were found.", call. = FALSE)
  dest_dir <- path.expand(dest_dir)
  if (file.exists(dest_dir) && !dir.exists(dest_dir)) {
    stop("dest_dir is an existing file.", call. = FALSE)
  }
  targets <- file.path(dest_dir, available$path)
  # Check all destinations before any network transfers or directory creation.
  for (path in available$path) {
    parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
    current <- dest_dir
    for (i in seq_along(parts)) {
      current <- file.path(current, parts[[i]])
      link <- Sys.readlink(current)
      if (!is.na(link) && nzchar(link)) stop("Destination contains a symbolic link: ", current, call. = FALSE)
      if (i < length(parts) && file.exists(current) && !dir.exists(current)) {
        stop("Destination folder is an existing file: ", current, call. = FALSE)
      }
      if (i == length(parts) && dir.exists(current)) {
        stop("Destination file is an existing folder: ", current, call. = FALSE)
      }
    }
  }
  existing <- file.exists(targets)
  if (!overwrite && any(existing)) {
    stop("Files already exist; use another dest_dir or set overwrite = TRUE: ",
         paste(available$path[existing], collapse = ", "), call. = FALSE)
  }
  stage <- tempfile("mics-survey-download-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  staged <- file.path(stage, as.character(seq_len(nrow(available))))
  for (i in seq_len(nrow(available))) {
    survey_files_fetch(available$url[[i]], staged[[i]], quiet = quiet)
    if (!file.exists(staged[[i]]) || file.info(staged[[i]])$size != available$size[[i]]) {
      stop("Incomplete download: ", available$path[[i]], ". Please retry.", call. = FALSE)
    }
    prefix <- readBin(staged[[i]], "raw", n = nchar("version https://git-lfs.github.com/spec/v1\n"))
    if (identical(prefix, charToRaw("version https://git-lfs.github.com/spec/v1\n"))) {
      stop("Git LFS files are not supported: ", available$path[[i]], call. = FALSE)
    }
  }
  for (i in seq_along(targets)) {
    parent <- dirname(targets[[i]])
    if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE)) {
      stop("Cannot create destination folder: ", parent, call. = FALSE)
    }
    if (!file.copy(staged[[i]], targets[[i]], overwrite = overwrite)) {
      stop("Cannot write destination file: ", targets[[i]], call. = FALSE)
    }
  }
  available$local_path <- normalizePath(targets, winslash = "/", mustWork = TRUE)
  attr(available, "commit") <- commit
  if (!quiet) message("Downloaded ", nrow(available), " files to ",
                       normalizePath(dest_dir, winslash = "/"), ".")
  invisible(available)
}

survey_files_string <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
    stop(name, " must be a non-empty string.", call. = FALSE)
  }
}

survey_files_safe_path <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  !grepl("[\\\\:]", path) && !startsWith(path, "/") &&
    !any(parts %in% c("", ".", "..")) && !grepl("[[:cntrl:]]", path)
}

survey_files_fetch <- function(url, destfile, quiet = TRUE, headers = NULL) {
  old <- options(timeout = max(300, getOption("timeout", 60)))
  on.exit(options(old), add = TRUE)
  tryCatch({
    status <- suppressWarnings(utils::download.file(
      url, destfile, method = "libcurl", mode = "wb", quiet = quiet, headers = headers
    ))
    if (!identical(status, 0L)) stop("Download returned status ", status, ".")
  }, error = function(e) {
    stop("Could not download survey files from GitHub. Check your internet connection, ",
         "that the repository and ref are public and available, or retry later if ",
         "GitHub's API rate limit was reached.\n", conditionMessage(e), call. = FALSE)
  })
  invisible(destfile)
}

survey_files_json <- function(url) {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  survey_files_fetch(url, path, headers = c(Accept = "application/vnd.github+json"))
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) {
    stop("Could not read GitHub's file listing: ", conditionMessage(e), call. = FALSE)
  })
}
