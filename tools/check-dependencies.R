# Audit executable package, Shiny, and engine code (excluding comments).
local({
  paths <- c(list.files("R", pattern = "[.]R$", full.names = TRUE),
    list.files("inst/shinyapp", pattern = "[.]R$", recursive = TRUE, full.names = TRUE))
  qualified <- list()
  calls <- character()
  walk <- function(x, path) {
    if (is.call(x) && is.symbol(x[[1L]])) {
      op <- as.character(x[[1L]])
      if (op %in% c("::", ":::")) {
        package <- as.character(x[[2L]])
        qualified[[package]] <<- union(qualified[[package]], as.character(x[[3L]]))
      } else {
        calls <<- union(calls, op)
        if (op == "loadNamespace" && length(x) >= 2L && is.character(x[[2L]])) {
          package <- x[[2L]]
          qualified[[package]] <<- union(qualified[[package]], "loadNamespace")
        }
      }
    }
    if (is.call(x) || is.expression(x) || is.pairlist(x)) {
      for (i in seq_along(x)) if (!identical(x[[i]], quote(expr = ))) walk(x[[i]], path)
    }
  }
  for (path in paths) walk(parse(path), path)
  imports <- trimws(strsplit(read.dcf("DESCRIPTION")[1L, "Imports"], ",")[[1L]])
  imports <- sub(" .*", "", imports)
  prep <- trimws(strsplit(read.dcf("DESCRIPTION")[1L,
    "Config/micsPlusTableR/prep-packages"], ",")[[1L]])
  if (length(setdiff(prep, imports))) {
    stop("Preparation dependencies must install with the package as Imports: ",
      paste(setdiff(prep, imports), collapse = ", "))
  }
  standard <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
                "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils")
  undeclared <- setdiff(names(qualified), c(imports, standard, "micsPlusTableR"))
  if (length(undeclared)) stop("Undeclared runtime dependencies: ", paste(undeclared, collapse = ", "))
  symbols <- list()
  for (entry in parse("NAMESPACE")) {
    if (!as.character(entry[[1L]]) %in% c("import", "importFrom")) next
    package <- as.character(entry[[2L]])
    symbols[[package]] <- if (as.character(entry[[1L]]) == "import") {
      getNamespaceExports(package)
    } else vapply(as.list(entry)[-(1:2)], as.character, character(1))
  }
  unused <- character()
  for (package in imports) {
    evidence <- sort(union(qualified[[package]], intersect(symbols[[package]], calls)))
    if (!length(evidence)) unused <- c(unused, package)
    else message(package, ": ", paste(head(evidence, 4L), collapse = ", "))
  }
  if (length(unused)) stop("Imports with no runtime usage: ", paste(unused, collapse = ", "))
  message("Validated app/engine/preparation usage for all ", length(imports), " Imports.")
})
