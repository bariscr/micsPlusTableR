# Used by both build scripts: every export must have its own contents entry.
local({
  namespace <- parse("NAMESPACE")
  exports <- unlist(lapply(namespace, function(x) {
    if (is.call(x) && identical(x[[1L]], as.name("export"))) {
      vapply(as.list(x)[-1L], as.character, character(1))
    }
  }), use.names = FALSE)
  topics <- vapply(list.files("man", pattern = "[.]Rd$", full.names = TRUE), function(path) {
    rd <- tools::parse_Rd(path)
    name <- Filter(function(x) identical(attr(x, "Rd_tag"), "\\name"), rd)
    paste(unlist(name[[1L]]), collapse = "")
  }, character(1))
  missing <- setdiff(exports, topics)
  if (length(missing)) {
    stop("Exported functions need separate manual contents entries: ",
         paste(missing, collapse = ", "),
         ". Add function-named help topics and regenerate man/.", call. = FALSE)
  }
  message("Validated individual manual entries for all ", length(exports), " exports.")
})
