# Read mean arguments after display precision has been removed from the statistic.
mics_is_mean <- function(stat_type) {
  grepl("^mean([_ ]?unw)?($|[[:space:](])", trimws(stat_type),
        ignore.case = TRUE)
}

mics_mean_spec <- function(stat_type) {
  out <- list(calculation = stat_type, argument = NULL,
              argument_name = "")
  if (is.na(stat_type) || !mics_is_mean(stat_type)) return(out)
  text <- trimws(stat_type)
  prefix <- regmatches(text, regexpr("^mean([_ ]?unw)?", text, ignore.case = TRUE))
  tail <- trimws(substring(text, nchar(prefix) + 1L))
  name <- if (tolower(prefix) != "mean") "mean_unw" else prefix
  if (!nzchar(tail)) {
    out$calculation <- name
    return(out)
  }
  # Keep legacy `Mean ...` indicator instructions intact.
  if (identical(prefix, "Mean") && !startsWith(tail, "(")) return(out)
  invalid <- function() stop("Invalid mean statistic '", stat_type,
    "': supply a variable, a supported named expression, or a bare mean indicator.", call. = FALSE)
  expr <- tryCatch(parse(text = paste0(name, tail), keep.source = FALSE),
                   error = function(e) invalid())
  if (length(expr) != 1L || !is.call(expr[[1L]]) ||
      !identical(as.character(expr[[1L]][[1L]]), name)) invalid()
  args <- as.list(expr[[1L]])[-1L]
  arg_names <- names(args)
  if (is.null(arg_names)) arg_names <- rep("", length(args))
  if (length(args) > 1L) stop("Invalid mean statistic '", stat_type,
    "': supply one variable or expression and the optional d argument.", call. = FALSE)
  if (length(args)) {
    out$argument <- args[[1L]]
    out$argument_name <- arg_names[[1L]]
    call <- as.call(c(list(as.name(name)), args))
    out$calculation <- paste(deparse(call, width.cutoff = 500L), collapse = " ")
    if (startsWith(text, "Mean ")) out$calculation <- sub("^Mean", "Mean ", out$calculation)
  } else {
    out$calculation <- name
  }
  out
}
