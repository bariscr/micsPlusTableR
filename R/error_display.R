# Presentation only: retain the original condition and full diagnostic message.
mics_error_display <- function(error) {
  full <- if (inherits(error, "condition")) mics_error_message(error) else as.character(error)
  fallback <- list(summary = full, details = NULL)
  if (!inherits(error, "condition")) return(fallback)

  tryCatch({
    contexts <- list()
    cause <- error
    while (inherits(cause, "condition")) {
      if (inherits(cause, "mics_user_error") && inherits(cause$parent, "condition")) {
        detail <- mics_error_message(cause$parent)
        message <- conditionMessage(cause)
        suffix <- paste0(":\n", detail)
        if (endsWith(message, suffix)) {
          contexts[[length(contexts) + 1L]] <- list(
            location = substr(message, 1L, nchar(message) - nchar(suffix)),
            cause = detail)
        }
      }
      cause <- cause$parent
    }

    # Use the most specific recognized context. Never infer a condition cell
    # from a result-cell coordinate or a block's starting column.
    for (context in rev(contexts)) {
      location <- context$location
      condition <- NULL
      row <- regmatches(location, regexec(
        "^Worksheet '(.+)'; Excel row ([0-9]+); row condition: ([\\s\\S]*)$",
        location, perl = TRUE))[[1]]
      if (length(row)) {
        location <- paste0("Worksheet '", row[2], "', cell B", row[3])
        condition <- row[4]
        # Expanded row setup belongs in details, not in the short notification.
        if (nchar(condition) > 240L || grepl("[\r\n]", condition)) next
      } else if (!grepl("^Worksheet '.+', cell [A-Z]+[0-9]+( -> Define!B[0-9]+ \\(id '.+'\\))?$",
                        location, perl = TRUE)) {
        next
      }
      reason <- context$cause
      missing <- regmatches(reason, regexec("^object ['‘](.+)['’] not found$", reason))[[1]]
      if (length(missing)) reason <- paste0("Variable '", missing[2], "' was not found.")
      summary <- paste0(location, ":\n", reason,
        if (!is.null(condition)) paste0("\nCondition: ", condition) else "")
      return(list(summary = summary, details = full))
    }
    fallback
  }, error = function(e) fallback)
}

mics_error_notification_ui <- function(error) {
  display <- mics_error_display(error)
  shiny::tags$div(
    shiny::tags$div(style = "white-space: pre-wrap; overflow-wrap: anywhere;",
      paste0("Error: ", display$summary)),
    if (!is.null(display$details)) shiny::tags$details(
      shiny::tags$summary("Technical details"),
      shiny::tags$pre(style = paste("white-space: pre-wrap; overflow-wrap: anywhere;",
        "max-height: 16em; overflow: auto;"), display$details)
    )
  )
}
