# Parse condition-row setup instructions separately from the column predicate.
# R's parser finds the end of multiline/nested calls, including quoted brackets.
mics_parse_condition_instruction <- function(x) {
  out <- list(df = NA_character_, weight = NA_character_,
              filter_condition = NA_character_, calculation = NA_character_,
              condition = NA_character_)
  if (is.na(x) || !nzchar(trimws(x))) return(out)
  lines <- strsplit(as.character(x), "\r\n|\r|\n", perl = TRUE)[[1L]]
  filters <- calculations <- predicates <- character()
  i <- 1L
  while (i <= length(lines)) {
    line <- trimws(lines[[i]])
    if (!nzchar(line) || grepl("^-\\s*-\\s*-$", line)) {
      i <- i + 1L
      next
    }
    if (grepl("\\.sav$", line, ignore.case = TRUE)) {
      out$df <- sub("\\.sav$", "", line, ignore.case = TRUE)
    } else if (grepl("^weight\\s+by\\s+", line, ignore.case = TRUE)) {
      out$weight <- sub("^weight\\s+by\\s+", "", line, ignore.case = TRUE)
    } else if (grepl("^(?:dplyr::)?(?:mutate|filter|unfilter|filter_block)\\s*\\(",
                     line, perl = TRUE)) {
      instruction <- line
      repeat {
        parsed <- tryCatch(parse(text = instruction), error = identity)
        if (!inherits(parsed, "error")) break
        if (i == length(lines) || grepl("^\\s*-\\s*-\\s*-\\s*$", lines[[i + 1L]])) {
          stop("Invalid condition-row instruction: ", instruction,
               "\n", conditionMessage(parsed), call. = FALSE)
        }
        i <- i + 1L
        instruction <- paste(instruction, trimws(lines[[i]]), sep = "\n")
      }
      if (length(parsed) != 1L) {
        stop("Use separate lines for condition-row setup instructions.", call. = FALSE)
      }
      if (grepl("\n", instruction, fixed = TRUE)) {
        instruction <- paste(deparse(parsed[[1L]], width.cutoff = 500L), collapse = " ")
      }
      if (grepl("^(?:dplyr::)?mutate\\s*\\(", line, perl = TRUE)) {
        calculations <- c(calculations, instruction)
      } else if (!grepl("^filter_block\\s*\\(", line)) {
        filters <- c(filters, instruction)
      }
    } else {
      predicates <- c(predicates, line)
    }
    i <- i + 1L
  }
  if (length(filters)) out$filter_condition <- paste(filters, collapse = " |> ")
  if (length(calculations)) out$calculation <- paste(calculations, collapse = " |> ")
  if (length(predicates)) out$condition <- paste(predicates, collapse = " ")
  out
}

# Accept the historical inline form as well as separated/multiline row setup.
# Only consume complete leading mutate calls, using R syntax rather than a
# regex that would stop at a nested function's closing parenthesis.
mics_parse_row_instruction <- function(x) {
  rest <- trimws(x)
  calls <- character()
  repeat {
    rest <- sub("^-\\s*-\\s*-[ \t]*(?:\\n|$)", "", rest, perl = TRUE)
    rest <- trimws(rest)
    if (!grepl("^(?:dplyr::)?mutate\\s*\\(", rest, perl = TRUE)) break
    ends <- gregexpr(")", rest, fixed = TRUE)[[1L]]
    end <- NA_integer_
    for (candidate in ends[ends > 0L]) {
      parsed <- tryCatch(parse(text = substr(rest, 1L, candidate)), error = function(e) NULL)
      if (length(parsed) == 1L) {
        end <- candidate
        break
      }
    }
    if (is.na(end)) stop("Invalid row mutation: ", rest, call. = FALSE)
    calls <- c(calls, paste(deparse(parsed[[1L]], width.cutoff = 500L), collapse = " "))
    rest <- trimws(substring(rest, end + 1L))
    rest <- sub("^(\\|>|%>%)\\s*", "", rest, perl = TRUE)
  }
  # Reuse the condition parser for separators and complete multiline predicates.
  out <- mics_parse_condition_instruction(paste(c(calls, rest), collapse = "\n"))
  if (any(!is.na(unlist(out[c("df", "weight", "filter_condition")])))) {
    stop("Row setup supports mutate(); put source, weight and filter instructions in the condition row.",
         call. = FALSE)
  }
  out
}
