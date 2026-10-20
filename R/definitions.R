# Definitions belong to the workbook being read, never to global/session state.
mics_read_definitions <- function(path) {
  if (!"Define" %in% readxl::excel_sheets(path)) return(NULL)
  cells <- readxl::read_excel(path, sheet = "Define", range = readxl::cell_limits(c(1, 1), c(NA, 2)),
    col_names = FALSE, col_types = "text", .name_repair = "minimal")
  if (!nrow(cells) || !identical(tolower(trimws(as.character(cells[1, ]))), c("id", "code"))) {
    stop("Worksheet 'Define': cells A1 and B1 must contain 'id' and 'code'.", call. = FALSE)
  }
  definitions <- data.frame(id = trimws(cells[[1]][-1]), code = cells[[2]][-1],
    row = seq_len(nrow(cells) - 1L) + 1L, stringsAsFactors = FALSE)
  blank_id <- is.na(definitions$id) | !nzchar(definitions$id)
  blank_code <- is.na(definitions$code) | !nzchar(trimws(definitions$code))
  incomplete <- which(xor(blank_id, blank_code))
  if (length(incomplete)) {
    i <- incomplete[1]
    stop("Worksheet 'Define', row ", definitions$row[i],
      ": each definition needs both an id and code.", call. = FALSE)
  }
  definitions <- definitions[!blank_id, , drop = FALSE]
  definitions
}

# Parse the original source before deparsing: R rewrites native pipes, so
# injecting the data AFTER deparsing would attach it to the wrong mutate().
mics_compile_definition <- function(code) {
  parsed <- parse(text = code, keep.source = TRUE)
  if (!length(parsed)) stop("The definition contains no mutation code.", call. = FALSE)
  sources <- attr(parsed, "srcref")
  valid_chain <- function(x, supplied_data = TRUE) {
    if (supplied_data && identical(x, quote(.mics_data))) return(TRUE)
    if (!is.call(x)) return(FALSE)
    if (identical(x[[1]], as.name("%>%")) && length(x) == 3L) {
      return(valid_chain(x[[2]]) && valid_chain(x[[3]], FALSE))
    }
    is_mutate <- identical(x[[1]], as.name("mutate")) ||
      identical(x[[1]], quote(dplyr::mutate))
    is_mutate && (!supplied_data || (length(x) >= 2L && valid_chain(x[[2]])))
  }
  vapply(sources, function(source) {
    expression <- parse(text = paste0(".mics_data |> ", paste(as.character(source), collapse = "\n")))[[1]]
    if (!valid_chain(expression)) {
      stop("Use only mutate() calls, optionally joined by |> or %>%. ",
        "Keep filter() and unfilter() in the table cell; nested define() calls are not supported.",
        call. = FALSE)
    }
    # Also reject define() nested inside a mutate argument.
    if ("define" %in% all.names(expression, functions = TRUE)) {
      stop("Definitions cannot call define(); call each definition in the table cell.", call. = FALSE)
    }
    paste(deparse(expression, width.cutoff = 500L), collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

mics_definition_instruction <- function(call, definitions, context) {
  if (length(call) != 2L ||
      !(is.symbol(call[[2]]) || (is.character(call[[2]]) && length(call[[2]]) == 1L))) {
    stop("Use define(id), with one definition ID, for example define(hhc).", call. = FALSE)
  }
  id <- as.character(call[[2]])
  if (is.null(definitions)) {
    stop("define(", id, ") needs a worksheet named 'Define' with 'id' and 'code' columns.",
      call. = FALSE)
  }
  matches <- which(definitions$id == id)
  if (!length(matches)) stop("Definition '", id, "' was not found in worksheet 'Define'.", call. = FALSE)
  if (length(matches) > 1L) {
    stop("Definition '", id, "' is duplicated in worksheet 'Define', cells ",
      paste0("A", definitions$row[matches], collapse = ", "), ". IDs must be unique.", call. = FALSE)
  }
  index <- matches[1]
  definition_context <- paste0("Define!B", definitions$row[index], " (id '", id, "')")
  code <- tryCatch(mics_compile_definition(definitions$code[index]),
    error = function(e) mics_abort_context(e, definition_context))
  runtime_context <- paste(context, definition_context, sep = " -> ")
  instruction <- substitute(mics_apply_definition(code = CODE, context = CONTEXT),
    list(CODE = code, CONTEXT = runtime_context))
  paste(deparse(instruction, width.cutoff = 500L), collapse = " ")
}

mics_apply_definition <- function(.data, code, context) {
  execution <- new.env(parent = parent.frame())
  execution$.mics_data <- .data
  tryCatch({
    for (step in code) {
      execution$.mics_data <- eval(parse(text = step)[[1]], envir = execution)
    }
    execution$.mics_data
  }, error = function(e) mics_abort_context(e, context))
}
