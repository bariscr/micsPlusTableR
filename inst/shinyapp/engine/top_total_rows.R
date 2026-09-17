# Identify the leading Total rows from their labels or explicit total logic.
# Use worksheet coordinates so header/logic/index previews share the same style.
top_total_rows <- function(table) {
  if (!is.data.frame(table) || !"row_index" %in% names(table) || !nrow(table)) {
    return(integer())
  }
  rows <- sort(unique(table$row_index[!is.na(table$row_index)]))
  is_total <- rep(FALSE, nrow(table))
  if ("row_header" %in% names(table)) {
    is_total <- is_total | grepl("^#?\\s*total\\b", trimws(table$row_header),
                                  ignore.case = TRUE, perl = TRUE)
  }
  if ("row_logic" %in% names(table)) {
    is_total <- is_total | grepl("^total[0-9]*==1$",
                                  gsub("\\s+", "", table$row_logic), perl = TRUE)
  }
  total_rows <- table$row_index[which(is_total)]
  rows[as.logical(cumprod(rows %in% total_rows))]
}
