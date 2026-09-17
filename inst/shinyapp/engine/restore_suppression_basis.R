# A filter boundary controls record selection, not the placement of a table's
# unweighted count. Preserve existing matches, but let an unmatched cell use
# the next n_unw on the same row even when another filter starts in between.
restore_suppression_basis <- function(results) {
  unmatched <- which(is.na(results$col_start_1))
  counts <- results[which(results$stat_type == "n_unw"), , drop = FALSE]
  if (!length(unmatched) || !nrow(counts)) return(results)

  counts <- counts[order(counts$col_index), , drop = FALSE]
  for (i in unmatched) {
    candidates <- which(counts$row_index == results$row_index[i] &
                          counts$col_index >= results$col_index[i])
    if (!length(candidates)) next
    count <- counts[candidates[1L], , drop = FALSE]
    # Do not borrow a count from a different source after a source switch.
    if ("df" %in% names(results) &&
        !identical(count$df[[1L]], results$df[[i]])) next

    preceding <- counts$col_index[counts$row_index == results$row_index[i] &
                                   counts$col_index < results$col_index[i]]
    start <- if (length(preceding)) max(preceding) + 1L else
      min(results$col_index[results$row_index == results$row_index[i]])
    results$n_unw[i] <- count$value[[1L]]
    results$col_start_1[i] <- start
    results$col_end_1[i] <- count$col_index[[1L]]
  }
  results
}
