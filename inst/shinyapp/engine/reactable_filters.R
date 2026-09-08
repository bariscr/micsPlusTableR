# R/reactable_filters.R

#' Exact-match filter method for Reactable columns
#'
#' A JavaScript filter method that keeps only rows where the column value
#' exactly equals the filter value. Use it in `reactable::colDef(filterMethod = filterMethod)`.
#'
#' @format A `htmlwidgets::JS` object.
#' @examples
#' \dontrun{
#' # reactable::reactable(
#' #   iris,
#' #   columns = list(
#' #     Species = reactable::colDef(
#' #       filterable = TRUE,
#' #       filterMethod = filterMethod,
#' #       filterInput = selectFilter("iris_tbl")
#' #     )
#' #   ),
#' #   id = "iris_tbl"
#' # )
#' }
#' @export
#' @importFrom htmlwidgets JS
filterMethod <- htmlwidgets::JS("(rows, columnId, filterValue) => {
  return rows.filter(row => row.values[columnId] === filterValue)
}")

#' Select input filter for Reactable (with "All")
#'
#' Creates a **filter input factory** for `reactable::colDef(filterInput = ...)`
#' that renders a `<select>` element listing the unique column values plus an
#' `"All"` option to clear the filter.
#'
#' @param tableId Character ID of the Reactable (the `id` argument you passed to
#'   `reactable::reactable()`).
#' @param style Inline CSS for the `<select>` element. Default `"width: 100%; height: 100%;"`.
#'
#' @return A function of the form `function(values, name) { ... }` that Reactable
#' calls to render the filter input for a column.
#'
#' @examples
#' \dontrun{
#' # columns = list(
#' #   Species = reactable::colDef(
#' #     filterable = TRUE,
#' #     filterMethod = filterMethod,
#' #     filterInput = selectFilter("my_table")
#' #   )
#' # )
#' }
#' @export
#' @importFrom htmltools tags
selectFilter <- function(tableId, style = "width: 100%; height: 100%;") {
  function(values, name) {
    htmltools::tags$select(
      onchange = sprintf("
        const value = event.target.value
        Reactable.setFilter('%s', '%s', value === '__ALL__' ? undefined : value)
      ", tableId, name),
      # "All" clears the filter
      htmltools::tags$option(value = "__ALL__", "All"),
      lapply(unique(values), htmltools::tags$option),
      "aria-label" = sprintf("Filter %s", name),
      style = style
    )
  }
}

#' Minimum range slider filter for numeric columns (Reactable)
#'
#' Creates a **filter input factory** that renders an HTML range slider to filter
#' rows by a minimum value for the column.
#'
#' @param tableId Character ID of the Reactable (the `id` you passed to `reactable()`).
#' @param style Inline CSS for the `<input type='range'>`. Default `"width: 100%;"`.
#'
#' @return A function of the form `function(values, name) { ... }` used by Reactable.
#'
#' @examples
#' \dontrun{
#' # columns = list(
#' #   Sepal.Length = reactable::colDef(
#' #     filterable = TRUE,
#' #     filterInput = minRangeFilter("my_table")
#' #   )
#' # )
#' }
#' @export
#' @importFrom htmltools tags
minRangeFilter <- function(tableId, style = "width: 100%;") {
  function(values, name) {
    values <- stats::na.omit(values)
    oninput <- sprintf("Reactable.setFilter('%s', '%s', this.value)", tableId, name)
    htmltools::tags$input(
      type = "range",
      min = floor(min(values)),
      max = ceiling(max(values)),
      value = floor(min(values)),
      oninput = oninput,
      style = style,
      "aria-label" = sprintf("Filter by minimum %s", name)
    )
  }
}
