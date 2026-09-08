# R/filter_second.R

#' Filter rows where the second column is not NA
#'
#' A small helper that drops rows where the **second column** of a data frame
#' is `NA`. Useful for cleaning comparison tables where the second variable is
#' expected to contain meaningful values.
#'
#' @param . A data frame or tibble.
#'
#' @return The same data frame but with rows removed where the second column
#'   was `NA`.
#'
#' @examples
#' df <- data.frame(x = 1:4, y = c(NA, 2, 3, NA))
#' filter_second(df)
#' # returns rows 2 and 3
#'
#' @export
#' @importFrom dplyr filter
filter_second <- \(.) dplyr::filter(., !is.na(.[[2]]))
