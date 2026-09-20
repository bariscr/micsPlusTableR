#' Worksheet statistic types and populations
#'
#' A worksheet's `stat_type` specifies the calculation for a result cell.
#' These instructions drive the application and the R tabulation engine.
#' The User's Guide's Understand the tabulation plan reference introduces the
#' worksheet layout, dataset, filters, conditions, and worked examples.
#'
#' @section Cell scope and direction:
#' Statistics are written below the condition row, from Excel column C onward.
#' The worksheet reader fills blank statistic cells from preceding instructions;
#' a blank is not necessarily a skipped cell or a zero. Direct [calc_cells()]
#' calls instead return NA for missing statistic specifications.
#'
#' Dataset and active filters first define the available records. In horizontal
#' tables, the row condition selects the percentage base and the column
#' condition defines its outcome. In vertical tables, the column condition
#' selects the base and the row condition defines the outcome. Counts, numeric
#' means, and medians use records satisfying both conditions.
#' See [worksheet-conditions] for preparing predicates, numbered total aliases,
#' and household-member bases.
#'
#' A percentage denominator comes from the applicable population, not a nearby
#' count cell. Weighted percentages are 100 times the sum of outcome weights
#' divided by the sum of base weights. Normal application tabulation uses the
#' plan's weight; the low-level [calc_cells()] helper also has an explicit
#' `weighted` argument for tests and code-only calculations.
#'
#' @section Counts and indicator percentages:
#' \describe{
#'   \item{`n`}{Weighted count: sum of weights for qualifying records. Both directions.}
#'   \item{`n_unw`}{Unweighted count of qualifying records. Both directions.
#'     In horizontal tables, the special `hhmembers` column condition instead
#'     sums `HLnum` for qualifying household records with `total == 1`.
#'     The worksheet column alias `totalHL == 1` selects this behavior;
#'     it does not change the weight or apply to vertical tables.}
#'   \item{`p`}{Weighted percentage of the outcome within the base. Both directions.}
#'   \item{`p_unw`}{Unweighted percentage of the row outcome within the column base.
#'     Vertical tables only.}
#'   \item{`mean`}{Bare mean: the same weighted indicator-percentage calculation
#'     as `p`, on a 0-100 scale. It does not select a numeric variable. Both directions.}
#'   \item{`mean_unw`}{Unweighted indicator percentage, on a 0-100 scale.
#'     Both directions. Legacy capitalized `Mean` also calculates this unweighted
#'     indicator percentage; it is distinct from lowercase weighted `mean`.}
#' }
#'
#' @section Numeric summaries:
#' \describe{
#'   \item{`mean(variable)`}{Weighted arithmetic mean of the named numeric variable
#'     among records satisfying both conditions. Both directions.}
#'   \item{`mean_unw(variable)`}{Unweighted arithmetic mean of that variable.
#'     Both directions.}
#'   \item{`median(variable)`}{Unweighted median, even when the plan specifies
#'     a weight. Both directions. `median_unw(variable)` is a vertical-only synonym.}
#'   \item{`mean(newvar = expression)`}{Vertical tables only: create the named
#'     variable from the expression, then calculate its weighted mean for the
#'     applicable records. For example, `mean(expense = HCS8 / 1000)`.}
#' }
#' Missing values of the summarized variable are omitted. Numeric codes for
#' unknown or inapplicable responses are not automatically missing.
#'
#' @section Totals and specialized forms:
#' \describe{
#'   \item{`100`, `100.0`}{Constant 100, not an estimated percentage. Presence in
#'     a worksheet selects horizontal mode and can mark a distribution total.
#'     Zero-base and suppression rules can change the displayed value.}
#'   \item{`p(100)`, `p_unw(100)`}{Vertical tables: return 100 on the first calculated
#'     row, then calculate weighted or unweighted percentages on later rows.}
#'   \item{`n_unw(100)`}{Vertical tables: return 100 on the first calculated row,
#'     then unweighted counts on later rows. Later counts are not percentages.}
#'   \item{`n(100)`}{Recognized only on the first calculated vertical row, returning
#'     100. Use `n` on later rows. Repeating `n(100)` is unsupported.}
#'   \item{`p_sum(variable)`}{Horizontal tables only: for records satisfying both
#'     conditions, calculate 100 times sum(variable) divided by sum(weight).
#'     The numerator is not multiplied by the weight; the prepared variable
#'     must be defined appropriately for this calculation.}
#'   \item{`n1`, `n_unw1`, `p1`}{Calculation variants of `n`, `n_unw`, and `p`
#'     in both directions. Suffixes do not set decimal places.}
#'   \item{`n2`, `n_unw2`}{Vertical-only count variants of `n` and `n_unw`.}
#' }
#' The `(100)` forms are not supported by the horizontal calculator. They refer
#' to the first calculated row, not any row whose label says Total. Prefer the
#' ordinary count and percentage forms unless an existing plan needs a variant:
#' identical calculations do not guarantee identical direction selection,
#' suppression, count linkage, or consistency-check handling.
#'
#' @section Missing conditions:
#' Missing outcome conditions do not contribute to weighted percentage
#' numerators, but their available weights remain in the base denominator.
#' Horizontal unweighted indicator percentages treat missing outcomes as false;
#' vertical unweighted indicator percentages omit missing indicators from their
#' denominator. Define exclusions explicitly if unknown responses should be
#' removed from the base.
#'
#' @section Filters and display:
#' Column B's filter remains global. A transition from a non-mean to a mean type
#' stops an inherited additional horizontal filter unless a new explicit filter
#' starts there. Consecutive bare, variable, and unweighted means keep the active
#' filter; see [tabulate_h()].
#'
#' Every supported statistic accepts optional `d=` display precision without
#' changing its calculation or direction support. For example, `mean(d=1)`,
#' `mean(HCS8, d=0)`, and `p(100, d=2)`. See [statistic-precision].
#' @seealso [tabulate_mics_table()], [statistic-precision]
#' @name statistic-types
NULL
