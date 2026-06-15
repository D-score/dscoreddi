#' Create age limits for VWO items
#'
#' For each VWO item, determine the minimum and maximum age (in months)
#' within which score propagation is allowed. The limits are based on the
#' contact moment before and after the prescribed month in the Van Wiechen schema.
#'
#' These limits are intended for propagated scores only (forward scoring /
#' backward scoring) and should not be used to censor observed data.
#'
#' @param item_table A data frame containing at least columns `item` and `month`.
#'
#' @return A data frame with one row per item and columns:
#'   `item`, `month`, `min_age`, `max_age`.
#' @export
vwo_item_age_limits <- function(item_table = dscoreddi::itemtableVWO) {

  contact_schedule <- tibble::tibble(
    month   = c(1, 2, 3, 6,  9, 12, 15, 18, 24, 30, 36, 42, 48, 54),
    min_age = c(0, 1, 2, 3,  6,  9, 12, 15, 18, 24, 30, 36, 42, 48),
    max_age = c(2, 3, 6, 9, 12, 15, 18, 24, 30, 36, 42, 48, 54, 78)
  )

  required_cols <- c("item", "month")
  missing_cols <- setdiff(required_cols, names(item_table))

  if (length(missing_cols) > 0) {
    stop(
      "item_table must contain columns: ",
      paste(required_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  item_table |>
    dplyr::left_join(contact_schedule, by = "month")
}
