#' Propogate item scores with age limits
#'
#' Apply age limits to propagated item scores in a longitudinal dataset.
#' For each selected item:
#' - a score of 0 is propagated backward only until the lower age limit;
#' - a score of 1 is propagated forward only until the upper age limit.
#'
#' These limits apply to propagated scores only and not to observed/recorded item scores.
#'
#' @param data A data frame containing age, item columns, and optionally an ID column.
#' @param age Unquoted column name containing age in months.
#' @param items Tidyselect expression selecting the item columns
#'   (e.g. `starts_with("ddi")`).
#' @param id Optional unquoted column name identifying children/subjects.
#'   If supplied, the function is applied within each subject.
#' @param item_limits A data frame with age limits per item, typically created by
#'   [vwo_item_age_limits()].
#'
#' @return The input data with the selected item columns updated.
#' @export
propagate_scores <- function(
    data,
    age,
    items,
    id = NULL,
    item_limits = vwo_item_age_limits()
) {

  age_name <- rlang::as_name(rlang::ensym(age))
  id_quo   <- rlang::enquo(id)

  out <- data

  if (!rlang::quo_is_null(id_quo)) {
    out <- dplyr::group_by(out, !!id_quo)
  }


  # internal helper
  limit_propagated_scores <- function(age, score, item_name, item_limits) {

    if (length(age) != length(score)) {
      stop("`age` and `score` must have the same length.", call. = FALSE)
    }

    limits <- item_limits[item_limits$item == item_name, , drop = FALSE]

    if (nrow(limits) == 0) {
      stop("No age limits found for item: ", item_name, call. = FALSE)
    }

    min_age <- limits$min_age[[1]]
    max_age <- limits$max_age[[1]]

    out <- score

    # Backward propagation of 0: only within lower age boundary
    if (any(score == 0, na.rm = TRUE)) {
      last_age_0 <- max(age[score == 0], na.rm = TRUE)
      idx_0 <- age < last_age_0 & age > min_age
      out[idx_0] <- 0
    }

    # Forward propagation of 1: only within upper age boundary
    if (any(score == 1, na.rm = TRUE)) {
      first_age_1 <- min(age[score == 1], na.rm = TRUE)
      idx_1 <- age > first_age_1 & age < max_age
      out[idx_1] <- 1
    }

    out
  }



  out <- out |>
    dplyr::mutate(
      dplyr::across(
        {{ items }},
        ~ limit_propagated_scores(
          age        = .data[[age_name]],
          score      = .x,
          item_name  = dplyr::cur_column(),
          item_limits = item_limits
        )
      )
    ) |>
    dplyr::ungroup()

  out
}


