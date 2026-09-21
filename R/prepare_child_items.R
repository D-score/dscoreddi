#' Prepare item-level data for child feedback
#'
#' @param data Wide contact-moment data.
#' @param items Character vector of item columns.
#' @param id,visit,age Column names.
#' @param item_metadata Optional data frame containing `item` and optionally
#'   `label`, `labelNL`, `domain`, `tau` or `month`.
#' @return Long item-level tibble, including observed/pass status.
#' @export
prepare_child_items <- function(data, items, id = "ID",
                                visit = "ContactmomentID", age = "age",
                                item_metadata = NULL) {
  required <- c(id, visit, age, items)
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)

  out <- data |>
    tidyr::pivot_longer(cols = dplyr::all_of(items),
                        names_to = "item", values_to = "score") |>
    dplyr::mutate(
      observed = !is.na(.data$score),
      result = dplyr::case_when(
        .data$score == 1 ~ "Behaald",
        .data$score == 0 ~ "Nog niet behaald",
        TRUE ~ "Niet afgenomen"
      ),
      result = factor(.data$result,
                      levels = c("Behaald", "Nog niet behaald", "Niet afgenomen"))
    )

  if (!is.null(item_metadata)) {
    if (!"item" %in% names(item_metadata)) stop("item_metadata must contain an 'item' column.", call. = FALSE)
    meta <- dplyr::distinct(item_metadata, .data$item, .keep_all = TRUE)
    out <- dplyr::left_join(out, meta, by = "item")
  }
  if (!"label" %in% names(out)) {
    out$label <- if ("labelNL" %in% names(out)) out$labelNL else out$item
  }
  dplyr::arrange(out, .data[[id]], .data[[age]], .data[[visit]], .data$item)
}
