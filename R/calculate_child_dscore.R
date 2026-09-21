#' Calculate longitudinal D-scores
#'
#' @param data Wide data frame with one row per contact moment.
#' @param items Character vector with D-score item columns. If NULL,
#'   columns starting with "ddi" are used.
#' @param id Name of the child identifier column.
#' @param visit Name of the contact-moment identifier column.
#' @param age Name of the age column.
#' @param age_unit Unit of the age variable.
#' @param key,population,itembank Arguments passed to dscore::dscore().
#' @param min_items Minimum number of observed items required.
#' @param ... Additional arguments passed to dscore::dscore().
#'
#' @return A tibble with identifiers, age, D-score, DAZ, SEM and item count.
#' @export
calculate_child_dscore <- function(
    data,
    items = NULL,
    id = "ID",
    visit = "ContactmomentID",
    age = "age",
    age_unit = c("decimal", "days", "months"),
    key = "gsed",
    itembank = dscoreddi::itembank_vwc,
    population = "dutch",
    min_items = 3L,
    ...) {

  age_unit <- match.arg(age_unit)

  # Identify item columns
  if (is.null(items)) {
    items <- grep("^ddi", names(data), value = TRUE)
  }

  # Check input
  required <- c(id, visit, age, items)
  missing_columns <- setdiff(required, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      "Missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(items) == 0L) {
    stop("No D-score item columns found.", call. = FALSE)
  }

  # Preserve identifiers outside dscore()
  identifiers <- data |>
    dplyr::select(
      dplyr::all_of(c(id, visit, age))
    )

  # Input required by dscore()
  score_input <- data |>
    dplyr::select(
      dplyr::all_of(c(age, items))
    )

  # Calculate scores directly
  scores <- dscore::dscore(
    data = score_input,
    items = items,
    key = key,
    population = population,
    itembank = itembank,
    xname = age,
    xunit = age_unit,
    ...
  ) |>
    tibble::as_tibble()

  # Check that dscore() retained the number of rows
  if (nrow(scores) != nrow(identifiers)) {
    stop(
      paste0(
        "dscore() returned ",
        nrow(scores),
        " rows, but the input contained ",
        nrow(identifiers),
        " rows."
      ),
      call. = FALSE
    )
  }

  # Add the original identifiers and age
  out <- dplyr::bind_cols(
    identifiers,
    scores
  )

  # Avoid duplicate age information if dscore() also returned `a`
  # Keep both:
  #   - the original age column, for example `age`
  #   - `a`, the age returned by dscore()

  out <- out |>
    dplyr::mutate(
      score_available = !is.na(.data$d) &
        .data$n >= min_items,
      d = dplyr::if_else(
        .data$score_available,
        .data$d,
        NA_real_
      )
    )

  if ("daz" %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        daz = dplyr::if_else(
          .data$score_available,
          .data$daz,
          NA_real_
        )
      )
  }

  if ("sem" %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        sem = dplyr::if_else(
          .data$score_available,
          .data$sem,
          NA_real_
        )
      )
  }

  out |>
    dplyr::arrange(
      .data[[id]],
      .data[[age]],
      .data[[visit]]
    )
}
