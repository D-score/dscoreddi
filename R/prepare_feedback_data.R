#' Prepare data for the child feedback app
#'
#' @param data Wide data with one row per child/contact moment.
#' @param items Character vector with D-score item columns. If NULL,
#'   columns starting with "ddi" are selected.
#' @param id Name of the child identifier column.
#' @param visit Name of the contact-moment identifier column.
#' @param age Name of the age column.
#' @param age_unit Unit of the age variable.
#' @param key,population Arguments passed to dscore::dscore().
#' @param min_items Minimum number of observed items required.
#' @param item_metadata Optional item metadata.
#' @param include_domains Whether to calculate domain scores.
#' @param domain_set Domain set passed to dscore::ddomain().
#' @param domains Optional domains to calculate.
#' @param vote_weight Optional vote weight passed to dscore::ddomain().
#' @param ... Additional arguments passed to score functions.
#'
#' @return An object of class dfeedback_data.
#' @export
prepare_feedback_data <- function(
    data,
    items = NULL,
    id = "ID",
    visit = "ContactmomentID",
    age = "age",
    age_unit = c("decimal", "days", "months"),
    key = "gsed",
    population = "dutch",
    min_items = 1L,
    item_metadata = NULL,
    include_domains = FALSE,
    domain_set = NULL,
    domains = NULL,
    vote_weight = NULL,
    ...) {

  age_unit <- match.arg(age_unit)

  if (is.null(items)) {
    items <- grep("^ddi", names(data), value = TRUE)
  }

  required <- c(id, visit, age)
  missing_columns <- setdiff(required, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      "Missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(items) == 0L) {
    stop(
      "No item columns starting with 'ddi' were found.",
      call. = FALSE
    )
  }

  # Only retain children with at least two distinct contact moments.
  eligible_children <- data |>
    dplyr::distinct(
      .data[[id]],
      .data[[visit]]
    ) |>
    dplyr::count(
      .data[[id]],
      name = "n_measurements"
    ) |>
    dplyr::filter(.data$n_measurements >= 2L)

  app_data <- data |>
    dplyr::semi_join(
      eligible_children,
      by = id
    ) |>
    dplyr::arrange(
      .data[[id]],
      .data[[age]],
      .data[[visit]]
    )

  visits <- calculate_child_dscore(
    data = app_data,
    items = items,
    id = id,
    visit = visit,
    age = age,
    age_unit = age_unit,
    key = key,
    population = population,
    min_items = min_items,
    ...
  )

  item_long <- prepare_child_items(
    data = app_data,
    items = items,
    id = id,
    visit = visit,
    age = age,
    item_metadata = item_metadata
  )

  domain_scores <- NULL

  if (isTRUE(include_domains)) {

    if (is.null(domain_set)) {
      stop(
        "Supply domain_set when include_domains = TRUE.",
        call. = FALSE
      )
    }

    domain_scores <- calculate_child_domains(
      data = app_data,
      items = items,
      domain_set = domain_set,
      domains = domains,
      vote_weight = vote_weight,
      id = id,
      visit = visit,
      age = age,
      age_unit = age_unit,
      key = key,
      population = population,
      min_items = min_items,
      ...
    )
  }

  structure(
    list(
      visits = visits,
      items = item_long,
      domains = domain_scores,
      children = eligible_children |>
        dplyr::arrange(
          dplyr::desc(.data$n_measurements),
          .data[[id]]
        ),
      settings = list(
        id = id,
        visit = visit,
        age = age,
        age_unit = age_unit,
        key = key,
        population = population,
        min_items = min_items,
        item_columns = items,
        domain_set = domain_set
      )
    ),
    class = "dfeedback_data"
  )
}
