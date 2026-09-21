#' Calculate domain-specific D-scores
#'
#' @param data Wide data frame with one row per contact moment.
#' @param items Character vector with D-score item columns. If NULL,
#'   columns starting with "ddi" are used.
#' @param domain_set Name of the domain set passed to dscore::ddomain().
#' @param domains Optional character vector with domains to calculate.
#' @param vote_weight Optional minimum vote weight for assigning an item
#'   to a domain.
#' @param id Name of the child identifier column.
#' @param visit Name of the contact-moment identifier column.
#' @param age Name of the age column.
#' @param age_unit Unit of the age variable.
#' @param key,population,itembank Arguments passed to dscore::ddomain().
#' @param min_items Minimum number of observed items required within a domain.
#' @param ... Additional arguments passed to dscore::ddomain().
#'
#' @return A long tibble with one row per child, contact moment and domain.
#' @export
calculate_child_domains <- function(
    data,
    items = NULL,
    domain_set = "VWO",
    domains = NULL,
    vote_weight = NULL,
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

  # --------------------------------------------------------------------------
  # Identify item columns
  # --------------------------------------------------------------------------

  if (is.null(items)) {
    items <- grep(
      pattern = "^ddi",
      x = names(data),
      value = TRUE
    )
  }

  # --------------------------------------------------------------------------
  # Check input
  # --------------------------------------------------------------------------

  required <- c(
    id,
    visit,
    age,
    items
  )

  missing_columns <- setdiff(
    required,
    names(data)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "Missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(items) == 0L) {
    stop(
      "No D-score item columns found.",
      call. = FALSE
    )
  }

  if (
    missing(domain_set) ||
    is.null(domain_set) ||
    length(domain_set) != 1L ||
    is.na(domain_set) ||
    !nzchar(domain_set)
  ) {
    stop(
      "A valid domain_set must be supplied.",
      call. = FALSE
    )
  }

  # --------------------------------------------------------------------------
  # Preserve identifiers outside ddomain()
  # --------------------------------------------------------------------------

  identifiers <- data |>
    dplyr::select(
      dplyr::all_of(
        c(
          id,
          visit,
          age
        )
      )
    )

  # --------------------------------------------------------------------------
  # Input required by ddomain()
  # --------------------------------------------------------------------------

  score_input <- data |>
    dplyr::select(
      dplyr::all_of(
        c(
          age,
          items
        )
      )
    )

  # --------------------------------------------------------------------------
  # Calculate domain scores
  #
  # ddomain() returns a named list. Each list element contains the results
  # for one domain, in the same row order as score_input.
  # --------------------------------------------------------------------------

  domain_results <- dscoreddi::ddomain(
    data = score_input,
    set = domain_set,
    domain = domains,
    vote_weight = vote_weight,
    items = items,
    key = key,
    population = population,
    itembank = itembank,
    domaintable = dscoreddi::domaintableddi,
    xname = age,
    xunit = age_unit,
    ...
  )

  # --------------------------------------------------------------------------
  # Check output structure
  # --------------------------------------------------------------------------

  if (!is.list(domain_results)) {
    stop(
      "ddomain() did not return a list of domain results.",
      call. = FALSE
    )
  }

  if (length(domain_results) == 0L) {
    stop(
      "ddomain() did not return any domain results.",
      call. = FALSE
    )
  }

  if (
    is.null(names(domain_results)) ||
    any(names(domain_results) == "")
  ) {
    names(domain_results) <- paste0(
      "domain_",
      seq_along(domain_results)
    )
  }

  # --------------------------------------------------------------------------
  # Add identifiers to every domain result
  # --------------------------------------------------------------------------

  domain_results <- Map(
    f = function(domain_result, domain_name) {

      domain_result <- tibble::as_tibble(
        domain_result
      )

      # Check that row order can safely be used for binding identifiers
      if (nrow(domain_result) != nrow(identifiers)) {
        stop(
          paste0(
            "ddomain() returned ",
            nrow(domain_result),
            " rows for domain '",
            domain_name,
            "', but the input contained ",
            nrow(identifiers),
            " rows."
          ),
          call. = FALSE
        )
      }

      # Avoid duplicate original age columns.
      #
      # ddomain() commonly returns `a`, which is retained. If it also returns
      # the original age column, remove that column before bind_cols().
      if (age %in% names(domain_result)) {
        domain_result <- domain_result |>
          dplyr::select(
            -dplyr::all_of(age)
          )
      }

      out <- dplyr::bind_cols(
        identifiers,
        domain_result
      )

      # Add domain name
      out$domain <- domain_name

      # Apply minimum number of items within the domain
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

      out
    },
    domain_result = domain_results,
    domain_name = names(domain_results)
  )

  # --------------------------------------------------------------------------
  # Combine domains into one long data frame
  # --------------------------------------------------------------------------

  out <- dplyr::bind_rows(
    domain_results
  )

  # Put identifying variables first
  out <- out |>
    dplyr::relocate(
      dplyr::all_of(id),
      dplyr::all_of(visit),
      dplyr::all_of(age),
      .data$domain
    ) |>
    dplyr::arrange(
      .data[[id]],
      .data[[age]],
      .data[[visit]],
      .data$domain
    )

  out
}
