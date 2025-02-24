#' VWC percentiles
#'
#' @param items character vector with items names for selected items
#' @param p percentiles default: `p = c(2, 10, 50, 90, 98)`
#' @param key D-score model key
#' @param population Reference population
#' @param reference data.frame with reference standard, if `reference = NULL` the
#' `key` and `population` arguments are used.
#'
#' @importFrom dscore get_reference
#' @importFrom dmetric calculate_age_equivalents
#'
#' @return data.frame with D-score percentiles and Age percentiles, age in months.
#' @export
#'
#' @examples
#' vwc_percentiles(select_vwc(age = 2))
vwc_percentiles <- function(items,
                            p = c(2, 10, 50, 90, 98),
                            key = "dutch",
                            population = "dutch",
                            reference = dscoreddi::expanded_reference |> filter(population == "dutch" & key == "dutch") ){

  scale <- dscore::builtin_keys |> filter(key == !!key) |> pull(.data$slope)
  ib <- dscore::builtin_itembank |> filter(.data$item %in% items & key == !!key)
  if(is.null(reference)){
    reference <- get_reference(population = population, key = key)
  }


  dmetric::calculate_age_equivalents(itembank = ib,
                                                scalefactor = scale,
                                                p = p,
                                                reference = reference)
}
