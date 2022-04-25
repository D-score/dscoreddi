#' Expand reference
#'
#' Expand reference obtained with `dscore::get_reference` with a count model
#' (based on dmetric/expand_referenced.Rmd)
#' 44.35 - 1.8 * t + 28.47 * log(t + 0.25)
#'
#'
#' @param population A string describing the population. Currently supported
#' are "dutch" and "gcdg" (default).
#' @importFrom dscore get_reference
#' @return
#' @export
#'
#' @examples
expanded_reference <- function(population = "gcdg") {

  dscore::get_reference(population = population) %>%
  select(pop, age, mu) %>%
  bind_rows(data.frame(pop = population, age = seq(3.2, 5, 0.04), mu = NA),
            data.frame(pop = population, age = 0, mu = NA)) %>%
  mutate(
    mu1 = mu,
    mu = ifelse(is.na(mu), 44.35 - 1.8 * age + 28.47 * log(age + 0.25), mu))
}
