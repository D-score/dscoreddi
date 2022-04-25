#' Expand reference
#'
#' Expand reference with a count model (based on dmetric/expand_referenced.Rmd)
#' 44.35 - 1.8 * t + 28.47 * log(t + 0.25)
#'
#'
#' @param reference A data.frame with the same structure as builtin_references.
#' The default is to use dscore::builtin_references.
#' @param population A string describing the population. Currently supported
#' are "dutch" and "gcdg" (default).
#'
#' @return
#' @export
#'
#' @examples
expanded_reference <- function(reference = dscore::get_reference(population = "gcdg"),
                               population = reference$pop[1]
) {

  reference %>%
  select(pop, age, mu) %>%
  bind_rows(data.frame(pop = population, age = seq(3.2, 5, 0.04), mu = NA),
            data.frame(pop = population, age = 0, mu = NA)) %>%
  mutate(
    mu1 = mu,
    mu = ifelse(is.na(mu), 44.35 - 1.8 * age + 28.47 * log(age + 0.25), mu))
}
