#' Van Wiechen continue plot
#'
#' @param data data.frame with all information to use in plot, created with
#' `percentile_plot_data` function.
#' @param side character vector "sideA" or "sideB" indicating the side of VWO
#' schema
#' @param age age of the child that is measured in months
#' @param format format of output plot "format = NULL" returns ggplot object.
#' @importFrom cowplot align_plots
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @return
#' @export
#' @examples
#' library(dplyr)
#' itembank <- dscore::builtin_itembank %>%
#' filter(key == "dutch") %>%
#' bind_rows(data.frame(item = "ddifmd028", tau = 72),
#'           data.frame(item = "ddicmm051", tau = 74),
#'           data.frame(item = "ddigmd075", tau = 72)
#'           )
#' expanded_reference <- dscoreddi::expand_reference(population = "dutch")
#' pref <- percentile_plot_data(itembank = itembank,
#' reference = expanded_reference, age = 6, refperc = 90, suggest = 6)
#' vwo_plot(data = pref, side = "sideA", age = 6)
vwo_plot <- function(data, side, age, format = NULL){

  fm1 <- vwo_domein_plot(data = data, domein = "Fijne motoriek", side = side, age = age)
  gm1 <- vwo_domein_plot(data = data, domein = "Grove motoriek", side = side, age = age)
  cm1 <- vwo_domein_plot(data = data, domein = "Communicatie", side = side, age = age)

  allplotslist1 <- align_plots(fm1,cm1,gm1, align = "v")

  if(side == "sideA") {relatieve_lengte <- c(11,9,15)}
  if(side == "sideB") {relatieve_lengte <- c(11.5,10,9)}

  m1 <- arrangeGrob(grobs = allplotslist1, nrow = 3, ncol = 1, heights = relatieve_lengte)
  grid.arrange(m1)



}
