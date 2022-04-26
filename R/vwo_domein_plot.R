#' Van Wiechen continue plot for domain
#'
#' @param data data.frame with all information to use in plot, created with
#' `percentile_plot_data` function.
#' @param domein domain to be plotted
#' @param side character vector "sideA" or "sideB" indicating the side of VWO
#' schema
#' @param age age of the child that is measured in months
#' @importFrom dplyr %>%  mutate filter select
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar scale_color_manual
#' geom_hline coord_flip theme scale_y_continuous ggtitle xlab element_text
#' @importFrom stats reorder
#' @return ggplot
#' @export
#'
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
#' p1 <- vwo_domein_plot(data = pref, domein = "Communicatie",  side = "sideA", age = 6)
#' p1
vwo_domein_plot <- function(data, domein, side, age){

  title <- domein
  if(domein == "Fijne motoriek"){title = "Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag"}

  if(side == "sideA"){
    xlab_breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3))
    xlab_limits = c(0,18)}
  if(side == "sideB"){
    xlab_breaks = seq(0,48,3)
    xlab_limits= c(8,50)}

  highlight <- data %>% filter(domein == !!domein & !!rlang::sym(side) == 1) %>%
    arrange(-nr) %>% select(highlight) #added
  bold <- data %>% filter(domein == !!domein & !!rlang::sym(side) == 1) %>%
    arrange(-nr) %>% select(bold)

  plot1 <- ggplot(data %>% filter(domein == !!domein & !!rlang::sym(side) == 1),  #selecteer milestons met A10 < 15 maanden
                  aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) + #changed
    geom_point()+
    geom_errorbar(aes(ymin = A10, ymax = A90))+
    scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d"))+ #added
    geom_hline(yintercept = age)+
    coord_flip()+ xlab("") +
    theme(legend.position = "none", axis.text.y = element_text(face = bold[[1]], color = highlight[[1]])) +
    scale_y_continuous(name = "", breaks = xlab_breaks, limits= xlab_limits ,position = "right")+
    ggtitle(title)
  plot1

}
