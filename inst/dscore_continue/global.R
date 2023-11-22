

## global for van wiechen continue
library(ggplot2)
library(dplyr)
library(tidyr)
library(dscore)
#library(dmetric)
library(gridExtra)
library(cowplot)
library(dscoreddi)
#library(dinstrument)


theme_set(theme_light())

##input from dd; vector of items that were passed
passed_items <- c("ddicmm029", "ddicmm030", "ddifmd001", "ddifmd002", "ddifmd003")


#child data for demo
# references <- dscore::builtin_references %>%
#   filter(pop == "dutch") %>%
#   mutate(month = age * 12) %>%
#   select(month, SDM2:SDP2) %>%
#   filter(month <= 60) %>%
#   pivot_longer(names_to = "centile", values_to = "d", cols = -month)
#
# childdat <- data.frame(month = c(1,2, 3, 6),
#                        d = c(12,16,24,35))


continuous_items <- continuous_item55 <- selected_items <- length(0)

agemos <- 6
suggest <- 4
refperc <- 70
if(!is.na(agemos)){

  if(agemos < 15) {continuous_items <- prefdat %>% filter(nr %in% 52:54) %>% pull(item)}
  if(agemos >= 15){continuous_items <- length(0)}

  if(agemos < 3) {continuous_item55 <- prefdat %>% filter(nr == 55) %>% pull(item)}
  if(agemos >= 3){continuous_item55 <- length(0)}

  item_candidates <- prefdat %>%
    filter((sideA==1 | sideB ==1) & !nr %in% 52:54) %>%
    filter(!item %in% passed_items) %>% #excluded passed items
    pull(item)

  itembank_candidates <- itembank_vwc %>%
    filter(item %in% item_candidates)

  selected_items <- dinstrument::dform1(itembank = itembank_candidates, ageband = agemos, reference = dscoreddi::expanded_reference, population = "dutch", leniency = refperc, n = suggest)$item
}

pref <-
dscoreddi::prefdat %>%
  filter(sideA==1 | sideB ==1) %>% #only use selection voor VWO
  mutate(
    #high light continuous items blue.
    highlight = ifelse(item %in% c(continuous_items, continuous_item55), "#3399ff", "black"),
    highlight = ifelse(item %in% selected_items, "#e6550d", highlight),
    #already passed as grey
    highlight = ifelse(item %in% passed_items, "grey60", highlight),

    bold = ifelse(item %in% c(selected_items, continuous_items, continuous_item55), "bold", "plain"),
    ## dashed line for continuous items - remove estimated target age.
    cont1 = ifelse(nr %in% 52:54, 0, NA),
    cont2 = ifelse(nr %in% 52:54, 12, NA),
    cont1 = ifelse(nr == 55, 1, cont1),
    cont2 = ifelse(nr == 55, 3, cont2),
    A10 = ifelse(nr %in% 52:54, NA, A10),
    A50 = ifelse(nr %in% 52:54, NA, A50), #to not plot it but have it in the plot
    A90 = ifelse(nr %in% 52:54, NA, A90))

#1A80C4 - blue color
#31a354 - green color
#e6550d - red color
