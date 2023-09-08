

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
