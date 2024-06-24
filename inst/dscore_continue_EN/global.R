

## global for van wiechen continue
library(ggplot2)
library(dplyr)
library(tidyr)
library(dscore)
library(dmetric)
library(gridExtra)
library(cowplot)
library(dscoreddi)
library(dinstrument)


theme_set(theme_light())


itembank <- dscore::builtin_itembank %>% filter(key == "dutch") %>%
  bind_rows(data.frame(item = "ddifmd028", tau = 72),
            data.frame(item = "ddicmm051", tau = 74),
            data.frame(item = "ddigmd075", tau = 72)
  )


#expand reference with a count model (based on dmetric/expand_referenced.Rmd)
#44.35 - 1.8 * t + 28.47 * log(t + 0.25)

refdata <- dmetric::calculate_age_equivalents(itembank = itembank, scalefactor = 2.099986, p = c(10, 50, 90), reference = expanded_reference |> filter(population == "dutch" & key == "dutch"))

#load("data/itemtableVWO.rda")

prefdat_EN <- dscoreddi::prefdat |>
  left_join(dscoreddi::itemtableVWO %>% select(item, labelEN), by = "item") %>%
  mutate(labelENnr = paste(nr, labelEN, sep = ". "),
         #labelENn = ifelse(nr %in% 52:55, paste(nr, labelEN, sep = ".* "), labelENn),
         labelENn = ifelse(month < 10, paste(labelENnr, month, sep = " |  "),
                           paste(labelENnr, month, sep = " |") ),
         labelENn = ifelse(nr %in% 52:55, paste(labelENnr, "**", sep = " | "), labelENn)

  ) %>%
  arrange(nr)

#child data for demo
references <- dscore::builtin_references %>%
  filter(population == "dutch" & key == "dutch") %>%
  mutate(month = age * 12) %>%
  select(month, SDM2:SDP2) %>%
  filter(month <= 60) %>%
  pivot_longer(names_to = "centile", values_to = "d", cols = -month)

childdat <- data.frame(month = c(1,2, 3, 6),
                       d = c(12,16,24,35))
