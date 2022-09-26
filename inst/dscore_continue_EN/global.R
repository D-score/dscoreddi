

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
expanded_reference <- dscore::get_reference(population = "dutch") %>%
  select(pop, age, mu) %>%
  bind_rows(data.frame(pop = "dutch", age = seq(3.2, 5, 0.04), mu = NA),
            data.frame(pop = "dutch", age = 0, mu = NA)) %>%
  mutate(
    mu1 = mu,
    mu = ifelse(is.na(mu), 44.35 - 1.8 * age + 28.47 * log(age + 0.25), mu))

refdata <- dmetric::calculate_age_equivalents(itembank = itembank, scalefactor = 2.099986, p = c(10, 50, 90), reference = expanded_reference)

#load("data/itemtableVWO.rda")

prefdat <- refdata %>%
   mutate(
    domain = substr(item, 4,5),
    domein = dplyr::recode(domain, "cm" = "Communicatie",
                                     "fm" = "Fijne motoriek",
                                     "gm" = "Grove motoriek")) %>%
  #rename(leeftijd = A) %>%
  left_join(dscoreddi::itemtableVWO %>% select(item, labelEN, ID.VWO2005, month), by = "item") %>%
  mutate(nr = as.numeric(substr(item, 7, 9)),
         nr = ifelse(nr == 136, 35, nr),
         nr = ifelse(nr == 148, 40, nr),
         nr = ifelse(nr == 068, 68.1, nr),
         nr = ifelse(nr == 168, 68.2, nr),
         nr = ifelse(nr == 268, 68.3, nr),
         domein = ifelse(nr == 6, "Fijne motoriek", domein),

         labelENnr = paste(nr, labelEN, sep = ". "),
         #labelENn = ifelse(nr %in% 52:55, paste(nr, labelEN, sep = ".* "), labelENn),
         labelENn = ifelse(month < 10, paste(labelENnr, month, sep = " |  "),
                           paste(labelENnr, month, sep = " |") ),
         labelENn = ifelse(nr %in% 52:55, paste(labelENnr, "**", sep = " | "), labelENn),
         sideA = ifelse(nr %in% c(1:12, 29:38, 52:67), 1, 0),
         sideB = ifelse(nr %in% c(11:28, 37:51, 66:75, 68.1, 68.2), 1, 0),
         A10 = ifelse(is.na(A10), 0, A10),
         A50 = ifelse(is.na(A50), 0, A50),
         A90 = ifelse(is.na(A90), 0, A90)

  ) %>%
  arrange(nr)


ggplot(expanded_reference, aes(x = age, y = mu))+
  geom_line()+
  geom_line(aes(x = age, y = mu1, color = "red"))


#child data for demo
references <- dscore::builtin_references %>%
  filter(pop == "dutch") %>%
  mutate(month = age * 12) %>%
  select(month, SDM2:SDP2) %>%
  filter(month <= 60) %>%
  pivot_longer(names_to = "centile", values_to = "d", cols = -month)

childdat <- data.frame(month = c(1,2, 3, 6),
                       d = c(12,16,24,35))
