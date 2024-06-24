
library(dplyr)
library(dscore)
library(tidyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(cowplot)
library(openxlsx)
library(dscoreddi)
theme_set(theme_light())
# Kans op behalen van mijlpalen

load("~/OneDrive - TNO/Documents/GitHub/dscoreddi/data/smoccmodel.rda")
model <- smoccmodel$model

#scalefactor <- smoccmodel$model$transform[2]
itembank <- dscore::builtin_itembank %>% filter(key == "gsed2406")

#expand reference with a count model (based on dmetric/expand_referenced.Rmd)
#44.35 - 1.8 * t + 28.47 * log(t + 0.25)

expanded_reference <- dscore::get_reference(population = "phase1")

ggplot(expanded_reference, aes(x = age, y = mu))+
  geom_line()+
  geom_line(aes(x = age, y = mu, color = "red"))

refdata <- dmetric::calculate_age_equivalents(itembank = itembank, p = c(10, 50, 70, 90),scalefactor = 4.064264,  reference = expanded_reference) %>% left_join(itembank)






ggplot(refdata %>% filter(instrument == "gs1"),aes(x = reorder(item, -D50),  y= D50)) +
  geom_point()+
  geom_errorbar(aes(ymin = D10, ymax = D90))+
  #geom_hline(yintercept = as.numeric(26))+
  coord_flip()+ xlab("") + ylab("D-score")+
  theme(legend.position = "none")

ggplot(refdata %>% filter(instrument == "gs1" & D10 > 50),
       aes(x = reorder(item, -D50),  y= D50)) +
  geom_point()+
 # geom_errorbar(aes(ymin = D10, ymax = D90))+
  #geom_hline(yintercept = as.numeric(26))+
  coord_flip()+ xlab("") + ylab("D-score")+
  theme(legend.position = "none")


ggplot(refdata %>% filter(instrument == "gs1" & D50 > 58 & D50 < 63),
       aes(x = reorder(label, -D50),  y= D50)) +
  geom_point()+
  #geom_errorbar(aes(ymin = D10, ymax = D90))+
  #geom_hline(yintercept = as.numeric(26))+
  coord_flip()+ xlab("") + ylab("D-score")+
  theme(legend.position = "none")+
  geom_hline(yintercept = 60.61) +
  theme(text = element_text(size = 18))






sf1 <-
  ggplot(refdata %>% filter(instrument == "gs1"),aes(x = reorder(item, -D50),  y= D50)) +
  geom_point(aes(x = reorder(item, -D50),  y= D50))+
  geom_point(aes(x = reorder(item, -D50),  y= D70))+
  geom_point(aes(x = reorder(item, -D50),  y= D90))+
  geom_errorbar(aes(ymin = D50, ymax = D90))+
  #geom_hline(yintercept = as.numeric(26))+
  coord_flip()+ xlab("") + ylab("D-score")+
  theme(legend.position = "none")



## start voorbeeld: 24 maanden - D-score 64.25

library(dinstrument)
data <-dinstrument::sim(n = 1,
                        reference = expanded_reference %>% filter(key == "gsed2406"),
                        age_range = c(2,2),
                        itembank = itembank %>% filter(instrument == "gs1"),
                        pop = "P50")



dcat_algorithm2(itembank = itembank %>% filter(instrument == "gs1"), data = data, scalefactor = 4.064264, reference = expanded_reference%>% filter(key == "gsed2406"), max_length = 30)

dcat_start(itembank = itembank %>% filter(instrument == "gs1"), p =50, reference = expanded_reference%>% filter(key == "gsed2406"), age = 2, scalefactor = 4.064264)


references <- dscore::builtin_references %>%
  filter(population == "phase1") %>%
  mutate(month = age * 12) %>%
  select(month, SDM2:SDP2) %>%
  filter(month <= 60) %>%
  pivot_longer(names_to = "centile", values_to = "d", cols = -month)


ggplot()+
  geom_line(data =references, aes(x = month, y = d, group = centile), color = "#e6550d", size = 0.8, alpha = 0.5) +
  geom_vline(xintercept = 24, lty = 2)+
  geom_hline(yintercept = 64.25, lty = 2)+
  theme(text = element_text(size = 14))+
  geom_point(aes(x=24, y = 64.25))+
  xlab("Age (months)") + ylab("D-score")


itembank %>% filter(instrument == "gs1" & tau > 62 & tau < 66)



refdata2 <- dmetric::calculate_age_equivalents(itembank = itembank%>% filter(instrument == "gs1"), scalefactor = 4.064264,  p = seq(1,99, 1), reference = expanded_reference%>% filter(key == "gsed2406"))


pref2 <- refdata2 %>%
  pivot_longer(cols = c(everything(), -item), names_pattern = "(.)(.*)",
               names_to = c(".value", "percentile")) %>%
  rename(Leeftijd = A, Dscore = D) %>%

  mutate(percentile = as.numeric(percentile)) %>%
  left_join(itembank)

ggplot(pref2,# %>% filter(tau > 58 & tau < 62),
       aes(x = Dscore, y = percentile, group = item, color = domain))+
  geom_line() +
  scale_x_continuous("D-score", limits = c(40,100),
                     breaks = seq(0, 100, 10)) +
  scale_y_continuous("% pass", breaks = seq(0, 100, 20),
                     limits = c(0, 100))+
  geom_vline(xintercept = 60.55, lty = 2)+
  geom_hline(yintercept = 50, lty = 2)+
  geom_point(aes(60.55, 50))+
  theme(legend.position = "none")
