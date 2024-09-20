#illustratie
library(ggplot2)
library(dplyr)
library(tidyr)
#ophalen uit JAMES? of gewoon dscore package
references <- dscore::builtin_references %>%
  filter(population == "dutch" & key == "dutch") %>%
  mutate(month = age * 12) %>%
  select(month, SDM2:SDP2) %>%
  filter(month <= 60) %>%
  pivot_longer(names_to = "centile", values_to = "d", cols = -month)




childdat <- data.frame(month = c(1,2, 3, 6, 9 ,13,16,21,28),
                       d = c(12,16,24,35, 41,46,49,58,63))

childdat2 <- data.frame(month = c(1,2, 3, 5, 8 ,11,15,22),
                        d = c(11,17,21,35, 40,46,48,60))

## ok D-score buiten richtlijn momenten gemeten

ggplot()+
  geom_line(data =references, aes(x = month, y = d, group = centile), color = "#e6550d", size = 0.8, alpha = 0.5) +
  geom_point(data = childdat, aes(x = month, y = d), size = 2, color = "darkgreen")+
  geom_line(data = childdat, aes(x = month, y = d), size = 1, color = "darkgreen")+
  geom_point(data = childdat2, aes(x = month, y = d), size = 2, color = "darkblue")+
  geom_line(data = childdat2, aes(x = month, y = d), size = 1, color = "darkblue")+
  geom_rect(aes(xmin = 23, xmax = 27, ymin = 0, ymax  = 80), fill = "lightblue", alpha = 0.50)+
  ylab("D-score") +
  scale_x_continuous("Leeftijd (in maanden)", limits = c(0,36),
                     breaks = seq(0, 36, 3))




## niet ok D-score buiten richtlijn momenten gemeten



childdat <- data.frame(month = c(1,2, 3, 6, 9 ,13,16,21,28),
                       d = c(10,12,20,33, 38,42,49,55,57))

childdat2 <- data.frame(month = c(1,2, 3, 5, 8 ,11,15,22),
                        d = c(9,14,18,32, 36,42,45,48))

ggplot()+
  geom_line(data =references, aes(x = month, y = d, group = centile), color = "#e6550d", size = 0.8, alpha = 0.5) +
  geom_point(data = childdat, aes(x = month, y = d), size = 2, color = "darkred")+
  geom_line(data = childdat, aes(x = month, y = d), size = 1, color = "darkred")+
  geom_point(data = childdat2, aes(x = month, y = d), size = 2, color = "purple")+
  geom_line(data = childdat2, aes(x = month, y = d), size = 1, color = "purple")+
  geom_rect(aes(xmin = 23, xmax = 27, ymin = 0, ymax  = 80), fill = "lightblue", alpha = 0.50)+
  ylab("D-score") +
  scale_x_continuous("Leeftijd (in maanden)", limits = c(0,36),
                     breaks = seq(0, 36, 3))




#illustratie voor populatie waarden
library(ggplot2)
library(dplyr)
library(tidyr)

references <- dscore::builtin_references %>%
  filter(population == "dutch" & key == "dutch") %>%
  mutate(month = age * 12) %>%
  select(month, P10,SDM2:SDP2) %>%
  filter(month <= 60) %>%
  pivot_longer(names_to = "centile", values_to = "d", cols = -c(month, P10))

NLdsc1 <- dscore::dscore(childdevdata::gcdg_nld_smocc, xname = "agedays", xunit = "days", key = "dutch", population = "dutch")

NLDsc <- NLdsc1 %>%
  filter(a > 0.8 & a <1.2 | a > 1.8 & a < 2.2) %>%
  slice_sample(n = 400) %>%
  mutate(b10 = ifelse(daz < -1.28, 0, 1),
         agr = ifelse(a < 1.5, 1, 2))




ggplot()+
  geom_line(data =references, aes(x = month/12, y = d, group = centile), color = "#228B22", size = 0.5, alpha = 1)+
  geom_line(data =references %>% filter(centile %in% c("SD0", "SDM2", "SDP2")), aes(x = month/12, y = d, group = centile), color = "#228B22", size = 0.8, alpha =1)+
  #geom_line(data = references, aes(x = month/12, y = P10), color = "red", size = 1)+
  #geom_point(data=NLDsc, aes(x = a, y = d))+
  ylab("D-score")+
  xlab("Leeftijd (jaren)")+
  theme_light()+
  scale_x_continuous(breaks = seq(0, 4, by = 0.25), minor_breaks = seq(0, 4, by = 0.083333), labels = function(x) ifelse(x %% 1 == 0, x, ""), limits = c(0,4) ) +
  scale_y_continuous(breaks = seq(0, 100, by = 5), minor_breaks = seq(0, 100, by = 1), labels = function(y) ifelse(y %% 5 == 0, y, "")) +
  theme(panel.grid.major = element_line(size = 0.8), panel.grid.minor = element_line(size = 0.2))+
  ggtitle("D-score voor leeftijd")


ggplot()+
  geom_line(data =references, aes(x = month/12, y = d, group = centile), color = "#228B22", size = 0.5, alpha = 1)+
  geom_line(data =references %>% filter(centile %in% c("SD0", "SDM2", "SDP2")), aes(x = month/12, y = d, group = centile), color = "#228B22", size = 0.8, alpha =1)+
  #geom_line(data = references, aes(x = month/12, y = P10), color = "red", size = 1)+
  geom_point(data=NLDsc, aes(x = a, y = d))+
  ylab("D-score")+
  xlab("Leeftijd (jaren)")+
  theme_light()+
  scale_x_continuous(breaks = seq(0, 4, by = 0.25), minor_breaks = seq(0, 4, by = 0.083333), labels = function(x) ifelse(x %% 1 == 0, x, ""), limits = c(0,4) ) +
  scale_y_continuous(breaks = seq(0, 100, by = 5), minor_breaks = seq(0, 100, by = 1), labels = function(y) ifelse(y %% 5 == 0, y, "")) +
  theme(panel.grid.major = element_line(size = 0.8), panel.grid.minor = element_line(size = 0.2))+
  ggtitle("D-score voor leeftijd")

ggplot()+
  geom_line(data =references, aes(x = month/12, y = d, group = centile), color = "#228B22", size = 0.5, alpha = 1)+
  geom_line(data =references %>% filter(centile %in% c("SD0", "SDM2", "SDP2")), aes(x = month/12, y = d, group = centile), color = "#228B22", size = 0.8, alpha = 1)+
  geom_line(data = references, aes(x = month/12, y = P10), color = "red", size = 1)+
  geom_point(data=NLDsc, aes(x = a, y = d))+
  ylab("D-score")+
  xlab("Leeftijd (jaren)")+
  theme_light()+
  scale_x_continuous(breaks = seq(0, 4, by = 0.25), minor_breaks = seq(0, 4, by = 0.083333), labels = function(x) ifelse(x %% 1 == 0, x, ""), limits = c(0,4) ) +
  scale_y_continuous(breaks = seq(0, 100, by = 5), minor_breaks = seq(0, 100, by = 1), labels = function(y) ifelse(y %% 5 == 0, y, "")) +
  theme(panel.grid.major = element_line(size = 0.8), panel.grid.minor = element_line(size = 0.2))+
  ggtitle("D-score voor leeftijd")



ggplot()+
  geom_line(data =references, aes(x = month/12, y = d, group = centile), color = "#228B22", size = 0.5, alpha = 1)+
  geom_line(data =references %>% filter(centile %in% c("SD0", "SDM2", "SDP2")), aes(x = month/12, y = d, group = centile), color = "#228B22", size = 0.8, alpha = 1)+
  geom_line(data = references, aes(x = month/12, y = P10), color = "red", size = 1)+
  geom_point(data=NLDsc, aes(x = a, y = d, group = factor(b10), color = factor(b10)))+
  ylab("D-score")+
  xlab("Leeftijd (jaren)")+
  theme_light()+
  scale_x_continuous(breaks = seq(0, 4, by = 0.25), minor_breaks = seq(0, 4, by = 0.083333), labels = function(x) ifelse(x %% 1 == 0, x, ""), limits = c(0,4) ) +
  scale_y_continuous(breaks = seq(0, 100, by = 5), minor_breaks = seq(0, 100, by = 1), labels = function(y) ifelse(y %% 5 == 0, y, "")) +
  theme(panel.grid.major = element_line(size = 0.8), panel.grid.minor = element_line(size = 0.2))+
  theme(legend.position = "none")+
  ggtitle("D-score voor leeftijd")


NLDsc %>% group_by(agr) %>% summarise(below10 = mean(1-b10))
