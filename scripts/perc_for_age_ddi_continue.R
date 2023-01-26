
library(dplyr)
library(dscore)
library(tidyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(cowplot)
library(openxlsx)
library(dscoreddi)

# Kans op behalen van mijlpalen

load("~/Documents/GitHub/dmetric/data/smoccmodel.rda")
model <- smoccmodel$model

scalefactor <- smoccmodel$model$transform[2]
itembank <- dscore::builtin_itembank %>% filter(key == "dutch")

#expand reference with a count model (based on dmetric/expand_referenced.Rmd)
#44.35 - 1.8 * t + 28.47 * log(t + 0.25)

expanded_reference <- dscore::get_reference(population = "dutch") %>% select(pop, age, mu) %>%
  bind_rows(data.frame(pop = "dutch", age = seq(3.2, 5, 0.04), mu = NA),
            data.frame(pop = "dutch", age = 0, mu = NA)) %>%
  mutate(
    mu1 = mu,
    mu = ifelse(is.na(mu), 44.35 - 1.8 * age + 28.47 * log(age + 0.25), mu))

ggplot(expanded_reference, aes(x = age, y = mu))+
  geom_line()+
  geom_line(aes(x = age, y = mu1, color = "red"))


calculate_age_equivalents <- function (itembank, scalefactor, p = c(10, 50, 90),
                                       reference,
                                       metric = "dscore")
{
  if (!inherits(model, "dmodel"))
    stop("Argument `model` not of class `dmodel`")
  ib <- itembank #changed
  names(ib)[names(ib) == "lex_gsed"] <- "item"
  #if (metric == "logit")
  #  ib$tau <- model$fit$item$b[ib$item]
  scalefactor <- scalefactor #chaned
  pd <- matrix(ib$tau, nrow = nrow(ib), ncol = length(p)) +
    matrix(scalefactor * qlogis(p/100), nrow = nrow(ib),
           ncol = length(p), byrow = TRUE)
  pa <- approx(x = reference$mu, y = reference$age, xout = as.vector(pd))$y *
    12
  pa <- matrix(pa, ncol = length(p))
  pda <- data.frame(ib[, "item"], round(pd, 2), round(pa,
                                                      2))
  names(pda) <- c("item", paste0("D", p), paste0("A",
                                                 p))
  pda
}


refdata <- calculate_age_equivalents(itembank = itembank, scalefactor = scalefactor, p = c(10, 50, 90), reference = expanded_reference)

load("data/itemtableVWO.rda")

pref <- refdata %>%
  #pivot_longer(cols = c(everything(), -item), names_pattern = "(.)(.*)",
  #             names_to = c(".value", "percentiel")) %>%
  mutate(#percentiel = as.numeric(percentiel),
    domain = substr(item, 4,5),
    domein = plyr::revalue(domain, c("cm" = "Communicatie",
                                     "fm" = "Fijne motoriek",
                                     "gm" = "Grove motoriek"))) %>%
  #rename(leeftijd = A) %>%
  left_join(itemtableVWO %>% select(item, labelNL, ID.VWO2005, month), by = "item") %>%
  mutate(nr = as.numeric(substr(item, 7, 9)),
         nr = ifelse(nr == 136, 35, nr),
         nr = ifelse(nr == 148, 40, nr),
         nr = ifelse(nr == 068, 68.1, nr),
         nr = ifelse(nr == 168, 68.2, nr),
         nr = ifelse(nr == 268, 68.3, nr),
         domein = ifelse(nr == 6, "Fijne motoriek", domein),

         labelNLn = paste(nr, labelNL, sep = ". "),
         labelNLn = ifelse(nr %in% 52:55, paste(nr, labelNL, sep = ".* "), labelNLn),
         labelNLn = ifelse(month < 10, paste(labelNLn, month, sep = " |  "), paste(labelNLn, month, sep = " |") ),
         sideA = ifelse(nr %in% c(1:12, 29:38, 52:67), 1, 0),
         sideB = ifelse(nr %in% c(11:28, 37:51, 66:75, 68.1, 68.2, 68.3), 1, 0),
         A10 = ifelse(is.na(A10), 0, A10),
         A50 = ifelse(is.na(A50), 0, A50),
         A90 = ifelse(is.na(A90), 0, A90),
         cont1 = ifelse(nr %in% 52:55, 0, NA),
         cont2 = ifelse(nr %in% 52:55, 12, NA),
         A10 = ifelse(nr %in% 52:55, NA, A10),
         A50 = ifelse(nr %in% 52:55, NA, A50), #to not plot it but have it in the plot
         A90 = ifelse(nr %in% 52:55, NA, A90)
          ) %>%
  arrange(nr)




theme_set(theme_minimal())



ddomain::get_color_domain("dutch")["Communication"]


## t/m 15 maanden
## with extra gridlines for first 12 months

fm <-
  ggplot(pref %>% filter(domein == "Fijne motoriek"& sideA == 1),  #selecteer milestons met A10 < 15 maanden
     aes(x = reorder(labelNLn, -nr),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  geom_hline(yintercept = as.numeric(26))+
  coord_flip()+ xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,12,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,18),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
  ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

cm <-
  ggplot(pref %>% filter(domein == "Communicatie"& sideA == 1),#selecteer milestons met A10 < 15 maanden
          aes(x = reorder(labelNLn, -nr),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  coord_flip() + xlab("") +
  geom_hline(yintercept = as.numeric(26))+
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,12,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,18),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
  ggtitle("Communicatie")

gm <-
  ggplot(pref %>% filter(domein == "Grove motoriek" & sideA == 1), #selecteer milestons met A10 < 15 maanden
         aes(x = reorder(labelNLn, -nr),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  geom_hline(yintercept = as.numeric(26))+
  geom_linerange(aes(ymin = cont1, ymax = cont2), lty = 2)+
  coord_flip()+
  xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,12,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,18),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
  ggtitle("Grove motoriek")



allplotslist <- align_plots(fm,cm,gm, align = "v")

m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(11,9,15))


pdf("results/percentiel_leeftijd_0-15m_gridlines.pdf", onefile = TRUE, width = 10, height = 12)
grid.arrange(m1)
dev.off()





fm <-
  ggplot(pref %>% filter(domein == "Fijne motoriek"& sideB == 1), #selecteer milestons met A10 < 15 maanden
          aes(x = reorder(labelNLn, -nr),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  coord_flip()+ xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,12,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(8,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

cm <-
  ggplot(pref %>% filter(domein == "Communicatie"& sideB == 1),#selecteer milestons met A10 < 15 maanden
           aes(x = reorder(labelNLn, -nr),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  coord_flip() + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,12,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(8,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1)) + ggtitle("Communicatie")

gm <-
  ggplot(pref %>% filter(domein == "Grove motoriek" & sideB == 1), #selecteer milestons met A10 < 15 maanden
     aes(x = reorder(labelNLn, -nr),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  coord_flip()+
  ylab("Leeftijd (maanden)") + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,12,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(8,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
  ggtitle("Grove motoriek")



allplotslist <- align_plots(fm,cm,gm, align = "v")

m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(10.5,9,8.5))


pdf("results/percentiel_leeftijd_15-48m.pdf", onefile = TRUE, width = 10, height = 12)
grid.arrange(m1)
dev.off()



###################################

## plot per domein
pref <- pref %>% filter(sideA ==1 |sideB ==1)

fm_dom <-
    ggplot(pref %>% filter(domein == "Fijne motoriek"), #selecteer milestons met A10 < 15 maanden
           aes(x = reorder(labelNLn, -nr),  y= A50)) +
    geom_point()+
    geom_errorbar(aes(ymin = A10, ymax = A90))+
    scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d"))+ #added
    coord_flip()+
   # theme(legend.position = "none", axis.text.y = element_text(face = fm_bold[[1]], color = fm_highlight[[1]])) +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,12,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
    xlab("")+
    ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

  fm_dom



  cm_dom <-
    ggplot(pref %>% filter(domein == "Communicatie"),#selecteer milestons met A10 < 15 maanden
           aes(x = reorder(labelNLn, -nr),  y= A50)) +
    geom_point()+
    geom_errorbar(aes(ymin = A10, ymax = A90))+
    scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d"))+ #added
    coord_flip()+
    xlab("") +
    scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,12,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
    ggtitle("Communicatie")

  cm_dom


  gm_dom <-
    ggplot(pref %>% filter(domein == "Grove motoriek"), #selecteer milestons met A10 < 15 maanden
           aes(x = reorder(labelNLn, -nr),  y= A50)) +
    geom_point()+
    geom_errorbar(aes(ymin = A10, ymax = A90))+
    geom_linerange(aes(ymin = cont1, ymax = cont2), lty = 2)+
    scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d"))+ #added
    coord_flip()+
    xlab("") +
    scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,12,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
    ggtitle("Grove motoriek")

  gm_dom




  allplotslist <- align_plots(fm_dom,cm_dom,gm_dom, align = "v")

  m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(12,10,11))


  pdf("results/percentiel_perdomein_onepage.pdf", onefile = TRUE, width = 10, height = 12)
  grid.arrange(m1)
  dev.off()


 # allplotslist2 <- align_plots(cm_dom,gm_dom, align = "v")
#  allplotslist2 <- align_plots(cm_dom,gm_dom, align = "v")
  m2.1 <- arrangeGrob(grobs = list(allplotslist[[1]]), nrow = 2, ncol =1, heights = c(12,9))
  m2.2 <- arrangeGrob(grobs = list(allplotslist[[2]], allplotslist[[3]]), nrow = 2, ncol = 1, heights = c(10,11))

  pdf("results/percentiel_perdomein_twopages.pdf", onefile = TRUE, width = 8.5, height = 12)
  grid.arrange(m2.1)
  grid.arrange(m2.2)
  dev.off()



  pdf("results/percentiel_perdomein_threepages_landscape.pdf", onefile = TRUE, width = 12, height = 8.5 )
  fm_dom
  cm_dom
  gm_dom
  dev.off()




###################################

## add DH data as reference


vwtab <- read.xlsx("data-raw/data/vanwiechen_percentage_cm.xlsx")

colnames(vwtab) <- c("nr", "NLlabel", "leeftijdwkn", "wk1", "wk2", "n/N", "%", "SMOCK", "SW", "Perc_D")

vwtab <-
  vwtab %>%
  mutate(mnd1 = wk1 * 7 / 30,
         mnd1 = ifelse(is.na(mnd1), as.numeric(leeftijdwkn), mnd1),
         mnd2 = wk2 * 7 / 30,
         mnd2 = ifelse(is.na(mnd2), as.numeric(leeftijdwkn), mnd2),
         mnd1 = round(mnd1,1),
         mnd2 = round(mnd2,1),
         DH = `%`,
         DHr = round(`%`,0)) %>%
  mutate(
         mndm = rowMeans(select(., c("mnd1","mnd2")), na.rm = T)

  ) %>%
  full_join(pref, by = "nr")







### Percentages Den Haag

## with extra gridlines for first 12 months

fm <-
  ggplot(data = vwtab %>% filter(domein == "Fijne motoriek"& sideA == 1) )+  #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#0072B2", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#0072B2", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = DHr), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip()+ xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18),position = "right")+
  ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

cm <-
  ggplot(vwtab %>% filter(domein == "Communicatie"& sideA == 1) ) + #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#0072B2", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#0072B2", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = DHr), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip() + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18), position = "right")+
  ggtitle("Communicatie")

gm <-
  ggplot(vwtab %>% filter(domein == "Grove motoriek" & sideA == 1))+ #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#0072B2", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#0072B2", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = DHr), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip()+
  ylab("Leeftijd (maanden)") + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Leeftijd (maanden)", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18),
                     sec.axis = sec_axis(trans = ~(.*1), breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3))))+
  ggtitle("Grove motoriek")



allplotslist <- align_plots(fm,cm,gm, align = "v")

m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(11,9,15))


pdf("results/percentiel_leeftijd_0-15m_gridlines_DHpercentages.pdf", onefile = TRUE, width = 10, height = 12)
grid.arrange(m1)
dev.off()




fm <-
  ggplot(vwtab %>% filter(domein == "Fijne motoriek"& sideB == 1))+ #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#0072B2", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#0072B2", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = DHr), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip()+ xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = seq(0,48,3), limits= c(8,48),position = "right")+
  ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

cm <-
  ggplot(vwtab %>% filter(domein == "Communicatie"& sideB == 1))+#selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#0072B2", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#0072B2", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = DHr), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip() + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = seq(0,48,3), limits= c(8,48), position = "right")+
  ggtitle("Communicatie")

gm <-
  ggplot(vwtab %>% filter(domein == "Grove motoriek" & sideB == 1))+ #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#0072B2", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#0072B2", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = DHr), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip()+
  ylab("Leeftijd (maanden)") + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Leeftijd (maanden)", breaks = seq(0,48,3), limits= c(8,48),
                     sec.axis = sec_axis(trans = ~(.*1), breaks = seq(0,48,3)))+
  ggtitle("Grove motoriek")



allplotslist <- align_plots(fm,cm,gm, align = "v")

m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(10.5,9,8.5))


pdf("results/percentiel_leeftijd_15-48m_DHpercentages.pdf", onefile = TRUE, width = 10, height = 12)
grid.arrange(m1)
dev.off()




### Schelesinger-Was

## with extra gridlines for first 12 months

fm <-
  ggplot(data = vwtab %>% filter(domein == "Fijne motoriek"& sideA == 1) )+  #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#009E73", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#009E73", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = SW), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip()+ xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18),position = "right")+
  ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

cm <-
  ggplot(vwtab %>% filter(domein == "Communicatie"& sideA == 1) ) + #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#009E73", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#009E73", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = SW), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip() + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18), position = "right")+
  ggtitle("Communicatie")

gm <-
  ggplot(vwtab %>% filter(domein == "Grove motoriek" & sideA == 1))+ #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#009E73", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#009E73", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = SW), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip()+
  ylab("Leeftijd (maanden)") + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Leeftijd (maanden)", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18),
                     sec.axis = sec_axis(trans = ~(.*1), breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3))))+
  ggtitle("Grove motoriek")



allplotslist <- align_plots(fm,cm,gm, align = "v")

m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(11,9,15))


pdf("results/percentiel_leeftijd_0-15m_gridlines_SWpercentages.pdf", onefile = TRUE, width = 10, height = 12)
grid.arrange(m1)
dev.off()




fm <-
  ggplot(vwtab %>% filter(domein == "Fijne motoriek"& sideB == 1))+ #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#009E73", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#009E73", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = SW), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip()+ xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = seq(0,48,3), limits= c(8,48),position = "right")+
  ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

cm <-
  ggplot(vwtab %>% filter(domein == "Communicatie"& sideB == 1))+#selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#009E73", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#009E73", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = SW), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip() + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = seq(0,48,3), limits= c(8,48), position = "right")+
  ggtitle("Communicatie")

gm <-
  ggplot(vwtab %>% filter(domein == "Grove motoriek" & sideB == 1))+ #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#009E73", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#009E73", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = SW), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip()+
  ylab("Leeftijd (maanden)") + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Leeftijd (maanden)", breaks = seq(0,48,3), limits= c(8,48),
                     sec.axis = sec_axis(trans = ~(.*1), breaks = seq(0,48,3)))+
  ggtitle("Grove motoriek")



allplotslist <- align_plots(fm,cm,gm, align = "v")

m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(10.5,9,8.5))


pdf("results/percentiel_leeftijd_15-48m_SWpercentages.pdf", onefile = TRUE, width = 10, height = 12)
grid.arrange(m1)
dev.off()






### SMOCK

## with extra gridlines for first 12 months

fm <-
  ggplot(data = vwtab %>% filter(domein == "Fijne motoriek"& sideA == 1) )+  #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#CC79A7", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#CC79A7", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = SMOCK), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip()+ xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18),position = "right")+
  ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

cm <-
  ggplot(vwtab %>% filter(domein == "Communicatie"& sideA == 1) ) + #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#CC79A7", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#CC79A7", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = SMOCK), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip() + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18), position = "right")+
  ggtitle("Communicatie")

gm <-
  ggplot(vwtab %>% filter(domein == "Grove motoriek" & sideA == 1))+ #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#CC79A7", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#CC79A7", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = SMOCK), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip()+
  ylab("Leeftijd (maanden)") + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Leeftijd (maanden)", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18),
                     sec.axis = sec_axis(trans = ~(.*1), breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3))))+
  ggtitle("Grove motoriek")



allplotslist <- align_plots(fm,cm,gm, align = "v")

m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(11,9,15))


pdf("results/percentiel_leeftijd_0-15m_gridlines_SMOCKpercentages.pdf", onefile = TRUE, width = 10, height = 12)
grid.arrange(m1)
dev.off()




fm <-
  ggplot(vwtab %>% filter(domein == "Fijne motoriek"& sideB == 1))+ #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#CC79A7", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#CC79A7", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = SMOCK), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip()+ xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = seq(0,48,3), limits= c(8,48),position = "right")+
  ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

cm <-
  ggplot(vwtab %>% filter(domein == "Communicatie"& sideB == 1))+#selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#CC79A7", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#CC79A7", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = SMOCK), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip() + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = seq(0,48,3), limits= c(8,48), position = "right")+
  ggtitle("Communicatie")

gm <-
  ggplot(vwtab %>% filter(domein == "Grove motoriek" & sideB == 1))+ #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = reorder(labelNLn, -nr),  y= A50))+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = A10, ymax = A90))+
  geom_point(aes(x = reorder(labelNLn, -nr),  y= mndm), color = "#CC79A7", alpha = 0.25, pch = 3)+
  geom_errorbar(aes(x = reorder(labelNLn, -nr),ymin = mnd1, ymax = mnd2), color = "#CC79A7", width = 0.5)+
  geom_text(aes(x = reorder(labelNLn, -nr),  y= mndm, label = SMOCK), nudge_x = 0.5, size = 2, check_overlap = T)+
  coord_flip()+
  ylab("Leeftijd (maanden)") + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Leeftijd (maanden)", breaks = seq(0,48,3), limits= c(8,48),
                     sec.axis = sec_axis(trans = ~(.*1), breaks = seq(0,48,3)))+
  ggtitle("Grove motoriek")



allplotslist <- align_plots(fm,cm,gm, align = "v")

m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(10.5,9,8.5))


pdf("results/percentiel_leeftijd_15-48m_SMOCKpercentages.pdf", onefile = TRUE, width = 10, height = 12)
grid.arrange(m1)
dev.off()
