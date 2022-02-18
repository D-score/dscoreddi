
library(dplyr)
library(dscore)
library(tidyr)
library(ggplot2)
library(plotly)
library(gridExtra)

# Kans op behalen van mijlpalen

load("~/Documents/GitHub/dmetric/data/smoccmodel.rda")
model <- smoccmodel$model

scalefactor <- smoccmodel$model$transform[2]
itembank <- dscore::builtin_itembank %>% filter(key == "dutch")

#expand reference with a count model (based on dmetric/expand_referenced.Rmd)
#44.35 - 1.8 * t + 28.47 * log(t + 0.25)

expanded_reference <- dscore::get_reference(population = "dutch") %>% select(pop, age, mu) %>%
  bind_rows(data.frame(pop = "dutch", age = seq(3.2, 5, 0.04), mu = NA)) %>%
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


pref <- refdata %>%
  #pivot_longer(cols = c(everything(), -item), names_pattern = "(.)(.*)",
  #             names_to = c(".value", "percentiel")) %>%
  mutate(#percentiel = as.numeric(percentiel),
    domain = substr(item, 4,5),
    domein = plyr::revalue(domain, c("cm" = "Communicatie",
                                     "fm" = "Fijne motoriek",
                                     "gm" = "Grove motoriek"))) %>%
  #rename(leeftijd = A) %>%
  left_join(ddomain::itemtableVWO %>% select(item, labelNL, ID.VWO2005), by = "item") %>%
  left_join(model$itemtable %>% select(item, label), by = "item")




theme_set(theme_minimal())



ddomain::get_color_domain("dutch")["Communication"]


## t/m 15 maanden

fm <-
ggplot(pref %>% filter(domein == "Fijne motoriek"& A10 < 15) %>% #selecteer milestons met A10 < 15 maanden
         drop_na(A50) %>% mutate(A10 = ifelse(is.na(A10), 0, A10)) , aes(x = reorder(labelNL, -A50),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  coord_flip()+ xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = seq(0,48,3), limits= c(0,24),position = "right")+
  ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

cm <-
ggplot(pref %>% filter(domein == "Communicatie"& A10 < 15) %>% #selecteer milestons met A10 < 15 maanden
         drop_na(A50) %>% mutate(A10 = ifelse(is.na(A10), 0, A10)) , aes(x = reorder(labelNL, -A50),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  coord_flip() + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = seq(0,48,3), limits= c(0,24), position = "right")+
  ggtitle("Communicatie")

gm <-
ggplot(pref %>% filter(domein == "Grove motoriek" & A10 < 15) %>% #selecteer milestons met A10 < 15 maanden
         drop_na(A50) %>%
         mutate(A10 = ifelse(is.na(A10), 0, A10)) ,
       aes(x = reorder(labelNL, -A50),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  coord_flip()+
  ylab("Leeftijd (maanden)") + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Leeftijd (maanden)", breaks = seq(0,48,3), limits= c(0,24),
                     sec.axis = sec_axis(trans = ~(.*1), breaks = seq(0,48,3)))+
  ggtitle("Grove motoriek")



allplotslist <- align_plots(fm,cm,gm, align = "v")

m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(11,10,14))


pdf("results/percentiel_leeftijd_0-15m.pdf", onefile = TRUE, width = 10, height = 12)
grid.arrange(m1)
dev.off()




fm <-
  ggplot(pref %>% filter(domein == "Fijne motoriek"& A10 < 15) %>% #selecteer milestons met A10 < 15 maanden
           drop_na(A50) %>% mutate(A10 = ifelse(is.na(A10), 0, A10)) , aes(x = reorder(labelNL, -A50),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  coord_flip()+ xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,24),position = "right")+
  ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

cm <-
  ggplot(pref %>% filter(domein == "Communicatie"& A10 < 15) %>% #selecteer milestons met A10 < 15 maanden
           drop_na(A50) %>% mutate(A10 = ifelse(is.na(A10), 0, A10)) , aes(x = reorder(labelNL, -A50),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  coord_flip() + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,24), position = "right")+
  ggtitle("Communicatie")

gm <-
  ggplot(pref %>% filter(domein == "Grove motoriek" & A10 < 15) %>% #selecteer milestons met A10 < 15 maanden
           drop_na(A50) %>%
           mutate(A10 = ifelse(is.na(A10), 0, A10)) ,
         aes(x = reorder(labelNL, -A50),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  coord_flip()+
  ylab("Leeftijd (maanden)") + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Leeftijd (maanden)", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,24),
                     sec.axis = sec_axis(trans = ~(.*1), breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3))))+
  ggtitle("Grove motoriek")



allplotslist <- align_plots(fm,cm,gm, align = "v")

m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(11,10,14))


pdf("results/percentiel_leeftijd_0-15m_gridlines.pdf", onefile = TRUE, width = 10, height = 12)
grid.arrange(m1)
dev.off()





fm <-
  ggplot(pref %>% filter(domein == "Fijne motoriek"& A10 >= 15) %>% #selecteer milestons met A10 < 15 maanden
           drop_na(A50) %>% mutate(A10 = ifelse(is.na(A10), 0, A10)) , aes(x = reorder(labelNL, -A50),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  coord_flip()+ xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = seq(0,48,3), limits= c(15,48),position = "right")+
  ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

cm <-
  ggplot(pref %>% filter(domein == "Communicatie"& A10 >= 15) %>% #selecteer milestons met A10 < 15 maanden
           drop_na(A50) %>% mutate(A10 = ifelse(is.na(A10), 0, A10)) , aes(x = reorder(labelNL, -A50),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  coord_flip() + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "", breaks = seq(0,48,3), limits= c(15,48), position = "right")+
  ggtitle("Communicatie")

gm <-
  ggplot(pref %>% filter(domein == "Grove motoriek" & A10 >= 15) %>% #selecteer milestons met A10 < 15 maanden
           drop_na(A50) %>%
           mutate(A10 = ifelse(is.na(A10), 0, A10)) ,
         aes(x = reorder(labelNL, -A50),  y= A50)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  coord_flip()+
  ylab("Leeftijd (maanden)") + xlab("") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Leeftijd (maanden)", breaks = seq(0,48,3), limits= c(15,48),
                     sec.axis = sec_axis(trans = ~(.*1), breaks = seq(0,48,3)))+
  ggtitle("Grove motoriek")



allplotslist <- align_plots(fm,cm,gm, align = "v")

m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(10,9,8))


pdf("results/percentiel_leeftijd_15-48m.pdf", onefile = TRUE, width = 10, height = 12)
grid.arrange(m1)
dev.off()

