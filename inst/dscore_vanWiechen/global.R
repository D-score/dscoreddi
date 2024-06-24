#global env.
library(shinydashboard)
library(shinyWidgets)
library(dmetric)
library(dscore)
library(shiny)
library(dplyr)
library(gtools)
library(plotly)
library(tidyr)
library(knitr)
library(kableExtra)
library(gseddata)

## references
references <- dscore::builtin_references %>%
  filter(population == "dutch" & key == "dutch") %>%
  mutate(month = age * 12) %>%
  select(month, SDM2:SDP2) %>%
  filter(month <= 60) %>%
  pivot_longer(names_to = "centile", values_to = "d", cols = -month)

theme_set(theme_light())
## model and observed pass per item
model_input <- dscoreddi::smoccmodel
data_input <- dscoreddi::gcdg_lean

pass <-
  data_input$itm %>%
  filter(item %in% model_input$items) %>%
  left_join(data_input$visit,
            by = c("subjid", "agedays")) %>%
    rename(study = cohort) %>%
    mutate(agemos = agedays / 365.25 * 12,
           agegp = cut(agemos, breaks = seq(0, 60, 1))) %>%
    group_by(item, study, agegp) %>%
    summarise(p = round(100 * mean(value)),
              a = mean(agemos),
              n = n()) %>%
    ungroup %>%
    left_join(dscore::get_itemtable(), by = "item") %>%
  left_join({dscoreddi::itemtableVWO %>% select(item, labelNL)}, by = "item") %>%
  mutate(item_wlab = paste(item, labelNL))

refdata <- dmetric::calculate_age_equivalents(model = model_input$model, p = seq(1,99, 1), reference = dscore::get_reference(population = "dutch"))


pref <- refdata %>%
  pivot_longer(cols = c(everything(), -item), names_pattern = "(.)(.*)",
               names_to = c(".value", "percentile")) %>%
  rename(Leeftijd = A) %>%
  mutate(percentile = as.numeric(percentile)) %>%
  left_join(model_input$model$itemtable) %>%
  mutate(domain = plyr::revalue(domain, c("cm" = "Communication",
                                          "fm" = "Fine motor",
                                          "gm" = "Gross motor"))) %>%
  left_join({dscoreddi::itemtableVWO %>% select(item, labelNL, ID.VWO2005)}, by = "item") %>%
  mutate(labelNL = ifelse(item == "ddicmd148", "Begrijpt spelopdrachtjes (M)", labelNL),
         labelNL = ifelse(item == "ddicmd136", "Reageert op mondeling verzoek (M)", labelNL),
         item_wlab = paste(item, labelNL)) %>%
         drop_na(labelNL)


p_a_all <-
  ggplot(pref, aes(x = Leeftijd, y = percentile, group = item, color = domain))+
  geom_line() +
  xlab("Leeftijd (maanden)") +
  ylab("% pass")

#ggplotly(p_a_all)



itembank <- dscore::builtin_itembank %>% filter(key == "dutch") %>%
  bind_rows(data.frame(item = "ddifmd028", tau = 72),
            data.frame(item = "ddicmm051", tau = 74),
            data.frame(item = "ddigmd075", tau = 72)
  ) %>%
  mutate(
    domain = substr(item, 4,5),
    domein = dplyr::recode(domain, "cm" = "Communicatie",
                           "fm" = "Fijne motoriek",
                           "gm" = "Grove motoriek")) %>%
  #rename(leeftijd = A) %>%
  left_join(dscoreddi::itemtableVWO %>% select(item, labelNL, ID.VWO2005, month), by = "item") %>%
    mutate(nr = as.numeric(substr(item, 7, 9)),
         nr = ifelse(nr == 136, 35, nr),
         nr = ifelse(nr == 148, 40, nr),
         nr = ifelse(nr == 068, 68.1, nr),
         nr = ifelse(nr == 168, 68.2, nr),
         nr = ifelse(nr == 268, 68.3, nr),
         domein = ifelse(nr == 6, "Fijne motoriek", domein),

         labelNLnr = paste(nr, labelNL, sep = ". "),
         #labelNLn = ifelse(nr %in% 52:55, paste(nr, labelNL, sep = ".* "), labelNLn),
         labelNLn = ifelse(month < 10, paste(labelNLnr, month, sep = " |  "),
                           paste(labelNLnr, month, sep = " |") ),
         labelNLn = ifelse(nr %in% 52:55, paste(labelNLnr, "**", sep = " | "), labelNLn),
         sideA = ifelse(nr %in% c(1:12, 29:38, 52:67), 1, 0),
         sideB = ifelse(nr %in% c(11:28, 37:51, 66:75, 68.1, 68.2), 1, 0))

itembankA <- itembank %>% filter(sideA == 1) %>% arrange(nr)

itembankB <- itembank %>% filter(sideB == 1) %>% arrange(nr)
