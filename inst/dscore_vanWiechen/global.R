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

## references
references <- dscore::builtin_references %>%
  filter(pop == "dutch") %>%
  mutate(month = age * 12) %>%
  select(month, SDM2:SDP2) %>%
  filter(month <= 60) %>%
  pivot_longer(names_to = "centile", values_to = "d", cols = -month)


## model and observed pass per item
model_input <- dmetric::smoccmodel
data_input <- dmetric::gcdg_lean

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
    left_join(dscore::get_itemtable(), by = "item")

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
  left_join({ddomain::itemtableVWO %>% select(item, labelNL, ID.VWO2005)}, by = "item") %>%
  mutate(labelNL = ifelse(item == "ddicmd148", "Begrijpt spelopdrachtjes (M)", labelNL),
         labelNL = ifelse(item == "ddicmd136", "Reageert op mondeling verzoek (M)", labelNL)) %>%
         drop_na(labelNL)


p_a_all <-
  ggplot(pref, aes(x = Leeftijd, y = percentile, group = item, color = domain))+
  geom_line() +
  xlab("Leeftijd (maanden)") +
  ylab("% pass")

#ggplotly(p_a_all)
