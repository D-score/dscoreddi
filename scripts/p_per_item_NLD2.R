library(gseddata)
library(dplyr)
library(tidyr)

NL2 <- data.frame(gseddata::get_data(cohorts = c(54)))


perweek <-
NL2 %>%
  mutate(ageweeks = (agedays + 1) / 7,
         agemos = agedays/ 365.25 *12) %>%
  mutate(week = ceiling(ageweeks),
         month = ceiling(agemos)) %>%
  pivot_longer(cols = starts_with("ddi"), names_to = "item", values_to = "score") %>%
  group_by(item, week) %>%
  summarise(n = sum(!is.na(score)),
            p = sum(score, na.rm = T)/n,
            .groups = "drop") %>%
  left_join(dscoreddi::itemtableVWO %>% select(ID.VWO2005, labelNL, item), by = "item") %>%
  select(item, ID.VWO2005, labelNL, week, n, p)


permaand <-
  NL2 %>%
  mutate(ageweeks = (agedays + 1) / 7,
         agemos = agedays/ 365.25 *12) %>%
  mutate(week = ceiling(ageweeks),
         month = ceiling(agemos)) %>%
  pivot_longer(cols = starts_with("ddi"), names_to = "item", values_to = "score") %>%
  group_by(item, month) %>%
  summarise(n = sum(!is.na(score)),
            p = sum(score, na.rm = T)/n,
            .groups = "drop") %>%
  left_join(dscoreddi::itemtableVWO %>% select(ID.VWO2005, labelNL, item), by = "item") %>%
  select(item, ID.VWO2005, labelNL, month, n, p)


library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "p_per_maand")
addWorksheet(wb, "p_per_week")
writeData(wb, "p_per_maand", permaand)
writeData(wb, "p_per_week", perweek)
saveWorkbook(wb, file = "results/p_per_item_NLD2.xlsx")
