#load updated d-score model
library(dplyr)
library(ggplot2)

dplot <- dmetric::gsed_model_818_6$dscore %>%
  mutate(study = gsub("\\-.*", "", cohort),
         country_ac = gsub("^.{5}", "", cohort),
         country_ac = gsub("\\-", "", country_ac),
         country_ac = substr(country_ac, 1, 3))

dplot %>% group_by(country_ac, study) %>% summarise(n = n())

ref <- dscore::get_reference() %>%
  tidyr::pivot_longer(c(P3:P97), names_to = "percentile", values_to = "d")

countries1 <- unique(dplot$country_ac)[c(1:7,23,25, 10:12)]

countries2 <- unique(dplot$country_ac)[13:24]
countries3 <- unique(dplot$country_ac)[25:28]

theme_set(theme_light())

ggplot() +
  geom_line(data = ref,
       aes(x = age*12, y =d,
                      group = percentile), color = "grey", size = 0.2)  + scale_x_continuous("Age (in months)",limits = c(0, 60), breaks = seq(0, 60, 12)) + scale_y_continuous(paste0("D-score"), breaks = seq(0, 100, 20), limits = c(0,  100))+
  geom_point(data = dplot %>% filter(country_ac %in% countries1), aes(a*12, d, color = country_ac), size = 0.4, shape = 20) +
  facet_wrap(~country_ac, nrow = 3, ncol = 4)+
  theme(legend.position = "none", text = element_text(size = 16))


ggplot() +
  geom_line(data = ref,
            aes(x = age*12, y =d,
                group = percentile), color = "grey")  + scale_x_continuous("Age (in months)",
                                                                           limits = c(0, 80), breaks = seq(0, 80, 12)) + scale_y_continuous(paste0("D-score"), breaks = seq(0, 100, 20), limits = c(0,  100))+
  geom_point(data = dplot %>% filter(country_ac %in% countries2), aes(a*12, d, color = country_ac), size = 0.2) +
  facet_wrap(~country_ac, nrow = 3, ncol = 4)+
  theme(legend.position = "none")
