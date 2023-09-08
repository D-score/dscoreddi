
itembank <- dscore::builtin_itembank %>% filter(key == "dutch") %>%
  bind_rows(
    data.frame(item = "ddicmm032", tau = 25.25), #lineare interpolatie 031[tau = 14.5]-033[tau = 36]
    data.frame(item = "ddicmm038", tau = 52), #lineare interpolatie 037[tau=50.1]-039[tau=53.2]
    data.frame(item = "ddifmd028", tau = 72),
    data.frame(item = "ddicmm051", tau = 74),
    data.frame(item = "ddigmd075", tau = 72)
  ) %>%
  mutate(item = ifelse(item == "ddicmd136", "ddicmm035", item),
         item = ifelse(item == "ddicmd148", "ddicmm040", item))

##note voor overgang naar gsed itembank missen we van meer kenmerken de D-score.
#itembank <- dscore::builtin_itembank %>% filter(key == "gsed2212") %>%
#  bind_rows(data.frame(item = "ddifmd028", tau = 72),
#            data.frame(item = "ddicmm051", tau = 74),
#            data.frame(item = "ddigmd075", tau = 72)
#  )


itembank_vwc <- itembank

usethis::use_data(itembank_vwc, overwrite = TRUE)
