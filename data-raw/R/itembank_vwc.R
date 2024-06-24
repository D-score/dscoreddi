library(dplyr)

itembank <- dscore::builtin_itembank %>% filter(key == "dutch" | key == "gsed2406" & instrument == "ddi") %>%
  bind_rows(
    data.frame(item = "ddicmm032", tau = 25.25, instrument = "ddi", key = "dutch"), #lineare interpolatie 031[tau = 14.5]-033[tau = 36]
    data.frame(item = "ddicmm038", tau = 52, instrument = "ddi", key = "dutch"), #lineare interpolatie 037[tau=50.1]-039[tau=53.2]
    data.frame(item = "ddifmd028", tau = 72, instrument = "ddi", key = "dutch"),
    data.frame(item = "ddicmm051", tau = 74, instrument = "ddi", key = "dutch"),
    data.frame(item = "ddigmd075", tau = 72, instrument = "ddi", key = "dutch"),

  ) %>%
  mutate(item = ifelse(item == "ddicmd136", "ddicmm035", item),
         item = ifelse(item == "ddicmd148", "ddicmm040", item))

##note voor overgang naar gsed itembank missen we van meer kenmerken de D-score.
#items in ddi without gsed difficulty:
gsd <- itembank %>% filter(key == "gsed2406") %>% pull(item)
itembank %>% filter(key  == "dutch" & !item %in% gsd)
#To get the tau for additional items -- look up bordering items in dutch calibration, and place in the same order in gsed calibration so use gsed tau and interpolate
#e.g. for ddifmd003 - between fmd002[13.81] and fmd004[15.29] but closer to fmd002 (about 1/3) so 15.29 + ((15.29-13.81) /3 )
itembank <-
  itembank %>%
  bind_rows(
# between fmd002[13.81] and fmd004[15.29] but closer to fmd002 (about 1/3) so 13.81 + ((15.29-13.81) /3 )
data.frame(item = "ddifmd003", tau =14.3 ,label = "Hands open occasionally", instrument = "ddi", key = "gsed2406", domain = "fm", model = "d", number  = "003"),
# ddigmd058[19.85] - ddigmd006[22.42] at 1/5: 19.82 + ((22.42 - 19.82)/5)
data.frame(item = "ddifmd005", tau =20.34 ,label = "Plays with hands in midline", instrument = "ddi", key = "gsed2406", domain = "fm", model = "d", number  = "005"),
# ddifmd018[60.68] - ddifmd017[61.33] very close to ddifmd018: so 60.75
data.frame(item = "ddifmm019", tau =60.75 ,label = "Takes off shoes and socks (M; can ask parents)", instrument = "ddi", key = "gsed2406", domain = "fm", model = "m", number  = "019"),
# similar item: mdtsed026[67.70] Is able to dress self but not completely
data.frame(item = "ddifmm025", tau = 67.70 ,label = "Can dress (one piece) (M; can ask parents)", instrument = "ddi", key = "gsed2406", domain = "fm", model = "m", number  = "025"),
# similar item; between ddicmd148[56,47] - ddifmd015	[56,88] so 56.65 - similar item: mdtsed014[48.32]Can hold a spoon and take porridge by self, but spills some
data.frame(item = "ddifmd154", tau = 48.32,label = "Eats with spoon without help (M; can ask parents)", instrument = "ddi", key = "gsed2406", domain = "fm", model = "d", number  = "154"),
# very early, lower end of the scale changed a lot, so set at < 5D
data.frame(item = "ddigmd052", tau = 3,label = "Moves arms equally well", instrument = "ddi", key = "gsed2406", domain = "gm", model = "d", number  = "052"),
# very early, lower end of the scale changed a lot, so set at < 5D
data.frame(item = "ddigmd053", tau = 3,label = "Moves legs equally well", instrument = "ddi", key = "gsed2406", domain = "gm", model = "d", number  = "053"),
# before ddifmd003
data.frame(item = "ddigmd054", tau = 14 ,label = "Stays suspended when lifted under the armpits", instrument = "ddi", key = "gsed2406", domain = "gm", model = "d", number  = "054"),
# similar item: mdtgmd001[3.28] Lifts chin off floor
data.frame(item = "ddigmd056", tau = 3.28 ,label = "Lifts chin off table for a moment", instrument = "ddi", key = "gsed2406", domain = "gm", model = "d", number  = "056"), #
# before ddifmd005
data.frame(item = "ddigmd059", tau = 18 ,label = "Flexes or stomps legs while being swung", instrument = "ddi", key = "gsed2406", domain = "gm", model = "d", number  = "059"), #
# similar item: dmcgmd016[61.28] "Kicks football"
data.frame(item = "ddigmd071", tau = 61.28,label = "Kicks ball", instrument = "ddi", key = "gsed2406", domain = "gm", model = "d", number  = "071"), #
#similar item: gs1lic067[48.15] Can your child drink from an open  cup without help
data.frame(item = "ddigmd146", tau = 48.15,label = "Drinks from cup (M; can ask parents)", instrument = "ddi", key = "gsed2406", domain = "gm", model = "d", number  = "146"),
# similar item: gs1lgc016[15.25] Does your child make sounds other than crying?
data.frame(item = "ddicmm032", tau = 15.25 ,label = "Makes varying sounds", instrument = "ddi", key = "gsed2406", domain = "cm", model = "m", number  = "032"),
# between ddicmm037[49.32]-ddicmm039[50.46], so 49.32 + ((50.46-49.32)/2)
data.frame(item = "ddicmm038", tau = 49.89,label = "Understands daily used sentences", instrument = "ddi", key = "gsed2406", domain = "cm", model = "m", number  = "038"),
# similar item mdtfmd033[78.10] Copies a cross
data.frame(item = "ddifmd028", tau = 78.10 ,label = "Copies cross", instrument = "ddi", key = "gsed2406", domain = "fm", model = "d", number  = "028"),
# similar item: mdtlgd032[85.04]Understands the concept of opposites
data.frame(item = "ddicmm051", tau = 85.04 ,label = "Understands analogies and contradictions", instrument = "ddi", key = "gsed2406", domain = "cm", model = "m", number  = "051"),
# similar item mdtgmd032[83.19] Stands on one foot for a longer time  (at least 5 seconds -tmi
data.frame(item = "ddigmd075", tau = 83.19 ,label = "Can stand on one leg for at least 5 seconds", instrument = "ddi", key = "gsed2406", domain = "gm", model = "d", number  = "075"),
) %>% arrange(tau)


itembank_vwc <- itembank

usethis::use_data(itembank_vwc, overwrite = TRUE)
