#Opschoonscript specifiek voor Utrecht

#In mlCas (tot 2016) is van Wiechen ook buiten BDS-items geregistreerd in Utrecht-specifieke items
#voor de meeste van deze Utrechtse items geldt dat zij niet gelijks zijn aan de BDS-items
#In deze BDS-items moet apart geregistreerd worden of het kind de handeling links en rechts kan,
#Bij de Utrechtse items is viir  geregistreerd of het kind

#vW.Utrecht<- read_excel("variabelelabels.xlsx") %>%
#  select(2,3) %>%
#  dplyr::filter(substr(`Observatietype Code`, 1, 7) == "vW.UTR_")
#names(vwd %>% select(starts_with("vW.Utr")))

## recode +/- to 1/0
df.VWO <- df.VWO %>%
  rename_at(vars(starts_with("vW.UTR_")), ~ str_c("t", .)) %>% #verander naam Utrechtse variabelen om te voorkomen dat Utrechtse variabelen naar NA worden gehercodeerd
  mutate(across(starts_with("vW."),
                ~case_when(.x == "-" ~ 0,
                           .x == "+" ~ 1,
                           .x == "M" ~ 1)),
         across(starts_with("tvW.UTR_"), #hercoderen Utrechtse variabelen
                ~case_when(.x == "Nee" ~ 0,
                           .x == "Nee, links niet" ~ 0,
                           .x == "Nee, rechts niet" ~ 0,
                           .x == "Ja" ~ 1,
                           .x == "Mededeling" ~ 1)))

## hercoderen van LinksRechts variabelen naar any in new columns zonder LinksRechts - check?
df.VWO <- df.VWO %>%
  mutate(vW.2 = ifelse((vW.2L == 1 & vW.2R == 1), 1, 0),
         vW.3 = ifelse((vW.3L == 1 & vW.3R == 1), 1, 0),
         vW.6 = ifelse((vW.6L == 1 & vW.6R == 1), 1, 0),
         vW.9 = ifelse((vW.9L == 1 & vW.9R == 1), 1, 0),
         vW.10 = ifelse((vW.10L == 1 & vW.10R == 1), 1, 0),
         vW.11 = ifelse((vW.11L == 1 & vW.11R == 1), 1, 0),
         vW.13 = ifelse((vW.13L == 1 & vW.13R == 1), 1, 0),
         vW.15 = ifelse((vW.15L == 1 & vW.15R == 1), 1, 0),
         vW.52 = ifelse((vW.52L == 1 & vW.52R == 1), 1, 0),
         vW.53 = ifelse((vW.53L == 1 & vW.53R == 1), 1, 0),
         vW.59 = ifelse((vW.59L == 1 & vW.59R == 1), 1, 0),
         vW.71 = ifelse((vW.71L == 1 & vW.71R == 1), 1, 0),
         vW.75 = ifelse((vW.75L == 1 & vW.75R == 1), 1, 0),
         ## break up some combined ones (v55/v68 into a-c) - deze later nog afh. van leeftijd missing maken. >> hier gaat nog iets mis met 55
         vW.55 = vW.55, #dit is de a eigenlijk
         vW.55b = vW.55,
         vW.55c = vW.55,
         vW.55d = vW.55,
         vW.68a = vW.68,
         vW.68b = vW.68,
         vW.68c = vW.68,
         vW.68 = NULL) %>% #originele weglaten, anders krijgen we 2 dezelfde variabelen
  mutate(vW.2 = case_when(is.na(vW.2) ~ tvW.UTR_D977, #omcoderen Utrechtse variabelen mlCAS
                          TRUE ~ vW.2),
         vW.3 = case_when(is.na(vW.3) ~ tvW.UTR_D979,
                          TRUE ~ vW.3),
         vW.5 = case_when(is.na(vW.5) ~ tvW.UTR_D982,
                          TRUE ~ vW.5),
         vW.6 = case_when(is.na(vW.6) ~ tvW.UTR_D983,
                          TRUE ~ vW.6),
         vW.9 = case_when(is.na(vW.9) ~ tvW.UTR_D1002,
                          TRUE ~ vW.9),
         vW.10 = case_when(is.na(vW.10) ~ tvW.UTR_D1004,
                           TRUE ~ vW.10),
         vW.11 = case_when(is.na(vW.11) ~ tvW.UTR_D868,
                           TRUE ~ vW.11),
         vW.13 = case_when(is.na(vW.13) ~ tvW.UTR_D870,
                           TRUE ~ vW.13),
         vW.15 = case_when(is.na(vW.15) ~ tvW.UTR_D873,
                           TRUE ~ vW.15),
         vW.41 = case_when(is.na(vW.41) ~ tvW.UTR_D2835,
                           TRUE ~ vW.41),
         vW.49 = case_when(is.na(vW.49) ~ tvW.UTR_D851,
                           TRUE ~ vW.49),
         vW.52 = case_when(is.na(vW.52) ~ tvW.UTR_D1010,
                           TRUE ~ vW.52),
         vW.53 = case_when(is.na(vW.53) ~ tvW.UTR_D1012,
                           TRUE ~ vW.53),
         vW.59 = case_when(is.na(vW.59) ~ tvW.UTR_D987,
                           TRUE ~ vW.59),
         vW.71 = case_when(is.na(vW.71) ~ tvW.UTR_D934,
                           TRUE ~ vW.71),
         vW.75 = case_when(is.na(vW.75) ~ tvW.UTR_D839,
                           TRUE ~ vW.75))