#VWD cleaning script (voorbeeld)

# Iris Eekhout
# iris.eekhout@tno.nl
# 12-7-2024


# D-score berekenen met cleaningstappen voor JGZ registratie data
#
# Disclaimer: let op dat je bij doorscoren (doorplussen en terugnemen) wel bias introduceert, omdat je slechts voor een deel van de respondenten extra item responsen imputeert. Je kunt namelijk alleen terugscoren als een kenmerk later niet is behaald, en alleen doorscoren als een kenmerk eerder wel is behaald. En je doet hierbij de aanname dat het onmogelijk is om een niet behaald kenmerk eerder wel behaald te hebben, of een reeds behaald kenmerk later toch niet meer te halen. Hoe dan ook vul je fictieve observaties in en doe je alsof dit werkelijke observaties zijn (de geimputeerde scores worden namelijk op dezelfde manier behandeld/gewogen/gebruikt als geobserveerde scores.
# Als je deze aannames niet kunt/ niet wilt doen, gebruik dan de D-scores op basis van de geobserveerde data uit regel 25.
#

library(dplyr)
library(dscore)
library(ggplot2)

vwd <- childdevdata::gcdg_nld_smocc %>%
  mutate(agemos = agedays/365.25 * 12,
         age = agemos / 12)

set.seed(62548)
subselection <- sample(unique(vwd$subjid), 100)

vwd <- vwd %>% filter(subjid %in% subselection)

#D-score obv geobserveerde/geregistreerde data
dgsed <- vwd %>%
  dscore::dscore(data = .,
                 items = colnames(vwd)[grepl("^ddi",colnames(vwd))],
                 itembank = dscore::builtin_itembank[dscore::builtin_itembank$key == "gsed2212",],
                 key = "gsed2212",
                 population = "dutch")

#Selecteer alleen D-scores met minimaal 3 items
dgsedc <- dgsed %>% filter(n >= 3)


ggplot(dgsedc, aes(a, d))+
  geom_point() +
  ggtitle("D-score voor leeftijd (ruwe data)")


ggplot(dgsedc, aes(a, n))+
  geom_point() +
  ggtitle("Aantal kenmerken in data voor leeftijd (ruwe data)")



ggplot(dgsedc, aes(a, p))+
  geom_point() +
  ggtitle("Kans op behalen van afgenomen kenmerken voor leeftijd (ruwe data)")




## Leeftijdslimiet per leeftijd - Voor elk item is een maand voorgeschreven in het Van Wiechen schema waarop deze afgenomen zou moeten worden. Voor het doorscoren gebruiken we een leeftijdsrange van het de leeftijd op het contactmoment ervoor tot de leeftijd van het contact moment erna.
## Deze restrictie geldt dus ALLEEN voor het doorscoren/terugminnen en niet voor de geobserveerde/geregisteerde data.
cm_max <- data.frame(
  month =   c(1, 2, 3, 6,  9, 12, 15, 18, 24, 30, 36, 42, 48, 55),
  max_lft = c(2, 3, 6, 9, 12, 15, 18, 24, 30, 36, 42, 48, 55, 78),
  min_lft = c(0, 1, 2, 3,  6,  9,  12, 15, 18, 24, 30, 36, 42, 48 )
)

itemtableVWO <- dscoreddi::itemtableVWO %>% select(item, labelNL, occ, month) %>%
  left_join(cm_max, by = "month") %>% select(item, labelNL, occ, month, min_lft, max_lft)

itemtableVWO


doorscoren_limit <- function(age, item, itname, itemtable){

  minage <- itemtable |> filter(item == itname) |> pull(min_lft)
  maxage <- itemtable |> filter(item == itname) |> pull(max_lft)

  if(any(item == 0, na.rm = T)){
    age0 = max(age[which(item == 0)])
    item = ifelse(age < age0 & age > minage, 0, item)

  }
  if(any(item == 1, na.rm = T)){
    age1 = min(age[which(item ==1)])
    item = ifelse(age > age1 & age < maxage, 1, item)
  }
  item
}


start1 <- Sys.time()

# per "id", leeftijd in maanden staat in kolom "agemos"
vwd_add_limit <-
  vwd %>%
  group_by(subjid) %>%
  mutate(across(starts_with("ddi"),
                ~doorscoren_limit(agemos, .,itname = cur_column(), itemtable = itemtableVWO))) %>% ungroup()
end1 <- Sys.time()
end1 - start1


library(tidyr)
start2 <- Sys.time()
### WERKT NOG NIET!!! MOET NOG WEL LUKKEN TOCH?!
vwd_add_limit2 <-
vwd %>%
 pivot_longer(cols = starts_with("ddi"), names_to ="item", values_to = "score") %>%
  left_join(itemtableVWO %>% select(item, min_lft, max_lft), by = "item") %>%
  group_by(subjid, item) %>%
  mutate(max_age_zero = ifelse(any(score == 0), max(agemos[which(score == 0)]), -Inf),
         min_age_one = ifelse(any(score == 1), min(agemos[which(score == 1)]), Inf),

         score = ifelse(is.na(score) & agemos < max_age_zero & agemos > min_lft, 0, score),
         score = ifelse(is.na(score) & agemos > min_age_one & agemos < max_lft, 1, score)
         ) %>%
  select(-max_age_zero, -min_age_one, -min_lft, -max_lft) %>%
  ungroup() %>%
  pivot_wider(names_from = "item", values_from = "score")


end2 <- Sys.time()
end2 - start2





#dscore bepalen obv doorgescoorde items
dgsed_add_limit <- vwd_add_limit %>%
  dscore::dscore(data = .,
                 items = colnames(vwd_add_limit)[grepl("^ddi",colnames(vwd_add_limit))],
                 itembank = dscore::builtin_itembank[dscore::builtin_itembank$key == "gsed2212",],
                 key = "gsed2212",
                 population = "dutch")

dgsed_add_limitc <- dgsed_add_limit %>% filter(n >= 3)


ggplot(dgsed_add_limitc, aes(a, d))+
  geom_point() +
  ggtitle("D-score voor leeftijd (doorscoren met leeftijdslimiet)")


ggplot(dgsed_add_limitc, aes(a, n))+
  geom_point() +
  ggtitle("Aantal kenmerken in data voor leeftijd (doorscoren met leeftijdslimiet)")



ggplot(dgsed_add_limitc, aes(a, p))+
  geom_point() +
  ggtitle("Kans op behalen van afgenomen kenmerken voor leeftijd (doorscoren met leeftijdslimiet)")



#vergelijking (zonder n >= 3 filter, zodat vergelijking kan)

plot(dgsed$daz, dgsed_add_limit$daz)
