##itemtable vwo
library(dplyr)
library(tidyr)
library(openxlsx)

itemtableVWO <- read.xlsx("data-raw/data/VWO_itemtable.xlsx")



##Updates:
#manual entry 7-5-2021 accounting assessment of v55 at 4 different occasions.
#manual entry that updates column occ with the assessment month in JGZ (13-7-2021)
#manual entry (7-9-2021): fix translation of English to Dutch labels - copied two items where two different gsed names are known for the same label (ddicmm040, ddicmd148: Understands play order/ Begrijps spelopdrachtjes) & (ddicmm035, ddicmm136: Reacts to verbal request/ reageert op modeling verzoek)
#manual edits in mijlpalen (labelNL) voor afstemming met NCJ website (24-8-2023)

itemtableVWO <- itemtableVWO |> mutate(labelNL = ifelse(labelNL == "Kan hurken en weer gaan staan zonder steun en zonder hulp van anderen", "Hurken en weer gaan staan zonder steun en hulp", labelNL))

usethis::use_data(itemtableVWO, overwrite = TRUE)

#check matching results by inspecting labels
#View(itmatch)
#matches fit, so use the matches to translate the ID.VWO2005 names to gsed names
#itmatch$test <- rename_vwo_gsed(itmatch$ID.VWO2005)
#View(itmatch)



#check matching for duchenne data
x <- names(duchenne)

y <-  gsub("Looptloslooptgoedloslooptsoepel_18m", "Looptlos_18m", x)
y <-  gsub("Looptloslooptgoedloslooptsoepel_24m","Looptgoedlos_24m",  y)
y <-  gsub( "Looptloslooptgoedloslooptsoepel_36m","Looptsoepel_36m", y)
y <-  gsub("_.*|lft", "", y)

m <- gsub('[[:space:]]|rl$|"|\\(M)|“|”|,|-|/|\\!|\\(|\\)|\\(RL)',"", itemtableVWO$labelNL)
m <- gsub("º", "graden", m)
m <- gsub("mijofik", "mijenik", m)
m <- gsub("gebeurtenissenthuis", "gebeurtenissenthuisspeelzaal", m)
m <- gsub("cirkel", "circel", m)
m <- gsub("ë", "", m)
m <- gsub("opeenbeen", "opbeen", m)
m <- gsub("buikvrijvandegrond", "vooruit", m)
m <- gsub("alleenoccasion4", "", m)
m <- gsub("bijverticaalzwaaien", "", m)
m <- gsub("waarhoe", "", m)



#check 1
cbind(input = x, match = m[match(y, m)], output = match(y, m))







