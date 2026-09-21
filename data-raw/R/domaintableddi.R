library(dplyr)
library(tidyr)
#vwo domains
domaintableddi <-
  dscoreddi::itemtableVWO |>
  tidyr::drop_na(item) |>
  mutate(dscore::decompose_itemnames(item),
         domain = case_when(
           domain == "gm" ~ "grove motoriek",
           domain == "fm" ~ "fijne motoriek",
           domain == "cm" ~ "communicatie",
           .default = NA_character_
         ),
         set = "VWO",
         weight =  1) |>
  select(set, item, domain, weight)



usethis::use_data(
  domaintableddi,
  overwrite = TRUE,
  compress = "xz"
)

