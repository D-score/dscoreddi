#prefdat

## DUTCH KEY

#plot reference data for the van wiechen continue plot
library(dscoreddi)

dmetric::gsed_model_818_6$transform
scale <- dmetric::smoccmodel$model$transform[2]

refdata <- dmetric::calculate_age_equivalents(itembank = itembank_vwc %>% filter(key == "dutch"),
                                              scalefactor = scale,
                                              p = c(2, 10, 50, 90, 98),
                                              reference = dscoreddi::expanded_reference %>% filter(population == "dutch" & key == "dutch")) #%>% mutate(A90 = ifelse(D90 > 80 & is.na(A90), 80, A90))

#load("data/itemtableVWO.rda")

prefdat <- refdata %>%
  mutate(
    domain = substr(item, 4,5),
    domein = dplyr::recode(domain, "cm" = "Communicatie",
                           "fm" = "Fijne motoriek",
                           "gm" = "Grove motoriek")) %>%
  #rename(leeftijd = A) %>%
  left_join(itemtableVWO %>% select(item, labelNL, ID.VWO2005, month), by = "item") %>%
  mutate(nr = as.numeric(substr(item, 7, 9)),
         # nr = ifelse(nr == 136, 35, nr), #dubbel met nr 035? -- 035 zit neit in model; 136 wel - wijzig op voorhand
         # nr = ifelse(nr == 148, 40, nr), #dubbel met nr 040? -- 040 zit niet in model; 148 wel - wijzig op voorhand
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
         sideB = ifelse(nr %in% c(11:28, 37:51, 66:75, 68.1, 68.2, 68.3), 1, 0),
         A10 = ifelse(is.na(A10), 0, A10),
         A50 = ifelse(is.na(A50), 0, A50),
         A90 = ifelse(is.na(A90), 0, A90)

  ) %>%
  arrange(nr)

## checkdata

vwc::plot_vwc(data = prefdat, domein = "Fijne motoriek")
vwc::plot_vwc(data = prefdat, domein = "Grove motoriek")
vwc::plot_vwc(data = prefdat, domein = "Communicatie")

usethis::use_data(prefdat, overwrite = TRUE)
