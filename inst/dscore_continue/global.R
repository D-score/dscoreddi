## consumes plumber API at http://127.0.0.1:3387

host <- "http://127.0.0.1:3387"  # interactive
host <- "http://127.0.0.1:8000"  # docker container

## global libraries for van wiechen continue
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(cowplot)

# API client
if (!require("jamesclient")) {
  remotes::install_github("growthcharts/jamesclient")
  require("jamesclient")
}

theme_set(theme_light())

# get Dutch reference
resp <- james_get(host, path = "reference", query = "population=dutch")
reference <- resp$parsed

# load and adapt itembank
itembank <- james_get(host, path = "itembank")$parsed %>%
  filter(key == "dutch") %>%
  bind_rows(data.frame(item = "ddifmd028", tau = 72),
            data.frame(item = "ddicmm051", tau = 74),
            data.frame(item = "ddigmd075", tau = 72)
  )


# get expanded reference
resp <- james_get(host, path = "expanded_reference", query = "population=dutch")
expanded_reference <- resp$parsed


expanded_reference <- dscoreddi::expand_reference(population = "dutch")


# execute a post request
refdata <- james_post(host = host,
                      path = "get_age_equivalents",
                      itembank = itembank,
                      scalefactor = 2.099986,
                      reference = expanded_reference)$parsed

itemtableVWO <- james_get(host = host, path = "itemtableVWO")$parsed

prefdat <- refdata %>%
  mutate(
    domain = substr(item, 4,5),
    domein = dplyr::recode(domain,
                           "cm" = "Communicatie",
                           "fm" = "Fijne motoriek",
                           "gm" = "Grove motoriek")) %>%
  #rename(leeftijd = A) %>%
  left_join(itemtableVWO %>% select(item, labelNL, ID.VWO2005, month), by = "item") %>%
  mutate(nr = as.numeric(substr(item, 7, 9)),
         nr = ifelse(nr == 136, 35, nr),
         nr = ifelse(nr == 148, 40, nr),
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
         sideB = ifelse(nr %in% c(11:28, 37:51, 66:75, 68.1, 68.2), 1, 0),
         A10 = ifelse(is.na(A10), 0, A10),
         A50 = ifelse(is.na(A50), 0, A50),
         A90 = ifelse(is.na(A90), 0, A90)
  ) %>%
  arrange(nr)


# child data for demo
references <- reference %>%
  mutate(month = age * 12) %>%
  select(month, SDM2:SDP2) %>%
  filter(month <= 60) %>%
  pivot_longer(names_to = "centile", values_to = "d", cols = -month)

childdat <- data.frame(month = c(1,2, 3, 6),
                       d = c(12,16,24,35))




