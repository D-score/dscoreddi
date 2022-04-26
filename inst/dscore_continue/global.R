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
resp <- james_get(host, path = "expand_reference", query = "population=dutch")
expanded_reference <- resp$parsed


expanded_reference <- dscoreddi::expand_reference(population = "dutch")



# child data for demo
references <- reference %>%
  mutate(month = age * 12) %>%
  select(month, SDM2:SDP2) %>%
  filter(month <= 60) %>%
  pivot_longer(names_to = "centile", values_to = "d", cols = -month)

childdat <- data.frame(month = c(1,2, 3, 6),
                       d = c(12,16,24,35))




