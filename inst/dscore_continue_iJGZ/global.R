#iJGZ

## global for van wiechen continue
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(dscore)
library(dscoreddi)
library(bdsreader)
library(jamesdemodata)


## Example bds data used to initialize app
library(bdsreader)
fn_o <- system.file("extdata", "bds_v2.0", "smocc", "Laura_S.json", package = "jamesdemodata")
tgt <- read_bds(fn_o, append_ddi = TRUE)
tgt$psn$dob <- "2021-01-21"
fn_o <- system.file("extdata", "bds_v2.0", "smocc", "Thomas_S.json", package = "jamesdemodata")
tgt <- read_bds(fn_o, append_ddi = TRUE)
tgt$psn$dob <- "2021-01-18"
fn_o <- system.file("extdata", "bds_v2.0", "smocc", "Mark_S.json", package = "jamesdemodata")
tgt <- read_bds(fn_o, append_ddi = TRUE)
tgt$psn$dob <- "2021-02-14"
fn_o <- system.file("extdata", "bds_v2.0", "smocc", "Iris_S.json", package = "jamesdemodata")
tgt <- read_bds(fn_o, append_ddi = TRUE)
tgt$psn$dob <- "2021-02-16"

#functions to extract relevant information from the bds data.
#data is the list of xyz data, so data = tgt$xyz
get_vwhistory <- function(data){

  data|>
    filter(grepl("^ddi", yname)) |>
  dplyr::select(age, yname, y) |>
    rename(item = yname, value = y) |>
    mutate(item = ifelse(item == "ddigmd055" & (age > 0.5   / 365.25 & age < 42.5  / 365.25),
                         "ddigmd155", item),
           item = ifelse(item == "ddigmd055" & (age > 42.5   / 365.25 & age < 102.5  / 365.25),
                         "ddigmd255", item),
           item = ifelse(item == "ddigmd055" & (age > 105.5   / 365.25 & age < 146.5  / 365.25),
                         "ddigmd355", item),
           value = ifelse(item == "ddigmd055" & (age < 146.5 / 365.25 | age > 1),
                         NA, value),
           item = ifelse(item == "ddigmd068" & (age > 1.75 & age < 2.5),
                         "ddigmd168", item),
           item = ifelse(item == "ddigmd068" & (age > 2.5 & age < 3.5),
                         "ddigmd268", item),
           value = ifelse(item == "ddigmd068" & (age < 0.75 & age > 1.75),
                         NA, value)


    )


}
get_dhistory <- function(data){

 data |>
    filter(grepl("^ddi", yname)) |>
    dplyr::select(age, yname, y) |>
    rename(item = yname, value = y) |>
    mutate(item = ifelse(item == "ddigmd055" & (age > 0.5   / 365.25 & age < 42.5  / 365.25),
                         "ddigmd155", item),
           item = ifelse(item == "ddigmd055" & (age > 42.5   / 365.25 & age < 102.5  / 365.25),
                         "ddigmd255", item),
           item = ifelse(item == "ddigmd055" & (age > 105.5   / 365.25 & age < 146.5  / 365.25),
                         "ddigmd355", item),
           value = ifelse(item == "ddigmd055" & (age < 146.5 / 365.25 | age > 1),
                          NA, value),
           item = ifelse(item == "ddigmd068" & (age > 1.75 & age < 2.5),
                         "ddigmd168", item),
           item = ifelse(item == "ddigmd068" & (age > 2.5 & age < 3.5),
                         "ddigmd268", item),
           value = ifelse(item == "ddigmd068" & (age < 0.75 & age > 1.75),
                          NA, value)) |>
    tidyr::pivot_wider(names_from = "item", values_from = "value") |>
    dscore() |>
    mutate(maand = round(a*12,0),
           leeftijd = a*12,
           d = round(d, 1))

}

vw_history <- get_vwhistory(tgt$xyz)
d_history <- get_dhistory(tgt$xyz)

# age based on dob from tgt$psn
agedays <- as.numeric(Sys.Date() - as.Date(tgt$psn$dob))
agemos_in <- round(agedays / 365.25 * 12)


