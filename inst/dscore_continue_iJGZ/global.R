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

#fixed input for vwc plots
theme_set(theme_light())
linecolors <-  c("black" = "black", "#e6550d" = "#e6550d", "grey60" = "grey60", "#3399ff" = "#3399ff", "maroon"="maroon")
major <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3))
minor <- c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50)
xtick <- c(major, minor) %>% sort
xtick_text <- ifelse(xtick %in% major, xtick, "")



## Example bds data
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
#data is the list of xyz data, so tgt$xyz
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
#notes:
#(1) what to do with walking items (3 items related to age, so translate to practical application into these three items)
#(2) ddigmd255 not showing, why?
#(3) continuous items?


#child data for demo
 references <- dscore::builtin_references %>%
   filter(pop == "phase1") %>%
   mutate(month = age * 12) %>%
   select(month, SDM2:SDP2) %>%
   filter(month <= 60) %>%
   pivot_longer(names_to = "centile", values_to = "d", cols = -month)


