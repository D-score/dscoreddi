#ddi2510

## script to extend the GSED2510 model with ddi items; so to estimate item parameters for gsed2510 model for ddi items.

library(dplyr)
library(tidyr)
library(dscore)
library(ddata)
library(dmetric)
library(gseddata)

#get dutch data of smocc and doove

# Reads Dutch Van Wiechen data from gseddata
# Modeled after gsedscripts::by3_extension.R

# Install packages
# private package: gseddata
if (!requireNamespace("gseddata", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package gseddata needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/gseddata")
}
pkg <- "gseddata"
require(pkg, quietly = TRUE, warn.conflicts = FALSE)
if (packageVersion(pkg) < "1.10.0") {
  stop("Needs gseddata >= 1.10.0")
}
# public package: dscore
pkg <- "dscore"
if (!requireNamespace(pkg, quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package", pkg, "needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dscore")
}
require(pkg, quietly = TRUE, warn.conflicts = FALSE)
if (packageVersion(pkg) < "2.0.4") {
  stop("Needs dscore >= 2.0.4")
}


items_ddi <- dscore::get_itemnames(ins = "ddi")

# Should we include cohorts with DDI data from gseddata::gsed_lean?
include_extra_ddi_cohorts <- TRUE

# Long form
if (include_extra_ddi_cohorts) {
  pad <- gseddata::get_data(adm = ".", items = items_ddi, min_cat = 10)
  pad$itm <- dplyr::filter(pad$itm, value %in% c(0, 1))
  long <- dplyr::left_join(
    pad$itm,
    pad$visit,
    by = c("subjid", "agedays")
  ) |>
    mutate(subjid = as.character(subjid), ins = "DDI", pair = agedays) |>
    dplyr::select(
      cohort,
      country = ctrycd,
      subjid,
      agedays,
      pair,
      ins,
      item,
      response = value
    )
}
table(long$cohort)

# Broad form
dat <- long |>
  pivot_wider(
    id_cols = c(cohort, country, subjid, agedays, pair, ins),
    names_from = item,
    values_from = response
  ) |>
  arrange(subjid, agedays)

table(dat$cohort)



#extract gsed items that were originally ddi items to link as fixed parameters
ddi_in_gsed <- dscore::builtin_translate |> filter(grepl("^ddi", gsed)) |>
  mutate(tau = get_tau(gsed3))

#compare labels between versions:
ddi_in_gsed |> select(gsed, gsed3, label, tau) |>
  left_join(dscore::builtin_itemtable, by = c("gsed" = "item"))


#get fitted rasch model gsed2510
modelgsed2510 <- readRDS("C:/Users/eekhouti/OneDrive - TNO/Projecten/gsed/Phase 2/202510/281_0_phase_1+2/model.Rds")

#extract beta parameters and match to ddi items that are directly used in gsed.
fixed_gsd <- -modelgsed2510$fit$betapar
lookup <- setNames(ddi_in_gsed$gsed, ddi_in_gsed$gsed3)

new_names <- ifelse(names(fixed_gsd) %in% names(lookup),
                    lookup[names(fixed_gsd)],
                    names(fixed_gsd))

names(fixed_gsd) <- new_names
ddi_fixed <- fixed_gsd[names(fixed_gsd) %in% names(dat)]


#prepare data for dmodel fit:
items <- dat |> select(starts_with("ddi")) |> colnames()

#note: data in gseddata is already cleaned - but these steps are common practice so that why we put them here.
## remove items with fewer than 10 responses - 0 removed
nvalid <- colSums(!is.na(dat[, items]))
sum(nvalid < 10)
items[nvalid < 10]
items <- items[nvalid >= 10]

# remove items with fewer than 10 response in least observed category - 0 removed
keep <- sapply(dat[, items], function(x) min(table(x)) >= 10)
sum(!keep)
names(keep[!keep])
items <- items[keep]


# remove items with no spread - 0 removed
keep <- sapply(dat[, items], function(x) sd(x, na.rm = TRUE) > 0.01)
sum(!keep)
names(keep[!keep])
items <- items[keep]


# dplyr::select data
data <- dat %>%
  dplyr::select(all_of(items), subjid, agedays, cohort, country)

varlist <- list(adm = c("subjid", "agedays", "cohort", "country"),
                items = items)

names(ddi_fixed)[names(ddi_fixed) %in% varlist$items]


#define anchor parameters to scale the logit rasch estimates to the gsed2510 scale
modelgsed2510$itembank %>% filter(item %in%  names(lookup))
anchors <- modelgsed2510$itembank %>% filter(item %in%  c("gl1fmd026", "gl1lgd026")) %>% dplyr::select(tau) %>% unlist
anchor_names <- ifelse(c("gl1fmd026", "gl1lgd026") %in% names(lookup),
                    lookup[c("gl1fmd026", "gl1lgd026")],
                    names(fixed_gsd))
names(anchors) <- anchor_names
names(anchors) %in% names(data)

#instead of using anchors its more robust to use the scaling (transform) parameters directly - its a linear transformation. For that to work in fit_dmodel we need to set the data_package = "" and transform = modelgsed2510$transform.


## model try 1
i <- length(varlist$items)
f <- length(ddi_fixed)
model_name <- paste("ddi", i, f, sep = "_")
model_name


model <- dmetric::fit_dmodel(data = data,
                             varlist = varlist,
                             name = model_name,
                             b_fixed = ddi_fixed,
                             data_package = "",
                             transform = modelgsed2510$transform
)

model$item_fit |> filter((outfit > 1 | infit > 1) & item %in% names(fixed_gsd))

#remove fixed parameter for ddigmd061 >> Because fit for fixed parameter was bad
ddi_fixed <- ddi_fixed[setdiff(names(ddi_fixed), "ddigmd061")]


## model try 2

i <- length(varlist$items)
f <- length(ddi_fixed)
model_name <- paste("ddi", i, f, sep = "_")
model_name

model <- dmetric::fit_dmodel(data = data,
                             varlist = varlist,
                             name = model_name,
                             b_fixed = ddi_fixed,
                             data_package = "",
                             transform = modelgsed2510$transform
)

model$item_fit |> filter((outfit > 1 | infit > 1) & item %in% names(fixed_gsd))


#remove fixed parameter for ddifmd010  >> Because fit for fixed parameter was bad
ddi_fixed <- ddi_fixed[setdiff(names(ddi_fixed), "ddifmd010")]


## model try 3

i <- length(varlist$items)
f <- length(ddi_fixed)
model_name <- paste("ddi", i, f, sep = "_")
model_name


model <- dmetric::fit_dmodel(data = data,
                             varlist = varlist,
                             name = model_name,
                             b_fixed = ddi_fixed,
                             data_package = "",
                             transform = modelgsed2510$transform
)

model$item_fit |> filter((outfit > 1 | infit > 1) & item %in% names(fixed_gsd))

#check ICC curves for fixed items to visually inspect if the parameter fits with the observed data

model$data_package <- "gseddata" #hack needed to get colors of plots
icc <- dmetric::plot_p_d_item(data = data, model = model, items = names(ddi_fixed))
icc


#remove fixed parameter for ddicmd044; ddifmd013; ddicmm047
# each have a fitted curved that deviates from the observed data:
# ddicmd044 appears more difficult in ddi than as was estimated for the corresponding gsed item - data from both ddi sources confirm this.
# ddicmm047 seems easier in ddi, the label of the items deviates a bit too: ddi: Talk spontaneously on daily events (M; can ask parents) vs gsed: Talks easily about daily events
# ddifmd013 also appears more difficult in ddi than as estimated in gsed

ddi_fixed <- ddi_fixed[setdiff(names(ddi_fixed), c("ddifmd013", "ddicmm047", "ddicmd044"))]
model$item_fit |>
  dplyr::filter(item %in% c("ddifmd013", "ddicmm047", "ddicmd044"))


## model try 4

i <- length(varlist$items)
f <- length(ddi_fixed)
model_name <- paste("ddi", i, f, sep = "_")
model_name


model <- dmetric::fit_dmodel(data = data,
                             varlist = varlist,
                             name = model_name,
                             b_fixed = ddi_fixed,
                             data_package = "",
                             transform = modelgsed2510$transform
)



#check ICC curves for fixed items to visually inspect if the parameter fits with the observed data

model$data_package <- "gseddata" #hack needed to get colors of plots
icc <- dmetric::plot_p_d_item(data = data, model = model, items =  c("ddifmd013", "ddicmm047", "ddicmd044"))
icc
model$item_fit |>
  dplyr::filter(item %in% c("ddifmd013", "ddicmm047", "ddicmd044"))




model$item_fit |> filter((outfit > 1 | infit > 1) & item %in% names(fixed_gsd))

#check the itemfit of all items:
model$item_fit |> filter(outfit > 1.1 | infit > 1.1)

#check ICC for all items:
model$data_package <- "gseddata" #hack needed to get colors of plots
icc <- dmetric::plot_p_d_item(data = data, model = model, items = varlist$items)
icc


## save final model
#final model location:
model_name
path <- paste("C:/Users/eekhouti/OneDrive - TNO/TNO - CH - D-score/Team/Work/GSED/phase2/202510", model_name, sep = "/")

dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")

model$data_package <- "gseddata"

r <- dmetric::plot_dmodel(data = data,
                          model = model,

                          path = path,
                          ref_name = "preliminary_standards",
                          maxy = 85,
                          xbreaks = seq(0, 80, 10))



## compare with model 2406

library(dscoreddi)
library(tidyr)
library(dplyr)

itembank_gsed2510 <- model$itembank |>
  select(item, tau, label) |>
  mutate(decompose_itemnames(item),
         key = "gsed2510") |>
  select(key, everything())

ibcomp <- itembank_vwc |> filter(key == "gsed2406") |>
  bind_rows(itembank_gsed2510) |> pivot_wider(names_from = "key", values_from = "tau")

cor(ibcomp$gsed2406, ibcomp$gsed2510, use = "pairwise.complete.obs")

library(ggplot2)
ggplot(ibcomp, aes(gsed2406, gsed2510))+
  geom_point()


dsc_2406 <- dscore(data = data, key = "gsed2406", population = "dutch", xname = "agedays", xunit = "days", itembank = dscoreddi::itembank_vwc)
dsc_2510 <- dscore(data = data, key = "gsed2510", population = "GSED-NLD", xname = "agedays", xunit = "days", itembank = itembank_gsed2510)

cor(dsc_2406$daz, dsc_2510$daz, use = "pairwise.complete.obs")
cor(dsc_2406$d, dsc_2510$d, use = "pairwise.complete.obs")

dsc_df <- bind_rows(
  {dsc_2406 |> mutate(key = "gsed2406")},
  {dsc_2510 |> mutate(key = "gsed2510")}
)

ggplot(dsc_df, aes(a*12, d, group = key, color = key)) +
  geom_point() +
  labs(
    x = "Leeftijd (maanden)",
    y = "D-score",
    title = "Vergelijking D-score GSED2406 en GSED2510"
  )


ggplot(dsc_df, aes(a*12, daz, group = key, color = key)) +
  geom_point() +
  labs(
    x = "Leeftijd (maanden)",
    y = "DAZ",
    title = "Vergelijking D-score GSED2406 en GSED2510"
  )
