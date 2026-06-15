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

table(broad$cohort)



#extract gsed items that were orignially ddi items to link via fixed parameters
ddi_in_gsed <- dscore::builtin_translate |> filter(grepl("^ddi", gsed)) |>
  mutate(tau = get_tau(gsed3))
ddi_in_gsed |> select(gsed, gsed3, tau)

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

#remove fixed parameter for ddigmd061 >> Because fit for fixed parameter was bad
ddi_fixed <- ddi_fixed[setdiff(names(ddi_fixed), "ddigmd061")]

i <- length(varlist$items)
f <- length(ddi_fixed)
model_name <- paste("ddi", i, f, sep = "_")
#final model location:
path <- paste("C:/Users/eekhouti/OneDrive - TNO/TNO - CH - D-score/Team/Work/GSED/phase2/202510", model_name, sep = "/")

model <- dmetric::fit_dmodel(data = data,
                             varlist = varlist,
                             name = model_name,
                             b_fixed = ddi_fixed
)

model$item_fit |> filter((outfit > 1 | infit > 1) & item %in% names(fixed_gsd))


dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")

r <- dmetric::plot_dmodel(data = data,
                          model = model,
                          path = path,
                          ref_name = "preliminary_standards",
                          maxy = 85,
                          xbreaks = seq(0, 80, 10))


model$item_fit |> filter(outfit > 1.1 | infit > 1.1)


