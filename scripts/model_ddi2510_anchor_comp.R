#ddi2510 - check anchors and transformation

# Key issue:
# In fit_dmodel(), `anchors`, `transform`, and `data_package` interact in a way
# that can lead to unintended transformations. When `data_package = "gseddata"`
# and `anchors = NULL`, two default DDI anchors are inserted automatically.
# Therefore, a user-supplied `transform` is only used as intended when
# `data_package = ""`. For linking the DDI extension to GSED2510, explicit
# anchors are needed to ensure that fixed beta parameters are transformed to
# tau values on the same D-score scale as the core model.


# Notes on fit_dmodel(): anchors, transform, and data_package
#
# This script checks how beta parameters from the Rasch model are transformed
# to tau parameters on the D-score scale. This transformation is linear:
# tau = intercept + slope * beta/difficulty.
#
# Main finding:
# - Fixed beta parameters alone are not sufficient to guarantee that the
#   resulting tau estimates align with the original/core GSED2510 model.
# - The tau estimates depend on the linear transformation that is applied after
#   fitting the Rasch model.
#
# Important behaviour in fit_dmodel():
# - If `anchors` are supplied, these items are used to estimate the linear
#   transformation from the fitted Rasch difficulties to the D-score scale.
#   This ensures that the new model is linked to the specified anchor tau values.
#
# - If `anchors = NULL`, the behaviour depends on `data_package`.
#   With the default `data_package = "gseddata"`, fit_dmodel() automatically
#   sets two default DDI anchor items:
#     ddigmd057 = 20
#     ddigmd063 = 40
#   if these items are present in the data. This means that a transformation is
#   still estimated from anchors, even when no anchors were explicitly provided.
#
# - If a custom `transform` is supplied while `data_package = "gseddata"`,
#   the custom transform may effectively be ignored/overruled because the
#   default anchors are inserted first. In that case, the function estimates
#   a new transformation from the default anchors instead of using the supplied
#   transform.
#
# - To use a manually specified transformation directly, set:
#     data_package = ""
#   and provide the data explicitly. This prevents automatic insertion of
#   package-specific default anchors.
#
# Implication for the DDI extension:
# - The model with explicitly defined anchors links the DDI item parameters to
#   the original GSED2510 scale as intended.
# - The model without explicit anchors does not reproduce the same tau values,
#   because it uses the default DDI anchors from `gseddata` and therefore
#   estimates a different linear transformation.
# - The model with a supplied transform only behaves as intended when
#   `data_package = ""`.
#
# Suggested future changes to fit_dmodel():
# - Avoid silently inserting default anchors when the user has supplied a
#   non-default `transform`.
# - Give a warning or error when both `anchors` and a non-default `transform`
#   are provided, since anchors determine the transform internally.
# - Make the precedence explicit:
#     anchors > transform
#   or require the user to choose one linking method.
# - Consider adding an argument such as `use_default_anchors = TRUE/FALSE`
#   to prevent unintended anchoring through `data_package`.
# - Store/report clearly whether the final transform was user-supplied,
#   estimated from explicit anchors, or estimated from default package anchors.


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


modelgsed2510$itembank %>% filter(item %in%  names(lookup))
anchors <- modelgsed2510$itembank %>% filter(item %in%  c("gl1fmd026", "gl1lgd026")) %>% dplyr::select(tau) %>% unlist
anchor_names <- ifelse(c("gl1fmd026", "gl1lgd026") %in% names(lookup),
                    lookup[c("gl1fmd026", "gl1lgd026")],
                    names(fixed_gsd))
names(anchors) <- anchor_names
names(anchors) %in% names(data)


## model try 1
i <- length(varlist$items)
f <- length(ddi_fixed)
model_name <- paste("ddi", i, f, sep = "_")
model_name

model_anchors <- dmetric::fit_dmodel(data = data,
                             varlist = varlist,
                             name = "anchors",
                             b_fixed = ddi_fixed,
                             anchors = anchors
)

model$item_fit |> filter((outfit > 1 | infit > 1) & item %in% names(fixed_gsd))


model_no_anchors <- dmetric::fit_dmodel(data = data,
                                     varlist = varlist,
                                     name = "no anchors",
                                     b_fixed = ddi_fixed
)

model$item_fit |> filter((outfit > 1 | infit > 1) & item %in% names(fixed_gsd))

## werkt alleen als data_package wordt uitgeschakelt. Anders gaat de functie via data.package de anchors fixed op twee ddi items.
model_transform <- dmetric::fit_dmodel(data = data,
                                        varlist = varlist,
                                        name = "transform",
                                        b_fixed = ddi_fixed,
                                        data_package = "",
                                        transform = c(55.72413, 3.603965)
)

model$item_fit |> filter((outfit > 1 | infit > 1) & item %in% names(fixed_gsd))

#with anchors
fx_anchors <- model_anchors$itembank |> filter(item %in% ddi_in_gsed$gsed) |> select(item, tau) |> arrange(tau)
fx_core <- ddi_in_gsed |> select(gsed, gsed3, tau) |> arrange(tau)
fx_no_anchors <- model_no_anchors$itembank |> filter(item %in% ddi_in_gsed$gsed) |> select(item, tau) |> arrange(tau)
fx_trans <- model_transform$itembank |> filter(item %in% ddi_in_gsed$gsed) |> select(item, tau) |> arrange(tau)

ft <- lm(fx_anchors$tau ~ fx_no_anchors$tau)
sum(resid(ft))

plot(fx_anchors$tau, fx_no_anchors$tau)

cor(fx_anchors$tau,fx_no_anchors$tau)

#model met gefixte anchors zorgt dat tijdens het schatten van het rasch model de beta parameters in het nieuwe model gelijk zijn aan corresponderende itesm uit het basis-kern model en daarnaast ook de transformatie van deze beta's naar d-score parameters (tau) op gelijke wijze gebeurt en dat het model voor de ddi aansluit op het kernmodel.
#wanneer we de anchors weglaten, dan gebruikt hij per default twee items uit ddi en zet deze op 20 en 40 -- hij gaat dus een nieuwe transformatie parameter bepalen op basis van deze (externe) anchors.
#als we zelf een tranformatie opgeven moet het data-package uitgeschakelt wordne, anders doet hij alsnog het vastzetten van de twee ddi items (als deze in de data voorkomen) op 20 en 40.  zie hieronder ook de print van de dmodel functie (regel 208-252):

# fit_dmodel
# function (varlist, data = NULL, equate = NULL, background_equate = FALSE,
#           name, key = name, anchors = NULL, transform = c(`(Intercept)` = 0,
#                                                           original = 1), age_unit = "months", data_package = "gseddata",
#           scale = NULL, oldstyle = FALSE, itemtable_from = NULL, relevance = c(-Inf,
#                                                                                Inf), population = NULL, prior_mean = NULL, ...)
# {
#   call <- match.call()
#   items <- varlist$items
#   if (data_package == "ddata") {
#     if (is.null(anchors)) {
#       anchors <- c(20, 40)
#       names(anchors) <- c("n12", "n26")
#     }
#     if (is.null(data)) {
#       data <- ddata::get_gcdg(adm = varlist$adm, items = items,
#                               study = varlist$study)
#     }
#     count <- ddata::get_gcdg_count(items = items)
#     if (any(!items %in% dimnames(ddata::get_gcdg_count())[[1]])) {
#       count <- NULL
#     }
#     lean <- NULL
#   }
#   if (data_package == "gseddata") {
#     if (is.null(anchors)) {
#       anchors <- c(20, 40)
#       names(anchors) <- c("ddigmd057", "ddigmd063")
#     }
#     if (is.null(data)) {
#       data <- gseddata::get_data(adm = varlist$adm, items = items,
#                                  min_cat = 0L)
#     }
#     lean <- NULL
#     if (inherits(data, "lean")) {
#       lean <- data
#       data <- as.data.frame(data)
#     }
#     data$age <- data$agedays/365.25
#     age_unit <- "years"
#     count <- NULL
#   }
#   if (data_package == "") {
#     if (is.null(data))
#       stop("Argument `data` not found")
#     if (!"subjid" %in% names(data))
#       stop("Variable `subjid` not found")
#     if (!"agedays" %in% names(data))
#       stop("Variable `agedays` not found")
#     data$age <- data$agedays/365.25
#     age_unit <- "years"
#     count <- NULL
#     lean <- NULL
#   }
#   if (is.null(lean))
#     fit <- rasch(data = data[, items], count = count, equate = equate,
#                  ...)
#   else fit <- rasch_lean(data = lean, items = items, equate = equate,
#                          ...)
#   if (!is.null(anchors)) {
#     found <- names(anchors) %in% items
#     if (any(!found))
#       stop("Not found in `data`: ", names(anchors)[!found])
#     tau <- anchor(get_diff(fit), values = anchors, items = names(anchors))
#     transform <- calculate_transform(original = get_diff(fit),
#                                      transformed = tau)
#   }
#   else {
#     tau <- transform[1L] + transform[2L] * get_diff(fit)
#   }
#   if (!is.null(scale))
#     transform <- c(44.66, scale)
#   qp <- -10:100
#   if (age_unit == "months")
#     data$age <- data$age/12
#   model_equatelist <- bind_equates(equate = equate, background_equate = background_equate,
#                                    items = items)
#   if (data_package == "ddata")
#     itemtable_from <- ddata::itemtable
#   model_itemtable <- calculate_itemtable(items = items, equatelist = model_equatelist,
#                                          itemtable = itemtable_from, activenames = names(equate))
#   itembank <- calculate_itembank(key = key, tau, itemtable = model_itemtable)
#   betad <- dscore(data = data, items = items, key = key, population = population,
#                   itembank = itembank, transform = transform, qp = qp,
#                   metric = "dscore", prior_mean = prior_mean, relevance = relevance)
#   df_d <- data.frame(data[, varlist$adm], betad)
#   beta <- dscore(data, items = items, key = key, population = population,
#                  itembank = itembank, transform = transform, qp = qp,
#                  metric = "logit", prior_mean = prior_mean, relevance = relevance)
#   df_b <- data.frame(data[, varlist$adm], beta)
#   if (!"agedays" %in% names(df_b)) {
#     df_b <- df_b %>% mutate(agedays = as.integer(.data$age *
#                                                    365.25))
#   }
#   if (!"agedays" %in% names(df_d)) {
#     df_d <- df_d %>% mutate(agedays = as.integer(.data$age *
#                                                    365.25))
#   }
#   if (is.null(lean)) {
#     fs <- calculate_fit_statistics(data = data, items = items,
#                                    fit = fit, pa = c("subjid", "agedays"), itembank = itembank,
#                                    ability = df_d, transform = transform)
#   }
#   else {
#     fs <- calculate_fit_statistics(data = lean, items = items,
#                                    fit = fit, pa = c("subjid", "agedays"), itembank = itembank,
#                                    ability = df_d, transform = transform)
#   }
#   version <- list(ddata = packageVersion("ddata"), dmetric = packageVersion("dmetric"),
#                   dscore = packageVersion("dscore"))
#   model <- list(name = name, equate = equate, items = items,
#                 fit = fit, beta_l = df_b, anchors = anchors, transform = transform,
#                 qp = qp, itemtable = model_itemtable, itembank = itembank,
#                 dscore = df_d, item_fit = fs$item_fit, person_fit = fs$person_fit,
#                 equate_fit = fs$equate_fit, residuals = fs$residuals,
#                 active_equates = names(fit$equate), data_package = data_package,
#                 call = call, version = version, date = Sys.time())
#   class(model) <- "dmodel"
#   return(model)
# }
